#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <string.h>
#include "terminal.h"
#include "pty.h"
#include "tabs.h"
#include "mouse.h"
#include "clipboard.h"
#include "config.h"
#include "scrollback.h"
#include "search.h"

static struct termios orig_termios;
static TabManager *g_tm = NULL;
static Config *g_config = NULL;
static volatile sig_atomic_t resize_pending = 0;
static int search_mode = 0;
static char search_query[MAX_SEARCH_QUERY] = {0};
static int search_query_len = 0;

void disable_raw_mode(void) {
    mouse_disable();
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enable_raw_mode(void) {
    tcgetattr(STDIN_FILENO, &orig_termios);
    atexit(disable_raw_mode);

    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
    raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
    if (g_config->mouse_enabled) {
        mouse_enable();
    }
}

void handle_sigwinch(int sig) {
    (void)sig;
    resize_pending = 1;
}

void get_terminal_size(int *width, int *height) {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0) {
        *width = ws.ws_col;
        *height = ws.ws_row;
    } else {
        *width = 80;
        *height = 24;
    }
}

void handle_resize(TabManager *tm) {
    int width, height;
    get_terminal_size(&width, &height);
    tabs_resize_all(tm, width, height);

    // Re-render current tab
    Tab *tab = tabs_current(tm);
    if (tab) {
        printf("\033[2J"); // Clear screen
        tabs_render_bar(tm);
        terminal_render(&tab->terminal);
    }
}

void render_search_bar(const char *query, int num_matches, int current) {
    printf("\033[%d;1H", 2); // Line 2 (below tab bar)
    printf("\033[K"); // Clear line
    printf("\033[48;5;58m"); // Brown background
    printf(" Search: %s", query);
    if (num_matches > 0) {
        printf(" [%d/%d]", current + 1, num_matches);
    } else if (query[0] != '\0') {
        printf(" [No matches]");
    }
    printf(" \033[90m(Enter:done Esc:cancel n:next p:prev)\033[0m");
    printf("\033[0m");
    fflush(stdout);
}

void full_render(Tab *tab, TabManager *tm) {
    printf("\033[2J"); // Clear screen
    tabs_render_bar(tm);

    if (search_mode) {
        render_search_bar(search_query, tab->search.num_matches, tab->search.current_match);
    }

    // Render scrollback if scrolled
    if (tab->scrollback.scroll_offset > 0) {
        scrollback_render(&tab->scrollback, &tab->terminal, tab->terminal.height);
    }

    // Apply search highlighting
    if (tab->search.active) {
        search_highlight(&tab->search, &tab->terminal, tab->terminal.height);
    }

    terminal_render(&tab->terminal);
}

int main(int argc, char *argv[]) {
    Config config;
    config_init(&config);

    // Load configuration
    char config_path[512];
    config_get_default_path(config_path, sizeof(config_path));
    if (config_load(&config, config_path) < 0) {
        // Create default config file
        config_save(&config, config_path);
        printf("Created default configuration at %s\n", config_path);
    }

    g_config = &config;

    TabManager tm;
    g_tm = &tm;

    tabs_init(&tm);

    // Get initial terminal size
    int width, height;
    get_terminal_size(&width, &height);

    // Create first tab
    if (tabs_create(&tm, config.shell, width, height - 1, config.scrollback_lines) < 0) {
        fprintf(stderr, "Failed to create initial tab\n");
        return 1;
    }

    // Set up signal handler for window resize
    struct sigaction sa;
    sa.sa_handler = handle_sigwinch;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGWINCH, &sa, NULL);

    enable_raw_mode();

    printf("\033[2J\033[H");
    printf("\033[1;36mZ-Terminal\033[0m \033[32mv2.0\033[0m (%dx%d)\n", width, height);
    printf("Config: \033[33m%s\033[0m\n", config_path);
    printf("Features: \033[35mScrollback(%d)\033[0m | \033[36mSearch\033[0m | \033[32mConfig\033[0m\n",
           config.scrollback_lines);
    sleep(2);

    char input_buf[256];
    char output_buf[4096];

    while (1) {
        // Handle resize if pending
        if (resize_pending) {
            resize_pending = 0;
            handle_resize(&tm);
        }

        Tab *current = tabs_current(&tm);
        if (!current) break;

        // Read from PTY of current tab (only if not in search mode)
        if (!search_mode) {
            ssize_t n = pty_read(&current->pty, output_buf, sizeof(output_buf) - 1);
            if (n > 0) {
                // Save current line to scrollback before writing new data
                if (current->terminal.cursor_y < current->terminal.height) {
                    scrollback_add_line(&current->scrollback,
                                      current->terminal.buffer[current->terminal.cursor_y],
                                      current->terminal.width);
                }

                terminal_write(&current->terminal, output_buf, n);
                full_render(current, &tm);
            }
        }

        // Read from stdin
        ssize_t n = read(STDIN_FILENO, input_buf, sizeof(input_buf));
        if (n > 0) {
            // Handle search mode
            if (search_mode) {
                if (input_buf[0] == 27) { // Escape
                    search_stop(&current->search);
                    search_mode = 0;
                    search_query[0] = '\0';
                    search_query_len = 0;
                    full_render(current, &tm);
                } else if (input_buf[0] == '\n' || input_buf[0] == '\r') { // Enter
                    search_mode = 0;
                    full_render(current, &tm);
                } else if (input_buf[0] == 127 || input_buf[0] == 8) { // Backspace
                    if (search_query_len > 0) {
                        search_query[--search_query_len] = '\0';
                        search_start(&current->search, search_query, 0);
                        search_execute(&current->search, &current->terminal, &current->scrollback);
                        full_render(current, &tm);
                    }
                } else if (input_buf[0] == 'n') { // Next match
                    search_next(&current->search);
                    full_render(current, &tm);
                } else if (input_buf[0] == 'p') { // Previous match
                    search_prev(&current->search);
                    full_render(current, &tm);
                } else if (input_buf[0] >= 32 && input_buf[0] < 127) { // Printable character
                    if (search_query_len < MAX_SEARCH_QUERY - 1) {
                        search_query[search_query_len++] = input_buf[0];
                        search_query[search_query_len] = '\0';
                        search_start(&current->search, search_query, 0);
                        search_execute(&current->search, &current->terminal, &current->scrollback);
                        full_render(current, &tm);
                    }
                }
                continue;
            }

            // Check for mouse events
            if (config.mouse_enabled && input_buf[0] == '\033' && n >= 3 &&
                input_buf[1] == '[' && input_buf[2] == '<') {
                MouseEvent event;
                if (mouse_parse_event(input_buf, n, &event)) {
                    // Handle scrolling
                    if (event.type == MOUSE_EVENT_SCROLL_UP) {
                        scrollback_scroll_up(&current->scrollback, 3);
                        full_render(current, &tm);
                    } else if (event.type == MOUSE_EVENT_SCROLL_DOWN) {
                        scrollback_scroll_down(&current->scrollback, 3);
                        full_render(current, &tm);
                    } else {
                        mouse_handle_selection(&current->terminal, &event);
                        if (event.type == MOUSE_EVENT_PRESS && event.button == 1) {
                            char *selection = mouse_get_selection(&current->terminal);
                            if (selection && config.clipboard_enabled) {
                                clipboard_set(selection);
                                free(selection);
                            }
                        }
                    }
                    continue;
                }
            }

            // Handle special key combinations
            if (n == 1) {
                if (input_buf[0] == 20) { // Ctrl+T - New tab
                    tabs_create(&tm, config.shell, width, height - 1, config.scrollback_lines);
                    full_render(current, &tm);
                    continue;
                } else if (input_buf[0] == 23) { // Ctrl+W - Close tab
                    tabs_close(&tm, tm.current_tab);
                    if (tm.num_tabs == 0) break;
                    current = tabs_current(&tm);
                    full_render(current, &tm);
                    continue;
                } else if (input_buf[0] == 3) { // Ctrl+C - Copy selection
                    if (config.clipboard_enabled) {
                        char *selection = mouse_get_selection(&current->terminal);
                        if (selection) {
                            clipboard_set(selection);
                            free(selection);
                        }
                    }
                    continue;
                } else if (input_buf[0] == 22) { // Ctrl+V - Paste
                    if (config.clipboard_enabled) {
                        char *clipboard = clipboard_get();
                        if (clipboard) {
                            pty_write(&current->pty, clipboard, strlen(clipboard));
                            free(clipboard);
                        }
                    }
                    continue;
                } else if (input_buf[0] == 6) { // Ctrl+F - Search
                    if (config.search_enabled) {
                        search_mode = 1;
                        search_query[0] = '\0';
                        search_query_len = 0;
                        search_start(&current->search, "", 0);
                        full_render(current, &tm);
                    }
                    continue;
                } else if (input_buf[0] == 19) { // Ctrl+S - Toggle scroll mode
                    if (current->scrollback.scroll_offset == 0) {
                        scrollback_scroll_up(&current->scrollback, current->terminal.height);
                    } else {
                        scrollback_scroll_to_bottom(&current->scrollback);
                    }
                    full_render(current, &tm);
                    continue;
                }
            } else if (n >= 3 && input_buf[0] == 27 && input_buf[1] == '[') {
                // Page Up/Down
                if (input_buf[2] == '5' && input_buf[3] == '~') { // Page Up
                    scrollback_scroll_up(&current->scrollback, current->terminal.height);
                    full_render(current, &tm);
                    continue;
                } else if (input_buf[2] == '6' && input_buf[3] == '~') { // Page Down
                    scrollback_scroll_down(&current->scrollback, current->terminal.height);
                    full_render(current, &tm);
                    continue;
                }
            } else if (n == 2 && input_buf[0] == 27) {
                if (input_buf[1] == ']') { // Ctrl+] - Next tab
                    tabs_next(&tm);
                    current = tabs_current(&tm);
                    full_render(current, &tm);
                    continue;
                } else if (input_buf[1] == '[') { // Ctrl+[ - Previous tab
                    tabs_prev(&tm);
                    current = tabs_current(&tm);
                    full_render(current, &tm);
                    continue;
                }
            }

            // Send to current tab's PTY
            pty_write(&current->pty, input_buf, n);
        }

        usleep(config.refresh_rate_ms * 1000);
    }

    tabs_cleanup(&tm);
    disable_raw_mode();
    printf("\n\n\033[1;35mZ-Terminal exited.\033[0m\n");

    return 0;
}
