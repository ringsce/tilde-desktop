#include "terminal.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

static const char* color_codes[] = {
    "\033[30m",  // BLACK
    "\033[31m",  // RED
    "\033[32m",  // GREEN
    "\033[33m",  // YELLOW
    "\033[34m",  // BLUE
    "\033[35m",  // MAGENTA
    "\033[36m",  // CYAN
    "\033[37m",  // WHITE
    "\033[90m",  // BRIGHT_BLACK
    "\033[91m",  // BRIGHT_RED
    "\033[92m",  // BRIGHT_GREEN
    "\033[93m",  // BRIGHT_YELLOW
    "\033[94m",  // BRIGHT_BLUE
    "\033[95m",  // BRIGHT_MAGENTA
    "\033[96m",  // BRIGHT_CYAN
    "\033[97m",  // BRIGHT_WHITE
};

static const char* bg_color_codes[] = {
    "\033[40m",  // BLACK
    "\033[41m",  // RED
    "\033[42m",  // GREEN
    "\033[43m",  // YELLOW
    "\033[44m",  // BLUE
    "\033[45m",  // MAGENTA
    "\033[46m",  // CYAN
    "\033[47m",  // WHITE
    "\033[100m", // BRIGHT_BLACK
    "\033[101m", // BRIGHT_RED
    "\033[102m", // BRIGHT_GREEN
    "\033[103m", // BRIGHT_YELLOW
    "\033[104m", // BRIGHT_BLUE
    "\033[105m", // BRIGHT_MAGENTA
    "\033[106m", // BRIGHT_CYAN
    "\033[107m", // BRIGHT_WHITE
};

void terminal_init(Terminal *term, int width, int height) {
    term->width = width > MAX_TERMINAL_WIDTH ? MAX_TERMINAL_WIDTH : width;
    term->height = height > MAX_TERMINAL_HEIGHT ? MAX_TERMINAL_HEIGHT : height;
    
    for (int y = 0; y < MAX_TERMINAL_HEIGHT; y++) {
        for (int x = 0; x < MAX_TERMINAL_WIDTH; x++) {
            term->buffer[y][x].ch = ' ';
            term->buffer[y][x].fg = COLOR_DEFAULT;
            term->buffer[y][x].bg = COLOR_DEFAULT;
            term->buffer[y][x].bold = 0;
            term->buffer[y][x].underline = 0;
        }
    }
    term->cursor_x = 0;
    term->cursor_y = 0;
    term->current_fg = COLOR_DEFAULT;
    term->current_bg = COLOR_DEFAULT;
    term->current_bold = 0;
    term->current_underline = 0;
    term->in_escape = 0;
    term->escape_len = 0;
}

void terminal_resize(Terminal *term, int new_width, int new_height) {
    // Clamp to maximum sizes
    new_width = new_width > MAX_TERMINAL_WIDTH ? MAX_TERMINAL_WIDTH : new_width;
    new_height = new_height > MAX_TERMINAL_HEIGHT ? MAX_TERMINAL_HEIGHT : new_height;
    
    if (new_width < 1) new_width = 1;
    if (new_height < 1) new_height = 1;
    
    // If growing horizontally, pad existing lines with spaces
    if (new_width > term->width) {
        for (int y = 0; y < term->height; y++) {
            for (int x = term->width; x < new_width; x++) {
                term->buffer[y][x].ch = ' ';
                term->buffer[y][x].fg = COLOR_DEFAULT;
                term->buffer[y][x].bg = COLOR_DEFAULT;
                term->buffer[y][x].bold = 0;
                term->buffer[y][x].underline = 0;
            }
        }
    }
    
    // If growing vertically, initialize new lines
    if (new_height > term->height) {
        for (int y = term->height; y < new_height; y++) {
            for (int x = 0; x < new_width; x++) {
                term->buffer[y][x].ch = ' ';
                term->buffer[y][x].fg = COLOR_DEFAULT;
                term->buffer[y][x].bg = COLOR_DEFAULT;
                term->buffer[y][x].bold = 0;
                term->buffer[y][x].underline = 0;
            }
        }
    }
    
    term->width = new_width;
    term->height = new_height;
    
    // Adjust cursor if it's out of bounds
    if (term->cursor_x >= term->width) term->cursor_x = term->width - 1;
    if (term->cursor_y >= term->height) term->cursor_y = term->height - 1;
    if (term->cursor_x < 0) term->cursor_x = 0;
    if (term->cursor_y < 0) term->cursor_y = 0;
}

void terminal_clear(Terminal *term) {
    for (int y = 0; y < term->height; y++) {
        for (int x = 0; x < term->width; x++) {
            term->buffer[y][x].ch = ' ';
            term->buffer[y][x].fg = COLOR_DEFAULT;
            term->buffer[y][x].bg = COLOR_DEFAULT;
            term->buffer[y][x].bold = 0;
            term->buffer[y][x].underline = 0;
        }
    }
    term->cursor_x = 0;
    term->cursor_y = 0;
}

static void parse_sgr(Terminal *term, const char *params) {
    if (params[0] == '\0' || strcmp(params, "0") == 0) {
        // Reset
        term->current_fg = COLOR_DEFAULT;
        term->current_bg = COLOR_DEFAULT;
        term->current_bold = 0;
        term->current_underline = 0;
        return;
    }
    
    char *params_copy = strdup(params);
    char *token = strtok(params_copy, ";");
    
    while (token != NULL) {
        int code = atoi(token);
        
        switch (code) {
            case 0:  // Reset
                term->current_fg = COLOR_DEFAULT;
                term->current_bg = COLOR_DEFAULT;
                term->current_bold = 0;
                term->current_underline = 0;
                break;
            case 1:  // Bold
                term->current_bold = 1;
                break;
            case 4:  // Underline
                term->current_underline = 1;
                break;
            case 22: // Normal intensity
                term->current_bold = 0;
                break;
            case 24: // Not underlined
                term->current_underline = 0;
                break;
            case 30: case 31: case 32: case 33:
            case 34: case 35: case 36: case 37:
                term->current_fg = code - 30;
                break;
            case 39: // Default foreground
                term->current_fg = COLOR_DEFAULT;
                break;
            case 40: case 41: case 42: case 43:
            case 44: case 45: case 46: case 47:
                term->current_bg = code - 40;
                break;
            case 49: // Default background
                term->current_bg = COLOR_DEFAULT;
                break;
            case 90: case 91: case 92: case 93:
            case 94: case 95: case 96: case 97:
                term->current_fg = code - 90 + 8;
                break;
            case 100: case 101: case 102: case 103:
            case 104: case 105: case 106: case 107:
                term->current_bg = code - 100 + 8;
                break;
        }
        
        token = strtok(NULL, ";");
    }
    
    free(params_copy);
}

static void process_escape_sequence(Terminal *term) {
    if (term->escape_len < 2) return;
    
    if (term->escape_buf[0] == '[') {
        char command = term->escape_buf[term->escape_len - 1];
        char params[256] = {0};
        
        if (term->escape_len > 2) {
            strncpy(params, term->escape_buf + 1, term->escape_len - 2);
        }
        
        switch (command) {
            case 'm': // SGR - Select Graphic Rendition
                parse_sgr(term, params);
                break;
            case 'H': case 'f': { // Cursor position
                int row = 1, col = 1;
                if (strlen(params) > 0) {
                    sscanf(params, "%d;%d", &row, &col);
                }
                term->cursor_y = row - 1;
                term->cursor_x = col - 1;
                if (term->cursor_y < 0) term->cursor_y = 0;
                if (term->cursor_y >= term->height) term->cursor_y = term->height - 1;
                if (term->cursor_x < 0) term->cursor_x = 0;
                if (term->cursor_x >= term->width) term->cursor_x = term->width - 1;
                break;
            }
            case 'J': // Erase in display
                if (params[0] == '2' || params[0] == '\0') {
                    terminal_clear(term);
                }
                break;
            case 'K': // Erase in line
                for (int x = term->cursor_x; x < term->width; x++) {
                    term->buffer[term->cursor_y][x].ch = ' ';
                    term->buffer[term->cursor_y][x].fg = COLOR_DEFAULT;
                    term->buffer[term->cursor_y][x].bg = COLOR_DEFAULT;
                }
                break;
        }
    }
}

void terminal_write(Terminal *term, const char *data, size_t len) {
    for (size_t i = 0; i < len; i++) {
        char c = data[i];
        
        if (term->in_escape) {
            term->escape_buf[term->escape_len++] = c;
            
            if (term->escape_len >= 255 ||
                (c >= 'A' && c <= 'Z') ||
                (c >= 'a' && c <= 'z')) {
                term->escape_buf[term->escape_len] = '\0';
                process_escape_sequence(term);
                term->in_escape = 0;
                term->escape_len = 0;
            }
            continue;
        }
        
        if (c == '\033') {
            term->in_escape = 1;
            term->escape_len = 0;
        } else if (c == '\n') {
            term->cursor_x = 0;
            term->cursor_y++;
            if (term->cursor_y >= term->height) {
                // Scroll up
                memmove(term->buffer[0], term->buffer[1],
                       sizeof(term->buffer[0]) * (term->height - 1));
                for (int x = 0; x < term->width; x++) {
                    term->buffer[term->height - 1][x].ch = ' ';
                    term->buffer[term->height - 1][x].fg = COLOR_DEFAULT;
                    term->buffer[term->height - 1][x].bg = COLOR_DEFAULT;
                    term->buffer[term->height - 1][x].bold = 0;
                    term->buffer[term->height - 1][x].underline = 0;
                }
                term->cursor_y = term->height - 1;
            }
        } else if (c == '\r') {
            term->cursor_x = 0;
        } else if (c == '\b') {
            if (term->cursor_x > 0) term->cursor_x--;
        } else if (c == '\t') {
            int spaces = 8 - (term->cursor_x % 8);
            for (int j = 0; j < spaces && term->cursor_x < term->width; j++) {
                term->buffer[term->cursor_y][term->cursor_x].ch = ' ';
                term->buffer[term->cursor_y][term->cursor_x].fg = term->current_fg;
                term->buffer[term->cursor_y][term->cursor_x].bg = term->current_bg;
                term->cursor_x++;
            }
        } else if (isprint(c) || (unsigned char)c >= 128) {
            term->buffer[term->cursor_y][term->cursor_x].ch = c;
            term->buffer[term->cursor_y][term->cursor_x].fg = term->current_fg;
            term->buffer[term->cursor_y][term->cursor_x].bg = term->current_bg;
            term->buffer[term->cursor_y][term->cursor_x].bold = term->current_bold;
            term->buffer[term->cursor_y][term->cursor_x].underline = term->current_underline;
            term->cursor_x++;
            
            if (term->cursor_x >= term->width) {
                term->cursor_x = 0;
                term->cursor_y++;
                if (term->cursor_y >= term->height) {
                    memmove(term->buffer[0], term->buffer[1],
                           sizeof(term->buffer[0]) * (term->height - 1));
                    for (int x = 0; x < term->width; x++) {
                        term->buffer[term->height - 1][x].ch = ' ';
                        term->buffer[term->height - 1][x].fg = COLOR_DEFAULT;
                        term->buffer[term->height - 1][x].bg = COLOR_DEFAULT;
                        term->buffer[term->height - 1][x].bold = 0;
                        term->buffer[term->height - 1][x].underline = 0;
                    }
                    term->cursor_y = term->height - 1;
                }
            }
        }
    }
}

void terminal_render(Terminal *term) {
    printf("\033[2J\033[H"); // Clear screen and move to home
    
    Color last_fg = COLOR_DEFAULT;
    Color last_bg = COLOR_DEFAULT;
    int last_bold = 0;
    int last_underline = 0;
    
    for (int y = 0; y < term->height; y++) {
        for (int x = 0; x < term->width; x++) {
            Cell *cell = &term->buffer[y][x];
            
            // Update colors and attributes if they changed
            if (cell->fg != last_fg || cell->bg != last_bg ||
                cell->bold != last_bold || cell->underline != last_underline) {
                
                printf("\033[0m"); // Reset
                
                if (cell->bold) printf("\033[1m");
                if (cell->underline) printf("\033[4m");
                
                if (cell->fg != COLOR_DEFAULT && cell->fg < 16) {
                    printf("%s", color_codes[cell->fg]);
                }
                
                if (cell->bg != COLOR_DEFAULT && cell->bg < 16) {
                    printf("%s", bg_color_codes[cell->bg]);
                }
                
                last_fg = cell->fg;
                last_bg = cell->bg;
                last_bold = cell->bold;
                last_underline = cell->underline;
            }
            
            putchar(cell->ch);
        }
        printf("\033[0m\n"); // Reset at end of line
        last_fg = last_bg = COLOR_DEFAULT;
        last_bold = last_underline = 0;
    }
    
    printf("\033[%d;%dH", term->cursor_y + 1, term->cursor_x + 1);
    fflush(stdout);
}
