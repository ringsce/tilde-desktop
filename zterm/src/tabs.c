#include "tabs.h"
#include <string.h>
#include <stdio.h>
#include <unistd.h>

void tabs_init(TabManager *tm) {
    tm->num_tabs = 0;
    tm->current_tab = -1;

    for (int i = 0; i < MAX_TABS; i++) {
        tm->tabs[i].active = 0;
    }
}

int tabs_create(TabManager *tm, const char *shell, int width, int height, int scrollback_lines) {
    if (tm->num_tabs >= MAX_TABS) {
        return -1;
    }

    int index = tm->num_tabs;
    Tab *tab = &tm->tabs[index];

    terminal_init(&tab->terminal, width, height);
    scrollback_init(&tab->scrollback, scrollback_lines, width);
    search_init(&tab->search);

    if (pty_create(&tab->pty, shell, width, height) < 0) {
        scrollback_free(&tab->scrollback);
        search_free(&tab->search);
        return -1;
    }

    snprintf(tab->title, sizeof(tab->title), "Tab %d", index + 1);
    tab->active = 1;

    tm->num_tabs++;
    if (tm->current_tab < 0) {
        tm->current_tab = 0;
    }

    return index;
}

int tabs_close(TabManager *tm, int index) {
    if (index < 0 || index >= tm->num_tabs || !tm->tabs[index].active) {
        return -1;
    }

    pty_close(&tm->tabs[index].pty);
    scrollback_free(&tm->tabs[index].scrollback);
    search_free(&tm->tabs[index].search);
    tm->tabs[index].active = 0;

    // Shift tabs down
    for (int i = index; i < tm->num_tabs - 1; i++) {
        tm->tabs[i] = tm->tabs[i + 1];
    }

    tm->num_tabs--;

    // Adjust current tab
    if (tm->num_tabs == 0) {
        tm->current_tab = -1;
    } else if (tm->current_tab >= tm->num_tabs) {
        tm->current_tab = tm->num_tabs - 1;
    }

    return 0;
}

void tabs_switch(TabManager *tm, int index) {
    if (index >= 0 && index < tm->num_tabs && tm->tabs[index].active) {
        tm->current_tab = index;
    }
}

void tabs_next(TabManager *tm) {
    if (tm->num_tabs > 0) {
        tm->current_tab = (tm->current_tab + 1) % tm->num_tabs;
    }
}

void tabs_prev(TabManager *tm) {
    if (tm->num_tabs > 0) {
        tm->current_tab = (tm->current_tab - 1 + tm->num_tabs) % tm->num_tabs;
    }
}

Tab* tabs_current(TabManager *tm) {
    if (tm->current_tab >= 0 && tm->current_tab < tm->num_tabs) {
        return &tm->tabs[tm->current_tab];
    }
    return NULL;
}

void tabs_render_bar(TabManager *tm) {
    printf("\033[H\033[K"); // Move to top and clear line
    printf("\033[48;5;236m"); // Dark gray background

    for (int i = 0; i < tm->num_tabs; i++) {
        if (i == tm->current_tab) {
            printf("\033[1;97;48;5;24m"); // Bright white on blue (active)
        } else {
            printf("\033[37;48;5;236m"); // White on dark gray (inactive)
        }

        printf(" %s ", tm->tabs[i].title);
        printf("\033[0m\033[48;5;236m"); // Reset to bar background
        printf("│");
    }

    // Show help on right side
    printf("\033[%dG", 50); // Move to column 50
    printf("\033[90m[Ctrl+F:Search Ctrl+S:Scroll PgUp/PgDn]\033[0m");

    printf("\033[0m\n"); // Reset and newline
    fflush(stdout);
}

void tabs_resize_all(TabManager *tm, int width, int height) {
    for (int i = 0; i < tm->num_tabs; i++) {
        if (tm->tabs[i].active) {
            terminal_resize(&tm->tabs[i].terminal, width, height - 1); // -1 for tab bar
            scrollback_resize(&tm->tabs[i].scrollback, width);
            pty_resize(&tm->tabs[i].pty, width, height - 1);
        }
    }
}

void tabs_cleanup(TabManager *tm) {
    for (int i = 0; i < tm->num_tabs; i++) {
        if (tm->tabs[i].active) {
            pty_close(&tm->tabs[i].pty);
            scrollback_free(&tm->tabs[i].scrollback);
            search_free(&tm->tabs[i].search);
        }
    }
}
