#include "search.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void search_init(Search *search) {
    search->query[0] = '\0';
    search->active = 0;
    search->current_match = 0;
    search->num_matches = 0;
    search->max_matches = 1000;
    search->match_lines = malloc(search->max_matches * sizeof(int));
    search->match_cols = malloc(search->max_matches * sizeof(int));
    search->case_sensitive = 0;
}

void search_free(Search *search) {
    if (search->match_lines) {
        free(search->match_lines);
        search->match_lines = NULL;
    }
    if (search->match_cols) {
        free(search->match_cols);
        search->match_cols = NULL;
    }
}

static int strncasecmp_custom(const char *s1, const char *s2, size_t n) {
    for (size_t i = 0; i < n; i++) {
        int c1 = tolower((unsigned char)s1[i]);
        int c2 = tolower((unsigned char)s2[i]);
        if (c1 != c2) return c1 - c2;
        if (c1 == '\0') return 0;
    }
    return 0;
}

void search_start(Search *search, const char *query, int case_sensitive) {
    strncpy(search->query, query, MAX_SEARCH_QUERY - 1);
    search->query[MAX_SEARCH_QUERY - 1] = '\0';
    search->case_sensitive = case_sensitive;
    search->active = 1;
    search->current_match = 0;
    search->num_matches = 0;
}

void search_execute(Search *search, Terminal *term, Scrollback *sb) {
    if (!search->active || search->query[0] == '\0') return;

    search->num_matches = 0;
    int query_len = strlen(search->query);

    // Search in scrollback
    if (sb && sb->lines) {
        for (int y = 0; y < sb->num_lines && search->num_matches < search->max_matches; y++) {
            for (int x = 0; x <= sb->width - query_len; x++) {
                int match = 1;
                for (int i = 0; i < query_len; i++) {
                    char ch = sb->lines[y * sb->width + x + i].ch;
                    char qch = search->query[i];

                    if (search->case_sensitive) {
                        if (ch != qch) {
                            match = 0;
                            break;
                        }
                    } else {
                        if (tolower(ch) != tolower(qch)) {
                            match = 0;
                            break;
                        }
                    }
                }

                if (match) {
                    search->match_lines[search->num_matches] = y - sb->num_lines; // Negative for scrollback
                    search->match_cols[search->num_matches] = x;
                    search->num_matches++;
                }
            }
        }
    }

    // Search in visible terminal
    for (int y = 0; y < term->height && search->num_matches < search->max_matches; y++) {
        for (int x = 0; x <= term->width - query_len; x++) {
            int match = 1;
            for (int i = 0; i < query_len; i++) {
                char ch = term->buffer[y][x + i].ch;
                char qch = search->query[i];

                if (search->case_sensitive) {
                    if (ch != qch) {
                        match = 0;
                        break;
                    }
                } else {
                    if (tolower(ch) != tolower(qch)) {
                        match = 0;
                        break;
                    }
                }
            }

            if (match) {
                search->match_lines[search->num_matches] = y;
                search->match_cols[search->num_matches] = x;
                search->num_matches++;
            }
        }
    }
}

void search_next(Search *search) {
    if (search->num_matches > 0) {
        search->current_match = (search->current_match + 1) % search->num_matches;
    }
}

void search_prev(Search *search) {
    if (search->num_matches > 0) {
        search->current_match = (search->current_match - 1 + search->num_matches) % search->num_matches;
    }
}

void search_highlight(Search *search, Terminal *term, int term_height) {
    if (!search->active || search->num_matches == 0) return;

    int query_len = strlen(search->query);

    for (int i = 0; i < search->num_matches; i++) {
        int line = search->match_lines[i];
        int col = search->match_cols[i];

        // Only highlight visible lines
        if (line >= 0 && line < term_height) {
            for (int j = 0; j < query_len && col + j < term->width; j++) {
                if (i == search->current_match) {
                    // Current match - yellow background
                    term->buffer[line][col + j].bg = COLOR_YELLOW;
                    term->buffer[line][col + j].fg = COLOR_BLACK;
                } else {
                    // Other matches - cyan background
                    term->buffer[line][col + j].bg = COLOR_CYAN;
                    term->buffer[line][col + j].fg = COLOR_BLACK;
                }
            }
        }
    }
}

void search_stop(Search *search) {
    search->active = 0;
    search->num_matches = 0;
    search->current_match = 0;
}
