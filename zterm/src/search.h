#ifndef SEARCH_H
#define SEARCH_H

#include "terminal.h"
#include "scrollback.h"

#define MAX_SEARCH_QUERY 256

typedef struct {
    char query[MAX_SEARCH_QUERY];
    int active;
    int current_match;
    int num_matches;
    int *match_lines;
    int *match_cols;
    int max_matches;
    int case_sensitive;
} Search;

void search_init(Search *search);
void search_free(Search *search);
void search_start(Search *search, const char *query, int case_sensitive);
void search_execute(Search *search, Terminal *term, Scrollback *sb);
void search_next(Search *search);
void search_prev(Search *search);
void search_highlight(Search *search, Terminal *term, int term_height);
void search_stop(Search *search);

#endif
