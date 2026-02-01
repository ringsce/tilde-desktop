#ifndef TABS_H
#define TABS_H

#include "terminal.h"
#include "pty.h"
#include "scrollback.h"
#include "search.h"

#define MAX_TABS 10

typedef struct {
    Terminal terminal;
    PTY pty;
    Scrollback scrollback;
    Search search;
    char title[64];
    int active;
} Tab;

typedef struct {
    Tab tabs[MAX_TABS];
    int num_tabs;
    int current_tab;
} TabManager;

void tabs_init(TabManager *tm);
int tabs_create(TabManager *tm, const char *shell, int width, int height, int scrollback_lines);
int tabs_close(TabManager *tm, int index);
void tabs_switch(TabManager *tm, int index);
void tabs_next(TabManager *tm);
void tabs_prev(TabManager *tm);
Tab* tabs_current(TabManager *tm);
void tabs_render_bar(TabManager *tm);
void tabs_resize_all(TabManager *tm, int width, int height);
void tabs_cleanup(TabManager *tm);

#endif
