#ifndef SCROLLBACK_H
#define SCROLLBACK_H

#include "terminal.h"

#define MAX_SCROLLBACK_LINES 50000

typedef struct {
    Cell *lines;
    int num_lines;
    int max_lines;
    int width;
    int scroll_offset;
} Scrollback;

void scrollback_init(Scrollback *sb, int max_lines, int width);
void scrollback_add_line(Scrollback *sb, const Cell *line, int width);
void scrollback_resize(Scrollback *sb, int new_width);
void scrollback_scroll_up(Scrollback *sb, int lines);
void scrollback_scroll_down(Scrollback *sb, int lines);
void scrollback_scroll_to_bottom(Scrollback *sb);
void scrollback_render(Scrollback *sb, Terminal *term, int term_height);
void scrollback_free(Scrollback *sb);

#endif
