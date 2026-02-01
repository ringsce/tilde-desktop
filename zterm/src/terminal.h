#ifndef TERMINAL_H
#define TERMINAL_H

#include <stddef.h>

#define MAX_TERMINAL_WIDTH 300
#define MAX_TERMINAL_HEIGHT 100

typedef enum {
    COLOR_BLACK = 0,
    COLOR_RED,
    COLOR_GREEN,
    COLOR_YELLOW,
    COLOR_BLUE,
    COLOR_MAGENTA,
    COLOR_CYAN,
    COLOR_WHITE,
    COLOR_BRIGHT_BLACK,
    COLOR_BRIGHT_RED,
    COLOR_BRIGHT_GREEN,
    COLOR_BRIGHT_YELLOW,
    COLOR_BRIGHT_BLUE,
    COLOR_BRIGHT_MAGENTA,
    COLOR_BRIGHT_CYAN,
    COLOR_BRIGHT_WHITE,
    COLOR_DEFAULT = 255
} Color;

typedef struct {
    char ch;
    Color fg;
    Color bg;
    int bold;
    int underline;
} Cell;

typedef struct {
    Cell buffer[MAX_TERMINAL_HEIGHT][MAX_TERMINAL_WIDTH];
    int cursor_x;
    int cursor_y;
    int width;
    int height;
    Color current_fg;
    Color current_bg;
    int current_bold;
    int current_underline;
    
    // ANSI escape sequence parser state
    char escape_buf[256];
    int escape_len;
    int in_escape;
} Terminal;

void terminal_init(Terminal *term, int width, int height);
void terminal_resize(Terminal *term, int new_width, int new_height);
void terminal_write(Terminal *term, const char *data, size_t len);
void terminal_render(Terminal *term);
void terminal_clear(Terminal *term);

#endif
