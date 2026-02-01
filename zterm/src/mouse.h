#ifndef MOUSE_H
#define MOUSE_H

#include "terminal.h"

typedef enum {
    MOUSE_EVENT_NONE,
    MOUSE_EVENT_PRESS,
    MOUSE_EVENT_RELEASE,
    MOUSE_EVENT_DRAG,
    MOUSE_EVENT_SCROLL_UP,
    MOUSE_EVENT_SCROLL_DOWN
} MouseEventType;

typedef struct {
    MouseEventType type;
    int x;
    int y;
    int button; // 0=left, 1=middle, 2=right
} MouseEvent;

void mouse_enable(void);
void mouse_disable(void);
int mouse_parse_event(const char *buf, size_t len, MouseEvent *event);
void mouse_handle_selection(Terminal *term, MouseEvent *event);
char* mouse_get_selection(Terminal *term);

#endif