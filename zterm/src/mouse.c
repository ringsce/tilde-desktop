#include "mouse.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int selection_start_x = -1;
static int selection_start_y = -1;
static int selection_end_x = -1;
static int selection_end_y = -1;
static int selecting = 0;

void mouse_enable(void) {
    // Enable mouse tracking
    printf("\033[?1000h"); // Normal tracking
    printf("\033[?1002h"); // Button event tracking
    printf("\033[?1006h"); // SGR extended mode
    fflush(stdout);
}

void mouse_disable(void) {
    printf("\033[?1000l");
    printf("\033[?1002l");
    printf("\033[?1006l");
    fflush(stdout);
}

int mouse_parse_event(const char *buf, size_t len, MouseEvent *event) {
    // Parse SGR mouse format: \033[<B;X;YM or \033[<B;X;Ym
    if (len < 6 || buf[0] != '\033' || buf[1] != '[' || buf[2] != '<') {
        return 0;
    }
    
    int button, x, y;
    char action;
    
    int parsed = sscanf(buf + 3, "%d;%d;%d%c", &button, &x, &y, &action);
    
    if (parsed != 4) {
        return 0;
    }
    
    event->x = x - 1; // Convert to 0-indexed
    event->y = y - 1;
    event->button = button & 3;
    
    if (action == 'M') {
        if (button >= 64) {
            if (button == 64) {
                event->type = MOUSE_EVENT_SCROLL_UP;
            } else if (button == 65) {
                event->type = MOUSE_EVENT_SCROLL_DOWN;
            }
        } else {
            event->type = MOUSE_EVENT_PRESS;
        }
    } else if (action == 'm') {
        event->type = MOUSE_EVENT_RELEASE;
    } else {
        return 0;
    }
    
    return 1;
}

void mouse_handle_selection(Terminal *term, MouseEvent *event) {
    if (event->type == MOUSE_EVENT_PRESS && event->button == 0) {
        // Left button press - start selection
        selection_start_x = event->x;
        selection_start_y = event->y;
        selection_end_x = event->x;
        selection_end_y = event->y;
        selecting = 1;
    } else if (event->type == MOUSE_EVENT_DRAG && selecting) {
        // Update selection
        selection_end_x = event->x;
        selection_end_y = event->y;
    } else if (event->type == MOUSE_EVENT_RELEASE && event->button == 0) {
        // End selection
        selecting = 0;
    }
}

char* mouse_get_selection(Terminal *term) {
    if (selection_start_x < 0 || selection_start_y < 0 ||
        selection_end_x < 0 || selection_end_y < 0) {
        return NULL;
    }
    
    int start_y = selection_start_y < selection_end_y ? selection_start_y : selection_end_y;
    int end_y = selection_start_y > selection_end_y ? selection_start_y : selection_end_y;
    int start_x = selection_start_y < selection_end_y ? selection_start_x : selection_end_x;
    int end_x = selection_start_y > selection_end_y ? selection_start_x : selection_end_x;
    
    if (start_y == end_y) {
        if (start_x > end_x) {
            int tmp = start_x;
            start_x = end_x;
            end_x = tmp;
        }
    }
    
    // Calculate buffer size
    size_t size = 0;
    for (int y = start_y; y <= end_y && y < term->height; y++) {
        int sx = (y == start_y) ? start_x : 0;
        int ex = (y == end_y) ? end_x : term->width - 1;
        size += (ex - sx + 1) + 1; // +1 for newline
    }
    
    char *selection = malloc(size + 1);
    if (!selection) return NULL;
    
    char *ptr = selection;
    for (int y = start_y; y <= end_y && y < term->height; y++) {
        int sx = (y == start_y) ? start_x : 0;
        int ex = (y == end_y) ? end_x : term->width - 1;
        
        for (int x = sx; x <= ex && x < term->width; x++) {
            *ptr++ = term->buffer[y][x].ch;
        }
        if (y < end_y) {
            *ptr++ = '\n';
        }
    }
    *ptr = '\0';
    
    return selection;
}