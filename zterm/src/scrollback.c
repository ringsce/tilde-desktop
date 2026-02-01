#include "scrollback.h"
#include <stdlib.h>
#include <string.h>

void scrollback_init(Scrollback *sb, int max_lines, int width) {
    sb->max_lines = max_lines > MAX_SCROLLBACK_LINES ? MAX_SCROLLBACK_LINES : max_lines;
    sb->num_lines = 0;
    sb->width = width;
    sb->scroll_offset = 0;
    sb->lines = calloc(sb->max_lines * width, sizeof(Cell));
}

void scrollback_add_line(Scrollback *sb, const Cell *line, int width) {
    if (!sb->lines) return;

    // If buffer is full, remove oldest line
    if (sb->num_lines >= sb->max_lines) {
        memmove(sb->lines, sb->lines + sb->width,
                (sb->max_lines - 1) * sb->width * sizeof(Cell));
        sb->num_lines = sb->max_lines - 1;
    }

    // Add new line
    int copy_width = width < sb->width ? width : sb->width;
    memcpy(sb->lines + (sb->num_lines * sb->width), line, copy_width * sizeof(Cell));

    // Pad with spaces if needed
    for (int i = copy_width; i < sb->width; i++) {
        sb->lines[sb->num_lines * sb->width + i].ch = ' ';
        sb->lines[sb->num_lines * sb->width + i].fg = COLOR_DEFAULT;
        sb->lines[sb->num_lines * sb->width + i].bg = COLOR_DEFAULT;
        sb->lines[sb->num_lines * sb->width + i].bold = 0;
        sb->lines[sb->num_lines * sb->width + i].underline = 0;
    }

    sb->num_lines++;
}

void scrollback_resize(Scrollback *sb, int new_width) {
    if (new_width == sb->width) return;

    Cell *new_lines = calloc(sb->max_lines * new_width, sizeof(Cell));
    if (!new_lines) return;

    int copy_width = new_width < sb->width ? new_width : sb->width;
    for (int i = 0; i < sb->num_lines; i++) {
        memcpy(new_lines + (i * new_width),
               sb->lines + (i * sb->width),
               copy_width * sizeof(Cell));
    }

    free(sb->lines);
    sb->lines = new_lines;
    sb->width = new_width;
}

void scrollback_scroll_up(Scrollback *sb, int lines) {
    sb->scroll_offset += lines;
    if (sb->scroll_offset > sb->num_lines) {
        sb->scroll_offset = sb->num_lines;
    }
}

void scrollback_scroll_down(Scrollback *sb, int lines) {
    sb->scroll_offset -= lines;
    if (sb->scroll_offset < 0) {
        sb->scroll_offset = 0;
    }
}

void scrollback_scroll_to_bottom(Scrollback *sb) {
    sb->scroll_offset = 0;
}

void scrollback_render(Scrollback *sb, Terminal *term, int term_height) {
    if (sb->scroll_offset == 0) return;

    int start_line = sb->num_lines - sb->scroll_offset;
    if (start_line < 0) start_line = 0;

    int lines_to_show = term_height;
    if (start_line + lines_to_show > sb->num_lines) {
        lines_to_show = sb->num_lines - start_line;
    }

    for (int i = 0; i < lines_to_show; i++) {
        int sb_line = start_line + i;
        int copy_width = sb->width < term->width ? sb->width : term->width;

        memcpy(term->buffer[i],
               sb->lines + (sb_line * sb->width),
               copy_width * sizeof(Cell));
    }
}

void scrollback_free(Scrollback *sb) {
    if (sb->lines) {
        free(sb->lines);
        sb->lines = NULL;
    }
}
