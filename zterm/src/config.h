#ifndef CONFIG_H
#define CONFIG_H

#include <stddef.h>

#define MAX_FONT_NAME 64
#define MAX_SHELL_PATH 256
#define MAX_COLOR_SCHEME_NAME 32

typedef struct {
    // Appearance
    char font_name[MAX_FONT_NAME];
    int font_size;
    int use_bold;
    int use_italic;

    // Colors
    char color_scheme[MAX_COLOR_SCHEME_NAME];
    int foreground_color;
    int background_color;

    // Behavior
    char shell[MAX_SHELL_PATH];
    int scrollback_lines;
    int tab_width;
    int bell_enabled;

    // Performance
    int refresh_rate_ms;
    int double_click_ms;

    // Features
    int mouse_enabled;
    int clipboard_enabled;
    int search_enabled;
} Config;

void config_init(Config *config);
int config_load(Config *config, const char *path);
int config_save(Config *config, const char *path);
void config_get_default_path(char *path, size_t size);

#endif
