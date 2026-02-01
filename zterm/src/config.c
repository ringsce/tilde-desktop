#include "config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>

void config_init(Config *config) {
    // Appearance defaults
    strncpy(config->font_name, "Monaco", MAX_FONT_NAME - 1);
    config->font_size = 12;
    config->use_bold = 1;
    config->use_italic = 0;

    // Color defaults
    strncpy(config->color_scheme, "default", MAX_COLOR_SCHEME_NAME - 1);
    config->foreground_color = 7;  // White
    config->background_color = 0;  // Black

    // Behavior defaults
    const char *shell = getenv("SHELL");
    strncpy(config->shell, shell ? shell : "/bin/zsh", MAX_SHELL_PATH - 1);
    config->scrollback_lines = 10000;
    config->tab_width = 8;
    config->bell_enabled = 1;

    // Performance defaults
    config->refresh_rate_ms = 10;
    config->double_click_ms = 500;

    // Features defaults
    config->mouse_enabled = 1;
    config->clipboard_enabled = 1;
    config->search_enabled = 1;
}

void config_get_default_path(char *path, size_t size) {
    const char *home = getenv("HOME");
    if (home) {
        snprintf(path, size, "%s/.zterminalrc", home);
    } else {
        strncpy(path, ".zterminalrc", size - 1);
    }
}

int config_load(Config *config, const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) {
        return -1;
    }

    char line[512];
    while (fgets(line, sizeof(line), f)) {
        // Remove comments
        char *comment = strchr(line, '#');
        if (comment) *comment = '\0';

        // Parse key=value
        char key[128], value[256];
        if (sscanf(line, " %127[^=] = %255[^\n]", key, value) == 2) {
            // Trim whitespace from value
            char *v = value;
            while (*v == ' ' || *v == '\t') v++;
            char *end = v + strlen(v) - 1;
            while (end > v && (*end == ' ' || *end == '\t' || *end == '\n')) {
                *end = '\0';
                end--;
            }

            // Match configuration options
            if (strcmp(key, "font_name") == 0) {
                strncpy(config->font_name, v, MAX_FONT_NAME - 1);
            } else if (strcmp(key, "font_size") == 0) {
                config->font_size = atoi(v);
            } else if (strcmp(key, "use_bold") == 0) {
                config->use_bold = atoi(v);
            } else if (strcmp(key, "use_italic") == 0) {
                config->use_italic = atoi(v);
            } else if (strcmp(key, "color_scheme") == 0) {
                strncpy(config->color_scheme, v, MAX_COLOR_SCHEME_NAME - 1);
            } else if (strcmp(key, "foreground_color") == 0) {
                config->foreground_color = atoi(v);
            } else if (strcmp(key, "background_color") == 0) {
                config->background_color = atoi(v);
            } else if (strcmp(key, "shell") == 0) {
                strncpy(config->shell, v, MAX_SHELL_PATH - 1);
            } else if (strcmp(key, "scrollback_lines") == 0) {
                config->scrollback_lines = atoi(v);
            } else if (strcmp(key, "tab_width") == 0) {
                config->tab_width = atoi(v);
            } else if (strcmp(key, "bell_enabled") == 0) {
                config->bell_enabled = atoi(v);
            } else if (strcmp(key, "refresh_rate_ms") == 0) {
                config->refresh_rate_ms = atoi(v);
            } else if (strcmp(key, "double_click_ms") == 0) {
                config->double_click_ms = atoi(v);
            } else if (strcmp(key, "mouse_enabled") == 0) {
                config->mouse_enabled = atoi(v);
            } else if (strcmp(key, "clipboard_enabled") == 0) {
                config->clipboard_enabled = atoi(v);
            } else if (strcmp(key, "search_enabled") == 0) {
                config->search_enabled = atoi(v);
            }
        }
    }

    fclose(f);
    return 0;
}

int config_save(Config *config, const char *path) {
    FILE *f = fopen(path, "w");
    if (!f) {
        return -1;
    }

    fprintf(f, "# Z-Terminal Configuration File\n");
    fprintf(f, "# This file is automatically loaded from ~/.zterminalrc\n\n");

    fprintf(f, "# Appearance\n");
    fprintf(f, "font_name = %s\n", config->font_name);
    fprintf(f, "font_size = %d\n", config->font_size);
    fprintf(f, "use_bold = %d\n", config->use_bold);
    fprintf(f, "use_italic = %d\n\n", config->use_italic);

    fprintf(f, "# Colors\n");
    fprintf(f, "color_scheme = %s\n", config->color_scheme);
    fprintf(f, "foreground_color = %d\n", config->foreground_color);
    fprintf(f, "background_color = %d\n\n", config->background_color);

    fprintf(f, "# Behavior\n");
    fprintf(f, "shell = %s\n", config->shell);
    fprintf(f, "scrollback_lines = %d\n", config->scrollback_lines);
    fprintf(f, "tab_width = %d\n", config->tab_width);
    fprintf(f, "bell_enabled = %d\n\n", config->bell_enabled);

    fprintf(f, "# Performance\n");
    fprintf(f, "refresh_rate_ms = %d\n", config->refresh_rate_ms);
    fprintf(f, "double_click_ms = %d\n\n", config->double_click_ms);

    fprintf(f, "# Features\n");
    fprintf(f, "mouse_enabled = %d\n", config->mouse_enabled);
    fprintf(f, "clipboard_enabled = %d\n", config->clipboard_enabled);
    fprintf(f, "search_enabled = %d\n", config->search_enabled);

    fclose(f);
    return 0;
}
