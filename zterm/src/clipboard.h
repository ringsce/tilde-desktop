#ifndef CLIPBOARD_H
#define CLIPBOARD_H

#ifdef __cplusplus
extern "C" {
#endif

    void clipboard_set(const char *text);
    char* clipboard_get(void);

#ifdef __cplusplus
}
#endif

#endif