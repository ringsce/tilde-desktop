#ifndef PTY_H
#define PTY_H

#include <sys/types.h>

typedef struct {
    int master_fd;
    int slave_fd;
    pid_t child_pid;
} PTY;

int pty_create(PTY *pty, const char *shell, int width, int height);
int pty_resize(PTY *pty, int width, int height);
ssize_t pty_read(PTY *pty, char *buffer, size_t size);
ssize_t pty_write(PTY *pty, const char *buffer, size_t size);
void pty_close(PTY *pty);

#endif
