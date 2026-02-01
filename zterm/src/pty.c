#include "pty.h"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <util.h>
#include <termios.h>
#include <sys/ioctl.h>

int pty_create(PTY *pty, const char *shell, int width, int height) {
    struct winsize ws = {
        .ws_row = height,
        .ws_col = width,
        .ws_xpixel = 0,
        .ws_ypixel = 0
    };
    
    pid_t pid = forkpty(&pty->master_fd, NULL, NULL, &ws);
    
    if (pid < 0) {
        return -1;
    } else if (pid == 0) {
        // Child process
        char *args[] = {(char *)shell, NULL};
        execvp(shell, args);
        exit(1);
    }
    
    pty->child_pid = pid;
    
    // Set non-blocking
    int flags = fcntl(pty->master_fd, F_GETFL, 0);
    fcntl(pty->master_fd, F_SETFL, flags | O_NONBLOCK);
    
    return 0;
}

int pty_resize(PTY *pty, int width, int height) {
    struct winsize ws = {
        .ws_row = height,
        .ws_col = width,
        .ws_xpixel = 0,
        .ws_ypixel = 0
    };
    
    return ioctl(pty->master_fd, TIOCSWINSZ, &ws);
}

ssize_t pty_read(PTY *pty, char *buffer, size_t size) {
    return read(pty->master_fd, buffer, size);
}

ssize_t pty_write(PTY *pty, const char *buffer, size_t size) {
    return write(pty->master_fd, buffer, size);
}

void pty_close(PTY *pty) {
    if (pty->master_fd >= 0) {
        close(pty->master_fd);
    }
}
