# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -O2
LDFLAGS = -ldos -lexec -lzip -lintuition

# Targets
TARGETS = main window

# Source files
SRCS = src/main.c src/window.c

# Object files
OBJS = $(SRCS:.c=.o)

# Default target
all: $(TARGETS)

# Build each target
main: main.o
	$(CC) $(CFLAGS) -o main main.o $(LDFLAGS)

window: window.o
	$(CC) $(CFLAGS) -o window window.o $(LDFLAGS)

# Compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build artifacts
clean:
	rm -f $(OBJS) $(TARGETS)

# Run the main program
run-main: main
	./main

# Run the window program
run-window: window
	./window

# Phony targets
.PHONY: all clean run-main run-window
