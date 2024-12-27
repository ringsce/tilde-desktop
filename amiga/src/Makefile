# Define target architecture and tools
ARCH            := aarch64
TARGET          := aarch64-all
CC              := aarch64-elf-gcc
CXX             := aarch64-elf-g++
AR              := aarch64-elf-ar
LD              := aarch64-elf-ld
OBJCOPY         := aarch64-elf-objcopy
OBJDUMP         := aarch64-elf-objdump
STRIP           := aarch64-elf-strip
SYSROOT         := /usr/local/aarch64-elf
BUILD_DIR       := build
OUTPUT_DIR      := output

# Source files
SRC_DIR         := src
SOURCES         := $(wildcard $(SRC_DIR)/**/*.c)
OBJECTS         := $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))

# Compiler and linker flags
CFLAGS          := -Wall -O2 -g -march=armv8-a -mtune=native --sysroot=$(SYSROOT)
CXXFLAGS        := $(CFLAGS)
LDFLAGS         := --sysroot=$(SYSROOT) -L$(SYSROOT)/lib

# Final binary
OUTPUT_BIN      := $(OUTPUT_DIR)/aros_$(TARGET).bin

# Rules
.PHONY: all clean

all: $(OUTPUT_BIN)

# Link the binary
$(OUTPUT_BIN): $(OBJECTS)
	@echo "Linking $@..."
	@mkdir -p $(OUTPUT_DIR)
	$(CC) $(LDFLAGS) -o $@ $^

# Compile source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@echo "Compiling $<..."
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# Clean the build directory
clean:
	@echo "Cleaning build and output directories..."
	@rm -rf $(BUILD_DIR) $(OUTPUT_DIR)
