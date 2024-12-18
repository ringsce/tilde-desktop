# Makefile for Tilde Project

# Compiler and flags
FPC = fpc
FLAGS = -Mobjfpc -Sh

# Directories
BIN_DIR = bin
SRC_DIR = src
API_DIR = api
TOOLS_DIR = tools
# Programs
PROGRAMS = dyld wc ls editor ringsGIT

# Default target: Compile all programs and API
all: prepare $(addprefix $(BIN_DIR)/, $(PROGRAMS)) $(BIN_DIR)/api

# Prepare bin directory
prepare:
	mkdir -p $(BIN_DIR)

# Compile each program
$(BIN_DIR)/%: $(SRC_DIR)/%.pas
	$(FPC) $(FLAGS) $< -o$@

# Compile API
$(BIN_DIR)/api: $(API_DIR)/api.pas
	$(FPC) $(FLAGS) $< -o$@

# Clean up generated files
clean:
	rm -rf $(BIN_DIR) *.o *.ppu
