# Z-Terminal

A lightweight terminal emulator for macOS Silicon written in ANSI C with full ANSI color support, multiple tabs, mouse support, clipboard integration, scrollback, and search.

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Platform](https://img.shields.io/badge/platform-macOS%20Silicon-lightgrey.svg)
![Language](https://img.shields.io/badge/language-C11-orange.svg)
![Version](https://img.shields.io/badge/version-2.0-green.svg)

## Features

- ✅ Native macOS Silicon (ARM64) support
- 🎨 Full ANSI color support (16 colors: standard + bright)
- ⚡ Lightweight and fast
- 🖥️ PTY (pseudo-terminal) implementation
- 📐 Dynamic terminal resizing (SIGWINCH)
- 🎯 Bold and underline text support
- 🖱️ Mouse support (selection, scroll wheel, click)
- 📋 Native macOS clipboard integration (copy/paste)
- 🗂️ Multiple tabs with independent shell sessions (up to 10)
- 🔧 INI-style configuration file (`~/.zterminalrc`)
- 📜 Scrollback buffer (configurable, default 10,000 lines)
- 🔍 Incremental search with highlighted matches
- 📦 Built with CMake

---

## Prerequisites

- macOS (Apple Silicon / M1 or later recommended)
- Xcode Command Line Tools
- CMake 3.20 or higher

### Installing Prerequisites

```bash
# Install Xcode Command Line Tools
xcode-select --install

# Install CMake (via Homebrew)
brew install cmake
```

---

## Building

```bash
# Clone the repository (or navigate to your project directory)
cd zterminal

# Create build directory
mkdir build && cd build

# Configure with CMake
cmake ..

# Build the project
cmake --build .
```

### Debug Build

```bash
cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
cmake --build .
```

### Verbose Output

```bash
cmake --build . --verbose
```

---

## Usage

```bash
# Run from the build directory
./zterminal

# Or install to a system path (optional)
sudo cp zterminal /usr/local/bin/
zterminal
```

On first run, Z-Terminal auto-generates a default configuration file at `~/.zterminalrc`.

---

## Key Bindings

### Tabs

| Shortcut | Action |
|---|---|
| `Ctrl+T` | New tab |
| `Ctrl+W` | Close current tab |
| `Ctrl+]` | Switch to next tab |
| `Ctrl+[` | Switch to previous tab |

### Clipboard

| Shortcut | Action |
|---|---|
| `Ctrl+C` | Copy selected text to clipboard |
| `Ctrl+V` | Paste from clipboard |
| Middle-click | Copy selection to clipboard |

### Search

| Shortcut | Action |
|---|---|
| `Ctrl+F` | Open search bar |
| Type | Incremental live search |
| `n` | Next match |
| `p` | Previous match |
| `Enter` | Close search bar (keep highlights) |
| `Escape` | Cancel search |

### Scrollback

| Shortcut | Action |
|---|---|
| `Page Up` | Scroll up one page |
| `Page Down` | Scroll down one page |
| `Ctrl+S` | Toggle scroll mode |
| Mouse wheel | Scroll up/down (3 lines per tick) |

---

## Testing Colors

Once running, try these commands to verify color support:

```bash
# List files with colors
ls --color=auto

# Test foreground colors
echo -e "\033[31mRed\033[0m \033[32mGreen\033[0m \033[34mBlue\033[0m"

# Test bold and underline
echo -e "\033[1;33mBold Yellow\033[0m \033[4;36mUnderlined Cyan\033[0m"

# Test background colors
echo -e "\033[41;37mWhite on Red\033[0m \033[44;93mBright Yellow on Blue\033[0m"

# Run colorful programs
htop
vim
neofetch
```

---

## Configuration

Z-Terminal loads its configuration from `~/.zterminalrc` on startup. If the file does not exist, a default one is created automatically.

### Full Configuration Reference

```ini
# Z-Terminal Configuration File
# Automatically loaded from ~/.zterminalrc

# ─── Appearance ─────────────────────────────
font_name = Monaco
font_size = 12
use_bold = 1
use_italic = 0

# ─── Colors ─────────────────────────────────
color_scheme = default
foreground_color = 7       # 0-15 ANSI color index (7 = white)
background_color = 0       # 0-15 ANSI color index (0 = black)

# ─── Behavior ───────────────────────────────
shell = /bin/zsh           # Shell to launch (defaults to $SHELL)
scrollback_lines = 10000   # Max scrollback history (up to 50000)
tab_width = 8              # Tab character width
bell_enabled = 1           # Audible bell on/off

# ─── Performance ────────────────────────────
refresh_rate_ms = 10       # Render loop interval in milliseconds
double_click_ms = 500      # Double-click detection window (ms)

# ─── Features ───────────────────────────────
mouse_enabled = 1          # Mouse support on/off
clipboard_enabled = 1      # Clipboard integration on/off
search_enabled = 1         # Search functionality on/off
```

### Applying Changes

Configuration is read at startup. To apply changes, restart Z-Terminal:

```bash
# Edit config
nano ~/.zterminalrc

# Restart
./zterminal
```

---

## Project Structure

```
zterminal/
├── CMakeLists.txt              # CMake build configuration
├── README.md                   # This file
└── src/
    ├── main.c                  # Entry point, event loop, input handling
    ├── terminal.c / .h         # Terminal buffer, ANSI parser, rendering
    ├── pty.c / .h              # PTY creation, read/write, resize
    ├── tabs.c / .h             # Tab manager (create, switch, close)
    ├── mouse.c / .h            # Mouse event parsing and text selection
    ├── clipboard.m / .h        # macOS clipboard via Cocoa (Objective-C)
    ├── config.c / .h           # INI config file load/save
    ├── scrollback.c / .h       # Scrollback buffer management
    └── search.c / .h           # Incremental search and highlighting
```

---

## Architecture

### Components

1. **PTY Module** (`pty.c`, `pty.h`)
   - Creates and manages pseudo-terminal pairs using `forkpty()`
   - Handles child process forking for shell execution
   - Provides non-blocking read/write interface
   - Supports runtime window size updates via `TIOCSWINSZ`

2. **Terminal Module** (`terminal.c`, `terminal.h`)
   - Cell-based buffer with per-character color and attribute storage
   - ANSI/VT100 escape sequence parser (SGR, cursor, erase)
   - Efficient diff-based rendering (only emits escape codes on attribute change)
   - Dynamic width/height with cursor clamping on resize

3. **Tab Manager** (`tabs.c`, `tabs.h`)
   - Manages up to 10 independent tabs
   - Each tab owns its own `Terminal`, `PTY`, `Scrollback`, and `Search` state
   - Renders a colored tab bar at the top of the screen

4. **Mouse Module** (`mouse.c`, `mouse.h`)
   - Enables SGR extended mouse tracking (`?1006h`)
   - Parses mouse press, release, drag, and scroll events
   - Tracks click-and-drag text selection regions

5. **Clipboard Module** (`clipboard.m`, `clipboard.h`)
   - Written in Objective-C to access macOS `NSPasteboard`
   - `clipboard_set()` / `clipboard_get()` for system clipboard access
   - Works with any app on macOS (copy in Z-Terminal, paste in Safari, etc.)

6. **Configuration Module** (`config.c`, `config.h`)
   - Parses simple `key = value` INI format
   - Supports inline `#` comments
   - Auto-generates default config on first run
   - Exposes all tunable settings via the `Config` struct

7. **Scrollback Module** (`scrollback.c`, `scrollback.h`)
   - Ring-buffer style storage (up to 50,000 lines)
   - Lines are saved as full `Cell` arrays (preserving color and attributes)
   - Supports scroll offset, resize reflow, and rendering into the terminal buffer

8. **Search Module** (`search.c`, `search.h`)
   - Incremental search across both visible buffer and scrollback
   - Case-insensitive by default
   - Stores all match positions; highlights current match in yellow, others in cyan
   - Navigation with next/previous cycling

9. **Main Loop** (`main.c`)
   - Raw mode terminal setup with `termios`
   - `SIGWINCH` signal handler for live resize
   - Unified input dispatch: PTY output, keyboard shortcuts, mouse events, search input
   - Coordinates rendering of tab bar, search bar, scrollback, and terminal buffer

---

## ANSI Color Support

Z-Terminal supports the following ANSI SGR (Select Graphic Rendition) codes:

### Foreground Colors

| Codes | Colors |
|---|---|
| `30-37` | Black, Red, Green, Yellow, Blue, Magenta, Cyan, White |
| `90-97` | Bright variants of the above |
| `39` | Default foreground |

### Background Colors

| Codes | Colors |
|---|---|
| `40-47` | Standard background colors |
| `100-107` | Bright background colors |
| `49` | Default background |

### Text Attributes

| Code | Attribute |
|---|---|
| `0` | Reset all |
| `1` | Bold |
| `4` | Underline |
| `22` | Normal intensity |
| `24` | Not underlined |

### Example Escape Sequences

```c
"\033[31m"        // Red foreground
"\033[1;32m"      // Bold green
"\033[4;34m"      // Underlined blue
"\033[41;37m"     // White text on red background
"\033[0m"         // Reset all attributes
```

---

## Troubleshooting

### Build Fails

```bash
# Clean and rebuild from scratch
rm -rf build
mkdir build && cd build
cmake ..
cmake --build .
```

### `unknown type name 'size_t'`

Make sure every `.h` file that uses `size_t` includes `<stddef.h>` at the top:

```c
#include <stddef.h>
```

### Objective-C / Clipboard Errors

The `clipboard.m` file requires Objective-C support. Ensure your `CMakeLists.txt` declares `ObjC` in the project languages and sets the file's language property:

```cmake
project(zterminal C CXX ObjC)
set_source_files_properties(src/clipboard.m PROPERTIES LANGUAGE ObjC)
```

Do not use `-pedantic` on Objective-C files. Apply strict flags only to your C sources:

```cmake
set_source_files_properties(${SOURCES} PROPERTIES COMPILE_FLAGS "-Wall -Wextra -pedantic")
```

### PTY Creation Fails

Ensure you have proper permissions and the shell path is valid:

```bash
# Check default shell
echo $SHELL

# Verify shell exists
which zsh
which bash
```

### Colors Not Showing

Ensure your shell is configured for colors:

```bash
# For zsh — add to ~/.zshrc
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

# For bash — add to ~/.bash_profile
export CLICOLOR=1
```

### Mouse Not Working

Mouse support requires SGR extended tracking. If your outer terminal overrides mouse mode, try running Z-Terminal in a plain `Terminal.app` window. You can also disable mouse in config if it causes issues:

```ini
mouse_enabled = 0
```

---

## Performance

Z-Terminal is designed to be lightweight:

- Memory footprint: ~500KB base + scrollback buffer size
- CPU usage: <1% idle, tunable via `refresh_rate_ms` in config
- No external runtime dependencies beyond macOS system frameworks (Cocoa, CoreFoundation, AppKit)

---

## Contributing

Contributions are welcome! Remaining areas for improvement:

- [ ] Extended ANSI sequence support (256 colors, 24-bit true color)
- [ ] Scrollback search navigation (jump scroll position to match)
- [ ] Double-click word selection
- [ ] Tab title auto-update from shell escape sequences
- [ ] Persistent session save/restore
- [ ] Split panes (horizontal / vertical)
- [ ] Theme presets (solarized, nord, gruvbox, etc.)
- [ ] Configuration hot-reload (no restart required)

---

## License

MIT License

Copyright (c) 2026

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---

## Acknowledgments

- Built for macOS using native PTY support via `util.h`
- ANSI escape sequence handling based on VT100/xterm standards
- Clipboard integration via macOS Cocoa `NSPasteboard`
- Inspired by classic terminal emulators

---

**Made with ❤️ for macOS Silicon**
