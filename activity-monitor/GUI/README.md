# System Activity Monitor

A cross-platform system activity monitor with multiple implementations: Terminal UI (Pascal), GUI (Lazarus/fpGUI), and Qt6 GUI (C++).

![Activity Monitor Screenshot](screenshot.png)

## Features

- 📊 **Real-time CPU usage monitoring** with history graph
- 💾 **Memory usage tracking** with detailed statistics
- 📋 **Process list viewer** with filtering
- 🎨 **Multiple UI implementations** (TUI, GUI, Qt6)
- 🖥️ **Cross-platform support** (Windows, macOS, Linux)
- ⚡ **Auto-refresh** every 2 seconds
- 🎯 **Color-coded indicators** (green/yellow/red based on usage)

## Available Versions

### 1. Terminal UI (Pascal/Free Pascal)
A beautiful terminal-based interface with box-drawing characters and color support.

**Location:** `activitymonitortui.lpr`

**Features:**
- Beautiful ASCII art interface
- Color-coded progress bars
- CPU history graph in the terminal
- Dashboard and Processes views
- Works in any terminal

### 2. Qt6 GUI (C++)
Modern Qt6-based graphical interface with smooth animations.

**Location:** `main.cpp`

**Features:**
- Modern, clean Qt6 interface
- Smooth animated CPU graph
- Styled progress bars
- Material design-inspired UI
- Native look and feel

## Requirements

### Terminal UI (Pascal)
- Free Pascal Compiler (FPC) 3.2.2 or newer
- Lazarus IDE (optional, for compilation)

### Qt6 GUI (C++)
- Qt6 (6.2 or newer)
- C++ compiler with C++17 support
- CMake 3.16+ or qmake

## Installation

### Installing Dependencies

#### macOS
```bash
# For Terminal UI
brew install fpc lazarus

# For Qt6 GUI
brew install qt6
```

#### Linux (Ubuntu/Debian)
```bash
# For Terminal UI
sudo apt install fp-compiler lazarus

# For Qt6 GUI
sudo apt install qt6-base-dev cmake
```

#### Linux (Fedora)
```bash
# For Terminal UI
sudo dnf install fpc lazarus

# For Qt6 GUI
sudo dnf install qt6-qtbase-devel cmake
```

#### Windows
- **Terminal UI:** Download Free Pascal from [freepascal.org](https://www.freepascal.org/)
- **Qt6 GUI:** Download Qt6 from [qt.io](https://www.qt.io/download)

## Building & Running

### Terminal UI (Pascal)

#### Using Lazarus IDE:
1. Open Lazarus IDE
2. Open `activitymonitortui.lpr`
3. Press F9 to compile and run

#### Using Command Line:
```bash
cd GUI
fpc activitymonitortui.lpr
./activitymonitortui
```

**Usage:**
- Press `1` or `ENTER` to refresh dashboard
- Press `2` to view processes
- Press `Q` to quit

### Qt6 GUI (C++)

#### Using CMake:
```bash
cd GUI
mkdir build && cd build
cmake ..
make
./ActivityMonitor
```

#### Using qmake:
```bash
cd GUI

# Create project file
cat > ActivityMonitor.pro << 'EOF'
QT += core gui widgets
CONFIG += c++17
SOURCES += main.cpp
TARGET = ActivityMonitor
EOF

# Build
qmake6 ActivityMonitor.pro
make
./ActivityMonitor
```

## Usage

### Terminal UI
The Terminal UI provides an interactive dashboard that updates on user input:

- **Dashboard View:** Shows CPU usage, memory usage, and CPU history graph
- **Processes View:** Lists all running processes (filtered kernel threads)
- **Navigation:** Use number keys (1, 2) to switch views
- **Refresh:** Press ENTER to update statistics
- **Exit:** Press Q to quit

### Qt6 GUI
The Qt6 GUI provides a modern graphical interface:

- **Overview Tab:** Real-time CPU and memory monitoring with animated graph
- **Processes Tab:** Scrollable list of running processes
- **Auto-refresh:** Automatically updates every 2 seconds
- **Manual refresh:** Click "Refresh" button to update immediately

## Platform-Specific Notes

### Windows
- Uses Windows API (`GetSystemTimes`, `GlobalMemoryStatusEx`)
- Process list via `tasklist` command
- Full CPU and memory statistics available

### Linux
- Reads from `/proc/stat` for CPU usage
- Reads from `/proc/meminfo` for memory statistics
- Process list via `ps aux` command
- Filters kernel threads automatically

### macOS
- Uses same `/proc` approach as Linux (limited support)
- Process list via `ps aux` command
- **Note:** macOS doesn't have `/proc/stat` and `/proc/meminfo`, so stats show as 0%
- For full macOS support, native APIs (Mach) would be needed

## Architecture

### Terminal UI (Pascal)
```
activitymonitortui.lpr
├── TSystemMonitor class
│   ├── GetCPUUsage()
│   ├── GetMemoryUsage()
│   ├── GetProcessList()
│   ├── ShowDashboard()
│   └── ShowProcesses()
└── Platform-specific implementations
    ├── Windows (WinAPI)
    └── Unix/Linux (/proc filesystem)
```

### Qt6 GUI (C++)
```
main.cpp
├── MainWindow (QMainWindow)
│   ├── Overview widget
│   ├── Processes widget
│   └── CPUGraphWidget
├── SystemMonitor class
│   ├── getCPUUsage()
│   ├── getMemoryUsage()
│   └── getProcessList()
└── Platform-specific code
    ├── Q_OS_WIN (Windows API)
    └── Unix/Linux (/proc)
```

## Troubleshooting

### Terminal UI shows no CPU/Memory stats
- **Linux:** Ensure `/proc/stat` and `/proc/meminfo` are accessible
- **macOS:** Stats will show 0% (requires native API implementation)
- **Windows:** Ensure the program has proper permissions

### Qt6 compilation errors
- Ensure Qt6 is properly installed: `qmake6 --version`
- Check CMake can find Qt6: `cmake .. -DQt6_DIR=/path/to/qt6`
- On macOS with Homebrew: `export CMAKE_PREFIX_PATH=/opt/homebrew/opt/qt6`

### Process list not showing
- **Linux/macOS:** Ensure `/bin/ps` is accessible
- **Windows:** Ensure `tasklist` is in PATH
- Try running with elevated permissions if needed

### Lazarus linking errors on macOS ARM
- The Terminal UI version avoids all LCL/Cocoa dependencies
- Use the pure console version (`activitymonitortui.lpr`)
- GUI versions may have issues with older Lazarus on Apple Silicon

## Contributing

Contributions are welcome! Areas for improvement:

1. **macOS native support:** Implement CPU/Memory reading using Mach APIs
2. **Additional metrics:** Disk I/O, network usage, temperature sensors
3. **Process management:** Kill processes, change priority
4. **System tray:** Minimize to system tray with notifications
5. **Themes:** Dark mode, custom color schemes
6. **Export:** Save statistics to CSV/JSON

## License

This project is provided as-is for educational and personal use.

## Credits

Created as a cross-platform system monitoring tool demonstrating:
- Free Pascal/Lazarus development
- Qt6/C++ GUI programming
- Cross-platform system programming
- Terminal UI design

## Version History

- **v1.0.0** (2024-12-31)
  - Initial release
  - Terminal UI (Pascal/Free Pascal)
  - Qt6 GUI (C++)
  - Cross-platform support (Windows, Linux, macOS)
  - Real-time CPU and memory monitoring
  - Process list viewer

## Contact & Support

For issues, questions, or contributions, please open an issue on the project repository.

---

**Enjoy monitoring your system! 🚀**
