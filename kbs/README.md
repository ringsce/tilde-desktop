# Kayte Build System

🔨 A modern, web-based IDE for building FreePascal, Lazarus, and CMake projects with automatic package distribution.

![Version](https://img.shields.io/badge/version-1.0.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)

## Features

### 🚀 Multi-Platform Build Support
- **FreePascal (FPC)**: Direct compilation of Pascal source files
- **Lazarus**: Full support for `.lpi` and `.lpr` project files via lazbuild
- **Delphi**: Build `.dpr` project files with FPC compiler
- **CMake**: Complete CMake workflow with multiple generators

### 📦 Automatic Package Distribution
After successful builds, automatically creates distribution packages:
- **macOS**: `.dmg` (Disk Image)
- **Linux**: `.tar.gz` (Compressed Archive)
- **Windows**: `.zip` (ZIP Archive)

### 🎯 Key Features
- **Auto-Detection**: Automatically detects FPC, Lazarus, and CMake installations
- **Project Explorer**: Visual file tree with build artifacts tracking
- **Build Modes**: Debug, Release, and Production configurations
- **Output Panel**: Real-time build output with color-coded messages
- **Package Management**: Download, delete, and manage distribution packages
- **Clean Builds**: Remove build artifacts and start fresh
- **Cross-Platform**: Works on macOS, Linux, and Windows

## Quick Start

### Prerequisites
- Modern web browser (Chrome, Firefox, Safari, Edge)
- FreePascal Compiler (optional, for Pascal projects)
- Lazarus IDE (optional, for Lazarus projects)
- CMake (optional, for CMake projects)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/ringsce/kayte-build-system.git
cd kayte-build-system
```

2. Open `index.html` in your web browser:
```bash
open index.html  # macOS
xdg-open index.html  # Linux
start index.html  # Windows
```

Or serve it with a local web server:
```bash
python -m http.server 8000
# Then open http://localhost:8000
```

## Usage

### Building a Project

1. **Open Project**: Click the "Open" button or use File menu
2. **Select File**: Choose your project file:
   - `.lpi` or `.lpr` for Lazarus projects
   - `.dpr` for Delphi projects
   - `CMakeLists.txt` for CMake projects
3. **Auto-Build**: The project builds automatically after loading
4. **View Output**: Check the Output tab for build progress
5. **Get Package**: Find your distribution package in the Projects tab

### Manual Build

1. Configure build settings in the "Build Configuration" tab
2. Click "Build Project" button in the toolbar
3. Or use the "Build Project" button in the MainForm tab

### Package Management

Navigate to the **Projects** tab to:
- View all created packages
- Download packages for distribution
- Delete individual packages
- Clear all packages

## Project Structure

```
kayte-build-system/
├── index.html          # Main HTML structure
├── styles.css          # All styling and layout
├── app.js              # Application logic
├── README.md           # This file
└── BUILD.md            # Build instructions
```

## Supported Build Configurations

### FreePascal (FPC)
- Compiler options: `-O2 -MObjFPC -Scghi`
- Target platforms: darwin-x86_64, darwin-aarch64, linux-x86_64, linux-aarch64, win64-x86_64
- Custom include paths support

### Lazarus (lazbuild)
- Build modes: Debug, Release, Production
- Full project dependency resolution
- Resource compilation
- Automatic unit discovery

### CMake
- Generators: Unix Makefiles, Ninja, Xcode, Visual Studio
- Build types: Debug, Release
- Configure and build in separate steps
- Out-of-source builds in `./bin/build/`

## Configuration

### Auto-Detection Paths

The system automatically searches for compilers in these locations:

**FreePascal (fpc)**:
- `/opt/homebrew/bin/fpc` (Homebrew Apple Silicon)
- `/usr/local/bin/fpc` (Homebrew Intel Mac)
- `/usr/bin/fpc` (System installation)

**Lazarus (lazbuild)**:
- `/Applications/Lazarus/lazbuild` (macOS)
- `/Applications/lazarus/lazbuild` (macOS lowercase)
- `/usr/local/bin/lazbuild` (Linux)

**CMake**:
- `/opt/homebrew/bin/cmake` (Homebrew Apple Silicon)
- `/usr/local/bin/cmake` (Homebrew Intel Mac)
- `/usr/bin/cmake` (System installation)

### Manual Configuration

You can manually set paths in the "Build Configuration" tab:
1. Enter the full path to your compiler/tool
2. Click "Detect" to verify the path
3. Save configuration

## Output Directory Structure

```
project/
├── bin/                    # Build output directory
│   ├── MyProject          # Compiled executable
│   └── build/             # CMake build directory
├── CMakeLists.txt         # CMake configuration
├── project.lpi            # Lazarus project
└── source files...
```

## Keyboard Shortcuts

- `Ctrl/Cmd + N`: New Project
- `Ctrl/Cmd + O`: Open Project
- `Ctrl/Cmd + S`: Save Project
- `Ctrl/Cmd + B`: Build Project

## Troubleshooting

### Build Fails - Compiler Not Found
1. Check that the compiler is installed
2. Verify the path in "Build Configuration"
3. Click "Detect" to auto-detect the compiler

### Package Creation Fails
1. Ensure the build completed successfully
2. Check that build artifacts exist in `./bin/`
3. Verify output directory permissions

### Project Explorer Not Updating
1. Refresh the browser
2. Check browser console for errors
3. Ensure JavaScript is enabled

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- FreePascal Team for the excellent compiler
- Lazarus IDE developers
- CMake development team
- All contributors to this project

## Support

- 📧 Email: support@kaytebuild.dev
- 📚 Documentation: https://docs.kaytebuild.dev
- 💬 Discord: Join our community
- ⭐ GitHub: Star the project

## Roadmap

- [ ] Remote build support
- [ ] Build history and logs
- [ ] Custom build scripts
- [ ] Plugin system
- [X] Dark/Light theme toggle
- [ ] Multi-language support
- [ ] Git integration
- [ ] Code editor integration

---

Made with ❤️ by the Kayte Build System Team