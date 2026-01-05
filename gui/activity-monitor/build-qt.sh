#!/bin/bash
# Setup script for Qt6 Windows ARM64 cross-compilation on macOS
set -e

echo "🔧 Setting up Qt6 Windows ARM64 cross-compilation environment..."

# Configuration
LLVM_MINGW_VERSION="20231128"
LLVM_MINGW_URL="https://github.com/mstorsjo/llvm-mingw/releases/download/${LLVM_MINGW_VERSION}/llvm-mingw-${LLVM_MINGW_VERSION}-ucrt-macos-universal.tar.xz"
LLVM_MINGW_DIR="$HOME/llvm-mingw"
QT6_WINDOWS_ARM64_DIR="$HOME/qt6-win-arm64"

echo ""
echo "This script will:"
echo "1. Download and install llvm-mingw for macOS"
echo "2. Guide you to copy Qt6 ARM64 binaries from Windows"
echo ""

# Step 1: Download llvm-mingw
if [ ! -d "$LLVM_MINGW_DIR" ]; then
    echo "📥 Step 1: Downloading llvm-mingw for macOS..."
    cd "$HOME"
    curl -L -o llvm-mingw.tar.xz "$LLVM_MINGW_URL"
    
    echo "📦 Extracting llvm-mingw..."
    tar xf llvm-mingw.tar.xz
    mv "llvm-mingw-${LLVM_MINGW_VERSION}-ucrt-macos-universal" "$LLVM_MINGW_DIR"
    rm llvm-mingw.tar.xz
    
    # Remove quarantine attributes on macOS
    echo "🔓 Removing quarantine attributes..."
    xattr -rd com.apple.quarantine "$LLVM_MINGW_DIR" 2>/dev/null || true
    
    echo "✅ llvm-mingw installed to: $LLVM_MINGW_DIR"
else
    echo "✅ llvm-mingw already installed at: $LLVM_MINGW_DIR"
fi

# Step 2: Instructions for copying Qt6 from Windows
echo ""
echo "📦 Step 2: Copy Qt6 for Windows ARM64"
echo ""
echo "On your Windows 11 ARM64 device:"
echo "1. Download Qt6 using the Qt Online Installer:"
echo "   https://www.qt.io/download-qt-installer"
echo ""
echo "2. Install Qt 6.7.x for Windows ARM64"
echo "   - Select: Qt 6.7.x → MSVC 2022 ARM64"
echo "   - Installation path will be something like: C:\\Qt\\6.7.3\\msvc2022_arm64"
echo ""
echo "3. Copy the entire Qt installation to your Mac:"
echo "   From Windows: C:\\Qt\\6.7.3\\msvc2022_arm64"
echo "   To Mac: $QT6_WINDOWS_ARM64_DIR"
echo ""
echo "   You can use:"
echo "   - Network share/file transfer"
echo "   - USB drive"
echo "   - Cloud storage (OneDrive, Google Drive, etc.)"
echo ""

if [ ! -d "$QT6_WINDOWS_ARM64_DIR" ]; then
    echo "⚠️  Qt6 Windows ARM64 binaries not found at: $QT6_WINDOWS_ARM64_DIR"
    echo ""
    echo "After copying Qt6 from Windows, create the directory:"
    echo "  mkdir -p $QT6_WINDOWS_ARM64_DIR"
    echo ""
    echo "Then copy the contents of msvc2022_arm64 folder there."
    echo ""
    read -p "Press Enter when you've copied Qt6, or Ctrl+C to exit..."
fi

# Verify Qt6 structure
if [ -d "$QT6_WINDOWS_ARM64_DIR" ]; then
    if [ ! -d "$QT6_WINDOWS_ARM64_DIR/lib/cmake/Qt6" ]; then
        echo "⚠️  Warning: Qt6 cmake configs not found"
        echo "   Expected: $QT6_WINDOWS_ARM64_DIR/lib/cmake/Qt6"
        echo ""
        echo "Make sure you copied the complete Qt installation folder"
    else
        echo "✅ Qt6 Windows ARM64 found at: $QT6_WINDOWS_ARM64_DIR"
    fi
fi

# Step 3: Create a build script
echo ""
echo "📝 Creating build configuration..."

cat > "$HOME/build-with-qt6-arm64.sh" << 'EOF'
#!/bin/bash
# Build script for your project using Qt6 Windows ARM64

set -e

LLVM_MINGW="$HOME/llvm-mingw"
QT6_WIN_ARM64="$HOME/Qt6-windows-arm64"
WINSDK="/Users/pedro/winsdk-arm64"

# Add llvm-mingw to PATH
export PATH="$LLVM_MINGW/bin:$PATH"

# Set Qt6 path
export QT6_WIN_ARM64

# Set compiler to use llvm-mingw
export CC="$LLVM_MINGW/bin/clang"
export CXX="$LLVM_MINGW/bin/clang++"
export AR="$LLVM_MINGW/bin/llvm-ar"
export RANLIB="$LLVM_MINGW/bin/llvm-ranlib"

# Target triple for Windows ARM64
export TARGET_TRIPLE="aarch64-w64-mingw32"

echo "🏗️  Building with llvm-mingw for Windows ARM64..."
echo "📦 Qt6: $QT6_WIN_ARM64"
echo "🔧 Toolchain: $LLVM_MINGW"

# Your project build directory
cd "$(dirname "$0")/GUI"

# Clean previous build
rm -rf build-windows-arm64
mkdir -p build-windows-arm64
cd build-windows-arm64

# Configure with CMake
cmake .. \
    -GNinja \
    -DCMAKE_SYSTEM_NAME=Windows \
    -DCMAKE_SYSTEM_PROCESSOR=ARM64 \
    -DCMAKE_C_COMPILER="$LLVM_MINGW/bin/$TARGET_TRIPLE-clang" \
    -DCMAKE_CXX_COMPILER="$LLVM_MINGW/bin/$TARGET_TRIPLE-clang++" \
    -DCMAKE_RC_COMPILER="$LLVM_MINGW/bin/$TARGET_TRIPLE-windres" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_PREFIX_PATH="$QT6_WIN_ARM64" \
    -DQt6_DIR="$QT6_WIN_ARM64/lib/cmake/Qt6" \
    -DUSE_PHNT=ON

# Build
cmake --build . --parallel $(sysctl -n hw.ncpu)

echo ""
echo "✅ Build complete!"
echo "📦 Output: build-windows-arm64/ActivityMonitor.exe"
EOF

chmod +x "$HOME/build-with-qt6-arm64.sh"

echo ""
echo "✅ Setup complete!"
echo ""
echo "📋 Summary:"
echo "   llvm-mingw: $LLVM_MINGW_DIR"
echo "   Qt6 ARM64:  $QT6_WINDOWS_ARM64_DIR"
echo "   Build script: $HOME/build-with-qt6-arm64.sh"
echo ""
echo "📖 Next steps:"
echo "1. Make sure Qt6 Windows ARM64 binaries are copied to:"
echo "   $QT6_WINDOWS_ARM64_DIR"
echo ""
echo "2. Run the build script:"
echo "   $HOME/build-with-qt6-arm64.sh"
echo ""
