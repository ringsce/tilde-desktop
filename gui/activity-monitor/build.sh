#!/bin/bash
# Build script for cross-compiling ActivityMonitor for Windows ARM64 on macOS

set -e

echo "🏗️  Building ActivityMonitor for Windows ARM64..."

# Use only /usr/local/llvm/bin
LLVM_BIN="/usr/local/llvm/bin"

if [ ! -d "$LLVM_BIN" ]; then
    echo "❌ Error: LLVM not found at $LLVM_BIN"
    echo ""
    echo "Please compile LLVM from source and install to /usr/local/llvm"
    echo ""
    echo "Quick install steps:"
    echo "  git clone --depth 1 --branch release/18.x https://github.com/llvm/llvm-project.git"
    echo "  cd llvm-project && mkdir build && cd build"
    echo "  cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local/llvm \\"
    echo "        -DLLVM_ENABLE_PROJECTS=\"clang;lld\" -DLLVM_TARGETS_TO_BUILD=\"AArch64;X86\" ../llvm"
    echo "  ninja && sudo ninja install"
    exit 1
fi

export PATH="$LLVM_BIN:$PATH"
echo "✅ Using LLVM at: $LLVM_BIN"

# Check for clang-cl
if ! command -v clang-cl &> /dev/null; then
    echo "❌ Error: clang-cl not found."
    echo ""
    echo "Please install LLVM:"
    echo "  brew install llvm"
    echo ""
    echo "Then add to your PATH:"
    echo "  export PATH=\"$LLVM_BIN:\$PATH\""
    exit 1
fi

echo "✅ Found clang-cl: $(which clang-cl)"

# Check for lld-link, create symlink if needed
if ! command -v lld-link &> /dev/null; then
    echo "⚠️  lld-link not found, checking for ld.lld..."
    
    if command -v ld.lld &> /dev/null; then
        echo "✅ Found ld.lld: $(which ld.lld)"
        echo "🔗 Creating lld-link symlink..."
        
        # Create symlink in the LLVM bin directory
        LLD_PATH=$(which ld.lld)
        LLD_LINK_PATH="$LLVM_BIN/lld-link"
        
        if [ -w "$LLVM_BIN" ]; then
            ln -sf "$LLD_PATH" "$LLD_LINK_PATH"
            echo "✅ Created symlink: $LLD_LINK_PATH -> $LLD_PATH"
        else
            echo "🔐 Need sudo to create symlink in $LLVM_BIN"
            sudo ln -sf "$LLD_PATH" "$LLD_LINK_PATH"
            echo "✅ Created symlink: $LLD_LINK_PATH -> $LLD_PATH"
        fi
        
        # Verify the symlink works
        if command -v lld-link &> /dev/null; then
            echo "✅ lld-link is now available"
        else
            echo "❌ Failed to create working lld-link symlink"
            exit 1
        fi
    else
        echo "❌ Error: Neither lld-link nor ld.lld found."
        echo ""
        echo "Please install LLVM with LLD:"
        echo "  brew install llvm"
        echo ""
        echo "Or compile LLVM from source with LLD enabled."
        exit 1
    fi
else
    echo "✅ Found lld-link: $(which lld-link)"
fi

# Set environment variables with proper defaults
: "${WINSDK:=/Users/pedro/winsdk-arm64}"
: "${WINSDK_VER:=10.0.26100.0}"
: "${QT6_WIN_ARM64:=$HOME/Qt6}"

export WINSDK
export WINSDK_VER
export QT6_WIN_ARM64

# Detect Windows SDK structure
if [ -d "$WINSDK/Program Files/Windows Kits/10" ]; then
    echo "📦 Detected direct Windows SDK structure"
    WINSDK_DISPLAY="$WINSDK/Program Files/Windows Kits/10"
elif [ -d "$WINSDK/sdk" ]; then
    echo "📦 Detected xwin SDK structure"
    WINSDK_DISPLAY="$WINSDK (xwin format)"
else
    echo "⚠️  Warning: Windows SDK structure not recognized at $WINSDK"
    echo "   Expected either:"
    echo "   - $WINSDK/Program Files/Windows Kits/10 (direct)"
    echo "   - $WINSDK/sdk and $WINSDK/crt (xwin)"
    echo ""
    echo "   Run: xwin --accept-license splat --output $WINSDK"
    WINSDK_DISPLAY="$WINSDK (not found)"
fi

# Check for Qt6
if [ -d "$QT6_WIN_ARM64/lib/cmake/Qt6" ]; then
    QT6_STATUS="✅ Qt6 found"
elif [ -d "$QT6_WIN_ARM64" ]; then
    QT6_STATUS="⚠️  Directory exists but no Qt6 cmake configs"
else
    QT6_STATUS="❌ Not found - will build without Qt6"
fi

echo "📦 Windows SDK: $WINSDK_DISPLAY (Version: $WINSDK_VER)"
echo "📦 Qt6 Windows ARM64: $QT6_WIN_ARM64 ($QT6_STATUS)"

# Download PHNT if not present
if [ ! -d "phnt" ]; then
    echo "📦 Downloading Process Hacker Native API headers..."
    git clone --depth 1 https://github.com/processhacker/phnt.git
fi

# Check for Ninja
if ! command -v ninja &> /dev/null; then
    echo "❌ Error: Ninja build system not found."
    echo "  brew install ninja"
    exit 1
fi

echo "✅ Found ninja: $(which ninja)"

# Create build directory
BUILD_DIR="build-windows-arm64"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

echo "🔨 Configuring CMake..."

# Build the cmake command with proper arguments
CMAKE_ARGS=(
    -DCMAKE_TOOLCHAIN_FILE=../windows-arm64-toolchain.cmake
    -DCMAKE_BUILD_TYPE=Release
    -DUSE_PHNT=ON
    -G "Ninja"
)

# Only add Qt6 paths if the directory exists and has cmake configs
if [ -d "$QT6_WIN_ARM64/lib/cmake/Qt6" ]; then
    echo "📦 Found Qt6 cmake configs, adding to build"
    CMAKE_ARGS+=(-DCMAKE_PREFIX_PATH="$QT6_WIN_ARM64")
    CMAKE_ARGS+=(-DQt6_DIR="$QT6_WIN_ARM64/lib/cmake/Qt6")
elif [ -d "$QT6_WIN_ARM64" ]; then
    echo "⚠️  Qt6 directory exists but no cmake configs found"
    echo "   Looking for: $QT6_WIN_ARM64/lib/cmake/Qt6/Qt6Config.cmake"
fi

cmake .. "${CMAKE_ARGS[@]}"

echo "🔨 Building..."
cmake --build . --config Release

echo ""
echo "✅ Build complete!"
echo "📦 Output: $BUILD_DIR/ActivityMonitor.exe"
echo ""
echo "To run on Windows ARM64 device:"
echo "  1. Copy ActivityMonitor.exe to your Windows ARM64 device"
echo "  2. Copy Qt6 DLLs (Qt6Core.dll, Qt6Gui.dll, Qt6Widgets.dll)"
echo "  3. Run ActivityMonitor.exe"
echo ""
echo "To test with Wine (x86_64 emulation):"
echo "  wine $BUILD_DIR/ActivityMonitor.exe"
