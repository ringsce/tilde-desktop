#!/bin/bash
# Check Windows SDK for required libraries

WINSDK="/Users/pedro/winsdk-arm64/Program Files/Windows Kits/10"
WINSDK_VER="10.0.26100.0"

echo "Checking Windows SDK at: $WINSDK"
echo "Version: $WINSDK_VER"
echo ""

# Check for CRT libraries
echo "Looking for CRT libraries (libcmt*.lib, oldnames.lib)..."
find "$WINSDK" -name "libcmt*.lib" -o -name "oldnames.lib"

echo ""
echo "Available library directories:"
find "$WINSDK/Lib/$WINSDK_VER" -type d -name "arm64"

echo ""
echo "Libraries in ucrt/arm64:"
if [ -d "$WINSDK/Lib/$WINSDK_VER/ucrt/arm64" ]; then
    ls "$WINSDK/Lib/$WINSDK_VER/ucrt/arm64" | head -20
else
    echo "  Directory not found"
fi

echo ""
echo "Libraries in um/arm64:"
if [ -d "$WINSDK/Lib/$WINSDK_VER/um/arm64" ]; then
    ls "$WINSDK/Lib/$WINSDK_VER/um/arm64" | head -20
else
    echo "  Directory not found"
fi

# Check for MSVC libraries (these contain libcmt*.lib and oldnames.lib)
echo ""
echo "Looking for MSVC library directories..."
find /Users/pedro/winsdk-arm64 -type d -name "MSVC" -o -path "*/VC/Tools/MSVC/*"
