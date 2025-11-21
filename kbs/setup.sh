#!/bin/bash

# Kayte Build System Setup Script
# This script sets up the complete project structure

set -e  # Exit on error

echo "🔨 Kayte Build System Setup"
echo "============================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_info() {
    echo -e "${YELLOW}ℹ️  $1${NC}"
}

# Check if we're in the right directory
if [ ! -f "KayteBuildSystem.lpr" ]; then
    print_error "KayteBuildSystem.lpr not found!"
    print_info "Please run this script from the project root directory"
    exit 1
fi

# Step 1: Create directory structure
echo "📁 Creating directory structure..."
mkdir -p public
mkdir -p uploads
mkdir -p bin
mkdir -p units
print_success "Directories created"

# Step 2: Create basic index.html if it doesn't exist
if [ ! -f "public/index.html" ]; then
    echo "📄 Creating index.html..."
    cat > public/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kayte Build System - IDE</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: #2c2c2c;
            color: #e0e0e0;
            padding: 20px;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        h1 {
            color: #0078d7;
            margin-bottom: 20px;
        }
        #status {
            background: #1e1e1e;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
            font-family: monospace;
        }
        .success { color: #4ec9b0; }
        .error { color: #f48771; }
        .loading { color: #ffa500; }
    </style>
</head>
<body>
    <div class="container">
        <h1>🔨 Kayte Build System</h1>
        <div id="status" class="loading">
            Loading application...
        </div>
        <div id="app"></div>
    </div>
    <script src="kaytebuild.js"></script>
    <script>
        // Fallback if Pascal code doesn't load
        setTimeout(function() {
            var status = document.getElementById('status');
            if (status.className === 'loading') {
                status.className = 'error';
                status.innerHTML = 'Failed to load application. Please check console for errors.';
            }
        }, 5000);
    </script>
</body>
</html>
EOF
    print_success "index.html created"
else
    print_info "index.html already exists, skipping"
fi

# Step 3: Check for Node.js
echo ""
echo "🔍 Checking Node.js..."
if ! command -v node &> /dev/null; then
    print_error "Node.js not found!"
    print_info "Please install Node.js from https://nodejs.org/"
    exit 1
fi
NODE_VERSION=$(node --version)
print_success "Node.js found: $NODE_VERSION"

# Step 4: Check for npm
if ! command -v npm &> /dev/null; then
    print_error "npm not found!"
    exit 1
fi
NPM_VERSION=$(npm --version)
print_success "npm found: $NPM_VERSION"

# Step 5: Install Node.js dependencies
echo ""
echo "📦 Installing Node.js dependencies..."
if npm install; then
    print_success "Dependencies installed"
else
    print_error "Failed to install dependencies"
    exit 1
fi

# Step 6: Check for pas2js
echo ""
echo "🔍 Checking for pas2js..."
if ! command -v pas2js &> /dev/null; then
    print_error "pas2js not found!"
    print_info "Please install pas2js:"
    echo "  - Clone: git clone https://github.com/pas2js/pas2js.git"
    echo "  - Build: cd pas2js && make && sudo make install"
    echo "  - Or download from: https://github.com/pas2js/pas2js/releases"
    exit 1
fi
print_success "pas2js found"

# Step 7: Check for FreePascal
echo ""
echo "🔍 Checking for FreePascal..."
if command -v fpc &> /dev/null; then
    FPC_VERSION=$(fpc -iV)
    print_success "FreePascal found: $FPC_VERSION"
else
    print_info "FreePascal not found (optional for pas2js)"
fi

# Step 8: Compile Pascal to JavaScript
echo ""
echo "🔧 Compiling Pascal to JavaScript..."
echo "Command: pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js"

if pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js; then
    print_success "Pascal compilation successful!"
    
    # Check file size
    if [ -f "public/kaytebuild.js" ]; then
        FILE_SIZE=$(du -h public/kaytebuild.js | cut -f1)
        print_info "Output file size: $FILE_SIZE"
    fi
else
    print_error "Pascal compilation failed!"
    print_info "Check the error messages above"
    exit 1
fi

# Step 9: Create .gitignore if it doesn't exist
if [ ! -f ".gitignore" ]; then
    echo ""
    echo "📝 Creating .gitignore..."
    cat > .gitignore << 'EOF'
# Node.js
node_modules/
npm-debug.log
package-lock.json

# Build output
public/kaytebuild.js
public/kaytebuild.js.map
*.ppu
*.o
*.compiled

# Uploads
uploads/*
!uploads/.gitkeep

# Build artifacts
bin/*
!bin/.gitkeep

# IDE
.vscode/
.idea/
*.lps
*.lpi.bak

# OS
.DS_Store
Thumbs.db

# Environment
.env
EOF
    print_success ".gitignore created"
fi

# Step 10: Create .gitkeep files
touch uploads/.gitkeep
touch bin/.gitkeep

# Summary
echo ""
echo "================================"
echo "✨ Setup Complete!"
echo "================================"
echo ""
print_success "Project structure created"
print_success "Dependencies installed"
print_success "Pascal code compiled"
echo ""
echo "🚀 Next steps:"
echo "  1. Start the server:    npm start"
echo "  2. Open browser:        http://localhost:3000"
echo "  3. Start development:   npm run dev"
echo ""
echo "📚 For more information, see BUILD_PASCAL.md"
echo ""
