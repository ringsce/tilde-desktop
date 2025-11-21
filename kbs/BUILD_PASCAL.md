# Building Kayte Build System with FreePascal & Node.js

This guide explains how to build and run the Kayte Build System using FreePascal with pas2js for the frontend and Node.js for the backend.

## Quick Fix for Setup

### Create Required Directories First

```bash
# Navigate to your project directory
cd kbs

# Create required directories
mkdir -p public
mkdir -p uploads
mkdir -p bin

# Verify directory structure
ls -la
```

### Now Compile Pascal to JavaScript

```bash
# Method 1: Compile to public directory
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js

# Method 2: Compile to current directory first
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -okaytebuild.js
# Then move to public
mv kaytebuild.js public/

# Method 3: Use absolute path
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -o$(pwd)/public/kaytebuild.js
```

**Note:** The `-o` flag requires no space between `-o` and the filename!

## Correct Compilation Commands

### Basic Compilation
```bash
# Correct syntax (no space after -o)
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js
```

### With Source Maps (for debugging)
```bash
pas2js -Jirtl.js -Jc -Tbrowser -g -gl KayteBuildSystem.lpr -opublic/kaytebuild.js
```

### Optimized Production Build
```bash
pas2js -Jirtl.js -Jc -Tbrowser -O3 KayteBuildSystem.lpr -opublic/kaytebuild.js
```

## Complete Setup Script

Create a setup script `setup.sh`:

```bash
#!/bin/bash

echo "🔨 Setting up Kayte Build System..."

# Create directories
echo "📁 Creating directories..."
mkdir -p public
mkdir -p uploads  
mkdir -p bin
mkdir -p units

# Copy static files to public
echo "📄 Setting up public files..."
cp index.html public/ 2>/dev/null || echo "index.html not found, skipping..."
cp styles.css public/ 2>/dev/null || echo "styles.css not found, skipping..."
cp -r assets public/ 2>/dev/null || echo "assets not found, skipping..."

# Install Node.js dependencies
echo "📦 Installing Node.js dependencies..."
npm install

# Compile Pascal to JavaScript
echo "🔧 Compiling Pascal to JavaScript..."
if command -v pas2js &> /dev/null; then
    pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js
    
    if [ $? -eq 0 ]; then
        echo "✅ Pascal compilation successful!"
    else
        echo "❌ Pascal compilation failed!"
        exit 1
    fi
else
    echo "⚠️  pas2js not found. Please install pas2js first."
    exit 1
fi

echo "✅ Setup complete!"
echo "🚀 Run 'npm start' to start the server"
```

Make it executable and run:
```bash
chmod +x setup.sh
./setup.sh
```

## Architecture

```
┌─────────────────────────────────────────┐
│         Frontend (Browser)              │
│  ┌─────────────────────────────────┐   │
│  │  Pascal Code (pas2js compiled)  │   │
│  │  - KayteBuildSystem.lpr          │   │
│  │  - KayteTypes.pas                 │   │
│  │  - KayteUI.pas                    │   │
│  │  - KayteBuild.pas                 │   │
│  │  - KayteGit.pas                   │   │
│  └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │ HTTP/REST API
┌──────────────▼──────────────────────────┐
│        Backend (Node.js)                │
│  ┌─────────────────────────────────┐   │
│  │  server.js                       │   │
│  │  - Build Management              │   │
│  │  - Compiler Integration          │   │
│  │  - Git Operations                │   │
│  │  - Package Creation              │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

## Prerequisites

### 1. FreePascal & pas2js

**Install FreePascal:**
```bash
# macOS (Homebrew)
brew install fpc

# Linux (Debian/Ubuntu)
sudo apt-get install fpc

# Linux (Fedora)
sudo dnf install fpc
```

**Install pas2js:**
```bash
# Clone pas2js from GitHub
git clone https://github.com/pas2js/pas2js.git
cd pas2js
make
sudo make install

# Or download pre-built binaries from:
# https://github.com/pas2js/pas2js/releases

# Verify installation
pas2js -h
```

### 2. Node.js

**Install Node.js (v16 or higher):**
```bash
# macOS (Homebrew)
brew install node

# Linux (using nvm)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install 16

# Verify installation
node --version
npm --version
```

## Project Structure

```
kbs/
├── KayteBuildSystem.lpr      # Main Pascal program
├── KayteTypes.pas             # Type definitions
├── KayteUI.pas                # UI management (to be created)
├── KayteBuild.pas             # Build system (to be created)
├── KayteGit.pas               # Git integration (to be created)
├── KaytePackage.pas           # Package management (to be created)
├── server.js                  # Node.js backend
├── package.json               # Node.js dependencies
├── setup.sh                   # Setup script
├── public/                    # Static files (created by setup)
│   ├── index.html
│   ├── styles.css
│   ├── kaytebuild.js         # Compiled from Pascal
│   └── assets/
├── uploads/                   # Temporary upload directory
├── bin/                       # Build output
└── README.md
```

## Manual Step-by-Step Setup

### Step 1: Create Project Structure
```bash
cd kbs
mkdir -p public uploads bin units
```

### Step 2: Install Node.js Dependencies
```bash
npm install
```

### Step 3: Create index.html in public/
```bash
cat > public/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kayte Build System</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <div id="app">
        <h1>Kayte Build System</h1>
        <p>Loading...</p>
    </div>
    <script src="kaytebuild.js"></script>
</body>
</html>
EOF
```

### Step 4: Compile Pascal Code
```bash
# Important: No space between -o and filename!
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js

# Check if compilation succeeded
if [ -f public/kaytebuild.js ]; then
    echo "✅ Compilation successful!"
    ls -lh public/kaytebuild.js
else
    echo "❌ Compilation failed!"
fi
```

### Step 5: Start the Server
```bash
npm start
```

### Step 6: Open in Browser
```
http://localhost:3000
```

## Common pas2js Errors and Solutions

### Error: "invalid empty output file"
**Problem:** Missing public directory or wrong syntax

**Solution:**
```bash
# Create directory first
mkdir -p public

# Use correct syntax (no space after -o)
pas2js -Jirtl.js -Jc -Tbrowser file.lpr -ooutput.js

# NOT: pas2js -o output.js  (wrong - space causes error)
```

### Error: "pas2js: command not found"
**Solution:**
```bash
# Add pas2js to PATH
export PATH=$PATH:/path/to/pas2js/bin

# Or create symlink
sudo ln -s /path/to/pas2js/bin/pas2js /usr/local/bin/pas2js

# Verify
which pas2js
```

### Error: "Unit not found"
**Solution:**
```bash
# Add unit search path with -Fu
pas2js -Fu./units -Jirtl.js -Jc -Tbrowser file.lpr -ooutput.js

# Or use -Fi for include files
pas2js -Fi./includes -Jirtl.js -Jc -Tbrowser file.lpr -ooutput.js
```

### Error: "Unknown directive"
**Solution:**
```pascal
// Use pas2js-compatible mode
{$mode objfpc}{$H+}
{$modeswitch externalclass}
```

## Building the Frontend (Pascal to JavaScript)

### Development Build
```bash
pas2js -Jirtl.js -Jc -Tbrowser -g KayteBuildSystem.lpr -opublic/kaytebuild.js
```

### Production Build  
```bash
pas2js -Jirtl.js -Jc -Tbrowser -O3 KayteBuildSystem.lpr -opublic/kaytebuild.js
```

### With Custom Options
```bash
pas2js \
  -Jirtl.js \
  -Jc \
  -Tbrowser \
  -Fu./units \
  -Fi./includes \
  -O2 \
  KayteBuildSystem.lpr \
  -opublic/kaytebuild.js
```

## Auto-Recompile on Changes

### Using watchman (macOS/Linux)
```bash
# Install watchman
brew install watchman  # macOS
# or
sudo apt-get install watchman  # Linux

# Watch and recompile
watchman-make -p '**/*.pas' '**/*.lpr' \
  -t compile-pascal
```

### Using fswatch (macOS)
```bash
# Install fswatch
brew install fswatch

# Create watch script
cat > watch-pascal.sh << 'EOF'
#!/bin/bash
while true; do
    fswatch -1 ./*.pas ./*.lpr
    echo "⚙️  Recompiling..."
    pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js
    echo "✅ Done at $(date)"
done
EOF

chmod +x watch-pascal.sh
./watch-pascal.sh
```

### Using inotifywait (Linux)
```bash
# Install inotify-tools
sudo apt-get install inotify-tools

# Create watch script
cat > watch-pascal.sh << 'EOF'
#!/bin/bash
while true; do
    inotifywait -e modify ./*.pas ./*.lpr
    echo "⚙️  Recompiling..."
    pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -opublic/kaytebuild.js
    echo "✅ Done at $(date)"
done
EOF

chmod +x watch-pascal.sh
./watch-pascal.sh
```

```
┌─────────────────────────────────────────┐
│         Frontend (Browser)              │
│  ┌─────────────────────────────────┐   │
│  │  Pascal Code (pas2js compiled)  │   │
│  │  - KayteBuildSystem.lpr          │   │
│  │  - KayteTypes.pas                 │   │
│  │  - KayteUI.pas                    │   │
│  │  - KayteBuild.pas                 │   │
│  │  - KayteGit.pas                   │   │
│  └─────────────────────────────────┘   │
└──────────────┬──────────────────────────┘
               │ HTTP/REST API
┌──────────────▼──────────────────────────┐
│        Backend (Node.js)                │
│  ┌─────────────────────────────────┐   │
│  │  server.js                       │   │
│  │  - Build Management              │   │
│  │  - Compiler Integration          │   │
│  │  - Git Operations                │   │
│  │  - Package Creation              │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

## Prerequisites

### 1. FreePascal & pas2js

**Install FreePascal:**
```bash
# macOS (Homebrew)
brew install fpc

# Linux (Debian/Ubuntu)
sudo apt-get install fpc

# Linux (Fedora)
sudo dnf install fpc
```

**Install pas2js:**
```bash
# Clone pas2js from GitHub
git clone https://github.com/pas2js/pas2js.git
cd pas2js
make

# Or download pre-built binaries from:
# https://github.com/pas2js/pas2js/releases
```

### 2. Node.js

**Install Node.js (v16 or higher):**
```bash
# macOS (Homebrew)
brew install node

# Linux (using nvm)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install 16

# Or download from: https://nodejs.org/
```

### 3. Lazarus IDE (Optional)

For development, you can use Lazarus IDE:
```bash
# macOS
brew install --cask lazarus

# Linux
sudo apt-get install lazarus
```

## Project Structure

```
kayte-build-system/
├── KayteBuildSystem.lpr      # Main Pascal program
├── KayteTypes.pas             # Type definitions
├── KayteUI.pas                # UI management (to be created)
├── KayteBuild.pas             # Build system (to be created)
├── KayteGit.pas               # Git integration (to be created)
├── KaytePackage.pas           # Package management (to be created)
├── server.js                  # Node.js backend
├── package.json               # Node.js dependencies
├── public/                    # Static files
│   ├── index.html
│   ├── styles.css
│   └── kaytebuild.js         # Compiled from Pascal
├── uploads/                   # Temporary upload directory
└── README.md
```

## Building the Frontend (Pascal to JavaScript)

### Step 1: Compile Pascal to JavaScript

```bash
# Navigate to project directory
cd kayte-build-system

# Compile with pas2js
pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -o public/kaytebuild.js

# Options explained:
# -Jirtl.js    : Include runtime library
# -Jc          : Create concatenated output
# -Tbrowser    : Target browser platform
# -o           : Output file
```

### Step 2: Verify Compilation

```bash
# Check if JavaScript file was created
ls -lh public/kaytebuild.js

# You should see the compiled JavaScript file
```

### Advanced Compilation Options

**Development Build (with source maps):**
```bash
pas2js -Jirtl.js -Jc -Tbrowser -g -gl KayteBuildSystem.lpr -o public/kaytebuild.js
```

**Production Build (optimized):**
```bash
pas2js -Jirtl.js -Jc -Tbrowser -O3 KayteBuildSystem.lpr -o public/kaytebuild.js
```

**With Custom Units Path:**
```bash
pas2js -Jirtl.js -Jc -Tbrowser -Fu./units KayteBuildSystem.lpr -o public/kaytebuild.js
```

## Building the Backend (Node.js)

### Step 1: Install Dependencies

```bash
# Navigate to project directory
cd kayte-build-system

# Install Node.js dependencies
npm install

# This installs:
# - express: Web framework
# - cors: Cross-origin resource sharing
# - multer: File upload handling
# - dotenv: Environment configuration
```

### Step 2: Configuration

Create a `.env` file:
```bash
# .env
PORT=3000
NODE_ENV=development
UPLOAD_DIR=./uploads
MAX_FILE_SIZE=10485760
```

### Step 3: Start the Server

```bash
# Production mode
npm start

# Development mode (with auto-restart)
npm run dev

# The server will start on http://localhost:3000
```

## Running the Complete System

### Option 1: Manual Start (Recommended for Development)

**Terminal 1 - Backend Server:**
```bash
cd kayte-build-system
npm run dev
```

**Terminal 2 - Watch and Recompile Pascal:**
```bash
# Install fswatch (macOS)
brew install fswatch

# Or inotify-tools (Linux)
sudo apt-get install inotify-tools

# Watch for changes and recompile
while true; do
  fswatch -1 ./*.pas
  pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -o public/kaytebuild.js
  echo "Recompiled at $(date)"
done
```

**Open Browser:**
```
http://localhost:3000
```

### Option 2: Using npm Scripts

Add to `package.json`:
```json
{
  "scripts": {
    "start": "node server.js",
    "dev": "nodemon server.js",
    "watch:pascal": "nodemon --watch '*.pas' --exec 'pas2js -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr -o public/kaytebuild.js'",
    "dev:all": "concurrently \"npm run dev\" \"npm run watch:pascal\""
  }
}
```

Install concurrently:
```bash
npm install --save-dev concurrently
```

Run everything:
```bash
npm run dev:all
```

## API Endpoints

### Compiler Detection
```bash
GET /api/detect-compilers
Response: {
  "success": true,
  "compilers": {
    "fpc": "/opt/homebrew/bin/fpc",
    "lazbuild": "/Applications/Lazarus/lazbuild",
    "cmake": "/usr/local/bin/cmake"
  }
}
```

### Build with FPC
```bash
POST /api/build/fpc
Body: {
  "projectFile": "main.pas",
  "outputDir": "./bin",
  "compilerOptions": "-O2 -MObjFPC",
  "fpcPath": "/opt/homebrew/bin/fpc"
}
```

### Build with Lazbuild
```bash
POST /api/build/lazbuild
Body: {
  "projectFile": "project.lpi",
  "buildMode": "Release",
  "lazbuildPath": "/Applications/Lazarus/lazbuild"
}
```

### Build with CMake
```bash
POST /api/build/cmake
Body: {
  "sourceDir": ".",
  "buildDir": "./build",
  "generator": "Unix Makefiles",
  "buildType": "Release",
  "cmakePath": "/usr/local/bin/cmake"
}
```

### Git Operations
```bash
POST /api/git/init
POST /api/git/status
POST /api/git/add
POST /api/git/commit
POST /api/git/push
POST /api/git/pull
```

### Package Creation
```bash
POST /api/package/create
Body: {
  "projectName": "MyApp",
  "outputDir": "./bin",
  "format": "dmg" | "tar.gz" | "zip"
}
```

## Testing the API

### Using curl

**Detect Compilers:**
```bash
curl http://localhost:3000/api/detect-compilers
```

**Build with FPC:**
```bash
curl -X POST http://localhost:3000/api/build/fpc \
  -H "Content-Type: application/json" \
  -d '{
    "projectFile": "test.pas",
    "outputDir": "./bin",
    "compilerOptions": "-O2",
    "fpcPath": "/opt/homebrew/bin/fpc"
  }'
```

**Git Init:**
```bash
curl -X POST http://localhost:3000/api/git/init \
  -H "Content-Type: application/json" \
  -d '{"projectDir": "."}'
```

### Using Postman or Thunder Client

Import the API collection:
1. Open Postman/Thunder Client
2. Import the endpoints from the examples above
3. Set base URL: `http://localhost:3000`

## Troubleshooting

### Pascal Compilation Errors

**Error: "pas2js: command not found"**
```bash
# Add pas2js to PATH
export PATH=$PATH:/path/to/pas2js/bin

# Or create symlink
sudo ln -s /path/to/pas2js/bin/pas2js /usr/local/bin/pas2js
```

**Error: "Unit not found"**
```bash
# Add unit search path
pas2js -Fu./units -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr
```

**Error: "Invalid mode"**
```bash
# Ensure using correct mode directive
# Add to top of .pas file:
{$mode objfpc}{$H+}
```

### Node.js Server Errors

**Error: "Port 3000 already in use"**
```bash
# Find and kill process
lsof -ti:3000 | xargs kill -9

# Or use different port
PORT=3001 npm start
```

**Error: "Cannot find module"**
```bash
# Reinstall dependencies
rm -rf node_modules package-lock.json
npm install
```

**Error: "Permission denied"**
```bash
# Fix permissions
chmod +x server.js

# Or run with sudo (not recommended)
sudo npm start
```

### Runtime Errors

**"Compiler not found"**
- Ensure FPC/Lazarus/CMake are installed
- Check PATH environment variable
- Verify paths in API requests

**"Build failed"**
- Check compiler output in response
- Verify source file exists
- Check compiler options syntax

## Performance Optimization

### Pascal Compilation

**Faster Compilation:**
```bash
# Use -O1 instead of -O3 for development
pas2js -O1 -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr
```

**Smaller Output:**
```bash
# Use -Xs for smart linking
pas2js -Xs -O3 -Jirtl.js -Jc -Tbrowser KayteBuildSystem.lpr

# Minify JavaScript output
npm install -g terser
terser public/kaytebuild.js -o public/kaytebuild.min.js -c -m
```

### Node.js Server

**Enable Clustering:**
```javascript
// server.js
const cluster = require('cluster');
const numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }
} else {
  // Start Express server
}
```

**Add Caching:**
```javascript
// server.js
const cache = require('memory-cache');

app.use((req, res, next) => {
  const key = req.url;
  const cached = cache.get(key);
  
  if (cached) {
    return res.json(cached);
  }
  
  res.originalJson = res.json;
  res.json = (data) => {
    cache.put(key, data, 60000); // 1 minute
    res.originalJson(data);
  };
  
  next();
});
```

## Deployment

### Development Deployment

```bash
# Start server
npm run dev

# Access at
http://localhost:3000
```

### Production Deployment

**Using PM2:**
```bash
# Install PM2
npm install -g pm2

# Start with PM2
pm2 start server.js --name kayte-build-system

# Auto-restart on reboot
pm2 startup
pm2 save

# Monitor
pm2 monit

# Logs
pm2 logs kayte-build-system
```

**Using Docker:**
```dockerfile
# Dockerfile
FROM node:16

WORKDIR /app

COPY package*.json ./
RUN npm install --production

COPY . .

EXPOSE 3000

CMD ["npm", "start"]
```

Build and run:
```bash
docker build -t kayte-build-system .
docker run -p 3000:3000 kayte-build-system
```

**Using systemd (Linux):**
```ini
# /etc/systemd/system/kayte-build-system.service
[Unit]
Description=Kayte Build System
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/kayte-build-system
ExecStart=/usr/bin/node server.js
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl enable kayte-build-system
sudo systemctl start kayte-build-system
sudo systemctl status kayte-build-system
```

## Development Workflow

### 1. Make Changes to Pascal Code
```bash
vim KayteBuildSystem.lpr
```

### 2. Compile to JavaScript
```bash
npm run build:pascal
```

### 3. Test in Browser
```bash
# Server should be running (npm run dev)
# Open: http://localhost:3000
```

### 4. Make Changes to Backend
```bash
vim server.js
# nodemon will auto-restart
```

### 5. Commit Changes
```bash
git add .
git commit -m "Added new feature"
git push
```

## Additional Resources

- **pas2js Documentation**: https://wiki.freepascal.org/pas2js
- **FreePascal Reference**: https://www.freepascal.org/docs.html
- **Node.js API**: https://nodejs.org/api/
- **Express.js Guide**: https://expressjs.com/

## Next Steps

1. Complete the remaining Pascal units:
   - `KayteUI.pas` - UI management with DOM manipulation
   - `KayteBuild.pas` - Build system integration
   - `KayteGit.pas` - Git operations
   - `KaytePackage.pas` - Package creation

2. Enhance the API:
   - Add authentication
   - Implement WebSocket for real-time updates
   - Add build queue management

3. Add tests:
   - Unit tests for Pascal code
   - API tests for Node.js backend
   - Integration tests

4. Create documentation:
   - API reference
   - User guide
   - Developer guide

---

**Last Updated:** 2025-01-20