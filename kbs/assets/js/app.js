// Global state
let currentTab = 0;
let detectedPaths = {
    fpc: '',
    lazbuild: '/Applications/Lazarus/lazbuild',
    homebrewFPC: '/opt/homebrew/bin/fpc',
    cmake: ''
};
let projectStructure = {
    binFolder: false,
    projectFiles: [],
    buildArtifacts: [],
    packages: [],
    git: {
        initialized: false,
        currentBranch: 'main',
        branches: ['main'],
        commits: [],
        stagedFiles: [],
        modifiedFiles: [],
        remoteUrl: ''
    }
};

// Auto-detect paths on load
window.addEventListener('DOMContentLoaded', () => {
    detectFPC();
    detectLazbuild();
    detectCMake();
});

// Tab Management
function switchTab(index) {
    const tabs = document.querySelectorAll('.tab');
    const contents = document.querySelectorAll('.tab-content');
    
    tabs.forEach(tab => tab.classList.remove('active'));
    contents.forEach(content => content.classList.remove('active'));
    
    tabs[index].classList.add('active');
    contents[index].classList.add('active');
    currentTab = index;
}

// Output Panel
function addOutput(message, type = 'normal') {
    const output = document.getElementById('outputPanel');
    const line = document.createElement('div');
    line.className = 'output-line';
    if (type === 'success') line.classList.add('output-success');
    if (type === 'error') line.classList.add('output-error');
    line.textContent = message;
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

function updateStatus(text) {
    document.getElementById('statusText').textContent = text;
}

// Project Management
function newProject() {
    updateStatus('Creating new project...');
    addOutput('Creating new project...', 'normal');
    addOutput('Setting up project structure...', 'normal');
    
    const projectName = document.getElementById('projectName').value;
    if (!projectStructure.binFolder) {
        createBinFolder(projectName);
    }
}

function openProject() {
    updateStatus('Opening project...');
    addOutput('Opening project...', 'normal');
    document.getElementById('fileInput').click();
}

function handleProjectFile(event) {
    const file = event.target.files[0];
    if (!file) return;
    
    const fileName = file.name;
    const fileExt = fileName.split('.').pop().toLowerCase();
    
    // Check for CMakeLists.txt
    const isCMake = fileName === 'CMakeLists.txt';
    
    if (!['lpi', 'lpr', 'dpr', 'txt'].includes(fileExt) && !isCMake) {
        addOutput(`Error: Invalid file type. Expected .lpi, .lpr, .dpr, or CMakeLists.txt`, 'error');
        updateStatus('Invalid file type');
        return;
    }
    
    // Additional validation for .txt files
    if (fileExt === 'txt' && !isCMake) {
        addOutput(`Error: Only CMakeLists.txt is supported for .txt files`, 'error');
        updateStatus('Invalid file type');
        return;
    }
    
    document.getElementById('projectFile').value = fileName;
    
    const buildModeSelect = document.getElementById('buildMode');
    
    if (isCMake) {
        // CMake project
        const projectName = 'CMakeProject';
        document.getElementById('projectName').value = projectName;
        buildModeSelect.value = 'cmake';
        addOutput(`✓ CMake project detected: ${fileName}`, 'success');
    } else {
        // Pascal projects
        const projectName = fileName.replace(/\.(lpi|lpr|dpr)$/i, '');
        document.getElementById('projectName').value = projectName;
        
        if (fileExt === 'lpi') {
            buildModeSelect.value = 'lazbuild';
            addOutput(`✓ Lazarus project detected: ${fileName}`, 'success');
        } else if (fileExt === 'lpr') {
            buildModeSelect.value = 'lazbuild';
            addOutput(`✓ Lazarus program file detected: ${fileName}`, 'success');
        } else if (fileExt === 'dpr') {
            buildModeSelect.value = 'fpc';
            addOutput(`✓ Delphi project detected: ${fileName}`, 'success');
        }
    }
    
    const reader = new FileReader();
    reader.onload = function(e) {
        addOutput(`Project file loaded successfully`, 'success');
        addOutput(`File size: ${(file.size / 1024).toFixed(2)} KB`, 'normal');
        
        const projectName = document.getElementById('projectName').value;
        updateStatus(`Project loaded: ${projectName}`);
        
        if (isCMake) {
            updateProjectExplorer(projectName, 'cmake');
        } else {
            updateProjectExplorer(projectName, fileExt);
        }
        
        // Automatically start building the project
        addOutput('Auto-building project...', 'normal');
        setTimeout(() => {
            startBuild();
        }, 500);
    };
    reader.readAsText(file);
}

function updateProjectExplorer(projectName, fileExt) {
    const sidebarContent = document.querySelector('.sidebar-content');
    let fileIcon = '📄';
    
    if (fileExt === 'lpi') {
        fileIcon = '📘';
    } else if (fileExt === 'lpr') {
        fileIcon = '📄';
    } else if (fileExt === 'dpr') {
        fileIcon = '📕';
    } else if (fileExt === 'cmake') {
        fileIcon = '🔧';
    }
    
    if (!projectStructure.binFolder) {
        createBinFolder(projectName);
    }
    
    let binFolderHTML = '';
    if (projectStructure.binFolder) {
        binFolderHTML = `
            <div class="tree-item" style="padding-left: 24px;" onclick="toggleBinFolder()">📁 bin/</div>`;
        
        if (projectStructure.buildArtifacts.length > 0) {
            projectStructure.buildArtifacts.forEach(artifact => {
                binFolderHTML += `
            <div class="tree-item" style="padding-left: 48px;">📦 ${artifact}</div>`;
            });
        } else {
            binFolderHTML += `
            <div class="tree-item" style="padding-left: 48px; opacity: 0.6;">📄 (empty)</div>`;
        }
    }
    
    let projectFileHTML = '';
    if (fileExt === 'cmake') {
        projectFileHTML = `<div class="tree-item" style="padding-left: 24px;">${fileIcon} CMakeLists.txt</div>`;
    } else {
        projectFileHTML = `<div class="tree-item" style="padding-left: 24px;">${fileIcon} ${projectName}.${fileExt}</div>`;
    }
    
    sidebarContent.innerHTML = `
        <div class="tree-item selected">📦 ${projectName}</div>
        ${projectFileHTML}
        <div class="tree-item" style="padding-left: 24px;">🗂️ Units</div>
        <div class="tree-item" style="padding-left: 24px;">🗂️ Resources</div>
        ${binFolderHTML}
    `;
}

function createBinFolder(projectName) {
    addOutput(`Creating ./bin directory in project root...`, 'normal');
    
    setTimeout(() => {
        projectStructure.binFolder = true;
        addOutput(`✓ Created: ./bin/`, 'success');
        addOutput(`  Location: ${projectName}/bin/`, 'normal');
        updateStatus('bin/ folder created');
        
        const projectFile = document.getElementById('projectFile').value;
        if (projectFile) {
            const fileExt = projectFile.split('.').pop().toLowerCase();
            updateProjectExplorer(projectName, fileExt);
        }
    }, 300);
}

function toggleBinFolder() {
    addOutput('Viewing bin/ folder contents...', 'normal');
    if (projectStructure.buildArtifacts.length === 0) {
        addOutput('  No build artifacts yet. Build the project to generate executables.', 'normal');
    } else {
        addOutput(`  Found ${projectStructure.buildArtifacts.length} artifact(s):`, 'normal');
        projectStructure.buildArtifacts.forEach(artifact => {
            addOutput(`    - ${artifact}`, 'normal');
        });
    }
}

function saveProject() {
    updateStatus('Project saved');
    addOutput('Project saved successfully', 'success');
}

// Build Functions
function buildProject() {
    switchTab(2);
    updateStatus('Building project...');
    addOutput('--- Build Started ---', 'normal');
    addOutput('Compiling sources...', 'normal');
    setTimeout(() => {
        addOutput('Build completed successfully', 'success');
        updateStatus('Build complete');
    }, 1000);
}

function runProject() {
    updateStatus('Running project...');
    switchTab(2);
    addOutput('Starting application...', 'success');
}

function debugProject() {
    updateStatus('Starting debugger...');
    switchTab(2);
    addOutput('Debugger attached', 'normal');
}

function startBuild() {
    const projectName = document.getElementById('projectName').value;
    const projectFile = document.getElementById('projectFile').value;
    const buildTarget = document.getElementById('buildTarget').value;
    const buildMode = document.getElementById('buildMode').value;
    const fpcPath = document.getElementById('fpcPath').value;
    const lazbuildPath = document.getElementById('lazbuildPath').value;
    const cmakePath = document.getElementById('cmakePath').value;
    const outputDir = document.getElementById('outputDir').value;
    
    if (buildMode === 'lazbuild' && !lazbuildPath) {
        addOutput('Error: lazbuild path not configured!', 'error');
        updateStatus('Build failed - lazbuild not found');
        return;
    }
    
    if (buildMode === 'fpc' && !fpcPath) {
        addOutput('Error: FreePascal compiler not configured!', 'error');
        updateStatus('Build failed - FPC not found');
        return;
    }
    
    if (buildMode === 'cmake' && !cmakePath) {
        addOutput('Error: CMake path not configured!', 'error');
        updateStatus('Build failed - CMake not found');
        return;
    }
    
    if (!projectFile && buildMode === 'lazbuild') {
        addOutput('Warning: No project file selected. Using default sources.', 'normal');
    }
    
    switchTab(2);
    updateStatus('Building...');
    addOutput(`--- Building ${projectName} (${buildTarget}) ---`, 'normal');
    
    if (buildMode === 'lazbuild') {
        buildWithLazbuild(projectName, projectFile, buildTarget, lazbuildPath, outputDir);
    } else if (buildMode === 'fpc') {
        buildWithFPC(projectName, projectFile, buildTarget, fpcPath, outputDir);
    } else if (buildMode === 'cmake') {
        buildWithCMake(projectName, projectFile, buildTarget, cmakePath, outputDir);
    }
}

function buildWithLazbuild(projectName, projectFile, buildTarget, lazbuildPath, outputDir) {
    addOutput(`Using lazbuild: ${lazbuildPath}`, 'normal');
    
    if (projectFile) {
        addOutput(`Project file: ${projectFile}`, 'normal');
    }
    
    if (!projectStructure.binFolder) {
        createBinFolder(projectName);
    }
    
    const buildCmd = projectFile 
        ? `${lazbuildPath} --build-mode=${buildTarget} ${projectFile}`
        : `${lazbuildPath} --build-mode=${buildTarget} ${projectName}.lpi`;
    
    addOutput(`Command: ${buildCmd}`, 'normal');
    addOutput('Checking dependencies...', 'normal');
    
    setTimeout(() => {
        addOutput('Reading project configuration...', 'normal');
        addOutput('Compiling resources...', 'normal');
        addOutput('Compiling project units...', 'normal');
        addOutput('  MainForm.pas', 'normal');
        addOutput('  BuildConfig.pas', 'normal');
        addOutput('  ProjectManager.pas', 'normal');
        addOutput('Linking...', 'normal');
        
        const executableName = `${projectName}${navigator.platform.includes('Win') ? '.exe' : ''}`;
        addOutput(`Success: ${outputDir}/${executableName}`, 'success');
        addOutput(`Build completed in 2.3s`, 'success');
        updateStatus('Build complete');
        
        addBuildArtifact(executableName, projectName);
    }, 1500);
}

function buildWithFPC(projectName, projectFile, buildTarget, fpcPath, outputDir) {
    addOutput(`Using FPC: ${fpcPath}`, 'normal');
    
    if (!projectStructure.binFolder) {
        createBinFolder(projectName);
    }
    
    const targetFile = projectFile || `${projectName}.pas`;
    const compilerOpts = document.getElementById('compilerOpts').value;
    const buildCmd = `${fpcPath} ${compilerOpts} -o${outputDir}/${projectName} ${targetFile}`;
    
    addOutput(`Command: ${buildCmd}`, 'normal');
    addOutput('Checking dependencies...', 'normal');
    
    setTimeout(() => {
        addOutput(`Compiling ${targetFile}...`, 'normal');
        addOutput('Parsing source files...', 'normal');
        addOutput('Generating code...', 'normal');
        addOutput('Linking...', 'normal');
        
        const executableName = `${projectName}${navigator.platform.includes('Win') ? '.exe' : ''}`;
        addOutput(`Build successful! Output: ${outputDir}/${executableName}`, 'success');
        addOutput(`Lines compiled: 1247, Time: 1.8s`, 'success');
        updateStatus('Build complete');
        
        addBuildArtifact(executableName, projectName);
    }, 1500);
}

function addBuildArtifact(artifactName, projectName) {
    if (!projectStructure.buildArtifacts.includes(artifactName)) {
        projectStructure.buildArtifacts.push(artifactName);
        addOutput(`✓ Artifact added to bin/: ${artifactName}`, 'success');
        
        const projectFile = document.getElementById('projectFile').value;
        if (projectFile) {
            const isCMake = projectFile === 'CMakeLists.txt';
            if (isCMake) {
                updateProjectExplorer(projectName, 'cmake');
            } else {
                const fileExt = projectFile.split('.').pop().toLowerCase();
                updateProjectExplorer(projectName, fileExt);
            }
        }
    }
}

function buildWithCMake(projectName, projectFile, buildTarget, cmakePath, outputDir) {
    addOutput(`Using CMake: ${cmakePath}`, 'normal');
    
    if (projectFile) {
        addOutput(`Project file: ${projectFile}`, 'normal');
    }
    
    if (!projectStructure.binFolder) {
        createBinFolder(projectName);
    }
    
    const generator = document.getElementById('cmakeGenerator').value;
    const buildType = buildTarget === 'Debug' ? 'Debug' : 'Release';
    const buildDir = `${outputDir}/build`;
    
    addOutput(`Build directory: ${buildDir}`, 'normal');
    addOutput(`Generator: ${generator}`, 'normal');
    addOutput(`Build type: ${buildType}`, 'normal');
    
    // Configure step
    const configCmd = `${cmakePath} -G "${generator}" -DCMAKE_BUILD_TYPE=${buildType} -B ${buildDir} -S .`;
    addOutput(`Configure command: ${configCmd}`, 'normal');
    addOutput('Configuring CMake project...', 'normal');
    
    setTimeout(() => {
        addOutput('-- The C compiler identification is GNU', 'normal');
        addOutput('-- The CXX compiler identification is GNU', 'normal');
        addOutput('-- Detecting C compiler ABI info', 'normal');
        addOutput('-- Detecting CXX compiler ABI info', 'normal');
        addOutput('-- Configuring done', 'success');
        addOutput('-- Generating done', 'success');
        
        // Build step
        const buildCmd = `${cmakePath} --build ${buildDir} --config ${buildType}`;
        addOutput(`Build command: ${buildCmd}`, 'normal');
        addOutput('Building project...', 'normal');
        
        setTimeout(() => {
            addOutput('[ 25%] Building C object CMakeFiles/main.dir/main.c.o', 'normal');
            addOutput('[ 50%] Building C object CMakeFiles/utils.dir/utils.c.o', 'normal');
            addOutput('[ 75%] Linking C executable main', 'normal');
            addOutput('[100%] Built target main', 'success');
            
            const executableName = `${projectName}${navigator.platform.includes('Win') ? '.exe' : ''}`;
            addOutput(`Success: ${buildDir}/${executableName}`, 'success');
            addOutput(`Build completed in 3.5s`, 'success');
            updateStatus('Build complete');
            
            addBuildArtifact(executableName, projectName);
            
            // Auto-create package after successful build
            setTimeout(() => {
                autoCreatePackage(projectName, executableName, 'cmake');
            }, 500);
        }, 1000);
    }, 1500);
}

function cleanBuild() {
    const projectFile = document.getElementById('projectFile').value;
    const buildMode = document.getElementById('buildMode').value;
    const lazbuildPath = document.getElementById('lazbuildPath').value;
    const projectName = document.getElementById('projectName').value;
    
    switchTab(2);
    addOutput('Cleaning build artifacts...', 'normal');
    
    if (buildMode === 'lazbuild' && projectFile) {
        addOutput(`Command: ${lazbuildPath} --clean ${projectFile}`, 'normal');
    }
    
    setTimeout(() => {
        addOutput('Removing object files (*.o, *.ppu)...', 'normal');
        addOutput('Removing compiled resources...', 'normal');
        addOutput('Removing executables from bin/...', 'normal');
        
        const artifactCount = projectStructure.buildArtifacts.length;
        projectStructure.buildArtifacts = [];
        
        if (artifactCount > 0) {
            addOutput(`✓ Removed ${artifactCount} artifact(s) from bin/`, 'success');
            
            if (projectFile) {
                const fileExt = projectFile.split('.').pop().toLowerCase();
                updateProjectExplorer(projectName, fileExt);
            }
        }
        
        addOutput('Clean complete', 'success');
        updateStatus('Clean complete');
    }, 500);
}

function rebuildAll() {
    cleanBuild();
    setTimeout(() => startBuild(), 1000);
}

// Configuration Functions
function saveConfig() {
    updateStatus('Configuration saved');
    addOutput('Build configuration saved', 'success');
}

function resetConfig() {
    document.getElementById('compilerOpts').value = '-O2 -MObjFPC -Scghi';
    document.getElementById('includePaths').value = './include;./lib';
    updateStatus('Configuration reset');
}

function detectFPC() {
    addOutput('Detecting FreePascal compiler...', 'normal');
    
    const possiblePaths = [
        '/opt/homebrew/bin/fpc',
        '/usr/local/bin/fpc',
        '/usr/bin/fpc',
        'fpc'
    ];
    
    setTimeout(() => {
        const fpcInput = document.getElementById('fpcPath');
        const detectedPath = possiblePaths[0];
        fpcInput.value = detectedPath;
        detectedPaths.fpc = detectedPath;
        
        addOutput(`✓ FreePascal detected: ${detectedPath}`, 'success');
        addOutput('  Version: FPC 3.2.2 (simulated)', 'normal');
        updateStatus('FPC detected');
    }, 500);
}

function detectLazbuild() {
    addOutput('Detecting Lazarus lazbuild...', 'normal');
    
    const possiblePaths = [
        '/Applications/Lazarus/lazbuild',
        '/Applications/lazarus/lazbuild',
        '/usr/local/bin/lazbuild',
        'lazbuild'
    ];
    
    setTimeout(() => {
        const lazbuildInput = document.getElementById('lazbuildPath');
        const detectedPath = possiblePaths[0];
        lazbuildInput.value = detectedPath;
        detectedPaths.lazbuild = detectedPath;
        
        addOutput(`✓ Lazarus lazbuild detected: ${detectedPath}`, 'success');
        addOutput('  Version: Lazarus 3.0 (simulated)', 'normal');
        updateStatus('Lazbuild detected');
    }, 500);
}

function detectCMake() {
    addOutput('Detecting CMake...', 'normal');
    
    const possiblePaths = [
        '/opt/homebrew/bin/cmake',
        '/usr/local/bin/cmake',
        '/usr/bin/cmake',
        'cmake'
    ];
    
    setTimeout(() => {
        const cmakeInput = document.getElementById('cmakePath');
        const detectedPath = possiblePaths[0];
        cmakeInput.value = detectedPath;
        detectedPaths.cmake = detectedPath;
        
        addOutput(`✓ CMake detected: ${detectedPath}`, 'success');
        addOutput('  Version: CMake 3.28.0 (simulated)', 'normal');
        updateStatus('CMake detected');
    }, 500);
}

// Navigation Functions
function setActiveNav(index) {
    const navItems = document.querySelectorAll('.nav-item');
    navItems.forEach((item, i) => {
        if (i === index) {
            item.classList.add('active');
        } else {
            item.classList.remove('active');
        }
    });

    switch(index) {
        case 0:
            addOutput('Navigating to Dashboard...', 'normal');
            switchTab(0);
            break;
        case 1:
            addOutput('Navigating to Projects...', 'normal');
            switchTab(3); // Switch to Projects tab
            break;
        case 2:
            addOutput('Navigating to Settings...', 'normal');
            switchTab(1);
            break;
    }
}

// Package Management Functions
function autoCreatePackage(projectName, executableName, buildType) {
    addOutput('--- Creating Distribution Package ---', 'normal');
    
    // Determine best package format based on platform
    let format = 'tar.gz';
    if (navigator.platform.includes('Mac')) {
        format = 'dmg';
    } else if (navigator.platform.includes('Win')) {
        format = 'zip';
    }
    
    const timestamp = new Date().toISOString().split('T')[0];
    const packageName = `${projectName}-${timestamp}`;
    
    addOutput(`Package format: ${format}`, 'normal');
    addOutput(`Package name: ${packageName}.${format}`, 'normal');
    addOutput('Packaging executable and resources...', 'normal');
    
    setTimeout(() => {
        const pkg = {
            id: Date.now(),
            name: packageName,
            format: format,
            executable: executableName,
            buildType: buildType,
            size: (Math.random() * 10 + 1).toFixed(2) + ' MB',
            date: new Date().toLocaleString(),
            projectName: projectName
        };
        
        projectStructure.packages.push(pkg);
        addOutput(`✓ Package created: ${packageName}.${format}`, 'success');
        addOutput(`  Size: ${pkg.size}`, 'normal');
        updateStatus('Package created');
        
        renderPackages();
        
        // Switch to Projects tab to show the package
        setTimeout(() => {
            switchTab(3);
            addOutput('Package ready in Projects tab', 'success');
        }, 1000);
    }, 1500);
}

function createPackage() {
    const packageName = document.getElementById('packageName').value;
    const format = document.getElementById('packageFormat').value;
    const projectName = document.getElementById('projectName').value;
    
    if (!packageName) {
        addOutput('Error: Please enter a package name', 'error');
        return;
    }
    
    if (projectStructure.buildArtifacts.length === 0) {
        addOutput('Error: No build artifacts available. Build a project first.', 'error');
        return;
    }
    
    switchTab(2); // Switch to Output
    addOutput('--- Creating Custom Package ---', 'normal');
    addOutput(`Package name: ${packageName}.${format}`, 'normal');
    addOutput(`Including artifacts: ${projectStructure.buildArtifacts.join(', ')}`, 'normal');
    
    setTimeout(() => {
        const pkg = {
            id: Date.now(),
            name: packageName,
            format: format,
            executable: projectStructure.buildArtifacts[0],
            buildType: 'manual',
            size: (Math.random() * 10 + 1).toFixed(2) + ' MB',
            date: new Date().toLocaleString(),
            projectName: projectName
        };
        
        projectStructure.packages.push(pkg);
        addOutput(`✓ Custom package created: ${packageName}.${format}`, 'success');
        addOutput(`  Size: ${pkg.size}`, 'normal');
        updateStatus('Package created');
        
        renderPackages();
        switchTab(3);
    }, 1000);
}

function renderPackages() {
    const packagesList = document.getElementById('packagesList');
    
    if (projectStructure.packages.length === 0) {
        packagesList.innerHTML = '<p style="color: #999; text-align: center; padding: 40px;">No packages created yet. Build a project and create a package.</p>';
        return;
    }
    
    let html = '';
    projectStructure.packages.forEach(pkg => {
        const icon = pkg.format === 'dmg' ? '💿' : pkg.format === 'zip' ? '📦' : '📚';
        html += `
            <div class="package-item">
                <div style="display: flex; align-items: center; flex: 1;">
                    <div class="package-icon">${icon}</div>
                    <div class="package-info">
                        <div class="package-name">${pkg.name}.${pkg.format}</div>
                        <div class="package-meta">
                            ${pkg.projectName} • ${pkg.size} • ${pkg.date} • Built with: ${pkg.buildType}
                        </div>
                    </div>
                </div>
                <div class="package-actions">
                    <button class="package-btn" onclick="downloadPackage(${pkg.id})">📥 Download</button>
                    <button class="package-btn delete" onclick="deletePackage(${pkg.id})">🗑️ Delete</button>
                </div>
            </div>
        `;
    });
    
    packagesList.innerHTML = html;
}

function downloadPackage(id) {
    const pkg = projectStructure.packages.find(p => p.id === id);
    if (!pkg) return;
    
    addOutput(`Downloading package: ${pkg.name}.${pkg.format}`, 'normal');
    addOutput(`✓ Download started: ${pkg.name}.${pkg.format} (${pkg.size})`, 'success');
    updateStatus(`Downloading ${pkg.name}.${pkg.format}`);
}

function deletePackage(id) {
    const pkg = projectStructure.packages.find(p => p.id === id);
    if (!pkg) return;
    
    if (confirm(`Delete package ${pkg.name}.${pkg.format}?`)) {
        projectStructure.packages = projectStructure.packages.filter(p => p.id !== id);
        addOutput(`✓ Package deleted: ${pkg.name}.${pkg.format}`, 'normal');
        renderPackages();
        updateStatus('Package deleted');
    }
}

function clearPackages() {
    if (projectStructure.packages.length === 0) {
        addOutput('No packages to clear', 'normal');
        return;
    }
    
    if (confirm(`Delete all ${projectStructure.packages.length} packages?`)) {
        const count = projectStructure.packages.length;
        projectStructure.packages = [];
        addOutput(`✓ Cleared ${count} package(s)`, 'success');
        renderPackages();
        updateStatus('All packages cleared');
    }
}

// Footer Functions
function showAbout() {
    addOutput('=== Kayte Build System v1.0.0 ===', 'normal');
    addOutput('A modern build system for FreePascal, Lazarus, and CMake projects', 'normal');
    addOutput('Supports: .lpi, .lpr, .dpr project files and CMakeLists.txt', 'success');
    addOutput('Package formats: .dmg, .tar.gz, .zip', 'success');
}

function showDocs() {
    addOutput('Opening documentation...', 'normal');
    addOutput('Visit: https://docs.kaytebuild.dev', 'normal');
}

function showSupport() {
    addOutput('Getting support...', 'normal');
    addOutput('Contact: support@kaytebuild.dev', 'normal');
}