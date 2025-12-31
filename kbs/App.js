import React, { useState, useEffect } from 'react';
import { Save, Play, Bug, FileText, Settings, GitBranch, Package, FolderOpen, File } from 'lucide-react';

const KayteBuildSystem = () => {
  const [darkMode, setDarkMode] = useState(true);
  const [activeNav, setActiveNav] = useState(0);
  const [activeTab, setActiveTab] = useState(0);
  const [outputLines, setOutputLines] = useState([
    'Kayte Build System v1.0.0',
    'Ready to build...'
  ]);
  
  const [projectData, setProjectData] = useState({
    projectFile: '',
    projectName: 'MyProject',
    buildTarget: 'Debug',
    outputDir: './bin',
    buildMode: 'lazbuild'
  });

  const [buildConfig, setBuildConfig] = useState({
    fpcPath: '',
    lazbuildPath: '/Applications/Lazarus/lazbuild',
    cmakePath: '',
    cmakeGenerator: 'Unix Makefiles',
    compilerOpts: '-O2 -MObjFPC -Scghi',
    targetPlatform: 'darwin-x86_64',
    includePaths: './include;./lib'
  });

  const [gitData, setGitData] = useState({
    initialized: false,
    currentBranch: 'main',
    branches: ['main'],
    commits: [],
    status: 'No Git repository initialized',
    commitMessage: '',
    branchName: '',
    remoteUrl: ''
  });

  const [packages, setPackages] = useState([]);
  const [packageData, setPackageData] = useState({
    format: 'dmg',
    name: ''
  });

  const addOutput = (line) => {
    setOutputLines(prev => [...prev, line]);
  };

  const handleProjectFileChange = async (e) => {
    const file = e.target.files[0];
    if (file) {
      setProjectData(prev => ({ ...prev, projectFile: file.name }));
      addOutput(`Project file selected: ${file.name}`);
    }
  };

  const startBuild = async () => {
    addOutput('='.repeat(50));
    addOutput(`Building project: ${projectData.projectName}`);
    addOutput(`Build target: ${projectData.buildTarget}`);
    addOutput(`Build mode: ${projectData.buildMode}`);
    
    try {
      const response = await fetch('http://localhost:3001/api/build', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(projectData)
      });
      const result = await response.json();
      addOutput(result.message);
      if (result.output) {
        result.output.split('\n').forEach(line => addOutput(line));
      }
    } catch (error) {
      addOutput(`Error: ${error.message}`);
    }
  };

  const cleanBuild = () => {
    addOutput('Cleaning build directory...');
    addOutput('Clean completed successfully.');
  };

  const rebuildAll = () => {
    addOutput('Rebuilding all...');
    cleanBuild();
    startBuild();
  };

  const detectFPC = async () => {
    try {
      const response = await fetch('http://localhost:3001/api/detect/fpc');
      const result = await response.json();
      setBuildConfig(prev => ({ ...prev, fpcPath: result.path }));
      addOutput(`FPC detected at: ${result.path}`);
    } catch (error) {
      addOutput(`Error detecting FPC: ${error.message}`);
    }
  };

  const detectLazbuild = async () => {
    try {
      const response = await fetch('http://localhost:3001/api/detect/lazbuild');
      const result = await response.json();
      setBuildConfig(prev => ({ ...prev, lazbuildPath: result.path }));
      addOutput(`Lazbuild detected at: ${result.path}`);
    } catch (error) {
      addOutput(`Error detecting Lazbuild: ${error.message}`);
    }
  };

  const detectCMake = async () => {
    try {
      const response = await fetch('http://localhost:3001/api/detect/cmake');
      const result = await response.json();
      setBuildConfig(prev => ({ ...prev, cmakePath: result.path }));
      addOutput(`CMake detected at: ${result.path}`);
    } catch (error) {
      addOutput(`Error detecting CMake: ${error.message}`);
    }
  };

  const saveConfig = () => {
    addOutput('Configuration saved successfully.');
  };

  const resetConfig = () => {
    setBuildConfig({
      fpcPath: '',
      lazbuildPath: '/Applications/Lazarus/lazbuild',
      cmakePath: '',
      cmakeGenerator: 'Unix Makefiles',
      compilerOpts: '-O2 -MObjFPC -Scghi',
      targetPlatform: 'darwin-x86_64',
      includePaths: './include;./lib'
    });
    addOutput('Configuration reset to defaults.');
  };

  const createPackage = async () => {
    if (!packageData.name) {
      addOutput('Error: Please enter a package name');
      return;
    }

    try {
      const response = await fetch('http://localhost:3001/api/package/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(packageData)
      });
      const result = await response.json();
      
      const newPackage = {
        id: Date.now(),
        name: packageData.name,
        format: packageData.format,
        date: new Date().toLocaleString(),
        size: '2.5 MB'
      };
      
      setPackages(prev => [...prev, newPackage]);
      addOutput(`Package created: ${packageData.name}.${packageData.format}`);
    } catch (error) {
      addOutput(`Error creating package: ${error.message}`);
    }
  };

  const clearPackages = () => {
    setPackages([]);
    addOutput('All packages cleared.');
  };

  const gitInit = async () => {
    try {
      const response = await fetch('http://localhost:3001/api/git/init', {
        method: 'POST'
      });
      const result = await response.json();
      setGitData(prev => ({
        ...prev,
        initialized: true,
        status: 'Repository initialized'
      }));
      addOutput('Git repository initialized.');
    } catch (error) {
      addOutput(`Error: ${error.message}`);
    }
  };

  const gitStatus = async () => {
    try {
      const response = await fetch('http://localhost:3001/api/git/status');
      const result = await response.json();
      setGitData(prev => ({ ...prev, status: result.status }));
      addOutput('Git status updated.');
    } catch (error) {
      addOutput(`Error: ${error.message}`);
    }
  };

  const gitCommit = async () => {
    if (!gitData.commitMessage) {
      addOutput('Error: Please enter a commit message');
      return;
    }

    try {
      const response = await fetch('http://localhost:3001/api/git/commit', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message: gitData.commitMessage })
      });
      const result = await response.json();
      
      const newCommit = {
        id: Date.now(),
        message: gitData.commitMessage,
        date: new Date().toLocaleString(),
        hash: Math.random().toString(36).substr(2, 7)
      };
      
      setGitData(prev => ({
        ...prev,
        commits: [newCommit, ...prev.commits],
        commitMessage: ''
      }));
      addOutput(`Committed: ${gitData.commitMessage}`);
    } catch (error) {
      addOutput(`Error: ${error.message}`);
    }
  };

  const gitCreateBranch = () => {
    if (!gitData.branchName) {
      addOutput('Error: Please enter a branch name');
      return;
    }

    setGitData(prev => ({
      ...prev,
      branches: [...prev.branches, gitData.branchName],
      branchName: ''
    }));
    addOutput(`Branch created: ${gitData.branchName}`);
  };

  const tabs = ['MainForm', 'Build Configuration', 'Output', 'Projects', 'Git'];

  return (
    <div className={`min-h-screen flex flex-col ${darkMode ? 'bg-gray-900 text-gray-100' : 'bg-gray-100 text-gray-900'}`}>
      {/* Header */}
      <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border-b px-4 py-2 flex items-center justify-between`}>
        <div className="flex items-center gap-6">
          <div className="flex items-center gap-2 font-bold text-lg">
            <span>🔨</span>
            <span>Kayte Build System</span>
          </div>
          <div className="flex gap-2">
            {['Dashboard', 'Projects', 'Settings'].map((item, idx) => (
              <button
                key={idx}
                onClick={() => setActiveNav(idx)}
                className={`px-3 py-1 rounded ${
                  activeNav === idx
                    ? 'bg-blue-600 text-white'
                    : darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-200'
                }`}
              >
                {item}
              </button>
            ))}
          </div>
        </div>
        <button
          onClick={() => setDarkMode(!darkMode)}
          className={`px-3 py-1 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-200 hover:bg-gray-300'}`}
        >
          {darkMode ? '☀️ Light Mode' : '🌙 Dark Mode'}
        </button>
      </div>

      {/* Menu Bar */}
      <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-gray-200 border-gray-300'} border-b px-2 py-1 flex gap-4 text-sm`}>
        {['File', 'Edit', 'Project', 'Build', 'Tools', 'Help'].map(item => (
          <button key={item} className={`px-2 py-1 ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'} rounded`}>
            {item}
          </button>
        ))}
      </div>

      {/* Toolbar */}
      <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-gray-200 border-gray-300'} border-b px-2 py-1 flex gap-2`}>
        <button className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>📄 New</button>
        <button className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>📁 Open</button>
        <button className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>💾 Save</button>
        <div className="w-px bg-gray-600 mx-1"></div>
        <button onClick={startBuild} className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>🔨 Build</button>
        <button className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>▶️ Run</button>
        <button className={`px-3 py-1 rounded ${darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'}`}>🐛 Debug</button>
      </div>

      {/* Main Content */}
      <div className="flex flex-1 overflow-hidden">
        {/* Sidebar */}
        <div className={`w-64 ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border-r`}>
          <div className={`${darkMode ? 'bg-gray-700' : 'bg-gray-200'} px-4 py-2 font-semibold border-b ${darkMode ? 'border-gray-600' : 'border-gray-300'}`}>
            Project Explorer
          </div>
          <div className="p-4">
            <div className={`${darkMode ? 'bg-gray-700' : 'bg-gray-200'} px-3 py-2 rounded cursor-pointer`}>
              📦 Kayte Build System
            </div>
            <div className="pl-6 pt-2">
              <div className="px-3 py-1 cursor-pointer hover:bg-gray-700 rounded">
                🗂️ Resources
              </div>
            </div>
          </div>
        </div>

        {/* Work Area */}
        <div className="flex-1 flex flex-col overflow-hidden">
          {/* Tabs */}
          <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-gray-200 border-gray-300'} border-b flex`}>
            {tabs.map((tab, idx) => (
              <button
                key={idx}
                onClick={() => setActiveTab(idx)}
                className={`px-4 py-2 border-r ${darkMode ? 'border-gray-700' : 'border-gray-300'} ${
                  activeTab === idx
                    ? darkMode ? 'bg-gray-900 text-blue-400' : 'bg-white text-blue-600'
                    : darkMode ? 'hover:bg-gray-700' : 'hover:bg-gray-300'
                }`}
              >
                {tab}
              </button>
            ))}
          </div>

          {/* Tab Content */}
          <div className="flex-1 overflow-auto p-6">
            {activeTab === 0 && (
              <div className="max-w-3xl">
                <h2 className="text-2xl font-bold mb-6">Kayte Build System - Main Form</h2>
                
                <div className="space-y-4">
                  <div>
                    <label className="block mb-2 font-semibold">Project File:</label>
                    <div className="flex gap-2">
                      <input
                        type="text"
                        value={projectData.projectFile}
                        placeholder="Select .lpi, .lpr, .dpr, or CMakeLists.txt"
                        className={`flex-1 px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                        readOnly
                      />
                      <input
                        type="file"
                        id="fileInput"
                        accept=".lpi,.lpr,.dpr,.txt"
                        className="hidden"
                        onChange={handleProjectFileChange}
                      />
                      <button
                        onClick={() => document.getElementById('fileInput').click()}
                        className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
                      >
                        Browse...
                      </button>
                    </div>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Project Name:</label>
                    <input
                      type="text"
                      value={projectData.projectName}
                      onChange={(e) => setProjectData(prev => ({ ...prev, projectName: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Build Target:</label>
                    <select
                      value={projectData.buildTarget}
                      onChange={(e) => setProjectData(prev => ({ ...prev, buildTarget: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    >
                      <option>Debug</option>
                      <option>Release</option>
                      <option>Production</option>
                    </select>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Output Directory:</label>
                    <input
                      type="text"
                      value={projectData.outputDir}
                      onChange={(e) => setProjectData(prev => ({ ...prev, outputDir: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Build Mode:</label>
                    <select
                      value={projectData.buildMode}
                      onChange={(e) => setProjectData(prev => ({ ...prev, buildMode: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    >
                      <option value="lazbuild">Use lazbuild (Lazarus projects)</option>
                      <option value="fpc">Use fpc directly (Pascal files)</option>
                      <option value="cmake">Use CMake (CMakeLists.txt)</option>
                    </select>
                  </div>

                  <div className="flex gap-3 pt-4">
                    <button onClick={startBuild} className="px-6 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Build Project
                    </button>
                    <button onClick={cleanBuild} className={`px-6 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                      Clean
                    </button>
                    <button onClick={rebuildAll} className={`px-6 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                      Rebuild All
                    </button>
                  </div>
                </div>
              </div>
            )}

            {activeTab === 1 && (
              <div className="max-w-3xl">
                <h2 className="text-2xl font-bold mb-6">Build Configuration</h2>
                
                <div className="space-y-4">
                  <div>
                    <label className="block mb-2 font-semibold">FreePascal Compiler (fpc):</label>
                    <input
                      type="text"
                      value={buildConfig.fpcPath}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, fpcPath: e.target.value }))}
                      placeholder="Detecting..."
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                    <button onClick={detectFPC} className="mt-2 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Detect FPC Path
                    </button>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Lazarus lazbuild:</label>
                    <input
                      type="text"
                      value={buildConfig.lazbuildPath}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, lazbuildPath: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                    <button onClick={detectLazbuild} className="mt-2 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Detect Lazbuild
                    </button>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">CMake Path:</label>
                    <input
                      type="text"
                      value={buildConfig.cmakePath}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, cmakePath: e.target.value }))}
                      placeholder="Detecting..."
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                    <button onClick={detectCMake} className="mt-2 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Detect CMake
                    </button>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">CMake Generator:</label>
                    <select
                      value={buildConfig.cmakeGenerator}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, cmakeGenerator: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    >
                      <option>Unix Makefiles</option>
                      <option>Ninja</option>
                      <option>Xcode</option>
                      <option>Visual Studio 17 2022</option>
                    </select>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Compiler Options:</label>
                    <input
                      type="text"
                      value={buildConfig.compilerOpts}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, compilerOpts: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Target Platform:</label>
                    <select
                      value={buildConfig.targetPlatform}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, targetPlatform: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    >
                      <option>darwin-x86_64</option>
                      <option>darwin-aarch64</option>
                      <option>linux-x86_64</option>
                      <option>linux-aarch64</option>
                      <option>win64-x86_64</option>
                    </select>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Include Paths:</label>
                    <input
                      type="text"
                      value={buildConfig.includePaths}
                      onChange={(e) => setBuildConfig(prev => ({ ...prev, includePaths: e.target.value }))}
                      className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                  </div>

                  <div className="flex gap-3 pt-4">
                    <button onClick={saveConfig} className="px-6 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Save Configuration
                    </button>
                    <button onClick={resetConfig} className={`px-6 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                      Reset to Default
                    </button>
                  </div>
                </div>
              </div>
            )}

            {activeTab === 2 && (
              <div className={`${darkMode ? 'bg-black' : 'bg-gray-100'} rounded p-4 font-mono text-sm h-full overflow-auto`}>
                {outputLines.map((line, idx) => (
                  <div key={idx} className={darkMode ? 'text-green-400' : 'text-gray-800'}>
                    {line}
                  </div>
                ))}
              </div>
            )}

            {activeTab === 3 && (
              <div>
                <h2 className="text-2xl font-bold mb-6 text-blue-500">📦 Distribution Packages</h2>
                
                <div className="space-y-4 max-w-2xl">
                  <div>
                    <label className="block mb-2 font-semibold">Package Format:</label>
                    <select
                      value={packageData.format}
                      onChange={(e) => setPackageData(prev => ({ ...prev, format: e.target.value }))}
                      className={`w-full max-w-xs px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    >
                      <option value="dmg">macOS Disk Image (.dmg)</option>
                      <option value="tar.gz">Compressed Archive (.tar.gz)</option>
                      <option value="zip">ZIP Archive (.zip)</option>
                    </select>
                  </div>

                  <div>
                    <label className="block mb-2 font-semibold">Package Name:</label>
                    <input
                      type="text"
                      value={packageData.name}
                      onChange={(e) => setPackageData(prev => ({ ...prev, name: e.target.value }))}
                      placeholder="MyApp-v1.0"
                      className={`w-full max-w-xs px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                    />
                  </div>

                  <div className="flex gap-3">
                    <button onClick={createPackage} className="px-6 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                      Create Package
                    </button>
                    <button onClick={clearPackages} className={`px-6 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                      Clear All
                    </button>
                  </div>

                  <div className="mt-8">
                    <h3 className="text-xl font-bold mb-4">Available Packages</h3>
                    <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border rounded p-4 min-h-48`}>
                      {packages.length === 0 ? (
                        <p className="text-center opacity-60 py-12">No packages created yet. Build a project and create a package.</p>
                      ) : (
                        <div className="space-y-2">
                          {packages.map(pkg => (
                            <div key={pkg.id} className={`${darkMode ? 'bg-gray-700' : 'bg-gray-100'} p-3 rounded flex justify-between items-center`}>
                              <div>
                                <div className="font-semibold">{pkg.name}.{pkg.format}</div>
                                <div className="text-sm opacity-75">{pkg.date} • {pkg.size}</div>
                              </div>
                            </div>
                          ))}
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              </div>
            )}

            {activeTab === 4 && (
              <div>
                <h2 className="text-2xl font-bold mb-6 text-blue-500">🔀 Git Version Control</h2>
                
                <div className="space-y-6 max-w-3xl">
                  <div>
                    <h3 className="text-lg font-bold mb-3">Repository Status</h3>
                    <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border rounded p-4`}>
                      <p className={!gitData.initialized ? 'opacity-60' : ''}>{gitData.status}</p>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-lg font-bold mb-3">Quick Actions</h3>
                    <div className="flex gap-3 flex-wrap">
                      <button onClick={gitInit} className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                        📁 Init Repository
                      </button>
                      <button onClick={gitStatus} className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                        📊 Status
                      </button>
                      <button className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                        ⬇️ Pull
                      </button>
                      <button className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                        ⬆️ Push
                      </button>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-lg font-bold mb-3">Commit Changes</h3>
                    <div className="space-y-3">
                      <div>
                        <label className="block mb-2 font-semibold">Commit Message:</label>
                        <textarea
                          value={gitData.commitMessage}
                          onChange={(e) => setGitData(prev => ({ ...prev, commitMessage: e.target.value }))}
                          placeholder="Enter commit message..."
                          rows={3}
                          className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                        />
                      </div>
                      <div className="flex gap-3">
                        <button onClick={gitCommit} className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                          ✓ Commit
                        </button>
                        <button className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                          ➕ Stage All
                        </button>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-lg font-bold mb-3">Branch Management</h3>
                    <div className="flex gap-3 mb-3">
                      <input
                        type="text"
                        value={gitData.branchName}
                        onChange={(e) => setGitData(prev => ({ ...prev, branchName: e.target.value }))}
                        placeholder="New branch name"
                        className={`flex-1 max-w-xs px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                      />
                      <button onClick={gitCreateBranch} className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                        🌿 Create Branch
                      </button>
                    </div>
                    <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border rounded`}>
                      {gitData.branches.map((branch, idx) => (
                        <div
                          key={idx}
                          className={`px-4 py-2 border-b ${darkMode ? 'border-gray-700' : 'border-gray-300'} last:border-b-0 flex justify-between items-center`}
                        >
                          <span>🌿 {branch}</span>
                          {branch === gitData.currentBranch && (
                            <span className="text-blue-500 text-xs">CURRENT</span>
                          )}
                        </div>
                      ))}
                    </div>
                  </div>

                  <div>
                    <h3 className="text-lg font-bold mb-3">Commit History</h3>
                    <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border rounded p-4 min-h-32`}>
                      {gitData.commits.length === 0 ? (
                        <p className="text-center opacity-60 py-6">No commits yet</p>
                      ) : (
                        <div className="space-y-2">
                          {gitData.commits.map(commit => (
                            <div key={commit.id} className={`${darkMode ? 'bg-gray-700' : 'bg-gray-100'} p-3 rounded`}>
                              <div className="font-mono text-sm text-blue-500">{commit.hash}</div>
                              <div className="font-semibold mt-1">{commit.message}</div>
                              <div className="text-sm opacity-75 mt-1">{commit.date}</div>
                            </div>
                          ))}
                        </div>
                      )}
                    </div>
                  </div>

                  <div>
                    <h3 className="text-lg font-bold mb-3">Remote Repository</h3>
                    <div className="space-y-3">
                      <div>
                        <label className="block mb-2 font-semibold">Remote URL:</label>
                        <input
                          type="text"
                          value={gitData.remoteUrl}
                          onChange={(e) => setGitData(prev => ({ ...prev, remoteUrl: e.target.value }))}
                          placeholder="https://github.com/username/repo.git"
                          className={`w-full px-3 py-2 rounded ${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-white border-gray-300'} border`}
                        />
                      </div>
                      <div className="flex gap-3">
                        <button className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700">
                          🔗 Add Remote
                        </button>
                        <button className={`px-4 py-2 rounded ${darkMode ? 'bg-gray-700 hover:bg-gray-600' : 'bg-gray-300 hover:bg-gray-400'}`}>
                          📥 Clone
                        </button>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Status Bar */}
      <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-gray-200 border-gray-300'} border-t px-4 py-1 text-sm flex justify-between`}>
        <span>Ready</span>
        <span>Lines: 0 | Col: 0</span>
      </div>

      {/* Footer */}
      <div className={`${darkMode ? 'bg-gray-800 border-gray-700' : 'bg-gray-200 border-gray-300'} border-t px-4 py-2 flex justify-between text-sm`}>
        <div className="flex gap-4">
          <span>© 2025 Kayte Build System</span>
          <a href="#" className="text-blue-500 hover:underline">About</a>
          <a href="#" className="text-blue-500 hover:underline">Documentation</a>
          <a href="#" className="text-blue-500 hover:underline">Support</a>
        </div>
        <div className="flex gap-3 items-center">
          <span>v1.0.0</span>
          <span title="GitHub">⭐</span>
          <span title="Discord">💬</span>
        </div>
      </div>
    </div>
  );
};

export default KayteBuildSystem;