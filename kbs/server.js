// server.js - Node.js Backend for Kayte Build System
const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const path = require('path');
const fs = require('fs').promises;

const app = express();
const PORT = 3001;

// Middleware
app.use(cors());
app.use(express.json());

// Utility function to execute shell commands
const execCommand = (command) => {
  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        resolve({ success: false, error: error.message, stderr });
      } else {
        resolve({ success: true, stdout, stderr });
      }
    });
  });
};

// Build Project
app.post('/api/build', async (req, res) => {
  try {
    const { projectFile, projectName, buildTarget, outputDir, buildMode } = req.body;
    
    console.log(`Building project: ${projectName}`);
    console.log(`Build mode: ${buildMode}`);
    console.log(`Target: ${buildTarget}`);
    
    let command = '';
    let output = '';
    
    switch (buildMode) {
      case 'lazbuild':
        if (!projectFile) {
          return res.status(400).json({ 
            success: false, 
            message: 'Error: No project file selected' 
          });
        }
        command = `lazbuild --build-mode=${buildTarget} "${projectFile}"`;
        break;
        
      case 'fpc':
        if (!projectFile) {
          return res.status(400).json({ 
            success: false, 
            message: 'Error: No project file selected' 
          });
        }
        command = `fpc -o"${outputDir}/${projectName}" "${projectFile}"`;
        break;
        
      case 'cmake':
        command = `cd "${path.dirname(projectFile || '.')}" && cmake -B build && cmake --build build`;
        break;
        
      default:
        return res.status(400).json({ 
          success: false, 
          message: 'Error: Invalid build mode' 
        });
    }
    
    output += `Executing: ${command}\n`;
    const result = await execCommand(command);
    
    if (result.success) {
      output += result.stdout;
      output += '\nBuild completed successfully!';
      res.json({ 
        success: true, 
        message: 'Build completed successfully',
        output: output
      });
    } else {
      output += result.stderr || result.error;
      output += '\nBuild failed!';
      res.json({ 
        success: false, 
        message: 'Build failed',
        output: output
      });
    }
  } catch (error) {
    console.error('Build error:', error);
    res.status(500).json({ 
      success: false, 
      message: `Build error: ${error.message}` 
    });
  }
});

// Detect FPC Path
app.get('/api/detect/fpc', async (req, res) => {
  try {
    const result = await execCommand('which fpc');
    if (result.success && result.stdout.trim()) {
      res.json({ 
        success: true, 
        path: result.stdout.trim() 
      });
    } else {
      // Try common paths
      const commonPaths = [
        '/usr/local/bin/fpc',
        '/usr/bin/fpc',
        '/opt/fpc/bin/fpc'
      ];
      
      for (const fpcPath of commonPaths) {
        try {
          await fs.access(fpcPath);
          return res.json({ success: true, path: fpcPath });
        } catch (e) {
          continue;
        }
      }
      
      res.json({ 
        success: false, 
        path: 'Not found',
        message: 'FPC not found in system PATH' 
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

// Detect Lazbuild Path
app.get('/api/detect/lazbuild', async (req, res) => {
  try {
    const result = await execCommand('which lazbuild');
    if (result.success && result.stdout.trim()) {
      res.json({ 
        success: true, 
        path: result.stdout.trim() 
      });
    } else {
      // Try common paths
      const commonPaths = [
        '/Applications/Lazarus/lazbuild',
        '/usr/local/bin/lazbuild',
        '/usr/bin/lazbuild',
        'C:\\lazarus\\lazbuild.exe'
      ];
      
      for (const lazbuildPath of commonPaths) {
        try {
          await fs.access(lazbuildPath);
          return res.json({ success: true, path: lazbuildPath });
        } catch (e) {
          continue;
        }
      }
      
      res.json({ 
        success: false, 
        path: 'Not found',
        message: 'Lazbuild not found' 
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

// Detect CMake Path
app.get('/api/detect/cmake', async (req, res) => {
  try {
    const result = await execCommand('which cmake');
    if (result.success && result.stdout.trim()) {
      res.json({ 
        success: true, 
        path: result.stdout.trim() 
      });
    } else {
      res.json({ 
        success: false, 
        path: 'Not found',
        message: 'CMake not found in system PATH' 
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

// Create Package
app.post('/api/package/create', async (req, res) => {
  try {
    const { format, name } = req.body;
    
    if (!name) {
      return res.status(400).json({ 
        success: false, 
        message: 'Package name is required' 
      });
    }
    
    console.log(`Creating package: ${name}.${format}`);
    
    let command = '';
    const outputPath = `./dist/${name}.${format}`;
    
    switch (format) {
      case 'dmg':
        command = `hdiutil create -volname "${name}" -srcfolder ./bin -ov -format UDZO "${outputPath}"`;
        break;
        
      case 'tar.gz':
        command = `tar -czf "${outputPath}" -C ./bin .`;
        break;
        
      case 'zip':
        command = `cd ./bin && zip -r "../${outputPath}" .`;
        break;
        
      default:
        return res.status(400).json({ 
          success: false, 
          message: 'Invalid package format' 
        });
    }
    
    // Ensure dist directory exists
    try {
      await fs.mkdir('./dist', { recursive: true });
    } catch (e) {
      // Directory might already exist
    }
    
    const result = await execCommand(command);
    
    if (result.success) {
      res.json({ 
        success: true, 
        message: `Package created: ${name}.${format}`,
        path: outputPath
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Package creation failed',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

// Git Operations
app.post('/api/git/init', async (req, res) => {
  try {
    const result = await execCommand('git init');
    if (result.success) {
      res.json({ 
        success: true, 
        message: 'Git repository initialized' 
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Failed to initialize repository',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

app.get('/api/git/status', async (req, res) => {
  try {
    const result = await execCommand('git status --short');
    if (result.success) {
      const status = result.stdout.trim() || 'Working tree clean';
      res.json({ 
        success: true, 
        status: status 
      });
    } else {
      res.json({ 
        success: false, 
        status: 'Not a git repository',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

app.post('/api/git/commit', async (req, res) => {
  try {
    const { message } = req.body;
    
    if (!message) {
      return res.status(400).json({ 
        success: false, 
        message: 'Commit message is required' 
      });
    }
    
    // Stage all changes first
    await execCommand('git add .');
    
    // Commit
    const result = await execCommand(`git commit -m "${message}"`);
    
    if (result.success) {
      res.json({ 
        success: true, 
        message: 'Changes committed successfully' 
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Commit failed',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

app.post('/api/git/branch', async (req, res) => {
  try {
    const { name } = req.body;
    
    if (!name) {
      return res.status(400).json({ 
        success: false, 
        message: 'Branch name is required' 
      });
    }
    
    const result = await execCommand(`git branch "${name}"`);
    
    if (result.success) {
      res.json({ 
        success: true, 
        message: `Branch '${name}' created successfully` 
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Failed to create branch',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

app.get('/api/git/branches', async (req, res) => {
  try {
    const result = await execCommand('git branch');
    if (result.success) {
      const branches = result.stdout
        .split('\n')
        .map(b => b.trim().replace('* ', ''))
        .filter(b => b);
      res.json({ 
        success: true, 
        branches: branches 
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Failed to list branches',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

app.post('/api/git/add-remote', async (req, res) => {
  try {
    const { url } = req.body;
    
    if (!url) {
      return res.status(400).json({ 
        success: false, 
        message: 'Remote URL is required' 
      });
    }
    
    const result = await execCommand(`git remote add origin "${url}"`);
    
    if (result.success) {
      res.json({ 
        success: true, 
        message: 'Remote added successfully' 
      });
    } else {
      res.json({ 
        success: false, 
        message: 'Failed to add remote',
        error: result.stderr || result.error
      });
    }
  } catch (error) {
    res.status(500).json({ 
      success: false, 
      message: error.message 
    });
  }
});

// Health check
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'ok', 
    version: '1.0.0',
    timestamp: new Date().toISOString()
  });
});

// Start server
app.listen(PORT, () => {
  console.log(`
╔═══════════════════════════════════════════════╗
║   Kayte Build System - Backend Server        ║
╠═══════════════════════════════════════════════╣
║   Server running on port ${PORT}               ║
║   API: http://localhost:${PORT}/api          ║
╚═══════════════════════════════════════════════╝
  `);
});

module.exports = app;