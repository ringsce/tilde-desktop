Welcome to CTM Interpreter
==========================

CTM Interpreter is a multi-language interpreter and scripting environment built with Free Pascal. It supports multiple scripting languages and integrates with GitHub and Claude AI for enhanced development workflows.

## Features

- **Multiple Language Support:**
  - Simple CTM language (.ctm files)
  - Kayte language (.kayte files)
  - Full Pascal Script support (.pas files)
  
- **GitHub Integration:**
  - Clone and sync repositories
  - Fetch repository contents
  - Analyze code with AI
  
- **Claude AI Integration:**
  - Code analysis
  - Repository summarization
  - Context-aware assistance

## Project Structure

```
├── cli.pas               - GitHub & Claude AI integration unit
├── pascalscript.pas      - Pascal Script embedding unit
├── CTMInterpreter.pas    - Main interpreter program
└── ringsce.pas          - Repository management tool
```

## Requirements

- Free Pascal Compiler (FPC) 3.2.0 or later
- Lazarus IDE (optional, for GUI development)
- Pascal Script units (fp-units-fcl-base, fp-units-misc)
- OpenSSL libraries (for HTTPS support)

## Installation

### On Debian/Ubuntu:
```bash
sudo apt-get install fpc lazarus fp-units-fcl-base fp-units-misc libssl-dev
```

### On macOS:
```bash
brew install fpc lazarus
```

### On Windows:
Download and install from https://www.lazarus-ide.org/

## Compilation

### Build the interpreter:
```bash
fpc CTMInterpreter.pas
```

### Build the repository manager:
```bash
fpc ringsce.pas
```

### Build all:
```bash
make clean all
```

## Usage

### CTM Interpreter

Run scripts with the interpreter:
```bash
# Simple CTM/Kayte scripts
./CTMInterpreter script.ctm
./CTMInterpreter script.kayte

# Full Pascal scripts
./CTMInterpreter script.pas

# Force simple interpreter mode
./CTMInterpreter script.pas --simple
```

### Repository Manager

Manage GitHub repositories:
```bash
# Set environment variables
export GITHUB_TOKEN="your_token_here"
export ANTHROPIC_API_KEY="your_api_key_here"

# Clone all ringsce repositories
./ringsce --clone-all

# List available repositories
./ringsce --list-repos

# Sync a repository
./ringsce --sync ~/ringsce-repos/kcc

# Commit and push changes
./ringsce --commit ~/ringsce-repos/kcc "Updated compiler"

# Analyze repository with AI
./ringsce --analyze kcc

# Summarize all repositories
./ringsce --summarize-all

# Build from source
./ringsce --build ~/ringsce-repos/kcc

# Reset repository
./ringsce --reset ~/ringsce-repos/kcc
```

## Simple Language Syntax

### CTM/Kayte Language

```
let x = 10
let y = 20
print x
print y
```

Supported keywords:
- `let` - Variable assignment
- `print` - Output variable value

### Pascal Script

Full Pascal syntax support with standard library:
```pascal
program Example;
var
  i: Integer;
  s: string;
begin
  WriteLn('Hello, World!');
  for i := 1 to 5 do
  begin
    s := 'Count: ' + IntToStr(i);
    WriteLn(s);
  end;
end.
```

## API Integration

### GitHub API

The `TRingsceRepoReader` class provides:
- Repository listing
- File content fetching
- Repository contents browsing
- Authentication via GitHub tokens

### Claude AI API

The `TClaudeAIIntegration` class provides:
- Code analysis
- Repository summarization
- Context-aware prompts
- Uses Claude Sonnet 4.5 model

## Environment Variables

- `GITHUB_TOKEN` - GitHub personal access token for API access
- `ANTHROPIC_API_KEY` - Claude AI API key
- `HOME` - Used for default repository directory

## Documentation

- Free Pascal Documentation: https://www.freepascal.org/docs.html
- Lazarus Documentation: https://wiki.freepascal.org/Lazarus_Documentation
- Pascal Script: https://github.com/remobjects/pascalscript
- GitHub API: https://docs.github.com/en/rest
- Anthropic API: https://docs.anthropic.com/

## Examples

### Example 1: Simple Script
```ctm
let greeting = 42
print greeting
```

### Example 2: Pascal Script
```pascal
program Fibonacci;
var
  a, b, temp, i: Integer;
begin
  a := 0;
  b := 1;
  WriteLn('Fibonacci sequence:');
  for i := 1 to 10 do
  begin
    WriteLn(IntToStr(a));
    temp := a + b;
    a := b;
    b := temp;
  end;
end.
```

### Example 3: GitHub Integration
```pascal
uses cli;

var
  Reader: TRingsceRepoReader;
  RepoList: TStringList;
  i: Integer;

begin
  Reader := TRingsceRepoReader.Create;
  try
    RepoList := Reader.GetRepoList('ringsce');
    for i := 0 to RepoList.Count - 1 do
      WriteLn(RepoList[i]);
  finally
    Reader.Free;
    RepoList.Free;
  end;
end.
```

## Contributing

Contributions are welcome! Please follow these guidelines:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## Mailing List

For questions and discussions, join our community:
- Lazarus Mailing List: http://lists.lazarus.freepascal.org/mailman/listinfo/lazarus
- Free Pascal Forums: https://forum.lazarus.freepascal.org/

## Bug Reports

If you find bugs, please report them:
- Create an issue with detailed description
- Include error messages and system information
- Provide steps to reproduce

## License

This project follows the same licensing as Free Pascal and Lazarus:
- Free Pascal RTL: Modified LGPL
- LCL: Modified LGPL
- IDE and other tools: GPL

See individual source files for specific license information.

## Credits

Built with:
- [Free Pascal](https://www.freepascal.org) - Fast Object Pascal compiler
- [Lazarus IDE](https://www.lazarus-ide.org) - RAD tool for Free Pascal
- [Pascal Script](https://github.com/remobjects/pascalscript) - Scripting engine
- [Anthropic Claude](https://www.anthropic.com) - AI assistance

## Support

For support:
- Check the documentation
- Search existing issues
- Ask on the mailing list
- Create a new issue with details

---

**Current Version:** 1.0.0  
**Last Updated:** 2025  
**Author:** Ringsce Development Team
