unit CLI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Bytecode, KayteParser, VirtualMachine, FPMarkdownProcessor;

type
  TCLIOptions = record
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    Verbose: Boolean;
    CompileKayte: Boolean;
    RunBytecode: Boolean;
    CreateWPPlugin: Boolean;
    BuildDocs: Boolean;
    InputFile: string;
    OutputFile: string;
  end;

  TCLIHandler = class
  private
    FOptions: TCLIOptions;
    FAppName: string;
    FAppVersion: string;
    procedure ShowHelp;
    procedure ShowVersion;
    procedure CompileKayteFile(const InputFile, OutputFile: string);
    procedure RunBytecodeFile(const BytecodeFile: string);
    procedure BuildDocsToHTML(const InputFile, OutputFile: string);
    procedure CreateWordPressPlugin(const KayteFile, OutputDir: string);
  public
    constructor Create(const AppName, AppVersion: string);
    procedure ParseArgs;
    procedure Execute;
  end;

implementation

constructor TCLIHandler.Create(const AppName, AppVersion: string);
begin
  FAppName := AppName;
  FAppVersion := AppVersion;
  FOptions.ShowHelp := False;
  FOptions.ShowVersion := False;
  FOptions.Verbose := False;
  FOptions.CompileKayte := False;
  FOptions.RunBytecode := False;
  FOptions.CreateWPPlugin := False;
  FOptions.BuildDocs := False;
  FOptions.InputFile := '';
  FOptions.OutputFile := '';
end;

procedure TCLIHandler.ShowHelp;
begin
  Writeln('Usage: ', FAppName, ' [OPTIONS] [FILE]');
  Writeln;
  Writeln('Options:');
  Writeln('  --help             Show this help message and exit');
  Writeln('  -v, --version      Show the version information and exit');
  Writeln('  --verbose          Run in verbose mode');
  Writeln('  --compile <file>   Compile a .kayte source file to bytecode');
  Writeln('  --run <file>       Run a bytecode (.bytecode) file');
  Writeln('  --create-plugin    Create a WordPress plugin with WebGL embedding of .kayte files');
  Writeln('  --build-docs       Build documentation (Markdown to HTML)');
  Writeln('  -o <file>          Specify the output file');
  Writeln;
end;

procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

procedure TCLIHandler.CompileKayteFile(const InputFile, OutputFile: string);
var
  BytecodeGen: TBytecodeGenerator;
begin
  if InputFile = '' then
  begin
    Writeln('Error: No input file specified for compilation.');
    Exit;
  end;

  if OutputFile = '' then
  begin
    Writeln('Error: No output file specified for bytecode.');
    Exit;
  end;

  BytecodeGen := TBytecodeGenerator.Create;
  try
    if FOptions.Verbose then
      Writeln('Compiling ', InputFile, ' to ', OutputFile);

    BytecodeGen.GenerateBytecode(InputFile, OutputFile);

    if FOptions.Verbose then
      Writeln('Compilation successful.');
  finally
    BytecodeGen.Free;
  end;
end;

procedure TCLIHandler.RunBytecodeFile(const BytecodeFile: string);
var
  VM: TVirtualMachine;
begin
  if BytecodeFile = '' then
  begin
    Writeln('Error: No bytecode file specified to run.');
    Exit;
  end
  else if not FileExists(BytecodeFile) then
  begin
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  VM := TVirtualMachine.Create;
  try
    if FOptions.Verbose then
      Writeln('Loading bytecode file: ', BytecodeFile);

    VM.LoadBytecode(BytecodeFile);

    if FOptions.Verbose then
      Writeln('Executing bytecode...');

    VM.Run;

    if FOptions.Verbose then
      Writeln('Execution completed successfully.');
  finally
    VM.Free;
  end;
end;

procedure TCLIHandler.BuildDocsToHTML(const InputFile, OutputFile: string);
begin
  if InputFile = '' then
  begin
    Writeln('Error: No input file specified for documentation build.');
    Exit;
  end;

  if OutputFile = '' then
  begin
    Writeln('Error: No output file specified for HTML.');
    Exit;
  end;

  if FOptions.Verbose then
    Writeln('Building documentation from ', InputFile, ' to ', OutputFile);

  ConvertMarkdownToHTML(InputFile, OutputFile);

  if FOptions.Verbose then
    Writeln('Documentation build completed successfully.');
end;

procedure TCLIHandler.CreateWordPressPlugin(const KayteFile, OutputDir: string);
var
  PluginDir, PHPFile, JSFile: string;
  JSContent, PHPContent: TStringList;
begin
  if KayteFile = '' then
  begin
    Writeln('Error: No .kayte file specified for WordPress plugin.');
    Exit;
  end;

  if OutputDir = '' then
  begin
    Writeln('Error: No output directory specified for WordPress plugin.');
    Exit;
  end;

  PluginDir := IncludeTrailingPathDelimiter(OutputDir) + 'kayte-webgl-plugin';
  if not DirectoryExists(PluginDir) then
    CreateDir(PluginDir);

  // Create the main plugin PHP file
PHPFile := PluginDir + PathDelim + 'kayte-webgl-plugin.php';
PHPContent := TStringList.Create;
try
  PHPContent.Add('<?php');
  PHPContent.Add('/*');
  PHPContent.Add(' * Plugin Name: Kayte WebGL Plugin');
  PHPContent.Add(' * Description: Embeds Kayte WebGL content into a WordPress page.');
  PHPContent.Add(' * Version: 1.0');
  PHPContent.Add(' */');
  PHPContent.Add('');
  PHPContent.Add('function kayte_webgl_embed() {');
  PHPContent.Add('    echo ''<div id="webgl-container"></div>'';');  // Fixed line
  PHPContent.Add('    echo ''<script src="'' . plugin_dir_url(__FILE__) . ''kayte-webgl.js"></script>'';');
  PHPContent.Add('}');
  PHPContent.Add('');
  PHPContent.Add('add_shortcode(''kayte_webgl'', ''kayte_webgl_embed'');');
  PHPContent.SaveToFile(PHPFile);
finally
  PHPContent.Free;
end;

  // Create the WebGL JavaScript file
  JSFile := PluginDir + PathDelim + 'kayte-webgl.js';
  JSContent := TStringList.Create;
  try
    JSContent.Add('function initWebGL() {');
    JSContent.Add('    var canvas = document.getElementById("webgl-container");');
    JSContent.Add('    // Add your WebGL code to render the .kayte content');
    JSContent.Add('    // This is where you will load and execute the .kayte file');
    JSContent.Add('}');
    JSContent.Add('');
    JSContent.Add('window.onload = initWebGL;');
    JSContent.SaveToFile(JSFile);
  finally
    JSContent.Free;
  end;

  if FOptions.Verbose then
    Writeln('WordPress plugin created successfully in: ', PluginDir);
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
begin
  I := 1;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '--help' then
      FOptions.ShowHelp := True
    else if (ParamStr(I) = '-v') or (ParamStr(I) = '--version') then
      FOptions.ShowVersion := True
    else if ParamStr(I) = '--verbose' then
      FOptions.Verbose := True
    else if ParamStr(I) = '--compile' then
    begin
      FOptions.CompileKayte := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end;
    end
    else if ParamStr(I) = '--create-plugin' then
    begin
      FOptions.CreateWPPlugin := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end;
    end
    else if ParamStr(I) = '--run' then
    begin
      FOptions.RunBytecode := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end;
    end
    else if ParamStr(I) = '--build-docs' then
      FOptions.BuildDocs := True
    else if ParamStr(I) = '-o' then
    begin
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.OutputFile := ParamStr(I);
      end;
    end
    else
      Writeln('Unknown option: ', ParamStr(I));

    Inc(I);
  end;
end;

procedure TCLIHandler.Execute;
begin
  if FOptions.ShowHelp then
  begin
    ShowHelp;
    Exit;
  end;

  if FOptions.ShowVersion then
  begin
    ShowVersion;
    Exit;
  end;

  if FOptions.CompileKayte then
  begin
    CompileKayteFile(FOptions.InputFile, FOptions.OutputFile);
    Exit;
  end;

  if FOptions.RunBytecode then
  begin
    RunBytecodeFile(FOptions.InputFile);
    Exit;
  end;

  if FOptions.BuildDocs then
  begin
    BuildDocsToHTML(FOptions.InputFile, FOptions.OutputFile);
    Exit;
  end;

  if FOptions.CreateWPPlugin then
  begin
    CreateWordPressPlugin(FOptions.InputFile, FOptions.OutputFile);
    Exit;
  end;

  Writeln('No action specified. Use --help to see available options.');
end;

end.

