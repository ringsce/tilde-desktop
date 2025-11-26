unit pascalscript;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uPSCompiler, uPSRuntime, uPSUtils;

type
  TPascalScriptEngine = class
  private
    FCompiler: TPSPascalCompiler;
    FRuntime: TPSExec;
    FOutput: TStringList;
    FLastError: string;
    procedure CompilerOnUses(Sender: TPSPascalCompiler; const Name: string);
    procedure RuntimeOnLineInfo(Sender: TPSExec; const FileName: string; Position, Row, Col: Cardinal);
    function CompilerOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function CompileScript(const Script: string): Boolean;
    function ExecuteScript: Boolean;
    function LoadAndExecute(const FileName: string): Boolean;
    procedure RegisterStandardLibrary;
    procedure RegisterCustomFunction(const FuncName: string; Proc: Pointer);
    property Output: TStringList read FOutput;
    property LastError: string read FLastError;
  end;

// Standard library functions that will be available in scripts
procedure ScriptWriteLn(const S: string);
procedure ScriptWrite(const S: string);
function ScriptReadLn: string;
function ScriptIntToStr(Value: Integer): string;
function ScriptStrToInt(const S: string): Integer;
function ScriptUpperCase(const S: string): string;
function ScriptLowerCase(const S: string): string;

implementation

var
  GlobalOutput: TStringList;

{ Standard Library Functions }

procedure ScriptWriteLn(const S: string);
begin
  if Assigned(GlobalOutput) then
    GlobalOutput.Add(S);
  WriteLn(S);
end;

procedure ScriptWrite(const S: string);
begin
  if Assigned(GlobalOutput) then
    GlobalOutput.Add(S);
  Write(S);
end;

function ScriptReadLn: string;
begin
  ReadLn(Result);
end;

function ScriptIntToStr(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

function ScriptStrToInt(const S: string): Integer;
begin
  Result := StrToInt(S);
end;

function ScriptUpperCase(const S: string): string;
begin
  Result := UpperCase(S);
end;

function ScriptLowerCase(const S: string): string;
begin
  Result := LowerCase(S);
end;

{ TPascalScriptEngine }

constructor TPascalScriptEngine.Create;
begin
  inherited Create;
  FCompiler := TPSPascalCompiler.Create;
  FRuntime := TPSExec.Create;
  FOutput := TStringList.Create;
  GlobalOutput := FOutput;
  FLastError := '';

  // Set up compiler callbacks
  FCompiler.OnUses := @CompilerOnUses;
  FCompiler.OnExportCheck := @CompilerOnExportCheck;

  // Set up runtime callbacks
  FRuntime.OnLineInfo := @RuntimeOnLineInfo;

  RegisterStandardLibrary;
end;

destructor TPascalScriptEngine.Destroy;
begin
  FOutput.Free;
  FRuntime.Free;
  FCompiler.Free;
  GlobalOutput := nil;
  inherited Destroy;
end;

procedure TPascalScriptEngine.CompilerOnUses(Sender: TPSPascalCompiler; const Name: string);
begin
  // Handle uses clauses - add imports here if needed
  if Name = 'SYSTEM' then
  begin
    // System unit is handled automatically
  end;
end;

procedure TPascalScriptEngine.RuntimeOnLineInfo(Sender: TPSExec; const FileName: string;
  Position, Row, Col: Cardinal);
begin
  // Called for each line during execution - useful for debugging
end;

function TPascalScriptEngine.CompilerOnExportCheck(Sender: TPSPascalCompiler;
  Proc: TPSInternalProcedure; const ProcDecl: string): Boolean;
begin
  // Allow all exports
  Result := True;
end;

procedure TPascalScriptEngine.RegisterStandardLibrary;
begin
  // Register standard functions
  FCompiler.AddDelphiFunction('procedure WriteLn(const S: string)');
  FCompiler.AddDelphiFunction('procedure Write(const S: string)');
  FCompiler.AddDelphiFunction('function ReadLn: string');
  FCompiler.AddDelphiFunction('function IntToStr(Value: Integer): string');
  FCompiler.AddDelphiFunction('function StrToInt(const S: string): Integer');
  FCompiler.AddDelphiFunction('function UpperCase(const S: string): string');
  FCompiler.AddDelphiFunction('function LowerCase(const S: string): string');

  // Register runtime functions
  RegisterDelphiFunction(@ScriptWriteLn, 'WRITELN', cdRegister);
  RegisterDelphiFunction(@ScriptWrite, 'WRITE', cdRegister);
  RegisterDelphiFunction(@ScriptReadLn, 'READLN', cdRegister);
  RegisterDelphiFunction(@ScriptIntToStr, 'INTTOSTR', cdRegister);
  RegisterDelphiFunction(@ScriptStrToInt, 'STRTOINT', cdRegister);
  RegisterDelphiFunction(@ScriptUpperCase, 'UPPERCASE', cdRegister);
  RegisterDelphiFunction(@ScriptLowerCase, 'LOWERCASE', cdRegister);
end;

procedure TPascalScriptEngine.RegisterCustomFunction(const FuncName: string; Proc: Pointer);
begin
  // Allow users to register their own functions
  RegisterDelphiFunction(Proc, FuncName, cdRegister);
end;

function TPascalScriptEngine.CompileScript(const Script: string): Boolean;
begin
  FLastError := '';
  FOutput.Clear;

  Result := FCompiler.Compile(Script);

  if not Result then
  begin
    FLastError := 'Compilation Error: ' + FCompiler.GetMsg(0);
    WriteLn(FLastError);
  end;
end;

function TPascalScriptEngine.ExecuteScript: Boolean;
var
  Data: string;
begin
  Result := False;
  FLastError := '';

  if not FCompiler.GetOutput(Data) then
  begin
    FLastError := 'Failed to get compiled output';
    WriteLn(FLastError);
    Exit;
  end;

  if not FRuntime.LoadData(Data) then
  begin
    FLastError := 'Failed to load compiled data';
    WriteLn(FLastError);
    Exit;
  end;

  Result := FRuntime.RunScript;

  if not Result then
  begin
    FLastError := 'Runtime Error: ' + FRuntime.ExecErrorToString;
    WriteLn(FLastError);
  end;
end;

function TPascalScriptEngine.LoadAndExecute(const FileName: string): Boolean;
var
  Script: TStringList;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    FLastError := 'File not found: ' + FileName;
    WriteLn(FLastError);
    Exit;
  end;

  Script := TStringList.Create;
  try
    Script.LoadFromFile(FileName);

    if CompileScript(Script.Text) then
      Result := ExecuteScript;
  finally
    Script.Free;
  end;
end;

end.
