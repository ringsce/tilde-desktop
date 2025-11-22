program CTMInterpreter;

uses
  SysUtils, Classes, fphttpclient, opensslsockets, cli, pascalscript;

type
  TTokenKind = (tkIdentifier, tkNumber, tkOperator, tkKeyword, tkEOF);

  TToken = record
    Kind: TTokenKind;
    Value: string;
  end;

  TSymbolTable = record
    Name: string;
    Value: Integer;
  end;

const
  Keywords: array[0..1] of string = ('print', 'let');

var
  Source: string;
  Position: Integer = 1;
  CurrentToken: TToken;
  Symbols: array[0..99] of TSymbolTable;
  SymbolCount: Integer = 0;
  Breakpoint: Integer = -1;

function FileExistsCustom(const FileName: string): Boolean;
begin
  Result := FileExists(FileName);
end;

function LoadFile(const FileName: string): string;
var
  FileContent: TStringList;
begin
  if not FileExists(FileName) then
    raise Exception.Create('Error: File not found - ' + FileName);

  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    Result := FileContent.Text;
  finally
    FileContent.Free;
  end;
end;

function NextChar: Char;
begin
  if Position <= Length(Source) then
    Result := Source[Position]
  else
    Result := #0;
end;

procedure Advance;
begin
  Inc(Position);
end;

procedure DownloadFromGitHub(const URL, SavePath: string);
var
  HTTP: TFPHTTPClient;
begin
  HTTP := TFPHTTPClient.Create(nil);
  try
    Writeln('Downloading: ', URL);
    HTTP.Get(URL, SavePath);
    Writeln('Download complete: ', SavePath);
  except
    on E: Exception do
      Writeln('Error downloading file: ', E.Message);
  end;
  HTTP.Free;
end;

function IsKeyword(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Keywords) do
    if Keywords[I] = S then
      Exit(True);
  Exit(False);
end;

function GetNextToken: TToken;
var
  Temp: string;
begin
  while (NextChar <> #0) and (NextChar in [' ', #10, #13]) do
    Advance;

  if NextChar = #0 then
  begin
    Result.Kind := tkEOF;
    Exit;
  end;

  if NextChar in ['A'..'Z', 'a'..'z'] then
  begin
    Temp := '';
    while NextChar in ['A'..'Z', 'a'..'z'] do
    begin
      Temp := Temp + NextChar;
      Advance;
    end;
    if IsKeyword(Temp) then
      Result.Kind := tkKeyword
    else
      Result.Kind := tkIdentifier;
    Result.Value := Temp;
    Exit;
  end;

  if NextChar in ['0'..'9'] then
  begin
    Temp := '';
    while NextChar in ['0'..'9'] do
    begin
      Temp := Temp + NextChar;
      Advance;
    end;
    Result.Kind := tkNumber;
    Result.Value := Temp;
    Exit;
  end;

  if NextChar in ['+', '-', '=', ';'] then
  begin
    Result.Kind := tkOperator;
    Result.Value := NextChar;
    Advance;
    Exit;
  end;

  Result.Kind := tkEOF;
end;

procedure Match(Expected: TTokenKind);
begin
  if CurrentToken.Kind = Expected then
    CurrentToken := GetNextToken
  else
    raise Exception.Create('Unexpected token: ' + CurrentToken.Value);
end;

function EvaluateExpression: Integer;
begin
  if CurrentToken.Kind = tkNumber then
  begin
    Result := StrToInt(CurrentToken.Value);
    Match(tkNumber);
  end
  else
    raise Exception.Create('Invalid expression');
end;

procedure ExecuteStatement;
var
  VarName: string;
  Value: Integer;
  I: Integer;
begin
  if CurrentToken.Kind = tkKeyword then
  begin
    if CurrentToken.Value = 'let' then
    begin
      Match(tkKeyword);
      VarName := CurrentToken.Value;
      Match(tkIdentifier);
      Match(tkOperator); // Expect '='
      Value := EvaluateExpression;
      for I := 0 to SymbolCount - 1 do
        if Symbols[I].Name = VarName then
        begin
          Symbols[I].Value := Value;
          Exit;
        end;
      Symbols[SymbolCount].Name := VarName;
      Symbols[SymbolCount].Value := Value;
      Inc(SymbolCount);
    end
    else if CurrentToken.Value = 'print' then
    begin
      Match(tkKeyword);
      VarName := CurrentToken.Value;
      Match(tkIdentifier);
      for I := 0 to SymbolCount - 1 do
        if Symbols[I].Name = VarName then
        begin
          Writeln(VarName, ' = ', Symbols[I].Value);
          Exit;
        end;
      Writeln('Error: Undefined variable ', VarName);
    end;
  end;
end;

procedure Debugger;
var
  Step: string;
begin
  Write('Debugger - Step (Enter), Break (b line), Continue (c): ');
  Readln(Step);
  if Step = 'b' then
  begin
    Write('Set breakpoint at line: ');
    Readln(Breakpoint);
  end;
end;

procedure Parse;
var
  Line: Integer;
begin
  Line := 1;
  CurrentToken := GetNextToken;
  while CurrentToken.Kind <> tkEOF do
  begin
    if (Breakpoint <> -1) and (Line = Breakpoint) then
      Debugger;
    ExecuteStatement;
    Inc(Line);
  end;
end;

var
  FileName: string;

begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: CTMInterpreter <filename .kayte/.pas/.c/.ctm/.h>');
    Halt(1);
  end;

  FileName := ParamStr(1);
  if not FileExistsCustom(FileName) then
  begin
    Writeln('Error: File not found - ', FileName);
    Halt(1);
  end;

  try
    Source := LoadFile(FileName);
    Position := 1;
    Parse;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
