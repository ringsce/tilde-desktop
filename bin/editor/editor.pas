program Editor;

{$mode objfpc}{$H+}

uses
  Crt, SysUtils;

const
  CMD_X = 'X';
  CMD_T = 'T';

var
  EditorRunning: Boolean;
  CurrentFileName: string;
  Buffer: TStringList;

procedure ShowHelp;
begin
  WriteLn('Commands:');
  WriteLn('  Cmd + X : Exit');
  WriteLn('  Cmd + T : New File');
  WriteLn('  Cmd + S : Save File');
end;

procedure NewFile;
begin
  CurrentFileName := '';
  Buffer.Clear;
  WriteLn('New file created. Start typing...');
end;

procedure SaveFile;
var
  FileName: string;
begin
  if CurrentFileName = '' then
  begin
    Write('Enter file name to save: ');
    ReadLn(FileName);
    CurrentFileName := FileName;
  end;

  try
    Buffer.SaveToFile(CurrentFileName);
    WriteLn('File saved as "', CurrentFileName, '"');
  except
    on E: Exception do
      WriteLn('Error saving file: ', E.Message);
  end;
end;

procedure ExitEditor;
begin
  WriteLn('Exiting editor...');
  EditorRunning := False;
end;

procedure HandleInput;
var
  Key: Char;
  Line: string;
begin
  Write('> ');
  while EditorRunning do
  begin
    Key := ReadKey;

    if Key = #13 then
    begin
      Buffer.Add(Line);
      Line := '';
      WriteLn;
      Write('> ');
    end
    else if (Ord(Key) = 27) then
    begin
      Key := ReadKey; // Skip '['
      Key := ReadKey;
      if Key = CMD_X then
        ExitEditor
      else if Key = CMD_T then
        NewFile;
    end
    else
    begin
      Line := Line + Key;
      Write(Key);
    end;
  end;
end;

begin
  { Initialize }
  Buffer := TStringList.Create;
  EditorRunning := True;

  { Display Help }
  WriteLn('Terminal Editor');
  ShowHelp;

  { Start with a new file }
  NewFile;

  { Main Input Loop }
  try
    HandleInput;
  finally
    Buffer.Free;
  end;
end.
