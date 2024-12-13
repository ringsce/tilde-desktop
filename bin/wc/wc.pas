program WordCount;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure CountFile(const FileName: string);
var
  FileStream: TFileStream;
  FileContent: TStringList;
  WordCount, LineCount, CharCount: Integer;
  Line: string;
begin
  if not FileExists(FileName) then
  begin
    WriteLn('Error: File not found - ', FileName);
    Exit;
  end;

  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromStream(FileStream);
      WordCount := 0;
      LineCount := FileContent.Count;
      CharCount := FileStream.Size;

      for Line in FileContent do
      begin
        WordCount := WordCount + WordCount(Line);
      end;

      WriteLn(FileName, ': ', LineCount, ' lines, ', WordCount, ' words, ', CharCount, ' characters');
    finally
      FileContent.Free;
      FileStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error reading file: ', E.Message);
  end;
end;

function WordCount(const S: string): Integer;
var
  i: Integer;
  InWord: Boolean;
begin
  Result := 0;
  InWord := False;
  for i := 1 to Length(S) do
  begin
    if S[i] in [' ', #9, #10, #13] then
    begin
      if InWord then
        Inc(Result);
      InWord := False;
    end
    else
      InWord := True;
  end;
  if InWord then
    Inc(Result);
end;

var
  i: Integer;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <file1> [file2 ...]');
    Halt(1);
  end;

  for i := 1 to ParamCount do
    CountFile(ParamStr(i));
end.
