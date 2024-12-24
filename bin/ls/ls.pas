program LSCommand;

{$mode objfpc}{$H+}

uses
  SysUtils;

procedure ListDirectory(const DirPath: string);
var
  SearchRec: TSearchRec;
  ResultCode: Integer;
begin
  if not DirectoryExists(DirPath) then
  begin
    WriteLn('Error: Directory does not exist - ', DirPath);
    Exit;
  end;

  ResultCode := FindFirst(IncludeTrailingPathDelimiter(DirPath) + '*', faAnyFile, SearchRec);
  try
    while ResultCode = 0 do
    begin
      // Skip special entries "." and ".."
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
          WriteLn('[DIR] ', SearchRec.Name)
        else
          WriteLn('[FILE] ', SearchRec.Name);
      end;
      ResultCode := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

var
  Directory: string;
begin
  if ParamCount < 1 then
    Directory := GetCurrentDir
  else
    Directory := ParamStr(1);

  WriteLn('Listing contents of: ', Directory);
  ListDirectory(Directory);
end.
