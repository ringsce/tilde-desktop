unit FileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

function GetFilesInDirectory(const APath: string): TStringList;

implementation

function GetFilesInDirectory(const APath: string): TStringList;
var
  SearchRec: TSearchRec;
begin
  Result := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          Result.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.

