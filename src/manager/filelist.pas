unit FileList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FileManager;

type
  TFileList = class(TListView)
  public
    procedure UpdateFileList(const APath: string);
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TFileList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  with Columns.Add do
  begin
    Caption := 'File Name';
    Width := 300;
  end;
  with Columns.Add do
  begin
    Caption := 'Size';
    Width := 100;
  end;
end;

procedure TFileList.UpdateFileList(const APath: string);
var
  Files: TStringList;
  I: Integer;
  ListItem: TListItem;
  FilePath: string;
  FileInfo: TSearchRec;
begin
  Items.Clear;
  Files := GetFilesInDirectory(APath);
  try
    for I := 0 to Files.Count - 1 do
    begin
      FilePath := IncludeTrailingPathDelimiter(APath) + Files[I];
      if FindFirst(FilePath, faAnyFile, FileInfo) = 0 then
      begin
        try
          ListItem := Items.Add;
          ListItem.Caption := Files[I];
          ListItem.SubItems.Add(IntToStr(FileInfo.Size)); // Use FileInfo.Size for file size
        finally
          FindClose(FileInfo);
        end;
      end;
    end;
  finally
    Files.Free;
  end;
end;

end.

