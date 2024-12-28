unit DirectoryTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FileCtrl;

type
  TDirectoryChangeEvent = procedure(const APath: string) of object;

  TDirectoryTree = class(TTreeView)
  private
    FOnDirectoryChange: TDirectoryChangeEvent;
    procedure PopulateTree;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
  public
    property OnDirectoryChange: TDirectoryChangeEvent read FOnDirectoryChange write FOnDirectoryChange;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TDirectoryTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnChange := @TreeChange;
  PopulateTree;
end;

procedure TDirectoryTree.PopulateTree;
var
  Drives: array[0..25] of Char;
  Drive: string;
  I: Integer;
  Node: TTreeNode;
begin
  // Use the Windows API to get logical drives
  {$IFDEF WINDOWS}
  GetLogicalDriveStrings(SizeOf(Drives), @Drives);
  Drive := StrPas(Drives);
  while Drive <> '' do
  begin
    Node := Items.Add(nil, Drive);
    Node.Data := Pointer(IncludeTrailingPathDelimiter(Drive));
    Inc(PChar(Drive), StrLen(PChar(Drive)) + 1);
  end;
  {$ELSE}
  // For non-Windows platforms, list mounted file systems in `/mnt` or `/media`
  for Drive in ['/mnt', '/media', '/'] do
  begin
    if DirectoryExists(Drive) then
    begin
      Node := Items.Add(nil, Drive);
      Node.Data := Pointer(IncludeTrailingPathDelimiter(Drive));
    end;
  end;
  {$ENDIF}
end;

procedure TDirectoryTree.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(FOnDirectoryChange) and Assigned(Node) then
    FOnDirectoryChange(string(Node.Data));
end;

end.

