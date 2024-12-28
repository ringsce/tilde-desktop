unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  FileManager, DirectoryTree, FileList;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    Splitter: TSplitter;
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDirectoryTree: TDirectoryTree;
    FFileList: TFileList;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize components
  FDirectoryTree := TDirectoryTree.Create(Self);
  FDirectoryTree.Parent := Self;
  FDirectoryTree.Align := alLeft;
  FDirectoryTree.Width := 200;

  FFileList := TFileList.Create(Self);
  FFileList.Parent := Self;
  FFileList.Align := alClient;

  // Link events
  FDirectoryTree.OnDirectoryChange := @FFileList.UpdateFileList;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

end.

