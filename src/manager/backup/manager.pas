unit Manager;

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
    Splitter: TSplitter; // Declare Splitter as a field
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
  // Initialize the directory tree
  FDirectoryTree := TDirectoryTree.Create(Self);
  FDirectoryTree.Parent := Self;
  FDirectoryTree.Align := alLeft;
  FDirectoryTree.Width := 200;

  // Initialize the splitter
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alLeft;

  // Initialize the file list
  FFileList := TFileList.Create(Self);
  FFileList.Parent := Self;
  FFileList.Align := alClient;

  // Link the directory tree's OnDirectoryChange event to the file list
  FDirectoryTree.OnDirectoryChange := @FFileList.UpdateFileList;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

end.

