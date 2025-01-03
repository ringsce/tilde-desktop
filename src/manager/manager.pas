unit manager;

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
    Splitter: TSplitter; // Declares the splitter as a form field
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDirectoryTree: TDirectoryTree;
    FFileList: TFileList;
  public
  end;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize components
  FDirectoryTree := TDirectoryTree.Create(Self);
  FDirectoryTree.Parent := Self;
  FDirectoryTree.Align := alLeft;
  FDirectoryTree.Width := 200;

  Splitter := TSplitter.Create(Self); // Create the splitter
  Splitter.Parent := Self;
  Splitter.Align := alLeft;

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

