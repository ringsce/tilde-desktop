unit Unit4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  Button1.Caption := 'Close';
end;

procedure TfrmAbout.Edit1Change(Sender: TObject);
begin
   Edit1.Caption := ' 2019 - 2023 Kreatyve Designs';
end;

end.

