unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TsplScreen }

  TsplScreen = class(TForm)
    Image1: TImage;
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  splScreen: TsplScreen;

implementation

{$R *.lfm}

{ TsplScreen }

procedure TsplScreen.Image1Click(Sender: TObject);
begin
  //image1.Picture.LoadFromFile('/Users/pedro/demo.jpg');   // Assumes Linux path
end;

end.

