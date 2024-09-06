unit search;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm5 }

  TForm5 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private

  public

  end;


var
  Form5: TForm5;

implementation

{$R *.lfm}


{ TForm5 }

// form for search
procedure TForm5.Button1Click(Sender: TObject);
var s:string='Plane1=*SSGB747-800i                          (fms)    {X-Plane}        [\Output\FMS plans\]';
begin
Label1.caption:=GetExt(S,'(',')');
Label2.caption:=GetExt(S,'{','}');
Label3.caption:=GetExt(S,'[',']');

end;

function GetExt(S : String;AFind,BFind:String) : String;
Var
  LeftChar : Integer;
  RightChar : Integer;
Begin
  Result := '';
  LeftChar := Pos(AFind,S);
  RightChar := Pos(BFind,S);
  if ((leftchar=0) or (rightchar=0)) then exit;
  inc(leftchar); // increment by one to ignore the AFind
  rightchar:=rightchar-leftchar;// this should give the count for the copy command
  Result := copy(S,LeftChar,RightChar);
end;

procedure TForm5.Label1Click(Sender: TObject);
begin

end;

procedure TForm5.Label2Click(Sender: TObject);
begin

end;

procedure TForm5.Label3Click(Sender: TObject);
begin

end;

end.


