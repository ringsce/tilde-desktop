program ringsce;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}  // Linux
  {$IFNDEF DARWIN}
    SQLiteDefaultLibrary := 'libsqlite3.so';
  {$ENDIF}
  {$IFDEF DARWIN}
    //SQLiteLibraryName:='/usr/lib/libsqlite3.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS} // Windows
  SQLiteDefaultLibrary := 'sqlite3.dll';
{$ENDIF}

{$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, setup1, Unit2, Unit3, Unit4, createFile, splashscreen, saveCPP,
  savePHP, saveBAS, Settings, search, updater;
  { you can add units after this }

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ringsCE';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TsplScreen, splScreen);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.

