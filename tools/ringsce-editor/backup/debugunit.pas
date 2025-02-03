unit DebugUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls;

type
  TDebugLogger = class
  private
    FOutputControl: TMemo; // Or TListBox
  public
    constructor Create(AOutputControl: TMemo); // Pass a TMemo control from the main form
    procedure Log(Message: string);
    procedure RunUnitTest(TestName: string; TestProcedure: TProc);
  end;

implementation

constructor TDebugLogger.Create(AOutputControl: TMemo);
begin
  FOutputControl := AOutputControl;
end;

procedure TDebugLogger.Log(Message: string);
begin
  if Assigned(FOutputControl) then
  begin
    FOutputControl.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Message);
  end;
end;

procedure TDebugLogger.RunUnitTest(TestName: string; TestProcedure: TProc);
begin
  try
    Log('Running test: ' + TestName);
    TestProcedure;
    Log('Test passed: ' + TestName);
  except
    on E: Exception do
      Log('Test failed: ' + TestName + ' - Error: ' + E.Message);
  end;
end;

end.

