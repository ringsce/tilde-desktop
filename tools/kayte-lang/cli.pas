unit CLI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TCLIOptions = record
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    Verbose: Boolean;
  end;

  TCLIHandler = class
  private
    FOptions: TCLIOptions;
    FAppName: string;
    FAppVersion: string;
    procedure ShowHelp;
    procedure ShowVersion;
  public
    constructor Create(const AppName, AppVersion: string);
    procedure ParseArgs;
    procedure Execute;
  end;

implementation

{ TCLIHandler }

constructor TCLIHandler.Create(const AppName, AppVersion: string);
begin
  FAppName := AppName;
  FAppVersion := AppVersion;
  FOptions.ShowHelp := False;
  FOptions.ShowVersion := False;
  FOptions.Verbose := False;
end;

procedure TCLIHandler.ShowHelp;
begin
  Writeln('Usage: ', FAppName, ' [OPTIONS]');
  Writeln;
  Writeln('Options:');
  Writeln('  --help          Show this help message and exit');
  Writeln('  -v, --version   Show the version information and exit');
  Writeln('  --verbose       Run in verbose mode');
  Writeln;
end;

procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
begin
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = '--help') then
      FOptions.ShowHelp := True
    else if (ParamStr(I) = '-v') or (ParamStr(I) = '--version') then
      FOptions.ShowVersion := True
    else if (ParamStr(I) = '--verbose') then
      FOptions.Verbose := True
    else
      Writeln('Unknown option: ', ParamStr(I));
  end;
end;

procedure TCLIHandler.Execute;
begin
  if FOptions.ShowHelp then
  begin
    ShowHelp;
    Exit;
  end;

  if FOptions.ShowVersion then
  begin
    ShowVersion;
    Exit;
  end;

  if FOptions.Verbose then
  begin
    Writeln('Running in verbose mode...');
  end;
end;

end.

