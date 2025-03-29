unit CLI;

{$mode objfpc}{$H+}

interface

procedure RunCLI;

implementation

uses
  SysUtils, Classes, Process;

procedure RunCommand(const ACommand: string);
var
  Output: TStringList;
  Process: TProcess;
begin
  Output := TStringList.Create;
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/sh';  // Shell interpreter
    Process.Parameters.Add('-c');     // Run as command
    Process.Parameters.Add(ACommand); // The actual command
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    // Read output
    Output.LoadFromStream(Process.Output);
    Writeln(Output.Text);
  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure RunCLI;
var
  RepoPath: string;
begin
  if (ParamCount > 0) and (ParamStr(1) = '--sync') then
  begin
    Writeln('Syncing GitHub repository...');

    // Define repository path (current directory)
    RepoPath := GetCurrentDir;

    // Execute `git pull`
    RunCommand('cd ' + RepoPath + ' && git pull');

    Writeln('Sync complete.');
  end
  else
  begin
    Writeln('Usage: cli_sync --sync');
  end;
end;

end.

