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
    Process.Executable := '/bin/sh';  // Use shell interpreter
    Process.Parameters.Add('-c');     // Run as command
    Process.Parameters.Add(ACommand); // The actual command
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;

    // Read output from the process
    Output.LoadFromStream(Process.Output);
    Writeln(Output.Text);
  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure ShowHelp;
begin
  Writeln('Usage: git <repo_url> [destination]');
  Writeln('Example: git https://github.com/ringsce/tilde-desktop.git');
  Writeln('         git https://github.com/ringsce/tilde-desktop.git my-folder');
  Writeln;
  Writeln('Options:');
  Writeln('  --help      Show this help message and exit');
  Halt(0);
end;

procedure CloneGitRepo(const RepoURL, Destination: string);
var
  GitProcess: TProcess;
begin
  GitProcess := TProcess.Create(nil);
  try
    GitProcess.Executable := 'git';
    GitProcess.Parameters.Add('clone');
    GitProcess.Parameters.Add(RepoURL);
    GitProcess.Parameters.Add(Destination);
    GitProcess.Options := [poWaitOnExit, poUsePipes];

    Writeln('Cloning repository: ', RepoURL);
    GitProcess.Execute;

    Writeln('Clone completed: ', Destination);
  except
    on E: Exception do
      Writeln('Error cloning repository: ', E.Message);
  end;
  GitProcess.Free;
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
    Writeln('Usage: cli --sync');
  end;
end;

begin
  CloneGitRepo('https://github.com/ringsce/tilde-desktop.git', 'tilde-desktop');
end.

