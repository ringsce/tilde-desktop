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
  Writeln('Usage: ctminterpreter <command> [arguments]');
  Writeln;
  Writeln('Commands:');
  Writeln('  git <repo_url> [destination]  Clone a Git repository');
  Writeln('  --sync                        Pull latest changes from the remote repo');
  Writeln('  --commit "<message>"          Commit all changes with a message');
  Writeln('  --build-from-source           Build the project using Makefile in "tilde-desktop"');
  Writeln('  --reset                       Hard reset the repository & sync again');
  Writeln('  --help                        Show this help message and exit');
  Halt(0);
end;

procedure SyncRepo;
begin
  RunCommand('cd tilde-desktop && git pull');
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

procedure CommitChanges(const Message: string);
begin
  RunCommand('git add .');
  RunCommand('git commit -m "' + Message + '"');
  RunCommand('git push -u --force');
end;

procedure BuildFromSource;
begin
  Writeln('Building project in "tilde-desktop" using Makefile...');
  RunCommand('cd tilde-desktop && make');
  Writeln('Build completed!');

  // Run --sync after build
  SyncRepo;
end;

procedure ResetProject;
begin
  Writeln('Resetting project in "tilde-desktop"...');
  RunCommand('cd tilde-desktop && git reset --hard && git clean -fd');
  Writeln('Project reset completed!');

  // Run --sync after reset
  SyncRepo;
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
    Writeln('Usage: ctminterpreter --sync');
  end;
end;

var
  RepoURL, Destination, CommitMessage: string;
begin
  if (ParamCount = 1) and (ParamStr(1) = '--help') then
    ShowHelp;

  if (ParamCount = 1) and (ParamStr(1) = '--sync') then
  begin
    SyncRepo;
    Halt(0);
  end;

  if (ParamCount = 2) and (ParamStr(1) = '--commit') then
  begin
    CommitMessage := ParamStr(2);
    CommitChanges(CommitMessage);
    Halt(0);
  end;

  if (ParamCount = 1) and (ParamStr(1) = '--build-from-source') then
  begin
    BuildFromSource;
    Halt(0);
  end;

   if (ParamCount = 1) and (ParamStr(1) = '--reset') then
  begin
    ResetProject;
    Halt(0);
  end;

  if ParamCount < 1 then
  begin
    Writeln('Error: Missing arguments. Use --help for usage.');
    Halt(1);
  end;

  RepoURL := ParamStr(1);

  if ParamCount >= 2 then
    Destination := ParamStr(2)
  else
    Destination := ExtractFileName(ChangeFileExt(RepoURL, ''));

  CloneGitRepo(RepoURL, Destination);
end.

