program CTMInterpreter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, cli, api;

var
  GitOps: TGitOperations;
  RepoReader: TRingsceRepoReader;
  ClaudeAI: TClaudeAIIntegration;
  GitHubToken, ClaudeAPIKey: string;
  OutputDir, RepoPath, CommitMessage, RepoName: string;
  RepoList: TStringList;
  i: Integer;
  RepoContent: string;
  Success: Boolean;

begin
  WriteLn('=== Ringsce GitHub Repository Manager ===');
  WriteLn;

  // Get environment variables
  GitHubToken := GetEnvironmentVariable('GITHUB_TOKEN');
  ClaudeAPIKey := GetEnvironmentVariable('ANTHROPIC_API_KEY');
  OutputDir := GetEnvironmentVariable('HOME') + '/ringsce-repos';

  // Parse command line arguments
  if ParamCount = 0 then
  begin
    ShowHelp;
    Halt(0);
  end;

  case ParamStr(1) of
    '--help':
      begin
        ShowHelp;
        Halt(0);
      end;

    '--clone-all':
      begin
        GitOps := TGitOperations.Create;
        try
          Success := GitOps.CloneAllRingsceRepos(OutputDir);
          if Success then
            WriteLn('All repositories cloned to: ', OutputDir)
          else
            WriteLn('Failed to clone repositories.');
        finally
          GitOps.Free;
        end;
      end;

    '--sync':
      begin
        if ParamCount < 2 then
        begin
          WriteLn('Error: Missing repository path');
          Halt(1);
        end;
        RepoPath := ParamStr(2);
        GitOps := TGitOperations.Create;
        try
          GitOps.SyncRepo(RepoPath);
        finally
          GitOps.Free;
        end;
      end;

    '--commit':
      begin
        if ParamCount < 3 then
        begin
          WriteLn('Error: Missing repository path or commit message');
          Halt(1);
        end;
        RepoPath := ParamStr(2);
        CommitMessage := ParamStr(3);
        GitOps := TGitOperations.Create;
        try
          GitOps.CommitChanges(RepoPath, CommitMessage);
        finally
          GitOps.Free;
        end;
      end;

    '--reset':
      begin
        if ParamCount < 2 then
        begin
          WriteLn('Error: Missing repository path');
          Halt(1);
        end;
        RepoPath := ParamStr(2);
        GitOps := TGitOperations.Create;
        try
          GitOps.ResetRepo(RepoPath);
        finally
          GitOps.Free;
        end;
      end;

    '--build':
      begin
        if ParamCount < 2 then
        begin
          WriteLn('Error: Missing repository path');
          Halt(1);
        end;
        RepoPath := ParamStr(2);
        GitOps := TGitOperations.Create;
        try
          GitOps.BuildFromSource(RepoPath);
        finally
          GitOps.Free;
        end;
      end;

    '--list-repos':
      begin
        RepoReader := TRingsceRepoReader.Create(GitHubToken);
        try
          WriteLn('Fetching repositories from ringsce...');
          RepoList := RepoReader.GetRepoList('ringsce');
          try
            WriteLn('Available repositories:');
            for i := 0 to RepoList.Count - 1 do
              WriteLn('  - ', RepoList[i]);
          finally
            RepoList.Free;
          end;
        finally
          RepoReader.Free;
        end;
      end;

    '--analyze':
      begin
        if ParamCount < 2 then
        begin
          WriteLn('Error: Missing repository name');
          Halt(1);
        end;

        if ClaudeAPIKey = '' then
        begin
          WriteLn('Error: ANTHROPIC_API_KEY not set');
          Halt(1);
        end;

        RepoName := ParamStr(2);
        RepoReader := TRingsceRepoReader.Create(GitHubToken);
        ClaudeAI := TClaudeAIIntegration.Create(ClaudeAPIKey);
        try
          WriteLn('Analyzing repository: ', RepoName);
          RepoContent := RepoReader.GetFileContent('ringsce', RepoName, 'README.md');

          if RepoContent <> '' then
          begin
            WriteLn(ClaudeAI.SummarizeRepo(RepoName, RepoContent));
          end
          else
            WriteLn('Could not read repository README');
        finally
          ClaudeAI.Free;
          RepoReader.Free;
        end;
      end;

    '--summarize-all':
      begin
        if ClaudeAPIKey = '' then
        begin
          WriteLn('Error: ANTHROPIC_API_KEY not set');
          Halt(1);
        end;

        RepoReader := TRingsceRepoReader.Create(GitHubToken);
        ClaudeAI := TClaudeAIIntegration.Create(ClaudeAPIKey);
        try
          RepoList := RepoReader.GetRepoList('ringsce');
          try
            for i := 0 to RepoList.Count - 1 do
            begin
              WriteLn('=== ', RepoList[i], ' ===');
              RepoContent := RepoReader.GetFileContent('ringsce', RepoList[i], 'README.md');
              if RepoContent <> '' then
              begin
                WriteLn(ClaudeAI.SummarizeRepo(RepoList[i], RepoContent));
                WriteLn;
              end;
            end;
          finally
            RepoList.Free;
          end;
        finally
          ClaudeAI.Free;
          RepoReader.Free;
        end;
      end;

  else
    WriteLn('Unknown command: ', ParamStr(1));
    WriteLn('Use --help for usage information');
    Halt(1);
  end;

  WriteLn;
  WriteLn('Done!');
end.
