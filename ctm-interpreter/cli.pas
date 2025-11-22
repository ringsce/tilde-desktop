unit cli;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, fphttpclient, opensslsockets, fpjson, jsonparser, base64, Process;

type
  TGitHubFile = record
    Name: string;
    Path: string;
    Content: string;
    SHA: string;
    Size: Integer;
  end;

  TGitHubRepo = record
    Owner: string;
    Name: string;
    Description: string;
  end;

  TRingsceRepoReader = class
  private
    FHTTPClient: TFPHTTPClient;
    FGitHubToken: string;
    FBaseURL: string;
    procedure SetupHTTPClient;
    function ParseJSONArray(const JSONStr: string): TJSONArray;
    function ParseJSONObject(const JSONStr: string): TJSONObject;
  public
    constructor Create(const GitHubToken: string = '');
    destructor Destroy; override;
    function GetRepoContents(const Owner, Repo, Path: string): TStringList;
    function GetFileContent(const Owner, Repo, FilePath: string): string;
    function GetRepoList(const Owner: string): TStringList;
    procedure SaveRepoToFile(const Owner, Repo, OutputFile: string);
  end;

  TGitOperations = class
  private
    procedure RunCommand(const ACommand: string);
  public
    procedure CloneRepo(const RepoURL, Destination: string);
    procedure SyncRepo(const RepoPath: string);
    procedure CommitChanges(const RepoPath, Message: string);
    procedure ResetRepo(const RepoPath: string);
    procedure BuildFromSource(const RepoPath: string);
    function CloneAllRingsceRepos(const OutputDir: string): Boolean;
  end;

  TClaudeAIIntegration = class
  private
    FHTTPClient: TFPHTTPClient;
    FAPIKey: string;
    FModel: string;
    procedure SetupHTTPClient;
    function ParseJSONObject(const JSONStr: string): TJSONObject;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function SendPromptWithContext(const Prompt, Context: string): string;
    function AnalyzeCode(const Code, Question: string): string;
    function SummarizeRepo(const RepoName, RepoContent: string): string;
  end;

procedure ShowHelp;

implementation

{ TRingsceRepoReader Implementation }

constructor TRingsceRepoReader.Create(const GitHubToken: string);
begin
  inherited Create;
  FGitHubToken := GitHubToken;
  FBaseURL := 'https://api.github.com';
  FHTTPClient := TFPHTTPClient.Create(nil);
  SetupHTTPClient;
end;

destructor TRingsceRepoReader.Destroy;
begin
  FHTTPClient.Free;
  inherited Destroy;
end;

procedure TRingsceRepoReader.SetupHTTPClient;
begin
  FHTTPClient.AddHeader('User-Agent', 'FreePascal-Ringsce-Client');
  FHTTPClient.AddHeader('Accept', 'application/vnd.github+json');
  if FGitHubToken <> '' then
    FHTTPClient.AddHeader('Authorization', 'Bearer ' + FGitHubToken);
end;

function TRingsceRepoReader.ParseJSONArray(const JSONStr: string): TJSONArray;
var
  Parser: TJSONParser;
  JSONData: TJSONData;
begin
  Parser := TJSONParser.Create(JSONStr, []);
  try
    JSONData := Parser.Parse;
    Result := JSONData as TJSONArray;
  finally
    Parser.Free;
  end;
end;

function TRingsceRepoReader.ParseJSONObject(const JSONStr: string): TJSONObject;
var
  Parser: TJSONParser;
  JSONData: TJSONData;
begin
  Parser := TJSONParser.Create(JSONStr, []);
  try
    JSONData := Parser.Parse;
    Result := JSONData as TJSONObject;
  finally
    Parser.Free;
  end;
end;

function TRingsceRepoReader.GetRepoList(const Owner: string): TStringList;
var
  URL, Response: string;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;
  URL := Format('%s/users/%s/repos', [FBaseURL, Owner]);

  try
    Response := FHTTPClient.Get(URL);
    JSONArray := ParseJSONArray(Response);
    try
      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONObj := JSONArray.Objects[i];
        Result.Add(JSONObj.Get('name', ''));
      end;
    finally
      JSONArray.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error getting repo list: ', E.Message);
  end;
end;

function TRingsceRepoReader.GetRepoContents(const Owner, Repo, Path: string): TStringList;
var
  URL, Response: string;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;

  if Path = '' then
    URL := Format('%s/repos/%s/%s/contents', [FBaseURL, Owner, Repo])
  else
    URL := Format('%s/repos/%s/%s/contents/%s', [FBaseURL, Owner, Repo, Path]);

  try
    Response := FHTTPClient.Get(URL);
    JSONArray := ParseJSONArray(Response);
    try
      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONObj := JSONArray.Objects[i];
        Result.Add(Format('%s|%s|%s', [
          JSONObj.Get('name', ''),
          JSONObj.Get('type', ''),
          JSONObj.Get('path', '')
        ]));
      end;
    finally
      JSONArray.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error getting repo contents: ', E.Message);
  end;
end;

function TRingsceRepoReader.GetFileContent(const Owner, Repo, FilePath: string): string;
var
  URL, Response, EncodedContent: string;
  JSONObj: TJSONObject;
  Decoder: TBase64DecodingStream;
  InputStream, OutputStream: TStringStream;
begin
  Result := '';
  URL := Format('%s/repos/%s/%s/contents/%s', [FBaseURL, Owner, Repo, FilePath]);

  try
    Response := FHTTPClient.Get(URL);
    JSONObj := ParseJSONObject(Response);
    try
      if JSONObj.Get('encoding', '') = 'base64' then
      begin
        EncodedContent := JSONObj.Get('content', '');
        EncodedContent := StringReplace(EncodedContent, #10, '', [rfReplaceAll]);
        EncodedContent := StringReplace(EncodedContent, #13, '', [rfReplaceAll]);

        InputStream := TStringStream.Create(EncodedContent);
        OutputStream := TStringStream.Create('');
        try
          Decoder := TBase64DecodingStream.Create(InputStream);
          try
            OutputStream.CopyFrom(Decoder, Decoder.Size);
            Result := OutputStream.DataString;
          finally
            Decoder.Free;
          end;
        finally
          InputStream.Free;
          OutputStream.Free;
        end;
      end;
    finally
      JSONObj.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error getting file content: ', E.Message);
  end;
end;

procedure TRingsceRepoReader.SaveRepoToFile(const Owner, Repo, OutputFile: string);
var
  Contents: TStringList;
  OutputText: TStringList;
  i: Integer;
  Parts: TStringArray;
  FileContent: string;
begin
  Contents := GetRepoContents(Owner, Repo, '');
  OutputText := TStringList.Create;
  try
    OutputText.Add('=== Repository: ' + Repo + ' ===');
    OutputText.Add('Owner: ' + Owner);
    OutputText.Add('URL: https://github.com/' + Owner + '/' + Repo);
    OutputText.Add('');

    for i := 0 to Contents.Count - 1 do
    begin
      Parts := Contents[i].Split('|');
      if Length(Parts) >= 3 then
      begin
        OutputText.Add('--- File: ' + Parts[2] + ' ---');

        if Parts[1] = 'file' then
        begin
          if (Pos('.md', Parts[0]) > 0) or
             (Pos('.txt', Parts[0]) > 0) or
             (Pos('.pas', Parts[0]) > 0) or
             (Pos('.c', Parts[0]) > 0) or
             (Pos('.h', Parts[0]) > 0) or
             (Pos('README', Parts[0]) > 0) then
          begin
            FileContent := GetFileContent(Owner, Repo, Parts[2]);
            if FileContent <> '' then
            begin
              OutputText.Add(FileContent);
              OutputText.Add('');
            end;
          end;
        end;
      end;
    end;

    OutputText.SaveToFile(OutputFile);
    WriteLn('Saved to: ', OutputFile);
  finally
    OutputText.Free;
    Contents.Free;
  end;
end;

{ TGitOperations Implementation }

procedure TGitOperations.RunCommand(const ACommand: string);
var
  Output: TStringList;
  Proc: TProcess;
begin
  Output := TStringList.Create;
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := '/bin/sh';
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(ACommand);
    Proc.Options := [poWaitOnExit, poUsePipes];

    Proc.Execute;
    Output.LoadFromStream(Proc.Output);
    WriteLn(Output.Text);
  finally
    Output.Free;
    Proc.Free;
  end;
end;

procedure TGitOperations.CloneRepo(const RepoURL, Destination: string);
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

    WriteLn('Cloning repository: ', RepoURL);
    GitProcess.Execute;
    WriteLn('Clone completed: ', Destination);
  except
    on E: Exception do
      WriteLn('Error cloning repository: ', E.Message);
  end;
  FreeAndNil(GitProcess);
end;

procedure TGitOperations.SyncRepo(const RepoPath: string);
begin
  WriteLn('Syncing repository: ', RepoPath);
  RunCommand('cd ' + RepoPath + ' && git pull');
  WriteLn('Sync complete.');
end;

procedure TGitOperations.CommitChanges(const RepoPath, Message: string);
begin
  WriteLn('Committing changes in: ', RepoPath);
  RunCommand('cd ' + RepoPath + ' && git add .');
  RunCommand('cd ' + RepoPath + ' && git commit -m "' + Message + '"');
  RunCommand('cd ' + RepoPath + ' && git push -u --force');
  WriteLn('Commit complete.');
end;

procedure TGitOperations.ResetRepo(const RepoPath: string);
begin
  WriteLn('Resetting repository: ', RepoPath);
  RunCommand('cd ' + RepoPath + ' && git reset --hard && git clean -fd');
  WriteLn('Reset complete.');
  SyncRepo(RepoPath);
end;

procedure TGitOperations.BuildFromSource(const RepoPath: string);
begin
  WriteLn('Building project in: ', RepoPath);
  RunCommand('cd ' + RepoPath + ' && make');
  WriteLn('Build completed!');
  SyncRepo(RepoPath);
end;

function TGitOperations.CloneAllRingsceRepos(const OutputDir: string): Boolean;
const
  Repos: array[0..9] of string = (
    'ekron-realms', 'kcc', 'kayteide', 'kayte-lang', 'sb_rpg',
    'editor_addons', 'tutorials', 'samples', 'documentation', 'rad-templates'
  );
var
  i: Integer;
  RepoURL, Destination: string;
begin
  Result := False;

  if not DirectoryExists(OutputDir) then
    CreateDir(OutputDir);

  WriteLn('Cloning all ringsce repositories...');

  for i := 0 to High(Repos) do
  begin
    RepoURL := 'https://github.com/ringsce/' + Repos[i] + '.git';
    Destination := IncludeTrailingPathDelimiter(OutputDir) + Repos[i];

    if DirectoryExists(Destination) then
    begin
      WriteLn('Repository already exists, syncing: ', Repos[i]);
      SyncRepo(Destination);
    end
    else
      CloneRepo(RepoURL, Destination);
  end;

  Result := True;
  WriteLn('All repositories processed!');
end;

{ TClaudeAIIntegration Implementation }

constructor TClaudeAIIntegration.Create(const APIKey: string);
begin
  inherited Create;
  FAPIKey := APIKey;
  FModel := 'claude-sonnet-4-20250514';
  FHTTPClient := TFPHTTPClient.Create(nil);
  SetupHTTPClient;
end;

destructor TClaudeAIIntegration.Destroy;
begin
  FHTTPClient.Free;
  inherited Destroy;
end;

procedure TClaudeAIIntegration.SetupHTTPClient;
begin
  FHTTPClient.AddHeader('x-api-key', FAPIKey);
  FHTTPClient.AddHeader('anthropic-version', '2023-06-01');
  FHTTPClient.AddHeader('content-type', 'application/json');
end;

function TClaudeAIIntegration.ParseJSONObject(const JSONStr: string): TJSONObject;
var
  Parser: TJSONParser;
  JSONData: TJSONData;
begin
  Parser := TJSONParser.Create(JSONStr, []);
  try
    JSONData := Parser.Parse;
    Result := JSONData as TJSONObject;
  finally
    Parser.Free;
  end;
end;

function TClaudeAIIntegration.SendPromptWithContext(const Prompt, Context: string): string;
var
  RequestJSON, ResponseStr: string;
  RequestObj, ResponseObj: TJSONObject;
  MessagesArray, ContentArray: TJSONArray;
  MessageObj: TJSONObject;
  InputStream: TStringStream;
  ContentData: TJSONData;
  TextObj: TJSONObject;
begin
  Result := '';

  RequestObj := TJSONObject.Create;
  try
    RequestObj.Add('model', FModel);
    RequestObj.Add('max_tokens', 4096);

    MessagesArray := TJSONArray.Create;
    MessageObj := TJSONObject.Create;
    MessageObj.Add('role', 'user');

    if Context <> '' then
      MessageObj.Add('content', Format('Here is the code context:%s%s%s%sQuestion: %s',
        [#10#10, Context, #10#10, #10, Prompt]))
    else
      MessageObj.Add('content', Prompt);

    MessagesArray.Add(MessageObj);
    RequestObj.Add('messages', MessagesArray);

    RequestJSON := RequestObj.AsJSON;

    InputStream := TStringStream.Create(RequestJSON);
    try
      ResponseStr := FHTTPClient.Post('https://api.anthropic.com/v1/messages', InputStream);

      ResponseObj := ParseJSONObject(ResponseStr);
      try
        ContentData := ResponseObj.FindPath('content');
        if Assigned(ContentData) and (ContentData is TJSONArray) then
        begin
          ContentArray := TJSONArray(ContentData);
          if ContentArray.Count > 0 then
          begin
            TextObj := TJSONObject(ContentArray.Items[0]);
            Result := TextObj.Get('text', '');
          end;
        end;
      finally
        ResponseObj.Free;
      end;
    finally
      InputStream.Free;
    end;
  finally
    RequestObj.Free;
  end;
end;

function TClaudeAIIntegration.AnalyzeCode(const Code, Question: string): string;
begin
  Result := SendPromptWithContext(Question, Code);
end;

function TClaudeAIIntegration.SummarizeRepo(const RepoName, RepoContent: string): string;
begin
  Result := SendPromptWithContext(
    Format('Provide a concise summary of the "%s" repository. What is its purpose and main features?', [RepoName]),
    RepoContent
  );
end;

{ CLI Procedures }

procedure ShowHelp;
begin
  WriteLn('=== Ringsce GitHub Repository Manager ===');
  WriteLn;
  WriteLn('Usage: ringsce <command> [arguments]');
  WriteLn;
  WriteLn('Commands:');
  WriteLn('  --clone-all                   Clone all ringsce repositories');
  WriteLn('  --sync <repo_path>            Pull latest changes from repository');
  WriteLn('  --commit <repo_path> "<msg>"  Commit and push changes');
  WriteLn('  --reset <repo_path>           Hard reset repository');
  WriteLn('  --build <repo_path>           Build from source using Makefile');
  WriteLn('  --analyze <repo_name>         Analyze repository with AI');
  WriteLn('  --summarize-all               Summarize all repositories with AI');
  WriteLn('  --list-repos                  List all available repositories');
  WriteLn('  --help                        Show this help message');
  WriteLn;
  WriteLn('Environment Variables:');
  WriteLn('  GITHUB_TOKEN                  GitHub personal access token');
  WriteLn('  ANTHROPIC_API_KEY             Claude AI API key');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  ringsce --clone-all');
  WriteLn('  ringsce --sync ~/ringsce-repos/kcc');
  WriteLn('  ringsce --analyze kcc');
  WriteLn('  ringsce --commit ~/ringsce-repos/kcc "Updated compiler"');
end;

end.
