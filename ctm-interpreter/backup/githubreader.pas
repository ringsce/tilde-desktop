unit GithubReader;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

implementation

end.

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
    function DownloadAllRepos(const Owner: string; const OutputDir: string): Boolean;
    procedure SaveRepoToFile(const Owner, Repo, OutputFile: string);
  end;

  TClaudeAIIntegration = class
  private
    FHTTPClient: TFPHTTPClient;
    FAPIKey: string;
    FModel: string;
    procedure SetupHTTPClient;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function SendPromptWithContext(const Prompt, Context: string): string;
    function AnalyzeCode(const Code, Question: string): string;
  end;

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
  FHTTPClient.AddHeader('User-Agent', 'FreePascal-GitHub-Client');
  FHTTPClient.AddHeader('Accept', 'application/vnd.github+json');
  if FGitHubToken <> '' then
    FHTTPClient.AddHeader('Authorization', 'Bearer ' + FGitHubToken);
end;

function TRingsceRepoReader.ParseJSONArray(const JSONStr: string): TJSONArray;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr, [joUTF8]);
  try
    Result := Parser.Parse as TJSONArray;
  finally
    Parser.Free;
  end;
end;

function TRingsceRepoReader.ParseJSONObject(const JSONStr: string): TJSONObject;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr, [joUTF8]);
  try
    Result := Parser.Parse as TJSONObject;
  finally
    Parser.Free;
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
        // Remove line breaks from base64 string
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

function TRingsceRepoReader.DownloadAllRepos(const Owner: string; const OutputDir: string): Boolean;
var
  Repos: array of string;
  i: Integer;
  RepoDir: string;
begin
  Result := False;

  // List of ringsce repositories
  SetLength(Repos, 10);
  Repos[0] := 'ekron-realms';
  Repos[1] := 'kcc';
  Repos[2] := 'kayteide';
  Repos[3] := 'kayte-lang';
  Repos[4] := 'sb_rpg';
  Repos[5] := 'editor_addons';
  Repos[6] := 'tutorials';
  Repos[7] := 'samples';
  Repos[8] := 'documentation';
  Repos[9] := 'rad-templates';

  // Create output directory
  if not DirectoryExists(OutputDir) then
    CreateDir(OutputDir);

  WriteLn('Downloading ringsce repositories...');

  for i := 0 to High(Repos) do
  begin
    WriteLn(Format('Processing %s...', [Repos[i]]));
    RepoDir := IncludeTrailingPathDelimiter(OutputDir) + Repos[i];
    if not DirectoryExists(RepoDir) then
      CreateDir(RepoDir);

    SaveRepoToFile(Owner, Repos[i], RepoDir + PathDelim + 'repo_data.txt');
  end;

  Result := True;
  WriteLn('Download complete!');
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
    OutputText.Add('');

    for i := 0 to Contents.Count - 1 do
    begin
      Parts := Contents[i].Split('|');
      if Length(Parts) >= 3 then
      begin
        OutputText.Add('--- File: ' + Parts[2] + ' ---');

        if Parts[1] = 'file' then
        begin
          // Try to get file content for common file types
          if (Pos('.md', Parts[0]) > 0) or
             (Pos('.txt', Parts[0]) > 0) or
             (Pos('.pas', Parts[0]) > 0) or
             (Pos('.c', Parts[0]) > 0) or
             (Pos('.h', Parts[0]) > 0) then
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

function TClaudeAIIntegration.SendPromptWithContext(const Prompt, Context: string): string;
var
  RequestJSON, ResponseStr: string;
  RequestObj, ResponseObj: TJSONObject;
  MessagesArray, ContentArray: TJSONArray;
  MessageObj: TJSONObject;
  InputStream: TStringStream;
begin
  Result := '';

  // Build JSON request
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
        ContentArray := ResponseObj.GetPath('content') as TJSONArray;
        if Assigned(ContentArray) and (ContentArray.Count > 0) then
        begin
          Result := (ContentArray.Objects[0] as TJSONObject).Get('text', '');
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

{ Main Program }

var
  RepoReader: TRingsceRepoReader;
  ClaudeAI: TClaudeAIIntegration;
  GitHubToken, ClaudeAPIKey: string;
  OutputDir: string;
  ReadmeContent: string;

begin
  WriteLn('=== Ringsce GitHub Repository Reader for macOS ===');
  WriteLn;

  // Optional: Set your GitHub token for higher rate limits
  GitHubToken := GetEnvironmentVariable('GITHUB_TOKEN');

  // Set your Claude API key
  ClaudeAPIKey := GetEnvironmentVariable('ANTHROPIC_API_KEY');

  // Set output directory (macOS default)
  OutputDir := GetEnvironmentVariable('HOME') + '/ringsce-repos';

  try
    // Initialize GitHub reader
    RepoReader := TRingsceRepoReader.Create(GitHubToken);
    try
      WriteLn('1. Downloading ringsce repositories...');
      if RepoReader.DownloadAllRepos('ringsce', OutputDir) then
        WriteLn('Successfully downloaded all repositories!')
      else
        WriteLn('Failed to download repositories.');

      WriteLn;
      WriteLn('2. Reading kcc (Kayte C Compiler) README...');
      ReadmeContent := RepoReader.GetFileContent('ringsce', 'kcc', 'README.md');

      if ReadmeContent <> '' then
      begin
        WriteLn('README Content Preview:');
        WriteLn(Copy(ReadmeContent, 1, 500));
        WriteLn('...');
        WriteLn;

        // If Claude API key is available, analyze the code
        if ClaudeAPIKey <> '' then
        begin
          WriteLn('3. Analyzing with Claude AI...');
          ClaudeAI := TClaudeAIIntegration.Create(ClaudeAPIKey);
          try
            WriteLn(ClaudeAI.AnalyzeCode(ReadmeContent,
              'Explain what the Kayte C Compiler does and how to use it.'));
          finally
            ClaudeAI.Free;
          end;
        end
        else
          WriteLn('3. Skipping AI analysis (no ANTHROPIC_API_KEY set)');
      end
      else
        WriteLn('Could not read README.md');

    finally
      RepoReader.Free;
    end;

    WriteLn;
    WriteLn('Done! Repository data saved to: ', OutputDir);

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

