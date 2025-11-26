unit openai;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient, opensslsockets, fpjson, jsonparser;

type
  TOpenAIIntegration = class
  private
    FHTTPClient: TFPHTTPClient;
    FAPIKey: string;
    FModel: string;
    FOrganization: string;
    procedure SetupHTTPClient;
    function ParseJSONObject(const JSONStr: string): TJSONObject;
  public
    constructor Create(const APIKey: string; const Model: string = 'gpt-4');
    destructor Destroy; override;
    function SendPromptWithContext(const Prompt, Context: string): string;
    function ChatCompletion(const Messages: array of string): string;
    function AnalyzeCode(const Code, Question: string): string;
    function SummarizeRepo(const RepoName, RepoContent: string): string;
    function GenerateCode(const Description: string): string;
    function ExplainCode(const Code: string): string;
    procedure SetModel(const Model: string);
    procedure SetOrganization(const OrgID: string);
    property Model: string read FModel write SetModel;
    property Organization: string read FOrganization write SetOrganization;
  end;

implementation

{ TOpenAIIntegration }

constructor TOpenAIIntegration.Create(const APIKey: string; const Model: string);
begin
  inherited Create;
  FAPIKey := APIKey;
  FModel := Model;
  FOrganization := '';
  FHTTPClient := TFPHTTPClient.Create(nil);
  SetupHTTPClient;
end;

destructor TOpenAIIntegration.Destroy;
begin
  FHTTPClient.Free;
  inherited Destroy;
end;

procedure TOpenAIIntegration.SetupHTTPClient;
begin
  FHTTPClient.AddHeader('Authorization', 'Bearer ' + FAPIKey);
  FHTTPClient.AddHeader('Content-Type', 'application/json');
  if FOrganization <> '' then
    FHTTPClient.AddHeader('OpenAI-Organization', FOrganization);
end;

procedure TOpenAIIntegration.SetModel(const Model: string);
begin
  FModel := Model;
end;

procedure TOpenAIIntegration.SetOrganization(const OrgID: string);
begin
  FOrganization := OrgID;
  SetupHTTPClient;
end;

function TOpenAIIntegration.ParseJSONObject(const JSONStr: string): TJSONObject;
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

function TOpenAIIntegration.SendPromptWithContext(const Prompt, Context: string): string;
var
  RequestJSON, ResponseStr: string;
  RequestObj, ResponseObj: TJSONObject;
  MessagesArray, ChoicesArray: TJSONArray;
  UserMessageObj, SystemMessageObj: TJSONObject;
  InputStream, ResponseStream: TStringStream;
  ChoicesData, ChoiceItemData, MessageData: TJSONData;
  ChoiceObj, MessageObjResult: TJSONObject;
  i: Integer;
begin
  Result := '';

  RequestObj := TJSONObject.Create;
  try
    RequestObj.Add('model', FModel);
    RequestObj.Add('max_tokens', 2000);
    RequestObj.Add('temperature', 0.7);

    MessagesArray := TJSONArray.Create;

    if Context <> '' then
    begin
      SystemMessageObj := TJSONObject.Create;
      SystemMessageObj.Add('role', 'system');
      SystemMessageObj.Add('content', 'You are a helpful coding assistant. Here is the code context: ' + Context);
      MessagesArray.Add(SystemMessageObj);
    end;

    UserMessageObj := TJSONObject.Create;
    UserMessageObj.Add('role', 'user');
    UserMessageObj.Add('content', Prompt);
    MessagesArray.Add(UserMessageObj);

    RequestObj.Add('messages', MessagesArray);

    RequestJSON := RequestObj.AsJSON;

    // --- SEND REQUEST ---
    InputStream := TStringStream.Create(RequestJSON);
    ResponseStream := TStringStream.Create('');
    try
      InputStream.Position := 0;

      // Correct overload — NO trailing comma
      FHTTPClient.Post(
        'https://api.openai.com/v1/chat/completions',
        InputStream);

      ResponseStr := ResponseStream.DataString;
    finally
      InputStream.Free;
      ResponseStream.Free;
    end;

    // --- PARSE RESPONSE ---
    ResponseObj := ParseJSONObject(ResponseStr);
    try
      ChoicesData := ResponseObj.FindPath('choices');
      if Assigned(ChoicesData) and (ChoicesData.JSONType = jtArray) then
      begin
        ChoicesArray := TJSONArray(ChoicesData);

        for i := 0 to ChoicesArray.Count - 1 do
        begin
          ChoiceItemData := ChoicesArray.Items[i];
          if (ChoiceItemData.JSONType = jtObject) then
          begin
            ChoiceObj := TJSONObject(ChoiceItemData);
            MessageData := ChoiceObj.FindPath('message');

            if Assigned(MessageData) and (MessageData.JSONType = jtObject) then
            begin
              MessageObjResult := TJSONObject(MessageData);
              Result := MessageObjResult.Get('content', '');
              Break;
            end;
          end;
        end;
      end;
    finally
      ResponseObj.Free;
    end;

  finally
    RequestObj.Free;
  end;
end;

function TOpenAIIntegration.ChatCompletion(const Messages: array of string): string;
var
  RequestJSON, ResponseStr: string;
  RequestObj, ResponseObj: TJSONObject;
  MessagesArray, ChoicesArray: TJSONArray;
  MessageObj: TJSONObject;
  InputStream, ResponseStream: TStringStream;
  ChoiceData, MessageData: TJSONData;
  ChoiceObj, RespMessageObj: TJSONObject;
  i: Integer;
begin
  Result := '';

  RequestObj := TJSONObject.Create;
  try
    RequestObj.Add('model', FModel);
    RequestObj.Add('max_tokens', 2000);

    MessagesArray := TJSONArray.Create;

    // Add all messages (alternating user/assistant pattern)
    for i := 0 to High(Messages) do
    begin
      MessageObj := TJSONObject.Create;
      if (i mod 2) = 0 then
        MessageObj.Add('role', 'user')
      else
        MessageObj.Add('role', 'assistant');
      MessageObj.Add('content', Messages[i]);
      MessagesArray.Add(MessageObj);
    end;

    RequestObj.Add('messages', MessagesArray);

    RequestJSON := RequestObj.AsJSON;

    // --- SEND REQUEST ---
    InputStream := TStringStream.Create(RequestJSON);
    ResponseStream := TStringStream.Create('');
    try
      InputStream.Position := 0;

      // Correct Post(): URL, InputStream, ResponseStream
      FHTTPClient.Post(
        'https://api.openai.com/v1/chat/completions',
        InputStream,
        ResponseStream
      );

      ResponseStr := ResponseStream.DataString;
    finally
      InputStream.Free;
      ResponseStream.Free;
    end;

    // --- PARSE RESPONSE ---
    ResponseObj := ParseJSONObject(ResponseStr);
    try
      ChoiceData := ResponseObj.FindPath('choices');
      if Assigned(ChoiceData) and (ChoiceData.JSONType = jtArray) then
      begin
        ChoicesArray := TJSONArray(ChoiceData);
        if ChoicesArray.Count > 0 then
        begin
          ChoiceData := ChoicesArray.Items[0];
          if (ChoiceData <> nil) and (ChoiceData.JSONType = jtObject) then
          begin
            ChoiceObj := TJSONObject(ChoiceData);
            MessageData := ChoiceObj.FindPath('message');
            if Assigned(MessageData) and (MessageData.JSONType = jtObject) then
            begin
              RespMessageObj := TJSONObject(MessageData);
              Result := RespMessageObj.Get('content', '');
            end;
          end;
        end;
      end;
    finally
      ResponseObj.Free;
    end;

  finally
    RequestObj.Free;
  end;
end;

function TOpenAIIntegration.AnalyzeCode(const Code, Question: string): string;
begin
  Result := SendPromptWithContext(Question, Code);
end;

function TOpenAIIntegration.SummarizeRepo(const RepoName, RepoContent: string): string;
begin
  Result := SendPromptWithContext(
    Format('Provide a concise summary of the "%s" repository. What is its purpose and main features?', [RepoName]),
    RepoContent
  );
end;

function TOpenAIIntegration.GenerateCode(const Description: string): string;
begin
  Result := SendPromptWithContext(
    'Generate code based on this description: ' + Description,
    ''
  );
end;

function TOpenAIIntegration.ExplainCode(const Code: string): string;
begin
  Result := SendPromptWithContext(
    'Explain what this code does in detail:',
    Code
  );
end;

end.
