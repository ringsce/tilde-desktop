unit api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TAPIClient = class
  private
    FBaseURL: string;
  public
    constructor Create(const ABaseURL: string);
    function Get(const AEndpoint: string): string;
    function Post(const AEndpoint: string; const AData: TJSONStringType): string;
  end;

function ExecuteShellCommand(const ACommand: string): string;

implementation

constructor TAPIClient.Create(const ABaseURL: string);
begin
  FBaseURL := ABaseURL;
end;

function TAPIClient.Get(const AEndpoint: string): string;
var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    Result := Client.Get(FBaseURL + AEndpoint);
  finally
    Client.Free;
  end;
end;

function TAPIClient.Post(const AEndpoint: string; const AData: TJSONStringType): string;
var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Content-Type', 'application/json');
    Result := Client.FormPost(FBaseURL + AEndpoint, AData);
  finally
    Client.Free;
  end;
end;

function ExecuteShellCommand(const ACommand: string): string;
var
  Output: TStringList;
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    AProcess.CommandLine := '/bin/sh -c "' + ACommand + '"';
    AProcess.Options := [poUsePipes, poWaitOnExit];
    AProcess.Execute;
    Output.LoadFromStream(AProcess.Output);
    Result := Output.Text;
  finally
    AProcess.Free;
    Output.Free;
  end;
end;

end.
