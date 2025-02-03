unit KayteFileDetector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  { TKayteFileDetector }
  TKayteFileDetector = class
  private
    FSoftwareFolder: string;
    FDetectedFiles: TStringList;
    function GetSoftwareFolder: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ScanForKayteFiles;
    procedure DisplayDetectedFiles;

    property SoftwareFolder: string read GetSoftwareFolder;
    property DetectedFiles: TStringList read FDetectedFiles;
  end;

implementation

{ TKayteFileDetector }

constructor TKayteFileDetector.Create;
begin
  inherited Create;
  FDetectedFiles := TStringList.Create;
  FSoftwareFolder := GetSoftwareFolder;
end;

destructor TKayteFileDetector.Destroy;
begin
  FDetectedFiles.Free;
  inherited Destroy;
end;

function TKayteFileDetector.GetSoftwareFolder: string;
begin
  {$IFDEF Darwin}
  // macOS: Use the application bundle's Resources directory or Documents folder
  Result := ExpandFileName(GetAppConfigDir(False));
  (*  be careful. might work on each version.
  {$ELSEIF Unix}
  // Linux/Unix: Use the application configuration directory
  Result := ExpandFileName(GetAppConfigDir(False));
  *)
  (* be careful. enable if you are on windows 11
  {$ELSEIF MSWINDOWS}
  // Windows: Use the application folder
  Result := ExtractFilePath(ParamStr(0));
  *)
  {$ELSE}
  raise Exception.Create('Unsupported platform');
  {$ENDIF}
end;

procedure TKayteFileDetector.ScanForKayteFiles;
begin
  if DirectoryExists(FSoftwareFolder) then
  begin
    FDetectedFiles.Clear;
    FindAllFiles(FDetectedFiles, FSoftwareFolder, '*.kayte', True);
  end
  else
    raise Exception.Create('Software folder not found: ' + FSoftwareFolder);
end;

procedure TKayteFileDetector.DisplayDetectedFiles;
var
  I: Integer;
begin
  if FDetectedFiles.Count = 0 then
  begin
    Writeln('No .kayte files detected in ', FSoftwareFolder);
    Exit;
  end;

  Writeln('Detected .kayte files:');
  for I := 0 to FDetectedFiles.Count - 1 do
    Writeln(FDetectedFiles[I]);
end;

end. // Ensure this "end." is present and ends with a period.

