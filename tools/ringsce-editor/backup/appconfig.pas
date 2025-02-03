unit AppConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TAppSettings }
  TAppSettings = class
  private
    FFileName: string;
    FSettings: TIniFile;
  public
    constructor Create(const ConfigFileName: string);
    destructor Destroy; override;

    // Methods to manage settings
    procedure SaveSetting(const Section, Key, Value: string);
    function LoadSetting(const Section, Key, DefaultValue: string): string;
    procedure SaveInteger(const Section, Key: string; Value: Integer);
    function LoadInteger(const Section, Key: string; DefaultValue: Integer): Integer;
    procedure SaveBoolean(const Section, Key: string; Value: Boolean);
    function LoadBoolean(const Section, Key: string; DefaultValue: Boolean): Boolean;

    procedure SaveToFile;
    procedure LoadFromFile;
  end;

implementation

{ TAppSettings }

constructor TAppSettings.Create(const ConfigFileName: string);
begin
  FFileName := ConfigFileName;
  FSettings := TIniFile.Create(FFileName);
end;

destructor TAppSettings.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

procedure TAppSettings.SaveSetting(const Section, Key, Value: string);
begin
  FSettings.WriteString(Section, Key, Value);
end;

function TAppSettings.LoadSetting(const Section, Key, DefaultValue: string): string;
begin
  Result := FSettings.ReadString(Section, Key, DefaultValue);
end;

procedure TAppSettings.SaveInteger(const Section, Key: string; Value: Integer);
begin
  FSettings.WriteInteger(Section, Key, Value);
end;

function TAppSettings.LoadInteger(const Section, Key: string; DefaultValue: Integer): Integer;
begin
  Result := FSettings.ReadInteger(Section, Key, DefaultValue);
end;

procedure TAppSettings.SaveBoolean(const Section, Key: string; Value: Boolean);
begin
  FSettings.WriteBool(Section, Key, Value);
end;

function TAppSettings.LoadBoolean(const Section, Key: string; DefaultValue: Boolean): Boolean;
begin
  Result := FSettings.ReadBool(Section, Key, DefaultValue);
end;

procedure TAppSettings.SaveToFile;
begin
  // TIniFile automatically writes changes to the file, so this method is optional
  FSettings.UpdateFile;
end;

procedure TAppSettings.LoadFromFile;
begin
  // TIniFile automatically loads the file on demand, so this method is optional
end;

end.

