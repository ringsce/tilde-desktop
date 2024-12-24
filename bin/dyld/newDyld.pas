program Dyld;

{$mode objfpc}{$H+}

uses
  dynlibs, SysUtils;

type
  TDyld = class
  private
    FHandle: TLibHandle;
    FLibraryPath: string;
  public
    constructor Create(const LibraryPath: string);
    destructor Destroy; override;

    procedure LoadSharedLibrary;
    function GetSymbol(const SymbolName: string): Pointer;
    procedure UnloadLibrary;
  end;

{ TDyld Implementation }

constructor TDyld.Create(const LibraryPath: string);
begin
  inherited Create;
  FLibraryPath := LibraryPath;
  FHandle := NilHandle;
end;

destructor TDyld.Destroy;
begin
  if FHandle <> NilHandle then
    UnloadLibrary;
  inherited Destroy;
end;

procedure TDyld.LoadSharedLibrary;
begin
  FHandle := DynLibs.LoadLibrary(FLibraryPath); // Explicit namespace for clarity
  if FHandle = NilHandle then
    raise Exception.CreateFmt('Failed to load library: %s', [FLibraryPath]);
end;

function TDyld.GetSymbol(const SymbolName: string): Pointer;
begin
  if FHandle = NilHandle then
    raise Exception.Create('Library not loaded.');

  Result := DynLibs.GetProcAddress(FHandle, PChar(SymbolName)); // Explicit namespace
  if not Assigned(Result) then
    raise Exception.CreateFmt('Symbol not found: %s', [SymbolName]);
end;

procedure TDyld.UnloadLibrary;
begin
  if FHandle <> NilHandle then
  begin
    if not DynLibs.FreeLibrary(FHandle) then
      WriteLn(StdErr, 'Warning: Failed to unload library.');
    FHandle := NilHandle;
  end;
end;

{ Main Program }

var
  DyldInstance: TDyld;
  ExampleFunction: function(x: Integer): Integer; cdecl;
begin
  try
    // Update with your library path
    DyldInstance := TDyld.Create('./mylibrary.so');
    try
      WriteLn('Loading library...');
      DyldInstance.LoadSharedLibrary;

      WriteLn('Fetching symbol: MyFunction');
      // Use PChar to pass the symbol name correctly
      Pointer(ExampleFunction) := DyldInstance.GetSymbol('MyFunction');
      if Assigned(ExampleFunction) then
        WriteLn('MyFunction(42) = ', ExampleFunction(42))
      else
        WriteLn('MyFunction not found or failed to assign.');
    finally
      WriteLn('Unloading library...');
      DyldInstance.UnloadLibrary;
      DyldInstance.Free;
    end;
  except
    on E: Exception do
      WriteLn(StdErr, 'Error: ', E.Message);
  end;
end.
