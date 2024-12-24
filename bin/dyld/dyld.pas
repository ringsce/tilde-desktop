program MacOSDyldExample;

{$mode objfpc}{$H+}

uses
  dynlibs;

var
  MyLibHandle: TLibHandle;
  MyFunction: function(x: Integer): Integer; cdecl;

begin
  // Load the dynamic library
  MyLibHandle := LoadLibrary('mylibrary.dylib');
  if MyLibHandle = NilHandle then
  begin
    WriteLn('Failed to load library!');
    Halt(1);
  end;

  // Find the function within the library
  Pointer(MyFunction) := GetProcAddress(MyLibHandle, 'MyFunction');
  if not Assigned(MyFunction) then
  begin
    WriteLn('Failed to find MyFunction!');
    FreeLibrary(MyLibHandle);
    Halt(1);
  end;

  // Call the function
  WriteLn('Result: ', MyFunction(42));

  // Unload the library
  if FreeLibrary(MyLibHandle) then
    WriteLn('Library successfully unloaded.')
  else
    WriteLn('Failed to unload the library.');
end.

