program UbuntuDyld;

{$mode objfpc}{$H+}

uses
  dynlibs, SysUtils;

var
  MyLibHandle: TLibHandle;
  MyFunction: function(x: Integer): Integer; cdecl;

procedure EmulateStaticLibrary;
begin
  WriteLn('Emulating static library mode.');
  WriteLn('Result of MyFunction(42): ', 42 * 2); // Example static behavior
end;

begin
  // Attempt to load the shared library
  MyLibHandle := LoadLibrary('./mylibrary.so'); // Adjust the path to your library
  if MyLibHandle = NilHandle then
  begin
    WriteLn('Failed to load library. Falling back to static emulation mode.');
    EmulateStaticLibrary;
    Halt(0);
  end;

  // Find the function within the library
  Pointer(MyFunction) := GetProcAddress(MyLibHandle, 'MyFunction');
  if not Assigned(MyFunction) then
  begin
    WriteLn('Failed to find MyFunction! Falling back to static emulation mode.');
    FreeLibrary(MyLibHandle);
    EmulateStaticLibrary;
    Halt(0);
  end;

  // Call the function from the library
  try
    WriteLn('Loaded from dynamic library:');
    WriteLn('Result of MyFunction(42): ', MyFunction(42));
  finally
    // Unload the library
    if FreeLibrary(MyLibHandle) then
      WriteLn('Library successfully unloaded.')
    else
      WriteLn('Failed to unload the library.');
  end;
end.
