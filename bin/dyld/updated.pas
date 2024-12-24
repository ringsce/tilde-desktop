{ Main Program }

var
  Dyld: TDyld;
  ExampleFunction: function(x: Integer): Integer; cdecl;
begin
  try
    // Update with your library path
    Dyld := TDyld.Create('./mylibrary.so');
    try
      WriteLn('Loading library...');
      Dyld.LoadLibrary;

      WriteLn('Fetching symbol: MyFunction');
      Pointer(ExampleFunction) := Dyld.GetSymbol('MyFunction');
      WriteLn('MyFunction(42) = ', ExampleFunction(42));

    finally
      WriteLn('Unloading library...');
      Dyld.UnloadLibrary;
      Dyld.Free;
    end;
  except
    on E: Exception do
      WriteLn(StdErr, 'Error: ', E.Message);
  end;
end.
