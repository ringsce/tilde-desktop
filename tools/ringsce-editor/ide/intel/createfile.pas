unit createFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  C_FNAME = 'textfile.txt';

var
  tfOut: TextFile;


implementation

end.

begin
  // Set the name of the file that will be created
  AssignFile(tfOut, C_FNAME);

  // Use exceptions to catch errors (this is the default so not absolutely requried)
  {$I+}

  // Embed the file creation in a try/except block to handle errors gracefully
  try
    // Create the file, write some text and close it.
    rewrite(tfOut);

    writeln(tfOut, 'Hello textfile!');
    writeln(tfOut, 'The answer to life, the universe and everything: ', 42);

    CloseFile(tfOut);

  except
    // If there was an error the reason can be found here
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;

  // Give feedback and wait for key press
  writeln('File ', C_FNAME, ' created if all went ok. Press Enter to stop.');
  readln;
end.
