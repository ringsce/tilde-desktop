unit KayteSave;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs; // Dialogs for file dialogs

type
  { TKayteFileManager }
  TKayteFileManager = class
  public
    // Saves the content of the editor to a .kayte file
    class procedure SaveToFile(const Content: string; const DefaultFileName: string = 'untitled.kayte');
  end;

implementation

{ TKayteFileManager }

class procedure TKayteFileManager.SaveToFile(const Content: string; const DefaultFileName: string);
var
  SaveDialog: TSaveDialog;
  FileStream: TFileStream;
  UTF8Content: TBytes;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.DefaultExt := '.kayte';
    SaveDialog.Filter := 'Kayte Files (*.kayte)|*.kayte|All Files (*.*)|*.*';
    SaveDialog.FileName := DefaultFileName;

    if SaveDialog.Execute then
    begin
      // Convert content to UTF-8 encoded bytes
      UTF8Content := TEncoding.UTF8.GetBytes(Content);

      // Save to the selected file
      FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
      try
        FileStream.WriteBuffer(UTF8Content[0], Length(UTF8Content));
      finally
        FileStream.Free;
      end;

      ShowMessage('File saved successfully: ' + SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

end.

