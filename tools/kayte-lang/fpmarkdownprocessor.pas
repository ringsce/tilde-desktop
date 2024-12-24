unit FPMarkdownProcessor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

procedure ConvertMarkdownToHTML(const MarkdownFile, OutputFile: string);

implementation

// Helper function to convert Markdown to HTML
function MarkdownToHTML(const MarkdownText: string): string;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  ResultHTML: TStringList;
begin
  Lines := TStringList.Create;
  ResultHTML := TStringList.Create;
  try
    Lines.Text := MarkdownText;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);

      // Convert headers (#, ##, ###)
      if Line.StartsWith('###') then
        ResultHTML.Add('<h3>' + Copy(Line, 4, Length(Line) - 3) + '</h3>')
      else if Line.StartsWith('##') then
        ResultHTML.Add('<h2>' + Copy(Line, 3, Length(Line) - 2) + '</h2>')
      else if Line.StartsWith('#') then
        ResultHTML.Add('<h1>' + Copy(Line, 2, Length(Line) - 1) + '</h1>')

      // Convert bold (**text** or __text__)
      else if Pos('**', Line) > 0 then
        ResultHTML.Add('<p>' + StringReplace(Line, '**', '<b>', [rfReplaceAll]) + '</b></p>')
      else if Pos('__', Line) > 0 then
        ResultHTML.Add('<p>' + StringReplace(Line, '__', '<b>', [rfReplaceAll]) + '</b></p>')

      // Convert italics (*text* or _text_)
      else if Pos('*', Line) > 0 then
        ResultHTML.Add('<p>' + StringReplace(Line, '*', '<i>', [rfReplaceAll]) + '</i></p>')
      else if Pos('_', Line) > 0 then
        ResultHTML.Add('<p>' + StringReplace(Line, '_', '<i>', [rfReplaceAll]) + '</i></p>')

      // Convert lists (- or *)
      else if Line.StartsWith('-') or Line.StartsWith('*') then
        ResultHTML.Add('<li>' + Copy(Line, 3, Length(Line) - 2) + '</li>')

      // Convert paragraphs
      else if Line <> '' then
        ResultHTML.Add('<p>' + Line + '</p>');
    end;

    Result := ResultHTML.Text;
  finally
    Lines.Free;
    ResultHTML.Free;
  end;
end;

procedure ConvertMarkdownToHTML(const MarkdownFile, OutputFile: string);
var
  MarkdownText, HTMLContent: TStringList;
begin
  MarkdownText := TStringList.Create;
  HTMLContent := TStringList.Create;
  try
    // Load Markdown file
    if FileExists(MarkdownFile) then
    begin
      MarkdownText.LoadFromFile(MarkdownFile);

      // Convert Markdown to HTML
      HTMLContent.Text := MarkdownToHTML(MarkdownText.Text);

      // Add basic HTML structure
      HTMLContent.Insert(0, '<html><head><title>Markdown Output</title></head><body>');
      HTMLContent.Add('</body></html>');

      // Save HTML to output file
      HTMLContent.SaveToFile(OutputFile);

      Writeln('Markdown converted to HTML and saved to: ', OutputFile);
    end
    else
    begin
      Writeln('Error: Markdown file not found: ', MarkdownFile);
    end;
  finally
    MarkdownText.Free;
    HTMLContent.Free;
  end;
end;

end.

