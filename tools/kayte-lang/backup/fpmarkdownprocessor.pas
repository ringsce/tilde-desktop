unit FPMarkdownProcessor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpmarkdown;

procedure ConvertMarkdownToHTML(const MarkdownFile, OutputFile: string);

implementation

procedure ConvertMarkdownToHTML(const MarkdownFile, OutputFile: string);
var
  MarkdownProcessor: TFPMarkdown;
  MarkdownText, HTMLText: TStringList;
begin
  // Create markdown processor
  MarkdownProcessor := TFPMarkdown.Create;
  MarkdownText := TStringList.Create;
  HTMLText := TStringList.Create;
  try
    // Load the markdown content from the input file
    if FileExists(MarkdownFile) then
    begin
      MarkdownText.LoadFromFile(MarkdownFile);

      // Process markdown to HTML
      HTMLText.Text := MarkdownProcessor.Transform(MarkdownText.Text);

      // Save the generated HTML to the output file
      HTMLText.SaveToFile(OutputFile);

      Writeln('Markdown converted to HTML and saved to: ', OutputFile);
    end
    else
    begin
      Writeln('Error: Markdown file not found: ', MarkdownFile);
    end;
  finally
    // Free resources
    MarkdownProcessor.Free;
    MarkdownText.Free;
    HTMLText.Free;
  end;
end;

end.

