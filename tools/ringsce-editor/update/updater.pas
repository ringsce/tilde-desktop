unit updater;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF} {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}

  Classes, SysUtils, libpascurl;
var
  handle : CURL;
  effectiveUrl, contentType, ip : PChar;
  responseCode, headerSize : Longint;
  contentLength, totalTime : Longword;
  buffer : TStringStream;


implementation

end.

function WriteFunctionCallback (ptr : PChar; size : LongWord;
   nmemb : LongWord; data : Pointer)
begin
   buffer.WriteString(string(ptr));
end;

curl_global_init(CURL_GLOBAL_ALL);
   curl_easy_setopt(handle, CURLOPT_URL, PChar('https://ringscejs.gleentech.com');
   curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);
   buffer := TStringStream.Create('');

   if curl_easy_perform = CURLE_OK then
   begin
     New(effectiveUrl);
     New(contentType);
     New(ip);

     curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, @effectiveUrl);
     curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, @responseCode);
     curl_easy_getinfo(handle, CURLINFO_HEADER_SIZE, @headerSize);
     curl_easy_getinfo(handle, CURLINFO_CONTENT_TYPE, @contentType);
     curl_easy_getinfo(handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, @contentLength);
     curl_easy_getinfo(handle, CURLINFO_LOCAL_IP, @ip);
     curl_easy_getinfo(handle, CURLINFO_TOTAL_TIME_T, @totalTime);

     writeln('URL: ':20,                 effectiveUrl);
     writeln('Response code: ':20,       responseCode);
     writeln('Header size, kB: ':20,     FormatFloat('0.00', headerSize / 1024));
     writeln('Content type: ',           contentType);
     writeln('Content length, kB: ':20,  FormatFloat('0.00', contentLength / 1024));
     writeln('IP: ':20,                  ip);
     writeln('Total time, ms: ':20,      totalTime);
     writeln('==== Content ====');
     writeln(buffer.DataString);
   end;

   curl_global_cleanup;

 end;
begin
   curl := curl_easy_init;
  if curl <> nil then begin
    curl_easy_setopt(curl, CURLOPT_URL, 'https://ringscejs.gleentech.com/');
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
    curl_easy_setopt(curl, CURLOPT_CAINFO, 'cacert.pem');

end.
