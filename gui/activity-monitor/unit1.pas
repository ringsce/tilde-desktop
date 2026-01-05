program ActivityMonitorTUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  Classes, SysUtils, Process, Math;

type
  TSystemMonitor = class
  private
    CPUHistory: array[0..79] of Integer;
    HistoryIndex: Integer;

    {$IFDEF UNIX}
    LastTotalTime, LastIdleTime: QWord;
    {$ENDIF}
    {$IFDEF WINDOWS}
    LastIdleTime, LastKernelTime, LastUserTime: Int64;
    {$ENDIF}

    function GetCPUUsage: Integer;
    function GetMemoryUsage: Integer;
    function GetMemoryStats: String;
    procedure GetProcessList(var Lines: TStringList);
    procedure DrawBox(X, Y, Width, Height: Integer; Title: String);
    procedure DrawProgressBar(X, Y, Width, Value: Integer; ShowPercent: Boolean = True);
    procedure DrawCPUGraph(X, Y, Width, Height: Integer);
    procedure GotoXY(X, Y: Integer);
    procedure ClearScreen;
    procedure SetColor(FG, BG: Integer);
    procedure ResetColor;
  public
    procedure ShowDashboard;
    procedure ShowProcesses;
    procedure Run;
  end;

{$IFDEF WINDOWS}
uses Windows;

procedure TSystemMonitor.GotoXY(X, Y: Integer);
var
  Coord: TCoord;
begin
  Coord.X := X - 1;
  Coord.Y := Y - 1;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
end;

procedure TSystemMonitor.ClearScreen;
var
  ConsoleInfo: TConsoleScreenBufferInfo;
  CharsWritten: DWORD;
  ConSize: DWORD;
  Origin: TCoord;
  Console: THandle;
begin
  Console := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Console, ConsoleInfo);
  ConSize := ConsoleInfo.dwSize.X * ConsoleInfo.dwSize.Y;
  Origin.X := 0;
  Origin.Y := 0;
  FillConsoleOutputCharacter(Console, ' ', ConSize, Origin, CharsWritten);
  FillConsoleOutputAttribute(Console, ConsoleInfo.wAttributes, ConSize, Origin, CharsWritten);
  GotoXY(1, 1);
end;

procedure TSystemMonitor.SetColor(FG, BG: Integer);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FG or (BG shl 4));
end;

procedure TSystemMonitor.ResetColor;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TSystemMonitor.GotoXY(X, Y: Integer);
begin
  Write(#27'[', Y, ';', X, 'H');
end;

procedure TSystemMonitor.ClearScreen;
begin
  Write(#27'[2J'#27'[H');
end;

procedure TSystemMonitor.SetColor(FG, BG: Integer);
begin
  Write(#27'[', FG + 30, ';', BG + 40, 'm');
end;

procedure TSystemMonitor.ResetColor;
begin
  Write(#27'[0m');
end;
{$ENDIF}

procedure TSystemMonitor.DrawBox(X, Y, Width, Height: Integer; Title: String);
var
  i: Integer;
begin
  GotoXY(X, Y);
  Write('┌');
  if Title <> '' then
  begin
    Write('─ ', Title, ' ');
    for i := 1 to Width - Length(Title) - 5 do
      Write('─');
  end
  else
    for i := 1 to Width - 2 do
      Write('─');
  Write('┐');

  for i := 1 to Height - 2 do
  begin
    GotoXY(X, Y + i);
    Write('│');
    GotoXY(X + Width - 1, Y + i);
    Write('│');
  end;

  GotoXY(X, Y + Height - 1);
  Write('└');
  for i := 1 to Width - 2 do
    Write('─');
  Write('┘');
end;

procedure TSystemMonitor.DrawProgressBar(X, Y, Width, Value: Integer; ShowPercent: Boolean = True);
var
  i, Filled: Integer;
  BarWidth: Integer;
begin
  if ShowPercent then
    BarWidth := Width - 6
  else
    BarWidth := Width;

  Filled := Round((Value / 100.0) * BarWidth);

  GotoXY(X, Y);
  Write('[');

  if Value > 75 then
    SetColor(1, 0)  // Red
  else if Value > 50 then
    SetColor(3, 0)  // Yellow
  else
    SetColor(2, 0); // Green

  for i := 1 to BarWidth do
  begin
    if i <= Filled then
      Write('█')
    else
      Write('░');
  end;

  ResetColor;
  Write(']');

  if ShowPercent then
    Write(Format(' %3d%%', [Value]));
end;

procedure TSystemMonitor.DrawCPUGraph(X, Y, Width, Height: Integer);
var
  i, j, h, DisplayIndex: Integer;
  Scale: Double;
  Line: String;
begin
  Scale := (Height - 1) / 100.0;

  for j := 0 to Height - 1 do
  begin
    GotoXY(X, Y + j);
    Line := '';

    for i := 0 to Width - 1 do
    begin
      if i < Length(CPUHistory) then
      begin
        DisplayIndex := (HistoryIndex + i) mod Length(CPUHistory);
        h := Round(CPUHistory[DisplayIndex] * Scale);

        if (Height - 1 - j) <= h then
        begin
          SetColor(2, 0);
          Line := Line + '█';
          ResetColor;
        end
        else
          Line := Line + ' ';
      end
      else
        Line := Line + ' ';
    end;
    Write(Line);
  end;
end;

{$IFDEF WINDOWS}
function TSystemMonitor.GetCPUUsage: Integer;
var
  IdleTime, KernelTime, UserTime: TFileTime;
  SysIdle, SysKernel, SysUser: Int64;
  DeltaIdle, DeltaKernel, DeltaUser, DeltaTotal: Int64;
begin
  Result := 0;
  if GetSystemTimes(@IdleTime, @KernelTime, @UserTime) then
  begin
    SysIdle := Int64(IdleTime.dwHighDateTime) shl 32 or IdleTime.dwLowDateTime;
    SysKernel := Int64(KernelTime.dwHighDateTime) shl 32 or KernelTime.dwLowDateTime;
    SysUser := Int64(UserTime.dwHighDateTime) shl 32 or UserTime.dwLowDateTime;

    if LastIdleTime <> 0 then
    begin
      DeltaIdle := SysIdle - LastIdleTime;
      DeltaKernel := SysKernel - LastKernelTime;
      DeltaUser := SysUser - LastUserTime;
      DeltaTotal := DeltaKernel + DeltaUser;

      if DeltaTotal > 0 then
        Result := Round(100.0 - (DeltaIdle * 100.0 / DeltaTotal));
    end;

    LastIdleTime := SysIdle;
    LastKernelTime := SysKernel;
    LastUserTime := SysUser;
  end;
  Result := Max(0, Min(100, Result));
end;

function TSystemMonitor.GetMemoryUsage: Integer;
var
  MemStatus: TMemoryStatusEx;
begin
  Result := 0;
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := MemStatus.dwMemoryLoad;
end;

function TSystemMonitor.GetMemoryStats: String;
var
  MemStatus: TMemoryStatusEx;
  TotalGB, UsedGB: Double;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
  begin
    TotalGB := MemStatus.ullTotalPhys / (1024 * 1024 * 1024);
    UsedGB := (MemStatus.ullTotalPhys - MemStatus.ullAvailPhys) / (1024 * 1024 * 1024);
    Result := Format('%.1f GB / %.1f GB', [UsedGB, TotalGB]);
  end
  else
    Result := 'N/A';
end;
{$ENDIF}

{$IFDEF UNIX}
function TSystemMonitor.GetCPUUsage: Integer;
var
  F: TextFile;
  Line: String;
  Parts: TStringList;
  User, Nice, System, Idle, IOWait, IRQ, SoftIRQ: QWord;
  Total, CurrentIdle: QWord;
  TotalDelta, IdleDelta: QWord;
begin
  Result := 0;
  Parts := TStringList.Create;
  try
    if FileExists('/proc/stat') then
    begin
      try
        AssignFile(F, '/proc/stat');
        Reset(F);
        ReadLn(F, Line);
        CloseFile(F);

        Parts.Delimiter := ' ';
        Parts.StrictDelimiter := False;
        Parts.DelimitedText := Line;

        if (Parts.Count >= 5) and (Parts[0] = 'cpu') then
        begin
          User := StrToQWordDef(Parts[1], 0);
          Nice := StrToQWordDef(Parts[2], 0);
          System := StrToQWordDef(Parts[3], 0);
          Idle := StrToQWordDef(Parts[4], 0);

          if Parts.Count >= 8 then
          begin
            IOWait := StrToQWordDef(Parts[5], 0);
            IRQ := StrToQWordDef(Parts[6], 0);
            SoftIRQ := StrToQWordDef(Parts[7], 0);
            Total := User + Nice + System + Idle + IOWait + IRQ + SoftIRQ;
          end
          else
            Total := User + Nice + System + Idle;

          CurrentIdle := Idle;

          if LastTotalTime > 0 then
          begin
            TotalDelta := Total - LastTotalTime;
            IdleDelta := CurrentIdle - LastIdleTime;

            if TotalDelta > 0 then
              Result := Round(100.0 * (1.0 - (IdleDelta / TotalDelta)));
          end;

          LastTotalTime := Total;
          LastIdleTime := CurrentIdle;
        end;
      except
        Result := 0;
      end;
    end;
  finally
    Parts.Free;
  end;

  Result := Max(0, Min(100, Result));
end;

function TSystemMonitor.GetMemoryUsage: Integer;
var
  F: TextFile;
  Line: String;
  Parts: TStringList;
  MemTotal, MemAvailable, MemFree, Buffers, Cached: QWord;
  UsedMem: QWord;
begin
  Result := 50;
  Parts := TStringList.Create;
  try
    MemTotal := 0;
    MemAvailable := 0;
    MemFree := 0;
    Buffers := 0;
    Cached := 0;

    if FileExists('/proc/meminfo') then
    begin
      try
        AssignFile(F, '/proc/meminfo');
        Reset(F);

        while not EOF(F) do
        begin
          ReadLn(F, Line);
          Parts.Delimiter := ' ';
          Parts.StrictDelimiter := False;
          Parts.DelimitedText := Line;

          if Parts.Count >= 2 then
          begin
            if Parts[0] = 'MemTotal:' then
              MemTotal := StrToQWordDef(Parts[1], 0)
            else if Parts[0] = 'MemAvailable:' then
              MemAvailable := StrToQWordDef(Parts[1], 0)
            else if Parts[0] = 'MemFree:' then
              MemFree := StrToQWordDef(Parts[1], 0)
            else if Parts[0] = 'Buffers:' then
              Buffers := StrToQWordDef(Parts[1], 0)
            else if Parts[0] = 'Cached:' then
              Cached := StrToQWordDef(Parts[1], 0);
          end;
        end;

        CloseFile(F);

        if MemTotal > 0 then
        begin
          if MemAvailable > 0 then
            Result := Round(100.0 * (1.0 - (MemAvailable / MemTotal)))
          else
          begin
            UsedMem := MemTotal - MemFree - Buffers - Cached;
            Result := Round(100.0 * (UsedMem / MemTotal));
          end;
        end;
      except
        Result := 50;
      end;
    end;
  finally
    Parts.Free;
  end;

  Result := Max(0, Min(100, Result));
end;

function TSystemMonitor.GetMemoryStats: String;
var
  F: TextFile;
  Line: String;
  Parts: TStringList;
  MemTotal, MemAvailable: QWord;
  TotalGB, UsedGB: Double;
begin
  Result := 'N/A';
  Parts := TStringList.Create;
  try
    MemTotal := 0;
    MemAvailable := 0;

    if FileExists('/proc/meminfo') then
    begin
      try
        AssignFile(F, '/proc/meminfo');
        Reset(F);

        while not EOF(F) do
        begin
          ReadLn(F, Line);
          Parts.Delimiter := ' ';
          Parts.StrictDelimiter := False;
          Parts.DelimitedText := Line;

          if Parts.Count >= 2 then
          begin
            if Parts[0] = 'MemTotal:' then
              MemTotal := StrToQWordDef(Parts[1], 0)
            else if Parts[0] = 'MemAvailable:' then
              MemAvailable := StrToQWordDef(Parts[1], 0);
          end;
        end;

        CloseFile(F);

        if MemTotal > 0 then
        begin
          TotalGB := MemTotal / (1024 * 1024);
          UsedGB := (MemTotal - MemAvailable) / (1024 * 1024);
          Result := Format('%.1f GB / %.1f GB', [UsedGB, TotalGB]);
        end;
      except
        Result := 'N/A';
      end;
    end;
  finally
    Parts.Free;
  end;
end;
{$ENDIF}

procedure TSystemMonitor.GetProcessList(var Lines: TStringList);
var
  AProcess: TProcess;
  i: Integer;
  Line: String;
  ShouldShow: Boolean;
begin
  Lines.Clear;

  AProcess := TProcess.Create(nil);
  try
    AProcess.Options := [poWaitOnExit, poUsePipes, poNoConsole];

    {$IFDEF WINDOWS}
    AProcess.Executable := 'tasklist';
    {$ENDIF}

    {$IFDEF UNIX}
    AProcess.Executable := '/bin/ps';
    AProcess.Parameters.Add('aux');
    {$ENDIF}

    try
      AProcess.Execute;
      Lines.LoadFromStream(AProcess.Output);

      // Filter kernel threads on Unix
      {$IFDEF UNIX}
      for i := Lines.Count - 1 downto 0 do
      begin
        Line := Lines[i];
        ShouldShow := True;

        if (Pos('[kworker', Line) > 0) or
           (Pos('[migration', Line) > 0) or
           (Pos('[ksoftirqd', Line) > 0) or
           (Pos('[rcu_', Line) > 0) or
           (Pos('[watchdog', Line) > 0) then
          ShouldShow := False;

        if not ShouldShow then
          Lines.Delete(i);
      end;
      {$ENDIF}

    except
      on E: Exception do
        Lines.Add('Error: ' + E.Message);
    end;
  finally
    AProcess.Free;
  end;
end;

procedure TSystemMonitor.ShowDashboard;
var
  CPUUsage, MemUsage: Integer;
  MemStats: String;
begin
  ClearScreen;

  SetColor(6, 0);
  DrawBox(2, 2, 96, 3, 'SYSTEM ACTIVITY MONITOR');
  ResetColor;

  CPUUsage := GetCPUUsage;
  MemUsage := GetMemoryUsage;
  MemStats := GetMemoryStats;

  DrawBox(2, 5, 96, 5, 'CPU Usage');
  GotoXY(4, 7);
  Write('CPU: ');
  DrawProgressBar(9, 7, 85, CPUUsage);

  DrawBox(2, 10, 96, 5, 'Memory Usage');
  GotoXY(4, 12);
  Write('RAM: ');
  DrawProgressBar(9, 12, 85, MemUsage);
  GotoXY(4, 13);
  Write('     ', MemStats);

  DrawBox(2, 15, 96, 15, 'CPU History');
  DrawCPUGraph(4, 17, 92, 12);

  CPUHistory[HistoryIndex] := CPUUsage;
  HistoryIndex := (HistoryIndex + 1) mod Length(CPUHistory);

  GotoXY(2, 31);
  SetColor(7, 0);
  Write('[1] Dashboard  [2] Processes  [Q] Quit  [R] Refresh');
  ResetColor;
end;

procedure TSystemMonitor.ShowProcesses;
var
  Lines: TStringList;
  i, MaxLines: Integer;
begin
  ClearScreen;

  SetColor(6, 0);
  DrawBox(2, 2, 96, 3, 'RUNNING PROCESSES');
  ResetColor;

  GotoXY(4, 6);
  Write('Loading processes...');

  Lines := TStringList.Create;
  try
    GetProcessList(Lines);

    ClearScreen;
    SetColor(6, 0);
    DrawBox(2, 2, 96, 3, 'RUNNING PROCESSES');
    ResetColor;

    MaxLines := 25;
    for i := 0 to Min(Lines.Count - 1, MaxLines - 1) do
    begin
      GotoXY(4, 6 + i);
      if Length(Lines[i]) > 92 then
        Write(Copy(Lines[i], 1, 92))
      else
        Write(Lines[i]);
    end;

    if Lines.Count > MaxLines then
    begin
      GotoXY(4, 6 + MaxLines);
      Write('... and ', Lines.Count - MaxLines, ' more processes');
    end;

  finally
    Lines.Free;
  end;

  GotoXY(2, 33);
  SetColor(7, 0);
  Write('[1] Dashboard  [2] Processes  [Q] Quit  [R] Refresh');
  ResetColor;
end;

procedure TSystemMonitor.Run;
var
  Choice: Char;
  CurrentView: Integer;
  i: Integer;
begin
  {$IFDEF UNIX}
  LastTotalTime := 0;
  LastIdleTime := 0;
  {$ENDIF}
  {$IFDEF WINDOWS}
  LastIdleTime := 0;
  LastKernelTime := 0;
  LastUserTime := 0;
  {$ENDIF}

  HistoryIndex := 0;
  for i := 0 to High(CPUHistory) do
    CPUHistory[i] := 0;

  GetCPUUsage;
  Sleep(500);

  CurrentView := 1; // 1 = Dashboard, 2 = Processes

  repeat
    if CurrentView = 1 then
      ShowDashboard
    else
      ShowProcesses;

    GotoXY(2, 35);
    Write('Choice: ');
    Read(Choice);
    Choice := UpCase(Choice);

    case Choice of
      '1': CurrentView := 1;
      '2': CurrentView := 2;
      'R': ; // Just refresh
    end;

    Sleep(100);
  until Choice = 'Q';

  ClearScreen;
  WriteLn('Thank you for using System Activity Monitor!');
end;

var
  Monitor: TSystemMonitor;

begin
  Monitor := TSystemMonitor.Create;
  try
    Monitor.Run;
  finally
    Monitor.Free;
  end;
end.
