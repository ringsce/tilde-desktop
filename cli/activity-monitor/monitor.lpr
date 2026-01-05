program ActivityMonitor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Process
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  TSystemMonitor = class
  private
    {$IFDEF UNIX}
    LastTotalTime, LastIdleTime: QWord;
    {$ENDIF}
    {$IFDEF WINDOWS}
    LastIdleTime, LastKernelTime, LastUserTime: Int64;
    {$ENDIF}
    function GetCPUUsage: Integer;
    function GetMemoryUsage: Integer;
    procedure GetProcessList;
    procedure DrawProgressBar(AValue: Integer; AWidth: Integer = 40);
  public
    procedure ShowOverview;
    procedure ShowProcesses;
    procedure Run;
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

  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
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

  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function TSystemMonitor.GetMemoryUsage: Integer;
var
  MemStatus: TMemoryStatusEx;
begin
  Result := 0;
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := MemStatus.dwMemoryLoad;
end;
{$ENDIF}

{$IFDEF UNIX}
function TSystemMonitor.GetMemoryUsage: Integer;
var
  F: TextFile;
  Line: String;
  Parts: TStringList;
  MemTotal, MemAvailable, MemFree, Buffers, Cached: QWord;
  UsedMem: QWord;
begin
  Result := 0;
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
        Result := 0;
      end;
    end;
  finally
    Parts.Free;
  end;

  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;
{$ENDIF}

procedure TSystemMonitor.DrawProgressBar(AValue: Integer; AWidth: Integer = 40);
var
  i, Filled: Integer;
begin
  Filled := Round(AValue * AWidth / 100);
  Write('[');
  for i := 1 to AWidth do
  begin
    if i <= Filled then
      Write('█')
    else
      Write('░');
  end;
  Write('] ');
end;

procedure TSystemMonitor.ShowOverview;
var
  CPUUsage, MemUsage: Integer;
begin
  CPUUsage := GetCPUUsage;
  MemUsage := GetMemoryUsage;

  WriteLn('╔════════════════════════════════════════════════════════╗');
  WriteLn('║          SYSTEM ACTIVITY MONITOR - OVERVIEW           ║');
  WriteLn('╠════════════════════════════════════════════════════════╣');
  WriteLn('║                                                        ║');
  Write('║  CPU Usage:     ');
  DrawProgressBar(CPUUsage, 25);
  WriteLn(Format('%3d%%  ║', [CPUUsage]));
  WriteLn('║                                                        ║');
  Write('║  Memory Usage:  ');
  DrawProgressBar(MemUsage, 25);
  WriteLn(Format('%3d%%  ║', [MemUsage]));
  WriteLn('║                                                        ║');
  WriteLn('╚════════════════════════════════════════════════════════╝');
  WriteLn;
end;

procedure TSystemMonitor.GetProcessList;
var
  AProcess: TProcess;
  OutputLines: TStringList;
  i, Count: Integer;
  Line: String;
  ShouldShow: Boolean;
begin
  WriteLn('╔════════════════════════════════════════════════════════════════════════════════╗');
  WriteLn('║                            RUNNING PROCESSES                                   ║');
  WriteLn('╠════════════════════════════════════════════════════════════════════════════════╣');
  WriteLn;

  AProcess := TProcess.Create(nil);
  OutputLines := TStringList.Create;
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
      OutputLines.LoadFromStream(AProcess.Output);

      Count := 0;
      for i := 0 to OutputLines.Count - 1 do
      begin
        Line := OutputLines[i];
        if Trim(Line) <> '' then
        begin
          ShouldShow := True;

          // Only filter out obvious kernel threads on Unix
          {$IFDEF UNIX}
          if (Pos('[kworker', Line) > 0) or
             (Pos('[migration', Line) > 0) or
             (Pos('[ksoftirqd', Line) > 0) or
             (Pos('[rcu_', Line) > 0) or
             (Pos('[watchdog', Line) > 0) or
             (Pos('[cpuhp', Line) > 0) or
             (Pos('[kthreadd', Line) > 0) or
             (Pos('[mm_percpu', Line) > 0) then
            ShouldShow := False;
          {$ENDIF}

          if ShouldShow and (Count < 100) then
          begin
            if Length(Line) > 130 then
              WriteLn(Copy(Line, 1, 130) + '...')
            else
              WriteLn(Line);
            Inc(Count);
          end;
        end;
      end;

      WriteLn;
      WriteLn(Format('Showing %d processes (filtered kernel threads)', [Count]));

    except
      on E: Exception do
        WriteLn('Error getting process list: ', E.Message);
    end;
  finally
    OutputLines.Free;
    AProcess.Free;
  end;

  WriteLn;
  WriteLn('╚════════════════════════════════════════════════════════════════════════════════╝');
  WriteLn;
end;

procedure TSystemMonitor.ShowProcesses;
begin
  GetProcessList;
end;

procedure TSystemMonitor.Run;
var
  Choice: String;
  Running: Boolean;
begin
  Running := True;

  {$IFDEF UNIX}
  LastTotalTime := 0;
  LastIdleTime := 0;
  {$ENDIF}
  {$IFDEF WINDOWS}
  LastIdleTime := 0;
  LastKernelTime := 0;
  LastUserTime := 0;
  {$ENDIF}

  // Initial reading to establish baseline
  GetCPUUsage;
  Sleep(500);

  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('           SYSTEM ACTIVITY MONITOR v1.0                   ');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  while Running do
  begin
    {$IFDEF UNIX}
    Write(#27'[2J'#27'[H'); // Clear screen and move cursor to home on Unix
    {$ENDIF}

    ShowOverview;

    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('Commands:');
    WriteLn('  [1] Continue Monitoring (refreshes in 2 seconds)');
    WriteLn('  [2] Show Process List');
    WriteLn('  [3] Exit');
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn;
    Write('Enter choice: ');

    ReadLn(Choice);
    Choice := Trim(Choice);
    WriteLn;

    case Choice of
      '1', '':
        begin
          // Just continue and refresh
          Sleep(2000);
        end;
      '2':
        begin
          {$IFDEF UNIX}
          Write(#27'[2J'#27'[H');
          {$ENDIF}
          ShowProcesses;
          WriteLn('Press ENTER to continue...');
          ReadLn;
        end;
      '3', 'q', 'Q', 'quit', 'exit':
        begin
          Running := False;
          WriteLn('Exiting...');
        end;
      else
        begin
          WriteLn('Invalid choice. Refreshing in 2 seconds...');
          Sleep(2000);
        end;
    end;
  end;
end;

procedure Main;
var
  Monitor: TSystemMonitor;
begin
  Monitor := TSystemMonitor.Create;
  try
    Monitor.Run;
  finally
    Monitor.Free;
  end;
end;

begin
  Main;
end.
