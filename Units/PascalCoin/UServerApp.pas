unit UServerApp;

{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}
{$ENDIF}

interface

uses
  {$IFDEF OS_MSWIN}
  Windows,
  Messages,
  {$ENDIF}
  SyncObjs;

type
  TPascalCoinServerLogType = (sltDebug, sltInfo, sltError, sltWarning);

  TPascalCoinServerAppLogEvent = procedure (LogType: TPascalCoinServerLogType;
      Msg: String; Level: Integer) of object;

  TPascalCoinServerApp = class
  private
    FLock : TCriticalSection;
    FTerminated : Boolean;
    {$IFDEF OS_MSWIN}
    hStdIn : THandle;
    {$ENDIF}
    FOnLog : TPascalCoinServerAppLogEvent;

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TPascalCoinServerLogType; const Msg: String;
              const Level: Integer = 0); overload;
    procedure Log(const LogType: TPascalCoinServerLogType; const Msg: String;
              const Params: array of const; const Level: Integer = 0); overload;

    function  GetTerminated: Boolean;
    procedure SetTerminated;

    function  ProcessOSMessage(out Terminate: Boolean): Boolean;
    function  QuitKeyPressed: Boolean;
    function  ProcessApplication: Boolean;
    function  Process: Boolean;
    procedure ProcessOrWait;

  public
    constructor Create;
    destructor Destroy; override;

    property  OnLog: TPascalCoinServerAppLogEvent read FOnLog write FOnLog;

    procedure Init;
    procedure Run;
    procedure Stop;

    property  Terminated: Boolean read GetTerminated;
  end;

var
  ServerApp : TPascalCoinServerApp = nil;

implementation

uses
  SysUtils;

{ TPascalCoinServerApp }

constructor TPascalCoinServerApp.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  {$IFDEF OS_MSWIN}
  // get the console input handle
  hStdIn := GetStdHandle(STD_INPUT_HANDLE);
  {$ENDIF}
end;

destructor TPascalCoinServerApp.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TPascalCoinServerApp.Lock;
begin
  FLock.Acquire;
end;

procedure TPascalCoinServerApp.Log(const LogType: TPascalCoinServerLogType;
          const Msg: String; const Level: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(LogType, Msg, Level);
end;

procedure TPascalCoinServerApp.Log(const LogType: TPascalCoinServerLogType;
  const Msg: String; const Params: array of const; const Level: Integer);
begin
  Log(LogType, Format(Msg, Params), Level);
end;

procedure TPascalCoinServerApp.Unlock;
begin
  FLock.Release;
end;

function TPascalCoinServerApp.GetTerminated: Boolean;
begin
  Lock;
  try
    Result := FTerminated;
  finally
    Unlock;
  end;
end;

procedure TPascalCoinServerApp.SetTerminated;
begin
  Lock;
  try
    FTerminated := True;
  finally
    Unlock;
  end;
end;

// Returns True if OS message processed
// Terminate is returned True if application terminated
function TPascalCoinServerApp.ProcessOSMessage(out Terminate: Boolean): Boolean;
{$IFDEF OS_MSWIN}
var
  Msg: TMsg;
{$ENDIF}
begin
  Terminate := False;
  {$IFDEF OS_MSWIN}
  Result := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);
  if Result then
    if Msg.Message = WM_QUIT then
      Terminate := True
    else
      begin
        TranslateMessage(Msg);
        DispatchMessageA(Msg);
      end
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TPascalCoinServerApp.QuitKeyPressed: Boolean;
{$IFDEF OS_MSWIN}
const
  MaxConsoleEvents = 64;
var
  NumberOfEvents     : DWORD;
  ConsoleEvents      : array[0..MaxConsoleEvents - 1] of TInputRecord;
  EvtP               : PInputRecord;
  NumberOfEventsRead : DWORD;
  I                  : Integer;
  QuitKeyDown        : Boolean;
begin
  QuitKeyDown := False;
  // get the number of events
  NumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(hStdIn, NumberOfEvents);
  if NumberOfEvents <> 0 then
    begin
      // retrieve the event
      NumberOfEventsRead := 0;
      PeekConsoleInput(hStdIn, ConsoleEvents[0], MaxConsoleEvents, NumberOfEventsRead);
      for I := 0 to NumberOfEventsRead - 1 do
        begin
          EvtP := @ConsoleEvents[I];
          if EvtP^.EventType = KEY_EVENT then
            if EvtP^.Event.KeyEvent.bKeyDown and
               ( (EvtP^.Event.KeyEvent.UnicodeChar = 'q') or
                 (EvtP^.Event.KeyEvent.UnicodeChar = 'Q') ) then
              begin
                QuitKeyDown := True;
                break;
              end;
        end;
      // flush the buffer
      FlushConsoleInputBuffer(hStdIn);
    end;
  Result := QuitKeyDown;
end;
{$ELSE}
var
  C : Char;
begin
  Result := False;
  Read(C);
  if (C = 'q') or (C = 'Q') then
    Result := True;
end;
{$ENDIF}

function TPascalCoinServerApp.ProcessApplication: Boolean;
begin
  Result := False;
end;

// Returns True if state processed
// Returns False if idle
function TPascalCoinServerApp.Process: Boolean;
var
  Busy : Boolean;
  DoTerminate : Boolean;
begin
  Busy := True;
  DoTerminate := False;
  if QuitKeyPressed then
    DoTerminate := True
  else
    if not ProcessOSMessage(DoTerminate) then
      if not ProcessApplication then
        Busy := False;
  if DoTerminate then
    SetTerminated;
  Result := Busy;
end;

procedure TPascalCoinServerApp.ProcessOrWait;
begin
  if not Process then
    Sleep(1);
end;

procedure TPascalCoinServerApp.Init;
begin
  Log(sltInfo, 'PascalCoin Server');
end;

procedure TPascalCoinServerApp.Run;
begin
  Log(sltInfo, 'Start');
  Log(sltInfo, 'Running (press Q to stop)');
  while not GetTerminated do
    ProcessOrWait;
end;

procedure TPascalCoinServerApp.Stop;
begin
  Log(sltInfo, 'Stop');
end;

end.
