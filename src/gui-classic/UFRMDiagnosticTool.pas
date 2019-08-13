unit UFRMDiagnosticTool;

interface

uses
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  SysUtils, Variants, Classes, UThread, UMemory, URandomHash, URandomHash2,
  {$IFNDEF FPC}
  System.TimeSpan,
  {$ENDIF}
  UCommon;

type
  TAlgorithmThread = class;

  TFRMDiagnosticTool = class(TForm)
    btnRH: TButton;
    btnRH2: TButton;
    txtLog: TMemo;
    btnRH2C: TButton;
    btnEntropy: TButton;
    btnRH2NonceScan: TButton;
    txtScanLevel: TEdit;
    btnRHC: TButton;
    procedure btnRH2Click(Sender: TObject);
    procedure btnRHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRH2CClick(Sender: TObject);
    procedure btnEntropyClick(Sender: TObject);
    procedure btnRH2NonceScanClick(Sender: TObject);
    procedure btnRHCClick(Sender: TObject);
  private
    { Private declarations }
    FDisposables : TDisposables;
    FRHThread : TAlgorithmThread;
    FRHCachedThread : TAlgorithmThread;
    FRH2Thread : TAlgorithmThread;
    FRH2CachedThread : TAlgorithmThread;
    FRH2NonceScanThread : TAlgorithmThread;
    procedure OnReport(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
    procedure OnFinish(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan; const AMemStats : TStatistics);
  public
    { Public declarations }
  end;

  { TAlgorithmNotify }

  TAlgorithmNotify  = procedure (const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan) of object;

  { TAlgorithmFinishNotify }

  TAlgorithmFinishNotify  = procedure (const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan; const AMemStats : TStatistics) of object;

  { TAlgorithmThread }

  TAlgorithmThread = class(TPCThread)
    private
      FDisposables : TDisposables;
      FNotifyHashes : UInt32;
      FNotifyDuration : TTimeSpan;
      FNotify : TAlgorithmNotify;
      FNotifyFinish : TAlgorithmFinishNotify;
      FExitLoop : Boolean;
      procedure ThreadSafeNotify;
    protected
      FTitle : String;
      FLastHash : TBytes;
      procedure BCExecute; override;
      function RandomizeNonce(const AHeader : TBytes): TBytes;
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; virtual; abstract;
      function GetMemStats : TStatistics; virtual; abstract;
    public
      property Notify : TAlgorithmNotify read FNotify write FNotify;
      property NotifyFinish : TAlgorithmFinishNotify read FNotifyFinish write FNotifyFinish;
      constructor Create(const ATitle : String); virtual;
      procedure Finish;
  end;

  { TRandomHashThread }

  TRandomHashThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHashFast;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
      function GetMemStats : TStatistics; override;
    public
      constructor Create; overload;
  end;

  { TRandomHashCachedThread }

  TRandomHashCachedThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHashFast;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
      function GetMemStats : TStatistics; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2Thread }

  TRandomHash2Thread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2Fast;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
      function GetMemStats : TStatistics; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2CachedThread }

  TRandomHash2CachedThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2Fast;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
      function GetMemStats : TStatistics; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2NonceScan }

  TRandomHash2NonceScan = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2Fast;
      FLevel : Integer;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
      function GetMemStats : TStatistics; override;
    public
      property Level : Integer read FLevel write FLevel;
      constructor Create; overload;
  end;

implementation

uses UBaseTypes, UCrypto;

{$R *.dfm}

{ TAlgorithmThread }

constructor TAlgorithmThread.Create(const ATitle : String);
begin
  Inherited Create(True);
  FTitle := ATitle;
  FExitLoop := False;
  SetLength(FLastHash, 32);
end;

procedure TAlgorithmThread.Finish;
begin
  FExitLoop := True;
end;

procedure TAlgorithmThread.BCExecute;
var
 LTC : TTickCount;
 LInput : TBytes;
 LStartTime, LNotifyStartTime  : TDateTime;
 LTotalHashes, LNotifyHashes : UInt32;
begin
  SetLength(LInput, 200);
  while True do begin
    LTotalHashes := 0;
    LNotifyHashes := 0;
    LStartTime := Now;
    LNotifyStartTime := LStartTime;
    LTC := TPlatform.GetTickCount;
    while Not FExitLoop do begin
     if TryNextRound (RandomizeNonce(LInput), FLastHash) then begin
       inc(LTotalHashes);
       inc(LNotifyHashes);
       Move(FLastHash, LInput[High(LInput)-32], 32);  // randomize input
     end;
     if TPlatform.GetElapsedMilliseconds(LTC)>2500 then begin
       FNotifyHashes := LNotifyHashes;
       FNotifyDuration := TTimeSpan.Subtract(Now, LNotifyStartTime);
       LNotifyHashes := 0;
       LNotifyStartTime := Now;
       Queue( ThreadSafeNotify );
       LTC := TPlatform.GetTickCount;
     end;
    end;
    if Assigned(FNotifyFinish) then
      FNotifyFinish(FTitle, LTotalHashes, TTimeSpan.Subtract(Now, LStartTime), GetMemStats );
    FExitLoop := False;
    Suspended := True;
  end;
end;

procedure TAlgorithmThread.ThreadSafeNotify;
begin
  if Assigned(FNotify) then
    FNotify(FTitle, FNotifyHashes, FNotifyDuration);
end;

function TAlgorithmThread.RandomizeNonce(const AHeader : TBytes): TBytes;
var
  LNonce : UInt32;
  LLen : Integer;
begin
  if Length(AHeader) < 4 then
    Result := TBytes.Create(0,0,0,0)
  else
    Result := Copy(AHeader);
  LNonce := Random(MaxInt);
  // If digest not big enough to contain a nonce, just return the clone
  LLen := Length(Result);
  if LLen < 4 then
    exit;

  // Overwrite the nonce in little-endian
  Result[LLen - 4] := Byte(LNonce);
  Result[LLen - 3] := (LNonce SHR 8) AND 255;
  Result[LLen - 2] := (LNonce SHR 16) AND 255;
  Result[LLen - 1] := (LNonce SHR 24) AND 255;
end;

{ TRandomHashThread }

constructor TRandomHashThread.Create;
begin
  Inherited Create('Random Hash');
  FHasher := TRandomHashFast.Create;
  FHasher.EnableCache := False;
  FDisposables.AddObject(FHasher);
end;

function TRandomHashThread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
   AOutput := FHasher.Hash(AInput);
   Result := True;
end;

function TRandomHashThread.GetMemStats : TStatistics;
begin
  Result := FHasher.MemStats;
end;

{ TRandomHashCachedThread }

constructor TRandomHashCachedThread.Create;
begin
  Inherited Create('Random Hash (Cached)');
  FHasher := TRandomHashFast.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHashCachedThread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  if Length(FHasher.NextHeader) > 0 then
    AOutput := FHasher.Hash(FHasher.NextHeader)
  else
    AOutput := FHasher.Hash(AInput);
  Result := True;
end;

function TRandomHashCachedThread.GetMemStats : TStatistics;
begin
  Result := FHasher.MemStats;
end;

{ TRandomHash2Thread }

constructor TRandomHash2Thread.Create;
begin
  Inherited Create('Random Hash 2');
  FHasher := TRandomHash2Fast.Create;
  FHasher.EnableCaching := False;
  FHasher.CaptureMemStats := True;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2Thread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  AOutput := FHasher.Hash(AInput);
  Result := True;
end;

function TRandomHash2Thread.GetMemStats : TStatistics;
begin
  Result := FHasher.MemStats;
end;

{ TRandomHash2CachedThread }

constructor TRandomHash2CachedThread.Create;
begin
  Inherited Create('Random Hash 2 (Cached)');
  FHasher := TRandomHash2Fast.Create;
  FHasher.EnableCaching := True;
  FHasher.Cache.EnablePartiallyComputed := True;
  FHasher.CaptureMemStats := True;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2CachedThread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  if FHasher.Cache.HasComputedHash then
    AOutput := FHasher.Cache.PopComputedHash.RoundOutputs[0]
  else if FHasher.Cache.HasNextPartiallyComputedHash then
    AOutput := FHasher.ResumeHash(FHasher.Cache.PopNextPartiallyComputedHash)
  else
    AOutput := FHasher.Hash(AInput);
  Result := True;
end;

function TRandomHash2CachedThread.GetMemStats : TStatistics;
begin
  Result := FHasher.MemStats;
end;

{ TRandomHash2NonceScan }

constructor TRandomHash2NonceScan.Create;
begin
  Inherited Create('Random Hash 2 (Nonce Scan)');
  FHasher := TRandomHash2Fast.Create;
  FHasher.EnableCaching := True;
  FHasher.Cache.EnablePartiallyComputed := True;
  FHasher.CaptureMemStats := True;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2NonceScan.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  if FHasher.Cache.HasComputedHash then begin
    AOutput := FHasher.Cache.PopComputedHash.RoundOutputs[0];
    Result := True;
  end else if FHasher.Cache.HasNextPartiallyComputedHash then begin
    AOutput := FHasher.ResumeHash(FHasher.Cache.PopNextPartiallyComputedHash);
    Result := True;
  end else if FHasher.TryHash(AInput, FLevel, AOutput) then begin
    Result := True;
  end else Result := False;
end;

function TRandomHash2NonceScan.GetMemStats : TStatistics;
begin
  Result := FHasher.MemStats;
end;

{ TFRMDiagnosicTool }

procedure TFRMDiagnosticTool.FormCreate(Sender: TObject);
begin
  FRH2CachedThread := TRandomHash2CachedThread.Create;
  FRH2Thread := TRandomHash2Thread.Create;
  FRHThread := TRandomHashThread.Create;
  FRHCachedThread := TRandomHashCachedThread.Create;
  FRH2NonceScanThread := TRandomHash2NonceScan.Create;
  FDisposables.AddObject(FRHThread);
  FDisposables.AddObject(FRH2Thread);
  FDisposables.AddObject(FRH2CachedThread);
  FDisposables.AddObject(FRH2NonceScanThread);

  TAlgorithmThread(FRHThread).Notify := OnReport;
  TAlgorithmThread(FRHCachedThread).Notify := OnReport;
  TAlgorithmThread(FRH2Thread).Notify := OnReport;
  TAlgorithmThread(FRH2CachedThread).Notify := OnReport;
  TAlgorithmThread(FRH2NonceScanThread).Notify := OnReport;

  TAlgorithmThread(FRHThread).NotifyFinish := OnFinish;
  TAlgorithmThread(FRHCachedThread).NotifyFinish := OnFinish;
  TAlgorithmThread(FRH2Thread).NotifyFinish := OnFinish;
  TAlgorithmThread(FRH2CachedThread).NotifyFinish := OnFinish;
  TAlgorithmThread(FRH2NonceScanThread).NotifyFinish := OnFinish;
end;

procedure TFRMDiagnosticTool.btnRHClick(Sender: TObject);
begin
  if FRHThread.Suspended then begin
    FRHThread.Suspended := False;
    btnRH.Caption := 'Stop Random Hash';
  end else begin
    FRHThread.Finish;
    btnRH.Caption := 'Start Random Hash';
  end;
end;

procedure TFRMDiagnosticTool.btnRHCClick(Sender: TObject);
begin
  if FRHCachedThread.Suspended then begin
    FRHCachedThread.Suspended := False;
    btnRHC.Caption := 'Stop Random Hash (Cached)';
  end else begin
    FRHCachedThread.Finish;
    btnRHC.Caption := 'Start Random Hash (Cached)';
  end;
end;

procedure TFRMDiagnosticTool.btnEntropyClick(Sender: TObject);

  procedure SetLastDWordLE(var ABytes: TBytes;  AValue: UInt32);
  var
    LHeaderLength : Integer;
  begin
    // NOTE: NONCE is last 4 bytes of header!

    // If digest not big enough to contain a nonce, just return the clone
    LHeaderLength := Length(ABytes);
    if LHeaderLength < 4 then
      exit;

    // Overwrite the nonce in little-endian
    ABytes[LHeaderLength - 4] := Byte(AValue);
    ABytes[LHeaderLength - 3] := (AValue SHR 8) AND 255;
    ABytes[LHeaderLength - 2] := (AValue SHR 16) AND 255;
    ABytes[LHeaderLength - 1] := (AValue SHR 24) AND 255;
  end;

var LIn, LOut : TBytes; i : Integer; TXT : String;
begin
  SetLength(LIn, 200);
  FillChar(LIn, 200, 1);
  TXT := '';
  for I := 1 to 10 do begin
    LOut := TRandomHash2.Compute(LIn);
    TXT := TXT + Format('RH2( %s ) = %s %s', [ TCrypto.ToHexaString(LIn), TCrypto.ToHexaString(LOut), sLineBreak]);
    SetLastDWordLE(LIn, I);
  end;
  txtLog.Text := TXT;
end;

procedure TFRMDiagnosticTool.btnRH2Click(Sender: TObject);
begin
  if FRH2Thread.Suspended then begin
    FRH2Thread.Suspended := False;
    btnRH2.Caption := 'Stop Random Hash 2';
  end else begin
    FRH2Thread.Finish;
    btnRH2.Caption := 'Start Random Hash 2';
  end;
end;

procedure TFRMDiagnosticTool.btnRH2CClick(Sender: TObject);
begin
  if FRH2CachedThread.Suspended then begin
    FRH2CachedThread.Suspended := False;
    btnRHC.Caption := 'Stop Random Hash 2 (Cached)';
  end else begin
    FRH2CachedThread.Finish;
    btnRHC.Caption := 'Start Random Hash 2 (Cached)';
  end;
end;

procedure TFRMDiagnosticTool.btnRH2NonceScanClick(Sender: TObject);
var LLevel : Integer;
begin
  if FRH2NonceScanThread.Suspended then begin
    if TryStrToInt(txtScanLevel.Text, LLevel) then begin
      TRandomHash2NonceScan(FRH2NonceScanThread).Level := LLevel;
      FRH2NonceScanThread.Suspended := False;
      btnRH2NonceScan.Caption := 'Stop Random Hash 2 (NonceScan)';
      txtScanLevel.Enabled := false;
    end;
  end else begin
    FRH2NonceScanThread.Finish;
    btnRH2NonceScan.Caption := 'Start Random Hash 2 (NonceScan)';
    txtScanLevel.Enabled := true;
  end;
end;

procedure TFRMDiagnosticTool.OnReport(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  txtLog.Text := txtLog.Text + Format('%s: %n H/S%s', [ATitle, LHPS, sLineBreak]);
end;

procedure TFRMDiagnosticTool.OnFinish(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan; const AMemStats : TStatistics);
var
 LHPS : Double;
 LMemStats : String;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  LMemStats := Format('Samples: %d, Mean: %n, Std: %n, Min: %n, Max: %n', [AMemStats.SampleCount, AMemStats.Mean, AMemStats.SampleStandardDeviation, AMemStats.Minimum, AMemStats.Maximum]);
  txtLog.Text := txtLog.Text + Format('FINISH: %s: %n Mean H/S, MemStats: %s%s', [ATitle, LHPS, LMemStats, sLineBreak]);
end;

end.
