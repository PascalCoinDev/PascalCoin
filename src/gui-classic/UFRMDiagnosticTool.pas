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
  TFRMDiagnosticTool = class(TForm)
    btnRH: TButton;
    btnRH2: TButton;
    txtLog: TMemo;
    btnRHC: TButton;
    btnEntropy: TButton;
    btnRH2NonceScan: TButton;
    procedure btnRH2Click(Sender: TObject);
    procedure btnRHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRHCClick(Sender: TObject);
    procedure btnEntropyClick(Sender: TObject);
    procedure btnRH2NonceScanClick(Sender: TObject);
  private
    { Private declarations }
    FDisposables : TDisposables;
    FRHThread : TPCThread;
    FRH2Thread : TPCThread;
    FRH2CachedThread : TPCThread;
    FRH2NonceScanThread : TPCThread;
    procedure OnReport(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
  public
    { Public declarations }
  end;

  { TAlgorithmNotify }

  TAlgorithmNotify  = procedure (const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan) of object;

  { TAlgorithmThread}

  TAlgorithmThread = class(TPCThread)
    private
      FDisposables : TDisposables;
      FNotifyHashCount : UInt32;
      FNotifyDuration : TTimeSpan;
      FNotify : TAlgorithmNotify;
      procedure ThreadSafeNotify;
    protected
      FTitle : String;
      FLastHash : TBytes;
      procedure BCExecute; override;
      function RandomizeNonce(const AHeader : TBytes): TBytes;
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; virtual; abstract;
    public
      property Notify : TAlgorithmNotify read FNotify write FNotify;
      constructor Create(const ATitle : String); virtual;
  end;

  { TRandomHashThread }

  TRandomHashThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHashFast;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2Thread }

  TRandomHash2Thread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2CachedThread }

  TRandomHash2CachedThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
    public
      constructor Create; overload;
  end;

  { TRandomHash2NonceScan }

  TRandomHash2NonceScan = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2;
    protected
      function TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean; override;
    public
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
  SetLength(FLastHash, 32);
end;

procedure TAlgorithmThread.BCExecute;
var
 LTC : TTickCount;
 LStartTime : TDateTime;
 LTotalHashes : UInt32;
begin
  LTotalHashes := 0;
  LStartTime := Now;
  LTC := TPlatform.GetTickCount;
  while True do begin
   if TryNextRound (RandomizeNonce(FLastHash), FLastHash) then
     inc(LTotalHashes);
   if TPlatform.GetElapsedMilliseconds(LTC)>2500 then begin
     FNotifyDuration := TTimeSpan.Subtract(Now, LStartTime);
     FNotifyHashCount := LTotalHashes;
     Queue( ThreadSafeNotify );
     LTotalHashes := 0;
     LStartTime := Now;
     LTC := TPlatform.GetTickCount;
   end;
  end;
end;

procedure TAlgorithmThread.ThreadSafeNotify;
begin
  if Assigned(FNotify) then
    FNotify(FTitle, FNotifyHashCount, FNotifyDuration);
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
  FDisposables.AddObject(FHasher);
end;

function TRandomHashThread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
   AOutput := FHasher.Hash(AInput);
   Result := True;
end;

{ TRandomHash2Thread }

constructor TRandomHash2Thread.Create;
begin
  Inherited Create('Random Hash 2');
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2Thread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  AOutput := FHasher.Hash(AInput);
  Result := True;
end;

{ TRandomHash2CachedThread }

constructor TRandomHash2CachedThread.Create;
begin
  Inherited Create('Random Hash (Cached)');
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2CachedThread.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  if FHasher.HasCachedHash then
    AOutput := FHasher.PopCachedHash.Hash
  else
    AOutput := FHasher.Hash(AInput);
  Result := True;
end;

{ TRandomHash2NonceScan }

constructor TRandomHash2NonceScan.Create;
begin
  Inherited Create('Random Hash (Nonce Scan)');
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2NonceScan.TryNextRound(const AInput : TBytes; out AOutput : TBytes) : Boolean;
begin
  if FHasher.HasCachedHash then begin
    AOutput := FHasher.PopCachedHash.Hash;
    Result := True;
  end else if FHasher.TryHash(AInput, TRandomHash2.MIN_N, AOutput) then begin
    Result := True;
  end else Result := False;
end;

{ TFRMDiagnosicTool }

procedure TFRMDiagnosticTool.FormCreate(Sender: TObject);
begin
  FRH2CachedThread := TRandomHash2CachedThread.Create;
  FRH2Thread := TRandomHash2Thread.Create;
  FRHThread := TRandomHashThread.Create;
  FRH2NonceScanThread := TRandomHash2NonceScan.Create;
  FDisposables.AddObject(FRHThread);
  FDisposables.AddObject(FRH2Thread);
  FDisposables.AddObject(FRH2CachedThread);
  FDisposables.AddObject(FRH2NonceScanThread);

  TAlgorithmThread(FRHThread).Notify := OnReport;
  TAlgorithmThread(FRH2Thread).Notify := OnReport;
  TAlgorithmThread(FRH2CachedThread).Notify := OnReport;
  TAlgorithmThread(FRH2NonceScanThread).Notify := OnReport;
end;

procedure TFRMDiagnosticTool.btnRHClick(Sender: TObject);
begin
  if FRHThread.Suspended then begin
    FRHThread.Suspended := False;
    btnRH.Caption := 'Stop Random Hash';
  end else begin
    FRHThread.Suspended := True;
    btnRH.Caption := 'Start Random Hash';
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
    FRH2Thread.Suspended := True;
    btnRH2.Caption := 'Start Random Hash 2';
  end;
end;

procedure TFRMDiagnosticTool.btnRHCClick(Sender: TObject);
begin
  if FRH2CachedThread.Suspended then begin
    FRH2CachedThread.Suspended := False;
    btnRHC.Caption := 'Stop Random Hash 2 (Cached)';
  end else begin
    FRH2CachedThread.Suspended := True;
    btnRHC.Caption := 'Start Random Hash 2 (Cached)';
  end;
end;

procedure TFRMDiagnosticTool.btnRH2NonceScanClick(Sender: TObject);
begin
  if FRH2NonceScanThread.Suspended then begin
    FRH2NonceScanThread.Suspended := False;
    btnRH2NonceScan.Caption := 'Stop Random Hash 2 (NonceScan)';
  end else begin
    FRH2NonceScanThread.Suspended := True;
    btnRH2NonceScan.Caption := 'Start Random Hash 2 (NonceScan)';
  end;
end;

procedure TFRMDiagnosticTool.OnReport(const ATitle : String; ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  txtLog.Text := txtLog.Text + Format('%s: %n H/S%s', [ATitle, LHPS, sLineBreak]);
end;

end.
