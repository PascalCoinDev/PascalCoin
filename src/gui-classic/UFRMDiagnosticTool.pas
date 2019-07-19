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
    procedure btnRH2Click(Sender: TObject);
    procedure btnRHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRHCClick(Sender: TObject);
    procedure btnEntropyClick(Sender: TObject);
  private
    { Private declarations }
    FDisposables : TDisposables;
    FRHThread : TPCThread;
    FRH2Thread : TPCThread;
    FRH2CachedThread : TPCThread;
    procedure OnRandomHashReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
    procedure OnRandomHash2Report(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
    procedure OnRandomHash2CachedReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
  public
    { Public declarations }
  end;

  { TAlgorithmNotify }

  TAlgorithmNotify  = procedure (ATotalHashes : UInt32; const ATimeSpan : TTimeSpan) of object;

  { TAlgorithmThread}

  TAlgorithmThread = class(TPCThread)
    private
      FDisposables : TDisposables;
      FNotifyHashCount : UInt32;
      FNotifyDuration : TTimeSpan;
      FNotify : TAlgorithmNotify;
      procedure ThreadSafeNotify;
    protected
      FLastHash : TBytes;
      procedure BCExecute; override;
      function NextRound : TBytes; virtual; abstract;
    public
      property Notify : TAlgorithmNotify read FNotify write FNotify;
      constructor Create; virtual;
  end;

  { TRandomHashThread }

  TRandomHashThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHashFast;
    protected
      function NextRound : TBytes; override;
    public
      constructor Create; override;
  end;

  { TRandomHash2Thread }

  TRandomHash2Thread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2;
    protected
      function NextRound : TBytes; override;
    public
      constructor Create; override;
  end;

  { TRandomHash2CachedThread }

  TRandomHash2CachedThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash2;
    protected
      function NextRound : TBytes; override;
    public
      constructor Create; override;
  end;

implementation

uses UBaseTypes, UCrypto;

{$R *.dfm}

{ TAlgorithmThread }

constructor TAlgorithmThread.Create;
begin
  Inherited Create(True);
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
   FLastHash := NextRound;
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
    FNotify(FNotifyHashCount, FNotifyDuration);
end;

{ TRandomHashThread }

constructor TRandomHashThread.Create;
begin
  Inherited Create;
  FHasher := TRandomHashFast.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHashThread.NextRound : TBytes;
begin
   Result := FHasher.Hash(FLastHash);
end;

{ TRandomHash2Thread }

constructor TRandomHash2Thread.Create;
begin
  Inherited Create;
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2Thread.NextRound : TBytes;
begin
  Result := FHasher.Hash(FLastHash);
end;

{ TRandomHash2CachedThread }

constructor TRandomHash2CachedThread.Create;
begin
  Inherited Create;
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2CachedThread.NextRound : TBytes;
begin
  if FHasher.HasCachedHash then
    Result := FHasher.PopCachedHash.Hash
  else
    Result := FHasher.Hash(FLastHash);
end;



{ TFRMDiagnosicTool }

procedure TFRMDiagnosticTool.FormCreate(Sender: TObject);
begin
  FRH2CachedThread := TRandomHash2CachedThread.Create;
  FRH2Thread := TRandomHash2Thread.Create;
  FRHThread := TRandomHashThread.Create;
  FDisposables.AddObject(FRHThread);
  FDisposables.AddObject(FRH2Thread);
  FDisposables.AddObject(FRH2CachedThread);

  TRandomHashThread(FRHThread).Notify := OnRandomHashReport;
  TRandomHash2Thread(FRH2Thread).Notify := OnRandomHash2Report;
  TRandomHash2CachedThread(FRH2CachedThread).Notify := OnRandomHash2CachedReport;
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

procedure TFRMDiagnosticTool.OnRandomHashReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  txtLog.Text := txtLog.Text + Format('Random Hash: %n H/S%s', [LHPS, sLineBreak]);
end;

procedure TFRMDiagnosticTool.OnRandomHash2Report(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  txtLog.Text := txtLog.Text + Format('Random Hash 2: %n H/S%s', [LHPS, sLineBreak]);
end;

procedure TFRMDiagnosticTool.OnRandomHash2CachedReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Trunc(ATotalHashes) / ATimeSpan.TotalSeconds;
  txtLog.Text := txtLog.Text + Format('Random Hash 2 (Cached): %n H/S%s', [LHPS, sLineBreak]);
end;

end.
