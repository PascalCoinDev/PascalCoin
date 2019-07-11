unit UFRMDiagnosticTool;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.TimeSpan, UThread, UMemory, URandomHash, URandomHash2,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFRMDiagnosticTool = class(TForm)
    btnRH: TButton;
    btnRH2: TButton;
    txtLog: TMemo;
    procedure btnRH2Click(Sender: TObject);
    procedure btnRHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDisposables : TDisposables;
    FRHThread : TPCThread;
    FRH2Thread : TPCThread;
    procedure OnRandomHashReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
    procedure OnRandomHash2Report(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
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
      FHasher : TRandomHash;
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

implementation

uses UBaseTypes;

{$R *.dfm}

{ TAlgorithmThread }

constructor TAlgorithmThread.Create;
begin
  Inherited Create(True);
  SetLength(FLastHash, 32);
  FillChar(FLastHash, 32, 0);
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
   if TPlatform.GetElapsedMilliseconds(LTC)>1000 then begin
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
  FHasher := TRandomHash.Create;
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

{ TFRMDiagnosicTool }

procedure TFRMDiagnosticTool.FormCreate(Sender: TObject);
begin
  FRHThread := TRandomHashThread.Create;
  FRH2Thread := TRandomHash2Thread.Create;
  FDisposables.AddObject(FRHThread);
  FDisposables.AddObject(FRH2Thread);

  TRandomHashThread(FRHThread).Notify := OnRandomHashReport;
  TRandomHash2Thread(FRH2Thread).Notify := OnRandomHash2Report;
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

procedure TFRMDiagnosticTool.OnRandomHashReport(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Double(ATotalHashes) / Double(ATimeSpan.TotalSeconds);
  txtLog.Text := txtLog.Text + Format('Random Hash: %n H/S%s', [LHPS, sLineBreak]);
end;

procedure TFRMDiagnosticTool.OnRandomHash2Report(ATotalHashes : UInt32; const ATimeSpan : TTimeSpan);
var
 LHPS : Double;
begin
  LHPS := Double(ATotalHashes) / Double(ATimeSpan.TotalSeconds);
  txtLog.Text := txtLog.Text + Format('Random Hash 2: %n H/S%s', [LHPS, sLineBreak]);
end;

end.
