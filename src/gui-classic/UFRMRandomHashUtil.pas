unit UFRMDiagnosticTool;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.TimeSpan, UThread, UMemory, URandomHash, URandomHash2,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFRMDiagnosticTool = class(TForm)
    txtLog: TEdit;
    btnRH: TButton;
    btnRH2: TButton;
  private
    { Private declarations }
    TRHThread : TPCThread;
    TRH2Thread : TPCThread;
  public
    { Public declarations }
  end;

  TRandomHashNotify  = procedure (ATotalHashes : UInt32; const ATimeSpan : TTimeSpan) of object;

  TAlgorithmThread = class(TPCThread)
    private
      FDisposables : TDisposables;
      FNotifyHashCount : UInt32;
      FNotifyDuration : TTimeSpan;
      FNotify : TRandomHashNotify;
      procedure ThreadSafeNotify;
    protected
      FLastHash : TBytes;
      procedure BCExecute; override;
      function NextRound : TBytes; virtual; abstract;
    public
      constructor Create; virtual;
  end;

  TRandomHashThread = class(TAlgorithmThread)
    private
      FHasher : TRandomHash;
    protected
      function NextRound : TBytes; override;
    public
      constructor Create; override;
  end;

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
  FNotify(FNotifyHashCount, FNotifyDuration);
end;

{ TRandomHashThread }

constructor TRandomHashThread.Create;
begin
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
  FHasher := TRandomHash2.Create;
  FDisposables.AddObject(FHasher);
end;

function TRandomHash2Thread.NextRound : TBytes;
begin
   Result := FHasher.Hash(FLastHash);
end;

{ TFRMDiagnosicTool }

end.
