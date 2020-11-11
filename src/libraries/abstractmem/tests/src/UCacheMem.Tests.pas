unit UCacheMem.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
 
 uses
   SysUtils,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   UCacheMem;
 type
   // Test methods for class TCalc
   TestTCacheMem = class(TTestCase)
   strict private
     FCurrentMem : TBytes;
     function OnNeedDataProc(var ABuffer; AStartPos : Integer; ASize : Integer) : Boolean;
     function OnSaveDataProc(const ABuffer; AStartPos : Integer; ASize : Integer) : Boolean;
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure TestCacheMem;
   end;

 implementation

function TestTCacheMem.OnNeedDataProc(var ABuffer; AStartPos, ASize: Integer): Boolean;
begin
  if (High(FCurrentMem) >= AStartPos + ASize) then begin
    Result := True;
    Move(FCurrentMem[AStartPos],ABuffer,ASize);
  end else Result := False;
end;

function TestTCacheMem.OnSaveDataProc(const ABuffer; AStartPos, ASize: Integer): Boolean;
begin
  if (High(FCurrentMem) >= AStartPos + ASize) then begin
    Result := True;
    Move(ABuffer,FCurrentMem[AStartPos],ASize);
  end else Result := False;
end;

procedure TestTCacheMem.SetUp;
var i : Integer;
begin
  SetLength(FCurrentMem,100000);
  for i :=0 to High(FCurrentMem) do begin
    FCurrentMem[i] := i MOD 89;
  end;
end;

procedure TestTCacheMem.TearDown;
begin
  SetLength(FCurrentMem,0);
end;

procedure TestTCacheMem.TestCacheMem;
Var LCMem : TCacheMem;
  LBuff : TBytes;
  i : Integer;
begin
  LCMem := TCacheMem.Create(OnNeedDataProc,OnSaveDataProc);
  Try
    SetLength(LBuff,Length(FCurrentMem));
    CheckTrue( LCMem.LoadData(LBuff[0],0,100) );
    // Incremental round
    i := 1;
    while (i+i < High(FCurrentMem)) do begin
      CheckTrue( LCMem.LoadData(LBuff[0],i,i) );
      inc(i);
    end;
    CheckFalse( LCMem.LoadData( LBuff[0],i,i) );
    LCMem.ConsistencyCheck;
  Finally
    LCMem.Free;
  End;
end;

initialization
  RegisterTest(TestTCacheMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
