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
     FReadCount, FSaveCount, FReadBytes, FSaveBytes : Int64;
     function OnNeedDataProc(var ABuffer; AStartPos : Int64; ASize : Integer) : Integer;
     function OnSaveDataProc(const ABuffer; AStartPos : Int64; ASize : Integer) : Integer;
     function OnNeedDataProc_BlackHole(var ABuffer; AStartPos : Int64; ASize : Integer) : Integer;
     function OnSaveDataProc_BlackHole(const ABuffer; AStartPos : Int64; ASize : Integer) : Integer;
     procedure CheckBytes(const ABytes : TBytes; ALoadedStartPos, ASize : Integer);
     procedure InitCurrentMem(ASize : Integer);
   public
     procedure SetUp; override;
     procedure TearDown; override;
     procedure TestCacheMem_Randomly(ASeed : Integer; ARounds : Integer; AMaxCacheSize : Int64; AMaxCacheDataBlocks : Int64; ADefaultCacheDataBlocksSize : Int64; AGridCache : Boolean);
     class function PosData(APosition : Int64) : Byte;
   published
     procedure TestCacheMem_1;
     procedure TestCacheMem;
     procedure TestCacheMem_64bits;
   end;

 implementation

procedure TestTCacheMem.CheckBytes(const ABytes: TBytes; ALoadedStartPos, ASize: Integer);
var i : Integer;
begin
  if ASize<=0 then ASize := Length(ABytes)
  else if ASize > Length(ABytes) then ASize := Length(ABytes);

  for i := 0 to ASize-1 do begin
    if (ABytes[i] <> (PosData(ALoadedStartPos+i))) then begin
      raise {$IFDEF FPC}Exception{$ELSE}ETestFailure{$ENDIF}.Create(Format('Value at pos %d (item %d) should be %d instead of %d',[ALoadedStartPos+i,i,PosData(ALoadedStartPos+i),ABytes[i]]));
    end;

  end;

end;

procedure TestTCacheMem.InitCurrentMem(ASize: Integer);
var i : Integer;
begin
  SetLength(FCurrentMem,ASize);
  for i :=0 to High(FCurrentMem) do begin
    FCurrentMem[i] := PosData(i)
  end;
  FReadCount := 0;
  FSaveCount := 0;
  FReadBytes := 0;
  FSaveBytes := 0;
end;

function TestTCacheMem.OnNeedDataProc(var ABuffer; AStartPos: Int64; ASize: Integer): Integer;
begin
  inc(FReadCount);
  inc(FReadBytes,ASize);
  if (Length(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(FCurrentMem[AStartPos],ABuffer,ASize);
  end else begin
    Result := Length(FCurrentMem) - AStartPos;
    if Result>0 then begin
      Move(FCurrentMem[AStartPos],ABuffer,Result);
    end;
  end;
end;

function TestTCacheMem.OnNeedDataProc_BlackHole(var ABuffer; AStartPos: Int64;
  ASize: Integer): Integer;
var LBuffer : TBytes;
begin
  // Just fill Buffer with 0 bytes
  FillChar(ABuffer,ASize,0);
  inc(FReadCount);
  inc(FReadBytes,ASize);
  Result := ASize;
end;

function TestTCacheMem.OnSaveDataProc(const ABuffer; AStartPos: Int64; ASize: Integer): Integer;
begin
  inc(FSaveCount);
  inc(FSaveBytes,ASize);
  if (Length(FCurrentMem) >= AStartPos + ASize) then begin
    Result := ASize;
    Move(ABuffer,FCurrentMem[AStartPos],ASize);
  end else begin
    Result := Length(FCurrentMem) - AStartPos;
    if Result>0 then begin
      Move(ABuffer,FCurrentMem[AStartPos],Result);
    end;
  end;
end;

function TestTCacheMem.OnSaveDataProc_BlackHole(const ABuffer; AStartPos: Int64;
  ASize: Integer): Integer;
begin
  inc(FSaveCount);
  inc(FSaveBytes,ASize);
  Result := ASize;
end;

class function TestTCacheMem.PosData(APosition: Int64): Byte;
begin
  Result := 10+((APosition+1) MOD 89);
end;

procedure TestTCacheMem.SetUp;
begin
  SetLength(FCurrentMem,0);
  FReadCount := 0;
  FSaveCount := 0;
  FReadBytes := 0;
  FSaveBytes := 0;
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
    InitCurrentMem(22);
    SetLength(LBuff,Length(FCurrentMem));

    LCMem.DefaultCacheDataBlocksSize :=5;
    LCMem.GridCache := True;
    // Check replacing initial position of buffer on Load
    LCMem.Clear;

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],3,3) );
    CheckBytes(LBuff,3,3);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],1,9) );
    CheckBytes(LBuff,1,9);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],9,2) );
    CheckBytes(LBuff,9,2);

    FillChar(LBuff[0],Length(LBuff),0);
    CheckTrue( LCMem.LoadData(LBuff[0],8,3) );
    CheckBytes(LBuff,8,3);

    // Check false and load final data
    FillChar(LBuff[0],Length(LBuff),0);
    CheckFalse( LCMem.LoadData(LBuff[0],Length(FCurrentMem)-3,4) );
    CheckBytes(LBuff,Length(FCurrentMem)-3,3);
    LCMem.ConsistencyCheck;

    // Load all to LBuff
    CheckTrue( LCMem.LoadData(LBuff[0],0,Length(LBuff)) );
    // Check replacing initial position of buffer on Save
    LCMem.Clear;
    LCMem.SaveToCache(LBuff[0],3,3,True);
    LCMem.SaveToCache(LBuff[0],7,0,True);

    // Check saving chunks
    LCMem.Clear;
    LCMem.DefaultCacheDataBlocksSize := 5;
    LCMem.GridCache := False;
    LCMem.SaveToCache(LBuff[2],5,2,True);
    LCMem.SaveToCache(LBuff[1],15,1,True);
    CheckTrue( LCMem.CacheDataBlocks=4, Format('3 Cache blocks: %d',[LCMem.CacheDataBlocks]));
    LCMem.Clear;
    LCMem.GridCache := True;
    LCMem.SaveToCache(LBuff[2],5,2,True);
    LCMem.SaveToCache(LBuff[1],15,1,True);
    CheckTrue( LCMem.CacheDataBlocks=4, Format('4 Cache blocks: %d',[LCMem.CacheDataBlocks]));
    LCMem.Clear;

    // Clear FCurrentMem
    LCMem.Clear;
    FillChar(FCurrentMem[0],Length(FCurrentMem),0);
    // Save from LBuff
    LCMem.SaveToCache(LBuff,0,True);
    LCMem.FlushCache;
    LCMem.ConsistencyCheck;

    LCMem.Clear;
    InitCurrentMem(100);
    SetLength(LBuff,Length(FCurrentMem));

    // Save 3 blocks
    LCMem.LoadData(LBuff[0],2,2*LCMem.DefaultCacheDataBlocksSize);
    LCMem.Clear;
    LCMem.SaveToCache(LBuff[0], 2*LCMem.DefaultCacheDataBlocksSize , 2,True);
    CheckTrue( LCMem.CacheDataBlocks=3, '3 Cache blocks');

    LCMem.ConsistencyCheck;

    CheckTrue( LCMem.LoadData(LBuff[0],1,98) );
    // Incremental round
    i := 1;
    while (i+i < High(FCurrentMem)) do begin
      CheckTrue( LCMem.LoadData(LBuff[0],i-1,i) );
      CheckBytes(LBuff,i-1,i);
      inc(i);
    end;
    CheckFalse( LCMem.LoadData( LBuff[0],i+1,i) );

    LCMem.ConsistencyCheck;
  Finally
    LCMem.Free;
  End;
end;

procedure TestTCacheMem.TestCacheMem_1;
Var
  iPos, nSize, nRounds, i : Integer;
begin
  TestCacheMem_Randomly(0,20000,1024*1024*100,5000,0,False);
  TestCacheMem_Randomly(0,20000,1024*1024*100,5000,500,False);
  TestCacheMem_Randomly(0,20000,1024*1024*100,5000,50,True);
end;

procedure TestTCacheMem.TestCacheMem_64bits;
Var LCMem : TCacheMem;
  LBuff : TBytes;
  i : Integer;
  LStartPos , LEndPos : Int64;

begin
  InitCurrentMem(0);
  SetLength(LBuff,256*200);
  LCMem := TCacheMem.Create(OnNeedDataProc_BlackHole,OnSaveDataProc_BlackHole);
  Try
    LCMem.GridCache := False;
    LCMem.DefaultCacheDataBlocksSize := -1;
    LCMem.MaxCacheSize := 1024*1024 * 1;
    LCMem.MaxCacheDataBlocks := 500;
    Try
      LStartPos := (256*256*256)-(1024*10);
      LEndPos := (LStartPos * 256) + Length(LBuff) + 1024;
      i := 0;
      repeat
        inc(i);
        Inc(LStartPos,Length(LBuff));
        LCMem.LoadData(LBuff[0],LStartPos,Length(LBuff));
        if (i MOD 2)=0 then begin
          LCMem.SaveToCache(LBuff,LStartPos,True);
        end;

      until LStartPos > LEndPos;
    Except
      on E:Exception do begin
        E.Message := Format('Round %d StartPos:%d %s (%s):%s',[i, LStartPos,LStartPos.ToHexString, E.ClassName,E.Message]);
        Raise;
      end;
    End;
    // Check replacing initial position of buffer on Load
    LCMem.Clear;
  Finally
    LCMem.Free;
  End;
end;

procedure TestTCacheMem.TestCacheMem_Randomly(ASeed, ARounds: Integer;
  AMaxCacheSize, AMaxCacheDataBlocks, ADefaultCacheDataBlocksSize: Int64;
  AGridCache: Boolean);
Var LCMem : TCacheMem;
  LBuff : TBytes;
  iPos, nSize, nRounds, i : Integer;
begin
  if ASeed>=0 then RandSeed := ASeed;

  LCMem := TCacheMem.Create(OnNeedDataProc,OnSaveDataProc);
  Try
    LCMem.MaxCacheSize := AMaxCacheSize;
    LCMem.MaxCacheDataBlocks := AMaxCacheDataBlocks;
    LCMem.DefaultCacheDataBlocksSize := ADefaultCacheDataBlocksSize;
    LCMem.GridCache := AGridCache;
    //
    InitCurrentMem(10000000);
    SetLength(LBuff,Length(FCurrentMem));
    nRounds := 0;
    //
    repeat
      inc(nRounds);
      iPos := Random(Length(FCurrentMem) - 1000);
      nSize := Random( (Length(FCurrentMem)-iPos) DIV 100000 )+1;
      if (Random(2)=0) then begin
        Assert(LCMem.LoadData(LBuff[0],iPos,nSize),Format('(Round %d) Cannot load data ad Pos %d size %d',[nRounds,iPos,nSize]));
        for i := 0 to nSize-1 do begin
          Assert(LBuff[i]=PosData(i+iPos),Format('(Round %d) Pos data %d (%d + %d) is %d not %d',[nRounds,i+iPos,i,iPos,LBuff[i],PosData(i+iPos)]));
          LBuff[i] := 0; // For future use
        end;
      end else begin
        // SAVE DATA TEST
        for i := 0 to nSize-1 do begin
          LBuff[i]:=PosData(i+iPos);
        end;
        LCMem.SaveToCache(LBuff[0],nSize,iPos,Random(2)=0);

        // CHECK this saved data
        Assert(LCMem.LoadData(LBuff[0],iPos,nSize),Format('(Round %d) Cannot load saved data ad Pos %d size %d',[nRounds,iPos,nSize]));
        for i := 0 to nSize-1 do begin
          Assert(LBuff[i]=PosData(i+iPos),Format('(Round %d) Pos saved data %d (%d + %d) is %d not %d',[nRounds,i+iPos,i,iPos,LBuff[i],PosData(i+iPos)]));
          LBuff[i] := 0; // For future use
        end;
      end;
      // Check
      if (Random(100)=0) then LCMem.ConsistencyCheck;
    until (nRounds>ARounds);
    LCMem.ConsistencyCheck;
  Finally
    LCMem.Free;
  End;
end;

initialization
  RegisterTest(TestTCacheMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
