unit UFileMem.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

 uses
   SysUtils, Classes,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
   UFileMem, UAbstractMem, UCacheMem, UOrderedList;

{$I ./../../ConfigAbstractMem.inc }


 type
   // Test methods for class TCalc

   { TestTFileMem }

   TestTFileMem = class(TTestCase)
   strict private
  private
   public
     procedure SetUp; override;
     procedure TearDown; override;
     procedure Test_FileMem_Aux(AUseCache : Boolean);
     procedure Test_AbstractMem_Aux(AAbstractMem : TAbstractMem; AStrings : TStrings; ACreateBlocks : Integer);
     {$IFDEF FPC}
     function ElapsedTestTime : Integer;
     {$ENDIF}
     function GetFullFileName(AFileName : String) : String;
     procedure CheckConsistency(const AAbstractMem : TAbstractMem);
   published
     procedure Test_FileMem_Cache;
     procedure Test_FileMem_No_Cache;
     procedure Test_FileMame_MaxSize;
   end;

 implementation


{$IFDEF FPC}
function TestTFileMem.ElapsedTestTime: Integer;
begin
  Result := 0;
end;
{$ENDIF}

procedure TestTFileMem.CheckConsistency(const AAbstractMem: TAbstractMem);
var LStructure : TStrings;
  LAbstractMemZoneInfoList : TList<TAbstractMemZoneInfo>;
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : TAbstractMemPosition;
begin
  LStructure := TStringList.Create;
  Try
    IF Not AAbstractMem.CheckConsistency(LStructure,Nil,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then begin
      raise Exception.Create(LStructure.Text);
    end;
  Finally
    LStructure.Free;
  End;
end;

function TestTFileMem.GetFullFileName(AFileName: String): String;
begin
//  Result := 'C:\Users\Albert\Desktop\TEMP\'+AFileName; // XXXXXXXXXXXXXXXX
  Result := ExtractFileDir(ParamStr(0))+PathDelim+AFileName;
// XXXXXXXXXXXXXXXX
end;

procedure TestTFileMem.SetUp;
begin
end;

procedure TestTFileMem.TearDown;
begin
end;

procedure TestTFileMem.Test_AbstractMem_Aux(AAbstractMem: TAbstractMem; AStrings : TStrings; ACreateBlocks: Integer);
var LAMs : TList<TAMZone>;
  i, j, k : integer;
  LData : TBytes;
begin
  if ACreateBlocks<=0 then Exit;
  Randomize;
  LAMs := TList<TAMZone>.Create;
  try
    SetLength(LData,10000);
    for i := 0 to Length(LData)-1 do begin
      LData[i] := Byte(i+1);
    end;
    for i := 0 to ACreateBlocks do begin
      LAMs.Add( AAbstractMem.New(20*(Random(15)+1)) );
      AAbstractMem.Write(LAMs.Items[i].position,LData[0],LAMs.Items[i].size);
    end;

    AStrings.Add(Format('Create %d blocks with max %d bytes ET %d',[ACreateBlocks,Length(LData),ElapsedTestTime]));
    // Blocks created
    // Play with them
    for i := 0 to ACreateBlocks*4 do begin
      // Read randomly
      j := Random(LAMs.Count DIV 10);
      AAbstractMem.Read(LAMs.Items[j].position,LData[0],LAMs.Items[j].size);
    end;
    AStrings.Add(Format('Read %d blocks randomly ET %d',[ACreateBlocks,ElapsedTestTime]));
    //
    for i := 0 to ACreateBlocks*4 do begin
      // Write randomly
      j := Random(LAMs.Count DIV 10);
      AAbstractMem.Write(LAMs.Items[j].position,LData[0],LAMs.Items[j].size);
    end;
    AStrings.Add(Format('Write %d blocks randomly ET %d',[ACreateBlocks,ElapsedTestTime]));

  finally
    LAMs.Free;
  end;
end;

procedure TestTFileMem.Test_FileMame_MaxSize;
var Lfm : TFileMem;
  LTotalSize : Int64;
  Lfile : TFileStream;
  s : String;
  LData : TBytes;
  i : Integer;
var LAMs : TList<TAMZone>;
begin
  Lfm := TFileMem.Create(GetFullFileName('test_FileMem_Maxsize.am'),False);
  LAMs := TList<TAMZone>.Create;
  try
    Lfm.UseCache := False;
    CheckConsistency(Lfm);
    //
    SetLength(LData,50000000);
    for i := 0 to Length(LData)-1 do begin
      LData[i] := Byte(i+1);
    end;
    LTotalSize := Lfm.NextAvailablePos;
    i := 0;
    while Lfm.NextAvailablePos<((Int64(2147483648)*2) + (Length(LData)*2)) do begin
      LAMs.Add( Lfm.New(Length(LData)) );
      inc(LTotalSize, Int64(Length(LData)) );
      inc(i);
      Lfm.FlushCache;
      if Lfm.NextAvailablePos>MaxInt then begin
        CheckConsistency(Lfm);
      end;
    end;

  finally
    LAMs.Free;
    Lfm.Free;
  end;
end;

procedure TestTFileMem.Test_FileMem_Aux(AUseCache: Boolean);
var Lfm : TFileMem;
  Ltc : Int64;
  Lfs : TStrings;
  Lfile : TFileStream;
  s : String;
begin
  Lfm := TFileMem.Create(GetFullFileName('test_FileMem_Aux.am'),False);
  Lfs := TStringList.Create;
  try
    Lfm.ClearContent;
//    Lfm.UseCache := AUseCache;
    if AUseCache then begin
      Lfm.MaxCacheSize := 1024 * 1024 * 2; // 2 Mb
      Lfm.MaxCacheDataBlocks := 10000; // 10 K
      Lfs.Add(Format('Use cache with MaxCacheSize:%d and MaxCacheDataBlocks:%d',[Lfm.MaxCacheSize,Lfm.MaxCacheDataBlocks]));
    end else begin
      Lfs.Add('NO use cache');
    end;
    //
    Test_AbstractMem_Aux(Lfm,Lfs,50000);
    Lfm.FlushCache;

    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    if AUseCache then begin
      Lfs.Add( Lfm.LockCache.CacheMemStats.ToString );
      Lfm.UnlockCache;
    end;
    {$ENDIF}

    Lfs.Add(Format('Elapsed time %d',[ElapsedTestTime]));
    Lfs.Add('');

    s := ExtractFileDir(ParamStr(0))+PathDelim+'test_FileMem_Aux.log';
    if FileExists(s) then Lfile := TFileStream.Create(s,fmOpenReadWrite)
    else Lfile := TFileStream.Create(s,fmCreate);
    try
      Lfile.Seek(0,soFromEnd);
      Lfs.SaveToStream(lfile);
    finally
      Lfile.Free;
    end;
  finally
    Lfs.Free;
    Lfm.Free;
  end;
end;

procedure TestTFileMem.Test_FileMem_Cache;
begin
  Test_FileMem_Aux(True);
end;

procedure TestTFileMem.Test_FileMem_No_Cache;
begin
  Test_FileMem_Aux(False);
end;

initialization
  RegisterTest(TestTFileMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
