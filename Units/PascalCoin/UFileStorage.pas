unit UFileStorage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, UBlockChain, SyncObjs, UThread, UAccounts, UCrypto;
{$I config.inc}

Type
  TBlockHeader = Record
    BlockNumber : Cardinal;
    StreamBlockRelStartPos : Int64;
    BlockSize : Cardinal;
  end; // 16 bytes

  TArrayOfInt64 = Array of Int64;

  { TFileStorage }

  TFileStorage = Class(TStorage)
  private
    FStorageLock : TPCCriticalSection;
    FBlockChainStream : TFileStream;
    FPendingBufferOperationsStream : TFileStream;
    FStreamFirstBlockNumber : Int64;
    FStreamLastBlockNumber : Int64;
    FBlockHeadersFirstBytePosition : TArrayOfInt64;
    FDatabaseFolder: AnsiString;
    FBlockChainFileName : AnsiString;
    Function StreamReadBlockHeader(Stream: TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block: Cardinal; CanSearchBackward : Boolean; var BlockHeader : TBlockHeader): Boolean;
    Function StreamBlockRead(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function StreamBlockSave(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function GetFolder(Const AOrphan : TOrphan): AnsiString;
    Function GetBlockHeaderFirstBytePosition(Stream : TStream; Block : Cardinal; CanInitialize : Boolean; var iBlockHeaders : Integer; var BlockHeaderFirstBlock : Cardinal) : Boolean;
    Function GetBlockHeaderFixedSize : Int64;
    procedure SetDatabaseFolder(const Value: AnsiString);
    Procedure ClearStream;
    Procedure GrowStreamUntilPos(Stream : TStream; newPos : Int64; DeleteDataStartingAtCurrentPos : Boolean);
    Function GetPendingBufferOperationsStream : TFileStream;
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    procedure SetOrphan(const Value: TOrphan); override;
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(Start_Block : Cardinal; Const DestOrphan : TOrphan; DestStorage : TStorage) : Boolean; override;
    Function DoSaveBank : Boolean; override;
    Function DoRestoreBank(max_block : Int64) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); override;
    Function BlockExists(Block : Cardinal) : Boolean; override;
    Function LockBlockChainStream : TFileStream;
    Procedure UnlockBlockChainStream;
    Function LoadBankFileInfo(Const Filename : AnsiString; var safeBoxHeader : TPCSafeBoxHeader) : Boolean;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoInitialize : Boolean; override;
    Function DoCreateSafeBoxStream(blockCount : Cardinal) : TStream; override;
    Procedure DoEraseStorage; override;
    Procedure DoSavePendingBufferOperations(OperationsHashTree : TOperationsHashTree); override;
    Procedure DoLoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree); override;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Function GetSafeboxCheckpointingFileName(Const BaseDataFolder : AnsiString; block : Cardinal) : AnsiString;
    Property DatabaseFolder : AnsiString read FDatabaseFolder write SetDatabaseFolder;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); override;
    Procedure SetBlockChainFile(BlockChainFileName : AnsiString);
    Function HasUpgradedToVersion2 : Boolean; override;
    Procedure CleanupVersion1Data; override;
  End;

implementation

Uses ULog, SysUtils, UConst;

{ TFileStorage }

Const CT_TBlockHeader_NUL : TBlockHeader = (BlockNumber:0;StreamBlockRelStartPos:0;BlockSize:0);

  CT_GroupBlockSize = 1000;
  CT_SizeOfBlockHeader = 16;
  {
  BlockChain file storage:

  BlockHeader 0 -> From Block 0 to (CT_GroupBlockSize-1)
    Foreach Block:
      BlockNumber : 4 bytes
      StreamBlockRelStartPos : 8 bytes  -> Start pos relative to End of BlockHeader
      BlockSizeH : 4 bytes
      -- Total size of BlockHeader: (4+8+4) * (CT_GroupBlockSize) = 16 * CT_GroupBlockSize
    -- Note: If BlockHeader starts at pos X, it ends at pos X + (16*CT_GroupBlockSize)
  Block 0
    BlockSizeC: 4 bytes
    Data: BlockSizeC bytes
  Block 1
    ...
  Block CT_GroupBlockSize-1

  BlockHeader 1 -> From Block CT_GroupBlockSize to ((CT_GroupBlockSize*2)-1)
    (Same as BlockHeader 1)
  Block CT_GroupBlockSize
    ...
  Block ((CT_GroupBlockSize*2)-1)

  ...
  BlockHeader X -> From (CT_GroupBlockSize*X) to ((CT_GroupBlockSize*(X+1))-1)
  ...

  }

function TFileStorage.BlockExists(Block: Cardinal): Boolean;
Var  iBlockHeaders : Integer;
  BlockHeaderFirstBlock : Cardinal;
  stream : TStream;
  BlockHeader : TBlockHeader;
begin
  Result := false;
  stream := LockBlockChainStream;
  try
    if Not GetBlockHeaderFirstBytePosition(stream,Block,False,iBlockHeaders,BlockHeaderFirstBlock) then exit;
    if not StreamReadBlockHeader(stream,iBlockHeaders,BlockHeaderFirstBlock,Block,False,BlockHeader) then exit;
    Result := (BlockHeader.BlockNumber = Block) And
        (((BlockHeader.BlockNumber MOD CT_GroupBlockSize)=0) OR (BlockHeader.StreamBlockRelStartPos>0)) And
        (BlockHeader.BlockSize>0);
  finally
    UnlockBlockChainStream;
  end;
end;

procedure TFileStorage.ClearStream;
begin
  FreeAndNil(FBlockChainStream);
  FreeAndNil(FPendingBufferOperationsStream);
  FStreamFirstBlockNumber := 0;
  FStreamLastBlockNumber := -1;
  SetLength(FBlockHeadersFirstBytePosition,0);
end;

procedure TFileStorage.GrowStreamUntilPos(Stream : TStream; newPos: Int64; DeleteDataStartingAtCurrentPos: Boolean);
Var null_buff : Array[1..CT_GroupBlockSize] of Byte;
  i,antPos,antSize : Int64;
begin
  antPos := Stream.Position;
  antSize := Stream.Size;
  if Not DeleteDataStartingAtCurrentPos then begin
    Stream.Position := Stream.Size;
  end;
  if (stream.Position<newPos) then begin
    FillChar(null_buff,length(null_buff),0);
    while (Stream.Position<newPos) do begin
      i := newPos - Stream.Position;
      if i>length(null_buff) then i := length(null_buff);
      Stream.WriteBuffer(null_buff,i);
    end;
  end;
  Stream.Position := newPos;
end;

function TFileStorage.GetPendingBufferOperationsStream: TFileStream;
Var fs : TFileStream;
  fn : TFileName;
  fm : Word;
begin
  If Not Assigned(FPendingBufferOperationsStream) then begin
    fn := GetFolder(Orphan)+PathDelim+'pendingbuffer.ops';
    If FileExists(fn) then fm := fmOpenReadWrite+fmShareExclusive
    else fm := fmCreate+fmShareExclusive;
    Try
      FPendingBufferOperationsStream := TFileStream.Create(fn,fm);
    Except
      On E:Exception do begin
        TLog.NewLog(ltError,ClassName,'Error opening PendingBufferOperationsStream '+fn+' ('+E.ClassName+'):'+ E.Message);
        Raise;
      end;
    end;
  end;
  Result := FPendingBufferOperationsStream;
end;

procedure TFileStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  inherited;
  if CopyFrom is TFileStorage then begin
    DatabaseFolder := TFileStorage(CopyFrom).DatabaseFolder;
  end;
end;

constructor TFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FDatabaseFolder := '';
  FBlockChainFileName := '';
  FBlockChainStream := Nil;
  SetLength(FBlockHeadersFirstBytePosition,0);
  FStreamFirstBlockNumber := 0;
  FStreamLastBlockNumber := -1;
  FPendingBufferOperationsStream := Nil;
  FStorageLock := TPCCriticalSection.Create('TFileStorage_StorageLock');
end;

destructor TFileStorage.Destroy;
begin
  inherited;
  ClearStream;
  FreeAndNil(FStorageLock);
end;

procedure TFileStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
Var stream : TStream;
  iBlockHeaders : Integer;
  BlockHeaderFirstBlock : Cardinal;
  _Header : TBlockHeader;
  _intBlockIndex : Cardinal;
  p : Int64;
begin
  stream := LockBlockChainStream;
  Try
    if Not GetBlockHeaderFirstBytePosition(stream,StartingDeleteBlock,False,iBlockHeaders,BlockHeaderFirstBlock) then exit;
    If Not StreamReadBlockHeader(Stream,iBlockHeaders,BlockHeaderFirstBlock,StartingDeleteBlock,True,_Header) then exit;
    _intBlockIndex := (_Header.BlockNumber-BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    Stream.Position:=p;
    // Write null data until end of header
    GrowStreamUntilPos(Stream,FBlockHeadersFirstBytePosition[iBlockHeaders] + GetBlockHeaderFixedSize,true);
    // End Stream at _Header
    Stream.Size := Stream.Position + _Header.StreamBlockRelStartPos;
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoInitialize: Boolean;
Var stream : TStream;
begin
  stream := LockBlockChainStream;
  Try
    Result := true;
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoCreateSafeBoxStream(blockCount: Cardinal): TStream;
var fn : TFilename;
  err : AnsiString;
begin
  Result := Nil;
  fn := GetSafeboxCheckpointingFileName(GetFolder(Orphan),blockCount);
  If (fn<>'') and (FileExists(fn)) then begin
    Result := TFileStream.Create(fn,fmOpenRead+fmShareDenyWrite);
  end;
  If Not Assigned(Result) then begin
    err := 'Cannot load SafeBoxStream (block:'+IntToStr(blockCount)+') file:'+fn;
    TLog.NewLog(ltError,ClassName,err);
  end;
end;

procedure TFileStorage.DoEraseStorage;
Var stream : TStream;
begin
  stream := LockBlockChainStream;
  try
    stream.Size:=0; // Erase
    ClearStream;
  finally
    UnlockBlockChainStream;
  end;
end;

procedure TFileStorage.DoSavePendingBufferOperations(OperationsHashTree : TOperationsHashTree);
Var fs : TFileStream;
begin
  LockBlockChainStream;
  Try
    fs := GetPendingBufferOperationsStream;
    fs.Position:=0;
    fs.Size:=0;
    OperationsHashTree.SaveOperationsHashTreeToStream(fs,true);
    TLog.NewLog(ltdebug,ClassName,Format('DoSavePendingBufferOperations operations:%d',[OperationsHashTree.OperationsCount]));
  finally
    UnlockBlockChainStream;
  end;
end;

procedure TFileStorage.DoLoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree);
Var fs : TFileStream;
  errors : AnsiString;
  n : Integer;
begin
  LockBlockChainStream;
  Try
    fs := GetPendingBufferOperationsStream;
    fs.Position:=0;
    If OperationsHashTree.LoadOperationsHashTreeFromStream(fs,true,true,errors) then begin
      TLog.NewLog(ltInfo,ClassName,Format('DoLoadPendingBufferOperations loaded operations:%d',[OperationsHashTree.OperationsCount]));
    end else TLog.NewLog(ltError,ClassName,Format('DoLoadPendingBufferOperations ERROR: loaded operations:%d errors:%s',[OperationsHashTree.OperationsCount,errors]));
  finally
    UnlockBlockChainStream;
  end;
end;

function TFileStorage.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
Var stream : TStream;
  iBlockHeaders : Integer;
  BlockHeaderFirstBlock : Cardinal;
begin
  Result := False;
  stream := LockBlockChainStream;
  Try
    if Not GetBlockHeaderFirstBytePosition(stream,Block,False,iBlockHeaders,BlockHeaderFirstBlock) then exit;
    Result := StreamBlockRead(stream,iBlockHeaders,BlockHeaderFirstBlock,Block,Operations);
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan; DestStorage : TStorage): Boolean;
  Procedure DoCopyFile(sourcefn,destfn : AnsiString);
  var sourceFS, destFS : TFileStream;
  Begin
    if Not FileExists(sourcefn) then Raise Exception.Create('Source file not found: '+sourcefn);
    sourceFS := TFileStream.Create(sourcefn,fmOpenRead+fmShareDenyNone);
    try
      sourceFS.Position:=0;
      destFS := TFileStream.Create(destfn,fmCreate+fmShareDenyWrite);
      try
        destFS.Size:=0;
        destFS.CopyFrom(sourceFS,sourceFS.Size);
      finally
        destFS.Free;
      end;
    finally
      sourceFS.Free;
    end;
  end;

  Procedure DoCopySafebox;
  var sr: TSearchRec;
    FileAttrs: Integer;
    folder : AnsiString;
    sourcefn,destfn : AnsiString;
  begin
    FileAttrs := faArchive;
    folder := GetFolder(Orphan);
    if SysUtils.FindFirst(GetFolder(Orphan)+PathDelim+'*.safebox', FileAttrs, sr) = 0 then begin
      repeat
        if (sr.Attr and FileAttrs) = FileAttrs then begin
          sourcefn := GetFolder(Orphan)+PathDelim+sr.Name;
          destfn := GetFolder('')+PathDelim+sr.Name;
          TLog.NewLog(ltInfo,ClassName,'Copying safebox file '+sourcefn+' to '+destfn);
          Try
            DoCopyFile(sourcefn,destfn);
          Except
            On E:Exception do begin
              TLog.NewLog(ltError,Classname,'Error copying file: ('+E.ClassName+') '+E.Message);
            end;
          End;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  End;

Var db : TFileStorage;
  i : Integer;
  ops : TPCOperationsComp;
  b : Cardinal;
begin
  Try
    if (Assigned(DestStorage)) And (DestStorage is TFileStorage) then db := TFileStorage(DestStorage)
    else db := Nil;
    try
      if Not assigned(db) then begin
        db := TFileStorage.Create(Nil);
        db.DatabaseFolder := Self.DatabaseFolder;
        db.Bank := Self.Bank;
        db.Orphan := DestOrphan;
        db.FStreamFirstBlockNumber := Start_Block;
      end;
      if db is TFileStorage then TFileStorage(db).LockBlockChainStream;
      try
        ops := TPCOperationsComp.Create(Nil);
        try
          b := Start_Block;
          while LoadBlockChainBlock(ops,b) do begin
            inc(b);
            TLog.NewLog(ltDebug,Classname,'Moving block from "'+Orphan+'" to "'+DestOrphan+'" '+TPCOperationsComp.OperationBlockToText(ops.OperationBlock));
            db.SaveBlockChainBlock(ops);
          end;
          TLog.NewLog(ltdebug,Classname,'Moved blockchain from "'+Orphan+'" to "'+DestOrphan+'" from block '+inttostr(Start_Block)+' to '+inttostr(b-1));
        finally
          ops.Free;
        end;
        // If DestOrphan is empty, then copy possible updated safebox (because, perhaps current saved safebox is from invalid blockchain)
        if (DestOrphan='') And (Orphan<>'') then begin
          DoCopySafebox;
        end;
      finally
        if db is TFileStorage then TFileStorage(db).UnlockBlockChainStream;
      end;
    Finally
      If Not Assigned(DestStorage) then db.Free;
    End;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error at DoMoveBlockChain: ('+E.ClassName+') '+E.Message);
      Raise;
    end;
  End;
end;

function TFileStorage.DoRestoreBank(max_block: Int64): Boolean;
var
    sr: TSearchRec;
    FileAttrs: Integer;
    folder : AnsiString;
    filename,auxfn : AnsiString;
    fs : TFileStream;
    ms : TMemoryStream;
    errors : AnsiString;
    blockscount : Cardinal;
    sbHeader : TPCSafeBoxHeader;
begin
  LockBlockChainStream;
  Try
    FileAttrs := faArchive;
    folder := GetFolder(Orphan);
    filename := '';
    blockscount := 0;
    if SysUtils.FindFirst(folder+PathDelim+'*.safebox', FileAttrs, sr) = 0 then begin
      repeat
        if (sr.Attr and FileAttrs) = FileAttrs then begin
          auxfn := folder+PathDelim+sr.Name;
          If LoadBankFileInfo(auxfn,sbHeader) then begin
            if (((max_block<0) Or (sbHeader.blocksCount<=max_block)) AND (sbHeader.blocksCount>blockscount)) And
              (sbHeader.startBlock=0) And (sbHeader.endBlock=sbHeader.startBlock+sbHeader.blocksCount-1) then begin
              filename := auxfn;
              blockscount := sbHeader.blocksCount;
            end;
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if (filename<>'') then begin
      TLog.NewLog(ltinfo,Self.ClassName,'Loading SafeBox with '+inttostr(blockscount)+' blocks from file '+filename);
      fs := TFileStream.Create(filename,fmOpenRead);
      try
        ms := TMemoryStream.Create;
        Try
          ms.CopyFrom(fs,0);
          fs.Position := 0;
          ms.Position := 0;
          if not Bank.LoadBankFromStream(ms,False,errors) then begin
            TLog.NewLog(lterror,ClassName,'Error reading bank from file: '+filename+ ' Error: '+errors);
          end;
        Finally
          ms.Free;
        End;
      finally
        fs.Free;
      end;
    end;
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoSaveBank: Boolean;
var fs: TFileStream;
    bankfilename: AnsiString;
    ms : TMemoryStream;
begin
  Result := true;
  bankfilename := GetSafeboxCheckpointingFileName(GetFolder(Orphan),Bank.BlocksCount);
  if (bankfilename<>'') then begin
    TLog.NewLog(ltInfo,ClassName,'Saving Safebox blocks:'+IntToStr(Bank.BlocksCount)+' file:'+bankfilename);
    fs := TFileStream.Create(bankfilename,fmCreate);
    try
      fs.Size := 0;
      ms := TMemoryStream.Create;
      try
        Bank.SafeBox.SaveSafeBoxToAStream(ms,0,Bank.SafeBox.BlocksCount-1);
        ms.Position := 0;
        fs.Position := 0;
        fs.CopyFrom(ms,0);
      finally
        ms.Free;
      end;
    finally
      fs.Free;
    end;
  end;
end;

function TFileStorage.DoSaveBlockChain(Operations: TPCOperationsComp): Boolean;
Var stream : TStream;
  iBlockHeaders : Integer;
  BlockHeaderFirstBlock : Cardinal;
begin
  Result := False;
  stream := LockBlockChainStream;
  Try
    if (Length(FBlockHeadersFirstBytePosition)=0) then begin
      // Is saving first block on the stream?
      if (Stream.Size=0) then begin
        // Yes! Positioning
        FStreamFirstBlockNumber := Operations.OperationBlock.block;
      end;
      TLog.NewLog(ltdebug,Classname,Format('Saving Block %d on a newer stream, stream first position=%d',[Operations.OperationBlock.block,FStreamFirstBlockNumber]));
    end;
    if Not GetBlockHeaderFirstBytePosition(stream,Operations.OperationBlock.block,True,iBlockHeaders,BlockHeaderFirstBlock) then exit;
    Result := StreamBlockSave(stream,iBlockHeaders,BlockHeaderFirstBlock,Operations);
  Finally
    UnlockBlockChainStream;
  End;
  if Assigned(Bank) then SaveBank;
end;

Const CT_SafeboxsToStore = 10;

class function TFileStorage.GetSafeboxCheckpointingFileName(const BaseDataFolder: AnsiString; block: Cardinal): AnsiString;
begin
  Result := '';
  If not ForceDirectories(BaseDataFolder) then exit;
  // We will store checkpointing
  Result := BaseDataFolder + PathDelim+'checkpoint'+ inttostr((block DIV CT_BankToDiskEveryNBlocks) MOD CT_SafeboxsToStore)+'.safebox';
end;

function TFileStorage.GetBlockHeaderFirstBytePosition(Stream : TStream; Block: Cardinal; CanInitialize : Boolean; var iBlockHeaders : Integer; var BlockHeaderFirstBlock: Cardinal): Boolean;
var iPos,start, nCurrBlock : Cardinal;
  bh : TBlockHeader;
  null_buff : Array[1..(CT_GroupBlockSize * CT_SizeOfBlockHeader)] of Byte;
begin
  Result := false;
  if Block<FStreamFirstBlockNumber then begin
    TLog.NewLog(lterror,Classname,Format('Block %d is lower than Stream First block %d',[Block,FStreamFirstBlockNumber]));
    exit;
  end;
  iPos := (Block-FStreamFirstBlockNumber) DIV CT_GroupBlockSize;
  if iPos>High(FBlockHeadersFirstBytePosition) then Begin
    if Length(FBlockHeadersFirstBytePosition)>0 then begin
      start := High(FBlockHeadersFirstBytePosition);
    end else begin
      If CanInitialize then begin
        // Initialize and start at 0
        SetLength(FBlockHeadersFirstBytePosition,1);
        FBlockHeadersFirstBytePosition[0] := 0;
        start := 0;
      end else exit;
    end;
    while (start<iPos) do begin
      // Read last start position
      if (Stream.Size<(FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize)) then begin
        // This position not exists...
        If (CanInitialize) then begin
          GrowStreamUntilPos(Stream,FBlockHeadersFirstBytePosition[start],false);
          // Save BlockHeader values (initialized to 0)
          FillChar(null_buff,length(null_buff),0);
          Stream.WriteBuffer(null_buff,length(null_buff));
        end else begin
          // This is a Fatal error due must find previos block!
          TLog.NewLog(ltError,Classname,Format('Stream size %d is lower than BlockHeader[%d] position %d + BlockHeaderSize %d',
            [Stream.size,start,FBlockHeadersFirstBytePosition[start],GetBlockHeaderFixedSize]));
          exit;
        end;
      end;
      Stream.Position := FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize - CT_SizeOfBlockHeader;
      // Read last saved Header
      nCurrBlock := FStreamFirstBlockNumber + ((start+1) * CT_GroupBlockSize) - 1;
      Repeat
        Stream.Read(bh.BlockNumber,SizeOf(bh.BlockNumber));
        Stream.Read(bh.StreamBlockRelStartPos,SizeOf(bh.StreamBlockRelStartPos));
        Stream.Read(bh.BlockSize,sizeof(bh.BlockSize));
        If (bh.BlockNumber<>nCurrBlock) then begin
          if (bh.BlockNumber<>0) Or (bh.StreamBlockRelStartPos<>0) Or (bh.BlockSize<>0) then begin
            TLog.NewLog(ltError,ClassName,Format('Fatal error. Found a Tblockheader with no 0 values searching for block:%d at nCurrBlock:%d - Number:%d RelStartPos:%d Size:%d',[block,nCurrBlock,bh.BlockNumber,bh.StreamBlockRelStartPos,bh.BlockSize]));
            exit;
          end;
          if ((start=0) And (nCurrBlock>FStreamFirstBlockNumber))
             Or
             ((start>0) And (nCurrBlock>(FStreamFirstBlockNumber + ((start) * CT_GroupBlockSize)))) then begin
            dec(nCurrBlock);
            // Positioning for new read:
            Stream.Seek(Int64(CT_SizeOfBlockHeader)*(-2),soFromCurrent);
          end else begin
            break; // End of blockheader!
          end;
        end;
      until (bh.BlockNumber>0);
      // Positioning!
      Stream.Position := FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize;
      //
      SetLength(FBlockHeadersFirstBytePosition,length(FBlockHeadersFirstBytePosition)+1);
      if bh.BlockNumber>0 then begin
        FBlockHeadersFirstBytePosition[High(FBlockHeadersFirstBytePosition)] := Stream.Position + bh.StreamBlockRelStartPos + bh.BlockSize;
      end else begin
        // Not found a block, starting at last pos
        FBlockHeadersFirstBytePosition[High(FBlockHeadersFirstBytePosition)] := Stream.Position;
      end;
      inc(start);

      // Check if blockheader size is ok:
      if (CanInitialize) And (Stream.Size<(FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize)) then begin
        Stream.Position := FBlockHeadersFirstBytePosition[start];
        TLog.NewLog(ltInfo,ClassName,Format('Increasing size for blockheader %d at pos:%d (current stream pos %d size %d) to position:%d',
          [start,FBlockHeadersFirstBytePosition[start],Stream.Position,Stream.Size,
          FBlockHeadersFirstBytePosition[start]+GetBlockHeaderFixedSize]));
        GrowStreamUntilPos(Stream,FBlockHeadersFirstBytePosition[start]+GetBlockHeaderFixedSize,true);
      end;

    end;
  End;
  iBlockHeaders := iPos;
  BlockHeaderFirstBlock := FStreamFirstBlockNumber + (iPos * CT_GroupBlockSize);
  Result := true;
end;

function TFileStorage.GetBlockHeaderFixedSize: Int64;
begin
  Result := (CT_GroupBlockSize* CT_SizeOfBlockHeader);
end;

function TFileStorage.GetFirstBlockNumber: Int64;
begin
  Result := FStreamFirstBlockNumber;
end;

function TFileStorage.GetFolder(const AOrphan: TOrphan): AnsiString;
begin
  if FDatabaseFolder = '' then raise Exception.Create('No Database Folder');
  if AOrphan<>'' then Result := FDatabaseFolder + PathDelim+AOrphan
  else Result := FDatabaseFolder;
  if not ForceDirectories(Result) then raise Exception.Create('Cannot create database folder: '+Result);
end;

function TFileStorage.GetLastBlockNumber: Int64;
begin
  Result := FStreamLastBlockNumber;
end;

function TFileStorage.LoadBankFileInfo(const Filename: AnsiString; var safeBoxHeader : TPCSafeBoxHeader) : Boolean;
var fs: TFileStream;
begin
  Result := false;
  safeBoxHeader := CT_PCSafeBoxHeader_NUL;
  If Not FileExists(Filename) then exit;
  fs := TFileStream.Create(Filename,fmOpenRead);
  try
    fs.Position:=0;
    Result := Bank.SafeBox.LoadSafeBoxStreamHeader(fs,safeBoxHeader);
  finally
    fs.Free;
  end;
end;

function TFileStorage.LockBlockChainStream: TFileStream;
  function InitStreamInfo(Stream : TStream; var errors : String) : Boolean;
  Var mem : TStream;
    iPos : Int64;
    i,j,k : Integer;
    bh,lastbh : TBlockHeader;
  begin
    errors := '';
    FStreamFirstBlockNumber := 0;
    FStreamLastBlockNumber := -1;
    SetLength(FBlockHeadersFirstBytePosition,0);
    //
    if stream.Size<GetBlockHeaderFixedSize then begin
      if (stream.Size=0) then begin
        Result := true;
        exit;
      end else begin
        // Invalid stream!
        Result := false;
        errors := Format('Invalid stream size %d. Lower than minimum %d',[stream.Size, GetBlockHeaderFixedSize]);
        exit;
      end;
    end;
    // Initialize it
    if stream.Size>GetBlockHeaderFixedSize then begin
      SetLength(FBlockHeadersFirstBytePosition,1);
      FBlockHeadersFirstBytePosition[0] := 0;
    end;
    mem := TMemoryStream.Create;
    Try
      iPos := 0;
      while (iPos + GetBlockHeaderFixedSize < Stream.Size) do begin
        Stream.Position := iPos;
        mem.Size := 0;
        mem.CopyFrom(Stream,GetBlockHeaderFixedSize);
        // Analize it:
        mem.Position := 0;
        for i := 0 to CT_GroupBlockSize-1 do begin
          mem.Read(bh.BlockNumber,SizeOf(bh.BlockNumber));
          mem.Read(bh.StreamBlockRelStartPos,SizeOf(bh.StreamBlockRelStartPos));
          mem.Read(bh.BlockSize,sizeof(bh.BlockSize));
          if (i=0) And (iPos=0) then begin
            FStreamFirstBlockNumber := bh.BlockNumber;
            FStreamLastBlockNumber := bh.BlockNumber;
            if (0<>bh.StreamBlockRelStartPos) then begin
              errors := Format('Invalid first block start rel pos %d',[bh.StreamBlockRelStartPos]);
              result := false;
              exit;
            end;
            lastbh := bh;
          end else begin
            // Protocol 2: We can find blocks not saved, with all values to 0
            if (bh.BlockNumber=0) then begin
              // This is an "empty" block. Check that ok
              If (bh.BlockNumber<>0) Or (bh.StreamBlockRelStartPos<>0) Or (bh.BlockSize<>0) then begin
                errors := Format('Invalid empty block on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                [iPos,i,bh.BlockNumber,bh.StreamBlockRelStartPos,bh.BlockSize,
                 FStreamLastBlockNumber,
                 lastbh.BlockNumber,lastbh.StreamBlockRelStartPos,lastbh.BlockSize]);
                result := false;
                exit;
              end;
              // Ok, inc blocknumber
              inc(lastbh.BlockNumber);
            end else begin
              if (lastbh.BlockNumber+1<>bh.BlockNumber) or
                ((lastbh.StreamBlockRelStartPos+lastbh.BlockSize<>bh.StreamBlockRelStartPos) And (i>0)) Or
                ((0<>bh.StreamBlockRelStartPos) And (i=0)) then begin
                errors := Format('Invalid check on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                  [iPos,i,bh.BlockNumber,bh.StreamBlockRelStartPos,bh.BlockSize,FStreamLastBlockNumber,
                   lastbh.BlockNumber,lastbh.StreamBlockRelStartPos,lastbh.BlockSize]);
                result := false;
                exit;
              end else begin
                FStreamLastBlockNumber := bh.BlockNumber;
                lastbh := bh;
              end;
            end;
          end;
        end;
        iPos := iPos + GetBlockHeaderFixedSize + lastbh.StreamBlockRelStartPos + lastBh.BlockSize;
        lastbh.StreamBlockRelStartPos:=0;
        lastbh.BlockSize:=0;
      end;
      Result := true;
    Finally
      mem.Free;
    End;
  end;

Var fn : TFileName;
  fm : Word;
  exists : Boolean;
  bh : TBlockHeader;
  errors : String;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FStorageLock);
  Try
    if Not Assigned(FBlockChainStream) then begin
      if FBlockChainFileName<>'' then begin
        fn := FBlockChainFileName
      end else begin
        fn := GetFolder(Orphan)+PathDelim+'BlockChainStream.blocks';
      end;
      exists := FileExists(fn);
      if ReadOnly then begin
        if exists then fm := fmOpenRead+fmShareDenyNone
        else raise Exception.Create('FileStorage not exists for open ReadOnly: '+fn);
      end else begin
        if exists then fm := fmOpenReadWrite+fmShareDenyWrite
        else fm := fmCreate+fmShareDenyWrite
      end;
      FBlockChainStream := TFileStream.Create(fn,fm);
      // Init stream
      If Not InitStreamInfo(FBlockChainStream,errors) then begin
        TLog.NewLog(lterror,ClassName,errors);
        raise Exception.Create('Error reading File: '+fn+#10+'Errors:'+#10+errors);
      end;
    end;
  Except
    FStorageLock.Release;
    Raise;
  End;
  Result := FBlockChainStream;
end;

procedure TFileStorage.SetBlockChainFile(BlockChainFileName: AnsiString);
begin
  ClearStream;
  FBlockChainFileName := BlockChainFileName;
end;

procedure TFileStorage.SetDatabaseFolder(const Value: AnsiString);
begin
  if FDatabaseFolder=Value then exit;
  FDatabaseFolder := Value;
  ClearStream;
end;

procedure TFileStorage.SetOrphan(const Value: TOrphan);
begin
  inherited;
  ClearStream;
end;

procedure TFileStorage.SetReadOnly(const Value: Boolean);
begin
  inherited;
  ClearStream;
end;

function TFileStorage.StreamBlockRead(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
Var p : Int64;
  errors : AnsiString;
  streamFirstBlock,
  _BlockSizeC,
  _intBlockIndex : Cardinal;
  _Header : TBlockHeader;
  _ops : TStream;
  _StreamBlockHeaderStartPos : Int64;
begin
  Result := False;
  If Not StreamReadBlockHeader(Stream,iBlockHeaders,BlockHeaderFirstBlock,Block,False,_Header) then exit;

  // Calculating block position
  _StreamBlockHeaderStartPos:=FBlockHeadersFirstBytePosition[iBlockHeaders];
  p := (_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize) +
     (_Header.StreamBlockRelStartPos);
  if Stream.Size<(p + _Header.BlockSize) then begin
    TLog.NewLog(ltError,Classname,Format(
      'Invalid stream size. Block %d need to be at relative %d after %d = %d BlockSize:%d (Size %d)',
      [Block,_Header.StreamBlockRelStartPos,(_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize),p,_Header.BlockSize,Stream.Size]));
    exit;
  end;
  Stream.Position := p;
  // Read the block
  // Reading size
  Stream.Read(_BlockSizeC,sizeof(_BlockSizeC));
  if ((_BlockSizeC+sizeof(_BlockSizeC))>(_Header.BlockSize)) then begin
    TLog.NewLog(lterror,Classname,Format('Corruption at stream Block size. Block %d SizeH:%d SizeC:%d',[Block,
      _Header.BlockSize,_BlockSizeC]));
    exit;
  end;
  // Reading Block
  _ops := TMemoryStream.Create;
  try
    _ops.CopyFrom(Stream,_BlockSizeC);
    _ops.Position := 0;
    If Not Operations.LoadBlockFromStorage(_ops,errors) then begin
      TLog.NewLog(lterror,Classname,'Error reading OperationBlock '+inttostr(Block)+' from stream. Errors: '+errors);
      exit;
    end;
    Result := true;
  Finally
    _ops.Free;
  end;
end;


function TFileStorage.StreamBlockSave(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock : Cardinal; Operations : TPCOperationsComp) : Boolean;
Var p : Int64;
  c : Cardinal;
  _Header, _HeaderPrevious : TBlockHeader;
  _intBlockIndex : Cardinal;
  _ops : TStream;
  _StreamBlockHeaderStartPos : Int64;
  {$IFDEF HIGHLOG}s : String;{$ENDIF}
begin
  Result := false;
  _Header := CT_TBlockHeader_NUL;
  _Header.BlockNumber := Operations.OperationBlock.block;
  if BlockHeaderFirstBlock>_Header.BlockNumber then raise Exception.Create('Dev error 20160917-3')
  else if BlockHeaderFirstBlock<_Header.BlockNumber then begin
    Result := StreamReadBlockHeader(Stream,iBlockHeaders,BlockHeaderFirstBlock,_Header.BlockNumber-1,True,_HeaderPrevious);
    // If true then Stream is positioned on blockheader for current block
    if not Result then begin
      raise Exception.Create('Cannot found header of previous block '+inttostr(Operations.OperationBlock.block));
    end;
    If ((_Header.BlockNumber-BlockHeaderFirstBlock) MOD CT_GroupBlockSize)=0 then begin
      _Header.StreamBlockRelStartPos := 0;
    end else begin
      _Header.StreamBlockRelStartPos := _HeaderPrevious.StreamBlockRelStartPos + _HeaderPrevious.BlockSize;
    end;
  end else begin
    // First block of the stream
    Result := true;
    _Header.StreamBlockRelStartPos := 0;
  end;
  _ops := TMemoryStream.Create;
  Try
    Operations.SaveBlockToStorage(_ops);
    _Header.BlockSize := _ops.Size;
    // Positioning until Header Position to save Header data
    _intBlockIndex := (_Header.BlockNumber-BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    _StreamBlockHeaderStartPos:=FBlockHeadersFirstBytePosition[iBlockHeaders];
    {$IFDEF HIGHLOG}s := Format('Saving block header (block %d) at position %d',[_Header.BlockNumber,Stream.Position]);{$ENDIF}
    GrowStreamUntilPos(Stream,_StreamBlockHeaderStartPos + p,false);
    // Save Header
    Stream.Write(_Header.BlockNumber,sizeof(_Header.BlockNumber));
    Stream.Write(_Header.StreamBlockRelStartPos,sizeof(_Header.StreamBlockRelStartPos));
    c := _Header.BlockSize + sizeof(c);
    Stream.Write(c,sizeof(_Header.BlockSize));
    // Positioning until Header end
    GrowStreamUntilPos(Stream,_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize,true);
    // And now positioning until Data:
    GrowStreamUntilPos(Stream,_StreamBlockHeaderStartPos + GetBlockHeaderFixedSize + _Header.StreamBlockRelStartPos, false );
    {$IFDEF HIGHLOG}
    s := s + Format(' saving content at position %d (size %d)',[Stream.Position,_Header.BlockSize]);
    TLog.NewLog(ltInfo,ClassName,s);
    {$ENDIF}
    // Save stream size
    Stream.Write(_Header.BlockSize,sizeof(_Header.BlockSize));
    // Save Data
    _ops.Position := 0;
    Stream.CopyFrom(_ops,_ops.Size);
    // End Stream here
    Stream.Size := Stream.Position;
    //
    FStreamLastBlockNumber := Operations.OperationBlock.block;
  Finally
    _ops.Free;
  end;
end;

function TFileStorage.StreamReadBlockHeader(Stream: TStream;
  iBlockHeaders : Integer; BlockHeaderFirstBlock, Block: Cardinal;
  CanSearchBackward : Boolean;
  var BlockHeader: TBlockHeader): Boolean;
Var iBlock : Cardinal;
  _iBlockHeaders : Integer;
  _StreamBlockHeaderStartPos: Int64;
  _BlockHeaderFirstBlock : Cardinal;
begin
  Result := false;
  BlockHeader := CT_TBlockHeader_NUL;
  if (BlockHeaderFirstBlock>Block) then raise Exception.Create('Dev error 20160917-1');
  if (BlockHeaderFirstBlock+CT_GroupBlockSize)<Block then raise Exception.Create('Dev error 20160917-2');
  _StreamBlockHeaderStartPos:=FBlockHeadersFirstBytePosition[iBlockHeaders];
  if Stream.Size< (_StreamBlockHeaderStartPos + (GetBlockHeaderFixedSize)) then begin
    // Not log... it's normal when finding block
    TLog.NewLog(ltError,Classname,Format('Invalid stream size %d < (%d + %d) Reading block %d',[Stream.Size,_StreamBlockHeaderStartPos,GetBlockHeaderFixedSize,Block]));
    exit;
  end;
  iBlock := Block;
  _iBlockHeaders:=iBlockHeaders;
  _BlockHeaderFirstBlock:=BlockHeaderFirstBlock;
  // Reading block header
  Repeat
    _StreamBlockHeaderStartPos:=FBlockHeadersFirstBytePosition[_iBlockHeaders];
    Stream.Position := _StreamBlockHeaderStartPos + (CT_SizeOfBlockHeader*(iBlock-_BlockHeaderFirstBlock));
    If Stream.Read(BlockHeader.BlockNumber,sizeof(BlockHeader.BlockNumber))<sizeof(BlockHeader.BlockNumber) then exit;
    If Stream.Read(BlockHeader.StreamBlockRelStartPos,sizeof(BlockHeader.StreamBlockRelStartPos))<sizeof(BlockHeader.StreamBlockRelStartPos) then exit;
    If Stream.Read(BlockHeader.BlockSize,sizeof(BlockHeader.BlockSize))<sizeof(BlockHeader.BlockSize) then exit;
    Result := (BlockHeader.BlockNumber = iBlock);
    If (Not Result) And (CanSearchBackward) then begin
      If (iBlock>_BlockHeaderFirstBlock) then dec(iBlock)
      else begin
        // Search on previous header...
        If (iBlockHeaders>0) then begin
          dec(_iBlockHeaders);
          dec(_BlockHeaderFirstBlock,CT_GroupBlockSize);
        end else begin
          break;
        end;
      end;
    end;
  Until Result Or (Not CanSearchBackward);
  // Positioning
  Stream.Position := FBlockHeadersFirstBytePosition[iBlockHeaders] + (CT_SizeOfBlockHeader*(Block+1-BlockHeaderFirstBlock));
  If Result then begin // For Backward searching...
    BlockHeader.BlockNumber:=Block;
    If (_iBlockHeaders<>iBlockHeaders) then begin
      BlockHeader.BlockSize:=0;
      BlockHeader.StreamBlockRelStartPos:=0;
    end;
  end;
end;

procedure TFileStorage.UnlockBlockChainStream;
begin
  FStorageLock.Release;
end;

function TFileStorage.HasUpgradedToVersion2: Boolean;
var searchRec: TSearchRec;
begin
  HasUpgradedToVersion2 := SysUtils.FindFirst( GetFolder(Orphan)+PathDelim+'*.safebox', faArchive, searchRec) = 0;
  FindClose(searchRec);
end;

procedure TFileStorage.CleanupVersion1Data;
var
  folder : AnsiString;
  searchRec : TSearchRec;
begin
  folder := GetFolder(Orphan);
  if SysUtils.FindFirst( folder+PathDelim+'*.bank', faArchive, searchRec) = 0 then
  repeat
    SysUtils.DeleteFile(folder+PathDelim+searchRec.Name);
  until FindNext(searchRec) <> 0;
  FindClose(searchRec);
end;

end.
