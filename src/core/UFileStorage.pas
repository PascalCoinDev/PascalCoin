unit UFileStorage;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ./../config.inc}

uses
  Classes, {$IFnDEF FPC}Windows,{$ENDIF} UBlockChain, SyncObjs, UThread, UAccounts, UCrypto, UPCDataTypes;


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
    FStreamFirstBlockNumber : Int64;
    FStreamLastBlockNumber : Int64;
    FBlockHeadersFirstBytePosition : TArrayOfInt64;
    Function StreamReadBlockHeader(Stream: TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block: Cardinal; CanSearchBackward : Boolean; var BlockHeader : TBlockHeader): Boolean;
    Function StreamBlockRead(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function StreamBlockSave(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function GetBlockHeaderFirstBytePosition(Stream : TStream; Block : Cardinal; CanInitialize : Boolean; var iBlockHeaders : Integer; var BlockHeaderFirstBlock : Cardinal) : Boolean;
    Function GetBlockHeaderFixedSize : Int64;
    Procedure ClearStream;
    Procedure GrowStreamUntilPos(Stream : TStream; newPos : Int64; DeleteDataStartingAtCurrentPos : Boolean);
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(Start_Block : Cardinal; Const DestOrphan : TOrphan) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); override;
    Function DoBlockExists(Block : Cardinal) : Boolean; override;
    Function LockBlockChainStream : TFileStream;
    Procedure UnlockBlockChainStream;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoInitialize : Boolean; override;
    Procedure DoEraseStorage; override;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); override;
    Procedure SetBlockChainFile(BlockChainFileName : AnsiString);
  End;

implementation

Uses ULog, SysUtils, UBaseTypes,
  {$IFDEF USE_ABSTRACTMEM}
  UPCAbstractMem,
  {$ENDIF}
  UConst;
{ TFileStorage }

Const CT_TBlockHeader_NUL : TBlockHeader = (BlockNumber:0;StreamBlockRelStartPos:0;BlockSize:0);


  CT_Safebox_Extension = {$IFDEF USE_ABSTRACTMEM}'.am_safebox'{$ELSE}'.safebox'{$ENDIF};

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

function TFileStorage.DoBlockExists(Block: Cardinal): Boolean;
Var  iBlockHeaders : Integer;
  BlockHeaderFirstBlock : Cardinal;
  stream : TStream;
  BlockHeader : TBlockHeader;
begin
  Result := false;
  BlockHeader := CT_TBlockHeader_NUL;
  iBlockHeaders:=0; BlockHeaderFirstBlock:=0;
  stream := LockBlockChainStream;
  try
    if Not GetBlockHeaderFirstBytePosition(stream,Block,False,iBlockHeaders,BlockHeaderFirstBlock) then exit;
    if not StreamReadBlockHeader(stream,iBlockHeaders,BlockHeaderFirstBlock,Block,False,BlockHeader) then exit;
    Result := (BlockHeader.BlockNumber = Block) And
        (BlockHeader.BlockSize>0);
  finally
    UnlockBlockChainStream;
  end;
end;

procedure TFileStorage.ClearStream;
begin
  FreeAndNil(FBlockChainStream);
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

procedure TFileStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  inherited;
end;

constructor TFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FBlockChainStream := Nil;
  SetLength(FBlockHeadersFirstBytePosition,0);
  FStreamFirstBlockNumber := 0;
  FStreamLastBlockNumber := -1;
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

    TLog.NewLog(ltInfo,ClassName,Format('Deleting Blockchain block %d',[StartingDeleteBlock]));

    p := FBlockHeadersFirstBytePosition[iBlockHeaders] + (Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader));

    Stream.Position:=p;
    // Write null data until end of header
    GrowStreamUntilPos(Stream,FBlockHeadersFirstBytePosition[iBlockHeaders] + GetBlockHeaderFixedSize,true);
    // Force to clean Block Headers future rows
    SetLength(FBlockHeadersFirstBytePosition,iBlockHeaders+1); // Force to clear future blocks on next Block Headers row (Bug solved on 2.1.8)
    FStreamLastBlockNumber:=Int64(StartingDeleteBlock)-1;
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

function TFileStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan): Boolean;
Var db : TFileStorage;
  i : Integer;
  ops : TPCOperationsComp;
  b : Cardinal;
begin
  Try
    db := TFileStorage.Create(Nil);
    try
      db.Bank := Self.Bank;
      db.FStreamFirstBlockNumber := Start_Block;
      db.FStorageFilename := TPCBank.GetStorageFolder(DestOrphan)+PathDelim+'BlockChainStream.blocks';
      db.LockBlockChainStream;
      try
        db.FIsMovingBlockchain:=True;
        ops := TPCOperationsComp.Create(Nil);
        try
          b := Start_Block;
          while LoadBlockChainBlock(ops,b) do begin
            inc(b);
            TLog.NewLog(ltDebug,Classname,'Moving block from "'+Bank.Orphan+'" to "'+DestOrphan+'" '+TPCOperationsComp.OperationBlockToText(ops.OperationBlock));
            db.SaveBlockChainBlock(ops);
          end;
          TLog.NewLog(ltdebug,Classname,'Moved blockchain from "'+Bank.Orphan+'" to "'+DestOrphan+'" from block '+inttostr(Start_Block)+' to '+inttostr(b-1));
        finally
          ops.Free;
        end;
      finally
        db.FIsMovingBlockchain:=False;
        db.UnlockBlockChainStream;
      end;
    Finally
      db.Free;
    End;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error at DoMoveBlockChain: ('+E.ClassName+') '+E.Message);
      Raise;
    end;
  End;
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
  if Assigned(Bank) then Bank.SaveBank(False);
end;

Const CT_SafeboxsToStore = 10;

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

function TFileStorage.GetLastBlockNumber: Int64;
begin
  Result := FStreamLastBlockNumber;
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
    Result := False;
    //
    if stream.Size<GetBlockHeaderFixedSize then begin
      if (stream.Size=0) then begin
        Result := true;
        exit;
      end else begin
        // Invalid stream!
        if (ReadOnly) then begin
          errors := Format('Invalid stream size %d. Lower than minimum %d',[stream.Size, GetBlockHeaderFixedSize]);
          exit;
        end else begin
          // Clear it
          TLog.NewLog(ltError,ClassName,Format('Invalid stream size %d. Lower than minimum %d - Initialized to 0',[stream.Size, GetBlockHeaderFixedSize]));
          stream.size := 0;
          Result := True;
          Exit;
        end;
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
              errors := Format('Invalid BlockChain stream. First block start rel pos %d',[bh.StreamBlockRelStartPos]);
              if (ReadOnly) then begin
                Exit;
              end else begin
                FStreamFirstBlockNumber := 0;
                FStreamLastBlockNumber := -1;
                SetLength(FBlockHeadersFirstBytePosition,0);
                stream.Size:=0; // Set size to 0, no data
                TLog.NewLog(ltError,ClassName,Format('%s - Initialized to 0',[errors]));
                Result := True;
              end;
              Exit;
            end;
            lastbh := bh;
          end else begin
            // Protocol 2: We can find blocks not saved, with all values to 0
            if (bh.BlockNumber=0) then begin
              // This is an "empty" block. Check that ok
              If (bh.BlockNumber<>0) Or (bh.StreamBlockRelStartPos<>0) Or (bh.BlockSize<>0) then begin
                errors := Format('Invalid BlockChain stream not empty block on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                  [iPos,i,bh.BlockNumber,bh.StreamBlockRelStartPos,bh.BlockSize,
                   FStreamLastBlockNumber,
                   lastbh.BlockNumber,lastbh.StreamBlockRelStartPos,lastbh.BlockSize]);
                if (ReadOnly) then begin
                  Exit;
                end else begin
                  TLog.NewLog(lterror,ClassName,Format('%s - Initialized to %d',[errors,FStreamFirstBlockNumber]));
                  DoDeleteBlockChainBlocks(FStreamLastBlockNumber+1);
                  Result := True;
                  Exit;
                end;
              end;
              // Ok, inc blocknumber
              inc(lastbh.BlockNumber);
            end else begin
              if (lastbh.BlockNumber+1<>bh.BlockNumber) or
                ((lastbh.StreamBlockRelStartPos+lastbh.BlockSize<>bh.StreamBlockRelStartPos) And (i>0)) Or
                ((0<>bh.StreamBlockRelStartPos) And (i=0)) then begin
                errors := Format('Invalid BlockChain stream on block header. iPos=%d i=%d BlockNumber=%d relstart=%d size=%d - Last block:%d BlockNumber=%d relstart=%d size=%d',
                  [iPos,i,bh.BlockNumber,bh.StreamBlockRelStartPos,bh.BlockSize,FStreamLastBlockNumber,
                   lastbh.BlockNumber,lastbh.StreamBlockRelStartPos,lastbh.BlockSize]);
                If (ReadOnly) then begin
                  Exit;
                end else begin
                  TLog.NewLog(lterror,ClassName,Format('%s - Initialized to %d',[errors,FStreamFirstBlockNumber]));
                  DoDeleteBlockChainBlocks(FStreamLastBlockNumber+1);
                  Result := True;
                  Exit;
                end;
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
      Result := True;
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
      if FStorageFilename<>'' then begin
        fn := FStorageFilename
      end else begin
        fn := TPCBank.GetStorageFolder(Orphan)+PathDelim+'BlockChainStream.blocks';
        FStorageFilename := fn;
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
      end else begin
        TLog.NewLog(ltInfo,ClassName,Format('Loaded blockchain file: %s with blocks from %d to %d',[fn,FStreamFirstBlockNumber,FStreamLastBlockNumber]));
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
  FStorageFilename := BlockChainFileName;
end;

procedure TFileStorage.SetReadOnly(const Value: Boolean);
begin
  inherited;
  ClearStream;
end;

function TFileStorage.StreamBlockRead(Stream : TStream; iBlockHeaders : Integer; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
Var p : Int64;
  errors : String;
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
    // If this is an override, force to clean Block Headers future rows
    SetLength(FBlockHeadersFirstBytePosition,iBlockHeaders+1); // Force to clear future blocks on next Block Headers row (Bug solved on 2.1.8)
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

end.
