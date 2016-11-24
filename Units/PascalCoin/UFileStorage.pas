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
  Classes, UBlockChain, SyncObjs;

Type
  TBlockHeader = Record
    BlockNumber : Cardinal;
    StreamBlockRelStartPos : Int64;
    BlockSize : Cardinal;
  end; // 16 bytes

  { TFileStorage }

  TFileStorage = Class(TStorage)
  private
    FStorageLock : TCriticalSection;
    FBlockChainStream : TFileStream;
    FStreamFirstBlockNumber : Cardinal;
    FBlockHeadersFirstBytePosition : Array of Int64;
    FDatabaseFolder: AnsiString;
    Function StreamReadBlockHeader(Stream: TStream; StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock, Block: Cardinal; var BlockHeader : TBlockHeader): Boolean;
    Function StreamBlockRead(Stream : TStream; StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function StreamBlockSave(Stream : TStream; StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal; Operations : TPCOperationsComp) : Boolean;
    Function GetFolder(Const AOrphan : TOrphan): AnsiString;
    Function GetBlockHeaderFirstBytePosition(Stream : TStream; Block : Cardinal; var StreamBlockHeaderStartPos : Int64; var BlockHeaderFirstBlock : Cardinal) : Boolean;
    Function GetBlockHeaderFixedSize : Int64;
    procedure SetDatabaseFolder(const Value: AnsiString);
    Procedure ClearStream;
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
    Function LoadBankFileInfo(Const Filename : AnsiString; var BlocksCount : Cardinal) : Boolean;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Function GetBankFileName(Const BaseDataFolder : AnsiString; block : Cardinal) : AnsiString;
    Property DatabaseFolder : AnsiString read FDatabaseFolder write SetDatabaseFolder;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); override;
  End;

implementation

Uses ULog, SysUtils, UThread, UConst;

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
Var  StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal;
  stream : TStream;
  BlockHeader : TBlockHeader;
begin
  Result := false;
  stream := LockBlockChainStream;
  try
    if Not GetBlockHeaderFirstBytePosition(stream,Block,StreamBlockHeaderStartPos,BlockHeaderFirstBlock) then exit;
    if not StreamReadBlockHeader(stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,Block,BlockHeader) then exit;
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
  FStreamFirstBlockNumber := 0;
  SetLength(FBlockHeadersFirstBytePosition,0);
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
  FBlockChainStream := Nil;
  SetLength(FBlockHeadersFirstBytePosition,0);
  FStreamFirstBlockNumber := 0;
  FStorageLock := TCriticalSection.Create;
end;

destructor TFileStorage.Destroy;
begin
  inherited;
  ClearStream;
  FreeAndNil(FStorageLock);
end;

procedure TFileStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
Var stream : TStream;
  StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal;
  _Header : TBlockHeader;
  _intBlockIndex : Cardinal;
  p : Int64;
  Procedure GrowUntilPos(newPos : Int64; DeleteDataStartingAtCurrentPos : Boolean);
  Var b : Byte;
  begin
    b := 0;
    if Not DeleteDataStartingAtCurrentPos then begin
      Stream.Position := Stream.Size;
    end;
    While (Stream.Position<newPos) do begin
      Stream.Write(b,1);
    end;
    Stream.Position := newPos;
  end;
begin
  stream := LockBlockChainStream;
  Try
    if Not GetBlockHeaderFirstBytePosition(stream,StartingDeleteBlock,StreamBlockHeaderStartPos,BlockHeaderFirstBlock) then exit;
    If Not StreamReadBlockHeader(Stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,StartingDeleteBlock,_Header) then exit;
    _intBlockIndex := (_Header.BlockNumber-BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    // Write null data until end of header
    GrowUntilPos(StreamBlockHeaderStartPos + GetBlockHeaderFixedSize,true);
    // End Stream at _Header
    Stream.Size := Stream.Position + _Header.StreamBlockRelStartPos-1;
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
Var stream : TStream;
  StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal;
begin
  Result := False;
  stream := LockBlockChainStream;
  Try
    if Not GetBlockHeaderFirstBytePosition(stream,Block,StreamBlockHeaderStartPos,BlockHeaderFirstBlock) then exit;
    Result := StreamBlockRead(stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,Block,Operations);
  Finally
    UnlockBlockChainStream;
  End;
end;

function TFileStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan; DestStorage : TStorage): Boolean;
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
            db.SaveBlockChainBlock(ops);
          end;
          TLog.NewLog(ltdebug,Classname,'Moved blockchain from "'+Orphan+'" to "'+DestOrphan+'" from block '+inttostr(Start_Block)+' to '+inttostr(b-1));
        finally
          ops.Free;
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
    blockscount, c : Cardinal;
begin
  LockBlockChainStream;
  Try
    FileAttrs := faArchive;
    folder := GetFolder(Orphan);
    filename := '';
    blockscount := 0;
    if SysUtils.FindFirst(folder+PathDelim+'*.bank', FileAttrs, sr) = 0 then begin
      repeat
        if (sr.Attr and FileAttrs) = FileAttrs then begin
          auxfn := folder+PathDelim+sr.Name;
          If LoadBankFileInfo(auxfn,c) then begin
            if ((c<=max_block) AND (c>blockscount)) then begin
              filename := auxfn;
              blockscount := c;
            end;
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if (filename<>'') then begin
      fs := TFileStream.Create(filename,fmOpenRead);
      try
        ms := TMemoryStream.Create;
        Try
          ms.CopyFrom(fs,0);
          fs.Position := 0;
          ms.Position := 0;
          if not Bank.LoadBankFromStream(ms,errors) then begin
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
  bankfilename := GetBankFileName(GetFolder(Orphan),Bank.BlocksCount);
  if (bankfilename<>'') then begin
    fs := TFileStream.Create(bankfilename,fmCreate);
    try
      fs.Size := 0;
      ms := TMemoryStream.Create;
      try
        Bank.SaveBankToStream(ms);
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
  StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal;
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
    if Not GetBlockHeaderFirstBytePosition(stream,Operations.OperationBlock.block,StreamBlockHeaderStartPos,BlockHeaderFirstBlock) then exit;
    Result := StreamBlockSave(stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,Operations);
  Finally
    UnlockBlockChainStream;
  End;
  SaveBank;
end;

class function TFileStorage.GetBankFileName(const BaseDataFolder: AnsiString;
  block: Cardinal): AnsiString;
Var c : Cardinal;
  folder : AnsiString;
begin
  Result := '';
  If not ForceDirectories(BaseDataFolder) then exit;
  // We will store last 5 banks
  Result := BaseDataFolder + PathDelim+'bank'+ inttostr((block DIV CT_BankToDiskEveryNBlocks) MOD 5)+'.bank';
end;

function TFileStorage.GetBlockHeaderFirstBytePosition(Stream : TStream; Block: Cardinal; var StreamBlockHeaderStartPos: Int64; var BlockHeaderFirstBlock: Cardinal): Boolean;
var iPos,start : Cardinal;
  bh : TBlockHeader;
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
      // Initialize and start at 0
      SetLength(FBlockHeadersFirstBytePosition,1);
      FBlockHeadersFirstBytePosition[0] := 0;
      start := 0;
    end;
    while (start<iPos) do begin
      // Read last start position
      if (Stream.Size<(FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize)) then begin
        // This position not exists... This is a Fatal error due must find previos block!
        TLog.NewLog(ltError,Classname,Format('Stream size %d is lower than BlockHeader[%d] position %d + BlockHeaderSize %d',
          [Stream.size,start,FBlockHeadersFirstBytePosition[start],GetBlockHeaderFixedSize]));
        exit;
      end;
      Stream.Position := FBlockHeadersFirstBytePosition[start] + GetBlockHeaderFixedSize - CT_SizeOfBlockHeader;
      // Read last Header
      Stream.Read(bh.BlockNumber,SizeOf(bh.BlockNumber));
      Stream.Read(bh.StreamBlockRelStartPos,SizeOf(bh.StreamBlockRelStartPos));
      Stream.Read(bh.BlockSize,sizeof(bh.BlockSize));
      SetLength(FBlockHeadersFirstBytePosition,length(FBlockHeadersFirstBytePosition)+1);
      FBlockHeadersFirstBytePosition[High(FBlockHeadersFirstBytePosition)] := Stream.Position + bh.StreamBlockRelStartPos + bh.BlockSize;
      inc(start);
    end;
  End;
  StreamBlockHeaderStartPos := FBlockHeadersFirstBytePosition[iPos];
  BlockHeaderFirstBlock := FStreamFirstBlockNumber + (iPos * CT_GroupBlockSize);
  Result := true;
end;

function TFileStorage.GetBlockHeaderFixedSize: Int64;
begin
  Result := (CT_GroupBlockSize* CT_SizeOfBlockHeader);
end;

function TFileStorage.GetFolder(const AOrphan: TOrphan): AnsiString;
begin
  if FDatabaseFolder = '' then raise Exception.Create('No Database Folder');
  if AOrphan<>'' then Result := FDatabaseFolder + PathDelim+AOrphan
  else Result := FDatabaseFolder;
  if not ForceDirectories(Result) then raise Exception.Create('Cannot create database folder: '+Result);
end;

function TFileStorage.LoadBankFileInfo(const Filename: AnsiString; var BlocksCount: Cardinal): Boolean;
var fs: TFileStream;
begin
  Result := false;
  BlocksCount:=0;
  If Not FileExists(Filename) then exit;
  fs := TFileStream.Create(Filename,fmOpenRead);
  try
    fs.Position:=0;
    Result := Bank.LoadBankStreamHeader(fs,BlocksCount);
  finally
    fs.Free;
  end;
end;

function TFileStorage.LockBlockChainStream: TFileStream;
Var fn : TFileName;
  fm : Word;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FStorageLock);
  Try
    if Not Assigned(FBlockChainStream) then begin
      fn := GetFolder(Orphan)+PathDelim+'BlockChainStream.blocks';
      if ReadOnly then begin
        if FileExists(fn) then fm := fmOpenRead+fmShareDenyNone
        else raise Exception.Create('FileStorage not exists for open ReadOnly: '+fn);
      end else begin
        if FileExists(fn) then fm := fmOpenReadWrite+fmShareDenyWrite  // DenyNone -> XXXXXXXXX Sure not to use fmShareDenyWrite ???
        else fm := fmCreate+fmShareDenyWrite  // XXXXXXXXX Sure not to use fmShareDenyWrite too ???
      end;
      FBlockChainStream := TFileStream.Create(fn,fm);
      // Read Headers:
      SetLength(FBlockHeadersFirstBytePosition,0);
    end;
  Except
    FStorageLock.Release;
    Raise;
  End;
  Result := FBlockChainStream;
end;

procedure TFileStorage.SetDatabaseFolder(const Value: AnsiString);
begin
  if FDatabaseFolder=Value then exit;
  FDatabaseFolder := Value;
  FreeAndNil(FBlockChainStream);
  SetLength(FBlockHeadersFirstBytePosition,0);
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

function TFileStorage.StreamBlockRead(Stream : TStream; StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock, Block : Cardinal; Operations : TPCOperationsComp) : Boolean;
Var p : Int64;
  errors : AnsiString;
  streamFirstBlock,
  _BlockSizeC,
  _intBlockIndex : Cardinal;
  _Header : TBlockHeader;
  _ops : TStream;
begin
  Result := StreamReadBlockHeader(Stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,Block,_Header);
  if Not Result then exit;
  // Calculating block position
  p := (StreamBlockHeaderStartPos + GetBlockHeaderFixedSize) +
     (_Header.StreamBlockRelStartPos);
  if Stream.Size<(p + _Header.BlockSize) then begin
    TLog.NewLog(ltError,Classname,Format(
      'Invalid stream size. Block %d need to be at relative %d after %d = %d BlockSize:%d (Size %d)',
      [Block,_Header.StreamBlockRelStartPos,(StreamBlockHeaderStartPos + GetBlockHeaderFixedSize),p,_Header.BlockSize,Stream.Size]));
    exit;
  end;
  Stream.Position := p;
  // Read the block
  // Reading size
  Stream.Read(_BlockSizeC,sizeof(_BlockSizeC));
  if (_BlockSizeC>(_Header.BlockSize+sizeof(_BlockSizeC))) then begin
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


function TFileStorage.StreamBlockSave(Stream : TStream; StreamBlockHeaderStartPos : Int64; BlockHeaderFirstBlock : Cardinal; Operations : TPCOperationsComp) : Boolean;
  Procedure GrowUntilPos(newPos : Int64; DeleteDataStartingAtCurrentPos : Boolean);
  Var b : Byte;
  begin
    b := 0;
    if Not DeleteDataStartingAtCurrentPos then begin
      Stream.Position := Stream.Size;
    end;
    While (Stream.Position<newPos) do begin
      Stream.Write(b,1);
    end;
    Stream.Position := newPos;
  end;
Var p : Int64;
  c : Cardinal;
  _Header, _HeaderPrevious : TBlockHeader;
  _intBlockIndex : Cardinal;
  _ops : TStream;
begin
  Result := false;
  _Header := CT_TBlockHeader_NUL;
  _Header.BlockNumber := Operations.OperationBlock.block;
  if BlockHeaderFirstBlock>_Header.BlockNumber then raise Exception.Create('Dev error 20160917-3')
  else if BlockHeaderFirstBlock<_Header.BlockNumber then begin
    Result := StreamReadBlockHeader(Stream,StreamBlockHeaderStartPos,BlockHeaderFirstBlock,_Header.BlockNumber-1,_HeaderPrevious);
    if not Result then begin
      raise Exception.Create('Cannot found header of previous block '+inttostr(Operations.OperationBlock.block));
    end;
    _Header.StreamBlockRelStartPos := _HeaderPrevious.StreamBlockRelStartPos + _HeaderPrevious.BlockSize;
  end else begin
    // First block of the stream
    _Header.StreamBlockRelStartPos := 0;
  end;
  _ops := TMemoryStream.Create;
  Try
    Operations.SaveBlockToStorage(_ops);
    _Header.BlockSize := _ops.Size;
    // Positioning until Header Position to save Header data
    _intBlockIndex := (_Header.BlockNumber-BlockHeaderFirstBlock);
    p := Int64(_intBlockIndex) * Int64(CT_SizeOfBlockHeader);
    GrowUntilPos(StreamBlockHeaderStartPos + p,false);
    // Save Header
    Stream.Write(_Header.BlockNumber,sizeof(_Header.BlockNumber));
    Stream.Write(_Header.StreamBlockRelStartPos,sizeof(_Header.StreamBlockRelStartPos));
    c := _Header.BlockSize + sizeof(c);
    Stream.Write(c,sizeof(_Header.BlockSize));
    // Positioning until Header end
    GrowUntilPos(StreamBlockHeaderStartPos + GetBlockHeaderFixedSize,true);
    // And now positioning until Data:
    GrowUntilPos(StreamBlockHeaderStartPos + GetBlockHeaderFixedSize + _Header.StreamBlockRelStartPos, false );
    // Save stream size
    Stream.Write(_Header.BlockSize,sizeof(_Header.BlockSize));
    // Save Data
    _ops.Position := 0;
    Stream.CopyFrom(_ops,_ops.Size);
  Finally
    _ops.Free;
  end;
end;

function TFileStorage.StreamReadBlockHeader(Stream: TStream;
  StreamBlockHeaderStartPos: Int64; BlockHeaderFirstBlock, Block: Cardinal;
  var BlockHeader: TBlockHeader): Boolean;
Var p : Int64;
  errors : AnsiString;
  streamFirstBlock : Cardinal;
  _intBlockIndex : Cardinal;
  _Blocks : Cardinal;
begin
  Result := false;
  BlockHeader := CT_TBlockHeader_NUL;
  if (BlockHeaderFirstBlock>Block) then raise Exception.Create('Dev error 20160917-1');
  if (BlockHeaderFirstBlock+CT_GroupBlockSize)<Block then raise Exception.Create('Dev error 20160917-2');
  if Stream.Size< (StreamBlockHeaderStartPos + (GetBlockHeaderFixedSize)) then begin
    // Not log... it's normal when finding block   TLog.NewLog(ltError,Classname,Format('Invalid stream size %d < (%d + %d) Reading block %d',[Stream.Size,StreamBlockHeaderStartPos,GetBlockHeaderFixedSize,Block]));
    exit;
  end;
  Stream.Position := StreamBlockHeaderStartPos + (CT_SizeOfBlockHeader*(Block-BlockHeaderFirstBlock));
  // Reading block header
  If Stream.Read(BlockHeader.BlockNumber,sizeof(BlockHeader.BlockNumber))<sizeof(BlockHeader.BlockNumber) then exit;
  If Stream.Read(BlockHeader.StreamBlockRelStartPos,sizeof(BlockHeader.StreamBlockRelStartPos))<sizeof(BlockHeader.StreamBlockRelStartPos) then exit;
  If Stream.Read(BlockHeader.BlockSize,sizeof(BlockHeader.BlockSize))<sizeof(BlockHeader.BlockSize) then exit;
  Result := (BlockHeader.BlockNumber = Block);
end;

procedure TFileStorage.UnlockBlockChainStream;
begin
  FStorageLock.Release;
end;

end.
