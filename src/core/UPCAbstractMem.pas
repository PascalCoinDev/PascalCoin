unit UPCAbstractMem;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ./../config.inc}

uses Classes, SysUtils, SyncObjs,
  UAbstractMem, UFileMem, UAbstractMemTList, UCacheMem,
  UAbstractBTree, UThread, UAbstractMemBTree,
  UAVLCache, ULog, UCrypto,
  UPCAbstractMemAccountKeys,
  UPCDataTypes, UBaseTypes, UConst, UPCSafeBoxRootHash, UOrderedList,
  UPCAccountsOrdenations,
  UPCAbstractMemAccounts,
{$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

type
  EPCAbstractMem = class(Exception);

  TPCAbstractMem = class;

  TOperationBlockExt = record
    operationBlock : TOperationBlock;
    accumulatedWork : UInt64;
  end;

  TPCAbstractMemListBlocks = class(TAbstractMemTList<TOperationBlockExt>)
  private
    FPCAbstractMem: TPCAbstractMem;
  protected
    function ToString(const AItem: TOperationBlockExt): string; override;

    procedure LoadFrom(const ABytes: TBytes; var AItem: TOperationBlockExt); override;
    procedure SaveTo(const AItem: TOperationBlockExt; AIsAddingItem : Boolean; var ABytes: TBytes); override;
  end;

  TAccountNameInfo = record
    accountName: string;
    accountNumber: cardinal;
  end;

  { TPCAbstractMemListAccountNames }

  TPCAbstractMemListAccountNames = Class(TAbstractMemBTreeData<TAccountNameInfo>)
  private
    FPCAbstractMem: TPCAbstractMem;
    procedure LoadFrom(const ABytes: TBytes; var AItem: TAccountNameInfo);
    procedure SaveTo(const AItem: TAccountNameInfo; var ABytes: TBytes);
    function FindByName(const AName : String; out AAbstractMemPosition : TAbstractMemPosition) : Boolean; overload;
  protected
    function LoadData(const APosition : TAbstractMemPosition) : TAccountNameInfo; override;
    function SaveData(const AData : TAccountNameInfo) : TAMZone; override;
  public
    function NodeDataToString(const AData : TAbstractMemPosition) : String; override;
    // Special
    procedure AddNameAndNumber(const AName : String; AAccountNumber : Cardinal);
    function FindByName(const AName : String) : Boolean; overload;
    function FindByName(const AName : String; out ANameInfo : TAccountNameInfo) : Boolean; overload;
    function DeleteAccountName(const AName : String) : Boolean;
  end;

  { TPCAbstractMemBytesBuffer32Safebox }

  TPCAbstractMemBytesBuffer32Safebox = Class(TBytesBuffer32Safebox)
  private
    FAbstractMem: TAbstractMem;
    FSaveBufferPosition : TAbstractMemPosition;
  protected
  public
    Constructor Create(AAbstractMem : TAbstractMem; APosition : TAbstractMemPosition; ACurrBlocksCount : Integer); reintroduce;
    procedure Flush;
  end;

  TPCAbstractMemCheckThread = Class(TPCThread)
    FPCAbstractMem : TPCAbstractMem;
    FErrorsCount : Integer;
    FErrors : TStrings;
    FMustRestart : Boolean;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(APCAbstractMem : TPCAbstractMem);
    Destructor Destroy; override;
    procedure Restart;
    property Errors : TStrings read FErrors;
  End;

  TAccountCache = Class(TAVLCache<TAccount>)
  End;

  TPCAbstractMemStats = Record
    FlushesCount : Integer;
    FlushesMillis : TTickCount;
    function ToString : String;
    procedure Clear;
  end;

  TPCAbstractMem = class
  private
    FFileName : String;
    FAbstractMem: TAbstractMem;
    FCheckingThread : TPCAbstractMemCheckThread;
    FLockAbstractMem : TPCCriticalSection;

    FStats : TPCAbstractMemStats;

    FBlocks: TPCAbstractMemListBlocks;
    FAccounts: TPCAbstractMemListAccounts;
    FAccountsNames: TPCAbstractMemListAccountNames;
    FAccountKeys: TPCAbstractMemAccountKeys;
    FAccountCache : TAccountCache;
    FBufferBlocksHash: TPCAbstractMemBytesBuffer32Safebox;
    FAggregatedHashrate : TBigNum;
    FZoneAggregatedHashrate : TAMZone;
    FUseCacheOnAbstractMemLists: Boolean;
    FMaxMemUsage: Integer;
    FSavingNewSafeboxMode: Boolean;

    FSavingOldGridCache : Boolean;
    FSavingOldDefaultCacheDataBlocksSize : Integer;
    FAccountsOrderedByUpdatedBlock : TAccountsOrderedByUpdatedBlock;
    FAccountsOrderedBySalePrice : TAccountsOrderedBySalePrice;

    function IsChecking : Boolean;
    procedure DoCheck;


    procedure AddBlockInfo(const ABlock : TOperationBlockExt);
    procedure SetBlockInfo(const ABlock : TOperationBlockExt);
    function DoInit(out AIsNewStructure : Boolean) : Boolean;
    procedure SetMaxMemUsage(const Value: Integer);
    procedure SetUseCacheOnAbstractMemLists(const Value: Boolean);
    procedure SetMaxAccountsCache(const Value: Integer);
    function GetMaxAccountsCache: Integer;
    function GetMaxAccountKeysCache: Integer;
    procedure SetMaxAccountKeysCache(const Value: Integer);
    procedure SetSavingNewSafeboxMode(const Value: Boolean);
  protected
    procedure UpgradeAbstractMemVersion(const ACurrentHeaderVersion : Integer);
    function DoGetAccount(AAccountNumber : Integer; var AAccount : TAccount) : Boolean;
  public
    constructor Create(const ASafeboxFileName: string; AReadOnly: boolean);
    class function AnalyzeFile(const ASafeboxFileName: string; var ABlocksCount : Integer) : Boolean;
    destructor Destroy; override;

    function BlocksCount: integer;
    function GetBlockInfo(ABlockNumber: cardinal): TOperationBlockExt;

    procedure AddBlockAccount(const ABlockAccount : TBlockAccount);
    procedure SetBlockAccount(const ABlockAccount : TBlockAccount);
    function GetBlockAccount(const ABlockNumber : Integer) : TBlockAccount;
    procedure DeleteBlockAccount(const ABlockNumber : Integer);

    function AccountsCount: integer;
    function GetAccount(AAccountNumber: cardinal): TAccount;
    procedure SetAccount(const AAccount : TAccount);

    property AccountKeys : TPCAbstractMemAccountKeys read FAccountKeys;
    property AccountsNames: TPCAbstractMemListAccountNames read FAccountsNames;

    property AbstractMem: TAbstractMem read FAbstractMem;
    procedure FlushCache;
    procedure CopyFrom(ASource : TPCAbstractMem);
    //
    property BufferBlocksHash: TPCAbstractMemBytesBuffer32Safebox read FBufferBlocksHash;
    property AggregatedHashrate : TBigNum read FAggregatedHashrate;

    function CheckConsistency(AReport : TStrings) : Boolean;
    procedure SaveToFile(const ASaveToFileName : String);
    procedure UpdateSafeboxFileName(const ANewSafeboxFileName : String);
    property AccountCache : TAccountCache read FAccountCache;
    property FileName : String read FFileName;
    procedure EraseData;
    function GetStatsReport(AClearStats : Boolean) : String;
    //
    Property UseCacheOnAbstractMemLists : Boolean read FUseCacheOnAbstractMemLists write SetUseCacheOnAbstractMemLists;
    Property MaxMemUsage : Integer read FMaxMemUsage write SetMaxMemUsage;
    Property MaxAccountsCache : Integer read GetMaxAccountsCache write SetMaxAccountsCache;
    Property MaxAccountKeysCache : Integer read GetMaxAccountKeysCache write SetMaxAccountKeysCache;
    Property SavingNewSafeboxMode : Boolean read FSavingNewSafeboxMode write SetSavingNewSafeboxMode;
    Property AccountsOrderedByUpdatedBlock : TAccountsOrderedByUpdatedBlock read FAccountsOrderedByUpdatedBlock;
    Property AccountsOrderedBySalePrice : TAccountsOrderedBySalePrice read FAccountsOrderedBySalePrice;
  end;

implementation

uses UAccounts;

const
  CT_PCAbstractMem_FileVersion = 100;
  CT_PCAbstractMem_HeaderVersion = 3;

function _AccountCache_Comparision(const Left, Right: TAccountCache.PAVLCacheMemData): Integer;
begin
  // Compare only by data.account number (not for content)
  Result := Left^.data.account - Right^.data.account;
end;

{ TPCAbstractMemBytesBuffer32Safebox }

constructor TPCAbstractMemBytesBuffer32Safebox.Create(AAbstractMem : TAbstractMem; APosition : TAbstractMemPosition; ACurrBlocksCount : Integer);
var LZone : TAMZone;
  LBufferBlockHashData,
  LCachedSafeboxHash : TBytes;
begin
  FCachedSafeboxHash := Nil;
  inherited Create(100000*32);
  FAbstractMem := AAbstractMem;
  FSaveBufferPosition:=APosition;
  if (APosition>0) then begin
    LZone.Clear;
    FAbstractMem.Read(FSaveBufferPosition,LZone.position,4);
    if FAbstractMem.GetUsedZoneInfo(LZone.position,True,LZone) then begin
      // LZone contains info
      // 32 bytes for FSavedSafeboxHash (as a cache)
      // 32*ACurrBlocksCount bytes for data
      // Minimum Size >= ((ACurrBlocksCount+1)*32)
      if (LZone.size>=((ACurrBlocksCount+1) * 32)) then begin
        // Valid
        System.SetLength(LCachedSafeboxHash,32);
        if FAbstractMem.Read(LZone.position,LCachedSafeboxHash[0],32)<>32 then Raise EPCAbstractMem.Create('Error dev 20200522-1');
        System.SetLength(LBufferBlockHashData,ACurrBlocksCount * 32);
        if FAbstractMem.Read(LZone.position + 32,LBufferBlockHashData[0],System.Length(LBufferBlockHashData))<>System.Length(LBufferBlockHashData) then Raise EPCAbstractMem.Create('Error dev 20200522-2');
        Self.Clear;
        Self.Add(LBufferBlockHashData);
        FCachedSafeboxHash := LCachedSafeboxHash; // Set cached info
      end;
    end;
  end;
  if (Self.Length<>(ACurrBlocksCount*32)) then Raise EPCAbstractMem.Create(Format('Error dev 20200403-4 %d <> %d (%d)',[Self.Length,ACurrBlocksCount*32,ACurrBlocksCount]));
end;

procedure TPCAbstractMemBytesBuffer32Safebox.Flush;
var LZone : TAMZone;
begin
  if FCachedSafeboxHash=Nil then FCachedSafeboxHash := GetSafeBoxHash;
  LZone.Clear;
  FAbstractMem.Read(FSaveBufferPosition,LZone.position,4);
  if FAbstractMem.GetUsedZoneInfo(LZone.position,True,LZone) then begin
    if ((Self.Length + 32)<=LZone.size) then begin
      // Use same:
      FAbstractMem.Write(LZone.position, FCachedSafeboxHash[0], 32);
      FAbstractMem.Write(LZone.position + 32,Self.Memory^,Self.Length);
      Exit;
    end else begin
      // Not enough space...
      FAbstractMem.Dispose(LZone);
    end;
  end;
  if (Self.Length>0) then begin
    LZone := FAbstractMem.New(((Self.Length + 32) * 3) DIV 2);
    FAbstractMem.Write(FSaveBufferPosition,LZone.position,4);
    FAbstractMem.Write(LZone.position, FCachedSafeboxHash[0], 32);
    FAbstractMem.Write(LZone.position + 32,Self.Memory^,Self.Length);
  end;
end;

{ TPCAbstractMem }

function TPCAbstractMem.CheckConsistency(AReport: TStrings) : Boolean;
begin
  AReport.Clear;
  FLockAbstractMem.Acquire;
  Try
    if Assigned(FCheckingThread) then begin
      FCheckingThread.Terminate;
      FCheckingThread.WaitFor;
      FreeAndNil(FCheckingThread);
    end;

    if Not Assigned(FCheckingThread) then begin
      FCheckingThread := TPCAbstractMemCheckThread.Create(Self);
    end;
    while Not FCheckingThread.Terminated do Sleep(1);
    AReport.Assign( FCheckingThread.Errors );

    FCheckingThread.Terminate;
    FCheckingThread.WaitFor;
    FreeAndNil(FCheckingThread);
  Finally
    FLockAbstractMem.Release;
  End;
  Result := AReport.Count=0;
end;

procedure TPCAbstractMem.CopyFrom(ASource: TPCAbstractMem);
var LIsNew : Boolean;
begin
  ASource.FlushCache;
  FAbstractMem.CopyFrom(ASource.FAbstractMem);
  FUseCacheOnAbstractMemLists := ASource.FUseCacheOnAbstractMemLists;
  FMaxMemUsage := ASource.FMaxMemUsage;
  DoInit(LIsNew);
end;

function _TComparison_TAccountNameInfo(const ALeft, ARight : TAccountNameInfo) : Integer;
begin
  Result := CompareText(ALeft.accountName,ARight.accountName);
end;

function TPCAbstractMem.DoInit(out AIsNewStructure : Boolean) : Boolean;
const
  CT_HEADER_MIN_SIZE = 100;
  CT_HEADER_STRING = 'TPCAbstractMem'; // Do not localize/modify. Fixed 14 bytes!
  {
  Header: 0..99 = 100 Bytes (CT_HEADER_MIN_SIZE)
  [ 0..13] 14 bytes: Literal "TPCAbstractMem"
  [14..15] 2 bytes: Protocol version
  [16..19] 4 bytes: LZoneBlocks.position
  [20..23] 4 bytes: LZoneAccounts.position
  [24..27] 4 bytes: LZoneAccountsNames.position
  [28..31] 4 bytes: LZoneAccountKeys.position
  [32..35] 4 bytes: FZoneAggregatedHashrate.position
  [36..39] 4 bytes: LZoneBuffersBlockHash
  [40..43] 4 bytes: LZoneAccountsOrderedByUpdatedBlock.position
  [44..47] 4 bytes: LZoneAccountsOrderedBySalePrice.position
  ...
  [96..99] 4 bytes: Header version
  }
var LZone,
  LZoneBlocks,
  LZoneAccounts,
  LZoneAccountsNames,
  LZoneAccountKeys,
  LZoneAccountsOrderedByUpdatedBlock,
  LZoneAccountsOrderedBySalePrice : TAMZone;
  LZoneBuffersBlockHash : TAbstractMemPosition;
  LHeader, LBuffer, LBigNum : TBytes;
  LIsGood : Boolean;
  w : Word;
  i : Integer;
  LHeaderVersion : UInt32;
begin
  // Free
  FreeAndNil(FBlocks);
  FreeAndNil(FAccounts);
  FreeAndNil(FAccountsNames);
  FreeAndNil(FAccountKeys);
  FreeAndNil(FBufferBlocksHash);
  FreeAndNil(FAccountsOrderedByUpdatedBlock);
  FreeAndNil(FAccountsOrderedBySalePrice);
  //
  Result := False;
  AIsNewStructure := True;
  LZone.Clear;
  LZoneBlocks.Clear;
  LZoneAccounts.Clear;
  LZoneAccountsNames.Clear;
  LZoneAccountKeys.Clear;
  FZoneAggregatedHashrate.Clear;
  LZoneBuffersBlockHash := 0;
  LZoneAccountsOrderedByUpdatedBlock.Clear;
  LZoneAccountsOrderedBySalePrice.Clear;

  if (FAbstractMem.ReadFirstData(LZone,LHeader)) then begin
    // Check if header is valid:
    if Length(LHeader)>=CT_HEADER_MIN_SIZE then begin
      LIsGood := True;
      i := 0;
      while (LIsGood) and (i<CT_HEADER_STRING.Length) do begin
        LIsGood := ord(CT_HEADER_STRING.Chars[i])=LHeader[i];
        inc(i);
      end;
      if i<>14 then LIsGood := False;
      if LIsGood then begin
        Move(LHeader[14], w, 2);
        LIsGood := (w = CT_PCAbstractMem_FileVersion);
      end;
      if LIsGood then begin
        Move(LHeader[16], LZoneBlocks.position, 4);
        Move(LHeader[20], LZoneAccounts.position, 4);
        Move(LHeader[24], LZoneAccountsNames.position, 4);
        Move(LHeader[28], LZoneAccountKeys.position, 4);
        Move(LHeader[32], FZoneAggregatedHashrate.position, 4);
        LZoneBuffersBlockHash := LZone.position + 36;
        Move(LHeader[40], LZoneAccountsOrderedByUpdatedBlock.position, 4);
        Move(LHeader[44], LZoneAccountsOrderedBySalePrice.position, 4);
        //
        Move(LHeader[96], LHeaderVersion, 4);
        if (LHeaderVersion>CT_PCAbstractMem_HeaderVersion) then begin
          TLog.NewLog(lterror,ClassName,Format('Header version readed %d is greater than expected %d',[LHeaderVersion,CT_PCAbstractMem_HeaderVersion]));
        end else begin
          AIsNewStructure := False;
          //
          if (Not FAbstractMem.ReadOnly) then begin
            if (LZoneAccountsOrderedByUpdatedBlock.position=0) then begin
              LZoneAccountsOrderedByUpdatedBlock := FAbstractMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize);
              Move(LZoneAccountsOrderedByUpdatedBlock.position,LHeader[40],4);
              FAbstractMem.Write(LZone.position,LHeader[0],Length(LHeader));
            end;
            if (LZoneAccountsOrderedBySalePrice.position=0) then begin
              LZoneAccountsOrderedBySalePrice := FAbstractMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize);
              Move(LZoneAccountsOrderedBySalePrice.position,LHeader[44],4);
              FAbstractMem.Write(LZone.position,LHeader[0],Length(LHeader));
            end;
          end;
        end;
      end;
    end;
  end;
  if (Not FAbstractMem.ReadOnly) and (AIsNewStructure) then begin
    // Initialize struct
    FAbstractMem.ClearContent(FAbstractMem.Is64Bytes);
    LZone := FAbstractMem.New( CT_HEADER_MIN_SIZE );  // Header zone
    SetLength(LHeader,100);
    FillChar(LHeader[0],Length(LHeader),0);
    //
    LBuffer.FromString(CT_HEADER_STRING);
    Move(LBuffer[0],LHeader[0],14);
    w := CT_PCAbstractMem_FileVersion;
    Move(w,LHeader[14],2);
    LZoneBlocks := FAbstractMem.New( CT_AbstractMemTList_HeaderSize );
    LZoneAccounts := FAbstractMem.New( CT_AbstractMemTList_HeaderSize );
    LZoneAccountsNames := FAbstractMem.New( CT_AbstractMemTList_HeaderSize );
    LZoneAccountKeys := FAbstractMem.New( 100 );
    FZoneAggregatedHashrate := FAbstractMem.New(100); // Note: Enough big to store a BigNum
    LZoneBuffersBlockHash := LZone.position+36;
    LZoneAccountsOrderedByUpdatedBlock := FAbstractMem.New(
      TAbstractMemBTree.MinAbstractMemInitialPositionSize);
    LZoneAccountsOrderedBySalePrice := FAbstractMem.New(
      TAbstractMemBTree.MinAbstractMemInitialPositionSize);

    Move(LZoneBlocks.position,       LHeader[16],4);
    Move(LZoneAccounts.position,     LHeader[20],4);
    Move(LZoneAccountsNames.position,LHeader[24],4);
    Move(LZoneAccountKeys.position,  LHeader[28],4);
    Move(FZoneAggregatedHashrate.position,LHeader[32],4);
    LHeaderVersion := CT_PCAbstractMem_HeaderVersion;
    Move(LZoneAccountsOrderedByUpdatedBlock, LHeader[40],4);
    Move(LZoneAccountsOrderedBySalePrice, LHeader[44],4);
    Move(LHeaderVersion,             LHeader[96],4);

    FAbstractMem.Write(LZone.position,LHeader[0],Length(LHeader));

  end;
  // Free
  FreeAndNil(FBlocks);
  //
  FBlocks := TPCAbstractMemListBlocks.Create( FAbstractMem, LZoneBlocks, 20000, Self.UseCacheOnAbstractMemLists);
  FBlocks.FPCAbstractMem := Self;

  FAccounts := TPCAbstractMemListAccounts.Create( FAbstractMem, LZoneAccounts, 100000, Self.UseCacheOnAbstractMemLists);

  FAccountsNames := TPCAbstractMemListAccountNames.Create( FAbstractMem, LZoneAccountsNames, False, 31, _TComparison_TAccountNameInfo);
  FAccountsNames.FPCAbstractMem := Self;

  FAccountKeys := TPCAbstractMemAccountKeys.Create( FAbstractMem, LZoneAccountKeys.position, Self.UseCacheOnAbstractMemLists);
  FAccounts.AccountKeys := FAccountKeys;

  // Read AggregatedHashrate
  SetLength(LBuffer,100);
  FAbstractMem.Read(FZoneAggregatedHashrate.position,LBuffer[0],Length(LBuffer));
  if LBigNum.FromSerialized(LBuffer) then begin
    FAggregatedHashrate.RawValue := LBigNum;
  end;
  FBufferBlocksHash := TPCAbstractMemBytesBuffer32Safebox.Create(FAbstractMem,LZoneBuffersBlockHash,FBlocks.Count);

  FAccountsOrderedByUpdatedBlock := TAccountsOrderedByUpdatedBlock.Create({$IFDEF USE_ABSTRACTMEM}FAbstractMem,LZoneAccountsOrderedByUpdatedBlock,{$ENDIF}DoGetAccount);
  FAccounts.AccountsOrderedByUpdatedBlock := FAccountsOrderedByUpdatedBlock;

  FAccountsOrderedBySalePrice := TAccountsOrderedBySalePrice.Create({$IFDEF USE_ABSTRACTMEM}FAbstractMem,LZoneAccountsOrderedBySalePrice,{$ENDIF}DoGetAccount);
  FAccounts.AccountsOrderedBySalePrice := FAccountsOrderedBySalePrice;

  FAccountCache.Clear;

  if (Not AIsNewStructure) And (Not FAbstractMem.ReadOnly) And (LHeaderVersion<CT_PCAbstractMem_HeaderVersion) then begin
    UpgradeAbstractMemVersion( LHeaderVersion );
    // Set for future
    LHeaderVersion := CT_PCAbstractMem_HeaderVersion;
    Move(LHeaderVersion,             LHeader[96],4);
    FAbstractMem.Write(LZone.position,LHeader[0],Length(LHeader));
  end;

end;

procedure TPCAbstractMem.EraseData;
var
  LIsNewStructure : Boolean;
begin
  FlushCache;
  FAbstractMem.ClearContent(FAbstractMem.Is64Bytes);
  DoInit(LIsNewStructure);
end;

constructor TPCAbstractMem.Create(const ASafeboxFileName: string; AReadOnly: boolean);
var
  LIsNewStructure : Boolean;
begin
  FStats.Clear;

  FUseCacheOnAbstractMemLists := False;
  FMaxMemUsage := 100 * 1024 * 1024;

  FBlocks := Nil;
  FAccounts:= Nil;
  FAccountsNames:= Nil;
  FAccountKeys:= Nil;
  FBufferBlocksHash:= Nil;

  FCheckingThread := Nil;
  FLockAbstractMem := TPCCriticalSection.Create(Self.ClassName);
  FAccountCache := TAccountCache.Create(50000,_AccountCache_Comparision);
  FSavingNewSafeboxMode := False;

  FAggregatedHashrate := TBigNum.Create(0);
  FFileName := ASafeboxFileName;
  if (FFileName<>'') then begin
    FAbstractMem := TFileMem.Create( ASafeboxFileName , AReadOnly);
  end else begin
    FAbstractMem := TMem.Create(0,AReadOnly);
  end;
  if FAbstractMem is TFileMem then begin
    TFileMem(FAbstractMem).MaxCacheSize := FMaxMemUsage;
    TFileMem(FAbstractMem).MaxCacheDataBlocks := 200000;
  end;

  DoInit(LIsNewStructure);
  //
  if (Not AReadOnly) and (((BlocksCount>0) And (ASafeboxFileName<>'')) Or (Not LIsNewStructure)) then begin
    TLog.NewLog(ltdebug,ClassName,Format('Opened PascalCoin AbstractMem File with %d blocks %d accounts %s aggregated hashrate and buffer %d size (%d blocks) at file: %s',
      [BlocksCount,AccountsCount,FAggregatedHashrate.ToDecimal,FBufferBlocksHash.Length,FBufferBlocksHash.Length DIV 32,ASafeboxFileName]));
  end;
end;

destructor TPCAbstractMem.Destroy;
var LFile : TFileStream;
begin
  FLockAbstractMem.Acquire;
  try
    if Assigned(FCheckingThread) then begin
      FCheckingThread.Terminate;
    end;
  finally
    FLockAbstractMem.Release;
  end;

  FlushCache;
  FreeAndNil(FAccountCache);
  FreeAndNil(FBlocks);
  FreeAndNil(FAccounts);
  FreeAndNil(FAccountsNames);
  FreeAndNil(FAccountKeys);
  FreeAndNil(FBufferBlocksHash);
  FreeAndNil(FAggregatedHashrate);
  FreeAndNil(FAccountsOrderedByUpdatedBlock);
  FreeAndNil(FAccountsOrderedBySalePrice);
  if (FFileName<>'') And (FAbstractMem is TMem) And (Not FAbstractMem.ReadOnly) then begin
    LFile := TFileStream.Create(FFileName,fmCreate);
    try
      LFile.Size := 0;
      LFile.Position := 0;
      FAbstractMem.SaveToStream(LFile);
    finally
      LFile.Free;
    end;
  end;
  FreeAndNil(FAbstractMem);


  FreeAndNil(FLockAbstractMem);

  inherited Destroy;
end;

procedure TPCAbstractMem.DoCheck;
begin
  if IsChecking then Exit;
  FLockAbstractMem.Acquire;
  Try
    if Not Assigned(FCheckingThread) then begin
      FCheckingThread := TPCAbstractMemCheckThread.Create(Self);
    end;
  Finally
    FLockAbstractMem.Release;
  End;
end;

function TPCAbstractMem.DoGetAccount(AAccountNumber: Integer; var AAccount: TAccount): Boolean;
begin
  AAccount := GetAccount(AAccountNumber);
  Result := True;
end;

procedure TPCAbstractMem.FlushCache;
var LBigNum : TBytes;
  Ltc : TTickCount;
begin
  if FAbstractMem.ReadOnly then Exit;
  Ltc := TPlatform.GetTickCount;
  FBlocks.FlushCache;
  FAccounts.FlushCache;
  FAccountKeys.FlushCache;
  FBufferBlocksHash.Flush;
  LBigNum := FAggregatedHashrate.RawValue.ToSerialized;
  FAbstractMem.Write( FZoneAggregatedHashrate.position, LBigNum[0], Length(LBigNum) );
  if FAbstractMem is TFileMem then begin
    TFileMem(FAbstractMem).FlushCache;
  end;
  Inc(FStats.FlushesCount);
  Inc(Fstats.FlushesMillis, TPlatform.GetElapsedMilliseconds(Ltc) );
end;

Procedure DoCopyFile(const ASource, ADest : String);
var LSourceFS, LDestFS : TFileStream;
Begin
  if Not FileExists(ASource) then Raise Exception.Create('Source file not found: '+ASource);
  LSourceFS := TFileStream.Create(ASource,fmOpenRead+fmShareDenyNone);
  try
    LSourceFS.Position:=0;
    LDestFS := TFileStream.Create(ADest,fmCreate+fmShareDenyWrite);
    try
      LDestFS.Size:=0;
      LDestFS.CopyFrom(LSourceFS,LSourceFS.Size);
    finally
      LDestFS.Free;
    end;
  finally
    LSourceFS.Free;
  end;
end;


procedure TPCAbstractMem.SaveToFile(const ASaveToFileName: String);
var LFile : TFileStream;
begin
  FlushCache;
  //
  ForceDirectories(ExtractFileDir(ASaveToFileName));
  if FileExists(ASaveToFileName) then DeleteFile(ASaveToFileName);
  //
  if (FAbstractMem is TFileMem) then begin
    DoCopyFile(TFileMem(FAbstractMem).FileName,ASaveToFileName);
  end else begin
    LFile := TFileStream.Create(ASaveToFileName,fmCreate);
    try
      LFile.Size := 0;
      LFile.Position := 0;
      FAbstractMem.SaveToStream(LFile);
    finally
      LFile.Free;
    end;
  end;
end;

procedure TPCAbstractMem.SetAccount(const AAccount: TAccount);
begin
  if (AAccount.account<0) or (AAccount.account>FAccounts.Count) then begin
    raise EPCAbstractMem.Create(Format('Account %d not in range %d..%d',[AAccount.account,0,FAccounts.Count]));
  end;
  if (AAccount.account = FAccounts.Count) then begin
    FAccounts.Add(AAccount);
  end else begin
    FAccounts.Item[ AAccount.account ] := AAccount;
  end;
  // Update cache
  FAccountCache.Remove(AAccount);
  FAccountCache.Add(AAccount);
end;

procedure TPCAbstractMem.AddBlockAccount(const ABlockAccount: TBlockAccount);
var i : Integer;
  LOpBlockExt :  TOperationBlockExt;
begin
  LOpBlockExt.operationBlock := ABlockAccount.blockchainInfo;
  LOpBlockExt.accumulatedWork := ABlockAccount.accumulatedWork;
  AddBlockInfo(LOpBlockExt);
  for i := Low(ABlockAccount.accounts) to High(ABlockAccount.accounts) do begin
    SetAccount( ABlockAccount.accounts[i] );
  end;
  FBufferBlocksHash.Replace(ABlockAccount.blockchainInfo.block * 32, ABlockAccount.block_hash);
end;

procedure TPCAbstractMem.SetBlockAccount(const ABlockAccount: TBlockAccount);
var i : Integer;
  LOpBlockExt, LSavedOpBlockExt :  TOperationBlockExt;
  LSavedAccount : TAccount;
begin
  if ABlockAccount.blockchainInfo.block=BlocksCount then AddBlockAccount(ABlockAccount)
  else if ABlockAccount.blockchainInfo.block<BlocksCount then begin
    LOpBlockExt.operationBlock := ABlockAccount.blockchainInfo;
    LOpBlockExt.accumulatedWork := ABlockAccount.accumulatedWork;
    LSavedOpBlockExt := GetBlockInfo( ABlockAccount.blockchainInfo.block );
    if (Not TAccountComp.EqualOperationBlocks( LOpBlockExt.operationBlock, LSavedOpBlockExt.operationBlock ))
       or (LOpBlockExt.accumulatedWork <> LSavedOpBlockExt.accumulatedWork) then begin
      SetBlockInfo(LOpBlockExt);
    end;
    for i := Low(ABlockAccount.accounts) to High(ABlockAccount.accounts) do begin
      if TAccountComp.AccountBlock(ABlockAccount.accounts[i].account)<>ABlockAccount.blockchainInfo.block then
        raise EPCAbstractMem.Create(Format('Account %d is not valid for block %d',[ABlockAccount.accounts[i].account,ABlockAccount.blockchainInfo.block]));
      LSavedAccount := GetAccount(ABlockAccount.accounts[i].account);
      if Not TAccountComp.EqualAccounts(LSavedAccount, ABlockAccount.accounts[i]) then begin
        SetAccount( ABlockAccount.accounts[i] );
      end;
    end;
    FBufferBlocksHash.Replace(ABlockAccount.blockchainInfo.block * 32, ABlockAccount.block_hash);
  end else raise EPCAbstractMem.Create(Format('Cannot add Block %d on %d count list',[ABlockAccount.blockchainInfo.block,BlocksCount]));
end;

procedure TPCAbstractMem.AddBlockInfo(const ABlock : TOperationBlockExt);
begin
  if (ABlock.operationBlock.block<>FBlocks.Count) then raise EPCAbstractMem.Create(Format('Cannot add blockInfo %d at pos %d',[ABlock.operationBlock.block,FBlocks.Count]));
  SetBlockInfo(ABlock);
end;

class function TPCAbstractMem.AnalyzeFile(const ASafeboxFileName: string; var ABlocksCount: Integer): Boolean;
var LPCAbstractMem : TPCAbstractMem;
begin
  ABlocksCount := 0;
  Result := False;
  if Not FileExists(ASafeboxFileName) then Exit(False);
  LPCAbstractMem := TPCAbstractMem.Create(ASafeboxFileName,True);
  try
    ABlocksCount := LPCAbstractMem.BlocksCount;
    Result := (ABlocksCount>0) And (LPCAbstractMem.AccountsCount = ABlocksCount * CT_AccountsPerBlock);
  finally
    LPCAbstractMem.Free;
  end;
end;

procedure TPCAbstractMem.SetBlockInfo(const ABlock: TOperationBlockExt);
var LCount : Integer;
begin
  LCount := FBlocks.Count;
  if ABlock.operationBlock.block<LCount then begin
    FBlocks.Item[ABlock.operationBlock.block] := ABlock;
  end else if ABlock.operationBlock.block=LCount then begin
    FBlocks.Add( ABlock );
  end else raise EPCAbstractMem.Create(Format('Cannot set block info %d (current %d blocks)',[ABlock.operationBlock.block,LCount]));
end;

procedure TPCAbstractMem.SetMaxAccountKeysCache(const Value: Integer);
begin
  FAccountKeys.AccountKeyByPositionCache.MaxRegisters := Value;
end;

procedure TPCAbstractMem.SetMaxAccountsCache(const Value: Integer);
begin
  FAccountCache.MaxRegisters := Value;
end;

procedure TPCAbstractMem.SetMaxMemUsage(const Value: Integer);
begin
  FMaxMemUsage := Value;
  if FAbstractMem is TFileMem then begin
    TFileMem(FAbstractMem).MaxCacheSize := FMaxMemUsage;
    TFileMem(FAbstractMem).MaxCacheDataBlocks := 200000;
  end;
end;

procedure TPCAbstractMem.SetSavingNewSafeboxMode(const Value: Boolean);
var Lcm : TCacheMem;
begin
  FSavingNewSafeboxMode := Value;
  // Will set in optimized state (cache and others) for maximum performance and minimum impact
  TLog.NewLog(ltinfo,ClassName,Format('Seting AbstractMem is Saving mode:%s',[Value.ToString]));
  if FAbstractMem is TFileMem then begin
    Lcm := TFileMem(FAbstractMem).LockCache;
    try
      if Value then begin
        FSavingOldGridCache := Lcm.GridCache;
        FSavingOldDefaultCacheDataBlocksSize := Lcm.DefaultCacheDataBlocksSize;
        Lcm.GridCache := False;
        Lcm.DefaultCacheDataBlocksSize := -1;
      end else begin
        Lcm.GridCache := FSavingOldGridCache;
        Lcm.DefaultCacheDataBlocksSize := FSavingOldDefaultCacheDataBlocksSize;
      end;
    finally
      TFileMem(FAbstractMem).UnlockCache;
    end;
  end;
end;

procedure TPCAbstractMem.SetUseCacheOnAbstractMemLists(const Value: Boolean);
var Lins : Boolean;
begin
  if Value=FUseCacheOnAbstractMemLists then Exit;
  FUseCacheOnAbstractMemLists := Value;
  DoInit(Lins);
end;

procedure TPCAbstractMem.UpdateSafeboxFileName(const ANewSafeboxFileName: String);
var LReadOnly, Ltmp : Boolean;
begin
  if SameFileName(FFileName,ANewSafeboxFileName) then Exit;

  if ANewSafeboxFileName<>'' then
    SaveToFile(ANewSafeboxFileName);

  FFileName := ANewSafeboxFileName;
  LReadOnly := FAbstractMem.ReadOnly;
  FreeAndNil(FAbstractMem);

  if (FFileName<>'') then begin
    FAbstractMem := TFileMem.Create( FFileName , LReadOnly)
  end else begin
    FAbstractMem := TMem.Create(0,LReadOnly);
  end;
  if FAbstractMem is TFileMem then begin
    TFileMem(FAbstractMem).MaxCacheSize := FMaxMemUsage;
    TFileMem(FAbstractMem).MaxCacheDataBlocks := 200000;
  end;
  DoInit(Ltmp);
end;

procedure TPCAbstractMem.UpgradeAbstractMemVersion(const ACurrentHeaderVersion: Integer);
var LFirstTC, LTC : TTickCount;
  i : integer;
  LAccount : TAccount;
  LaccInfoNul : TAccountInfo;
begin
  LFirstTC := TPlatform.GetTickCount;
  LTC := LFirstTC;
  if (ACurrentHeaderVersion=2) then begin
    // Set accounts price
    LaccInfoNul.Clear;
    for i := 0 to AccountsCount-1 do begin
      LAccount := GetAccount(i);
      AccountsOrderedBySalePrice.UpdateAccountBySalePrice(LAccount.account,LaccInfoNul,LAccount.accountInfo);
    end;
  end;
  TLog.NewLog(ltinfo,ClassName,Format('Finalized upgrade AbstractMem file from %d to %d in %.2f seconds',[ACurrentHeaderVersion,CT_PCAbstractMem_HeaderVersion, TPlatform.GetElapsedMilliseconds(LFirstTC)/1000]));
end;

function TPCAbstractMem.BlocksCount: integer;
begin
  Result := FBlocks.Count;
end;

function TPCAbstractMem.GetBlockAccount(const ABlockNumber: Integer): TBlockAccount;
var i : Integer;
 LBlock : TOperationBlockExt;
begin
  Result := CT_BlockAccount_NUL;
  LBlock := GetBlockInfo(ABlockNumber);
  Result.blockchainInfo := LBlock.operationBlock;
  Result.accumulatedWork := LBlock.accumulatedWork;

  Result.block_hash := FBufferBlocksHash.Capture( ABlockNumber * 32, 32);

  for i := Low(Result.accounts) to High(Result.accounts) do begin
    Result.accounts[i] := GetAccount( i + (ABlockNumber * CT_AccountsPerBlock) );
  end;
end;

procedure TPCAbstractMem.DeleteBlockAccount(const ABlockNumber: Integer);
var i : Integer;
  LBlAcc :  TBlockAccount;
  LExtract : TBytes;
begin
  LBlAcc := GetBlockAccount(ABlockNumber);
  FBlocks.Delete(ABlockNumber);
  for i := High(LBlAcc.accounts) downto Low(LBlAcc.accounts) do begin
    FAccounts.Delete(LBlAcc.accounts[i].account );
  end;
  LExtract := FBufferBlocksHash.Capture((ABlockNumber+1)*32,32);
  FBufferBlocksHash.Replace(ABlockNumber*32,LExtract);
  FBufferBlocksHash.SetLength(BufferBlocksHash.Length - 32);
end;

function TPCAbstractMem.GetBlockInfo(ABlockNumber: cardinal): TOperationBlockExt;
begin
  Result := FBlocks.GetItem( ABlockNumber );
end;

function TPCAbstractMem.GetMaxAccountKeysCache: Integer;
begin
  Result := FAccountKeys.AccountKeyByPositionCache.MaxRegisters;
end;

function TPCAbstractMem.GetMaxAccountsCache: Integer;
begin
  Result := FAccountCache.MaxRegisters;
end;

function TPCAbstractMem.GetStatsReport(AClearStats: Boolean): String;
begin
  Result := AbstractMem.GetStatsReport(AClearStats) + #10 + FStats.ToString;
  if AClearStats then FStats.Clear;
end;

function TPCAbstractMem.IsChecking: Boolean;
begin
  Result := Assigned(TPCThread.GetThreadByClass(TPCAbstractMemCheckThread,Nil));
  FLockAbstractMem.Acquire;
  Try
    Result := Assigned(FCheckingThread);
  Finally
    FLockAbstractMem.Release;
  End;
end;

function TPCAbstractMem.AccountsCount: integer;
begin
  Result := FAccounts.Count;
end;

function TPCAbstractMem.GetAccount(AAccountNumber: cardinal): TAccount;
begin
  Result.Clear;
  Result.account := AAccountNumber;
  if Not FAccountCache.Find(Result,Result) then begin
    Result := FAccounts.Item[ AAccountNumber ];
    // Save for future usage:
    FAccountCache.Add(Result);
  end;
end;


{ TPCAbstractMemListAccountNames }

function TPCAbstractMemListAccountNames.LoadData(const APosition: TAbstractMemPosition): TAccountNameInfo;
var LZone : TAMZone;
  LBytes : TBytes;
begin
  if Not FPCAbstractMem.AbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractMemTList.Create(Format('%s.LoadData Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  SetLength(LBytes,LZone.size);
  if FPCAbstractMem.AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
    raise EAbstractMemTList.Create(Format('%s.LoadData Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  LoadFrom(LBytes,Result);
end;

procedure TPCAbstractMemListAccountNames.LoadFrom(const ABytes: TBytes; var AItem: TAccountNameInfo);
var LTmp : TBytes;
begin
  if Not LTmp.FromSerialized(ABytes) then raise EPCAbstractMem.Create('INCONSISTENT 20200318-5');
  AItem.accountName := LTmp.ToString;
  Move(ABytes[LTmp.GetSerializedLength],AItem.accountNumber,4);
end;

function TPCAbstractMemListAccountNames.NodeDataToString(const AData: TAbstractMemPosition): String;
var Lani : TAccountNameInfo;
begin
  Lani := LoadData(AData);
  Result:= Format('AccountNameInfo: Account:%d Name(%d):%d',[Lani.accountNumber, Length(Lani.accountName), Lani.accountName]);
end;

function TPCAbstractMemListAccountNames.DeleteAccountName(const AName: String) : Boolean;
var  Lani : TAccountNameInfo;
begin
  Lani.accountName := AName;
  Lani.accountNumber := 0;
  Result := DeleteData(Lani);
end;

function TPCAbstractMemListAccountNames.SaveData(const AData: TAccountNameInfo): TAMZone;
var LBytes : TBytes;
begin
  SetLength(LBytes,0);
  SaveTo(AData,LBytes);
  Result := FPCAbstractMem.AbstractMem.New(Length(LBytes));
  FPCAbstractMem.AbstractMem.Write(Result.position,LBytes[0],Length(LBytes));
end;

procedure TPCAbstractMemListAccountNames.SaveTo(const AItem: TAccountNameInfo; var ABytes: TBytes);
var LStream : TStream;
  LTmp : TBytes;
begin
  LStream := TMemoryStream.Create;
  Try
    LTmp.FromString(AItem.accountName);
    LTmp.ToSerialized(LStream);
    LStream.Write(AItem.accountNumber,4);
    //
    ABytes.FromStream(LStream);
  Finally
    LStream.Free;
  End;
end;

procedure TPCAbstractMemListAccountNames.AddNameAndNumber(const AName: String; AAccountNumber: Cardinal);
var Lani : TAccountNameInfo;
  Lposition : TAbstractMemPosition;
begin
  Lani.accountName := AName;
  Lani.accountNumber := AAccountNumber;
  if Not AddData(Lani) then begin
    if Not FindData(Lani,Lposition) then
      raise EPCAbstractMem.Create(Format('Fatal error Cannot add account(%d) name %s',[AAccountNumber,AName]))
    else raise EPCAbstractMem.Create(Format('Cannot add account(%d) name %s because used by %d with %s',[AAccountNumber,AName,
      Lani.accountNumber,Lani.accountName]));
  end;
end;

function TPCAbstractMemListAccountNames.FindByName(const AName: String): Boolean;
var Lpos : TAbstractMemPosition;
begin
  Result := FindByName(AName,Lpos);
end;

function TPCAbstractMemListAccountNames.FindByName(const AName: String; out ANameInfo: TAccountNameInfo): Boolean;
var Lpos : TAbstractMemPosition;
begin
  if FindByName(AName,Lpos) then begin
    ANameInfo := LoadData(Lpos);
    Result := True;
  end else begin
    if Lpos<>0 then begin
      ANameInfo := LoadData(Lpos);
    end;
    Result := False;
  end;
end;

function TPCAbstractMemListAccountNames.FindByName(const AName: String; out AAbstractMemPosition: TAbstractMemPosition): Boolean;
var Lani : TAccountNameInfo;
begin
  Lani.accountName := AName;
  Lani.accountNumber := 0;
  Result := FindData(Lani,AAbstractMemPosition);
end;

{ TPCAbstractMemListBlocks }

procedure TPCAbstractMemListBlocks.LoadFrom(const ABytes: TBytes; var AItem: TOperationBlockExt);
var
  LPointer: TAbstractMemPosition;
  LIndex: integer;
begin
  AItem.accumulatedWork := 0;
  Move(ABytes[0], AItem.operationBlock.block, 4);
  Move(ABytes[4], LPointer, 4);
  // Load account_key
  AItem.operationBlock.account_key := FPCAbstractMem.FAccountKeys.GetKeyAtPosition(LPointer);

  Move(ABytes[8], AItem.operationBlock.reward, 8);
  Move(ABytes[16], AItem.operationBlock.fee, 8);
  Move(ABytes[24], AItem.operationBlock.protocol_version, 2);
  Move(ABytes[26], AItem.operationBlock.protocol_available, 2);
  Move(ABytes[28], AItem.operationBlock.timestamp, 4);
  Move(ABytes[32], AItem.operationBlock.compact_target, 4);
  Move(ABytes[36], AItem.operationBlock.nonce, 4);
  Move(ABytes[40], AItem.accumulatedWork, 8);

  LIndex := 48;
  if not AItem.operationBlock.block_payload.LoadFromTBytes(ABytes, LIndex) then
    raise EPCAbstractMem.Create(Format('LoadFrom Invalid 20200317-2 %d', [LIndex]));
  if not AItem.operationBlock.initial_safe_box_hash.LoadFromTBytes(ABytes, LIndex) then
    raise EPCAbstractMem.Create(Format('LoadFrom Invalid 20200317-3 %d', [LIndex]));
  if not AItem.operationBlock.operations_hash.LoadFromTBytes(ABytes, LIndex) then
    raise EPCAbstractMem.Create(Format('LoadFrom Invalid 20200317-4 %d', [LIndex]));
  if not AItem.operationBlock.proof_of_work.LoadFromTBytes(ABytes, LIndex) then
    raise EPCAbstractMem.Create(Format('LoadFrom Invalid 20200317-5 %d', [LIndex]));
  if not AItem.operationBlock.previous_proof_of_work.LoadFromTBytes(ABytes, LIndex) then
    raise EPCAbstractMem.Create(Format('LoadFrom Invalid 20200317-6 %d', [LIndex]));
end;

procedure TPCAbstractMemListBlocks.SaveTo(const AItem: TOperationBlockExt; AIsAddingItem : Boolean; var ABytes: TBytes);
var
  LPointer: TAbstractMemPosition;
  LStream : TStream;
begin
  LStream := TMemoryStream.Create;
  Try
    LStream.Write(AItem.operationBlock.block, 4);
    // Pointer
    LPointer := FPCAbstractMem.FAccountKeys.GetPositionOfKey(AItem.operationBlock.account_key,True);
    LStream.Write(LPointer, 4);
    LStream.Write(AItem.operationBlock.reward, 8);
    LStream.Write(AItem.operationBlock.fee, 8);
    LStream.Write(AItem.operationBlock.protocol_version, 2);
    LStream.Write(AItem.operationBlock.protocol_available, 2);
    LStream.Write(AItem.operationBlock.timestamp, 4);
    LStream.Write(AItem.operationBlock.compact_target, 4);
    LStream.Write(AItem.operationBlock.nonce, 4);
    LStream.Write(AItem.accumulatedWork, 8);
    AItem.operationBlock.block_payload.ToSerialized(LStream);
    AItem.operationBlock.initial_safe_box_hash.ToSerialized(LStream);
    AItem.operationBlock.operations_hash.ToSerialized(LStream);
    AItem.operationBlock.proof_of_work.ToSerialized(LStream);
    AItem.operationBlock.previous_proof_of_work.ToSerialized(LStream);
    ABytes.FromStream(LStream);
  Finally
    LStream.Free;
  End;
end;

function TPCAbstractMemListBlocks.ToString(const AItem: TOperationBlockExt): string;
begin
  Result := Format('Block %d AccWork:%s', [AItem.operationBlock.block,AItem.accumulatedWork]);
end;

{ TPCAbstractMemCheckThread }

procedure TPCAbstractMemCheckThread.BCExecute;
  procedure _error(const AError : String);
  begin
    FErrors.Add( AError );
    inc(FErrorsCount);
    TLog.NewLog(ltError,ClassName,'CheckConsistency: '+AError);
  end;
var iBlock, i : Integer;
  LAccount : TAccount;
  LBlockAccount : TBlockAccount;
  LHighestOperationBlock : TOperationBlockExt;
  LOrdered : TOrderedList<Integer>;
  LOrderedNames : TOrderedList<String>;
  LTC, LTCInitial : TTickCount;
  LAggregatedHashrate, LBlockHashRate : TBigNum;
  LBuff1,LBuff2 : TRawBytes;
  Laninfo : TAccountNameInfo;
begin
  LBlockAccount := CT_BlockAccount_NUL;
  iBlock :=0;
  LOrdered := TOrderedList<Integer>.Create(False,TComparison_Integer);
  LOrderedNames := TOrderedList<String>.Create(False,TComparison_String);
  LAggregatedHashrate := TBigNum.Create;
  Try
    LTC := TPlatform.GetTickCount;
    LTCInitial := LTC;
    LHighestOperationBlock := FPCAbstractMem.GetBlockInfo(FPCAbstractMem.BlocksCount-1);
    while (iBlock < FPCAbstractMem.BlocksCount) and (Not Terminated) do begin
      if FMustRestart then begin
        TLog.NewLog(ltdebug,ClassName,Format('Restarting check thread after %d/%d blocks',[iBlock+1,FPCAbstractMem.BlocksCount]) );
        FMustRestart := False;
        FErrorsCount := 0;
        FErrors.Clear;
        iBlock := 0;
        LOrdered.Clear;
        LOrderedNames.Clear;
        LAggregatedHashrate.Value := 0;
        LHighestOperationBlock := FPCAbstractMem.GetBlockInfo(FPCAbstractMem.BlocksCount-1);
      end;

      LBlockAccount := FPCAbstractMem.GetBlockAccount(iBlock);
      for i:=Low(LBlockAccount.accounts) to high(LBlockAccount.accounts) do begin
        //
        LAccount := LBlockAccount.accounts[i];
        if Length(LAccount.name)>0 then begin
          if LOrderedNames.Add(LAccount.name.ToString)<0 then begin
            _error(Format('Account %d name %s allready added',[LAccount.account,LAccount.name.ToString]));
          end;
          if Not FPCAbstractMem.AccountsNames.FindByName(LAccount.name.ToString,Laninfo) then begin
            // ERROR
            _error(Format('Account %d name %s not found at list',[LAccount.account,LAccount.name.ToString]));
          end else begin
            if Laninfo.accountNumber<>LAccount.account then begin
              _error(Format('Account %d name %s found at list but links to %d',[LAccount.account,LAccount.name.ToString,Laninfo.accountNumber]));
            end;
            if (LOrdered.Add(LAccount.account)<0) then begin
              _error(Format('Account %d (with name %s) allready added',[LAccount.account,LAccount.name.ToString]));
            end;
          end;
        end;
        if LAccount.GetLastUpdatedBlock>=FPCAbstractMem.BlocksCount then begin
          _error(Format('Account Updated %d > %d - %s',[LAccount.GetLastUpdatedBlock,FPCAbstractMem.BlocksCount,TAccountComp.AccountToTxt(LAccount)]));
        end;

      end;
      LBuff1 := TPCSafeBox.CalcBlockHash(LBlockAccount,LHighestOperationBlock.operationBlock.protocol_version);
      If Not (LBuff1.IsEqualTo(LBlockAccount.block_hash)) then begin
        _error(Format('Blockaccount hash for %d are not equals: calculated %s <> saved %s',[LBlockAccount.blockchainInfo.block,LBuff1.ToHexaString,LBlockAccount.block_hash.ToHexaString]));
      end;
      LBuff2 := FPCAbstractMem.FBufferBlocksHash.Capture((iBlock*32),32);
      if Not LBuff1.IsEqualTo(LBuff2) then begin
        _error(Format('Blockaccount hash for %d are not equals: %s <> %s',[LBlockAccount.blockchainInfo.block,LBuff1.ToHexaString,LBuff2.ToHexaString]));
      end;

      LBlockHashRate := TBigNum.TargetToHashRate( LBlockAccount.blockchainInfo.compact_target );
      Try
        LAggregatedHashrate.Add( LBlockHashRate );
      finally
        LBlockHashRate.Free;
      end;

      if (TPlatform.GetElapsedMilliseconds(LTC)>2000) then begin
        LTC := TPlatform.GetTickCount;
        TLog.NewLog(ltDebug,ClassName,Format('Checking consistency at %d/%d',[iBlock+1,FPCAbstractMem.BlocksCount]));
      end;
      inc(iBlock);
    end;
    //
    FPCAbstractMem.FBufferBlocksHash.SafeBoxHashCalcType := sbh_Single_Sha256;
    FPCAbstractMem.FBufferBlocksHash.SafeBoxHashCalcType := sbh_Merkle_Root_Hash;
    LBuff1 := FPCAbstractMem.FBufferBlocksHash.GetSafeBoxHash;
    FErrors.Add(Format('Last Block %d - SBH %s - Next SBH: %s',[LBlockAccount.blockchainInfo.block,LBlockAccount.blockchainInfo.initial_safe_box_hash.ToHexaString,LBuff1.ToHexaString]));
    //
    i := 0;
    if FPCAbstractMem.AccountsNames.FindDataLowest(Laninfo) then begin
      repeat
        inc(i);
        if LOrdered.IndexOf(Laninfo.accountNumber ) < 0 then begin
          _error(Format('Account name %s at index %d/%d not found in search',[Laninfo.accountName, i,FPCAbstractMem.AccountsNames.Count]));
        end;
      until Not FPCAbstractMem.AccountsNames.FindDataSuccessor(Laninfo,Laninfo);
    end;

    if (LOrdered.Count)<>FPCAbstractMem.AccountsNames.Count then begin
      _error(Format('Found %d accounts with names but %d on list',[LOrdered.Count,FPCAbstractMem.AccountsNames.Count]));
    end;
    if (LOrderedNames.Count)<>FPCAbstractMem.AccountsNames.Count then begin
      _error(Format('Found %d accounts with names but %d on ordered names list',[FPCAbstractMem.AccountsNames.Count,LOrderedNames.Count]));
    end;
    //
    if FPCAbstractMem.AggregatedHashrate.CompareTo(LAggregatedHashrate)<>0 then begin
      _error(Format('Different AggregatedHashRate Found %s vs previous %s',[LAggregatedHashrate.ToDecimal,FPCAbstractMem.AggregatedHashrate.ToDecimal]));
      FPCAbstractMem.AggregatedHashrate.RawValue := LAggregatedHashrate.RawValue;
    end;

  finally
    LOrdered.Free;
    LOrderedNames.Free;
    LAggregatedHashrate.Free;
  end;

  TLog.NewLog(ltDebug,ClassName,Format('Finalized checking consistency at %d %d blocks in %.2f sec',[iBlock+1,FPCAbstractMem.BlocksCount,TPlatform.GetElapsedMilliseconds(LTCInitial)/1000]));
end;

constructor TPCAbstractMemCheckThread.Create(APCAbstractMem: TPCAbstractMem);
begin
  FPCAbstractMem := APCAbstractMem;

  FPCAbstractMem.FLockAbstractMem.Acquire;
  try
    FPCAbstractMem.FCheckingThread := Self;
  finally
    FPCAbstractMem.FLockAbstractMem.Release;
  end;

  FErrorsCount := 0;
  FErrors := TStringList.Create;
  FMustRestart := False;
  inherited Create(True);
  FreeOnTerminate := False;
  Suspended := False;
end;

destructor TPCAbstractMemCheckThread.Destroy;
begin
  FPCAbstractMem.FLockAbstractMem.Acquire;
  try
    FPCAbstractMem.FCheckingThread := Nil;
  finally
    FPCAbstractMem.FLockAbstractMem.Release;
  end;
  FErrors.Free;
  inherited Destroy;
end;

procedure TPCAbstractMemCheckThread.Restart;
begin
  FMustRestart := True;
  TLog.NewLog(ltdebug,ClassName,Format('Callirg Restart at %d',[FPCAbstractMem.BlocksCount]) );
end;


{ TPCAbstractMemStats }

procedure TPCAbstractMemStats.Clear;
begin
  Self.FlushesCount := 0;
  Self.FlushesMillis := 0;
end;

function TPCAbstractMemStats.ToString: String;
begin
  Result := Format('PCAbstractMem flushes:%d in %d millis',[Self.FlushesCount,Self.FlushesMillis]);
end;

end.
