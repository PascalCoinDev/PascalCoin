unit UPCAbstractMem;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, SysUtils, SyncObjs,
  UAbstractMem, UFileMem, UAbstractMemTList,
  UAbstractBTree, UThread,
  UAVLCache, ULog, UCrypto,
  UPCAbstractMemAccountKeys,
  UPCDataTypes, UBaseTypes, UConst, UPCSafeBoxRootHash, UOrderedList,
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

  TPCAbstractMemListAccounts = class;

  TAccountNameInfo = record
    accountName: string;
    accountNumber: cardinal;
  end;

  { TPCAbstractMemListAccountNames }

  TPCAbstractMemListAccountNames = class(TAbstractMemOrderedTList<TAccountNameInfo>)
  private
    FPCAbstractMem: TPCAbstractMem;
  protected
    function ToString(const AItem: TAccountNameInfo): string; override;

    procedure LoadFrom(const ABytes: TBytes; var AItem: TAccountNameInfo); override;
    procedure SaveTo(const AItem: TAccountNameInfo; AIsAddingItem : Boolean; var ABytes: TBytes); override;
    function Compare(const ALeft, ARight: TAccountNameInfo): integer; override;
  public
    function IndexOf(const AName : String) : Integer;
    procedure Remove(const AName : String);
    procedure Add(const AName : String; AAccountNumber : Cardinal);
    function FindByName(const AName : String; out AIndex : Integer) : Boolean;
  end;

  { TPCAbstractMemListAccounts }

  TPCAbstractMemListAccounts = class(TAbstractMemTList<TAccount>)
  private
    FPCAbstractMem: TPCAbstractMem;
  protected
    procedure LoadFrom(const ABytes: TBytes; var AItem: TAccount); override;
    procedure SaveTo(const AItem: TAccount; AIsAddingItem : Boolean; var ABytes: TBytes); override;
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
  protected
    procedure UpgradeAbstractMemVersion(const ACurrentHeaderVersion : Integer);
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
  end;

implementation

uses UAccounts;

const
  CT_PCAbstractMem_FileVersion = CT_PROTOCOL_5;
  CT_PCAbstractMem_HeaderVersion = 1;

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
  inherited Create(1000*32);
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

{ TPCAbstractMemListAccounts }

procedure TPCAbstractMemListAccounts.LoadFrom(const ABytes: TBytes; var AItem: TAccount);
var
  LPointer: TAbstractMemPosition;
  LStream : TStream;
  w : Word;
begin
  AItem.Clear;
  LStream := TMemoryStream.Create;
  Try
    LPointer := 0;
    LStream.Write(ABytes[0],Length(ABytes));
    LStream.Position := 0;

    LStream.Read( AItem.account , 4 );

    LStream.Read( w,2 );
    if (w<>CT_PROTOCOL_5) then raise EPCAbstractMem.Create(Format('Invalid Account %d protocol %d',[AItem.account,w]));

    LStream.Read( w, 2 );
    case w of
      CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : Begin
        AItem.accountInfo.state := as_Normal;
        LStream.Read(LPointer,4);
        AItem.accountInfo.accountKey := FPCAbstractMem.FAccountKeys.GetKeyAtPosition( LPointer );
        if w<>AItem.accountInfo.accountKey.EC_OpenSSL_NID then raise EPCAbstractMem.Create('INCONSISTENT 20200318-2');
      End;
      CT_AccountInfo_ForSale, CT_AccountInfo_ForAccountSwap, CT_AccountInfo_ForCoinSwap : Begin
        case w of
          CT_AccountInfo_ForSale : AItem.accountInfo.state := as_ForSale;
          CT_AccountInfo_ForAccountSwap : AItem.accountInfo.state := as_ForAtomicAccountSwap;
          CT_AccountInfo_ForCoinSwap : AItem.accountInfo.state := as_ForAtomicCoinSwap;
        end;
        LStream.Read(LPointer,4);
        AItem.accountInfo.accountKey := FPCAbstractMem.FAccountKeys.GetKeyAtPosition( LPointer );

        LStream.Read(AItem.accountInfo.locked_until_block,4);
        LStream.Read(AItem.accountInfo.price,8);
        LStream.Read(AItem.accountInfo.account_to_pay,4);
        LStream.Read(LPointer,4);
        AItem.accountInfo.new_publicKey := FPCAbstractMem.FAccountKeys.GetKeyAtPosition( LPointer );
        if (w<>CT_AccountInfo_ForSale) then begin
          AItem.accountInfo.hashed_secret.FromSerialized(LStream);
        end;

      End;
      else raise EPCAbstractMem.Create(Format('Unknow accountInfo type %d for account %d',[w,Aitem.account]));
    end;
    //
    LStream.Read( AItem.balance , 8);
    LStream.Read( AItem.updated_on_block_passive_mode , 4);
    LStream.Read( AItem.updated_on_block_active_mode , 4);
    LStream.Read( AItem.n_operation , 4);
    AItem.name.FromSerialized( LStream );
    LStream.Read( AItem.account_type ,2);
    AItem.account_data.FromSerialized( LStream );
    if AItem.account_seal.FromSerialized( LStream )<0 then raise EPCAbstractMem.Create('INCONSISTENT 20200318-4');
    // Force account_seal to 20 bytes
    if Length(AItem.account_seal)<>20 then begin
      AItem.account_seal := TBaseType.T20BytesToRawBytes( TBaseType.To20Bytes(AItem.account_seal) );
    end;
  Finally
    LStream.Free;
  End;
end;

procedure TPCAbstractMemListAccounts.SaveTo(const AItem: TAccount; AIsAddingItem : Boolean; var ABytes: TBytes);
var LStream : TStream;
  LPointer : TAbstractMemPosition;
  w : Word;
  LPrevious : TAccount;
begin
  if (Length(ABytes)>0) and (Not AIsAddingItem) then begin
    // Capture previous values
    LoadFrom(ABytes,LPrevious);
    if (LPrevious.account<>AItem.account) then raise EPCAbstractMem.Create(Format('INCONSISTENT account number %d<>%d',[AItem.account,LPrevious.account]));

    if Not LPrevious.accountInfo.accountKey.IsEqualTo( AItem.accountInfo.accountKey ) then begin
      // Remove previous account link
      FPCAbstractMem.FAccountKeys.GetPositionOfKeyAndRemoveAccount( LPrevious.accountInfo.accountKey, LPrevious.account );
    end;
  end;

  LStream := TMemoryStream.Create;
  try
    LStream.Position := 0;


    LStream.Write( AItem.account , 4 );

    w := CT_PROTOCOL_5;
    LStream.Write( w, 2 );

    w := 0;
    case AItem.accountInfo.state of
      as_Normal : begin
        LPointer := FPCAbstractMem.FAccountKeys.GetPositionOfKeyAndAddAccount(AItem.accountInfo.accountKey,AItem.account);
        LStream.Write( AItem.accountInfo.accountKey.EC_OpenSSL_NID , 2 );
        LStream.Write( LPointer, 4);
      end;
      as_ForSale : w := CT_AccountInfo_ForSale;
      as_ForAtomicAccountSwap : w := CT_AccountInfo_ForAccountSwap;
      as_ForAtomicCoinSwap :  w := CT_AccountInfo_ForCoinSwap;
    end;
    if (w>0) then begin
      LStream.Write(w,2);

      LPointer := FPCAbstractMem.FAccountKeys.GetPositionOfKeyAndAddAccount(AItem.accountInfo.accountKey,AItem.account);
      LStream.Write( LPointer, 4);

      LStream.Write(AItem.accountInfo.locked_until_block,4);
      LStream.Write(AItem.accountInfo.price,8);
      LStream.Write(AItem.accountInfo.account_to_pay,4);
      LPointer := FPCAbstractMem.FAccountKeys.GetPositionOfKey(AItem.accountInfo.new_publicKey,True);
      LStream.Write(LPointer,4);
      if (w<>CT_AccountInfo_ForSale) then begin
        AItem.accountInfo.hashed_secret.ToSerialized(LStream);
      end;
    end;
    //
    LStream.Write( AItem.balance , 8);
    LStream.Write( AItem.updated_on_block_passive_mode , 4);
    LStream.Write( AItem.updated_on_block_active_mode , 4);
    LStream.Write( AItem.n_operation , 4);

    AItem.name.ToSerialized( LStream );

    LStream.Write( AItem.account_type ,2);
    AItem.account_data.ToSerialized( LStream );
    AItem.account_seal.ToSerialized( LStream );
    //
    ABytes.FromStream( LStream );

  finally
    LStream.Free;
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
  ...
  [96..99] 4 bytes: Header version
  }
var LZone,
  LZoneBlocks,
  LZoneAccounts,
  LZoneAccountsNames,
  LZoneAccountKeys : TAMZone;
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
        Move(LHeader[96], LHeaderVersion, 4);
        if (LHeaderVersion>CT_PCAbstractMem_HeaderVersion) then begin
          TLog.NewLog(lterror,ClassName,Format('Header version readed %d is greater than expected %d',[LHeaderVersion,CT_PCAbstractMem_HeaderVersion]));
        end else begin
          AIsNewStructure := False;
        end;
      end;
    end;
  end;
  if (Not FAbstractMem.ReadOnly) and (AIsNewStructure) then begin
    // Initialize struct
    FAbstractMem.ClearContent;
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

    Move(LZoneBlocks.position,       LHeader[16],4);
    Move(LZoneAccounts.position,     LHeader[20],4);
    Move(LZoneAccountsNames.position,LHeader[24],4);
    Move(LZoneAccountKeys.position,  LHeader[28],4);
    Move(FZoneAggregatedHashrate.position,LHeader[32],4);
    LHeaderVersion := CT_PCAbstractMem_HeaderVersion;
    Move(LHeaderVersion,             LHeader[96],4);

    FAbstractMem.Write(LZone.position,LHeader[0],Length(LHeader));

  end;
  // Free
  FreeAndNil(FBlocks);
  //
  FBlocks := TPCAbstractMemListBlocks.Create( FAbstractMem, LZoneBlocks, 10000, Self.UseCacheOnAbstractMemLists);
  FBlocks.FPCAbstractMem := Self;

  FAccounts := TPCAbstractMemListAccounts.Create( FAbstractMem, LZoneAccounts, 50000, Self.UseCacheOnAbstractMemLists);
  FAccounts.FPCAbstractMem := Self;

  FAccountsNames := TPCAbstractMemListAccountNames.Create( FAbstractMem, LZoneAccountsNames, 5000 , False, Self.UseCacheOnAbstractMemLists);
  FAccountsNames.FPCAbstractMem := Self;

  FAccountKeys := TPCAbstractMemAccountKeys.Create( FAbstractMem, LZoneAccountKeys.position, Self.UseCacheOnAbstractMemLists);

  // Read AggregatedHashrate
  SetLength(LBuffer,100);
  FAbstractMem.Read(FZoneAggregatedHashrate.position,LBuffer[0],Length(LBuffer));
  if LBigNum.FromSerialized(LBuffer) then begin
    FAggregatedHashrate.RawValue := LBigNum;
  end;
  FBufferBlocksHash := TPCAbstractMemBytesBuffer32Safebox.Create(FAbstractMem,LZoneBuffersBlockHash,FBlocks.Count);

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
  FAbstractMem.ClearContent;
  DoInit(LIsNewStructure);
end;

constructor TPCAbstractMem.Create(const ASafeboxFileName: string; AReadOnly: boolean);
var
  LIsNewStructure : Boolean;
begin
  FStats.Clear;

  FUseCacheOnAbstractMemLists := True;
  FMaxMemUsage := 100 * 1024 * 1024;

  FBlocks := Nil;
  FAccounts:= Nil;
  FAccountsNames:= Nil;
  FAccountKeys:= Nil;
  FBufferBlocksHash:= Nil;

  FCheckingThread := Nil;
  FLockAbstractMem := TPCCriticalSection.Create(Self.ClassName);
  FAccountCache := TAccountCache.Create(10000,_AccountCache_Comparision);

  FAggregatedHashrate := TBigNum.Create(0);
  FFileName := ASafeboxFileName;
  if (FFileName<>'') {and (FileExists(ASafeboxFileName))} then begin
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

procedure TPCAbstractMem.FlushCache;
var LBigNum : TBytes;
  Ltc : TTickCount;
begin
  if FAbstractMem.ReadOnly then Exit;
  Ltc := TPlatform.GetTickCount;
  FBlocks.FlushCache;
  FAccounts.FlushCache;
  FAccountsNames.FlushCache;
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
  FAccountCache.Remove(AAccount);
  if (AAccount.account = FAccounts.Count) then begin
    FAccounts.Add(AAccount);
  end else begin
    FAccounts.SetItem( AAccount.account , AAccount);
  end;
  // Update cache
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
begin
  LFirstTC := TPlatform.GetTickCount;
  LTC := LFirstTC;
  if (ACurrentHeaderVersion=0) then begin
    // Redo AccountNames
    TLog.NewLog(ltinfo,ClassName,Format('Upgrade AbstractMem file from %d to %d with %d Accounts and %d AccNames',[ACurrentHeaderVersion,CT_PCAbstractMem_HeaderVersion, AccountsCount, AccountsNames.Count]));
    AccountsNames.Clear;
    for i := 0 to AccountsCount-1 do begin
      LAccount := GetAccount(i);
      if Length(LAccount.name)>0 then begin
        AccountsNames.Add( LAccount.name.ToString, LAccount.account );
      end;
      if TPlatform.GetElapsedMilliseconds(LTC)>5000 then begin
        LTC := TPlatform.GetTickCount;
        TLog.NewLog(ltdebug,ClassName,Format('Upgrading %d/%d found %d',[i,AccountsCount,AccountsNames.Count]));
      end;
    end;
    TLog.NewLog(ltdebug,ClassName,Format('End upgrade found %d',[AccountsNames.Count]));
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
    Result := FAccounts.GetItem( AAccountNumber );
    // Save for future usage:
    FAccountCache.Add(Result);
  end;
end;

{ TPCAbstractMemListAccountNames }

function TPCAbstractMemListAccountNames.ToString(const AItem: TAccountNameInfo): string;
begin
  Result:= Format('AccountNameInfo: Account:%d Name(%d):%d',[AItem.accountNumber, Length(AItem.accountName), AItem.accountName]);
end;

function TPCAbstractMemListAccountNames.IndexOf(const AName: String): Integer;
var LFind : TAccountNameInfo;
begin
  LFind.accountName := AName;
  LFind.accountNumber := 0;
  if Not Find(LFind,Result) then Result := -1;
end;

procedure TPCAbstractMemListAccountNames.LoadFrom(const ABytes: TBytes; var AItem: TAccountNameInfo);
var LTmp : TBytes;
begin
  if Not LTmp.FromSerialized(ABytes) then raise EPCAbstractMem.Create('INCONSISTENT 20200318-5');
  AItem.accountName := LTmp.ToString;
  Move(ABytes[LTmp.GetSerializedLength],AItem.accountNumber,4);
end;

procedure TPCAbstractMemListAccountNames.Remove(const AName: String);
var i : Integer;
begin
  i := IndexOf(AName);
  if i>=0 then Delete(i);
end;

procedure TPCAbstractMemListAccountNames.SaveTo(const AItem: TAccountNameInfo; AIsAddingItem : Boolean; var ABytes: TBytes);
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

procedure TPCAbstractMemListAccountNames.Add(const AName: String; AAccountNumber: Cardinal);
var LItem : TAccountNameInfo;
  i : Integer;
begin
  LItem.accountName := AName;
  LItem.accountNumber := AAccountNumber;
  i := inherited Add(LItem);
  if (i<0) then begin
    i := IndexOf(AName);
    if (i<0) then
      raise EPCAbstractMem.Create(Format('Fatal error Cannot add account(%d) name %s',[AAccountNumber,AName]))
    else raise EPCAbstractMem.Create(Format('Cannot add account(%d) name %s because used by %d with %s',[AAccountNumber,AName,
      GetItem(i).accountNumber,GetItem(i).accountName]));
  end;
end;

function TPCAbstractMemListAccountNames.Compare(const ALeft, ARight: TAccountNameInfo): integer;
Var LBytesLeft,LBytesRight : TBytes;
begin
  LBytesLeft.FromString(ALeft.accountName);
  LBytesRight.FromString(ARight.accountName);
  Result := TBaseType.BinStrComp(LBytesLeft,LBytesRight);
end;

function TPCAbstractMemListAccountNames.FindByName(const AName: String; out AIndex: Integer): Boolean;
var LFind : TAccountNameInfo;
begin
  LFind.accountName := AName;
  LFind.accountNumber := 0;
  Result := Find(LFind,AIndex);
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
var iBlock, i, iAccName : Integer;
  LAccount : TAccount;
  LBlockAccount : TBlockAccount;
  LOrdered : TOrderedList<Integer>;
  LOrderedNames : TOrderedList<String>;
  LAccountNameInfo : TAccountNameInfo;
  LTC, LTCInitial : TTickCount;
  LAggregatedHashrate, LBlockHashRate : TBigNum;
begin
  iBlock :=0;
  LOrdered := TOrderedList<Integer>.Create(False,TComparison_Integer);
  LOrderedNames := TOrderedList<String>.Create(False,TComparison_String);
  LAggregatedHashrate := TBigNum.Create;
  Try
    LTC := TPlatform.GetTickCount;
    LTCInitial := LTC;
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
      end;

      LBlockAccount := FPCAbstractMem.GetBlockAccount(iBlock);
      for i:=Low(LBlockAccount.accounts) to high(LBlockAccount.accounts) do begin
        //
        LAccount := LBlockAccount.accounts[i];
        if Length(LAccount.name)>0 then begin
          if LOrderedNames.Add(LAccount.name.ToString)<0 then begin
            _error(Format('Account %d name %s allready added',[LAccount.account,LAccount.name.ToString]));
          end;
          iAccName := FPCAbstractMem.AccountsNames.IndexOf(LAccount.name.ToString);
          if iAccName<0 then begin
            // ERROR
            _error(Format('Account %d name %s not found at list',[LAccount.account,LAccount.name.ToString]));
          end else begin
            if FPCAbstractMem.AccountsNames.Item[iAccName].accountNumber<>LAccount.account then begin
              _error(Format('Account %d name %s found at list at pos %d but links to %d',[LAccount.account,LAccount.name.ToString,iAccName,FPCAbstractMem.AccountsNames.Item[iAccName].accountNumber]));
            end;
            if (LOrdered.Add(LAccount.account)<0) then begin
              _error(Format('Account %d (with name %s) allready added',[LAccount.account,LAccount.name.ToString]));
            end;
          end;
        end;
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
    for i := 0 to FPCAbstractMem.AccountsNames.Count-1 do begin
      LAccountNameInfo := FPCAbstractMem.AccountsNames.Item[i];
      if LOrdered.IndexOf( LAccountNameInfo.accountNumber ) < 0 then begin
        _error(Format('Account name %s at index %d/%d not found in search',[LAccountNameInfo.accountName, i+1,FPCAbstractMem.AccountsNames.Count]));
      end;
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
