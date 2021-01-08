unit UPCAbstractMemAccountKeys;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, SysUtils,
  SyncObjs,
  UAbstractMem, UFileMem, UAbstractMemTList,
  UAbstractBTree, UAbstractAVLTree,
  UPCDataTypes, UBaseTypes, UAVLCache,
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

type
  TAccountsUsingThisKey = Class;

  TAbstractMemAccountKeyNode = record
    myPosition :   TAbstractMemPosition;    // Position in the AbstractMem
    accountKey : TAccountKey;
    accounts_using_this_key_position : TAbstractMemPosition;
    function GetSize : Integer;
    procedure ReadFromMem(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem);
    procedure WriteToMem(AAbstractMem : TAbstractMem);
    procedure Clear;
    function ToString : String;
  end;

  EPCAbstractMemAccountKeys = Class(Exception);

  { TAccountsUsingThisKey }

  TAccountsUsingThisKey = Class(TAbstractMemOrderedTList<Cardinal>)
  protected
    function GetItem(index : Integer) : Cardinal; override;
    procedure LoadFrom(const ABytes : TBytes; var AItem : Cardinal); override;
    procedure SaveTo(const AItem : Cardinal; AIsAddingItem : Boolean; var ABytes : TBytes); override;
    function Compare(const ALeft, ARight : Cardinal) : Integer; override;
  public
    Constructor Create(AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; AUseCache : Boolean); reintroduce;
    Function Add(const AItem : Cardinal) : Integer; reintroduce;
    procedure Delete(index : Integer); reintroduce;
  End;

  TAccountKeyByPosition = record
    position : TAbstractMemPosition;
    accountKey : TAccountKey;
    accountsUsingThisKey : TAccountsUsingThisKey;
    procedure Clear;
    procedure Dispose;
  end;

  TPCAccountKeyByPositionCache = Class(TAVLCache<TAccountKeyByPosition>)
  protected
    procedure BeforeDelete(var AData : TAccountKeyByPosition); override;
  public
  End;

  TPCAbstractMemAccountKeys = Class(TAVLAbstractTree<TAbstractMemAccountKeyNode>)
  private
    FAccountKeysLock : TCriticalSection;
    FAbstractMem : TAbstractMem;
    FPointerToRootPosition : TAbstractMemPosition;
    FRootPosition : TAbstractMemPosition;
    FAccountKeyByPositionCache : TPCAccountKeyByPositionCache;
    FUseCacheOnAbstractMemLists: Boolean;
  protected
    function GetRoot: TAbstractMemAccountKeyNode; override;
    procedure SetRoot(const Value: TAbstractMemAccountKeyNode); override;
    function HasPosition(const ANode : TAbstractMemAccountKeyNode; APosition : TAVLTreePosition) : Boolean; override;
    function GetPosition(const ANode : TAbstractMemAccountKeyNode; APosition : TAVLTreePosition) : TAbstractMemAccountKeyNode; override;
    procedure SetPosition(var ANode : TAbstractMemAccountKeyNode; APosition : TAVLTreePosition; const ANewValue : TAbstractMemAccountKeyNode); override;
    procedure ClearPosition(var ANode : TAbstractMemAccountKeyNode; APosition : TAVLTreePosition); override;
    function GetBalance(const ANode : TAbstractMemAccountKeyNode) : Integer; override;
    procedure SetBalance(var ANode : TAbstractMemAccountKeyNode; ANewBalance : Integer); override;
    function AreEquals(const ANode1, ANode2 : TAbstractMemAccountKeyNode) : Boolean; override;
    procedure ClearNode(var ANode : TAbstractMemAccountKeyNode); override;
    procedure DisposeNode(var ANode : TAbstractMemAccountKeyNode); override;
  public
    function IsNil(const ANode : TAbstractMemAccountKeyNode) : Boolean; override;
    function ToString(const ANode: TAbstractMemAccountKeyNode) : String; override;
    constructor Create(AAbstractMem : TAbstractMem; APointerToRootPosition : TAbstractMemPosition; AUseCacheOnAbstractMemLists : Boolean); reintroduce;
    destructor Destroy; override;
    //
    function GetKeyAtPosition(APosition : TAbstractMemPosition) : TAccountKey;
    function GetPositionOfKey(const AAccountKey : TAccountKey; AAddIfNotFound : Boolean) : TAbstractMemPosition;
    function GetPositionOfKeyAndAddAccount(const AAccountKey : TAccountKey; const AAccountNumber : Cardinal) : TAbstractMemPosition;
    function GetPositionOfKeyAndRemoveAccount(const AAccountKey : TAccountKey; const AAccountNumber : Cardinal) : TAbstractMemPosition;
    procedure GetAccountsUsingKey(const AAccountKey : TAccountKey; const AList : TList<Cardinal>);
    function GetAccountsUsingThisKey(const AAccountKey : TAccountKey) : TAccountsUsingThisKey;
    procedure FlushCache;
    property UseCacheOnAbstractMemLists : Boolean read FUseCacheOnAbstractMemLists write FUseCacheOnAbstractMemLists;
    property AccountKeyByPositionCache : TPCAccountKeyByPositionCache read FAccountKeyByPositionCache;
  end;


implementation

type
  TAccountsUsingThisKey_BlackHole = Class(TAccountsUsingThisKey)
  public
    class function GetInstance : TAccountsUsingThisKey_BlackHole;
  End;

var _TAccountsUsingThisKey_BlackHole : TAccountsUsingThisKey_BlackHole = Nil;
    _BlackHoleAbstractMem : TMem;

{ TAccountsUsingThisKey_BlackHole }

class function TAccountsUsingThisKey_BlackHole.GetInstance: TAccountsUsingThisKey_BlackHole;
var LZone : TAMZone;
begin
  if Not Assigned(_TAccountsUsingThisKey_BlackHole) then begin
    if Not Assigned(_BlackHoleAbstractMem) then begin
      _BlackHoleAbstractMem := TMem.Create(0,True);
    end;
    LZone.Clear;
    _TAccountsUsingThisKey_BlackHole := TAccountsUsingThisKey_BlackHole.Create(_BlackHoleAbstractMem,LZone,True);
  end;
  Result :=  _TAccountsUsingThisKey_BlackHole;
end;


{ TAbstractMemAccountKeyNode }

procedure TAbstractMemAccountKeyNode.Clear;
begin
  Self.myPosition := 0;
  Self.accountKey.EC_OpenSSL_NID := 0;
  Self.accountKey.x := Nil;
  Self.accountKey.y := Nil;
  Self.accounts_using_this_key_position := 0;
end;

function TAbstractMemAccountKeyNode.GetSize: Integer;
begin
  Result := accountKey.GetSerializedLength + 4 + TAbstractMemAVLTreeNodeInfoClass.GetSize;
end;

procedure TAbstractMemAccountKeyNode.ReadFromMem(AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem);
var LBytes : TBytes;
  LRawSize : Integer;
  i : Integer;
begin
  Self.Clear;
  Self.myPosition := AMyPosition;
  inc(AMyPosition,TAbstractMemAVLTreeNodeInfoClass.GetSize);
  // Minimum size is  4 + 2 + 2 = 8 bytes
  i := 8;
  SetLength(LBytes,i);
  if AAbstractMem.Read(AMyPosition,LBytes[0],Length(LBytes))<>Length(LBytes) then raise EPCAbstractMemAccountKeys.Create(Format('Not enough data to read AccountKeyNode at %d',[AMyPosition]));
  Move(LBytes[0],Self.accounts_using_this_key_position,4);
  Move(LBytes[4],Self.accountKey.EC_OpenSSL_NID,2);
  //
  LRawSize := 0;
  Move(LBytes[6],LRawSize,2);
  SetLength(Self.accountKey.x,LRawSize);
  if AAbstractMem.Read(AMyPosition + i,Self.accountKey.x[0],Length(Self.accountKey.x))<>Length(Self.accountKey.x) then raise EPCAbstractMemAccountKeys.Create(Format('Not enough data to read AccountKeyNode.x at %d',[AMyPosition]));
  // Read next
  inc(i,LRawSize);
  LRawSize := 0;
  if AAbstractMem.Read(AMyPosition + i,LRawSize,2)<>2 then raise EPCAbstractMemAccountKeys.Create(Format('Not enough data to read AccountKeyNode.Length(y) at %d',[AMyPosition]));
  SetLength(Self.accountKey.y,LRawSize);
  inc(i,2);
  if AAbstractMem.Read(AMyPosition + i,Self.accountKey.y[0],Length(Self.accountKey.y))<>Length(Self.accountKey.y) then raise EPCAbstractMemAccountKeys.Create(Format('Not enough data to read AccountKeyNode.y at %d',[AMyPosition]));
end;

function TAbstractMemAccountKeyNode.ToString: String;
begin
  Result := Format('AccountKeyNode type %d (length x:%d y:%d) at position %d',[Self.accountKey.EC_OpenSSL_NID,Length(Self.accountKey.x),Length(Self.accountKey.y),Self.myPosition]);
end;

procedure TAbstractMemAccountKeyNode.WriteToMem(AAbstractMem: TAbstractMem);
var LBytes : TBytes;
  LStream : TStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.Write(Self.accounts_using_this_key_position,4);
    Self.accountKey.ToSerialized(LStream);
    LBytes.FromStream(LStream);
    AAbstractMem.Write(Self.myPosition + TAbstractMemAVLTreeNodeInfoClass.GetSize,LBytes[0],Length(LBytes));
  finally
    LStream.Free;
  end;
end;

{ TPCAbstractMemAccountKeys }

function _TPCAbstractMemAccountKeys_Compare(const Left, Right: TAbstractMemAccountKeyNode): Integer;
begin
  Result := Left.accountKey.EC_OpenSSL_NID - Right.accountKey.EC_OpenSSL_NID;
  if (Result = 0) then begin
    Result := TBaseType.BinStrComp(Left.accountKey.x,Right.accountKey.x);
    if Result=0 then begin
      Result := TBaseType.BinStrComp(Left.accountKey.y,Right.accountKey.y);
    end;
  end;

  if (Result=0) and (Left.myPosition>0) and (Right.myPosition>0) then begin
    // This will allow to find exactly a node when both are real (otherwise is searching for a position)
    Result := Left.myPosition - Right.myPosition;
  end;
end;


function TPCAbstractMemAccountKeys.AreEquals(const ANode1, ANode2: TAbstractMemAccountKeyNode): Boolean;
begin
  Result := (ANode1.accountKey.EC_OpenSSL_NID = ANode2.accountKey.EC_OpenSSL_NID)
    and (TBaseType.Equals(ANode1.accountKey.x,ANode2.accountKey.x))
    And (TBaseType.Equals(ANode1.accountKey.y,ANode2.accountKey.y));
end;

procedure TPCAbstractMemAccountKeys.ClearNode(var ANode: TAbstractMemAccountKeyNode);
begin
  ANode.Clear;
end;

procedure TPCAbstractMemAccountKeys.ClearPosition(var ANode: TAbstractMemAccountKeyNode; APosition: TAVLTreePosition);
begin
  TAbstractMemAVLTreeNodeInfoClass.ClearPosition(ANode.myPosition,FAbstractMem,APosition);
end;

function _AccountKeyByPositionCache_Comparision(const Left, Right: TPCAccountKeyByPositionCache.PAVLCacheMemData): Integer;
begin
  // Compare only by data.account number (not for content)
  Result := Left.data.position - Right.data.position;
end;

constructor TPCAbstractMemAccountKeys.Create(AAbstractMem: TAbstractMem; APointerToRootPosition : TAbstractMemPosition; AUseCacheOnAbstractMemLists : Boolean);
begin
  FAccountKeysLock := TCriticalSection.Create;
  FAbstractMem := AAbstractMem;
  FPointerToRootPosition := APointerToRootPosition;
  FRootPosition := 0;
  FUseCacheOnAbstractMemLists := AUseCacheOnAbstractMemLists;
  // Read Root position
  FAbstractMem.Read(FPointerToRootPosition,FRootPosition,4);
  FAccountKeyByPositionCache := TPCAccountKeyByPositionCache.Create(5000,_AccountKeyByPositionCache_Comparision);
  inherited Create(_TPCAbstractMemAccountKeys_Compare,False);
end;

destructor TPCAbstractMemAccountKeys.Destroy;
begin
  FreeAndNil(FAccountKeyByPositionCache);
  FreeAndNil(FAccountKeysLock);
  inherited;
end;

procedure TPCAbstractMemAccountKeys.DisposeNode(var ANode: TAbstractMemAccountKeyNode);
begin
  // Free from mem
  FAbstractMem.Dispose(ANode.myPosition);
end;

procedure TPCAbstractMemAccountKeys.FlushCache;
begin
  FAccountKeyByPositionCache.Clear;
end;

procedure TPCAbstractMemAccountKeys.GetAccountsUsingKey(
  const AAccountKey: TAccountKey; const AList: TList<Cardinal>);
var Lautk : TAccountsUsingThisKey;
  i : Integer;
begin
  AList.Clear;
  FAccountKeysLock.Acquire;
  try
  Lautk := GetAccountsUsingThisKey(AAccountKey);
  if Assigned(Lautk) then begin
    for i:=0 to Lautk.Count-1 do begin
      AList.Add( Lautk.GetItem(i) );
    end;
  end;
  finally
    FAccountKeysLock.Release;
  end;
end;

function TPCAbstractMemAccountKeys.GetAccountsUsingThisKey(const AAccountKey: TAccountKey): TAccountsUsingThisKey;
var LNode : TAbstractMemAccountKeyNode;
  LZone : TAMZone;
  i : Integer;
  LP : TAccountKeyByPosition;
begin
  Result := TAccountsUsingThisKey_BlackHole.GetInstance;
  FAccountKeysLock.Acquire;
  try
  LNode.Clear;
  LNode.accountKey := AAccountKey;
  LNode := Find(LNode);
  if IsNil(LNode) then Exit;
  LZone.Clear;
  LZone.position := LNode.accounts_using_this_key_position;
  // Add Account Number
  if (LNode.accounts_using_this_key_position=0) then Exit;
  LP.Clear;
  LP.position := LNode.myPosition;
  if Not FAccountKeyByPositionCache.Find(LP,LP) then begin
    LP.Clear;
    LP.position := LNode.myPosition;
    LP.accountKey := AAccountKey;
    LP.accountsUsingThisKey := TAccountsUsingThisKey.Create(FAbstractMem,LZone,Self.UseCacheOnAbstractMemLists);
    FAccountKeyByPositionCache.Add(LP); // Add to cache!
  end;
  Result := LP.accountsUsingThisKey;
  finally
    FAccountKeysLock.Release;
  end;
end;

function TPCAbstractMemAccountKeys.GetBalance(const ANode: TAbstractMemAccountKeyNode): Integer;
begin
  Result := TAbstractMemAVLTreeNodeInfoClass.GetBalance(ANode.myPosition,FAbstractMem);
end;

function TPCAbstractMemAccountKeys.GetKeyAtPosition(APosition: TAbstractMemPosition): TAccountKey;
var LNode : TAbstractMemAccountKeyNode;
  LP, LPFound : TAccountKeyByPosition;
  LAccZone : TAMZone;
begin
  FAccountKeysLock.Acquire;
  try
  LP.Clear;
  LP.position := APosition;
  LPFound.Clear;
  if FAccountKeyByPositionCache.Find(LP,LPFound) then begin
    Result := LPFound.accountKey;
  end else begin
    LNode.ReadFromMem(APosition,FAbstractMem);
    Result := LNode.accountKey;
    LP.accountKey := Result;
    if LNode.accounts_using_this_key_position>0 then begin
      LAccZone.Clear;
      LAccZone.position := LNode.accounts_using_this_key_position;
      LP.accountsUsingThisKey := TAccountsUsingThisKey.Create(FAbstractMem,LAccZone,Self.UseCacheOnAbstractMemLists);
    end else LP.accountsUsingThisKey := Nil;
    FAccountKeyByPositionCache.Add(LP); // Add to cache!
  end;
  finally
    FAccountKeysLock.Release;
  end;
end;

function TPCAbstractMemAccountKeys.GetPosition(
  const ANode: TAbstractMemAccountKeyNode;
  APosition: TAVLTreePosition): TAbstractMemAccountKeyNode;
var LPos : TAbstractMemPosition;
begin
  if ANode.myPosition>0 then begin
    LPos := TAbstractMemAVLTreeNodeInfoClass.GetPosition(ANode.myPosition,FAbstractMem,APosition);
  end else LPos := 0;
  if (LPos>0) then begin
    Result.ReadFromMem(LPos,FAbstractMem);
  end else Result.Clear;
end;

function TPCAbstractMemAccountKeys.GetPositionOfKeyAndAddAccount(const AAccountKey: TAccountKey; const AAccountNumber: Cardinal): TAbstractMemPosition;
var LNode : TAbstractMemAccountKeyNode;
  LZone : TAMZone;
  Lacckutk : TAccountsUsingThisKey;
  LAccKeyByPos  : TAccountKeyByPosition;
begin
  FAccountKeysLock.Acquire;
  try
  LNode.Clear;
  LNode.accountKey := AAccountKey;
  LNode := Find(LNode);
  if IsNil(LNode) then begin
    // if LNode does not exists, then ADD
    LNode.accountKey := AAccountKey;
    LNode.accounts_using_this_key_position := 0;
    LNode.myPosition := FAbstractMem.New( LNode.GetSize ).position;
    LNode.WriteToMem(FAbstractMem);
    Add(LNode);
  end;

  LAccKeyByPos.Clear;
  LAccKeyByPos.position := LNode.myPosition;
  if FAccountKeyByPositionCache.Find(LAccKeyByPos,LAccKeyByPos) then begin
    if Not Assigned(LAccKeyByPos.accountsUsingThisKey) then begin
      // We will need to add... remove from cache
      FAccountKeyByPositionCache.Remove(LAccKeyByPos);
      LAccKeyByPos.Clear;
    end;
  end else LAccKeyByPos.Clear;
  if (LAccKeyByPos.position<=0) then begin
    // Create
    LAccKeyByPos.position := LNode.myPosition;
    LAccKeyByPos.accountKey := AAccountKey;
    LZone.Clear;
    if (LNode.accounts_using_this_key_position=0) then begin
      // Create
      LZone := FAbstractMem.New( CT_AbstractMemTList_HeaderSize );
      LNode.accounts_using_this_key_position := LZone.position;
      LNode.WriteToMem( FAbstractMem ); // Save update:
    end else LZone.position := LNode.accounts_using_this_key_position;
    LAccKeyByPos.accountsUsingThisKey := TAccountsUsingThisKey.Create(FAbstractMem,LZone,Self.UseCacheOnAbstractMemLists);
    // Add to cache
    FAccountKeyByPositionCache.Add( LAccKeyByPos );
  end;
  //
  LAccKeyByPos.accountsUsingThisKey.Add( AAccountNumber );

  Result := LNode.myPosition;
  finally
    FAccountKeysLock.Release;
  end;
end;

function TPCAbstractMemAccountKeys.GetPositionOfKeyAndRemoveAccount(
  const AAccountKey: TAccountKey;
  const AAccountNumber: Cardinal): TAbstractMemPosition;
var LNode : TAbstractMemAccountKeyNode;
  LZone : TAMZone;
  i : Integer;
  Lacckutk : TAccountsUsingThisKey;
  LAccKeyByPos : TAccountKeyByPosition;
begin
  FAccountKeysLock.Acquire;
  try
  LNode.Clear;
  LNode.accountKey := AAccountKey;
  LNode := Find(LNode);
  if IsNil(LNode) then begin
    Exit(0);
  end;
  Result := LNode.myPosition;
  // Remove Account Number

  if (LNode.accounts_using_this_key_position=0) then Exit;

  LAccKeyByPos.Clear;
  LAccKeyByPos.position := LNode.myPosition;
  if Not FAccountKeyByPositionCache.Find(LAccKeyByPos,LAccKeyByPos) then begin
    // Create
    LAccKeyByPos.position := LNode.myPosition;
    LAccKeyByPos.accountKey := AAccountKey;
    LZone.Clear;
    LZone.position := LNode.accounts_using_this_key_position;
    LAccKeyByPos.accountsUsingThisKey := TAccountsUsingThisKey.Create(FAbstractMem,LZone,Self.UseCacheOnAbstractMemLists);
    // Add to cache
    FAccountKeyByPositionCache.Add( LAccKeyByPos );
  end;

  if Assigned(LAccKeyByPos.accountsUsingThisKey) then begin
    i := LAccKeyByPos.accountsUsingThisKey.IndexOf( AAccountNumber );
    if i>=0 then begin
      LAccKeyByPos.accountsUsingThisKey.Delete( i );
    end;
  end;
  finally
    FAccountKeysLock.Release;
  end;
end;

function TPCAbstractMemAccountKeys.GetPositionOfKey(const AAccountKey: TAccountKey; AAddIfNotFound : Boolean): TAbstractMemPosition;
var LNode : TAbstractMemAccountKeyNode;
begin
  LNode.Clear;
  LNode.accountKey := AAccountKey;
  LNode := Find(LNode);
  if (IsNil(LNode) and (AAddIfNotFound)) then begin
    // if LNode does not exists, then ADD
    LNode.accountKey := AAccountKey;
    LNode.accounts_using_this_key_position := 0;
    LNode.myPosition := FAbstractMem.New( LNode.GetSize ).position;
    LNode.WriteToMem(FAbstractMem);
    Add(LNode);
  end;
  Result := LNode.myPosition;
end;

function TPCAbstractMemAccountKeys.GetRoot: TAbstractMemAccountKeyNode;
begin
  if FRootPosition>0 then Result.ReadFromMem( FRootPosition , FAbstractMem )
  else Result.Clear;
end;

function TPCAbstractMemAccountKeys.HasPosition(
  const ANode: TAbstractMemAccountKeyNode;
  APosition: TAVLTreePosition): Boolean;
begin
  if (ANode.myPosition>0) then begin
    Result := TAbstractMemAVLTreeNodeInfoClass.GetPosition(ANode.myPosition,FAbstractMem,APosition)>0;
  end else Result := False;
end;

function TPCAbstractMemAccountKeys.IsNil(
  const ANode: TAbstractMemAccountKeyNode): Boolean;
begin
  Result := ANode.myPosition=0;
end;

procedure TPCAbstractMemAccountKeys.SetBalance(
  var ANode: TAbstractMemAccountKeyNode; ANewBalance: Integer);
begin
  TAbstractMemAVLTreeNodeInfoClass.SetBalance(ANode.myPosition,FAbstractMem,ANewBalance);
end;

procedure TPCAbstractMemAccountKeys.SetPosition(
  var ANode: TAbstractMemAccountKeyNode; APosition: TAVLTreePosition;
  const ANewValue: TAbstractMemAccountKeyNode);
begin
  TAbstractMemAVLTreeNodeInfoClass.SetPosition(ANode.myPosition,FAbstractMem,APosition,ANewValue.myPosition);
end;

procedure TPCAbstractMemAccountKeys.SetRoot(
  const Value: TAbstractMemAccountKeyNode);
begin
  FRootPosition := Value.myPosition;
  // Save
  if Value.myPosition>0 then begin
    Value.WriteToMem(FAbstractMem);
  end;
  FAbstractMem.Write(FPointerToRootPosition,FRootPosition,4);
end;

function TPCAbstractMemAccountKeys.ToString(const ANode: TAbstractMemAccountKeyNode): String;
begin
  Result := ANode.ToString;
end;

{ TAccountsUsingThisKey }

procedure TAccountsUsingThisKey.LoadFrom(const ABytes: TBytes; var AItem: Cardinal);
begin
  Move(ABytes[0],AItem,4);
end;

procedure TAccountsUsingThisKey.SaveTo(const AItem: Cardinal; AIsAddingItem : Boolean; var ABytes: TBytes);
begin
  SetLength(ABytes,4);
  Move(AItem,ABytes[0],4);
  raise Exception.Create('INCONSISTENT 20200324-1 NEVER CALL HERE');
end;

function TAccountsUsingThisKey.Add(const AItem: Cardinal): Integer;
var
  LFound : Boolean;
  LBytes : TBytes;
  LZone : TAMZone;
begin
  FList.LockList;
  try
    LFound := Find(AItem,Result);
    if (LFound and AllowDuplicates) or (Not LFound) then begin
      FList.Insert( Result , AItem );
    end else Result := -1;
  finally
    FList.UnlockList;
  end;
end;

function TAccountsUsingThisKey.Compare(const ALeft, ARight: Cardinal): Integer;
begin
  Result := ALeft - ARight;
end;

constructor TAccountsUsingThisKey.Create(AAbstractMem: TAbstractMem; const AInitialZone: TAMZone; AUseCache : Boolean);
begin
  inherited Create(AAbstractMem,AInitialZone,1000,False, AUseCache);
end;

procedure TAccountsUsingThisKey.Delete(index: Integer);
begin
  FList.Delete( index );
end;

function TAccountsUsingThisKey.GetItem(index: Integer): Cardinal;
begin
  Result := FList.Position[index];
end;

{ TPCAccountKeyByPositionCache }

procedure TPCAccountKeyByPositionCache.BeforeDelete(var AData: TAccountKeyByPosition);
begin
  inherited;
  if Assigned(AData.accountsUsingThisKey) then begin
    FreeAndNil(AData.accountsUsingThisKey);
  end;
end;

{ TAccountKeyByPosition }

procedure TAccountKeyByPosition.Clear;
begin
  Self.position := 0;
  Self.accountKey := CT_AccountInfo_NUL.accountKey;
  Self.accountsUsingThisKey := Nil;
end;

procedure TAccountKeyByPosition.Dispose;
begin
  FreeAndNil(Self.accountsUsingThisKey);
end;

initialization
  _TAccountsUsingThisKey_BlackHole := Nil;
  _BlackHoleAbstractMem := Nil;
finalization
  FreeAndNil(_TAccountsUsingThisKey_BlackHole);
  FreeAndNil(_BlackHoleAbstractMem);
end.
