unit UPCAbstractMemAccounts;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, SysUtils, SyncObjs,
  UPCAbstractMemAccountKeys, UPCAccountsOrdenations,
  UAbstractMem,
  UAbstractMemTList,
  UPCDataTypes,
  UBaseTypes,
  UConst,
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

type
  { TPCAbstractMemListAccounts }

  TPCAbstractMemListAccounts = class(TAbstractMemTList<TAccount>)
  private
    FAccountKeys: TPCAbstractMemAccountKeys;
    FAccountsOrderedByUpdatedBlock : TAccountsOrderedByUpdatedBlock;
    FAccountsOrderedBySalePrice : TAccountsOrderedBySalePrice;
  protected
    procedure LoadFrom(const ABytes: TBytes; var AItem: TAccount); override;
    procedure SaveTo(const AItem: TAccount; AIsAddingItem : Boolean; var ABytes: TBytes); override;
  public
    Constructor Create(AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; ADefaultElementsPerBlock : Integer; AUseCache : Boolean); override;
    class procedure LoadAccountFromTBytes(const ABytes: TBytes; const AAccountKeys : TPCAbstractMemAccountKeys; var AItem: TAccount);
    property AccountKeys: TPCAbstractMemAccountKeys read FAccountKeys write FAccountKeys;
    property AccountsOrderedByUpdatedBlock: TAccountsOrderedByUpdatedBlock read FAccountsOrderedByUpdatedBlock write FAccountsOrderedByUpdatedBlock;
    property AccountsOrderedBySalePrice: TAccountsOrderedBySalePrice read FAccountsOrderedBySalePrice write FAccountsOrderedBySalePrice;
  end;

  EAbsctractMemAccounts = Class(Exception);

implementation

uses UAccounts, UOrderedList;

{ TPCAbstractMemListAccounts }

constructor TPCAbstractMemListAccounts.Create(AAbstractMem: TAbstractMem;
  const AInitialZone: TAMZone; ADefaultElementsPerBlock: Integer;
  AUseCache: Boolean);
begin
  inherited;
  FAccountKeys := Nil;
  FAccountsOrderedByUpdatedBlock := Nil;
  FAccountsOrderedBySalePrice := Nil;
end;

class procedure TPCAbstractMemListAccounts.LoadAccountFromTBytes(
  const ABytes: TBytes; const AAccountKeys: TPCAbstractMemAccountKeys;
  var AItem: TAccount);
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
    if (w<>CT_PROTOCOL_5) then raise EAbsctractMemAccounts.Create(Format('Invalid Account %d protocol %d',[AItem.account,w]));

    LStream.Read( w, 2 );
    case w of
      CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : Begin
        AItem.accountInfo.state := as_Normal;
        LStream.Read(LPointer,4);
        if Assigned(AAccountKeys) then begin
          AItem.accountInfo.accountKey := AAccountKeys.GetKeyAtPosition( LPointer );
          if w<>AItem.accountInfo.accountKey.EC_OpenSSL_NID then raise EAbsctractMemAccounts.Create('INCONSISTENT 20200318-2');
        end;
      End;
      CT_AccountInfo_ForSale, CT_AccountInfo_ForAccountSwap, CT_AccountInfo_ForCoinSwap : Begin
        case w of
          CT_AccountInfo_ForSale : AItem.accountInfo.state := as_ForSale;
          CT_AccountInfo_ForAccountSwap : AItem.accountInfo.state := as_ForAtomicAccountSwap;
          CT_AccountInfo_ForCoinSwap : AItem.accountInfo.state := as_ForAtomicCoinSwap;
        end;
        LStream.Read(LPointer,4);
        if Assigned(AAccountKeys) then begin
          AItem.accountInfo.accountKey := AAccountKeys.GetKeyAtPosition( LPointer );
        end;

        LStream.Read(AItem.accountInfo.locked_until_block,4);
        LStream.Read(AItem.accountInfo.price,8);
        LStream.Read(AItem.accountInfo.account_to_pay,4);
        LStream.Read(LPointer,4);
        if Assigned(AAccountKeys) then begin
          AItem.accountInfo.new_publicKey := AAccountKeys.GetKeyAtPosition( LPointer );
        end;
        if (w<>CT_AccountInfo_ForSale) then begin
          AItem.accountInfo.hashed_secret.FromSerialized(LStream);
        end;

      End;
      else raise EAbsctractMemAccounts.Create(Format('Unknow accountInfo type %d for account %d',[w,Aitem.account]));
    end;
    //
    LStream.Read( AItem.balance , 8);
    LStream.Read( AItem.updated_on_block_passive_mode , 4);
    LStream.Read( AItem.updated_on_block_active_mode , 4);
    LStream.Read( AItem.n_operation , 4);
    AItem.name.FromSerialized( LStream );
    LStream.Read( AItem.account_type ,2);
    AItem.account_data.FromSerialized( LStream );
    if AItem.account_seal.FromSerialized( LStream )<0 then raise EAbsctractMemAccounts.Create('INCONSISTENT 20200318-4');
    // Force account_seal to 20 bytes
    if Length(AItem.account_seal)<>20 then begin
      AItem.account_seal := TBaseType.T20BytesToRawBytes( TBaseType.To20Bytes(AItem.account_seal) );
    end;
  Finally
    LStream.Free;
  End;
end;

procedure TPCAbstractMemListAccounts.LoadFrom(const ABytes: TBytes; var AItem: TAccount);
begin
  LoadAccountFromTBytes(ABytes,FAccountKeys,AItem);
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
    if (LPrevious.account<>AItem.account) then raise EAbsctractMemAccounts.Create(Format('INCONSISTENT account number %d<>%d',[AItem.account,LPrevious.account]));

    if Not LPrevious.accountInfo.accountKey.IsEqualTo( AItem.accountInfo.accountKey ) then begin
      // Remove previous account link
      FAccountKeys.GetPositionOfKeyAndRemoveAccount( LPrevious.accountInfo.accountKey, LPrevious.account );
    end;

    if LPrevious.updated_on_block_active_mode<>AItem.updated_on_block_active_mode then begin
      FAccountsOrderedByUpdatedBlock.Update(AItem.account,LPrevious.updated_on_block_active_mode,AItem.updated_on_block_active_mode);
    end;
    FAccountsOrderedBySalePrice.UpdateAccountBySalePrice(AItem.account,LPrevious.accountInfo,AItem.accountInfo);
  end else begin
    FAccountsOrderedByUpdatedBlock.Update(AItem.account,0,AItem.updated_on_block_active_mode);
    FAccountsOrderedBySalePrice.UpdateAccountBySalePrice(AItem.account,CT_AccountInfo_NUL,AItem.accountInfo);
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
        LPointer := FAccountKeys.GetPositionOfKeyAndAddAccount(AItem.accountInfo.accountKey,AItem.account);
        LStream.Write( AItem.accountInfo.accountKey.EC_OpenSSL_NID , 2 );
        LStream.Write( LPointer, 4);
      end;
      as_ForSale : w := CT_AccountInfo_ForSale;
      as_ForAtomicAccountSwap : w := CT_AccountInfo_ForAccountSwap;
      as_ForAtomicCoinSwap :  w := CT_AccountInfo_ForCoinSwap;
    end;
    if (w>0) then begin
      LStream.Write(w,2);

      LPointer := FAccountKeys.GetPositionOfKeyAndAddAccount(AItem.accountInfo.accountKey,AItem.account);
      LStream.Write( LPointer, 4);

      LStream.Write(AItem.accountInfo.locked_until_block,4);
      LStream.Write(AItem.accountInfo.price,8);
      LStream.Write(AItem.accountInfo.account_to_pay,4);
      LPointer := FAccountKeys.GetPositionOfKey(AItem.accountInfo.new_publicKey,True);
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


end.
