﻿unit UOpTransaction;

{ Copyright (c) 2016 by Albert Molina

  Acknowledgements:
  - Herman Schoenfeld for implementing atomic swap operations (HLTC)

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

Uses UCrypto, UBlockChain, Classes, UAccounts, UBaseTypes,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UPCDataTypes, UEPasa, UOrderedList;

Type
  // Operations Type
  TOpTransactionStyle = (transaction, transaction_with_auto_buy_account, buy_account, transaction_with_auto_atomic_swap);
    // transaction = Single standard transaction
    // transaction_with_auto_buy_account = Single transaction made over an account listed for private sale. For STORING purposes only
    // buy_account = A Buy account operation
    // transaction_with_auto_atomic_swap = Single transaction made over an account listed for atomic swap (coin swap or account swap)

  TOpTransactionData = Record
    sender: Cardinal;
    n_operation : Cardinal;
    target: Cardinal;
    amount: UInt64;
    fee: UInt64;
    payload: TOperationPayload;
    public_key: TECDSA_Public;
    sign: TECDSA_SIG;
    // Protocol 2
    // Next values will only be filled after this operation is executed
    opTransactionStyle : TOpTransactionStyle;
    AccountPrice : UInt64;
    SellerAccount : Cardinal;
    new_accountkey : TAccountKey;
  End;

  TOpChangeKeyData = Record
    account_signer,
    account_target: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    payload: TOperationPayload;
    public_key: TECDSA_Public;
    new_accountkey: TAccountKey;
    sign: TECDSA_SIG;
  End;

  TOpRecoverFoundsData = Record
    account: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    new_accountkey: TAccountKey;
  End;

Const
  CT_TOpTransactionData_NUL : TOpTransactionData = (sender:0;n_operation:0;target:0;amount:0;fee:0;payload:(payload_type:0;payload_raw:Nil);public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);sign:(r:Nil;s:Nil);opTransactionStyle:transaction;AccountPrice:0;SellerAccount:0;new_accountkey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));
  CT_TOpChangeKeyData_NUL : TOpChangeKeyData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:(payload_type:0;payload_raw:Nil);public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);new_accountkey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);sign:(r:Nil;s:Nil));
  CT_TOpRecoverFoundsData_NUL : TOpRecoverFoundsData = (account:0;n_operation:0;fee:0;new_accountkey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));

Type
  { TOpTransaction }

  TOpTransaction = Class(TPCOperation)
  private
    FData : TOpTransactionData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(APrevious : TAccountPreviousBlockInfo; ASafeBoxTransaction : TPCSafeBoxTransaction; var AErrors : String) : Boolean; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    //
    class function OpType : Byte; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Property Data : TOpTransactionData read FData;

    Constructor CreateTransaction(ACurrentProtocol : Word; sender, n_operation, target: Cardinal; key: TECPrivateKey; amount, fee: UInt64; const payload: TOperationPayload);
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;

    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;

  { TOpChangeKey }

  TOpChangeKey = Class(TPCOperation)
  private
    FData : TOpChangeKeyData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor Create(ACurrentProtocol : Word; account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; const payload: TOperationPayload);
    Property Data : TOpChangeKeyData read FData;
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;

    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;

  { TOpChangeKeySigned }

  TOpChangeKeySigned = Class(TOpChangeKey)
  public
    class function OpType : Byte; override;
  end;


  { TOpRecoverFounds }

  TOpRecoverFounds = Class(TPCOperation)
  private
    FData : TOpRecoverFoundsData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function N_Operation : Cardinal; override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    Constructor Create(ACurrentProtocol : word; account_number, n_operation: Cardinal; fee: UInt64; new_accountkey : TAccountKey);
    Property Data : TOpRecoverFoundsData read FData;
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;
    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;

  // NEW OPERATIONS PROTOCOL 2
  TOpListAccountOperationType = (lat_Unknown, lat_ListAccount, lat_DelistAccount);

  TOpListAccountData = Record
    account_signer,
    account_target: Cardinal;
    operation_type : TOpListAccountOperationType;
    n_operation : Cardinal;
    account_state: TAccountState;
    account_price: UInt64;
    account_to_pay : Cardinal;
    fee: UInt64;
    hash_lock : T32Bytes;
    payload: TOperationPayload;
    public_key: TAccountKey;
    new_public_key: TAccountKey;   // If EC_OpenSSL_NID=0 then is OPEN, otherwise is for only 1 public key
    locked_until_block : Cardinal; //
    sign: TECDSA_SIG;
  End;

  TOpChangeAccountInfoData = Record
    account_signer,
    account_target: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    payload: TOperationPayload;
    public_key: TECDSA_Public;
    changes_type : TOpChangeAccountInfoTypes; // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type , $0008 = New Data
    new_accountkey: TAccountKey;  // If (changes_mask and $0001)=$0001 then change account key
    new_name: TRawBytes;          // If (changes_mask and $0002)=$0002 then change name
    new_type: Word;               // If (changes_mask and $0004)=$0004 then change type
    new_data: TRawBytes;          // If (changes_mask and $0008)=$0008 then change type
    sign: TECDSA_SIG;
  End;


Const
  CT_TOpListAccountData_NUL : TOpListAccountData = (account_signer:0;account_target:0;operation_type:lat_Unknown;n_operation:0;account_state:as_Unknown;account_price:0;account_to_pay:0;fee:0;
    hash_lock:(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);payload:(payload_type:0;payload_raw:Nil);public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);new_public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;sign:(r:Nil;s:Nil));
  CT_TOpChangeAccountInfoData_NUL : TOpChangeAccountInfoData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:(payload_type:0;payload_raw:Nil);public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);changes_type:[];
    new_accountkey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);new_name:Nil;new_type:0;new_data:Nil;sign:(r:Nil;s:Nil));

Type

  { TOpListAccount }

  TOpListAccount = Class(TPCOperation)
  private
    FData : TOpListAccountData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Property Data : TOpListAccountData read FData;
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;
    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;

  TOpListAccountForSaleOrSwap = Class(TOpListAccount)
  private
    function GetOpSubType : Integer;
  public
    class function OpType : Byte; override;
    Constructor CreateListAccountForSaleOrSwap(ACurrentProtocol : Word; ANewAccountState : TAccountState; AAccountSigner, ANOperation, AAccountTarget: Cardinal; AAccountPrice, AFee: UInt64; AAccountToPay: Cardinal;  ANewPublicKey: TAccountKey; ALockedUntilBlock: Cardinal; AKey: TECPrivateKey; const AHashLock : T32Bytes; const APayload: TOperationPayload);
    property OpSubType : Integer read GetOpSubType;
  End;

  TOpDelistAccountForSale = Class(TOpListAccount)
  public
    class function OpType : Byte; override;
    Constructor CreateDelistAccountForSale(ACurrentProtocol : Word; account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; const payload: TOperationPayload);
  End;

  { TOpBuyAccount }

  TOpBuyAccount = Class(TOpTransaction)
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
  public
    class function OpType : Byte; override;
    Constructor CreateBuy(ACurrentProtocol : Word; account_number, n_operation, account_to_buy, account_to_pay: Cardinal; price, amount, fee : UInt64; new_public_key:TAccountKey; key:TECPrivateKey; const payload: TOperationPayload);
  End;

  { TOpChangeAccountInfo }

  TOpChangeAccountInfo = Class(TPCOperation)
  private
    FData : TOpChangeAccountInfoData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor CreateChangeAccountInfo(ACurrentProtocol : word;
      account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey;
      change_key : Boolean; const new_account_key : TAccountKey;
      change_name: Boolean; const new_name : TRawBytes;
      change_type: Boolean; const new_type : Word;
      change_data: Boolean; const new_data : TRawBytes;
      fee: UInt64; const payload: TOperationPayload);
    Property Data : TOpChangeAccountInfoData read FData;
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;

    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;


  TOpDataData = Record
    account_signer,              // The account paying fees (if any) and signing operation
    account_sender,              // The account sender. Public key must be EQUAL to account_signer public key
    account_target: Cardinal;    // The destination account. Will recive DATA and amount (if any)
    n_operation : Cardinal;      // Signer n_operation
    guid: TGUID;                 // GUID value, added on Protocol V5
    dataType : Word;             // 2 byte data type
    dataSequence : Word;         // 2 byte data sequence
    amount: UInt64;              // Allow amount=0
    fee: UInt64;                 // Allow fee=0
    payload: TOperationPayload;  // Standard arbitrary data with length<256
    sign: TECDSA_SIG;
  End;

  { TOpData }

  TOpData = Class(TPCOperation)
  private
    FData : TOpDataData;
  protected
    procedure InitializeData(AProtocolVersion : Word); override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TOperationPayload; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TOrderedList<Cardinal>); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor CreateOpData( ACurrentProtocol : word; account_signer, account_sender, account_target : Cardinal; signer_key:TECPrivateKey; n_operation : Cardinal; dataType, dataSequence : Word; AGUID : TGUID; amount, fee : UInt64; const payload: TOperationPayload);
    Property Data : TOpDataData read FData;
    Function toString : String; Override;
    Function GetDigestToSign : TRawBytes; override;
    function IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction : TPCSafeBoxTransaction) : Boolean; override;
  End;

Const
  CT_TOpDataData_NUL : TOpDataData = (account_signer:0;account_sender:0;account_target:0;n_operation:0;guid:(D1:0;D2:0;D3:0;D4:(0,0,0,0,0,0,0,0));dataType:0;dataSequence:0;amount:0;fee:0;payload:(payload_type:0;payload_raw:Nil);sign:(r:Nil;s:Nil));

Procedure RegisterOperationsClass;

implementation

uses
  SysUtils, UConst, ULog, UTxMultiOperation;

Procedure RegisterOperationsClass;
Begin
  TPCOperationsComp.RegisterOperationClass(TOpTransaction);
  TPCOperationsComp.RegisterOperationClass(TOpChangeKey);
  TPCOperationsComp.RegisterOperationClass(TOpRecoverFounds);
  TPCOperationsComp.RegisterOperationClass(TOpListAccountForSaleOrSwap);
  TPCOperationsComp.RegisterOperationClass(TOpDelistAccountForSale);
  TPCOperationsComp.RegisterOperationClass(TOpBuyAccount);
  TPCOperationsComp.RegisterOperationClass(TOpChangeKeySigned);
  TPCOperationsComp.RegisterOperationClass(TOpChangeAccountInfo);
  TPCOperationsComp.RegisterOperationClass(TOpMultiOperation);
  TPCOperationsComp.RegisterOperationClass(TOpData);
End;

{ TOpChangeAccountInfo }

procedure TOpChangeAccountInfo.InitializeData(AProtocolVersion : Word);
begin
  inherited InitializeData(AProtocolVersion);
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TOpChangeAccountInfo.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
var LAccount : TAccount;
begin
  if (FData.account_signer<0) or (FData.account_signer>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then Exit(False); // Preventing exception
  LAccount := ASafeBoxTransaction.Account(FData.account_signer);
  Result := IsValidECDSASignature(LAccount.accountInfo.accountkey,FData.sign);
end;

function TOpChangeAccountInfo.SaveOpToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var b : byte;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  SaveOperationPayloadToStream(Stream,FData.payload);
  TStreamOp.WriteAccountKey(Stream,FData.public_key);
  b := 0;
  if (public_key in FData.changes_type) then b:=b OR $01;
  if (account_name in FData.changes_type) then b:=b OR $02;
  if (account_type in FData.changes_type) then b:=b OR $04;
  if (account_data in FData.changes_type) then b:=b OR $08;
  Stream.Write(b,Sizeof(b));
  TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
  TStreamOp.WriteAnsiString(Stream,FData.new_name);
  Stream.Write(FData.new_type,Sizeof(FData.new_type));
  if FProtocolVersion>=CT_PROTOCOL_5 then begin
    TStreamOp.WriteAnsiString(Stream,FData.new_data);
  end;
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpChangeAccountInfo.LoadOpFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var b : Byte;
begin
  Result := False;
  If Stream.Size - Stream.Position < 20 then exit;
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if Not LoadOperationPayloadFromStream(Stream,FData.payload) then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.public_key)<0 then Exit;
  Stream.Read(b,SizeOf(b));
  FData.changes_type:=[];
  if (b AND $01)=$01 then FData.changes_type:=FData.changes_type + [public_key];
  if (b AND $02)=$02 then FData.changes_type:=FData.changes_type + [account_name];
  if (b AND $04)=$04 then FData.changes_type:=FData.changes_type + [account_type];
  if (b AND $08)=$08 then FData.changes_type:=FData.changes_type + [account_data];
  // Check
  if (FProtocolVersion<CT_PROTOCOL_5) and ((b AND $F8)<>0) then Exit;
  if (b AND $F0)<>0 then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.new_accountkey)<0 then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.new_name)<0 then Exit;
  Stream.Read(FData.new_type,Sizeof(FData.new_type));
  if FProtocolVersion>=CT_PROTOCOL_5 then begin
    if TStreamOp.ReadAnsiString(Stream,FData.new_data)<0 then Exit;
  end else FData.new_data := Nil;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then Exit;
  Result := true;
end;

procedure TOpChangeAccountInfo.FillOperationResume(Block: Cardinal; getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  SetLength(OperationResume.Changers,1);
  OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
  OperationResume.Changers[0].Account := FData.account_target;
  OperationResume.Changers[0].Changes_type := FData.changes_type;
  OperationResume.Changers[0].New_Accountkey := FData.new_accountkey;
  OperationResume.Changers[0].New_Name := FData.new_name;
  OperationResume.Changers[0].New_Type := FData.new_type;
  OperationResume.Changers[0].New_Data := FData.new_data;
  If (FData.account_signer=FData.account_target) then begin
    OperationResume.Changers[0].N_Operation := FData.n_operation;
    OperationResume.Changers[0].Signature := FData.sign;
    OperationResume.Changers[0].Fee := FData.fee;
  end else begin
    SetLength(OperationResume.Changers,2);
    OperationResume.Changers[1] := CT_TMultiOpChangeInfo_NUL;
    OperationResume.Changers[1].Account := FData.account_signer;
    OperationResume.Changers[1].N_Operation := FData.n_operation;
    OperationResume.Changers[1].Fee := FData.fee;
    OperationResume.Changers[1].Signature := FData.sign;
  end;
end;

class function TOpChangeAccountInfo.OpType: Byte;
begin
  Result := CT_Op_ChangeAccountInfo;
end;

function TOpChangeAccountInfo.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
begin
  Result:=inherited GetBufferForOpHash(True);
end;

function TOpChangeAccountInfo.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean;
Var account_signer, account_target : TAccount;
  LSafeboxCurrentProtocol : Integer;
begin
  Result := false;
  LSafeboxCurrentProtocol := AccountTransaction.FreezedSafeBox.CurrentProtocol;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account target number';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Target account is blocked for protocol';
      Exit;
    end;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if ((account_signer.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (account_signer.balance<FData.fee) then begin
    errors := 'Insuficient funds';
    exit;
  end;
  if (length(FData.payload.payload_raw)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload.payload_raw))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (LSafeboxCurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;
  If (public_key in FData.changes_type) then begin
    If Not TAccountComp.IsValidAccountKey( FData.new_accountkey, LSafeboxCurrentProtocol, errors) then begin
      exit;
    end;
  end;
  If (account_name in FData.changes_type) then begin
    If (Length(FData.new_name)>0) then begin
      If Not TPCSafeBox.ValidAccountName(FData.new_name,errors) then Exit;
    end;
  end else begin
    If (Length(FData.new_name)>0) then begin
      errors := 'Invalid data in new_name field';
      Exit;
    end;
  end;
  if (account_data in FData.changes_type) then begin
    // TAccount.Data is a 0..32 bytes length
    if (Length(FData.new_data)>CT_MaxAccountDataSize) then begin
      errors := 'New data length ('+IntToStr(Length(FData.new_data))+') > '+IntToStr(CT_MaxAccountDataSize);
      Exit;
    end;
  end else begin
    if Length(FData.new_data)<>0 then begin
      errors := 'New data must be null when no data change';
      Exit;
    end;
  end;
  If (FData.changes_type=[]) then begin
    errors := 'No change';
    Exit;
  end;
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    If Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and target accounts have different public key';
      exit;
    end;
  end;

  // Check signature
  If Not IsValidECDSASignature(account_signer.accountInfo.accountkey,FData.sign) then begin
    errors := 'Invalid ECDSA signature';
    Exit;
  end;

  If (public_key in FData.changes_type) then begin
    account_target.accountInfo.accountKey := FData.new_accountkey;
  end;
  If (account_name in FData.changes_type) then begin
    account_target.name := FData.new_name;
  end;
  If (account_type in FData.changes_type) then begin
    account_target.account_type := FData.new_type;
  end;
  If (account_data in FData.changes_type) then begin
    if LSafeboxCurrentProtocol>=CT_PROTOCOL_5 then begin
      account_target.account_data := FData.new_data
    end else begin
      errors := 'Account Data not available until protocol 5';
      Exit;
    end;
  end;
  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,
         GetOpID,
         FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_data,
         account_target.account_type,
         FData.fee,errors);
end;

function TOpChangeAccountInfo.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpChangeAccountInfo.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpChangeAccountInfo.OperationPayload: TOperationPayload;
begin
  Result := FData.payload;
end;

function TOpChangeAccountInfo.SignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpChangeAccountInfo.DestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpChangeAccountInfo.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

procedure TOpChangeAccountInfo.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.account_signer);
  if (FData.account_target<>FData.account_signer) then list.Add(FData.account_target);
end;

function TOpChangeAccountInfo.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

constructor TOpChangeAccountInfo.CreateChangeAccountInfo(ACurrentProtocol : word;
  account_signer, n_operation,
  account_target: Cardinal; key: TECPrivateKey; change_key: Boolean;
  const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word;
  change_data: Boolean; const new_data : TRawBytes;
  fee: UInt64; const payload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  FData.account_signer:=account_signer;
  FData.account_target:=account_target;
  FData.n_operation:=n_operation;
  FData.fee:=fee;
  FData.payload:=payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key:=key.PublicKey;
  FData.changes_type:=[];
  If change_key then begin
    FData.changes_type:=FData.changes_type + [public_key];
    FData.new_accountkey:=new_account_key;
  end;
  If change_name then begin
    FData.changes_type:=FData.changes_type + [account_name];
    FData.new_name:=new_name;
  end;
  If change_type then begin
    FData.changes_type:=FData.changes_type + [account_type];
    FData.new_type:=new_type;
  end;
  If change_data then begin
    FData.changes_type:=FData.changes_type + [account_data];
    FData.new_data:=new_data;
  end;

  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Change Info operation');
    FHasValidSignature := false;
  end;
end;

function TOpChangeAccountInfo.toString: String;
var s : String;
begin
  s := '';
  If (public_key IN FData.changes_type) then s := 'new public key '+TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID);
  If (account_name IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new name to "'+FData.new_name.ToPrintable+'"';
  end;
  If (account_type IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new type to '+IntToStr(FData.new_type);
  end;
  If (account_data IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new data to '+FData.new_data.ToHexaString;
  end;
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d',[
     TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
     s,
     TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw)]);
end;

function TOpChangeAccountInfo.GetDigestToSign: TRawBytes;
var Stream : TMemoryStream;
  b : Byte;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
    Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
    Stream.Write(FData.fee,Sizeof(FData.fee));
    SaveOperationPayloadToStream(Stream,FData.payload);
    TStreamOp.WriteAccountKey(Stream,FData.public_key);
    b := 0;
    if (public_key in FData.changes_type) then b:=b OR $01;
    if (account_name in FData.changes_type) then b:=b OR $02;
    if (account_type in FData.changes_type) then b:=b OR $04;
    if (account_data in FData.changes_type) then b:=b OR $08;
    Stream.Write(b,Sizeof(b));
    TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
    TStreamOp.WriteAnsiString(Stream,FData.new_name);
    Stream.Write(FData.new_type,Sizeof(FData.new_type));
    if (ProtocolVersion>=CT_PROTOCOL_5) then begin
      TStreamOp.WriteAnsiString(Stream,FData.new_data);
    end;
    if (ProtocolVersion<=CT_PROTOCOL_3) then begin
      Stream.Position := 0;
      setlength(Result,Stream.Size);
      Stream.ReadBuffer(Result[Low(Result)],Stream.Size);
    end else begin
      b := OpType;
      Stream.Write(b,1);
      Result := TCrypto.DoSha256(Stream.Memory,Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

{ TOpTransaction }

procedure TOpTransaction.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.sender);
  list.Add(FData.target);
  if (FData.opTransactionStyle in [transaction_with_auto_buy_account, buy_account, transaction_with_auto_atomic_swap]) then begin
    list.Add(FData.SellerAccount);
  end;
end;

constructor TOpTransaction.CreateTransaction(ACurrentProtocol : Word;
  sender, n_operation, target: Cardinal;
  key: TECPrivateKey; amount, fee: UInt64; const payload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  FData.sender := sender;
  FData.n_operation := n_operation;
  FData.target := target;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Transaction');
    FHasValidSignature := false;
  end;
end;

function TOpTransaction.DoOperation(APrevious : TAccountPreviousBlockInfo; ASafeBoxTransaction : TPCSafeBoxTransaction; var AErrors : String) : Boolean;
Var s_new, t_new : Int64;
  LTotalAmount : Cardinal;
  LSender,LTarget,LSeller : TAccount;
  LRecipientSignable, LIsCoinSwap : Boolean;
  LCurrentBlock, LSafeboxCurrentProtocol : Integer;
  LBuyAccountNewPubkey : TAccountKey;
begin
  Result := false;
  AErrors := '';
  LCurrentBlock := ASafeBoxTransaction.FreezedSafeBox.BlocksCount;
  LSafeboxCurrentProtocol := ASafeboxTransaction.FreezedSafeBox.CurrentProtocol;

  {$region 'Common Validation'}

  if (FData.sender>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then begin
    AErrors := Format('Invalid sender %d',[FData.sender]);
    Exit;
  end;
  if (FData.target>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then begin
    AErrors := Format('Invalid target %d',[FData.target]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.sender,LCurrentBlock) then begin
    AErrors := Format('sender (%d) is blocked for protocol',[FData.sender]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.target,LCurrentBlock) then begin
    AErrors := Format('target (%d) is blocked for protocol',[FData.target]);
    Exit;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    AErrors := Format('Invalid fee %d (max %d)',[FData.fee,CT_MaxTransactionFee]);
    Exit;
  end;
  if (length(FData.payload.payload_raw)>CT_MaxPayloadSize) then begin
    AErrors := 'Invalid Payload size:'+inttostr(length(FData.payload.payload_raw))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (LSafeboxCurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;

  LSender := ASafeBoxTransaction.Account(FData.sender);
  LTarget := ASafeBoxTransaction.Account(FData.target);

  // V5 - Allow recipient-signed OP_BUY operations. This is defined as
  //  - Sender Account = Target Account
  //  - Account (sender = target) is for PRIVATE SALE or ACCOUNT SWAP
  //  - TIME LOCK not expired
  LRecipientSignable :=
    ( FData.opTransactionStyle = buy_Account )
    And (TAccountComp.IsOperationRecipientSignable(LSender, LTarget, LCurrentBlock, LSafeboxCurrentProtocol));

  LIsCoinSwap := TAccountComp.IsAccountForCoinSwap(LTarget.accountInfo)
    And (TAccountComp.IsAccountForSaleOrSwapAcceptingTransactions(LTarget, LCurrentBlock, LSafeboxCurrentProtocol, FData.payload.payload_raw));

  if (FData.sender=FData.target) AND (NOT LRecipientSignable) then begin
    AErrors := Format('Sender=Target and Target is not recipient-signable. Account: %d',[FData.sender]);
    Exit;
  end;
  if (LRecipientSignable Or LIsCoinSwap) then begin
    IF ((FData.amount < 0) or (FData.amount>CT_MaxTransactionAmount)) then begin
      AErrors := Format('Recipient-signed transaction had invalid amount %d. Must be within 0 or %d.)',[FData.amount,CT_MaxTransactionAmount]);
      Exit;
    end
  end else if((FData.amount<=0) Or (FData.amount>CT_MaxTransactionAmount)) then begin
    AErrors := Format('Invalid amount %d (1 or max: %d)',[FData.amount,CT_MaxTransactionAmount]);
    Exit;
  end;
  if ((LSender.n_operation+1)<>FData.n_operation) then begin
    AErrors := Format('Invalid n_operation %d (expected %d)',[FData.n_operation,LSender.n_operation+1]);
    Exit;
  end;
  LTotalAmount := FData.amount + FData.fee;
  if (LSender.balance<LTotalAmount) then begin
    AErrors := Format('Insufficient sender funds %d < (%d + %d = %d)',[LSender.balance,FData.amount,FData.fee,LTotalAmount]);
    Exit;
  end;
  if (LTarget.balance+FData.amount>CT_MaxWalletAmount) then begin
    AErrors := Format('Target cannot accept this transaction due to max amount %d+%d=%d > %d',[LTarget.balance,FData.amount,LTarget.balance+FData.amount,CT_MaxWalletAmount]);
    Exit;
  end;
  // Is locked? Protocol 2 check
  if (NOT LRecipientSignable) AND (TAccountComp.IsAccountLocked(LSender.accountInfo,LCurrentBlock)) then begin
    AErrors := 'Sender Account is currently locked';
    exit;
  end;
  // Build 1.4
  If (NOT TAccountComp.IsNullAccountKey(FData.public_key)) And (NOT TAccountComp.EqualAccountKeys(FData.public_key,LSender.accountInfo.accountkey)) then begin
    AErrors := Format('Invalid sender public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.sender,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(LSender.accountInfo.accountkey))]);
    exit;
  end;
  // Check signature
  if LRecipientSignable AND (NOT IsValidECDSASignature(LSender.accountInfo.new_publicKey, FData.sign)) then begin
    AErrors := 'Invalid recipient-signed ECDSA signature';
    Exit;
  end else If (NOT LRecipientSignable) AND (NOT IsValidECDSASignature(LSender.accountInfo.accountkey, FData.sign)) then begin
    AErrors := 'Invalid ECDSA signature';
    Exit;
  end;

  {$endregion}


  // Is buy account ?
  if (FData.opTransactionStyle = buy_Account ) then begin
    {$region 'Buy Account Validation'}
    if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
      AErrors := 'Buy account is not allowed on Protocol 1';
      exit;
    end;

    if (TAccountComp.IsAccountForCoinSwap(LTarget.accountInfo)) then begin
      AErrors := 'Atomic coin swap cannot be made purchasing, use standard tx instead';
      Exit;
    end;

    if (LSafeboxCurrentProtocol < CT_PROTOCOL_5) then begin
      if (TAccountComp.IsAccountForSwap(LTarget.accountInfo)) then begin
        AErrors := 'Atomic swaps are not allowed until Protocol 5';
        exit;
      end;
    end else begin
      if (Not TAccountComp.IsAccountForPublicSale(LTarget.accountInfo)) then begin
        // On V5 cannot BUY accounts with time-lock EXPIRED  (private sale or Swaps)
        if (Not TAccountComp.IsAccountLocked(LTarget.accountInfo,LCurrentBlock)) then begin
          AErrors := Format('Target %s time lock expired on block %d (Current %d)',
            [TAccountComp.AccountNumberToAccountTxtNumber(LTarget.account),
            LTarget.accountInfo.locked_until_block,
            LCurrentBlock]);
          Exit;
        end;
      end;
    end;

    LSeller := ASafeBoxTransaction.Account(FData.SellerAccount);
    if Not TAccountComp.IsAccountForSaleOrSwap(LTarget.accountInfo) then begin
      AErrors := Format('%d is not for sale or swap',[LTarget.account]);
      exit;
    end;
    // Check that seller is the expected seller
    If (LTarget.accountInfo.account_to_pay<>LSeller.account) then begin
      AErrors := Format('Seller account %d is not expected account %d',[FData.SellerAccount,LTarget.accountInfo.account_to_pay]);
      exit;
    end;

    if (LSender.account = LTarget.account) then begin
      // Self signed operation, amount is not used because has no effect
      if (LSender.balance + FData.fee) < LTarget.accountInfo.price then begin
        AErrors := Format('Self signed Account %d balance (%d) + fee (%d) < target price (%d)',[LTarget.account,LTarget.balance,FData.fee,LTarget.accountInfo.price]);
        exit;
      end;
    end else begin
      if (LTarget.balance + FData.amount) < LTarget.accountInfo.price then begin
        AErrors := Format('Target Account %d balance (%d) + amount (%d) < target price (%d)',[LTarget.account,LTarget.balance,FData.amount,LTarget.accountInfo.price]);
        exit;
      end;
    end;

    if (FData.AccountPrice<>LTarget.accountInfo.price) then begin
      AErrors := Format('Signed price (%d) is not the same of account price (%d)',[FData.AccountPrice,LTarget.accountInfo.price]);
      exit;
    end;
    if NOT TAccountComp.IsValidNewAccountKey(LTarget.accountInfo, FData.new_accountkey, LSafeboxCurrentProtocol) then begin
      AErrors := Format('Specified new public key for %d does not equal (or is not valid) the new public key stored in account: %s <> %s',
      [LTarget.account,
       TAccountComp.AccountKey2RawString(LTarget.accountInfo.new_publicKey).ToHexaString,
        TAccountComp.AccountKey2RawString(FData.new_accountkey).ToHexaString]);
      exit;
    end;

    If Not (TAccountComp.IsValidAccountKey(FData.new_accountkey,LSafeboxCurrentProtocol,AErrors)) then exit;
    LBuyAccountNewPubkey := FData.new_accountkey;
    {$endregion}
  end else if // (is auto buy) OR (is transaction that can buy)
              (
                (FData.opTransactionStyle in [transaction,transaction_with_auto_buy_account,transaction_with_auto_atomic_swap]) AND
                (LSafeboxCurrentProtocol >= CT_PROTOCOL_2) AND
                (TAccountComp.IsAccountForSaleOrSwapAcceptingTransactions(LTarget, LCurrentBlock, LSafeboxCurrentProtocol, FData.payload.payload_raw)) AND
                ((LTarget.balance + FData.amount >= LTarget.accountInfo.price))
              )  then begin
    {$region 'Transaction Auto Buy Validation'}
    if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
      AErrors := 'Tx-Buy account is not allowed on Protocol 1';
      exit;
    end;

    If (LSafeboxCurrentProtocol<CT_PROTOCOL_5) then begin
      if (TAccountComp.IsAccountForSwap( LTarget.accountInfo )) then begin
        AErrors := 'Tx-Buy atomic swaps are not allowed until Protocol 5';
        exit;
      end else begin
        // the below line was a bug fix that introduced a new bug, and is retained here for
        // V2-V4 consistency
        //------
        if Not (TAccountComp.IsValidAccountKey(FData.new_accountkey,LSafeboxCurrentProtocol,AErrors)) then exit;
        //------
      end;
    end;

    // Check that stored "new_publicKey" is valid (when not in coin swap)
    if (Not TAccountComp.IsAccountForCoinSwap(LTarget.accountInfo)) and
       (Not (TAccountComp.IsValidAccountKey(LTarget.accountInfo.new_publicKey,LSafeboxCurrentProtocol,AErrors))) then exit;

    // NOTE: This is a Transaction opereation (not a buy account operation) that
    // has some "added" effects (private sale, swap...)
    // in order to Store at the blockchain file we will fill this fields not specified
    // on a transaction (otherwise JSON-RPC calls will not be abble to know what
    // happened in this transaction as extra effect):
    //
    //  FData.opTransactionStyle: TOpTransactionStyle;
    //  FData.AccountPrice : UInt64;
    //  FData.SellerAccount : Cardinal;
    //  FData.new_accountkey : TAccountKey;

    // Fill the purchase data
    if TAccountComp.IsAccountForSale( LTarget.accountInfo ) then begin
      FData.opTransactionStyle := transaction_with_auto_buy_account; // Set this data!
    end else begin
      FData.opTransactionStyle := transaction_with_auto_atomic_swap; // Set this data!
    end;
    FData.AccountPrice := LTarget.accountInfo.price;
    FData.SellerAccount := LTarget.accountInfo.account_to_pay;
    LSeller := ASafeBoxTransaction.Account(LTarget.accountInfo.account_to_pay);
    if TAccountComp.IsAccountForCoinSwap( LTarget.accountInfo ) then begin
      // We will save extra info that account key has not changed
      FData.new_accountkey := CT_TECDSA_Public_Nul;
      // COIN SWAP: Ensure public key will not change and will be the same
      LBuyAccountNewPubkey := LTarget.accountInfo.accountKey;
    end else begin
      FData.new_accountkey := LTarget.accountInfo.new_publicKey;
      LBuyAccountNewPubkey := LTarget.accountInfo.new_publicKey;
    end;
    {$endregion}
  end else if (FData.opTransactionStyle <> transaction) then begin
     AErrors := 'INTERNAL ERROR: 477C2A3C53C34E63A6B82C057741C44D';
     exit;
  end;

  if (FData.opTransactionStyle in [buy_account, transaction_with_auto_buy_account, transaction_with_auto_atomic_swap]) then begin
    // account purchase
    if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
      AErrors := 'NOT ALLOWED ON PROTOCOL 1';
      exit;
    end;

    if (LTarget.accountInfo.state in [as_ForAtomicAccountSwap, as_ForAtomicCoinSwap]) AND
       (LSafeboxCurrentProtocol<CT_PROTOCOL_5) then begin
      AErrors := 'NOT ALLOWED UNTIL PROTOCOL 5';
      exit;
    end;

    Result := ASafeBoxTransaction.BuyAccount(
      APrevious,
      GetOpID,
      LSender.account,
      LTarget.account,
      LSeller.account,
      FData.n_operation,
      FData.amount,
      LTarget.accountInfo.price,
      FData.fee,
      LBuyAccountNewPubkey,
      FData.payload.payload_raw,
      LRecipientSignable,
      AErrors
    );

  end else begin
    // Standard transaction
    Result := ASafeBoxTransaction.TransferAmount(
      APrevious,
      GetOpID,
      FData.sender,
      FData.sender,
      FData.target,
      FData.n_operation,
      FData.amount,
      FData.fee,AErrors
    );
  end;
end;

function TOpTransaction.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
Var ms : TMemoryStream;
begin
  If UseProtocolV2 then Result := inherited GetBufferForOpHash(UseProtocolV2)
  else begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.sender,Sizeof(FData.sender));
      ms.Write(FData.n_operation,Sizeof(FData.n_operation));
      ms.Write(FData.target,Sizeof(FData.target));
      ms.Write(FData.amount,Sizeof(FData.amount));
      ms.Write(FData.fee,Sizeof(FData.fee));
      if Length(FData.payload.payload_raw)>0 then
        ms.WriteBuffer(FData.payload.payload_raw[Low(FData.payload.payload_raw)],Length(FData.payload.payload_raw));
      ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
      if Length(FData.public_key.x)>0 then
        ms.WriteBuffer(FData.public_key.x[Low(FData.public_key.x)],Length(FData.public_key.x));
      if Length(FData.public_key.y)>0 then
        ms.WriteBuffer(FData.public_key.y[Low(FData.public_key.y)],Length(FData.public_key.y));
      if Length(FData.sign.r)>0 then
        ms.WriteBuffer(FData.sign.r[Low(FData.sign.r)],Length(FData.sign.r));
      if Length(FData.sign.s)>0 then
        ms.WriteBuffer(FData.sign.s[Low(FData.sign.s)],Length(FData.sign.s));
      SetLength(Result,ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TOpTransaction.InitializeData;
begin
  inherited;
  FData := CT_TOpTransactionData_NUL;
end;

function TOpTransaction.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
var LAccount : TAccount;
begin
  if (FData.sender<0) or (FData.sender>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then Exit(False); // Preventing exception
  LAccount := ASafeBoxTransaction.Account(FData.sender);
  Result := IsValidECDSASignature(LAccount.accountInfo.accountkey,FData.sign);
end;

function TOpTransaction.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var b : Byte;
begin
  Result := false;
  if Stream.Size-Stream.Position < 28  then exit; // Invalid stream
  Stream.Read(FData.sender,Sizeof(FData.sender));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.target,Sizeof(FData.target));
  Stream.Read(FData.amount,Sizeof(FData.amount));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if Not LoadOperationPayloadFromStream(Stream,FData.payload) then Exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
  if ((LoadExtendedData) Or (Self is TOpBuyAccount)) then begin
    If Stream.Read(b,1)<>1 then begin
      exit;
    end;
    case b of
      0 : FData.opTransactionStyle := transaction;
      1 : FData.opTransactionStyle := transaction_with_auto_buy_account;
      2 : Begin
        FData.opTransactionStyle := buy_account;
        if (Not (Self is TOpBuyAccount)) then exit;
      End;
      3 : FData.opTransactionStyle := transaction_with_auto_atomic_swap;
    else exit;
    end;
    if ((Self is TOpBuyAccount) and (FData.opTransactionStyle<>buy_account)) or
       ((Not (Self is TOpBuyAccount)) and (FData.opTransactionStyle=buy_account)) then begin
      // Protection invalid case added 20190705
      Exit;
    end;
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,buy_account,transaction_with_auto_atomic_swap]) then begin
      Stream.Read(FData.AccountPrice,SizeOf(FData.AccountPrice));
      Stream.Read(FData.SellerAccount,SizeOf(FData.SellerAccount));
      if Stream.Read(FData.new_accountkey.EC_OpenSSL_NID,Sizeof(FData.new_accountkey.EC_OpenSSL_NID))<0 then exit;
      if TStreamOp.ReadAnsiString(Stream,FData.new_accountkey.x)<0 then exit;
      if TStreamOp.ReadAnsiString(Stream,FData.new_accountkey.y)<0 then exit;
    end;
  end;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

procedure TOpTransaction.FillOperationResume(Block: Cardinal; getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  SetLength(OperationResume.Senders,1);
  OperationResume.Senders[0] := CT_TMultiOpSender_NUL;
  OperationResume.Senders[0].Account:=FData.sender;
  OperationResume.Senders[0].Amount:=Int64(FData.amount + FData.fee);
  OperationResume.Senders[0].N_Operation:=FData.n_operation;
  OperationResume.Senders[0].Payload:=FData.payload;
  OperationResume.Senders[0].Signature:=FData.sign;
  case FData.opTransactionStyle of
    transaction : begin
      SetLength(OperationResume.Receivers,1);
      OperationResume.Receivers[0] := CT_TMultiOpReceiver_NUL;
      OperationResume.Receivers[0].Account:=FData.target;
      OperationResume.Receivers[0].Amount:=FData.amount;
      OperationResume.Receivers[0].Payload:=FData.payload;
    end;
    buy_account, transaction_with_auto_buy_account, transaction_with_auto_atomic_swap : begin
      SetLength(OperationResume.Receivers,2);
      OperationResume.Receivers[0] := CT_TMultiOpReceiver_NUL;
      OperationResume.Receivers[0].Account:=FData.target;
      OperationResume.Receivers[0].Amount:= (FData.amount - FData.AccountPrice);
      OperationResume.Receivers[0].Payload:=FData.payload;
      OperationResume.Receivers[1] := CT_TMultiOpReceiver_NUL;
      OperationResume.Receivers[1].Account:=FData.SellerAccount;
      OperationResume.Receivers[1].Amount:= FData.AccountPrice;
      OperationResume.Receivers[1].Payload:=FData.payload;
      if (Not TAccountComp.IsNullAccountKey(FData.new_accountkey)) then begin
        SetLength(OperationResume.Changers,1);
        OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
        OperationResume.Changers[0].Account := FData.target;
        OperationResume.Changers[0].Changes_type := [public_key];
        OperationResume.Changers[0].New_Accountkey := FData.new_accountkey;
      end;
    end;
  end;
end;

function TOpTransaction.OperationAmount: Int64;
begin
  Result := FData.amount;
end;

function TOpTransaction.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpTransaction.OperationPayload: TOperationPayload;
begin
  Result := FData.payload;
end;

class function TOpTransaction.OpType: Byte;
begin
  Result := CT_Op_Transaction;
end;

function TOpTransaction.SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
Var b : Byte;
begin
  Stream.Write(FData.sender,Sizeof(FData.sender));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.target,Sizeof(FData.target));
  Stream.Write(FData.amount,Sizeof(FData.amount));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  SaveOperationPayloadToStream(Stream,FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
  if ((SaveExtendedData) Or (Self is TOpBuyAccount)) then begin
    case FData.opTransactionStyle of
      transaction : b:=0;
      transaction_with_auto_buy_account : b:=1;
      buy_account : b:=2;
      transaction_with_auto_atomic_swap : b:=3;
    else raise Exception.Create('ERROR DEV 20170424-1');
    end;
    Stream.Write(b,1);
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,buy_account,transaction_with_auto_atomic_swap]) then begin
      Stream.Write(FData.AccountPrice,SizeOf(FData.AccountPrice));
      Stream.Write(FData.SellerAccount,SizeOf(FData.SellerAccount));
      Stream.Write(FData.new_accountkey.EC_OpenSSL_NID,Sizeof(FData.new_accountkey.EC_OpenSSL_NID));
      TStreamOp.WriteAnsiString(Stream,FData.new_accountkey.x);
      TStreamOp.WriteAnsiString(Stream,FData.new_accountkey.y);
    end;
  end;
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpTransaction.SignerAccount: Cardinal;
begin
  Result := FData.sender;
end;

function TOpTransaction.DestinationAccount: Int64;
begin
  Result:=FData.target;
end;

function TOpTransaction.SellerAccount: Int64;
begin
  Case FData.opTransactionStyle of
    transaction_with_auto_buy_account, buy_account, transaction_with_auto_atomic_swap : Result := FData.SellerAccount;
  else Result:=inherited SellerAccount;
  end;
end;

function TOpTransaction.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpTransaction.OperationAmountByAccount(account: Cardinal): Int64;
begin
  Result := 0;
  If (FData.sender = account) then inc(Result, Int64(FData.amount+FData.fee) * (-1));
  If (FData.target = account) then begin
    if (FData.opTransactionStyle in [buy_account,transaction_with_auto_buy_account]) then inc(Result, FData.amount - FData.AccountPrice)
    else inc(Result,FData.amount);
  end;
  If ((FData.SellerAccount = account) And (FData.opTransactionStyle in [buy_account,transaction_with_auto_buy_account] )) then begin
    inc(Result, FData.AccountPrice)
  end;
end;

function TOpTransaction.toString: String;
begin
  case FData.opTransactionStyle of
    transaction :
      Result := Format('Transaction from %s to %s amount:%s fee:%s (n_op:%d) payload size:%d payload:%s',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw),
         TCrypto.ToHexaString(FData.payload.payload_raw)]);
    transaction_with_auto_buy_account :
      Result := Format('Transaction/Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d payload:%s',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.FormatMoney(FData.AccountPrice), TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw),
         TCrypto.ToHexaString(FData.payload.payload_raw)]);
    buy_account :
      Result := Format('Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d payload:%s',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.FormatMoney(FData.AccountPrice), TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw),
         TCrypto.ToHexaString(FData.payload.payload_raw)]);
    transaction_with_auto_atomic_swap :
      Result := Format('Transaction/Swap account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d payload:%s',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.FormatMoney(FData.AccountPrice), TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw),
         TCrypto.ToHexaString(FData.payload.payload_raw)]);
  else raise Exception.Create('ERROR DEV 20170424-2');
  end;
end;

function TOpTransaction.GetDigestToSign: TRawBytes;
Var ms : TMemoryStream;
  b : Byte;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.sender,Sizeof(FData.sender));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.target,Sizeof(FData.target));
    ms.Write(FData.amount,Sizeof(FData.amount));
    ms.Write(FData.fee,Sizeof(FData.fee));
    if ProtocolVersion>=CT_PROTOCOL_5 then begin
      ms.Write(FData.payload.payload_type,SizeOf(FData.payload.payload_type));
    end;
    if Length(FData.payload.payload_raw)>0 then
      ms.WriteBuffer(FData.payload.payload_raw[Low(FData.payload.payload_raw)],Length(FData.payload.payload_raw));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if Length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[Low(FData.public_key.x)],Length(FData.public_key.x));
    if Length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[Low(FData.public_key.y)],Length(FData.public_key.y));
    if FData.opTransactionStyle=buy_account then begin
      ms.Write(FData.AccountPrice,Sizeof(FData.AccountPrice));
      ms.Write(FData.SellerAccount,Sizeof(FData.SellerAccount));
      ms.Write(FData.new_accountkey.EC_OpenSSL_NID,Sizeof(FData.new_accountkey.EC_OpenSSL_NID));
      if Length(FData.new_accountkey.x)>0 then
        ms.WriteBuffer(FData.new_accountkey.x[Low(FData.new_accountkey.x)],Length(FData.new_accountkey.x));
      if Length(FData.new_accountkey.y)>0 then
        ms.WriteBuffer(FData.new_accountkey.y[Low(FData.new_accountkey.y)],Length(FData.new_accountkey.y));
    end;
    if (ProtocolVersion<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    end else begin
      b := OpType;
      ms.Write(b,1);
      Result := TCrypto.DoSha256(ms.Memory,ms.Size);
    end;
  finally
    ms.Free;
  end;
end;

{ TOpChangeKey }

procedure TOpChangeKey.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.account_signer);
  if (FData.account_target<>FData.account_signer) then list.Add(FData.account_target);
end;

function TOpChangeKey.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

constructor TOpChangeKey.Create(ACurrentProtocol : Word; account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; const payload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  If (OpType=CT_Op_Changekey) then begin
    If (account_signer<>account_target) then Raise Exception.Create('ERROR DEV 20170530-4');
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    // Allowed signer<>target
  end else Raise Exception.Create('ERROR DEV 20170530-5');
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_accountkey := new_account_key;
  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Change key');
    FHasValidSignature := false;
  end;
end;

function TOpChangeKey.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean;
Var account_signer, account_target : TAccount;
  LSafeboxCurrentProtocol : Integer;
begin
  Result := false;
  LSafeboxCurrentProtocol := AccountTransaction.FreezedSafeBox.CurrentProtocol;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account target number';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Target account is blocked for protocol';
      Exit;
    end;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if ((account_signer.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (account_signer.balance<FData.fee) then begin
    errors := 'Insuficient funds';
    exit;
  end;
  if (length(FData.payload.payload_raw)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload.payload_raw))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (LSafeboxCurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  If Not TAccountComp.IsValidAccountKey( FData.new_accountkey,LSafeboxCurrentProtocol,errors) then begin
    exit;
  end;
  // NEW v2 protocol protection: Does not allow to change key for same key
  if (LSafeboxCurrentProtocol>=CT_PROTOCOL_2) then begin
    if (TAccountComp.EqualAccountKeys(account_target.accountInfo.accountKey,FData.new_accountkey)) then begin
      errors := 'New public key is the same public key';
      exit;
    end;
  end;
  // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;
  If (FData.account_signer<>FData.account_target) then begin
    if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Account target is currently locked';
      exit;
    end;
    // Check have same public key
    If Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and target accounts have different public key';
      exit;
    end;
    if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      exit;
    end;
  end;

  // Check signature
  If Not IsValidECDSASignature(account_signer.accountInfo.accountkey,FData.sign) then begin
    errors := 'Invalid ECDSA signature';
    Exit;
  end;

  account_target.accountInfo.accountKey := FData.new_accountkey;
  // Set to normal:
  account_target.accountInfo.state := as_Normal;
  account_target.accountInfo.locked_until_block := 0;
  account_target.accountInfo.price := 0;
  account_target.accountInfo.account_to_pay := 0;
  account_target.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,
         GetOpID,
         FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_data,
         account_target.account_type,
         FData.fee,errors);
end;

function TOpChangeKey.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
var ms : TMemoryStream;
  raw : TRawBytes;
begin
  If UseProtocolV2 then Result := inherited GetBufferForOpHash(UseProtocolV2)
  else begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account_signer,Sizeof(FData.account_signer)); //Protocol 1 does not allow signer/target. signer=target always
      ms.Write(FData.n_operation,Sizeof(FData.n_operation));
      ms.Write(FData.fee,Sizeof(FData.fee));
      if Length(FData.payload.payload_raw)>0 then
        ms.WriteBuffer(FData.payload.payload_raw[Low(FData.payload.payload_raw)],Length(FData.payload.payload_raw));
      ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
      if Length(FData.public_key.x)>0 then
        ms.WriteBuffer(FData.public_key.x[Low(FData.public_key.x)],Length(FData.public_key.x));
      if Length(FData.public_key.y)>0 then
        ms.WriteBuffer(FData.public_key.y[Low(FData.public_key.y)],Length(FData.public_key.y));
      raw := TAccountComp.AccountKey2RawString(FData.new_accountkey);
      if Length(raw)>0 then
        ms.WriteBuffer(raw[Low(raw)],Length(raw));
      if Length(FData.sign.r)>0 then
        ms.WriteBuffer(FData.sign.r[Low(FData.sign.r)],Length(FData.sign.r));
      if Length(FData.sign.s)>0 then
        ms.WriteBuffer(FData.sign.s[Low(FData.sign.s)],Length(FData.sign.s));
      ms.Position := 0;
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TOpChangeKey.InitializeData(AProtocolVersion : Word);
begin
  inherited InitializeData(AProtocolVersion);
  FData := CT_TOpChangeKeyData_NUL;
end;

function TOpChangeKey.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
var LAccount : TAccount;
begin
  if (FData.account_signer<0) or (FData.account_signer>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then Exit(False); // Preventing exception
  LAccount := ASafeBoxTransaction.Account(FData.account_signer);
  Result := IsValidECDSASignature(LAccount.accountInfo.accountkey,FData.sign);
end;

function TOpChangeKey.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var raw : TRawBytes;
begin
  Result := false;
  if Stream.Size-Stream.Position < 16  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  If (OpType=CT_Op_ChangeKey) then begin
    FData.account_target:=FData.account_signer;
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    Stream.Read(FData.account_target,Sizeof(FData.account_target));
  end else Raise Exception.Create('ERROR DEV 20170530-1');
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if Not LoadOperationPayloadFromStream(Stream,FData.payload) then Exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,raw)<0 then exit;
  FData.new_accountkey := TAccountComp.RawString2Accountkey(raw);
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

procedure TOpChangeKey.FillOperationResume(Block: Cardinal; getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  SetLength(OperationResume.Changers,1);
  OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
  OperationResume.Changers[0].Account := FData.account_target;
  OperationResume.Changers[0].Changes_type := [public_key];
  OperationResume.Changers[0].New_Accountkey := FData.new_accountkey;
  if (FData.account_signer=FData.account_target) then begin
    OperationResume.Changers[0].N_Operation := FData.n_operation;
    OperationResume.Changers[0].Fee := FData.fee;
    OperationResume.Changers[0].Signature := FData.sign;
  end else begin
    SetLength(OperationResume.Changers,2);
    OperationResume.Changers[1] := CT_TMultiOpChangeInfo_NUL;
    OperationResume.Changers[1].Account := FData.account_signer;
    OperationResume.Changers[1].N_Operation := FData.n_operation;
    OperationResume.Changers[1].Fee := FData.fee;
    OperationResume.Changers[1].Signature := FData.sign;
  end;
end;

function TOpChangeKey.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpChangeKey.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpChangeKey.OperationPayload: TOperationPayload;
begin
  Result := FData.payload;
end;

class function TOpChangeKey.OpType: Byte;
begin
  Result := CT_Op_Changekey;
end;

function TOpChangeKey.SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  If (OpType=CT_Op_ChangeKey) then begin
    If FData.account_target<>FData.account_signer then Raise Exception.Create('ERROR DEV 20170530-2');
  end else if (OpType=CT_Op_ChangeKeySigned) then begin
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
  end else Raise Exception.Create('ERROR DEV 20170530-3');
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  SaveOperationPayloadToStream(Stream,FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
  TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(FData.new_accountkey));
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpChangeKey.SignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpChangeKey.DestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpChangeKey.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpChangeKey.toString: String;
begin
  Result := Format('Change key of %s to new key: %s fee:%s (n_op:%d) payload size:%d',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
    TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID),
    TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload.payload_raw)]);
end;

function TOpChangeKey.GetDigestToSign: TRawBytes;
var ms : TMemoryStream;
  raw : TRawBytes;
  b : Byte;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.account_signer,Sizeof(FData.account_signer));
    if (FData.account_signer<>FData.account_target) then ms.Write(FData.account_target,Sizeof(FData.account_target));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.fee,Sizeof(FData.fee));
    if ProtocolVersion>=CT_PROTOCOL_5 then begin
      ms.Write(FData.payload.payload_type,SizeOf(FData.payload.payload_type));
    end;
    if Length(FData.payload.payload_raw)>0 then
      ms.WriteBuffer(FData.payload.payload_raw[Low(FData.payload.payload_raw)],Length(FData.payload.payload_raw));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if Length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[Low(FData.public_key.x)],Length(FData.public_key.x));
    if Length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[Low(FData.public_key.y)],Length(FData.public_key.y));
    raw := TAccountComp.AccountKey2RawString(FData.new_accountkey);
    if Length(raw)>0 then
      ms.WriteBuffer(raw[Low(raw)],Length(raw));
    if (ProtocolVersion<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    end else begin
      b := OpType;
      ms.Write(b,1);
      Result := TCrypto.DoSha256(ms.Memory,ms.Size);
    end;
  finally
    ms.Free;
  end;
end;

{ TOpChangeKeySigned }

class function TOpChangeKeySigned.OpType: Byte;
begin
  Result:=CT_Op_ChangeKeySigned;
end;

{ TOpRecoverFounds }

procedure TOpRecoverFounds.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.account);
end;

constructor TOpRecoverFounds.Create(ACurrentProtocol : word; account_number, n_operation : Cardinal; fee: UInt64; new_accountkey : TAccountKey);
begin
  inherited Create(ACurrentProtocol);
  FData.account := account_number;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.new_accountkey := new_accountkey;
  FHasValidSignature := true; // Recover founds doesn't need a signature
end;

function TOpRecoverFounds.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean;
Var acc : TAccount;
  LSafeboxCurrentProtocol : Integer;
begin
  Result := false;
  LSafeboxCurrentProtocol := AccountTransaction.FreezedSafeBox.CurrentProtocol;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  acc := AccountTransaction.Account(FData.account);
  if TAccountComp.IsAccountLocked(acc.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is locked';
    Exit;
  end;
  if (acc.updated_on_block_active_mode + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('Account is active to recover founds! Account %d Updated %d + %d >= BlockCount : %d',[FData.account,acc.updated_on_block_active_mode,CT_RecoverFoundsWaitInactiveCount,AccountTransaction.FreezedSafeBox.BlocksCount]);
    Exit;
  end;
  if (TAccountComp.AccountBlock(FData.account) + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('AccountBlock is active to recover founds! AccountBlock %d + %d >= BlockCount : %d',[TAccountComp.AccountBlock(FData.account),CT_RecoverFoundsWaitInactiveCount,AccountTransaction.FreezedSafeBox.BlocksCount]);
    Exit;
  end;
  if ((acc.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee '+Inttostr(FData.fee);
    exit;
  end;
  if (acc.balance<FData.fee) then begin
    errors := 'Insuficient funds';
    exit;
  end;
  if Not TAccountComp.IsValidAccountKey(FData.new_accountkey,LSafeboxCurrentProtocol,errors) then begin
    Exit;
  end;

  // Poll on Discord
  // https://discordapp.com/channels/383064643482025984/391780165669093377/719437469329915945
  // RESULTS ON 2020-07-21
  // 1 (22 votes) - Remove PASC/PASA Recovery rule
  // 2 (27 votes) - Recover only EMPTY non used, not named PASA's
  // 3 (3 votes) - Change Recovery to 10 year rule
  // 4 (2 votes) - Leave As Is.
  // ----------
  // Winner option 2: Will apply on next Hard Fork (Protocol 6)
  if (LSafeboxCurrentProtocol>CT_PROTOCOL_5) then begin
    if (acc.balance>0) or (Length(acc.name)>0) then begin
      errors := 'Recover account is only valid for Balance 0 and unnamed accounts';
      exit;
    end;
  end;

  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,
    GetOpID,
    FData.account,FData.n_operation, FData.account,
    TAccountComp.RawString2AccountInfo( TAccountComp.AccountKey2RawString(FData.new_accountkey) ),
    acc.name, acc.account_data, acc.account_type, FData.fee, errors);
end;

function TOpRecoverFounds.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
var ms : TMemoryStream;
begin
  If UseProtocolV2 then Result := inherited GetBufferForOpHash(UseProtocolV2)
  else begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account,Sizeof(FData.account));
      ms.Write(FData.n_operation,Sizeof(FData.n_operation));
      ms.Write(FData.fee,Sizeof(FData.fee));
      TStreamOp.WriteAccountKey(ms,FData.new_accountkey);
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TOpRecoverFounds.InitializeData;
begin
  inherited;
  FData := CT_TOpRecoverFoundsData_NUL;
end;

function TOpRecoverFounds.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
begin
  Result := True; // Nobody signs here
end;

function TOpRecoverFounds.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
begin
  Result := false;
  if Stream.Size - Stream.Position<16 then exit;
  Stream.Read(FData.account,Sizeof(FData.account));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAccountKey(Stream,FData.new_accountkey)<0 then Exit;
  Result := true;
end;

procedure TOpRecoverFounds.FillOperationResume(Block: Cardinal; getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  SetLength(OperationResume.Changers,1);
  OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
  OperationResume.Changers[0].Account := FData.account;
  OperationResume.Changers[0].Fee := FData.fee;
  OperationResume.Changers[0].N_Operation := FData.n_operation;
  OperationResume.Changers[0].Changes_type := [public_key];
  OperationResume.Changers[0].New_Accountkey := FData.new_accountkey;
end;

function TOpRecoverFounds.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpRecoverFounds.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpRecoverFounds.OperationPayload: TOperationPayload;
begin
  Result := CT_TOperationPayload_NUL;
end;

class function TOpRecoverFounds.OpType: Byte;
begin
  Result := CT_Op_Recover;
end;

function TOpRecoverFounds.SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
begin
  Stream.Write(FData.account,Sizeof(FData.account));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
  Result := true;
end;

function TOpRecoverFounds.SignerAccount: Cardinal;
begin
  Result := FData.account;
end;

function TOpRecoverFounds.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpRecoverFounds.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

function TOpRecoverFounds.toString: String;
begin
  Result := Format('Recover account %s fee:%s (n_op:%d) set Public key to %s',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account),
    TAccountComp.FormatMoney(FData.fee),fData.n_operation,
    TAccountComp.AccountKey2RawString(FData.new_accountkey).ToHexaString]);
end;

function TOpRecoverFounds.GetDigestToSign: TRawBytes;
begin
  SetLength(Result,0); // Nothing to be signed!
end;

{ TOpListAccount }

procedure TOpListAccount.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.account_signer);
  if FData.account_signer<>FData.account_target then
    list.Add(FData.account_target);
end;

function TOpListAccount.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

function TOpListAccount.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : String) : Boolean;
Var
  account_signer, account_target : TAccount;
  LIsDelist, LIsSale, LIsPrivateSale, LIsPublicSale, LIsSwap, LIsAccountSwap, LIsCoinSwap : boolean;
  LSafeboxCurrentProtocol : Integer;
begin
  Result := false;
  // Determine which flow this function will execute
  LIsDelist := OpType = CT_Op_DelistAccount;
  LIsSale := (OpType = CT_Op_ListAccountForSale) AND (FData.account_state = as_ForSale);
  if LIsSale then begin
    if (FData.account_state = as_ForSale) and (FData.new_public_key.EC_OpenSSL_NID <> CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
      LIsPublicSale := false;
      LIsPrivateSale := true;
    end else begin
      LIsPublicSale := true;
      LIsPrivateSale := false;
    end;
  end else begin
    LIsPublicSale := false;
    LIsPrivateSale := false;
  end;
  LIsSwap := (OpType = CT_Op_ListAccountForSale) AND (FData.account_state in [as_ForAtomicAccountSwap, as_ForAtomicCoinSwap]);
  if LIsSwap then begin
    if FData.account_state =  as_ForAtomicCoinSwap then begin
      LIsAccountSwap := false;
      LIsCoinSwap := true;
    end else begin
      LIsAccountSwap := true;
      LIsCoinSwap := false;
    end;
  end else begin
    LIsAccountSwap := false;
    LIsCoinSwap := false;
  end;

  LSafeboxCurrentProtocol := AccountTransaction.FreezedSafeBox.CurrentProtocol;
  if (LSafeboxCurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'List/Delist Account is not allowed on Protocol 1';
    exit;
  end;
  if LIsSwap AND (LSafeboxCurrentProtocol<CT_PROTOCOL_5) then begin
    errors := 'Atomic Swaps are not allowed before Protocol 5';
    exit;
  end;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid signer account number';
    Exit;
  end;
  if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid target account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Signer account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Target account is blocked for protocol';
    Exit;
  end;

  if NOT LIsDelist then begin
    if (FData.account_to_pay>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account to pay number';
      Exit;
    end;
    if (FData.account_target = FData.account_to_pay) AND NOT (LIsAccountSwap AND (FData.account_price = 0))  then begin
      // Note: atomic account swap with 0 sale price can have tself as seller
      errors := 'Account to pay is itself';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_to_pay, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Account to pay is blocked for protocol';
      Exit;
    end;
    if (NOT LIsAccountSwap) AND (FData.account_price<=0) then begin
      if (LIsSale) then errors := 'Account price must be greater than 0'
      else errors := 'Account swap amount must be greater than 0';
      exit;
    end;

    if (LIsAccountSwap) and (FData.account_price<>0) then begin
      errors := 'Account for Account Swap must have 0 price';
      Exit;
    end;


    if (FData.locked_until_block > (AccountTransaction.FreezedSafeBox.BlocksCount + CT_MaxFutureBlocksLockedAccount)) then begin
      errors := 'Invalid locked block: Current block '+Inttostr(AccountTransaction.FreezedSafeBox.BlocksCount)+' cannot lock to block '+IntToStr(FData.locked_until_block)+' (Max is future '+IntToStr(CT_MaxFutureBlocksLockedAccount)+' blocks)';
      exit;
    end;
    if LIsPrivateSale OR LIsAccountSwap then begin
      If Not TAccountComp.IsValidAccountKey( FData.new_public_key,LSafeboxCurrentProtocol,errors) then begin
        errors := 'Invalid new public key: '+errors;
        exit;
      end;
    end else if (LSafeboxCurrentProtocol>=CT_PROTOCOL_5) then begin
      // COIN SWAP or PUBLIC SALE must set FData.new_public_key to NULL
      if Not TAccountComp.IsNullAccountKey(FData.new_public_key) then begin
        errors := 'Coin swap/Public sale needs a NULL new public key';
        Exit;
      end;
    end;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account_signer := AccountTransaction.Account(FData.account_signer);
  account_target := AccountTransaction.Account(FData.account_target);
  if (FData.account_signer<>FData.account_target) then begin
    // Both accounts must have same PUBLIC KEY!
    if Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey) then begin
      errors := 'Signer and affected accounts have different public key';
      Exit;
    end;
  end;

  if ((account_signer.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (account_signer.balance<FData.fee) then begin
    errors := 'Insuficient funds';
    exit;
  end;
  if (length(FData.payload.payload_raw)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload.payload_raw))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    Exit;
  end;
  // Is locked?
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Signer account is currently locked';
    exit;
  end;
  if (TAccountComp.IsAccountLocked(account_target.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Target account is currently locked';
    exit;
  end;
  if LIsPrivateSale OR LIsAccountSwap then begin
    // NOTE: Atomic coin swap avoids this validation since it is a transfer
    // of account to same owner, paying counter-party in seller field
    if TAccountComp.EqualAccountKeys(account_target.accountInfo.accountKey,FData.new_public_key) then begin
      errors := 'New public key for private sale is the same public key';
      Exit;
    end;
  end else if LIsCoinSwap then begin
    if (account_target.balance < (FData.account_price + FData.fee)) then begin
      errors := 'Not enough funds for coin swap amount and fee';
      exit;
    end;
  end;

   // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;

  // Check signature
  If Not IsValidECDSASignature(account_signer.accountInfo.accountkey,FData.sign) then begin
    errors := 'Invalid ECDSA signature';
    Exit;
  end;

  if LIsDelist then begin
    account_target.accountInfo.state := as_Normal;
    account_target.accountInfo.locked_until_block := CT_AccountInfo_NUL.locked_until_block;
    account_target.accountInfo.price := CT_AccountInfo_NUL.price;
    account_target.accountInfo.account_to_pay := CT_AccountInfo_NUL.account_to_pay;
    account_target.accountInfo.new_publicKey := CT_AccountInfo_NUL.new_publicKey;
    account_target.accountInfo.hashed_secret := CT_AccountInfo_NUL.hashed_secret;
  end else begin
    account_target.accountInfo.state := FData.account_state;
    account_target.accountInfo.locked_until_block := FData.locked_until_block;
    account_target.accountInfo.price := FData.account_price;
    account_target.accountInfo.account_to_pay := FData.account_to_pay;
    account_target.accountInfo.new_publicKey := FData.new_public_key;
    if LIsSwap then begin
      account_target.accountInfo.hashed_secret := TBaseType.ToRawBytes( FData.hash_lock );
    end else begin
      // FData.hash_lock has no utility when no AtomicSwap
      account_target.accountInfo.hashed_secret := CT_AccountInfo_NUL.hashed_secret;
    end;
  end;

  Result := AccountTransaction.UpdateAccountInfo(
    AccountPreviousUpdatedBlock,
    GetOpID,
    FData.account_signer,
    FData.n_operation,
    FData.account_target,
    account_target.accountInfo,
    account_target.name,
    account_target.account_data,
    account_target.account_type,
    FData.fee,
    errors
  );
end;

function TOpListAccount.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
begin
  // This Operation is new from protocol V2, so we cannot hash it as a previous protocol!
  Result := inherited GetBufferForOpHash(true);
end;

procedure TOpListAccount.InitializeData;
begin
  inherited;
  FData := CT_TOpListAccountData_NUL;
end;

function TOpListAccount.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
var LAccount : TAccount;
begin
  if (FData.account_signer<0) or (FData.account_signer>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then Exit(False); // Preventing exception
  LAccount := ASafeBoxTransaction.Account(FData.account_signer);
  Result := IsValidECDSASignature(LAccount.accountInfo.accountkey,FData.sign);
end;

function TOpListAccount.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var raw : TRawBytes;
  w : Word;
begin
  Result := false;
  if Stream.Size-Stream.Position < 14  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(w,2);
  case w of
    CT_Op_ListAccountForSale : FData.operation_type := lat_ListAccount;
    CT_Op_DelistAccount : FData.operation_type := lat_DelistAccount;
    else exit; // Invalid data info
  end;
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  if (FData.operation_type in [lat_ListAccount]) then begin
    Stream.Read(FData.account_price,Sizeof(FData.account_price));
    Stream.Read(FData.account_to_pay,Sizeof(FData.account_to_pay));
    if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,raw)<0 then exit;
    FData.new_public_key := TAccountComp.RawString2Accountkey(raw);
    Stream.Read(FData.locked_until_block,Sizeof(FData.locked_until_block));
    // VERSION 5: read the new account state and hash-lock
    if FProtocolVersion >= CT_PROTOCOL_5 then begin
      if Stream.Read(w, 2) < 0 then exit;  // the new account state to set
      if Not (w in [Integer(as_ForSale),Integer(as_ForAtomicAccountSwap),Integer(as_ForAtomicCoinSwap)]) then begin
        // Invalid value readed
        Exit;
      end;
      FData.account_state := TAccountState(w);
      if TStreamOp.ReadAnsiString(Stream, FData.hash_lock) < 0 then exit;  // the hash-lock if any
    end else begin
      // On V4 and below only as_ForSale is possible
      if FData.operation_type = lat_ListAccount then
        FData.account_state := as_ForSale;
    end;
  end;
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if Not LoadOperationPayloadFromStream(Stream,FData.payload) then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

procedure TOpListAccount.FillOperationResume(Block: Cardinal; getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  SetLength(OperationResume.Changers,1);
  OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
  OperationResume.Changers[0].Account:=FData.account_target;
  case FData.operation_type of
    lat_ListAccount : begin
        if (FData.account_state = as_ForSale) And (FData.new_public_key.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
          OperationResume.Changers[0].Changes_type:=[list_for_public_sale];
        end else begin
          if FData.account_state = as_ForAtomicAccountSwap then
            OperationResume.Changers[0].Changes_type:=[list_for_account_swap]
          else if FData.account_state = as_ForAtomicCoinSwap then
            OperationResume.Changers[0].Changes_type:=[list_for_coin_swap]
          else
            OperationResume.Changers[0].Changes_type:=[list_for_private_sale];
          OperationResume.Changers[0].New_Accountkey := FData.new_public_key;
          OperationResume.Changers[0].Locked_Until_Block := FData.locked_until_block;
        end;
        OperationResume.Changers[0].Seller_Account:=FData.account_to_pay;
        OperationResume.Changers[0].Account_Price:=FData.account_price;
        if (FData.account_state in [as_ForAtomicAccountSwap, as_ForAtomicCoinSwap]) then begin
          OperationResume.Changers[0].Hashed_secret := TBaseType.ToRawBytes( FData.hash_lock );
        end;
    end;
    lat_DelistAccount : begin
        OperationResume.Changers[0].Changes_type:=[delist];
      end;
  end;
  if (FData.account_signer = FData.account_target) then begin
    OperationResume.Changers[0].Fee:=FData.fee;
    OperationResume.Changers[0].N_Operation:=FData.n_operation;
    OperationResume.Changers[0].Signature:=FData.sign;
  end else begin
    SetLength(OperationResume.Changers,2);
    OperationResume.Changers[1] := CT_TMultiOpChangeInfo_NUL;
    OperationResume.Changers[1].Account := FData.account_signer;
    OperationResume.Changers[1].N_Operation := FData.n_operation;
    OperationResume.Changers[1].Fee := FData.fee;
    OperationResume.Changers[1].Signature := FData.sign;
  end;
end;

function TOpListAccount.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpListAccount.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpListAccount.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpListAccount.OperationPayload: TOperationPayload;
begin
  Result := FData.payload;
end;

function TOpListAccount.SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
Var w : Word;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  case FData.operation_type of
    lat_ListAccount : w := CT_Op_ListAccountForSale;
    lat_DelistAccount : w := CT_Op_DelistAccount;
  else raise Exception.Create('ERROR DEV 20170412-1');
  end;
  Stream.Write(w,2);
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  if FData.operation_type in [lat_ListAccount] then begin
    Stream.Write(FData.account_price,Sizeof(FData.account_price));
    Stream.Write(FData.account_to_pay,Sizeof(FData.account_to_pay));
    Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
    TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
    TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(FData.new_public_key));
    Stream.Write(FData.locked_until_block,Sizeof(FData.locked_until_block));
    // VERSION 5: write the new account state and hash-lock
    if FProtocolVersion >= CT_PROTOCOL_5 then begin
      w := Word(FData.account_state);
      Stream.Write(w, 2);  // the new account state to set
      TStreamOp.WriteAnsiString(Stream, FData.hash_lock); // the hash-lock if any
    end;
  end;
  Stream.Write(FData.fee,Sizeof(FData.fee));
  SaveOperationPayloadToStream(Stream,FData.payload);
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpListAccount.SignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpListAccount.DestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpListAccount.SellerAccount: Int64;
begin
  Case FData.operation_type of
    lat_ListAccount : Result := FData.account_to_pay;
  else Result:=inherited SellerAccount;
  end;
end;

function TOpListAccount.toString: String;
begin
  case FData.operation_type of
    lat_ListAccount : begin
      case FData.account_state of
        as_ForSale: begin
          if (FData.new_public_key.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
            Result := Format('List account %s for sale price %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
              TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
              FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
              FData.n_operation, Length(FData.payload.payload_raw)])
          end else begin
            Result := Format('List account %s for private sale price %s reserved for %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
              TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
              TAccountComp.GetECInfoTxt(FData.new_public_key.EC_OpenSSL_NID),
              FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
              FData.n_operation, Length(FData.payload.payload_raw)])
          end;
        end;
        as_ForAtomicAccountSwap: begin
          Result := Format('List account %s for atomic account swap hash-lock:%s time-lock:%d fee:%s (n_op:%d) payload size:%d',[
            TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
            TCrypto.ToHexaString( TBaseType.ToRawBytes( FData.hash_lock ) ),
            FData.locked_until_block,
            TAccountComp.FormatMoney(FData.fee),
            FData.n_operation,
            Length(FData.payload.payload_raw)]
          );
        end;
        as_ForAtomicCoinSwap: begin
          Result := Format('List account %s for atomic coin swap for %s PASC hash-lock:%s time-lock:%d fee:%s (n_op:%d) payload size:%d',[
            TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
            TAccountComp.FormatMoney(FData.account_price),
            TCrypto.ToHexaString( TBaseType.ToRawBytes( FData.hash_lock ) ),
            FData.locked_until_block,
            TAccountComp.FormatMoney(FData.fee),
            FData.n_operation,
            Length(FData.payload.payload_raw)]
          );
        end;
      end;
    end;
    lat_DelistAccount : begin
      Result := Format('Delist account %s for sale fee:%s (n_op:%d) payload size:%d',[
        TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload.payload_raw)])
    end;
  else Result := 'ERROR DEV 20170414-2';
  end;
end;

function TOpListAccount.GetDigestToSign: TRawBytes;
var ms : TMemoryStream;
  s : TRawBytes;
  b : Byte;
  w : Word;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.account_signer,Sizeof(FData.account_signer));
    ms.Write(FData.account_target,Sizeof(FData.account_target));
   // HS 2019-06-09: NOTE TO ALBERT on de-list, the below fields are included in the signable digest.
   // This is unnecessary, but cannot be changed now.
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.account_price,Sizeof(FData.account_price));
    ms.Write(FData.account_to_pay,Sizeof(FData.account_to_pay));
    ms.Write(FData.fee,Sizeof(FData.fee));
    if FProtocolVersion>=CT_PROTOCOL_5 then begin
      ms.Write(FData.payload.payload_type,SizeOf(FData.payload.payload_type));
    end;
    if Length(FData.payload.payload_raw)>0 then
      ms.WriteBuffer(FData.payload.payload_raw[Low(FData.payload.payload_raw)],Length(FData.payload.payload_raw));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if Length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[Low(FData.public_key.x)],Length(FData.public_key.x));
    if Length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[Low(FData.public_key.y)],Length(FData.public_key.y));
    s := TAccountComp.AccountKey2RawString(FData.new_public_key);
    if Length(s)>0 then
      ms.WriteBuffer(s[Low(s)],Length(s));
    ms.Write(FData.locked_until_block,Sizeof(FData.locked_until_block));
    // VERSION 5: write the new account state and hash-lock
    if (FProtocolVersion >= CT_PROTOCOL_5) then begin
      w := Word(FData.account_state);
      ms.Write(w, 2);
      TStreamOp.WriteAnsiString(ms, FData.hash_lock); // the hash-lock if any
    end;
    if (FProtocolVersion<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    end else begin
      b := OpType;
      ms.Write(b,1);
      Result := TCrypto.DoSha256(ms.Memory,ms.Size);
    end;
  finally
    ms.Free;
  end;
end;

{ TOpListAccountForSaleOrSwap }

constructor TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(ACurrentProtocol : Word; ANewAccountState : TAccountState; AAccountSigner, ANOperation, AAccountTarget: Cardinal; AAccountPrice, AFee: UInt64; AAccountToPay: Cardinal;  ANewPublicKey: TAccountKey; ALockedUntilBlock: Cardinal; AKey: TECPrivateKey;  const AHashLock : T32Bytes; const APayload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  case ANewAccountState of
    as_Normal: raise EArgumentOutOfRangeException.Create('Listing to normal state is not a listing');
    as_ForSale: ;
    as_ForAtomicAccountSwap: ;
    as_ForAtomicCoinSwap: ;
  else
    raise EArgumentOutOfRangeException.Create('Invalid new list account state');
  end;

  FData.account_signer := AAccountSigner;
  FData.account_target := AAccountTarget;
  FData.operation_type := lat_ListAccount;
  FData.n_operation := ANOperation;

  FData.account_state := ANewAccountState;
  if ANewAccountState in [as_ForAtomicAccountSwap,as_ForAtomicCoinSwap] then begin
    // Hash lock is stored only if AtomicSwap
    FData.hash_lock := AHashLock;
  end;
  FData.locked_until_block := ALockedUntilBlock;
  FData.account_price := AAccountPrice;
  FData.account_to_pay := AAccountToPay;
  FData.fee := AFee;
  FData.payload := APayload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  if (ANewAccountState in [as_ForAtomicCoinSwap]) then begin
    // Force NULL new_public_key
    FData.new_public_key := CT_TECDSA_Public_Nul;
  end else begin
    FData.new_public_key := ANewPublicKey;
  end;



  if Assigned(AKey) then begin
    FData.sign := TCrypto.ECDSASign(AKey.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := AKey.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new list account for sale operation');
    FHasValidSignature := false;
  end;
end;

class function TOpListAccountForSaleOrSwap.OpType: Byte;
begin
  Result := CT_Op_ListAccountForSale;
end;

function TOpListAccountForSaleOrSwap.GetOpSubType : Integer;
begin
  case FData.account_state of
    as_ForSale:
      if (FData.new_public_key.EC_OpenSSL_NID<>0) then Exit(CT_OpSubtype_ListAccountForPrivateSale) else Exit(CT_OpSubtype_ListAccountForPublicSale);
    as_ForAtomicAccountSwap: Exit(CT_OpSubtype_ListAccountForAccountSwap);
    as_ForAtomicCoinSwap: Exit(CT_OpSubtype_ListAccountForCoinSwap);
  end;
end;

{ TOpDelistAccountForSale }

constructor TOpDelistAccountForSale.CreateDelistAccountForSale(ACurrentProtocol : Word; account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; const payload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_DelistAccount;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a delist account operation');
    FHasValidSignature := false;
  end;
end;

class function TOpDelistAccountForSale.OpType: Byte;
begin
  Result := CT_Op_DelistAccount;
end;

{ TOpBuyAccount }

constructor TOpBuyAccount.CreateBuy(ACurrentProtocol : Word; account_number, n_operation, account_to_buy,
  account_to_pay: Cardinal; price, amount, fee: UInt64;
  new_public_key: TAccountKey; key: TECPrivateKey; const payload: TOperationPayload);
begin
  inherited Create(ACurrentProtocol);
  FData.sender := account_number;
  FData.n_operation := n_operation;
  FData.target := account_to_buy;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.opTransactionStyle := buy_account;
  FData.AccountPrice := price;
  FData.SellerAccount := account_to_pay;
  FData.new_accountkey := new_public_key;

  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Buy operation');
    FHasValidSignature := false;
  end;
end;

procedure TOpBuyAccount.InitializeData;
begin
  inherited;
  FData.opTransactionStyle := buy_account;
end;

class function TOpBuyAccount.OpType: Byte;
begin
  Result := CT_Op_BuyAccount;
end;


{ TOpData }

procedure TOpData.InitializeData(AProtocolVersion : Word);
begin
  inherited InitializeData(AProtocolVersion);
  FData := CT_TOpDataData_NUL;
end;

function TOpData.IsValidSignatureBasedOnCurrentSafeboxState(ASafeBoxTransaction: TPCSafeBoxTransaction): Boolean;
var LAccount : TAccount;
begin
  if (FData.account_signer<0) or (FData.account_signer>=ASafeBoxTransaction.FreezedSafeBox.AccountsCount) then Exit(False); // Preventing exception
  LAccount := ASafeBoxTransaction.Account(FData.account_signer);
  Result := IsValidECDSASignature(LAccount.accountInfo.accountkey,FData.sign);
end;

function TOpData.SaveOpToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_sender,Sizeof(FData.account_sender));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  // VERSION 5: write the GUID
  if FProtocolVersion >= CT_PROTOCOL_5 then begin
    TStreamOp.writeGuid(Stream,FData.guid);
  end;
  Stream.Write(FData.dataType,Sizeof(FData.dataType));
  Stream.Write(FData.dataSequence,Sizeof(FData.dataSequence));
  Stream.Write(FData.amount,Sizeof(FData.amount));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  SaveOperationPayloadToStream(Stream,FData.payload);
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpData.LoadOpFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
begin
  Result := false;
  if Stream.Size-Stream.Position < 16  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_sender,Sizeof(FData.account_sender));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  // VERSION 5: write the GUID
  if FProtocolVersion >= CT_PROTOCOL_5 then begin
    if TStreamOp.ReadGUID(Stream,FData.guid)<16 then Exit;
  end;
  if Stream.Size-Stream.Position < 20  then exit; // Invalid stream
  Stream.Read(FData.dataType,Sizeof(FData.dataType));
  Stream.Read(FData.dataSequence,Sizeof(FData.dataSequence));
  Stream.Read(FData.amount,Sizeof(FData.amount));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if Not LoadOperationPayloadFromStream(Stream,FData.payload) then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

procedure TOpData.FillOperationResume(Block: Cardinal;
  getInfoForAllAccounts: Boolean; Affected_account_number: Cardinal;
  var OperationResume: TOperationResume);
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts,Affected_account_number, OperationResume);
  if (getInfoForAllAccounts) then
    OperationResume.OpSubtype := CT_OpSubtype_Data_GlobalInfo
  else if (Affected_account_number=FData.account_sender) then
    OperationResume.OpSubtype := CT_OpSubtype_Data_Sender
  else if (Affected_account_number=FData.account_target) then
    OperationResume.OpSubtype := CT_OpSubtype_Data_Receiver
  else if (Affected_account_number=FData.account_signer) then
    OperationResume.OpSubtype := CT_OpSubtype_Data_Signer;

  SetLength(OperationResume.Senders,1);
  OperationResume.Senders[0] := CT_TMultiOpSender_NUL;

  OperationResume.Senders[0].Account:=FData.account_sender;
  OperationResume.Senders[0].Payload:=FData.payload;
  if (FData.account_sender=FData.account_signer) then begin
    OperationResume.Senders[0].N_Operation:=FData.n_operation;
    OperationResume.Senders[0].Signature:=FData.sign;
    OperationResume.Senders[0].Amount:=Int64(FData.amount + FData.fee)*(-1);
  end else begin
    OperationResume.Senders[0].Amount:=Int64(FData.amount)*(-1);
    SetLength(OperationResume.Changers,1);
    OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
    OperationResume.Changers[0].Account:=FData.account_signer;
    OperationResume.Changers[0].Fee:=FData.fee;
    OperationResume.Changers[0].N_Operation:=FData.n_operation;
    OperationResume.Changers[0].Signature:=FData.sign;
  end;
  //
  SetLength(OperationResume.Receivers,1);
  OperationResume.Receivers[0] := CT_TMultiOpReceiver_NUL;
  OperationResume.Receivers[0].Account:=FData.account_target;
  OperationResume.Receivers[0].Amount:=FData.amount;
  OperationResume.Receivers[0].Payload:=FData.payload;
  // Add OpData missing in V4, added to V5
  OperationResume.Senders[0].OpData.ID := FData.guid;
  OperationResume.Senders[0].OpData.Sequence := FData.dataSequence;
  OperationResume.Senders[0].OpData.&Type := FData.dataType;

  //
  OperationResume.n_operation:=FData.n_operation;
  if (Affected_account_number = FData.account_signer) or (getInfoForAllAccounts) then begin
    OperationResume.Fee:=Int64(FData.fee)*(-1);
  end else OperationResume.Fee:=0;
  OperationResume.OperationTxt := ToString;
  if (getInfoForAllAccounts) then OperationResume.Amount:= OperationAmount
  else OperationResume.Amount := OperationAmountByAccount(Affected_account_number);
end;

class function TOpData.OpType: Byte;
begin
  Result := CT_Op_Data;
end;

function TOpData.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
begin
  Result:=inherited GetBufferForOpHash(UseProtocolV2);
end;

function TOpData.DoOperation(
  AccountPreviousUpdatedBlock: TAccountPreviousBlockInfo;
  AccountTransaction: TPCSafeBoxTransaction; var errors: String): Boolean;
Var account_signer, account_sender, account_target : TAccount;
  LSafeboxCurrentProtocol : Integer;
begin
  Result := false;
  LSafeboxCurrentProtocol := AccountTransaction.FreezedSafeBox.CurrentProtocol;
  if (LSafeboxCurrentProtocol<CT_PROTOCOL_4) then begin
    errors := 'OpData is not allowed on Protocol < 4';
    exit;
  end;
  if (FData.account_signer>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid signer account number';
    Exit;
  end;
  if (FData.account_sender>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid sender account number';
    Exit;
  end;
  if (FData.account_target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid target account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_signer, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Signer account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_sender, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Sender account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account_target, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'Target account is blocked for protocol';
    Exit;
  end;

  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  if (FData.amount<0) Or (FData.amount>CT_MaxTransactionAmount) then begin
    errors := 'Invalid amount: '+Inttostr(FData.fee);
    exit;
  end;

  account_signer := AccountTransaction.Account(FData.account_signer);
  account_sender := AccountTransaction.Account(FData.account_sender);
  account_target := AccountTransaction.Account(FData.account_target);
  // Is signer locked?
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Signer account is currently locked';
    exit;
  end;
  if (FData.account_signer<>FData.account_sender) then begin
    // Both accounts must have same PUBLIC KEY!
    if Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_sender.accountInfo.accountKey) then begin
      errors := 'Signer and sender accounts have different public key';
      Exit;
    end;
    if (account_signer.balance<FData.fee) then begin
      errors := 'Insuficient signer funds';
      exit;
    end;
    if (account_sender.balance<FData.amount) then begin
      errors := 'Insuficient sender funds';
      exit;
    end;
    // Is sender locked?
    if (TAccountComp.IsAccountLocked(account_sender.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Sender account is currently locked';
      exit;
    end;
  end else begin
    // signer = sender
    if (account_signer.balance<(FData.fee + FData.amount)) then begin
      errors := 'Insuficient funds';
      exit;
    end;
  end;

  if ((account_signer.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;

  if (length(FData.payload.payload_raw)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload.payload_raw))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    Exit;
  end;

  // Check signature
  If Not IsValidECDSASignature(account_signer.accountInfo.accountkey,FData.sign) then begin
    errors := 'Invalid ECDSA signature';
    Exit;
  end;

  Result := AccountTransaction.TransferAmount(AccountPreviousUpdatedBlock,
    GetOpID,
    FData.account_sender,FData.account_signer,FData.account_target,
    FData.n_operation,FData.amount,FData.fee,errors);
end;

function TOpData.OperationAmount: Int64;
begin
  Result := FData.amount + FData.fee;
end;

function TOpData.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpData.OperationPayload: TOperationPayload;
begin
  Result := FData.payload;
end;

function TOpData.SignerAccount: Cardinal;
begin
  Result := FData.account_signer;
end;

function TOpData.DestinationAccount: Int64;
begin
  Result := FData.account_target;
end;

function TOpData.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

procedure TOpData.AffectedAccounts(list: TOrderedList<Cardinal>);
begin
  list.Add(FData.account_signer);
  if (FData.account_signer<>FData.account_sender) then begin
    list.Add(FData.account_sender);
  end;
  if (FData.account_signer<>FData.account_target) And (FData.account_sender<>FData.account_target) then begin
    list.Add(FData.account_target);
  end;
end;

function TOpData.OperationAmountByAccount(account: Cardinal): Int64;
begin
  Result := 0;
  if (account = FData.account_signer) then begin
    Result := Int64(FData.fee) * (-1);
  end;
  if (account = FData.account_sender) then begin
    Result := Result + (Int64(FData.amount)*(-1));
  end;
  if (account = FData.account_target) then begin
    Result := Result + FData.amount;
  end;
end;

constructor TOpData.CreateOpData(ACurrentProtocol : word; account_signer, account_sender,
  account_target: Cardinal; signer_key: TECPrivateKey; n_operation: Cardinal;
  dataType, dataSequence: Word; AGUID : TGUID; amount, fee: UInt64; const payload: TOperationPayload);
begin
  Inherited Create(ACurrentProtocol);
  FData.account_sender:=account_sender;
  FData.account_signer:=account_signer;
  FData.account_target:=account_target;
  FData.amount:=amount;
  FData.fee:=fee;
  FData.n_operation:=n_operation;
  FData.payload:=payload;
  FData.dataSequence:=dataSequence;
  FData.dataType:=dataType;
  FData.guid := AGUID;
  if Assigned(signer_key) then begin
    FData.sign := TCrypto.ECDSASign(signer_key.PrivateKey, GetDigestToSign);
    FHasValidSignature := true;
    FUsedPubkeyForSignature := signer_key.PublicKey;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new OpData');
    FHasValidSignature := false;
  end;
end;

function TOpData.toString: String;
begin
  Result := Format('OpData from:%d to:%d type:%d sequence:%d Amount:%s',
    [FData.account_sender,FData.account_target,FData.dataType,FData.dataSequence,
     TAccountComp.FormatMoney(FData.amount)]);
end;

function TOpData.GetDigestToSign: TRawBytes;
var Stream : TMemoryStream;
  b : Byte;
begin
  Stream := TMemoryStream.Create;
  Try
    Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
    Stream.Write(FData.account_sender,Sizeof(FData.account_sender));
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
    Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
    // VERSION 5: write the GUID to the digest
    if ProtocolVersion >= CT_PROTOCOL_5 then begin
      TStreamOp.WriteGUID(Stream,FData.guid);
    end;
    Stream.Write(FData.dataType,Sizeof(FData.dataType));
    Stream.Write(FData.dataSequence,Sizeof(FData.dataSequence));
    Stream.Write(FData.amount,Sizeof(FData.amount));
    Stream.Write(FData.fee,Sizeof(FData.fee));
    SaveOperationPayloadToStream(Stream,FData.payload);
    b := OpType;
    Stream.Write(b,1);
    if (ProtocolVersion<=CT_PROTOCOL_4) then begin
      Result := TStreamOp.SaveStreamToRaw(Stream);
    end else begin
      Result := TCrypto.DoSha256(Stream.Memory,Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

initialization
  RegisterOperationsClass;
end.
