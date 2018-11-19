unit UOpTransaction;

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

Uses UCrypto, UBlockChain, Classes, UAccounts, UBaseTypes;

Type
  // Operations Type
  TOpTransactionStyle = (transaction, transaction_with_auto_buy_account, buy_account);
  TOpTransactionData = Record
    sender: Cardinal;
    n_operation : Cardinal;
    target: Cardinal;
    amount: UInt64;
    fee: UInt64;
    payload: TRawBytes;
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
    payload: TRawBytes;
    public_key: TECDSA_Public;
    new_accountkey: TAccountKey;
    sign: TECDSA_SIG;
  End;

  TOpRecoverFoundsData = Record
    account: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
  End;

Const
  CT_TOpTransactionData_NUL : TOpTransactionData = (sender:0;n_operation:0;target:0;amount:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');sign:(r:'';s:'');opTransactionStyle:transaction;AccountPrice:0;SellerAccount:0;new_accountkey:(EC_OpenSSL_NID:0;x:'';y:''));
  CT_TOpChangeKeyData_NUL : TOpChangeKeyData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');new_accountkey:(EC_OpenSSL_NID:0;x:'';y:'');sign:(r:'';s:''));
  CT_TOpRecoverFoundsData_NUL : TOpRecoverFoundsData = (account:0;n_operation:0;fee:0);

Type
  { TOpTransaction }

  TOpTransaction = Class(TPCOperation)
  private
    FData : TOpTransactionData;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    procedure AffectedAccounts(list : TList); override;
    //
    class function OpType : Byte; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Property Data : TOpTransactionData read FData;

    Constructor CreateTransaction(current_protocol : Word; sender, n_operation, target: Cardinal; key: TECPrivateKey; amount, fee: UInt64; payload: TRawBytes);
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
  End;

  { TOpChangeKey }

  TOpChangeKey = Class(TPCOperation)
  private
    FData : TOpChangeKeyData;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor Create(current_protocol : Word; account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: TRawBytes);
    Property Data : TOpChangeKeyData read FData;
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
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
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function N_Operation : Cardinal; override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    procedure AffectedAccounts(list : TList); override;
    Constructor Create(account_number, n_operation: Cardinal; fee: UInt64);
    Property Data : TOpRecoverFoundsData read FData;
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
  End;

  // NEW OPERATIONS PROTOCOL 2
  TOpListAccountOperationType = (lat_Unknown, lat_ListForSale, lat_DelistAccount);

  TOpListAccountData = Record
    account_signer,
    account_target: Cardinal;
    operation_type : TOpListAccountOperationType;
    n_operation : Cardinal;
    account_price: UInt64;
    account_to_pay : Cardinal;
    fee: UInt64;
    payload: TRawBytes;
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
    payload: TRawBytes;
    public_key: TECDSA_Public;
    changes_type : TOpChangeAccountInfoTypes; // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type
    new_accountkey: TAccountKey;  // If (changes_mask and $0001)=$0001 then change account key
    new_name: TRawBytes;          // If (changes_mask and $0002)=$0002 then change name
    new_type: Word;               // If (changes_mask and $0004)=$0004 then change type
    sign: TECDSA_SIG;
  End;


Const
  CT_TOpListAccountData_NUL : TOpListAccountData = (account_signer:0;account_target:0;operation_type:lat_Unknown;n_operation:0;account_price:0;account_to_pay:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');new_public_key:(EC_OpenSSL_NID:0;x:'';y:'');locked_until_block:0;sign:(r:'';s:''));
  CT_TOpChangeAccountInfoData_NUL : TOpChangeAccountInfoData = (account_signer:0;account_target:0;n_operation:0;fee:0;payload:'';public_key:(EC_OpenSSL_NID:0;x:'';y:'');changes_type:[];
    new_accountkey:(EC_OpenSSL_NID:0;x:'';y:'');new_name:'';new_type:0;sign:(r:'';s:''));

Type

  { TOpListAccount }
  TOpListAccount = Class(TPCOperation)
  private
    FData : TOpListAccountData;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    Function IsPrivateSale : Boolean;
    Function IsDelist : Boolean; virtual; abstract;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Property Data : TOpListAccountData read FData;
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
  End;

  TOpListAccountForSale = Class(TOpListAccount)
  public
    class function OpType : Byte; override;
    Constructor CreateListAccountForSale(current_protocol : Word; account_signer, n_operation, account_target: Cardinal; account_price, fee : UInt64; account_to_pay:Cardinal; new_public_key:TAccountKey; locked_until_block : Cardinal; key:TECPrivateKey; payload: TRawBytes);
    Function IsDelist : Boolean; override;
  End;

  TOpDelistAccountForSale = Class(TOpListAccount)
  public
    class function OpType : Byte; override;
    Constructor CreateDelistAccountForSale(current_protocol : Word; account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
    Function IsDelist : Boolean; override;
  End;

  { TOpBuyAccount }

  TOpBuyAccount = Class(TOpTransaction)
  protected
    procedure InitializeData; override;
  public
    class function OpType : Byte; override;
    Constructor CreateBuy(current_protocol : Word; account_number, n_operation, account_to_buy, account_to_pay: Cardinal; price, amount, fee : UInt64; new_public_key:TAccountKey; key:TECPrivateKey; payload: TRawBytes);
  End;

  { TOpChangeAccountInfo }

  TOpChangeAccountInfo = Class(TPCOperation)
  private
    FData : TOpChangeAccountInfoData;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor CreateChangeAccountInfo(current_protocol : word;
      account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey;
      change_key : Boolean; const new_account_key : TAccountKey;
      change_name: Boolean; const new_name : TRawBytes;
      change_type: Boolean; const new_type : Word;
      fee: UInt64; payload: TRawBytes);
    Property Data : TOpChangeAccountInfoData read FData;
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
  End;


  TOpDataData = Record
    account_signer,              // The account paying fees (if any) and signing operation
    account_sender,              // The account sender. Public key must be EQUAL to account_signer public key
    account_target: Cardinal;    // The destination account. Will recive DATA and amount (if any)
    n_operation : Cardinal;      // Signer n_operation
    dataType : Word;             // 2 byte data type
    dataSequence : Word;         // 2 byte data sequence
    amount: UInt64;              // Allow amount=0
    fee: UInt64;                 // Allow fee=0
    payload: TRawBytes;          // Standard arbitrary data with length<=256
    sign: TECDSA_SIG;
  End;

  { TOpData }

  TOpData = Class(TPCOperation)
  private
    FData : TOpDataData;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    class function OpType : Byte; override;

    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : Int64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    Constructor CreateOpData(
      account_signer, account_sender, account_target : Cardinal; signer_key:TECPrivateKey;
      n_operation : Cardinal;
      dataType, dataSequence : Word;
      amount, fee : UInt64;
      payload: TRawBytes);
    Property Data : TOpDataData read FData;
    Function toString : String; Override;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; override;
  End;

Const
  CT_TOpDataData_NUL : TOpDataData = (account_signer:0;account_sender:0;account_target:0;n_operation:0;dataType:0;dataSequence:0;amount:0;fee:0;payload:'';sign:(r:'';s:''));

Procedure RegisterOperationsClass;

implementation

uses
  SysUtils, UConst, ULog, UTxMultiOperation;

Procedure RegisterOperationsClass;
Begin
  TPCOperationsComp.RegisterOperationClass(TOpTransaction);
  TPCOperationsComp.RegisterOperationClass(TOpChangeKey);
  TPCOperationsComp.RegisterOperationClass(TOpRecoverFounds);
  TPCOperationsComp.RegisterOperationClass(TOpListAccountForSale);
  TPCOperationsComp.RegisterOperationClass(TOpDelistAccountForSale);
  TPCOperationsComp.RegisterOperationClass(TOpBuyAccount);
  TPCOperationsComp.RegisterOperationClass(TOpChangeKeySigned);
  TPCOperationsComp.RegisterOperationClass(TOpChangeAccountInfo);
  TPCOperationsComp.RegisterOperationClass(TOpMultiOperation);
  TPCOperationsComp.RegisterOperationClass(TOpData);
End;

{ TOpChangeAccountInfo }

procedure TOpChangeAccountInfo.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpChangeAccountInfoData_NUL;
end;

function TOpChangeAccountInfo.SaveOpToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var b : byte;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  TStreamOp.WriteAccountKey(Stream,FData.public_key);
  b := 0;
  if (public_key in FData.changes_type) then b:=b OR $01;
  if (account_name in FData.changes_type) then b:=b OR $02;
  if (account_type in FData.changes_type) then b:=b OR $04;
  Stream.Write(b,Sizeof(b));
  TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
  TStreamOp.WriteAnsiString(Stream,FData.new_name);
  Stream.Write(FData.new_type,Sizeof(FData.new_type));
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
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.public_key)<0 then Exit;
  Stream.Read(b,SizeOf(b));
  FData.changes_type:=[];
  if (b AND $01)=$01 then FData.changes_type:=FData.changes_type + [public_key];
  if (b AND $02)=$02 then FData.changes_type:=FData.changes_type + [account_name];
  if (b AND $04)=$04 then FData.changes_type:=FData.changes_type + [account_type];
  // Check
  if (b AND $F8)<>0 then Exit;
  if TStreamOp.ReadAccountKey(Stream,FData.new_accountkey)<0 then Exit;
  if TStreamOp.ReadAnsiString(Stream,FData.new_name)<0 then Exit;
  Stream.Read(FData.new_type,Sizeof(FData.new_type));
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

function TOpChangeAccountInfo.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;
Var account_signer, account_target : TAccount;
begin
  Result := false;
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
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'NOT ALLOWED ON PROTOCOL 1';
    exit;
  end;
  If (public_key in FData.changes_type) then begin
    If Not TAccountComp.IsValidAccountKey( FData.new_accountkey, errors ) then begin
      exit;
    end;
  end;
  If (account_name in FData.changes_type) then begin
    If (FData.new_name<>'') then begin
      If Not TPCSafeBox.ValidAccountName(FData.new_name,errors) then Exit;
    end;
  end else begin
    If (FData.new_name<>'') then begin
      errors := 'Invalid data in new_name field';
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

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetDigestToSign(AccountTransaction.FreezedSafeBox.CurrentProtocol),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;
  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  If (public_key in FData.changes_type) then begin
    account_target.accountInfo.accountKey := FData.new_accountkey;
  end;
  If (account_name in FData.changes_type) then begin
    account_target.name := FData.new_name;
  end;
  If (account_type in FData.changes_type) then begin
    account_target.account_type := FData.new_type;
  end;
  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,
         FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
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

function TOpChangeAccountInfo.OperationPayload: TRawBytes;
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

procedure TOpChangeAccountInfo.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target<>FData.account_signer) then list.Add(TObject(FData.account_target));
end;

function TOpChangeAccountInfo.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

constructor TOpChangeAccountInfo.CreateChangeAccountInfo(current_protocol : word;
  account_signer, n_operation,
  account_target: Cardinal; key: TECPrivateKey; change_key: Boolean;
  const new_account_key: TAccountKey; change_name: Boolean;
  const new_name: TRawBytes; change_type: Boolean; const new_type: Word;
  fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
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

  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
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
    s := s + 'new name to "'+FData.new_name+'"';
  end;
  If (account_type IN FData.changes_type)  then begin
    if s<>'' then s:=s+', ';
    s := s + 'new type to '+IntToStr(FData.new_type);
  end;
  Result := Format('Change account %s info: %s fee:%s (n_op:%d) payload size:%d',[
     TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target),
     s,
     TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;

function TOpChangeAccountInfo.GetDigestToSign(current_protocol : Word): TRawBytes;
var Stream : TMemoryStream;
  b : Byte;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
    Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
    Stream.Write(FData.fee,Sizeof(FData.fee));
    TStreamOp.WriteAnsiString(Stream,FData.payload);
    TStreamOp.WriteAccountKey(Stream,FData.public_key);
    b := 0;
    if (public_key in FData.changes_type) then b:=b OR $01;
    if (account_name in FData.changes_type) then b:=b OR $02;
    if (account_type in FData.changes_type) then b:=b OR $04;
    Stream.Write(b,Sizeof(b));
    TStreamOp.WriteAccountKey(Stream,FData.new_accountkey);
    TStreamOp.WriteAnsiString(Stream,FData.new_name);
    Stream.Write(FData.new_type,Sizeof(FData.new_type));
    if (current_protocol<=CT_PROTOCOL_3) then begin
      Stream.Position := 0;
      setlength(Result,Stream.Size);
      Stream.ReadBuffer(Result[1],Stream.Size);
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

procedure TOpTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.sender));
  list.Add(TObject(FData.target));
  if (FData.opTransactionStyle in [transaction_with_auto_buy_account, buy_account]) then begin
    list.Add(TObject(FData.SellerAccount));
  end;
end;

constructor TOpTransaction.CreateTransaction(current_protocol : Word;
  sender, n_operation, target: Cardinal;
  key: TECPrivateKey; amount, fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
  FData.sender := sender;
  FData.n_operation := n_operation;
  FData.target := target;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Transaction');
    FHasValidSignature := false;
  end;
end;

function TOpTransaction.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;
Var s_new, t_new : Int64;
  totalamount : Cardinal;
  sender,target,seller : TAccount;
  _h : TRawBytes;
  _IsBuyTransaction :  Boolean;
begin
  Result := false;
  errors := '';
  //
  if (FData.sender>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := Format('Invalid sender %d',[FData.sender]);
    Exit;
  end;
  if (FData.target>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := Format('Invalid target %d',[FData.target]);
    Exit;
  end;
  if (FData.sender=FData.target) then begin
    errors := Format('Sender=Target %d',[FData.sender]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.sender,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('sender (%d) is blocked for protocol',[FData.sender]);
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.target,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('target (%d) is blocked for protocol',[FData.target]);
    Exit;
  end;
  if (FData.amount<=0) Or (FData.amount>CT_MaxTransactionAmount) then begin
    errors := Format('Invalid amount %d (0 or max: %d)',[FData.amount,CT_MaxTransactionAmount]);
    Exit;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := Format('Invalid fee %d (max %d)',[FData.fee,CT_MaxTransactionFee]);
    Exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;

  sender := AccountTransaction.Account(FData.sender);
  target := AccountTransaction.Account(FData.target);
  if ((sender.n_operation+1)<>FData.n_operation) then begin
    errors := Format('Invalid n_operation %d (expected %d)',[FData.n_operation,sender.n_operation+1]);
    Exit;
  end;
  totalamount := FData.amount + FData.fee;
  if (sender.balance<totalamount) then begin
    errors := Format('Insuficient founds %d < (%d + %d = %d)',[sender.balance,FData.amount,FData.fee,totalamount]);
    Exit;
  end;
  if (target.balance+FData.amount>CT_MaxWalletAmount) then begin
    errors := Format('Target cannot accept this transaction due to max amount %d+%d=%d > %d',[target.balance,FData.amount,target.balance+FData.amount,CT_MaxWalletAmount]);
    Exit;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(sender.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Sender Account is currently locked';
    exit;
  end;
  // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,sender.accountInfo.accountkey)) then begin
    errors := Format('Invalid sender public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.sender,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(sender.accountInfo.accountkey))]);
    exit;
  end;

  // Check signature
  _h := GetDigestToSign(AccountTransaction.FreezedSafeBox.CurrentProtocol);
  if (Not TCrypto.ECDSAVerify(sender.accountInfo.accountkey,_h,FData.sign)) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end else FHasValidSignature := true;
  //
  FPrevious_Signer_updated_block := sender.updated_block;
  FPrevious_Destination_updated_block := target.updated_block;


  // Is buy account ?
  if (FData.opTransactionStyle=buy_Account) then begin
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'Buy account is not allowed on Protocol 1';
      exit;
    end;

    seller := AccountTransaction.Account(FData.SellerAccount);
    if Not (TAccountComp.IsAccountForSale(target.accountInfo)) then begin
      errors := Format('%d is not for sale',[target.account]);
      exit;
    end;
    // Check that seller is the expected seller
    If (target.accountInfo.account_to_pay<>seller.account) then begin
      errors := Format('Seller account %d is not expected account %d',[FData.SellerAccount,target.accountInfo.account_to_pay]);
      exit;
    end;
    if (target.balance + FData.amount < target.accountInfo.price) then begin
      errors := Format('Account %d balance (%d) + amount (%d) < price (%d)',[target.account,target.balance,FData.amount,target.accountInfo.price]);
      exit;
    end;
    if (FData.AccountPrice<>target.accountInfo.price) then begin
      errors := Format('Signed price (%d) is not the same of account price (%d)',[FData.AccountPrice,target.accountInfo.price]);
      exit;
    end;
    If Not (TAccountComp.IsValidAccountKey(FData.new_accountkey,errors)) then exit; // BUG 20171511
    _IsBuyTransaction := True;
    FPrevious_Seller_updated_block := seller.updated_block;
  end else if
    // Is automatic buy account?
    (FData.opTransactionStyle = transaction_with_auto_buy_account)
    Or // New automatic buy ?
    ( (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) And
      (FData.opTransactionStyle=transaction) And
      (TAccountComp.IsAccountForSaleAcceptingTransactions(target.accountInfo)) And
      (target.balance + FData.amount >= target.accountInfo.price) ) then begin

    if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'Tx-Buy account is not allowed on Protocol 1';
      exit;
    end;
    If Not (TAccountComp.IsValidAccountKey(FData.new_accountkey,errors)) then exit; // BUG 20171511

    _IsBuyTransaction := true; // Automatic buy
    // Fill the purchase data
    FData.opTransactionStyle := transaction_with_auto_buy_account; // Set this data!
    FData.AccountPrice := target.accountInfo.price;
    FData.SellerAccount := target.accountInfo.account_to_pay;
    seller := AccountTransaction.Account(target.accountInfo.account_to_pay);
    FPrevious_Seller_updated_block := seller.updated_block;
    FData.new_accountkey := target.accountInfo.new_publicKey;
  end else begin
    _IsBuyTransaction := false;
  end;

  if (_IsBuyTransaction) then begin
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      exit;
    end;
    Result := AccountTransaction.BuyAccount(AccountPreviousUpdatedBlock,sender.account,target.account,seller.account,FData.n_operation,FData.amount,target.accountInfo.price,FData.fee,FData.new_accountkey,errors);
  end else begin
    Result := AccountTransaction.TransferAmount(AccountPreviousUpdatedBlock,FData.sender,FData.sender,FData.target,FData.n_operation,FData.amount,FData.fee,errors);
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
      if length(FData.payload)>0 then
        ms.WriteBuffer(FData.payload[1],length(FData.payload));
      ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
      if length(FData.public_key.x)>0 then
        ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
      if length(FData.public_key.y)>0 then
        ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
      if length(FData.sign.r)>0 then
        ms.WriteBuffer(FData.sign.r[1],length(FData.sign.r));
      if length(FData.sign.s)>0 then
        ms.WriteBuffer(FData.sign.s[1],length(FData.sign.s));
      SetLength(Result,ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(Result[1],ms.Size);
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
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
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
      End
    else exit;
    end;
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,buy_account]) then begin
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
    buy_account,transaction_with_auto_buy_account : begin
      SetLength(OperationResume.Receivers,2);
      OperationResume.Receivers[0] := CT_TMultiOpReceiver_NUL;
      OperationResume.Receivers[0].Account:=FData.target;
      OperationResume.Receivers[0].Amount:= (FData.amount - FData.AccountPrice);
      OperationResume.Receivers[0].Payload:=FData.payload;
      OperationResume.Receivers[1] := CT_TMultiOpReceiver_NUL;
      OperationResume.Receivers[1].Account:=FData.SellerAccount;
      OperationResume.Receivers[1].Amount:= FData.AccountPrice;
      OperationResume.Receivers[1].Payload:=FData.payload;
      SetLength(OperationResume.Changers,1);
      OperationResume.Changers[0] := CT_TMultiOpChangeInfo_NUL;
      OperationResume.Changers[0].Account := FData.target;
      OperationResume.Changers[0].Changes_type := [public_key];
      OperationResume.Changers[0].New_Accountkey := FData.new_accountkey;
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

function TOpTransaction.OperationPayload: TRawBytes;
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
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
  TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
  TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
  if ((SaveExtendedData) Or (Self is TOpBuyAccount)) then begin
    case FData.opTransactionStyle of
      transaction : b:=0;
      transaction_with_auto_buy_account : b:=1;
      buy_account : b:=2;
    else raise Exception.Create('ERROR DEV 20170424-1');
    end;
    Stream.Write(b,1);
    if (FData.opTransactionStyle in [transaction_with_auto_buy_account,buy_account]) then begin
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
    transaction_with_auto_buy_account, buy_account : Result := FData.SellerAccount;
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
  If (FData.sender = account) then Result := Result + (Int64(FData.amount+FData.fee) * (-1));
  If (FData.target = account) then begin
  if (FData.opTransactionStyle in [buy_account,transaction_with_auto_buy_account]) then Result := Result + (FData.amount - FData.AccountPrice)
  else Result := Result + FData.amount;
  end;
  If ((FData.SellerAccount = account) And (FData.opTransactionStyle in [buy_account,transaction_with_auto_buy_account] )) then begin
  Result := Result + FData.AccountPrice;
  end;
end;

function TOpTransaction.toString: String;
begin
  case FData.opTransactionStyle of
    transaction :
      Result := Format('Transaction from %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
    transaction_with_auto_buy_account :
      Result := Format('Transaction/Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.FormatMoney(FData.AccountPrice), TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
    buy_account :
      Result := Format('Buy account %s by %s paying %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',[
         TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
         TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
         TAccountComp.FormatMoney(FData.AccountPrice), TAccountComp.AccountNumberToAccountTxtNumber(FData.SellerAccount),
         TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
  else raise Exception.Create('ERROR DEV 20170424-2');
  end;
end;

function TOpTransaction.GetDigestToSign(current_protocol : Word): TRawBytes;
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
    if length(FData.payload)>0 then
      ms.WriteBuffer(FData.payload[1],length(FData.payload));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
    if length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
    if FData.opTransactionStyle=buy_account then begin
      ms.Write(FData.AccountPrice,Sizeof(FData.AccountPrice));
      ms.Write(FData.SellerAccount,Sizeof(FData.SellerAccount));
      ms.Write(FData.new_accountkey.EC_OpenSSL_NID,Sizeof(FData.new_accountkey.EC_OpenSSL_NID));
      if length(FData.new_accountkey.x)>0 then
        ms.WriteBuffer(FData.new_accountkey.x[1],length(FData.new_accountkey.x));
      if length(FData.new_accountkey.y)>0 then
        ms.WriteBuffer(FData.new_accountkey.y[1],length(FData.new_accountkey.y));
    end;
    if (current_protocol<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
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

procedure TOpChangeKey.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_target<>FData.account_signer) then list.Add(TObject(FData.account_target));
end;

function TOpChangeKey.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

constructor TOpChangeKey.Create(current_protocol : Word; account_signer, n_operation, account_target: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: TRawBytes);
begin
  inherited Create;
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
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new Change key');
    FHasValidSignature := false;
  end;
end;

function TOpChangeKey.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;
Var account_signer, account_target : TAccount;
begin
  Result := false;
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
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    If (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
      Exit; // BUG from protocol 1
    end;
  end;
  // Is locked? Protocol 2 check
  if (TAccountComp.IsAccountLocked(account_signer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
    errors := 'Account signer is currently locked';
    exit;
  end;
  If Not TAccountComp.IsValidAccountKey( FData.new_accountkey, errors ) then begin
    exit;
  end;
  // NEW v2 protocol protection: Does not allow to change key for same key
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_2) then begin
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
    if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
      errors := 'NOT ALLOWED ON PROTOCOL 1';
      exit;
    end;
  end;

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetDigestToSign(AccountTransaction.FreezedSafeBox.CurrentProtocol),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;

  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  account_target.accountInfo.accountKey := FData.new_accountkey;
  // Set to normal:
  account_target.accountInfo.state := as_Normal;
  account_target.accountInfo.locked_until_block := 0;
  account_target.accountInfo.price := 0;
  account_target.accountInfo.account_to_pay := 0;
  account_target.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,
         FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_type,
         FData.fee,errors);
end;

function TOpChangeKey.GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  If UseProtocolV2 then Result := inherited GetBufferForOpHash(UseProtocolV2)
  else begin
    ms := TMemoryStream.Create;
    try
      ms.Write(FData.account_signer,Sizeof(FData.account_signer)); //Protocol 1 does not allow signer/target. signer=target always
      ms.Write(FData.n_operation,Sizeof(FData.n_operation));
      ms.Write(FData.fee,Sizeof(FData.fee));
      if length(FData.payload)>0 then
        ms.WriteBuffer(FData.payload[1],length(FData.payload));
      ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
      if length(FData.public_key.x)>0 then
        ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
      if length(FData.public_key.y)>0 then
        ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
      s := TAccountComp.AccountKey2RawString(FData.new_accountkey);
      if length(s)>0 then
        ms.WriteBuffer(s[1],length(s));
      if length(FData.sign.r)>0 then
        ms.WriteBuffer(FData.sign.r[1],length(FData.sign.r));
      if length(FData.sign.s)>0 then
        ms.WriteBuffer(FData.sign.s[1],length(FData.sign.s));
      ms.Position := 0;
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TOpChangeKey.InitializeData;
begin
  inherited;
  FData := CT_TOpChangeKeyData_NUL;
end;

function TOpChangeKey.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var s : AnsiString;
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
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
  if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
  FData.new_accountkey := TAccountComp.RawString2Accountkey(s);
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

function TOpChangeKey.OperationPayload: TRawBytes;
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
  TStreamOp.WriteAnsiString(Stream,FData.payload);
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
    TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;

function TOpChangeKey.GetDigestToSign(current_protocol : Word): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
  b : Byte;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.account_signer,Sizeof(FData.account_signer));
    if (FData.account_signer<>FData.account_target) then ms.Write(FData.account_target,Sizeof(FData.account_target));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.fee,Sizeof(FData.fee));
    if length(FData.payload)>0 then
      ms.WriteBuffer(FData.payload[1],length(FData.payload));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
    if length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
    s := TAccountComp.AccountKey2RawString(FData.new_accountkey);
    if length(s)>0 then
      ms.WriteBuffer(s[1],length(s));
    if (current_protocol<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
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

procedure TOpRecoverFounds.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account));
end;

constructor TOpRecoverFounds.Create(account_number, n_operation : Cardinal; fee: UInt64);
begin
  inherited Create;
  FData.account := account_number;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FHasValidSignature := true; // Recover founds doesn't need a signature
end;

function TOpRecoverFounds.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;
Var acc : TAccount;
begin
  Result := false;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  acc := AccountTransaction.Account(FData.account);
  if (acc.updated_block + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('Account is active to recover founds! Account %d Updated %d + %d >= BlockCount : %d',[FData.account,acc.updated_block,CT_RecoverFoundsWaitInactiveCount,AccountTransaction.FreezedSafeBox.BlocksCount]);
    Exit;
  end;
  // Build 1.0.8 ... there was a BUG. Need to prevent recent created accounts
  if (TAccountComp.AccountBlock(FData.account) + CT_RecoverFoundsWaitInactiveCount >= AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := Format('AccountBlock is active to recover founds! AccountBlock %d + %d >= BlockCount : %d',[TAccountComp.AccountBlock(FData.account),CT_RecoverFoundsWaitInactiveCount,AccountTransaction.FreezedSafeBox.BlocksCount]);
    Exit;
  end;
  if ((acc.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (FData.fee<=0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee '+Inttostr(FData.fee);
    exit;
  end;
  if (acc.balance<FData.fee) then begin
    errors := 'Insuficient founds';
    exit;
  end;
  FPrevious_Signer_updated_block := acc.updated_block;
  Result := AccountTransaction.TransferAmount(AccountPreviousUpdatedBlock,FData.account,FData.account,FData.account,FData.n_operation,0,FData.fee,errors);
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
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
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

function TOpRecoverFounds.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
begin
  Result := false;
  if Stream.Size - Stream.Position<16 then exit;
  Stream.Read(FData.account,Sizeof(FData.account));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
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
end;

function TOpRecoverFounds.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpRecoverFounds.OperationFee: Int64;
begin
  Result := FData.fee;
end;

function TOpRecoverFounds.OperationPayload: TRawBytes;
begin
  Result := '';
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
  Result := Format('Recover founds of account %s fee:%s (n_op:%d)',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account),
    TAccountComp.FormatMoney(FData.fee),fData.n_operation]);
end;

function TOpRecoverFounds.GetDigestToSign(current_protocol : Word): TRawBytes;
begin
  Result := ''; // Nothing to be signed!
end;

{ TOpListAccount }

procedure TOpListAccount.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if FData.account_signer<>FData.account_target then
    list.Add(TObject(FData.account_target));
end;

function TOpListAccount.OperationAmountByAccount(account: Cardinal): Int64;
begin
  if (FData.account_signer = account) then Result := Int64(FData.fee)*(-1)
  else Result := 0;
end;

function TOpListAccount.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;
Var account_signer, account_target : TAccount;
begin
  Result := false;
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_2) then begin
    errors := 'List/Delist Account is not allowed on Protocol 1';
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
  if Not IsDelist then begin
    if (FData.account_to_pay>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid account to pay number';
      Exit;
    end;
    if (FData.account_target = FData.account_to_pay) then begin
      errors := 'Account to pay is itself';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(FData.account_to_pay, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'Account to pay is blocked for protocol';
      Exit;
    end;
    if (FData.account_price<=0) then begin
      errors := 'Account for sale price must be > 0';
      exit;
    end;
    if (FData.locked_until_block > (AccountTransaction.FreezedSafeBox.BlocksCount + CT_MaxFutureBlocksLockedAccount)) then begin
      errors := 'Invalid locked block: Current block '+Inttostr(AccountTransaction.FreezedSafeBox.BlocksCount)+' cannot lock to block '+IntToStr(FData.locked_until_block);
      exit;
    end;
    if IsPrivateSale then begin
      If Not TAccountComp.IsValidAccountKey( FData.new_public_key, errors ) then begin
        errors := 'Invalid new public key: '+errors;
        exit;
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
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
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
  if (IsPrivateSale) then begin
    if TAccountComp.EqualAccountKeys(account_target.accountInfo.accountKey,FData.new_public_key) then begin
      errors := 'New public key for private sale is the same public key';
      Exit;
    end;
  end;

  //
  // Build 1.4
  If (FData.public_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And (Not TAccountComp.EqualAccountKeys(FData.public_key,account_signer.accountInfo.accountkey)) then begin
    errors := Format('Invalid public key for account %d. Distinct from SafeBox public key! %s <> %s',[
      FData.account_signer,
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(FData.public_key)),
      TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account_signer.accountInfo.accountkey))]);
    exit;
  end;

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetDigestToSign(AccountTransaction.FreezedSafeBox.CurrentProtocol),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    exit;
  end else FHasValidSignature := true;

  FPrevious_Signer_updated_block := account_signer.updated_block;
  FPrevious_Destination_updated_block := account_target.updated_block;
  if IsDelist then begin
    account_target.accountInfo.state := as_Normal;
    account_target.accountInfo.locked_until_block := CT_AccountInfo_NUL.locked_until_block;
    account_target.accountInfo.price := CT_AccountInfo_NUL.price;
    account_target.accountInfo.account_to_pay := CT_AccountInfo_NUL.account_to_pay;
    account_target.accountInfo.new_publicKey := CT_AccountInfo_NUL.new_publicKey;
  end else begin
    account_target.accountInfo.state := as_ForSale;
    account_target.accountInfo.locked_until_block := FData.locked_until_block;
    account_target.accountInfo.price := FData.account_price;
    account_target.accountInfo.account_to_pay := FData.account_to_pay;
    account_target.accountInfo.new_publicKey := FData.new_public_key;
  end;
  Result := AccountTransaction.UpdateAccountInfo(AccountPreviousUpdatedBlock,FData.account_signer,FData.n_operation,FData.account_target,
         account_target.accountInfo,
         account_target.name,
         account_target.account_type,
         FData.fee,errors);
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

function TOpListAccount.IsPrivateSale: Boolean;
begin
  Result := (Not IsDelist) And (FData.new_public_key.EC_OpenSSL_NID<>0);
end;

function TOpListAccount.LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean;
var s : AnsiString;
  w : Word;
begin
  Result := false;
  if Stream.Size-Stream.Position < 14  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(w,2);
  case w of
    CT_Op_ListAccountForSale : FData.operation_type := lat_ListForSale;
    CT_Op_DelistAccount : FData.operation_type := lat_DelistAccount;
  else exit; // Invalid data info
  end;
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  if (FData.operation_type = lat_ListForSale) then begin
    Stream.Read(FData.account_price,Sizeof(FData.account_price));
    Stream.Read(FData.account_to_pay,Sizeof(FData.account_to_pay));
    if Stream.Read(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID))<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.x)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,FData.public_key.y)<0 then exit;
    if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
    FData.new_public_key := TAccountComp.RawString2Accountkey(s);
    Stream.Read(FData.locked_until_block,Sizeof(FData.locked_until_block));
  end;
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
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
    lat_ListForSale : begin
        if (FData.new_public_key.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
          OperationResume.Changers[0].Changes_type:=[list_for_public_sale];
        end else begin
          OperationResume.Changers[0].Changes_type:=[list_for_private_sale];
          OperationResume.Changers[0].New_Accountkey := FData.new_public_key;
          OperationResume.Changers[0].Locked_Until_Block := FData.locked_until_block;
        end;
        OperationResume.Changers[0].Seller_Account:=FData.account_to_pay;
        OperationResume.Changers[0].Account_Price:=FData.account_price;
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

function TOpListAccount.OperationPayload: TRawBytes;
begin
  Result := FData.payload;
end;

function TOpListAccount.SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean;
Var w : Word;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  case FData.operation_type of
    lat_ListForSale : w := CT_Op_ListAccountForSale;
    lat_DelistAccount : w := CT_Op_DelistAccount;
  else raise Exception.Create('ERROR DEV 20170412-1');
  end;
  Stream.Write(w,2);
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  if FData.operation_type=lat_ListForSale then begin
    Stream.Write(FData.account_price,Sizeof(FData.account_price));
    Stream.Write(FData.account_to_pay,Sizeof(FData.account_to_pay));
    Stream.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(Stream,FData.public_key.x);
    TStreamOp.WriteAnsiString(Stream,FData.public_key.y);
    TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(FData.new_public_key));
    Stream.Write(FData.locked_until_block,Sizeof(FData.locked_until_block));
  end;
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
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
    lat_ListForSale : Result := FData.account_to_pay;
  else Result:=inherited SellerAccount;
  end;
end;

function TOpListAccount.toString: String;
begin
  case FData.operation_type of
    lat_ListForSale : begin
      if (FData.new_public_key.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        Result := Format('List account %s for sale price %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
          TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
          FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
      end else begin
        Result := Format('List account %s for private sale price %s reserved for %s locked until block:%d fee:%s (n_op:%d) payload size:%d',[
          TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.account_price),
          TAccountComp.GetECInfoTxt(FData.new_public_key.EC_OpenSSL_NID),
          FData.locked_until_block, TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
      end;
    end;
    lat_DelistAccount : begin
      Result := Format('Delist account %s for sale fee:%s (n_op:%d) payload size:%d',[
        TAccountComp.AccountNumberToAccountTxtNumber(FData.account_target), TAccountComp.FormatMoney(FData.fee),
          FData.n_operation, Length(FData.payload)])
    end;
  else Result := 'ERROR DEV 20170414-2';
  end;
end;

function TOpListAccount.GetDigestToSign(current_protocol : Word): TRawBytes;
var ms : TMemoryStream;
  s : TRawBytes;
  b : Byte;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.account_signer,Sizeof(FData.account_signer));
    ms.Write(FData.account_target,Sizeof(FData.account_target));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.account_price,Sizeof(FData.account_price));
    ms.Write(FData.account_to_pay,Sizeof(FData.account_to_pay));
    ms.Write(FData.fee,Sizeof(FData.fee));
    if length(FData.payload)>0 then
      ms.WriteBuffer(FData.payload[1],length(FData.payload));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    if length(FData.public_key.x)>0 then
      ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
    if length(FData.public_key.y)>0 then
      ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
    s := TAccountComp.AccountKey2RawString(FData.new_public_key);
    if length(s)>0 then
      ms.WriteBuffer(s[1],length(s));
    ms.Write(FData.locked_until_block,Sizeof(FData.locked_until_block));
    if (current_protocol<=CT_PROTOCOL_3) then begin
      ms.Position := 0;
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
    end else begin
      b := OpType;
      ms.Write(b,1);
      Result := TCrypto.DoSha256(ms.Memory,ms.Size);
    end;
  finally
    ms.Free;
  end;
end;

{ TOpListAccountForSale }

constructor TOpListAccountForSale.CreateListAccountForSale(current_protocol : Word; account_signer, n_operation, account_target: Cardinal;
  account_price, fee: UInt64; account_to_pay: Cardinal;
  new_public_key: TAccountKey; locked_until_block: Cardinal; key: TECPrivateKey;
  payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_ListForSale;
  FData.n_operation := n_operation;
  FData.account_price := account_price;
  FData.account_to_pay := account_to_pay;
  FData.fee := fee;
  FData.payload := payload;
  // V2: No need to store public key because it's at safebox. Saving at least 64 bytes!
  // FData.public_key := key.PublicKey;
  FData.new_public_key := new_public_key;
  FData.locked_until_block := locked_until_block;

  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a new list account for sale operation');
    FHasValidSignature := false;
  end;
end;

function TOpListAccountForSale.IsDelist: Boolean;
begin
  Result := False;
end;

class function TOpListAccountForSale.OpType: Byte;
begin
  Result := CT_Op_ListAccountForSale;
end;

{ TOpDelistAccountForSale }

constructor TOpDelistAccountForSale.CreateDelistAccountForSale(current_protocol : Word; account_signer, n_operation, account_target: Cardinal; fee: UInt64; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
  FData.account_signer := account_signer;
  FData.account_target := account_target;
  FData.operation_type := lat_DelistAccount;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  if Assigned(key) then begin
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
  end else begin
    TLog.NewLog(ltdebug,Classname,'No key for signing a delist account operation');
    FHasValidSignature := false;
  end;
end;

function TOpDelistAccountForSale.IsDelist: Boolean;
begin
  Result := True;
end;

class function TOpDelistAccountForSale.OpType: Byte;
begin
  Result := CT_Op_DelistAccount;
end;

{ TOpBuyAccount }

constructor TOpBuyAccount.CreateBuy(current_protocol : Word; account_number, n_operation, account_to_buy,
  account_to_pay: Cardinal; price, amount, fee: UInt64;
  new_public_key: TAccountKey; key: TECPrivateKey; payload: TRawBytes);
begin
  inherited Create;
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
    FData.sign := TCrypto.ECDSASign(key.PrivateKey, GetDigestToSign(current_protocol));
    FHasValidSignature := true;
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

procedure TOpData.InitializeData;
begin
  inherited InitializeData;
  FData := CT_TOpDataData_NUL;
end;

function TOpData.SaveOpToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
begin
  Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Write(FData.account_sender,Sizeof(FData.account_sender));
  Stream.Write(FData.account_target,Sizeof(FData.account_target));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.dataType,Sizeof(FData.dataType));
  Stream.Write(FData.dataSequence,Sizeof(FData.dataSequence));
  Stream.Write(FData.amount,Sizeof(FData.amount));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  TStreamOp.WriteAnsiString(Stream,FData.payload);
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;

function TOpData.LoadOpFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
begin
  Result := false;
  if Stream.Size-Stream.Position < 36  then exit; // Invalid stream
  Stream.Read(FData.account_signer,Sizeof(FData.account_signer));
  Stream.Read(FData.account_sender,Sizeof(FData.account_sender));
  Stream.Read(FData.account_target,Sizeof(FData.account_target));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.dataType,Sizeof(FData.dataType));
  Stream.Read(FData.dataSequence,Sizeof(FData.dataSequence));
  Stream.Read(FData.amount,Sizeof(FData.amount));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  if TStreamOp.ReadAnsiString(Stream,FData.payload)<0 then exit;
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
  AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
Var account_signer, account_sender, account_target : TAccount;
begin
  Result := false;
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_4) then begin
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

  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
    Exit;
  end;

  If Not TCrypto.ECDSAVerify(account_signer.accountInfo.accountkey,GetDigestToSign(AccountTransaction.FreezedSafeBox.CurrentProtocol),FData.sign) then begin
    errors := 'Invalid sign';
    FHasValidSignature := false;
    Exit;
  end else FHasValidSignature := true;

  Result := AccountTransaction.TransferAmount(AccountPreviousUpdatedBlock,
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

function TOpData.OperationPayload: TRawBytes;
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

procedure TOpData.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account_signer));
  if (FData.account_signer<>FData.account_sender) then begin
    list.Add(TObject(FData.account_sender));
  end;
  if (FData.account_signer<>FData.account_target) And (FData.account_sender<>FData.account_target) then begin
    list.Add(TObject(FData.account_target));
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

constructor TOpData.CreateOpData(account_signer, account_sender,
  account_target: Cardinal; signer_key: TECPrivateKey; n_operation: Cardinal;
  dataType, dataSequence: Word; amount, fee: UInt64; payload: TRawBytes);
begin
  Inherited Create;
  FData.account_sender:=account_sender;
  FData.account_signer:=account_signer;
  FData.account_target:=account_target;
  FData.amount:=amount;
  FData.fee:=fee;
  FData.n_operation:=n_operation;
  FData.payload:=payload;
  FData.dataSequence:=dataSequence;
  FData.dataType:=dataType;
  if Assigned(signer_key) then begin
    FData.sign := TCrypto.ECDSASign(signer_key.PrivateKey, GetDigestToSign(CT_PROTOCOL_4));
    FHasValidSignature := true;
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

function TOpData.GetDigestToSign(current_protocol: Word): TRawBytes;
var Stream : TStream;
  b : Byte;
begin
  Stream := TMemoryStream.Create;
  Try
    Stream.Write(FData.account_signer,Sizeof(FData.account_signer));
    Stream.Write(FData.account_sender,Sizeof(FData.account_sender));
    Stream.Write(FData.account_target,Sizeof(FData.account_target));
    Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
    Stream.Write(FData.dataType,Sizeof(FData.dataType));
    Stream.Write(FData.dataSequence,Sizeof(FData.dataSequence));
    Stream.Write(FData.amount,Sizeof(FData.amount));
    Stream.Write(FData.fee,Sizeof(FData.fee));
    TStreamOp.WriteAnsiString(Stream,FData.payload);
    b := OpType;
    Stream.Write(b,1);
    Result := TStreamOp.SaveStreamToRaw(Stream);
  finally
    Stream.Free;
  end;
end;

initialization
  RegisterOperationsClass;
end.
