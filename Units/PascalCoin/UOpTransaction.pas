unit UOpTransaction;

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

Uses UCrypto, UBlockChain, Classes, UAccounts;

Type
  // Operations Type
  TOpTransactionData = Record
    sender: Cardinal;
    n_operation : Cardinal;
    target: Cardinal;
    amount: UInt64;
    fee: UInt64;
    payload: AnsiString;
    public_key: TECDSA_Public;
    sign: TECDSA_SIG;
  End;

  TOpChangeKeyData = Record
    account: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
    payload: AnsiString;
    public_key: TECDSA_Public;
    new_accountkey: TAccountKey;
    sign: TECDSA_SIG;
  End;

  TOpRecoverFoundsData = Record
    account: Cardinal;
    n_operation : Cardinal;
    fee: UInt64;
  End;

  { TOpTransaction }

  TOpTransaction = Class(TPCOperation)
  private
    FData : TOpTransactionData;
  public
    function GetOperationBufferToHash : TRawBytes; override;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function SaveToStream(Stream : TStream) : Boolean; override;
    function LoadFromStream(Stream : TStream) : Boolean; override;
    procedure AffectedAccounts(list : TList); override;
    //
    Class Function GetTransactionHasthToSign(const trans : TOpTransactionData) : TRawBytes;
    Class Function DoSignOperation(key : TECPrivateKey; var trans : TOpTransactionData) : Boolean;
    class function OpType : Byte; override;
    function OperationAmount : Int64; override;
    function OperationFee : UInt64; override;
    function OperationPayload : TRawBytes; override;
    function SenderAccount : Cardinal; override;
    function N_Operation : Cardinal; override;
    Property Data : TOpTransactionData read FData;

    Constructor Create(sender, n_operation, target: Cardinal; key: TECPrivateKey; amount, fee: UInt64; payload: AnsiString);
    Function toString : String; Override;
  End;

  { TOpChangeKey }

  TOpChangeKey = Class(TPCOperation)
  private
    FData : TOpChangeKeyData;
  public
    Class Function GetOperationHasthToSign(const op : TOpChangeKeyData) : TRawBytes;
    Class Function DoSignOperation(key : TECPrivateKey; var op : TOpChangeKeyData) : Boolean;
    class function OpType : Byte; override;

    function GetOperationBufferToHash : TRawBytes; override;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function SaveToStream(Stream : TStream) : Boolean; override;
    function LoadFromStream(Stream : TStream) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : UInt64; override;
    function OperationPayload : TRawBytes; override;
    function SenderAccount : Cardinal; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    Constructor Create(account_number, n_operation: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: AnsiString);
    Property Data : TOpChangeKeyData read FData;
    Function toString : String; Override;
  End;

  { TOpRecoverFounds }

  TOpRecoverFounds = Class(TPCOperation)
  private
    FData : TOpRecoverFoundsData;
  public
    class function OpType : Byte; override;

    function GetOperationBufferToHash : TRawBytes; override;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    function SaveToStream(Stream : TStream) : Boolean; override;
    function LoadFromStream(Stream : TStream) : Boolean; override;
    function OperationAmount : Int64; override;
    function OperationFee : UInt64; override;
    function OperationPayload : TRawBytes; override;
    function SenderAccount : Cardinal; override;
    function N_Operation : Cardinal; override;
    procedure AffectedAccounts(list : TList); override;
    Constructor Create(account_number, n_operation: Cardinal; fee: UInt64);
    Property Data : TOpRecoverFoundsData read FData;
    Function toString : String; Override;
  End;

Procedure RegisterOperationsClass;

implementation

uses
  SysUtils, UConst, ULog;

Procedure RegisterOperationsClass;
Begin
  TPCOperationsComp.RegisterOperationClass(TOpTransaction);
  TPCOperationsComp.RegisterOperationClass(TOpChangeKey);
  TPCOperationsComp.RegisterOperationClass(TOpRecoverFounds);
End;

{ TOpTransaction }

procedure TOpTransaction.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.sender));
  list.Add(TObject(FData.target));
end;

constructor TOpTransaction.Create(sender, n_operation, target: Cardinal;
  key: TECPrivateKey; amount, fee: UInt64; payload: AnsiString);
begin
  FData.sender := sender;
  FData.n_operation := n_operation;
  FData.target := target;
  FData.amount := amount;
  FData.fee := fee;
  FData.payload := payload;
  FData.public_key := key.PublicKey;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a new Transaction');
  end;
end;

function TOpTransaction.DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString): Boolean;
Var s_new, t_new : Int64;
  totalamount : Cardinal;
  sender,target : TAccount;
  _h : TRawBytes;
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
  // Check signature
  _h := GetTransactionHasthToSign(FData);
  if (Not TCrypto.ECDSAVerify(FData.public_key,_h,FData.sign)) then begin
    errors := 'Invalid sign';
    Exit;
  end;
  //
  FPrevious_Sender_updated_block := sender.updated_block;
  FPrevious_Destination_updated_block := target.updated_block;
  // Do operation
  Result := AccountTransaction.TransferAmount(FData.sender,FData.target,FData.n_operation,FData.amount,FData.fee,errors);
end;

class function TOpTransaction.DoSignOperation(key : TECPrivateKey; var trans : TOpTransactionData) : Boolean;
var s : AnsiString;
  _sign : TECDSA_SIG;
begin
  If Not Assigned(key.PrivateKey) then begin
    Result := false;
    trans.sign.r:='';
    trans.sign.s:='';
    exit;
  end;
  s := GetTransactionHasthToSign(trans);
  Try
    _sign := TCrypto.ECDSASign(key.PrivateKey,s);
    trans.sign := _sign;
    Result := true;
  Except
    trans.sign.r:='';
    trans.sign.s:='';
    Result := false;
  End;
  SetLength(s,0);
end;

function TOpTransaction.GetOperationBufferToHash: TRawBytes;
Var ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.sender,Sizeof(FData.sender));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.target,Sizeof(FData.target));
    ms.Write(FData.amount,Sizeof(FData.amount));
    ms.Write(FData.fee,Sizeof(FData.fee));
    ms.WriteBuffer(FData.payload[1],length(FData.payload));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
    ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
    ms.WriteBuffer(FData.sign.r[1],length(FData.sign.r));
    ms.WriteBuffer(FData.sign.s[1],length(FData.sign.s));
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

class function TOpTransaction.GetTransactionHasthToSign(const trans: TOpTransactionData): TRawBytes;
Var ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(trans.sender,Sizeof(trans.sender));
    ms.Write(trans.n_operation,Sizeof(trans.n_operation));
    ms.Write(trans.target,Sizeof(trans.target));
    ms.Write(trans.amount,Sizeof(trans.amount));
    ms.Write(trans.fee,Sizeof(trans.fee));
    ms.WriteBuffer(trans.payload[1],length(trans.payload));
    ms.Write(trans.public_key.EC_OpenSSL_NID,Sizeof(trans.public_key.EC_OpenSSL_NID));
    ms.WriteBuffer(trans.public_key.x[1],length(trans.public_key.x));
    ms.WriteBuffer(trans.public_key.y[1],length(trans.public_key.y));
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

function TOpTransaction.LoadFromStream(Stream: TStream): Boolean;
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
  if TStreamOp.ReadAnsiString(Stream,FData.sign.r)<0 then exit;
  if TStreamOp.ReadAnsiString(Stream,FData.sign.s)<0 then exit;
  Result := true;
end;

function TOpTransaction.OperationAmount: Int64;
begin
  Result := FData.amount;
end;

function TOpTransaction.OperationFee: UInt64;
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

function TOpTransaction.SaveToStream(Stream: TStream): Boolean;
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
  TStreamOp.WriteAnsiString(Stream,FData.sign.r);
  TStreamOp.WriteAnsiString(Stream,FData.sign.s);
  Result := true;
end;


function TOpTransaction.SenderAccount: Cardinal;
begin
  Result := FData.sender;
end;

function TOpTransaction.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpTransaction.toString: String;
begin
  Result := Format('Transaction from %s to %s amount:%s fee:%s (n_op:%d) payload size:%d',[
     TAccountComp.AccountNumberToAccountTxtNumber(FData.sender),
     TAccountComp.AccountNumberToAccountTxtNumber(FData.target),
     TAccountComp.FormatMoney(FData.amount),TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;

{ TOpChangeKey }

procedure TOpChangeKey.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account));
end;

constructor TOpChangeKey.Create(account_number, n_operation: Cardinal; key:TECPrivateKey; new_account_key : TAccountKey; fee: UInt64; payload: AnsiString);
begin
  FData.account := account_number;
  FData.n_operation := n_operation;
  FData.fee := fee;
  FData.payload := payload;
  FData.public_key := key.PublicKey;
  FData.new_accountkey := new_account_key;
  If Not DoSignOperation(key,FData) then begin
    TLog.NewLog(lterror,Classname,'Error signing a new Change key');
  end;
end;

function TOpChangeKey.DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
Var account : TAccount;
begin
  Result := false;
  if (FData.account>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
    errors := 'Invalid account number';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(FData.account, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  if (FData.fee<0) Or (FData.fee>CT_MaxTransactionFee) then begin
    errors := 'Invalid fee: '+Inttostr(FData.fee);
    exit;
  end;
  account := AccountTransaction.Account(FData.account);
  if ((account.n_operation+1)<>FData.n_operation) then begin
    errors := 'Invalid n_operation';
    Exit;
  end;
  if (account.balance<FData.fee) then begin
    errors := 'Insuficient founds';
    exit;
  end;
  if (length(FData.payload)>CT_MaxPayloadSize) then begin
    errors := 'Invalid Payload size:'+inttostr(length(FData.payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
  end;
  If Not TAccountComp.IsValidAccountKey( FData.new_accountkey, errors ) then begin
    exit;
  end;
  If Not TCrypto.ECDSAVerify(FData.public_key,GetOperationHasthToSign(FData),FData.sign) then begin
    errors := 'Invalid sign';
    exit;
  end;
  FPrevious_Sender_updated_block := account.updated_block;
  Result := AccountTransaction.UpdateAccountkey(FData.account,FData.n_operation,FData.new_accountkey,FData.fee,errors);
end;

class function TOpChangeKey.DoSignOperation(key: TECPrivateKey; var op: TOpChangeKeyData): Boolean;
var s : AnsiString;
  _sign : TECDSA_SIG;
begin
  s := GetOperationHasthToSign(op);
  Try
    _sign := TCrypto.ECDSASign(key.PrivateKey,s);
    op.sign := _sign;
    Result := true;
  Except
    On E:Exception do begin
      Result := false;
      TLog.NewLog(lterror,ClassName,'Error signing ChangeKey operation: '+E.Message);
    end;
  End;
end;

function TOpChangeKey.GetOperationBufferToHash: TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FData.account,Sizeof(FData.account));
    ms.Write(FData.n_operation,Sizeof(FData.n_operation));
    ms.Write(FData.fee,Sizeof(FData.fee));
    ms.WriteBuffer(FData.payload[1],length(FData.payload));
    ms.Write(FData.public_key.EC_OpenSSL_NID,Sizeof(FData.public_key.EC_OpenSSL_NID));
    ms.WriteBuffer(FData.public_key.x[1],length(FData.public_key.x));
    ms.WriteBuffer(FData.public_key.y[1],length(FData.public_key.y));
    s := TAccountComp.AccountKey2RawString(FData.new_accountkey);
    ms.WriteBuffer(s[1],length(s));
    ms.WriteBuffer(FData.sign.r[1],length(FData.sign.r));
    ms.WriteBuffer(FData.sign.s[1],length(FData.sign.s));
    ms.Position := 0;
    setlength(Result,ms.Size);
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

class function TOpChangeKey.GetOperationHasthToSign(const op: TOpChangeKeyData): TRawBytes;
var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(op.account,Sizeof(op.account));
    ms.Write(op.n_operation,Sizeof(op.n_operation));
    ms.Write(op.fee,Sizeof(op.fee));
    ms.WriteBuffer(op.payload[1],length(op.payload));
    ms.Write(op.public_key.EC_OpenSSL_NID,Sizeof(op.public_key.EC_OpenSSL_NID));
    ms.WriteBuffer(op.public_key.x[1],length(op.public_key.x));
    ms.WriteBuffer(op.public_key.y[1],length(op.public_key.y));
    s := TAccountComp.AccountKey2RawString(op.new_accountkey);
    ms.WriteBuffer(s[1],length(s));
    ms.Position := 0;
    setlength(Result,ms.Size);
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;


function TOpChangeKey.LoadFromStream(Stream: TStream): Boolean;
var s : AnsiString;
begin
  Result := false;
  if Stream.Size-Stream.Position < 16  then exit; // Invalid stream
  Stream.Read(FData.account,Sizeof(FData.account));
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

function TOpChangeKey.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpChangeKey.OperationFee: UInt64;
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

function TOpChangeKey.SaveToStream(Stream: TStream): Boolean;
begin
  Stream.Write(FData.account,Sizeof(FData.account));
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

function TOpChangeKey.SenderAccount: Cardinal;
begin
  Result := FData.account;
end;

function TOpChangeKey.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpChangeKey.toString: String;
begin
  Result := Format('Change key of %s to new key: %s fee:%s (n_op:%d) payload size:%d',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account),
    TAccountComp.GetECInfoTxt(FData.new_accountkey.EC_OpenSSL_NID),
    TAccountComp.FormatMoney(FData.fee),FData.n_operation,Length(FData.payload)]);
end;

{ TOpRecoverFounds }

procedure TOpRecoverFounds.AffectedAccounts(list: TList);
begin
  list.Add(TObject(FData.account));
end;

constructor TOpRecoverFounds.Create(account_number, n_operation : Cardinal; fee: UInt64);
begin
  FData.account := account_number;
  FData.n_operation := n_operation;
  FData.fee := fee;
end;

function TOpRecoverFounds.DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
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
  FPrevious_Sender_updated_block := acc.updated_block;
  Result := AccountTransaction.TransferAmount(FData.account,FData.account,FData.n_operation,0,FData.fee,errors);
end;

function TOpRecoverFounds.GetOperationBufferToHash: TRawBytes;
var ms : TMemoryStream;
begin
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

function TOpRecoverFounds.LoadFromStream(Stream: TStream): Boolean;
begin
  Result := false;
  if Stream.Size - Stream.Position<16 then exit;
  Stream.Read(FData.account,Sizeof(FData.account));
  Stream.Read(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Read(FData.fee,Sizeof(FData.fee));
  Result := true;
end;

function TOpRecoverFounds.OperationAmount: Int64;
begin
  Result := 0;
end;

function TOpRecoverFounds.OperationFee: UInt64;
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

function TOpRecoverFounds.SaveToStream(Stream: TStream): Boolean;
begin
  Stream.Write(FData.account,Sizeof(FData.account));
  Stream.Write(FData.n_operation,Sizeof(FData.n_operation));
  Stream.Write(FData.fee,Sizeof(FData.fee));
  Result := true;
end;

function TOpRecoverFounds.SenderAccount: Cardinal;
begin
  Result := FData.account;
end;

function TOpRecoverFounds.N_Operation: Cardinal;
begin
  Result := FData.n_operation;
end;

function TOpRecoverFounds.toString: String;
begin
  Result := Format('Recover founds of account %s fee:%s (n_op:%d)',[
    TAccountComp.AccountNumberToAccountTxtNumber(FData.account),
    TAccountComp.FormatMoney(FData.fee),fData.n_operation]);
end;

initialization
  RegisterOperationsClass;
end.
