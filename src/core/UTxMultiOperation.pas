unit UTxMultiOperation;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{ Copyright (c) 2018 by Albert Molina - PascalCoin developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, SysUtils, UCrypto, UBlockChain, UAccounts;

Type

  // NEW OPERATIONS PROTOCOL 3

  { TOpMultiOperation }

  {
    Based on PIP-0017, proposed by Herman Schoenfeld <herman@sphere10.com>

    Will include multiple operations (tx, changes of info...) in a single operation

    Those operations could be:
      - Send from N accounts to M receivers,  (transaction mixing, with anonymity)
      - Change account info:
        - Change account public key
        - Change account type
        - Change account name

    This operation will work as a TRANSACTION operation, so, will execute ALL opererations or NONE

    Also, can be signed off-line by all senders/signers, knowing previously the OpHash because
    the OpHash algo will not include the signature (due it's checked separately)

    ALLOWED:
    - N senders to M receivers
    - Each sender can add a Payload
    - Each receiver can receive a Payload
    - Allow receive multiple incomes in a single receiver. For example:
      - Account A sends X coins to account B three times (X1,X2,X3) using Payload to discriminate (3 operations with only 1 signer, account A).
        - Example: Pool sending rewards to an exchange. Only 1 sender and to 1 receiver but multiple times, each time with a distinct amount/payload
        - Sender: Account A, amount X
        - Receivers (three times same account):
           - Account B, amount X1, Payload value P1
           - Account B, amount X2, Payload value P2
           - Account B, amount X3, Payload value P3
        - X1+X2+X3 <= X  (Fee is X - (X1+X2+X3))
        - Affected accounts are only A and B
        - There is only 1 signature: Account A (less space!)

    LIMITATIONS:
    - At least 1 operation must be made (1 change info, or 1 send + 1 receive)
    - Obvious: Total amount sent >= total amount received
      - Operation fee will be (total sent - total received)
      - If no send/receive operation, then there is no fee in multioperation
    - Senders cannot be duplicated (sender A can send only 1 time)
    - Senders must have previously enough amount to send: Example
      - Account A had 0 coins previously
      - Account A receive X coins in this multioperation
      - Account A sends X coins in this multioperation: NOT POSSIBLE, no previous funds
    - Change account op cannot be duplicated
    - Cannot change name between 2 accounts: Example
      - Account A has name X  (X not null)
      - Account B has name Y  (Y not null)
      - Cannot change name X to B and Y to A at same multioperation
    - Senders cannot be both in change account info and viceversa: Example
      - Account A sends X
      - Account A changes info
      - Not allowed (same sender and same change info account)
    - When a transfer is made to an account in private sale mode, the receiver account
      will not execute a change private key, so sale will not be executed
  }

  TOpMultiOperationData = Record
    // senders are unique and not duplicable
    txSenders : TMultiOpSenders;
    // receivers can be duplicated
    txReceivers : TMultiOpReceivers;
    // changers are unique and not duplicable
    changesInfo : TMultiOpChangesInfo;
  end;

  TOpMultiOperation = Class(TPCOperation)
  private
    FData : TOpMultiOperationData;
    //
    FSaveSignatureValue : Boolean;
    FTotalAmount : Int64;
    FTotalFee : Int64;
    Function IndexOfAccountChangeNameTo(const newName : AnsiString) : Integer;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); override;
  public
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;

    function CheckSignatures(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;

    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    procedure AffectedAccounts(list : TList); override;
    //
    Function GetTransactionHashToSign : TRawBytes;
    Function DoSignMultiOperationSigner(SignerAccount : Cardinal; key : TECPrivateKey) : Integer;
    class function OpType : Byte; override;
    function OperationAmount : Int64; override;
    function OperationFee : UInt64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    procedure SignerAccounts(list : TList); override;
    function IsSignerAccount(account : Cardinal) : Boolean; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    function GetAccountN_Operation(account : Cardinal) : Cardinal; override;
    function OperationAmountByAccount(account : Cardinal) : Int64; override;
    //
    Constructor CreateMultiOperation(const senders : TMultiOpSenders; const receivers : TMultiOpReceivers; const changes : TMultiOpChangesInfo; const senders_keys, changes_keys: Array of TECPrivateKey);
    Destructor Destroy; override;
    Function AddTx(const senders : TMultiOpSenders; const receivers : TMultiOpReceivers; setInRandomOrder : Boolean) : Boolean;
    Function AddChangeInfos(const changes : TMultiOpChangesInfo; setInRandomOrder : Boolean) : Boolean;
    Function AddTxSender(const sender : TMultiOpSender) : Boolean;
    Function AddTxReceiver(const receiver : TMultiOpReceiver) : Boolean;
    Function AddChangeInfo(const changeInfo : TMultiOpChangeInfo) : Boolean;
    //
    Function IndexOfAccountSender(nAccount : Cardinal) : Integer; overload;
    class Function IndexOfAccountSender(nAccount : Cardinal; startPos : Integer; const txSenders : TMultiOpSenders) : Integer; overload;
    Function IndexOfAccountReceiver(nAccount : Cardinal; startPos : Integer) : Integer;
    Function IndexOfAccountChanger(nAccount : Cardinal) : Integer; overload;
    class Function IndexOfAccountChanger(nAccount : Cardinal; startPos : Integer; const changesInfo : TMultiOpChangesInfo) : Integer; overload;
    class Function OpChangeAccountInfoTypesToText(const OpChangeAccountInfoTypes : TOpChangeAccountInfoTypes) : AnsiString;
    //
    Function toString : String; Override;
    Property Data : TOpMultiOperationData read FData;
  End;

implementation

Uses ULog, UConst;

{ TOpMultiOperation }

function TOpMultiOperation.IndexOfAccountSender(nAccount: Cardinal): Integer;
begin
  Result := IndexOfAccountSender(nAccount,0,FData.txSenders);
end;

class function TOpMultiOperation.IndexOfAccountSender(nAccount: Cardinal; startPos : Integer; const txSenders: TMultiOpSenders): Integer;
begin
  for Result:=startPos to high(txSenders) do begin
    If (txSenders[Result].Account = nAccount) then exit;
  end;
  Result := -1;
end;

function TOpMultiOperation.IndexOfAccountReceiver(nAccount: Cardinal; startPos : Integer): Integer;
begin
  if startPos<Low(FData.txReceivers) then startPos := Low(FData.txReceivers);
  for Result:=startPos to high(FData.txReceivers) do begin
    If (FData.txReceivers[Result].Account = nAccount) then exit;
  end;
  Result := -1;
end;

function TOpMultiOperation.IndexOfAccountChanger(nAccount: Cardinal): Integer;
begin
  Result := IndexOfAccountChanger(nAccount,0,FData.changesInfo);
end;

class function TOpMultiOperation.IndexOfAccountChanger(nAccount: Cardinal; startPos : Integer; const changesInfo: TMultiOpChangesInfo): Integer;
begin
  for Result:=startPos to high(changesInfo) do begin
    If (changesInfo[Result].Account = nAccount) then exit;
  end;
  Result := -1;
end;

class function TOpMultiOperation.OpChangeAccountInfoTypesToText(const OpChangeAccountInfoTypes: TOpChangeAccountInfoTypes): AnsiString;
Var opcit : TOpChangeAccountInfoType;
begin
  Result := '';
  for opcit:=Low(opcit) to High(opcit) do begin
    if opcit in OpChangeAccountInfoTypes then begin
      If Result<>'' then Result := Result +',';
      Result := Result + CT_TOpChangeAccountInfoType_Txt[opcit];
    end;
  end;
end;

procedure TOpMultiOperation.FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume);
Var iSender,iReceiver,iChanger : Integer;
  changerTxt : AnsiString;
begin
  inherited FillOperationResume(Block, getInfoForAllAccounts, Affected_account_number, OperationResume);
  OperationResume.isMultiOperation:=True;

  OperationResume.Senders := FData.txSenders;
  OperationResume.Receivers := FData.txReceivers;
  OperationResume.Changers := FData.changesInfo;
  if (getInfoForAllAccounts) then begin
    OperationResume.OperationTxt := ToString;
    OperationResume.Amount := OperationAmount;
    OperationResume.Fee := OperationFee * (-1);
    OperationResume.OpSubtype := CT_OpSubtype_MultiOperation_Global;
  end else begin
    OperationResume.OpSubtype := CT_OpSubtype_MultiOperation_AccountInfo;
    OperationResume.Fee := 0;
    OperationResume.Amount := OperationAmountByAccount(Affected_account_number);
    // Set Text and OpSubtype based on Affected_account_number
    iSender := (IndexOfAccountSender(Affected_account_number));
    iReceiver := (IndexOfAccountReceiver(Affected_account_number,0));
    iChanger := (IndexOfAccountChanger(Affected_account_number));
    if (iChanger>=0) then begin
      changerTxt:='Changes ['+OpChangeAccountInfoTypesToText(FData.changesInfo[iChanger].Changes_type)+']';
    end else changerTxt:='';
    if (iSender>=0) then begin
      // Is a Sender account
      OperationResume.OperationTxt:='Multi Tx-Out '+TAccountComp.FormatMoney(OperationResume.Amount * (-1))+' PASC from '+TAccountComp.AccountNumberToAccountTxtNumber(Affected_account_number)+' '+changerTxt;
    end else if (iReceiver>=0) then begin
      OperationResume.OperationTxt:='Multi Tx-In '+TAccountComp.FormatMoney(OperationResume.Amount)+' PASC to '+TAccountComp.AccountNumberToAccountTxtNumber(Affected_account_number)+' '+changerTxt;
    end else begin
      OperationResume.OperationTxt:='Multi '+changerTxt+' to '+TAccountComp.AccountNumberToAccountTxtNumber(Affected_account_number);
    end;
  end;
end;

function TOpMultiOperation.IndexOfAccountChangeNameTo(const newName: AnsiString): Integer;
begin
  If (newName<>'') then begin
    for Result:=0 to high(FData.changesInfo) do begin
      If (account_name in FData.changesInfo[Result].Changes_type) And
         (AnsiCompareText(FData.changesInfo[Result].New_Name,newName)=0) then exit;
    end;
  end;
  Result := -1;
end;

procedure TOpMultiOperation.InitializeData;
begin
  inherited InitializeData;
  SetLength(FData.txSenders,0);
  SetLength(FData.txReceivers,0);
  SetLength(FData.changesInfo,0);
  FSaveSignatureValue := True;
  FTotalAmount:=0;
  FTotalFee:=0;
end;

function TOpMultiOperation.SaveOpToStream(Stream: TStream; SaveExtendedData: Boolean): Boolean;
var i : Integer;
  w : Word;
  txs : TMultiOpSender;
  txr : TMultiOpReceiver;
  chi : TMultiOpChangeInfo;
  b : Byte;
begin
  // Will save protocol info
  w := CT_PROTOCOL_3;
  stream.Write(w,SizeOf(w));
  // Save senders count
  w := Length(FData.txSenders);
  stream.Write(w,SizeOf(w));
  for i:=0 to length(FData.txSenders)-1 do begin
    txs := FData.txSenders[i];
    stream.Write(txs.Account,SizeOf(txs.Account));
    stream.Write(txs.Amount,SizeOf(txs.Amount));
    stream.Write(txs.N_Operation,SizeOf(txs.N_Operation));
    TStreamOp.WriteAnsiString(stream,txs.Payload);
    If FSaveSignatureValue then begin
      TStreamOp.WriteAnsiString(stream,txs.Signature.r);
      TStreamOp.WriteAnsiString(stream,txs.Signature.s);
    end;
  end;
  // Save receivers count
  w := Length(FData.txReceivers);
  stream.Write(w,SizeOf(w));
  for i:=0 to length(FData.txReceivers)-1 do begin
    txr := FData.txReceivers[i];
    stream.Write(txr.Account,SizeOf(txr.Account));
    stream.Write(txr.Amount,SizeOf(txr.Amount));
    TStreamOp.WriteAnsiString(stream,txr.Payload);
  end;
  // Save changes info count
  w := Length(FData.changesInfo);
  stream.Write(w,SizeOf(w));
  for i:=0 to length(FData.changesInfo)-1 do begin
    chi := FData.changesInfo[i];
    stream.Write(chi.Account,SizeOf(chi.Account));
    Stream.Write(chi.N_Operation,Sizeof(chi.N_Operation));
    b := 0;
    if (public_key in chi.Changes_type) then b:=b OR $01;
    if (account_name in chi.changes_type) then b:=b OR $02;
    if (account_type in chi.changes_type) then b:=b OR $04;
    Stream.Write(b,Sizeof(b));
    TStreamOp.WriteAccountKey(Stream,chi.New_Accountkey);
    TStreamOp.WriteAnsiString(Stream,chi.New_Name);
    Stream.Write(chi.New_Type,Sizeof(chi.New_Type));
    If FSaveSignatureValue then begin
      TStreamOp.WriteAnsiString(Stream,chi.Signature.r);
      TStreamOp.WriteAnsiString(Stream,chi.Signature.s);
    end;
  end;
  Result := true;
end;

function TOpMultiOperation.LoadOpFromStream(Stream: TStream; LoadExtendedData: Boolean): Boolean;
var i : Integer;
  w : Word;
  txs : TMultiOpSender;
  txr : TMultiOpReceiver;
  chi : TMultiOpChangeInfo;
  b : Byte;
  txsenders : TMultiOpSenders;
  txreceivers : TMultiOpReceivers;
  changes : TMultiOpChangesInfo;
begin
  // Clear all data!
  SetLength(FData.txSenders,0);
  SetLength(FData.txReceivers,0);
  SetLength(FData.changesInfo,0);
  FTotalAmount:=0;
  FTotalFee:=0;
  FHasValidSignature:=False;

  SetLength(txsenders,0);
  SetLength(txreceivers,0);
  SetLength(changes,0);

  Result := False;
  Try
    // Read protocol info
    stream.Read(w,SizeOf(w));
    If w<>CT_PROTOCOL_3 then Raise Exception.Create('Invalid protocol found');
    // Load senders
    stream.Read(w,SizeOf(w));
    If w>CT_MAX_MultiOperation_Senders then Raise Exception.Create('Max senders');
    setLength(txsenders,w);
    If (w>0) then begin
      for i:=0 to w-1 do begin
        txs := CT_TMultiOpSender_NUL;
        stream.Read(txs.Account,SizeOf(txs.Account));
        stream.Read(txs.Amount,SizeOf(txs.Amount));
        stream.Read(txs.N_Operation,SizeOf(txs.N_Operation));
        TStreamOp.ReadAnsiString(stream,txs.Payload);
        TStreamOp.ReadAnsiString(stream,txs.Signature.r);
        TStreamOp.ReadAnsiString(stream,txs.Signature.s);
        //
        txsenders[i] := txs;
      end;
    end;
    // Load receivers
    stream.Read(w,SizeOf(w));
    If w>CT_MAX_MultiOperation_Receivers then Raise Exception.Create('Max receivers');
    SetLength(txreceivers,w);
    If (w>0) then begin
      for i:=0 to w-1 do begin
        txr := CT_TMultiOpReceiver_NUL;
        stream.Read(txr.Account,SizeOf(txr.Account));
        stream.Read(txr.Amount,SizeOf(txr.Amount));
        TStreamOp.ReadAnsiString(stream,txr.Payload);
        //
        txreceivers[i] := txr;
      end;
    end;
    // Load changes info
    stream.Read(w,SizeOf(w));
    If w>CT_MAX_MultiOperation_Changers then Raise Exception.Create('Max changers');
    SetLength(changes,w);
    if (w>0) then begin
      for i:=0 to w-1 do begin
        chi := CT_TMultiOpChangeInfo_NUL;
        stream.Read(chi.Account,SizeOf(chi.Account));
        Stream.Read(chi.N_Operation,Sizeof(chi.N_Operation));
        Stream.Read(b,Sizeof(b));
        chi.Changes_type:=[];
        if (b AND $01)=$01 then chi.changes_type:=chi.changes_type + [public_key];
        if (b AND $02)=$02 then chi.changes_type:=chi.changes_type + [account_name];
        if (b AND $04)=$04 then chi.changes_type:=chi.changes_type + [account_type];
        // Check
        if (b AND $F8)<>0 then Exit;
        TStreamOp.ReadAccountKey(Stream,chi.New_Accountkey);
        TStreamOp.ReadAnsiString(Stream,chi.New_Name);
        Stream.Read(chi.New_Type,Sizeof(chi.New_Type));
        TStreamOp.ReadAnsiString(Stream,chi.Signature.r);
        TStreamOp.ReadAnsiString(Stream,chi.Signature.s);
        //
        changes[i]:=chi;
      end;
    end;
    Result := AddTx(txsenders,txreceivers,False); // Important: Set in same order!
    Result := Result And AddChangeInfos(changes,False); // Important: Set in same order!
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Self.ClassName,'('+E.ClassName+'):'+E.Message);
      Result := False;
    end;
  end;
end;

function TOpMultiOperation.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
var old : Boolean;
begin
  // Note: Will set FIsBufferingForOpHash to true because we don't want to hash the signature value
  old := FSaveSignatureValue;
  try
    FSaveSignatureValue := False;
    Result:=inherited GetBufferForOpHash(True);
  finally
    FSaveSignatureValue:=old;
  end;
end;

function TOpMultiOperation.CheckSignatures(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
var i : Integer;
  acc : TAccount;
  ophtosign : TRawBytes;
begin
  // Init
  FHasValidSignature:=False;
  SetLength(errors,0);
  // Do check it!
  Try
    ophtosign := GetTransactionHashToSign;
    // Tx verification
    For i:=Low(FData.txSenders) to High(FData.txSenders) do begin
      acc := AccountTransaction.Account(FData.txSenders[i].Account);
      If (length(FData.txSenders[i].Signature.r)>0) And
         (length(FData.txSenders[i].Signature.s)>0) then begin
        If Not TCrypto.ECDSAVerify(acc.accountInfo.accountkey,ophtosign,FData.txSenders[i].Signature) then begin
          errors := Format('Invalid signature for sender %d/%d',[i+1,length(FData.txSenders)]);
          Exit;
        end;
      end else begin
        errors := Format('sender not signed %d/%d',[i+1,length(FData.txSenders)]);
        Exit;
      end;
    end;
    // Change verification
    For i:=Low(FData.changesInfo) to High(FData.changesInfo) do begin
      acc := AccountTransaction.Account(FData.changesInfo[i].Account);
      If (length(FData.changesInfo[i].Signature.r)>0) And
         (length(FData.changesInfo[i].Signature.s)>0) then begin
        If Not TCrypto.ECDSAVerify(acc.accountInfo.accountkey,ophtosign,FData.changesInfo[i].Signature) then begin
          errors := Format('Invalid signature for change info %d/%d',[i+1,length(FData.changesInfo)]);
          Exit;
        end;
      end else begin
        errors := Format('change info not signed %d/%d',[i+1,length(FData.changesInfo)]);
        Exit;
      end;
    end;
    // If here... all Ok
    FHasValidSignature:=True;
  finally
    Result := FHasValidSignature;
  end;
end;

function TOpMultiOperation.DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
var i,j : Integer;
  txs : TMultiOpSender;
  txr : TMultiOpReceiver;
  chi : TMultiOpChangeInfo;
  sender,receiver,changer : TAccount;
  newNameWasAdded, newNameWasDeleted : Boolean;
  senders,senders_n_operation,receivers : Array of Cardinal;
  senders_amount : Array of UInt64;
  receivers_amount : Array of UInt64;
begin
  // Check valid info:
  Result := False;
  errors := '';
  if (AccountTransaction.FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_3) then begin
    errors := 'NEED PROTOCOL 3';
    exit;
  end;
  if (FTotalAmount<0) Or (FTotalFee<0) then begin
    errors := 'Invalid Amount or Fee';
    Exit;
  end;
  if ((length(FData.txReceivers)=0) And (length(FData.txSenders)=0) And (length(FData.changesInfo)=0))
     Or
     ((length(FData.txSenders)=0) XOR (length(FData.txReceivers)=0))  // Both must be 0 length or length>0
    then begin
    errors := 'Invalid receivers/senders/changesinfo length';
    Exit;
  end;
  SetLength(senders,Length(FData.txSenders));
  SetLength(senders_amount,Length(FData.txSenders));
  SetLength(senders_n_operation,Length(FData.txSenders));
  SetLength(receivers,Length(FData.txReceivers));
  SetLength(receivers_amount,Length(FData.txReceivers));
  // Check senders accounts:
  for i:=low(FData.txSenders) to high(FData.txSenders) do begin
    txs := FData.txSenders[i];
    senders[i] := txs.Account;
    senders_amount[i] := txs.Amount;
    senders_n_operation[i] := txs.N_Operation;
    if (txs.Account>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := Format('Invalid sender %d',[txs.Account]);
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(txs.Account,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := Format('sender (%d) is blocked for protocol',[txs.Account]);
      Exit;
    end;
    if (txs.Amount<=0) Or (txs.Amount>CT_MaxTransactionAmount) then begin
      errors := Format('Invalid amount %d (0 or max: %d)',[txs.Amount,CT_MaxTransactionAmount]);
      Exit;
    end;
    if (length(txs.Payload)>CT_MaxPayloadSize) then begin
      errors := 'Invalid Payload size:'+inttostr(length(txs.Payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
      Exit;
    end;
    //
    sender := AccountTransaction.Account(txs.Account);
    if ((sender.n_operation+1)<>txs.N_Operation) then begin
      errors := Format('Invalid n_operation %d (expected %d)',[txs.N_Operation,sender.n_operation+1]);
      Exit;
    end;
    if (sender.balance<txs.Amount) then begin
      errors := Format('Insufficient funds account %d %d < %d',[sender.account, sender.balance,txs.Amount]);
      Exit;
    end;
    // Is locked? Protocol 2 check
    if (TAccountComp.IsAccountLocked(sender.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Sender Account is currently locked';
      exit;
    end;
  end;
  // Check receivers accounts:
  for i:=Low(FData.txReceivers) to High(FData.txReceivers) do begin
    txr := FData.txReceivers[i];
    receivers[i] := txr.Account;
    receivers_amount[i] := txr.Amount;
    if (txr.Account>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := Format('Invalid receiver %d',[txr.Account]);
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(txr.Account,AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := Format('receiver (%d) is blocked for protocol',[txr.Account]);
      Exit;
    end;
    if (txr.Amount<=0) Or (txr.Amount>CT_MaxTransactionAmount) then begin
      errors := Format('Invalid amount %d (0 or max: %d)',[txr.Amount,CT_MaxTransactionAmount]);
      Exit;
    end;
    if (length(txr.Payload)>CT_MaxPayloadSize) then begin
      errors := 'Invalid Payload size:'+inttostr(length(txr.Payload))+' (Max: '+inttostr(CT_MaxPayloadSize)+')';
      Exit;
    end;
    //
    receiver := AccountTransaction.Account(txr.Account);
    if (receiver.balance+txr.Amount>CT_MaxWalletAmount) then begin
      errors := Format('Target cannot accept this transaction due to max amount %d+%d=%d > %d',[receiver.balance,txr.Amount,receiver.balance+txr.Amount,CT_MaxWalletAmount]);
      Exit;
    end;
  end;
  // Check change info accounts:
  for i:=Low(FData.changesInfo) to High(FData.changesInfo) do begin
    chi := FData.changesInfo[i];
    if (chi.Account>=AccountTransaction.FreezedSafeBox.AccountsCount) then begin
      errors := 'Invalid change info account number';
      Exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(chi.Account, AccountTransaction.FreezedSafeBox.BlocksCount) then begin
      errors := 'change info account is blocked for protocol';
      Exit;
    end;

    changer := AccountTransaction.Account(chi.Account);
    if ((changer.n_operation+1)<>chi.N_Operation) then begin
      errors := 'Invalid changer n_operation';
      Exit;
    end;
    // Is locked? Protocol 2 check
    if (TAccountComp.IsAccountLocked(changer.accountInfo,AccountTransaction.FreezedSafeBox.BlocksCount)) then begin
      errors := 'Account changer is currently locked';
      exit;
    end;
    If (public_key in chi.Changes_type) then begin
      If Not TAccountComp.IsValidAccountKey( chi.New_Accountkey, errors ) then begin
        Exit;
      end;
    end;
    If (account_name in chi.changes_type) then begin
      If (chi.New_Name<>'') then begin
        If Not TPCSafeBox.ValidAccountName(chi.New_Name,errors) then Exit;
        // Check name not found!
        j := AccountTransaction.FindAccountByNameInTransaction(chi.New_Name,newNameWasAdded, newNameWasDeleted);
        If (j>=0) Or (newNameWasAdded) or (newNameWasDeleted) then begin
          errors := 'New name is in use or was added or deleted at same transaction';
          Exit;
        end;
      end;
    end else begin
      If (chi.New_Name<>'') then begin
        errors := 'Invalid data in new_name field';
        Exit;
      end;
    end;
    If (chi.changes_type=[]) then begin
      errors := 'No change';
      Exit;
    end;
  end;
  // Check signatures!
  If Not CheckSignatures(AccountTransaction,errors) then Exit;
  // Execute!
  If (length(senders)>0) then begin
    If Not AccountTransaction.TransferAmounts(AccountPreviousUpdatedBlock,
      senders,senders_n_operation,senders_amount,
      receivers,receivers_amount,errors) then Begin
      TLog.NewLog(ltError,ClassName,'FATAL ERROR DEV 20180312-1 '+errors); // This must never happen!
      Raise Exception.Create('FATAL ERROR DEV 20180312-1 '+errors); // This must never happen!
      Exit;
    end;
  end;
  for i:=Low(FData.changesInfo) to High(FData.changesInfo) do begin
    chi := FData.changesInfo[i];
    changer := AccountTransaction.Account(chi.Account);
    If (public_key in chi.Changes_type) then begin
      changer.accountInfo.accountKey := chi.New_Accountkey;
      // Set to normal:
      changer.accountInfo.state := as_Normal;
      changer.accountInfo.locked_until_block := 0;
      changer.accountInfo.price := 0;
      changer.accountInfo.account_to_pay := 0;
      changer.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
    end;
    If (account_name in chi.Changes_type) then begin
      changer.name := chi.New_Name;
    end;
    If (account_type in chi.Changes_type) then begin
      changer.account_type := chi.New_Type;
    end;
    If Not AccountTransaction.UpdateAccountInfo(
           AccountPreviousUpdatedBlock,
           chi.Account,chi.N_Operation,chi.Account,
           changer.accountInfo,
           changer.name,
           changer.account_type,
           0,errors) then begin
      TLog.NewLog(ltError,ClassName,'FATAL ERROR DEV 20180312-2 '+errors); // This must never happen!
      Raise Exception.Create('FATAL ERROR DEV 20180312-2 '+errors); // This must never happen!
    end;
  end;
  Result := True;
end;

procedure TOpMultiOperation.AffectedAccounts(list: TList);
Var i : Integer;
  Procedure _doAdd(nAcc : Cardinal);
  Begin
    If list.IndexOf(TObject(nAcc))<0 then list.Add(TObject(nAcc));
  end;
begin
  For i:=low(FData.txSenders) to High(FData.txSenders) do begin
    _doAdd(FData.txSenders[i].Account);
  end;
  For i:=Low(FData.txReceivers) to High(FData.txReceivers) do begin
    _doAdd(FData.txReceivers[i].Account);
  end;
  For i:=Low(FData.changesInfo) to High(FData.changesInfo) do begin
    _doAdd(FData.changesInfo[i].Account);
  end;
end;

function TOpMultiOperation.GetTransactionHashToSign : TRawBytes;
Var ms : TMemoryStream;
  rb : TRawBytes;
  old : Boolean;
begin
  ms := TMemoryStream.Create;
  try
    old := FSaveSignatureValue;
    Try
      FSaveSignatureValue:=False;
      SaveOpToStream(ms,False);
    finally
      FSaveSignatureValue:=old;
    end;
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

function TOpMultiOperation.DoSignMultiOperationSigner(SignerAccount : Cardinal; key : TECPrivateKey) : Integer;
Var i : Integer;
  raw : TRawBytes;
  _sign : TECDSA_SIG;
begin
  Result := 0;
  If Not Assigned(key.PrivateKey) then begin
    exit;
  end;
  raw := GetTransactionHashToSign;
  Try
    _sign := TCrypto.ECDSASign(key.PrivateKey,raw);
  Except
    On E:Exception do begin
      TLog.NewLog(ltError,ClassName,'Error signing ('+E.ClassName+') '+E.Message);
      Exit;
    end;
  End;
  For i:=Low(FData.txSenders) to High(FData.txSenders) do begin
    If FData.txSenders[i].Account=SignerAccount then begin
      FData.txSenders[i].Signature := _sign;
      inc(Result);
    end;
  end;
  For i:=Low(FData.changesInfo) to High(FData.changesInfo) do begin
    If FData.changesInfo[i].Account=SignerAccount then begin
      FData.changesInfo[i].Signature := _sign;
      inc(Result);
    end;
  end;
  If (Result>0) Then begin
    FHasValidSignature := False;
  end;
end;

class function TOpMultiOperation.OpType: Byte;
begin
  Result := CT_Op_MultiOperation;
end;

function TOpMultiOperation.OperationAmount: Int64;
begin
  Result := FTotalAmount;
end;

function TOpMultiOperation.OperationFee: UInt64;
begin
  If FTotalFee<0 then Result := 0 // Alert!
  else Result := FTotalFee;
end;

function TOpMultiOperation.OperationPayload: TRawBytes;
begin
  Result := '';
end;

function TOpMultiOperation.SignerAccount: Cardinal;
begin
  // On a multioperation, the signer account are senders N accounts, cannot verify which one is correct... will send first one
  If length(FData.txSenders)>0 then Result := FData.txSenders[0].Account
  else if (length(FData.changesInfo)>0) then Result := FData.changesInfo[0].Account
  else Result := MaxInt;
end;

procedure TOpMultiOperation.SignerAccounts(list: TList);
var i : Integer;
begin
  list.Clear;
  for i := 0 to High(FData.txSenders) do begin
    list.Add(TObject(FData.txSenders[i].Account));
  end;
  for i:= 0 to High(FData.changesInfo) do begin
    if list.IndexOf(TObject(FData.changesInfo[i].Account))<0 then list.Add(TObject(FData.changesInfo[i].Account));
  end;
end;

function TOpMultiOperation.IsSignerAccount(account: Cardinal): Boolean;
begin
  // This function will override previous due it can be Multi signed
  Result := (IndexOfAccountSender(account)>=0) Or (IndexOfAccountChanger(account)>=0);
end;

function TOpMultiOperation.DestinationAccount: Int64;
begin
  Result:=inherited DestinationAccount;
end;

function TOpMultiOperation.SellerAccount: Int64;
begin
  Result:=inherited SellerAccount;
end;

function TOpMultiOperation.N_Operation: Cardinal;
begin
  // On a multioperation, the signer account are senders N accounts, cannot verify which one is correct... will send first one
  If length(FData.txSenders)>0 then Result := FData.txSenders[0].N_Operation
  else if (length(FData.changesInfo)>0) then Result := FData.changesInfo[0].N_Operation
  else Result := 0;
end;

function TOpMultiOperation.GetAccountN_Operation(account: Cardinal): Cardinal;
var i : Integer;
begin
  // On a multitoperation, there are N signers
  i := IndexOfAccountSender(account);
  If (i>=0) then begin
    Result := FData.txSenders[i].N_Operation;
    Exit;
  end;
  i := IndexOfAccountChanger(account);
  If (i>=0) then begin
    Result := FData.changesInfo[i].N_Operation;
    Exit;
  end;
  Result := 0;
end;

function TOpMultiOperation.OperationAmountByAccount(account: Cardinal): Int64;
Var i,j : Integer;
begin
  Result := 0;
  i := IndexOfAccountSender(account);
  if (i>=0) then begin
    Result := FData.txSenders[i].Amount * (-1);
  end;
  j := 0;
  Repeat
    i := IndexOfAccountReceiver(account,j);
    if (i>=0) then begin
      Result := Result + FData.txReceivers[i].Amount;
    end;
    j := i+1;
  until i<0;
end;

constructor TOpMultiOperation.CreateMultiOperation(
  const senders: TMultiOpSenders; const receivers: TMultiOpReceivers;
  const changes: TMultiOpChangesInfo; const senders_keys,
  changes_keys: array of TECPrivateKey);
Var i : Integer;
begin
  inherited Create;
  AddTx(senders,receivers,True);
  AddChangeInfos(changes,True);
  // Protection for "Exit"
  FHasValidSignature:=False;
  If (length(senders_keys)<>length(senders)) then exit; // Cannot sign!
  If (length(changes_keys)<>length(changes)) then exit; // Cannot sign!
  For i:=low(senders) to high(senders) do begin
    If DoSignMultiOperationSigner(senders[i].Account,senders_keys[i])=0 then begin
      TLog.NewLog(lterror,Classname,'Error signing a new MultiOperation sender');
      Exit;
    end;
  end;
  For i:=Low(changes) to high(changes) do begin
    If DoSignMultiOperationSigner(changes[i].Account,changes_keys[i])=0 then begin
      TLog.NewLog(lterror,Classname,'Error signing a new MultiOperation change');
      Exit;
    end;
  end;
end;

function TOpMultiOperation.AddTx(const senders: TMultiOpSenders; const receivers: TMultiOpReceivers; setInRandomOrder : Boolean) : Boolean;
Var i,j,k : Integer;
  total_spend, total_receive : Int64;
begin
  Result := False;
  total_spend:=0;
  total_receive:=0;
  // Check not duplicate and invalid data
  For i:=Low(senders) to High(senders) do begin
    If IndexOfAccountSender(senders[i].Account)>=0 then Exit;
    If IndexOfAccountSender(senders[i].Account,i+1,senders)>=0 then Exit;
    If IndexOfAccountChanger(senders[i].Account)>=0 then Exit;
    If (senders[i].Amount<=0) then Exit; // Must always sender >0
  end;
  For i:=Low(receivers) to High(receivers) do begin
    // Allow receivers as a duplicate!
    If (receivers[i].Amount<=0) then Exit; // Must always receive >0
  end;
  // Ok, let's go
  FHasValidSignature:=False;
  If setInRandomOrder then begin
    // Important:
    // When a sender/receiver is added, everybody must sign again
    // In order to create high anonymity, will add senders/receivers in random order
    // to difficult know who was the first or last to add
    For i:=Low(senders) to High(senders) do begin
      SetLength(FData.txSenders,length(FData.txSenders)+1);
      // Set sender in a random order
      If (length(FData.txSenders)>0) then begin
        j := Random(length(FData.txSenders)); // Find random position 0..n-1
      end else j:=0;
      for k:=High(FData.txSenders) downto (j+1) do FData.txSenders[k] := FData.txSenders[k-1];
      FData.txSenders[j] := senders[i];
      inc(total_spend,senders[i].Amount);
    end;
    For i:=Low(receivers) to High(receivers) do begin
      SetLength(FData.txReceivers,length(FData.txReceivers)+1);
      // Set receiver in a random order
      If (length(FData.txReceivers)>0) then begin
        j := Random(length(FData.txReceivers)); // Find random position 0..n-1
      end else j:=0;
      for k:=High(FData.txReceivers) downto (j+1) do FData.txReceivers[k] := FData.txReceivers[k-1];
      FData.txReceivers[j] := receivers[i];
      inc(total_receive,receivers[i].Amount);
    end;
  end else begin
    j := length(FData.txSenders);
    SetLength(FData.txSenders,length(FData.txSenders)+length(senders));
    For i:=Low(senders) to High(senders) do begin
      FData.txSenders[j+i] := senders[i];
      inc(total_spend,senders[i].Amount);
    end;
    j := length(FData.txReceivers);
    SetLength(FData.txReceivers,length(FData.txReceivers)+length(receivers));
    For i:=Low(receivers) to High(receivers) do begin
      FData.txReceivers[j+i] := receivers[i];
      inc(total_receive,receivers[i].Amount);
    end;
  end;
  inc(FTotalAmount,total_receive);
  inc(FTotalFee,total_spend - total_receive);
  Result := True;
end;

function TOpMultiOperation.AddChangeInfos(const changes: TMultiOpChangesInfo; setInRandomOrder : Boolean): Boolean;
Var i,j,k : Integer;
begin
  Result := False;
  // Check not duplicate / invalid data
  For i:=Low(changes) to High(changes) do begin
    If IndexOfAccountSender(changes[i].Account)>=0 then Exit;
    If IndexOfAccountChanger(changes[i].Account)>=0 then Exit;
    If IndexOfAccountChanger(changes[i].Account,i+1,changes)>=0 then Exit;
    If (changes[i].Changes_type=[]) then Exit; // Must change something
  end;
  // Ok, let's go
  FHasValidSignature:=False;
  // Important:
  // When a change is added, everybody must sign again
  // In order to create high anonymity, will add in random order
  // to difficult know who was the first or last to add
  For i:=Low(changes) to High(changes) do begin
    If (account_name in changes[i].Changes_type) And
       (IndexOfAccountChangeNameTo(changes[i].New_Name)>=0) then Begin
       // Does not allow set same name twice!
       Exit;
    end;
    SetLength(FData.changesInfo,length(FData.changesInfo)+1);
    If setInRandomOrder then begin
      // Set sender in a random order
      If (length(FData.changesInfo)>0) then begin
        j := Random(length(FData.changesInfo)); // Find random position 0..n-1
      end else j:=0;
      for k:=High(FData.changesInfo) downto (j+1) do FData.changesInfo[k] := FData.changesInfo[k-1];
    end else j := High(FData.changesInfo);
    FData.changesInfo[j] := changes[i];
  end;
  Result := True;
end;

function TOpMultiOperation.AddTxSender(const sender: TMultiOpSender): Boolean;
Var senders : TMultiOpSenders;
  receivers : TMultiOpReceivers;
begin
  SetLength(senders,1);
  senders[0] := sender;
  SetLength(receivers,0);
  Result := AddTx(senders,receivers,True);
end;

function TOpMultiOperation.AddTxReceiver(const receiver: TMultiOpReceiver): Boolean;
Var senders : TMultiOpSenders;
  receivers : TMultiOpReceivers;
begin
  SetLength(senders,0);
  SetLength(receivers,1);
  receivers[0] := receiver;
  Result := AddTx(senders,receivers,True);
end;

function TOpMultiOperation.AddChangeInfo(const changeInfo: TMultiOpChangeInfo): Boolean;
Var changes : TMultiOpChangesInfo;
begin
  SetLength(changes,1);
  changes[0] := changeInfo;
  Result := AddChangeInfos(changes,True);
end;

destructor TOpMultiOperation.Destroy;
begin
  SetLength(FData.txSenders,0);
  SetLength(FData.txReceivers,0);
  SetLength(FData.changesInfo,0);
  inherited Destroy;
end;

function TOpMultiOperation.toString: String;
begin
  Result := Format('Multioperation senders:%d receivers:%d changes:%d Amount:%s Fees:%s',
    [length(FData.txSenders),length(FData.txReceivers),length(FData.changesInfo),
     TAccountComp.FormatMoney(FTotalAmount),
     TAccountComp.FormatMoney(FTotalFee)]);
end;

end.

