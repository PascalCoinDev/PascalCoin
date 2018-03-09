unit UtxMultiOperation;

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

  }

  TOpMultiOperationData = Record
    txSenders : TMultiOpSenders;
    txReceivers : TMultiOpReceivers;
    changesInfo : TMultiOpChangesInfo;
  end;

  TOpMultiOperation = Class(TPCOperation)
  private
    FData : TOpMultiOperationData;
    //
    FSaveSignatureValue : Boolean;
    FTotalAmount : Int64;
    FTotalFee : Int64;
    Function IndexOfAccountSender(nAccount : Cardinal) : Integer;
    Function IndexOfAccountReceiver(nAccount : Cardinal) : Integer;
    Function IndexOfAccountChanger(nAccount : Cardinal) : Integer;
    Function IndexOfAccountChangeNameTo(const newName : AnsiString) : Integer;
  protected
    procedure InitializeData; override;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; override;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; override;
  public
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; override;

    function CheckSignatures(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean;

    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors : AnsiString) : Boolean; override;
    procedure AffectedAccounts(list : TList); override;
    //
    Function GetTransactionHashToSign : TRawBytes;
    Function DoSignMultiOperationSigner(SignerAccount : Cardinal; key : TECPrivateKey) : Integer;
    class function OpType : Byte; override;
    function OperationAmount : Int64; override;
    function OperationFee : UInt64; override;
    function OperationPayload : TRawBytes; override;
    function SignerAccount : Cardinal; override;
    function DestinationAccount : Int64; override;
    function SellerAccount : Int64; override;
    function N_Operation : Cardinal; override;
    //
    Constructor CreateMultiOperation(const senders : TMultiOpSenders; const receivers : TMultiOpReceivers; const changes : TMultiOpChangesInfo; const senders_keys, changes_keys: Array of TECPrivateKey);
    Function AddTx(const senders : TMultiOpSenders; const receivers : TMultiOpReceivers; setInRandomOrder : Boolean) : Boolean;
    Function AddChangeInfo(const changes : TMultiOpChangesInfo; setInRandomOrder : Boolean) : Boolean;
    Destructor Destroy; override;
    Function toString : String; Override;
  End;


implementation

Uses ULog, UConst;

{ TOpMultiOperation }

function TOpMultiOperation.IndexOfAccountSender(nAccount: Cardinal): Integer;
begin
  for Result:=0 to high(FData.txSenders) do begin
    If (FData.txSenders[Result].Account = nAccount) then exit;
  end;
  Result := -1;
end;

function TOpMultiOperation.IndexOfAccountReceiver(nAccount: Cardinal): Integer;
begin
  for Result:=0 to high(FData.txReceivers) do begin
    If (FData.txReceivers[Result].Account = nAccount) then exit;
  end;
  Result := -1;
end;

function TOpMultiOperation.IndexOfAccountChanger(nAccount: Cardinal): Integer;
begin
  for Result:=0 to high(FData.changesInfo) do begin
    If (FData.changesInfo[Result].Account = nAccount) then exit;
  end;
  Result := -1;
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
  w := CT_PROTOCOL_2;
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
  FSignatureChecked:=False;
  FHasValidSignature:=False;

  SetLength(txsenders,0);
  SetLength(txreceivers,0);
  SetLength(changes,0);

  Result := False;
  Try
    // Read protocol info
    stream.Read(w,SizeOf(w));
    If w<>CT_PROTOCOL_2 then Raise Exception.Create('Invalid protocol found');
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
    Result := Result or AddChangeInfo(changes,False); // Important: Set in same order!
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
    Result:=inherited GetBufferForOpHash(UseProtocolV2);
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
  FSignatureChecked:=True;
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

function TOpMultiOperation.DoOperation(AccountTransaction: TPCSafeBoxTransaction; var errors: AnsiString): Boolean;
begin
  // TODO
  { XXXXXXXXXXXXXXXXXXXXXXXXXX

  Implementation as expected and explained at PIP-0017

  Note: I've added "payload", that must be checked too



  TODO:
  - If a destination account is for a PRIVATE SALE... must work same as working currently for TOpTransaction
    when opTransactionStyle is in transaction_with_auto_buy_account?
    - IMPORTANT: If Yes, then is possible that a future change operation in same multioperation
      does not work due changed PUBLIC KEY when executing transaction_with_auto_buy_account
    - We can limit multioperation to not be able to "auto buy" account, simply add coins to target,
      this solves possible bad checking

  - When changing name of accounts in a multioperation, is possible that 2 accounts wants
    to set same name for account. Must prevent this! <-- Partially Prevented thanks to "IndexOfAccountChangeNameTo"
    - Must prevent that can set new names in real safebox

  - Conclusion:
    - Prior to execute each "setAccount", must check ALL is ok
    - HARD JOB!

  }
  Raise Exception.Create('NOT IMPLEMENTED ERROR DEV 20180308-1');
  Result := False;
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
    FSignatureChecked := False;
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
  else Result := MaxInt;
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
  // On a multitoperation, there are senders N accounts, need specify
  Result := 0;  // Note: N_Operation = 0 means NO OPERATION
end;

constructor TOpMultiOperation.CreateMultiOperation(
  const senders: TMultiOpSenders; const receivers: TMultiOpReceivers;
  const changes: TMultiOpChangesInfo; const senders_keys,
  changes_keys: array of TECPrivateKey);
Var i : Integer;
begin
  inherited Create;
  AddTx(senders,receivers,True);
  AddChangeInfo(changes,True);
  // Protection for "Exit"
  FSignatureChecked:=True;
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
  // Check as a Valid after everybody signed properly
  FSignatureChecked:=True;
  FHasValidSignature:=True;
end;

function TOpMultiOperation.AddTx(const senders: TMultiOpSenders; const receivers: TMultiOpReceivers; setInRandomOrder : Boolean) : Boolean;
Var i,j,k : Integer;
  total_spend, total_receive : Int64;
begin
  Result := False;
  total_spend:=0;
  total_receive:=0;
  // Check not duplicate
  For i:=Low(senders) to High(senders) do begin
    If IndexOfAccountSender(senders[i].Account)>=0 then Exit;
  end;
  For i:=Low(receivers) to High(receivers) do begin
    If IndexOfAccountReceiver(receivers[i].Account)>=0 then Exit;
  end;
  // Ok, let's go
  FHasValidSignature:=False;
  FSignatureChecked:=False;
  // Important:
  // When a sender/receiver is added, everybody must sign again
  // In order to create high anonymity, will add senders/receivers in random order
  // to difficult know who was the first or last to add
  For i:=Low(senders) to High(senders) do begin
    SetLength(FData.txSenders,length(FData.txSenders)+1);
    If setInRandomOrder then begin
      // Set sender in a random order
      If (length(FData.txSenders)>0) then begin
        j := Random(length(FData.txSenders)); // Find random position 0..n-1
      end else j:=0;
      for k:=High(FData.txSenders) downto (j+1) do FData.txSenders[k] := FData.txSenders[k-1];
    end else j:=High(FData.txSenders);
    FData.txSenders[j] := senders[i];
    inc(total_spend,senders[i].Amount);
  end;
  For i:=Low(receivers) to High(receivers) do begin
    SetLength(FData.txReceivers,length(FData.txReceivers)+1);
    If setInRandomOrder then begin
      // Set receiver in a random order
      If (length(FData.txReceivers)>0) then begin
        j := Random(length(FData.txReceivers)); // Find random position 0..n-1
      end else j:=0;
      for k:=High(FData.txReceivers) downto (j+1) do FData.txReceivers[k] := FData.txReceivers[k-1];
    end else j:=High(FData.txReceivers);
    FData.txReceivers[j] := receivers[i];
    inc(total_receive,receivers[i].Amount);
  end;
  inc(FTotalAmount,total_receive);
  inc(FTotalFee,total_spend - total_receive);
  Result := True;
end;

function TOpMultiOperation.AddChangeInfo(const changes: TMultiOpChangesInfo; setInRandomOrder : Boolean): Boolean;
Var i,j,k : Integer;
begin
  Result := False;
  // Check not duplicate
  For i:=Low(changes) to High(changes) do begin
    If IndexOfAccountChanger(changes[i].Account)>=0 then Exit;
  end;
  // Ok, let's go
  FHasValidSignature:=False;
  FSignatureChecked:=False;
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

destructor TOpMultiOperation.Destroy;
begin
  SetLength(FData.txSenders,0);
  SetLength(FData.txReceivers,0);
  SetLength(FData.changesInfo,0);
  inherited Destroy;
end;

function TOpMultiOperation.toString: String;
var i : Integer;
  ssenders,sreceivers,schanges : String;
begin
  ssenders := '';
  for i:=low(FData.txSenders) to High(FData.txSenders) do begin
    ssenders := ssenders + Format('%d:(%s,%s,%d)',[i+1,TAccountComp.AccountNumberToAccountTxtNumber(FData.txSenders[i].Account),
        TAccountComp.FormatMoney(FData.txSenders[i].Amount),FData.txSenders[i].N_Operation]);
  end;
  sreceivers:='';
  for i:=low(FData.txReceivers) to High(FData.txReceivers) do begin
    sreceivers := sreceivers + Format('%d:(%s,%s)',[i+1,TAccountComp.AccountNumberToAccountTxtNumber(FData.txReceivers[i].Account),
        TAccountComp.FormatMoney(FData.txReceivers[i].Amount)]);
  end;
  schanges := '';
  for i:=low(FData.changesInfo) to High(FData.changesInfo) do begin
    schanges := schanges + Format('%d:(%s,%d)',[i+1,TAccountComp.AccountNumberToAccountTxtNumber(FData.changesInfo[i].Account),
        FData.changesInfo[i].N_Operation]);
  end;
  Result := Format('Multioperation senders %s receivers %s changes %s Amount:%s Fees:%s',
    [ssenders,sreceivers,schanges,
     TAccountComp.FormatMoney(FTotalAmount),
     TAccountComp.FormatMoney(FTotalFee)]);
end;

end.

