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
  Classes, SysUtils, UCrypto;

Type
  TAccountTxInfo = Record
    Account : Cardinal;
    Amount : Int64;
    Fee : Int64;               // Not used if AffectedAccount is the receiver
    N_Operation : Cardinal;    // Not used if AffectedAccount is the receiver
    Payload : TRawBytes;
    Signature : TECDSA_SIG;    // Not used if AffectedAccount is the receiver
  end;

  TAccountsTxInfoArray = Array of TAccountTxInfo;

  { TTxInfo }

  TTxInfo = Class
  private
    FAccountTxInfo : Array of TAccountTxInfo;
    FDisableds : Integer;
    FNeedRecalc : Boolean;
    Procedure Recalc;
    function GetAccounTxInfo(index : Integer): TAccountTxInfo;
    function GetCount: Integer;
  protected
    FHash : TRawBytes;
    FTotalAmount : Int64;
    FTotalFees : Int64;
    procedure InternalLoadTxFromStream(stream : TStream; var tx : TAccountTxInfo); virtual; abstract;
    procedure InternalSaveTxToStream(stream : TStream; const tx : TAccountTxInfo); virtual; abstract;
    Procedure InternalRecalc;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure LoadFromStream(stream : TStream);
    procedure SaveToStream(stream : TStream);
    Property Count : Integer read GetCount;
    Function IndexOf(searchAccount : Cardinal) : Integer;
    Procedure DeleteAccount(Account : Integer);
    Procedure Disable;
    Procedure Enable;
    Property AccountTxInfo[index : Integer] : TAccountTxInfo read GetAccounTxInfo;
    Procedure Clear;
    Function GetHash : TRawBytes;
    Function TotalAmount : Int64;
    Function TotalFees : Int64;
    Procedure ToArray(var txArray : TAccountsTxInfoArray);
  end;

  { TTxInfoSender }

  TTxInfoSender = Class(TTxInfo)
  protected
    procedure InternalLoadTxFromStream(stream : TStream; var tx : TAccountTxInfo); override;
    procedure InternalSaveTxToStream(stream : TStream; const tx : TAccountTxInfo); override;
  public
    Procedure AddSender(SenderAccount : Cardinal; Amount : Int64; Fee : Int64; N_Operation : Cardinal; Payload : TRawBytes);
    Function SetSignatureForAccount(SignerAccount : Cardinal; const SignatureValue : TECDSA_SIG) : Integer;
    Function toString : String; Override;
  end;

  { TTxInfoReceiver }

  TTxInfoReceiver = Class(TTxInfo)
  protected
    procedure InternalLoadTxFromStream(stream : TStream; var tx : TAccountTxInfo); override;
    procedure InternalSaveTxToStream(stream : TStream; const tx : TAccountTxInfo); override;
  public
    Procedure AddReceiver(DestinationAccount : Cardinal; Amount : Int64; Payload : TRawBytes);
    Function toString : String; Override;
  end;

Const
  CT_TAccountTxInfo_NULL : TAccountTxInfo = (Account:0;Amount:0;Fee:0;N_Operation:0;Payload:'';Signature:(r:'';s:''));

implementation

Uses UAccounts;

{ TTxInfoReceiver }

procedure TTxInfoReceiver.InternalLoadTxFromStream(stream: TStream; var tx: TAccountTxInfo);
begin
  tx := CT_TAccountTxInfo_NULL;
  // Note: Receiver will read ONLY account, amount and Payload (not fee, not N_Operation nor signature...)
  stream.Read(tx.Account,SizeOf(tx.Account));
  stream.Read(tx.Amount,SizeOf(tx.Amount));
  TStreamOp.ReadAnsiString(stream,tx.Payload);
end;

procedure TTxInfoReceiver.InternalSaveTxToStream(stream: TStream; const tx: TAccountTxInfo);
begin
  stream.Write(tx.Account,SizeOf(tx.Account));
  stream.Write(tx.Amount,SizeOf(tx.Amount));
  TStreamOp.WriteAnsiString(stream,tx.Payload);
end;

procedure TTxInfoReceiver.AddReceiver(DestinationAccount: Cardinal; Amount: Int64; Payload : TRawBytes);
var i : Integer;
begin
  i := IndexOf(DestinationAccount);
  If i>=0 then Raise Exception.Create(Format('Cannot add Destination Account %d (found at pos %d)',[DestinationAccount,i]));
  i := length(FAccountTxInfo);
  SetLength(FAccountTxInfo,i+1);
  FAccountTxInfo[i] := CT_TAccountTxInfo_NULL;
  FAccountTxInfo[i].Account:= DestinationAccount;
  FAccountTxInfo[i].Amount:= Amount;
  FAccountTxInfo[i].Payload:= Payload;
  Recalc;
end;

function TTxInfoReceiver.toString: String;
Var i : Integer;
begin
  Result := '';
  for i:=0 to Count-1 do begin
    Result := Result + Format('%d:(%s,%s)',[i+1,TAccountComp.AccountNumberToAccountTxtNumber(AccountTxInfo[i].Account),
      TAccountComp.FormatMoney(AccountTxInfo[i].Amount)]);
  end;
end;

{ TTxInfoSender }

procedure TTxInfoSender.InternalLoadTxFromStream(stream: TStream; var tx : TAccountTxInfo);
begin
  tx := CT_TAccountTxInfo_NULL;
  stream.Read(tx.Account,SizeOf(tx.Account));
  stream.Read(tx.Amount,SizeOf(tx.Amount));
  stream.Read(tx.Fee,SizeOf(tx.Fee));
  stream.Read(tx.N_Operation,SizeOf(tx.N_Operation));
  TStreamOp.ReadAnsiString(stream,tx.Payload);
  TStreamOp.ReadAnsiString(stream,tx.Signature.r);
  TStreamOp.ReadAnsiString(stream,tx.Signature.s);
end;

procedure TTxInfoSender.InternalSaveTxToStream(stream: TStream; const tx : TAccountTxInfo);
begin
  stream.Write(tx.Account,SizeOf(tx.Account));
  stream.Write(tx.Amount,SizeOf(tx.Amount));
  stream.Write(tx.Fee,SizeOf(tx.Fee));
  stream.Write(tx.N_Operation,SizeOf(tx.N_Operation));
  TStreamOp.WriteAnsiString(stream,tx.Payload);
  TStreamOp.WriteAnsiString(stream,tx.Signature.r);
  TStreamOp.WriteAnsiString(stream,tx.Signature.s);
end;

procedure TTxInfoSender.AddSender(SenderAccount: Cardinal; Amount: Int64; Fee: Int64; N_Operation: Cardinal; Payload: TRawBytes);
var i : Integer;
begin
  i := IndexOf(SenderAccount);
  If i>=0 then Raise Exception.Create(Format('Cannot add Sender Account %d (found at pos %d)',[SenderAccount,i]));
  i := length(FAccountTxInfo);
  SetLength(FAccountTxInfo,i+1);
  FAccountTxInfo[i] := CT_TAccountTxInfo_NULL;
  FAccountTxInfo[i].Account:= SenderAccount;
  FAccountTxInfo[i].Amount:= Amount;
  FAccountTxInfo[i].Fee:= Fee;
  FAccountTxInfo[i].N_Operation:= N_Operation;
  FAccountTxInfo[i].Payload:=Payload;
  Recalc;
end;

function TTxInfoSender.SetSignatureForAccount(SignerAccount: Cardinal; const SignatureValue: TECDSA_SIG): Integer;
Var i : Integer;
begin
  Result := 0;
  For i:=0 to Count-1 do begin
    If (AccountTxInfo[i].Account=SignerAccount) then begin
      FAccountTxInfo[i].Signature := SignatureValue;
      Inc(Result);
    end;
  end;
  // Note: SignatureValue has not effect on Recalc because is not hashed, so we don't need to call Recalc
end;

function TTxInfoSender.toString: String;
Var i : Integer;
begin
  Result := '';
  for i:=0 to Count-1 do begin
    Result := Result + Format('%d:(%s,%s,%s)',[i+1,TAccountComp.AccountNumberToAccountTxtNumber(AccountTxInfo[i].Account),
      TAccountComp.FormatMoney(AccountTxInfo[i].Amount),TAccountComp.FormatMoney(AccountTxInfo[i].Fee)]);
  end;
end;

{ TTxInfo }

procedure TTxInfo.Recalc;
begin
  If (FDisableds>0) then begin
    FNeedRecalc:=True;
    Exit;
  end;
  FNeedRecalc:=False;
  If Count>0 then InternalRecalc
  else FHash:='';
end;

function TTxInfo.GetAccounTxInfo(index : Integer): TAccountTxInfo;
begin
  If (index<low(FAccountTxInfo)) Or (index>High(FAccountTxInfo)) then Raise Exception.Create(ClassName+' invalid index');
  Result := FAccountTxInfo[index];
end;

function TTxInfo.GetCount: Integer;
begin
  Result := Length(FAccountTxInfo);
end;

procedure TTxInfo.LoadFromStream(stream: TStream);
Var w : Word;
  i : Integer;
  tx : TAccountTxInfo;
begin
  Clear;
  stream.read(w,SizeOf(w));
  SetLength(FAccountTxInfo,w);
  If w>0 then begin
    for i:=0 to w-1 do begin
      InternalLoadTxFromStream(stream,tx);
      If IndexOf(tx.Account)<0 then FAccountTxInfo[i] := tx
      else begin
        Clear;
        Raise Exception.Create(Format('Error reading TTxInfo Stream. Cannot add Account %d (%d/%d)',[tx.Account,i+1,w]));
      end;
    end;
  end;
  Recalc;
end;

procedure TTxInfo.SaveToStream(stream: TStream);
Var i : Integer;
  w : Word;
begin
  w := Count;
  stream.Write(w,Sizeof(w));
  If w>0 then begin
    for i:=0 to w-1 do begin
      InternalSaveTxToStream(stream,AccountTxInfo[i]);
    end;
  end;
end;

procedure TTxInfo.InternalRecalc;
Var stream : TMemoryStream;
  i : Integer;
  tx : TAccountTxInfo;
begin
  { Will SHA256( for each account (account+amount+fee+N_Operation+payload) )
    Note: Will NOT hash the Signature value as explained at PIP-0017 in order to create
    a OpHash that can be known prior to every account has signed }
  If Count=0 then begin
    FHash:='';
    FTotalAmount:=0;
    FTotalFees:=0;
    Exit;
  end;
  stream := TMemoryStream.Create;
  try
    for i:=0 to Count-1 do begin
      tx := AccountTxInfo[i];
      stream.Write(tx.Account,SizeOf(tx.Account));
      stream.Write(tx.Amount,SizeOf(tx.Amount));
      stream.Write(tx.Fee,SizeOf(tx.Fee));
      stream.Write(tx.N_Operation,SizeOf(tx.N_Operation));
      If length(tx.Payload)>0 then stream.WriteBuffer(tx.Payload[1],Length(tx.Payload));
      inc(FTotalAmount,tx.Amount);
      inc(FTotalFees,tx.Fee);
    end;
    stream.Position:=0;
    FHash := TCrypto.DoSha256(TMemoryStream(stream).Memory,stream.Size);
  finally
    stream.Free;
  end;
end;

constructor TTxInfo.Create;
begin
  SetLength(FAccountTxInfo,0);
  FDisableds := 0;
  FNeedRecalc:=False;
  FHash:='';
  Clear;
end;

destructor TTxInfo.Destroy;
begin
  inherited Destroy;
end;

function TTxInfo.IndexOf(searchAccount: Cardinal): Integer;
begin
  For Result:=low(FAccountTxInfo) to high(FAccountTxInfo) do begin
    If (FAccountTxInfo[Result].Account = searchAccount) then Exit;
  end;
  Result := -1;
end;

procedure TTxInfo.DeleteAccount(Account: Integer);
Var i,j : Integer;
begin
  i := IndexOf(Account);
  If i<0 then Exit;
  For j:=i+1 to High(FAccountTxInfo) do begin
    FAccountTxInfo[j-1] := FAccountTxInfo[j];
  end;
  SetLength(FAccountTxInfo,length(FAccountTxInfo)-1);
  Recalc;
end;

procedure TTxInfo.Disable;
begin
  Inc(FDisableds);
end;

procedure TTxInfo.Enable;
begin
  If FDisableds<=0 then Raise Exception.Create('ERROR DEV 20180306-2');
  Dec(FDisableds);
  If (FDisableds=0) And (FNeedRecalc) then Recalc;
end;

procedure TTxInfo.Clear;
begin
  SetLength(FAccountTxInfo,0);
  FHash:='';
  FTotalAmount:=0;
  FTotalFees:=0;
end;

function TTxInfo.GetHash: TRawBytes;
begin
  If (FDisableds>0) Or (FNeedRecalc) then Raise Exception.Create('ERROR DEV 20180306-3');
  Result := FHash;
end;

function TTxInfo.TotalAmount: Int64;
begin
  If (FDisableds>0) Or (FNeedRecalc) then Raise Exception.Create('ERROR DEV 20180306-4');
  Result := FTotalAmount;
end;

function TTxInfo.TotalFees: Int64;
begin
  If (FDisableds>0) Or (FNeedRecalc) then Raise Exception.Create('ERROR DEV 20180306-5');
  Result := FTotalFees;
end;

procedure TTxInfo.ToArray(var txArray: TAccountsTxInfoArray);
Var i : Integer;
begin
  SetLength(txArray,Count);
  For i:=0 to Count-1 do begin
    txArray[i] := GetAccounTxInfo(i);
  end;
end;

end.

