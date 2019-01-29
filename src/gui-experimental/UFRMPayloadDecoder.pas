unit UFRMPayloadDecoder;

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

{$mode Delphi}

interface

{$I ../config.inc}

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UCommon.UI,
  UBlockChain, UCrypto, UWallet, Buttons, ComCtrls,UAppParams;

type

  { TFRMPayloadDecoder }

  TFRMPayloadDecoder = class(TApplicationForm)
    Label1: TLabel;
    lblBlock: TLabel;
    lblDateTime: TLabel;
    Label6: TLabel;
    Label2: TLabel;
    lblOperationTxt: TLabel;
    PageControl: TPageControl;
    tsDecoded: TTabSheet;
    tsDecodeMethods: TTabSheet;
    cbMethodPublicPayload: TCheckBox;
    cbUsingPrivateKeys: TCheckBox;
    cbUsingPasswords: TCheckBox;
    memoPasswords: TMemo;
    Label7: TLabel;
    lblDecodedMethod: TLabel;
    Label3: TLabel;
    bbSaveMethods: TBitBtn;
    bbClose: TBitBtn;
    memoDecoded: TMemo;
    memoOriginalPayloadInHexa: TMemo;
    lblPasswordsInfo: TLabel;
    lblAmountCaption: TLabel;
    lblAmount: TLabel;
    lblFeeCaption: TLabel;
    lblFee: TLabel;
    Label4: TLabel;
    bbFind: TBitBtn;
    ebOphash: TEdit;
    lblSenderCaption: TLabel;
    lblSender: TLabel;
    lblReceiverCaption: TLabel;
    lblReceiver: TLabel;
    lblReceiverInfo: TLabel;
    cbShowAsHexadecimal: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure cbMethodPublicPayloadClick(Sender: TObject);
    procedure bbSaveMethodsClick(Sender: TObject);
    procedure memoDecodedKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bbFindClick(Sender: TObject);
    procedure ebOphashExit(Sender: TObject);
    procedure ebOphashKeyPress(Sender: TObject; var Key: Char);
    procedure cbShowAsHexadecimalClick(Sender: TObject);
  private
    FOpResume : TOperationResume;
    FSavedDecodeMethods : boolean;
    FSemaphor : Boolean;
    { Private declarations }
    Procedure TryToDecode;
    Procedure SaveMethods;
    procedure SetOpResume(const Value: TOperationResume);
  public
    { Public declarations }
    Procedure Init(Const AOperationResume : TOperationResume; WalletKeys : TWalletKeys; AppParams : TAppParams); overload;
    Procedure Init(Const AOperationResume : TOperationResume); overload;
    Property OpResume : TOperationResume read FOpResume write SetOpResume;
    Procedure DoFind(Const OpHash : String);
  end;

implementation

{$R *.lfm}

Uses UNode, UTime, UECIES, UAES, UAccounts, UCommon, UFRMMemoText, USettings, UBaseTypes;

{ TFRMPayloadDecoder }

procedure TFRMPayloadDecoder.bbSaveMethodsClick(Sender: TObject);
begin
  SaveMethods;
  PageControl.ActivePage := tsDecoded;
  TryToDecode;
end;

procedure TFRMPayloadDecoder.bbFindClick(Sender: TObject);
Var oph : String;
begin
  oph := TCrypto.ToHexaString( FOpResume.OperationHash );
  if Not InputQuery('Search operation by OpHash','Insert Operation Hash value (OpHash)',oph) then exit;
  DoFind(oph);
end;

procedure TFRMPayloadDecoder.cbMethodPublicPayloadClick(Sender: TObject);
begin
  FSavedDecodeMethods := false;
  lblPasswordsInfo.Caption := Format('Possible passwords: %d',[memoPasswords.Lines.Count]);
end;

procedure TFRMPayloadDecoder.cbShowAsHexadecimalClick(Sender: TObject);
begin
  TryToDecode;
end;

procedure TFRMPayloadDecoder.DoFind(Const OpHash : String);
Var
  r,md160 : TRawBytes;
  pcops : TPCOperationsComp;
  nBlock,nAccount,nN_Operation : Cardinal;
  opbi : Integer;
  opr : TOperationResume;
  strings : TStrings;
  FRM : TFRMMemoText;
begin
  // Search for an operation based on "ophash"
  if (trim(OpHash)='') then begin
    OpResume := CT_TOperationResume_NUL;
    exit;
  end;
  try
    // Build 2.1.4 new decoder option: Check if OpHash is a posible double spend
    r := TCrypto.HexaToRaw(trim(OpHash));
    If not TPCOperation.TryParseOperationHash(OpHash,nBlock,nAccount,nN_Operation,md160) then begin
      raise Exception.Create('Invalid OPHASH');
    end;
    Case TNode.Node.FindNOperation(nBlock,nAccount,nN_Operation,opr) of
      invalid_params : raise Exception.Create(Format('Not a valid OpHash searching at Block:%d Account:%d N_Operation:%d',[nBlock,nAccount,nN_Operation]));
      blockchain_block_not_found : raise Exception.Create('Your blockchain file does not contain all blocks to find');
      found : ;
    else raise Exception.Create('ERROR DEV 20171120-6');
    end;
    If (TPCOperation.EqualOperationHashes(opr.OperationHash,r)) Or
       (TPCOperation.EqualOperationHashes(opr.OperationHash_OLD,r)) then begin
      // Found!
      OpResume := opr;
    end else begin
      // Not found!
      strings := TStringList.Create;
      try
        strings.Add('Possible double spend detected!');
        strings.Add(Format('OpHash: %s',[OpHash]));
        strings.Add(Format('Decode OpHash info: Block:%d Account:%s N_Operation:%d',[nBlock,TAccountComp.AccountNumberToAccountTxtNumber(nAccount),nN_Operation]));
        strings.Add('');
        strings.Add('Real OpHash found in PascalCoin Blockchain:');
        strings.Add(Format('OpHash: %s',[TCrypto.ToHexaString(opr.OperationHash)]));
        strings.Add(Format('Decode OpHash info: Block:%d Account:%s N_Operation:%d',[opr.Block,TAccountComp.AccountNumberToAccountTxtNumber(opr.SignerAccount),opr.n_operation]));
        If (opr.Block=0) then begin
          strings.Add('* Note: This is a pending operation not included on Blockchain');
        end;
        OpResume := opr; // Do show operation resume!
        FRM := TFRMMemoText.Create(Self);
        try
          FRM.InitData('Possible double spend detected',strings.Text);
          FRM.ShowModal;
        finally
          FRM.Free;
        end;
      finally
        strings.Free;
      end;
    end;
  Except
    OpResume := CT_TOperationResume_NUL;
    try
      FSemaphor := true;
      ebOphash.Text := trim(ophash);
    finally
      FSemaphor := false;
    end;
    Raise;
  end;
end;

procedure TFRMPayloadDecoder.ebOphashExit(Sender: TObject);
begin
  DoFind(ebOphash.Text);
end;

procedure TFRMPayloadDecoder.ebOphashKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then DoFind(ebOphash.Text);
end;

procedure TFRMPayloadDecoder.FormCreate(Sender: TObject);
begin
  FSemaphor := true;
  try
    memoDecoded.Lines.Clear;
    memoOriginalPayloadInHexa.Lines.Clear;
    lblPasswordsInfo.Caption := '';
    OpResume := CT_TOperationResume_NUL;
  finally
    FSemaphor := false;
  end;
end;

// Needed for classic compat
Procedure TFRMPayloadDecoder.Init(Const AOperationResume : TOperationResume; WalletKeys : TWalletKeys; AppParams : TAppParams);
begin
  Init(AOperationResume);
end;

procedure TFRMPayloadDecoder.Init(Const AOperationResume : TOperationResume);
begin
  OpResume := AOperationResume;
  FSavedDecodeMethods := true;
  PageControl.ActivePage := tsDecoded;
  TryToDecode;
end;

procedure TFRMPayloadDecoder.memoDecodedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then Close;

end;

procedure TFRMPayloadDecoder.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  //
  if PageControl.ActivePage=tsDecodeMethods then begin
    If not FSavedDecodeMethods then begin
      case Application.MessageBox(PChar('Save new decode methods?'),PChar(Application.Title),MB_YESNOCANCEL+MB_ICONQUESTION) of
        IDYES : Begin
          SaveMethods;
        End;
        IDCANCEL : begin
          AllowChange := false;
        end;
      end;

    end;
  end else begin
    FSavedDecodeMethods := true;
  end;
end;

procedure TFRMPayloadDecoder.SaveMethods;
begin
  TSettings.AppParams.ParamByName['PayloadDecoder.notencrypted'].SetAsBoolean(cbMethodPublicPayload.Checked);
  TSettings.AppParams.ParamByName['PayloadDecoder.usingprivatekeys'].SetAsBoolean(cbUsingPrivateKeys.Checked);
  TSettings.AppParams.ParamByName['PayloadDecoder.usingpasswords'].SetAsBoolean(cbUsingPasswords.Checked);
  TSettings.AppParams.ParamByName['PayloadDecoder.passwords'].SetAsString(memoPasswords.Lines.Text);
  TSettings.AppParams.ParamByName['PayloadDecoder.showashexadecimal'].SetAsBoolean(cbShowAsHexadecimal.Checked);
  TSettings.Save;
  FSavedDecodeMethods := true;
end;

procedure TFRMPayloadDecoder.SetOpResume(const Value: TOperationResume);
Var sem : Boolean;
begin
  sem := FSemaphor;
  Try
    FSemaphor := false;
    FOpResume := Value;
    if Not Value.valid then begin
      lblBlock.Caption := '';
      lblDateTime.Caption := '';
      lblOperationTxt.Caption := '';
      lblDecodedMethod.Caption := '';
      lblFee.Caption := '';
      lblPasswordsInfo.Caption := '';
      lblAmount.Caption := '';
      lblSender.Caption := '';
      lblReceiver.Caption := '';
      lblReceiverInfo.Visible := false;
      exit;
    end;
    If (Value.NOpInsideBlock>=0) then
      lblBlock.Caption := inttostr(Value.Block)+'/'+inttostr(Value.NOpInsideBlock+1)+' '+IntToStr(Value.n_operation)
    else lblBlock.Caption := inttostr(Value.Block)+' '+IntToStr(Value.n_operation);
    if Value.time>10000 then begin
      lblDateTime.Caption := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime(Value.time)));
      lblDateTime.Font.Color := clBlack;
    end else begin
      lblDateTime.Caption := '(Pending block)';
      lblDateTime.Font.Color := clRed;
    end;
    lblOperationTxt.Caption := Value.OperationTxt;
    lblAmount.Caption := TAccountComp.FormatMoney(value.Amount);
    if Value.Amount>0 then lblAmount.Font.Color := clGreen
    else if Value.Amount=0 then lblAmount.Font.Color := clGray
    else lblAmount.Font.Color := clRed;
    If (Value.SignerAccount>=0) And (Value.DestAccount>=0) then begin
      lblSenderCaption.Caption := 'Sender:';
      lblSender.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Value.SignerAccount);
      lblReceiverCaption.Visible := true;
      lblReceiver.Caption := TAccountComp.AccountNumberToAccountTxtNumber(Value.DestAccount);
      lblReceiver.Visible := true;
      lblFeeCaption.Visible := Value.AffectedAccount=Value.SignerAccount;
      lblFee.Visible := lblFeeCaption.Visible;
      lblReceiverInfo.Visible := Not lblFee.Visible;
    end else begin
      lblSenderCaption.Caption := 'Account:';
      lblSender.caption := TAccountComp.AccountNumberToAccountTxtNumber(Value.AffectedAccount);
      lblReceiverCaption.Visible := false;
      lblReceiver.Visible := false;
      lblFeeCaption.Visible := true;
      lblFee.Visible := true;
      lblReceiverInfo.Visible := false;
    end;
    lblFee.Caption := TAccountComp.FormatMoney(value.Fee);
    if Value.Fee>0 then lblFee.Font.Color := clGreen
    else if Value.Fee=0 then lblFee.Font.Color := clGray
    else lblFee.Font.Color := clRed;
    ebOpHash.text := TCrypto.ToHexaString(Value.OperationHash);
    memoOriginalPayloadInHexa.Lines.Text := TCrypto.ToHexaString(Value.OriginalPayload);
    cbMethodPublicPayload.Checked := TSettings.AppParams.ParamByName['PayloadDecoder.notencrypted'].GetAsBoolean(true);
    cbUsingPrivateKeys.Checked := TSettings.AppParams.ParamByName['PayloadDecoder.usingprivatekeys'].GetAsBoolean(true);
    cbUsingPasswords.Checked := TSettings.AppParams.ParamByName['PayloadDecoder.usingpasswords'].GetAsBoolean(true);
    memoPasswords.Lines.Text := TSettings.AppParams.ParamByName['PayloadDecoder.passwords'].GetAsString('');
    cbShowAsHexadecimal.Checked := TSettings.AppParams.ParamByName['PayloadDecoder.showashexadecimal'].GetAsBoolean(false);
    FSavedDecodeMethods := true;
    PageControl.ActivePage := tsDecoded;
    TryToDecode;
  Finally
    FSemaphor := sem;
  End;
end;

procedure TFRMPayloadDecoder.TryToDecode;
  Function UseWallet(Const raw : TRawBytes; var Decrypted : TRawBytes; var WalletKey : TWalletKey) : Boolean;
  Var i : Integer;
  begin
    Result := false;
    for i := 0 to TWallet.Keys.Count - 1 do begin
      WalletKey := TWallet.Keys.Key[i];
      If Assigned(WalletKey.PrivateKey) then begin
        If ECIESDecrypt(WalletKey.PrivateKey.EC_OpenSSL_NID,WalletKey.PrivateKey.PrivateKey,false,raw,Decrypted) then begin
          Result := true;
          exit;
        end;
      end;
    end;

  end;

  Function  UsePassword(const raw : TRawBytes; var Decrypted: TRawBytes; var PasswordUsed : AnsiString) : Boolean;
  Var i : Integer;
  Begin
    Result := false;
    for i := 0 to memoPasswords.Lines.Count - 1 do begin
      if (TAESComp.EVP_Decrypt_AES256(raw,memoPasswords.Lines[i],Decrypted)) then begin
        if (TCrypto.IsHumanReadable(Decrypted)) then begin
          Result := true;
          PasswordUsed := memoPasswords.Lines[i];
          exit;
        end;
      end;
    end;
  End;


Var raw : TRawBytes;
  WalletKey : TWalletKey;
  Decrypted: TRawBytes;
  PasswordUsed : AnsiString;
  ok : boolean;
begin
  ok := true;
  raw := FOpResume.OriginalPayload;
  if Length(raw)>0 then begin
    // First try to a human readable...
    if (cbMethodPublicPayload.Checked) and (TCrypto.IsHumanReadable(raw)) then begin
      if cbShowAsHexadecimal.Checked then memoDecoded.Lines.Text := TCrypto.ToHexaString(raw)
      else memoDecoded.Lines.Text := TEncoding.ANSI.GetString(raw);
      lblDecodedMethod.Caption := 'Not encrypted payload';
    end else if (cbUsingPrivateKeys.Checked) And (UseWallet(raw,Decrypted,WalletKey)) then begin
      if cbShowAsHexadecimal.Checked then memoDecoded.Lines.Text := TCrypto.ToHexaString(Decrypted)
      else memoDecoded.Lines.Text := TEncoding.ANSI.GetString(Decrypted);
      lblDecodedMethod.Caption := 'Encrypted with EC '+TAccountComp.GetECInfoTxt(WalletKey.PrivateKey.EC_OpenSSL_NID);
    end else if (cbUsingPasswords.Checked) And (UsePassword(raw,Decrypted,PasswordUsed)) then begin
      if cbShowAsHexadecimal.Checked then memoDecoded.Lines.Text := TCrypto.ToHexaString(Decrypted)
      else memoDecoded.Lines.Text := TEncoding.ANSI.GetString(Decrypted);
      lblDecodedMethod.Caption := 'Encrypted with pwd:"'+PasswordUsed+'"';
    end else begin
      memoDecoded.Lines.Text := 'CANNOT DECRYPT';
      lblDecodedMethod.Caption := '';
      ok := false;
    end;
    if ok then begin
      memoDecoded.Font.Color := clBlack;
      memoDecoded.Color := clWhite;
    end else begin
      memoDecoded.Font.Color := clRed;
      memoDecoded.Color := clBtnFace;
    end;
  end else begin
    memoDecoded.Lines.Text := '(No payload)';
    memoDecoded.Font.Color := clDkGray;
    memoDecoded.Color := clLtGray;
    lblDecodedMethod.Caption := '';
  end;
end;

end.
