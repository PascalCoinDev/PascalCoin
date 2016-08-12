unit UFRMPayloadDecoder;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UBlockChain, UCrypto, UWalletKeys, Buttons, ComCtrls,
  UAppParams;

type
  TFRMPayloadDecoder = class(TForm)
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
    BitBtn1: TBitBtn;
    memoDecoded: TMemo;
    memoOriginalPayloadInHexa: TMemo;
    lblPasswordsInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure cbMethodPublicPayloadClick(Sender: TObject);
    procedure bbSaveMethodsClick(Sender: TObject);
    procedure memoDecodedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPayloadData : TRawBytes;
    FWalletKeys : TWalletKeys;
    FOldECPrivateKey : TECPrivateKey;
    FAccountECPrivateKey : TECPrivateKey;
    FSavedDecodeMethods : boolean;
    FAppParams : TAppParams;
    { Private declarations }
    Procedure TryToDecode;
    Procedure SaveMethods;
  public
    { Public declarations }
    Procedure Init(block, timestamp : Cardinal; const OperationText : AnsiString; Const PayloadData : TRawBytes; WalletKeys : TWalletKeys; AppParams : TAppParams);
  end;

implementation

{$R *.dfm}

Uses UNode, UTime, UECIES, UAES, UAccounts;

{ TFRMPayloadDecoder }

procedure TFRMPayloadDecoder.bbSaveMethodsClick(Sender: TObject);
begin
  SaveMethods;
  PageControl.ActivePage := tsDecoded;
  TryToDecode;
end;

procedure TFRMPayloadDecoder.cbMethodPublicPayloadClick(Sender: TObject);
begin
  FSavedDecodeMethods := false;
  lblPasswordsInfo.Caption := Format('Possible passwords: %d',[memoPasswords.Lines.Count]);
end;

procedure TFRMPayloadDecoder.FormCreate(Sender: TObject);
begin
  FWalletKeys := Nil;
  FAppParams := Nil;
  memoDecoded.Lines.Clear;
  memoOriginalPayloadInHexa.Lines.Clear;
  lblPasswordsInfo.Caption := '';
end;

procedure TFRMPayloadDecoder.Init(block, timestamp : Cardinal; const OperationText : AnsiString; Const PayloadData : TRawBytes; WalletKeys : TWalletKeys; AppParams : TAppParams);
begin
  FWalletKeys := WalletKeys;
  FAppParams := AppParams;
  lblBlock.Caption := inttostr(block);
  if timestamp>10000 then begin
    lblDateTime.Caption := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime(timestamp)));
    lblDateTime.Font.Color := clBlack;
  end else begin
    lblDateTime.Caption := '(Pending block)';
    lblDateTime.Font.Color := clRed;
  end;
  lblOperationTxt.Caption := OperationText;
  FPayloadData := PayloadData;
  memoOriginalPayloadInHexa.Lines.Text := TCrypto.ToHexaString(FPayloadData);
  if Assigned(FWalletKeys) then begin
    cbMethodPublicPayload.Checked := FAppParams.ParamByName['PayloadDecoder.notencrypted'].GetAsBoolean(true);
    cbUsingPrivateKeys.Checked := FAppParams.ParamByName['PayloadDecoder.usingprivatekeys'].GetAsBoolean(true);
    cbUsingPasswords.Checked := FAppParams.ParamByName['PayloadDecoder.usingpasswords'].GetAsBoolean(true);
    memoPasswords.Lines.Text := FAppParams.ParamByName['PayloadDecoder.passwords'].GetAsString('');
  end else begin
    cbMethodPublicPayload.Checked := true;
    cbUsingPrivateKeys.Checked := true;
    cbUsingPasswords.Checked := true;
    memoPasswords.Lines.Text := '';
  end;
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
  FAppParams.ParamByName['PayloadDecoder.notencrypted'].SetAsBoolean(cbMethodPublicPayload.Checked);
  FAppParams.ParamByName['PayloadDecoder.usingprivatekeys'].SetAsBoolean(cbUsingPrivateKeys.Checked);
  FAppParams.ParamByName['PayloadDecoder.usingpasswords'].SetAsBoolean(cbUsingPasswords.Checked);
  FAppParams.ParamByName['PayloadDecoder.passwords'].SetAsString(memoPasswords.Lines.Text);
  FSavedDecodeMethods := true;
end;

procedure TFRMPayloadDecoder.TryToDecode;
  Function UseWallet(Const raw : TRawBytes; var Decrypted : AnsiString; var WalletKey : TWalletKey) : Boolean;
  Var i : Integer;
  begin
    Result := false;
    if Not assigned(FWalletKeys) then exit;

    for i := 0 to FWalletKeys.Count - 1 do begin
      WalletKey := FWalletKeys.Key[i];
      If Assigned(WalletKey.PrivateKey) then begin
        If ECIESDecrypt(WalletKey.PrivateKey.EC_OpenSSL_NID,WalletKey.PrivateKey.PrivateKey,false,raw,Decrypted) then begin
          Result := true;
          exit;
        end;
      end;
    end;

  end;

  Function  UsePassword(const raw : TRawBytes; var Decrypted,PasswordUsed : AnsiString) : Boolean;
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
  Decrypted,PasswordUsed : AnsiString;
  ok : boolean;
begin
  ok := true;
    raw := FPayloadData;
    if raw<>'' then begin
      // First try to a human readable...
      if (cbMethodPublicPayload.Checked) and (TCrypto.IsHumanReadable(raw)) then begin
        memoDecoded.Lines.Text := raw;
        lblDecodedMethod.Caption := 'Not encrypted payload';
      end else if (cbUsingPrivateKeys.Checked) And (UseWallet(raw,Decrypted,WalletKey)) then begin
        memoDecoded.Lines.Text := Decrypted;
        lblDecodedMethod.Caption := 'Encrypted with EC '+TAccountComp.GetECInfoTxt(WalletKey.PrivateKey.EC_OpenSSL_NID);
      end else if (cbUsingPasswords.Checked) And (UsePassword(raw,Decrypted,PasswordUsed)) then begin
        memoDecoded.Lines.Text := Decrypted;
        lblDecodedMethod.Caption := 'Encrypted with Pwd:"'+PasswordUsed+'"';
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
