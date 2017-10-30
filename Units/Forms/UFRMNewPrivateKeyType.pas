unit UFRMNewPrivateKeyType;

{$MODE Delphi}

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
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, UCommonUI, UWalletKeys,UCrypto;

type
  TFRMNewPrivateKeyType = class(TApplicationForm)
    Label1: TLabel;
    ebName: TEdit;
    rgKeyType: TRadioGroup;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWalletKeys: TWalletKeys;
    FGeneratedPrivateKey: TECPrivateKey;
    procedure SetWalletKeys(const Value: TWalletKeys);
    { Private declarations }
  public
    { Public declarations }
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property GeneratedPrivateKey : TECPrivateKey read FGeneratedPrivateKey write FGeneratedPrivateKey;
  end;


implementation

uses
  UAccounts, UConst ;

{$R *.lfm}

procedure TFRMNewPrivateKeyType.bbOkClick(Sender: TObject);
begin
  if Not Assigned(WalletKeys) then exit;
  if rgKeyType.ItemIndex<0 then raise Exception.Create('Select a key type');

  if Assigned(FGeneratedPrivateKey) then FGeneratedPrivateKey.Free;

  FGeneratedPrivateKey := TECPrivateKey.Create;
  FGeneratedPrivateKey.GenerateRandomPrivateKey( PtrInt(rgKeyType.Items.Objects[rgKeyType.ItemIndex]) );
  WalletKeys.AddPrivateKey(ebName.Text,FGeneratedPrivateKey);
  ModalResult := MrOk;
end;

procedure TFRMNewPrivateKeyType.FormCreate(Sender: TObject);
Var l : TList;
  i : Integer;
begin
  FGeneratedPrivateKey := Nil;
  FWalletKeys := Nil;
  ebName.Text := DateTimeToStr(now);
  rgKeyType.Items.Clear;
  l := TList.Create;
  Try
    TAccountComp.ValidsEC_OpenSSL_NID(l);
    for i := 0 to l.Count - 1 do begin
      rgKeyType.Items.AddObject(TAccountComp.GetECInfoTxt(PtrInt(l[i])),l[i]);
    end;
  Finally
    l.free;
  End;
end;

procedure TFRMNewPrivateKeyType.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGeneratedPrivateKey);
end;

procedure TFRMNewPrivateKeyType.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
end;

end.
