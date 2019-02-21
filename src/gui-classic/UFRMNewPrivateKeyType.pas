unit UFRMNewPrivateKeyType;

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

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, UWallet, UCrypto,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TFRMNewPrivateKeyType = class(TForm)
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

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

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
Var l : TList<Word>;
  i : Integer;
begin
  FGeneratedPrivateKey := Nil;
  FWalletKeys := Nil;
  ebName.Text := DateTimeToStr(now);
  rgKeyType.Items.Clear;
  l := TList<Word>.Create;
  Try
    TAccountComp.ValidsEC_OpenSSL_NID(l);
    for i := 0 to l.Count - 1 do begin
      rgKeyType.Items.AddObject(TAccountComp.GetECInfoTxt(l[i]),TObject(l[i]));
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
