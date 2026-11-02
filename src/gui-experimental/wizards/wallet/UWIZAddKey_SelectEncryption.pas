unit UWIZAddKey_SelectEncryption;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, UWizard, UWIZAddKey;

type

  { TWIZAddKey_SelectEncryption }

  TWIZAddKey_SelectEncryption = class(TWizardForm<TWIZAddKeyModel>)
    rgKeyType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    class function GetOptionText(const EC_OpenSSL_NID: Word): String;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message : String) : boolean; override;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UAccounts, UConst, UMemory, Generics.Collections;

{ TWIZAddKey_SelectEncryption }

procedure TWIZAddKey_SelectEncryption.FormCreate(Sender: TObject);
var
  i : Integer;
  GC : TDisposables;
  availableEncryptionTypes : TList<Word>;
begin
  rgKeyType.Items.Clear;
  availableEncryptionTypes := GC.AddObject( TList<Word>.Create ) as TList<Word>;
  TAccountComp.ValidsEC_OpenSSL_NID(availableEncryptionTypes);
  for i := 0 to availableEncryptionTypes.Count - 1 do begin
    rgKeyType.Items.AddObject(GetOptionText(availableEncryptionTypes[i]),TObject(availableEncryptionTypes[i]));
  end;
end;

procedure TWIZAddKey_SelectEncryption.OnNext;
begin
  Self.Model.EncryptionTypeNID := PtrInt(rgKeyType.Items.Objects[rgKeyType.ItemIndex]);
end;

procedure TWIZAddKey_SelectEncryption.OnPresent;
var
  i: Integer;
  LNID: Word;
begin
  LNID := Model.EncryptionTypeNID;
  if LNID = 0 then
    LNID := CT_Default_EC_OpenSSL_NID;
  for i := 0 to rgKeyType.Items.Count - 1 do
    if PtrInt(rgKeyType.Items.Objects[i]) = LNID then
    begin
      rgKeyType.ItemIndex := i;
      Break;
    end;
end;

function TWIZAddKey_SelectEncryption.Validate(out message : String) : boolean;
begin
  Result := true;
  if rgKeyType.ItemIndex < 0 then begin
    message := 'A type of encryption must be selected';
    Result := false;
  end;
end;


class function TWIZAddKey_SelectEncryption.GetOptionText(const EC_OpenSSL_NID: Word): String;
begin
  case EC_OpenSSL_NID of
    CT_NID_secp256k1 : begin
      Result := 'SECP256K1 - Cryptocurrency Standard (same as Bitcoin)';
    end;
    CT_NID_secp384r1 : begin
      Result := 'SECP384R1 - Stronger, different algorithm (larger key) ';
    end;
    CT_NID_sect283k1 : Begin
      Result := 'SECP283K1 - Even stronger (larger key)';
    End;
    CT_NID_secp521r1 : begin
      Result := 'SECP521R1 - Quantum-resistant (largest key)';
    end
    else Result := '(Unknown ID:'+ IntToStr(EC_OpenSSL_NID)+')';
  end;
end;

end.

