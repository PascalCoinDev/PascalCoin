unit UWIZAddKey_SelectEncryption;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, UWizard, UWIZAddKey;

type

  { TWIZAddKey_SelectEncryption }

  TWIZAddKey_SelectEncryption = class(TWizardForm<TWIZAddKeyModel>)
    rgKeyType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    class function GetOptionText(const EC_OpenSSL_NID: Word): AnsiString;
  public
    procedure OnNext; override;
    function Validate(out message : AnsiString) : boolean; override;
  end;

implementation

{$R *.lfm}

uses UAccounts, UConst, UMemory;

{ TWIZAddKey_SelectEncryption }

procedure TWIZAddKey_SelectEncryption.FormCreate(Sender: TObject);
var
  i : Integer;
  GC : TDisposables;
  availableEncryptionTypes : TList;
  name, desc : UTF8String;
begin
  rgKeyType.Items.Clear;
  availableEncryptionTypes := GC.AddObject( TList.Create ) as TList;
  TAccountComp.ValidsEC_OpenSSL_NID(availableEncryptionTypes);
  for i := 0 to availableEncryptionTypes.Count - 1 do begin
    rgKeyType.Items.AddObject(GetOptionText(PtrInt(availableEncryptionTypes[i])),availableEncryptionTypes[i]);
  end;
end;

procedure TWIZAddKey_SelectEncryption.OnNext;
begin
  Self.Model.EncryptionTypeNID := PtrInt(rgKeyType.Items.Objects[rgKeyType.ItemIndex]);
end;

function TWIZAddKey_SelectEncryption.Validate(out message : AnsiString) : boolean;
begin
  Result := true;
  if rgKeyType.ItemIndex < 0 then begin
    message := 'A type of encryption must be selected';
    Result := false;
  end;
end;


class function TWIZAddKey_SelectEncryption.GetOptionText(const EC_OpenSSL_NID: Word): AnsiString;
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
    else Result := '(Unknown ID:'+inttostr(EC_OpenSSL_NID)+')';
  end;
end;

end.

