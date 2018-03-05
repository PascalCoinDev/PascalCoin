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
  public
    procedure OnNext; override;
    function Validate(out message : AnsiString) : boolean; override;
  end;

implementation

{$R *.lfm}

uses UAccounts;

{ TWIZAddKey_SelectEncryption }

procedure TWIZAddKey_SelectEncryption.FormCreate(Sender: TObject);
var
  i : Integer;
  availableEncryptionTypes : TList;
begin
  rgKeyType.Items.Clear;
  availableEncryptionTypes := TList.Create;
  try
    TAccountComp.ValidsEC_OpenSSL_NID(availableEncryptionTypes);
    for i := 0 to availableEncryptionTypes.Count - 1 do begin
      rgKeyType.Items.AddObject(TAccountComp.GetECInfoTxt(PtrInt(availableEncryptionTypes[i])),availableEncryptionTypes[i]);
    end;
  finally
    availableEncryptionTypes.free;
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

end.

