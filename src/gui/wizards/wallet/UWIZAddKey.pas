unit UWIZAddKey;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, Forms, UWizard, UDataObjects;

type

  { TWIZAddKeyWizard }

  TWIZAddKeyWizard = class(TWizard<TWIZAddKeyModel>)
    public
      constructor Create(AOwner: TComponent); override;
      function DetermineHasNext : boolean; override;
      function DetermineHasPrevious : boolean;  override;
      function FinishRequested(out message : AnsiString) : boolean; override;
      function CancelRequested(out message : AnsiString) : boolean; override;
  end;

implementation

uses
  UCrypto,
  UWallet,
  UWIZAddKey_Start,
  UWIZAddKey_GenerateOrImport,
  UWIZAddKey_ImportPubKey,
  UWIZAddKey_ImportPrivKey,
  UWIZAddKey_EnterName;

constructor TWIZAddKeyWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [TWIZAddKey_Start, TWIZAddKey_EnterName]);
  TitleText := 'Add Key';
  FinishText := 'Add Key';
end;

function TWIZAddKeyWizard.DetermineHasNext : boolean;
begin
  Result := NOT (CurrentScreen is TWIZAddKey_EnterName);
end;

function TWIZAddKeyWizard.DetermineHasPrevious : boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZAddKeyWizard.FinishRequested(out message : AnsiString) : boolean;
begin
  // Execute the key addition here
  try
    Result := true;
    case Model.Action of
      akaGenerateKey: TWallet.GenerateNewKey(Model.Name, Model.EncryptionTypeNID);
      akaImportPrivateKey: TWallet.ImportPrivateKey(Model.Name, Model.KeyText, Model.Password);
      akaImportPublicKey: TWallet.ImportPublicKey(Model.Name, Model.KeyText);
    end;
  except
      On E:Exception do begin
        Result := false;
        message := E.ToString;
      end;
  end;
end;

function TWIZAddKeyWizard.CancelRequested(out message : AnsiString) : boolean;
begin
  Result := true;
end;

end.

