unit UWIZPayloadOverride;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections,
  UWizard, UWIZSendPASC, UWIZPayloadContentOverride, UWIZPayloadPasswordOverride, UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZPayloadOverride }

  TWIZPayloadOverride = class(TWizardForm<TWIZOperationsModel>)
    grpPayload: TGroupBox;
    Label1: TLabel;
    lblNoEncryption: TLabel;
    lblNoEncryption1: TLabel;
    lblNoEncryption2: TLabel;
    lblNote1: TLabel;
    paPayload: TPanel;
    rbEncryptedWithRecipient: TRadioButton;
    rbEncryptedWithSender: TRadioButton;
    rbEncryptedWithPassword: TRadioButton;
    rbNotEncrypted: TRadioButton;
  public
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface;

{ TWIZPayloadOverride }

procedure TWIZPayloadOverride.OnNext;
begin
  if rbEncryptedWithRecipient.Checked then
  begin
    Model.Payload.Mode := akaEncryptWithSender;
  end
  else
  if rbEncryptedWithSender.Checked then
  begin
    Model.Payload.Mode := akaEncryptWithReceiver;
  end
  else
  if rbEncryptedWithPassword.Checked then
  begin
    Model.Payload.Mode := akaEncryptWithPassword;
  end
  else
  if rbNotEncrypted.Checked then
  begin
    Model.Payload.Mode := akaNotEncrypt;
  end;
  case Model.Payload.Mode of
    akaEncryptWithPassword:
    begin
      UpdatePath(ptReplaceAllNext, [TWIZPayloadPasswordOverride,
        TWIZSendPASC_Confirmation]);
    end
    else
    begin
      UpdatePath(ptReplaceAllNext, [TWIZPayloadContentOverride,
        TWIZSendPASC_Confirmation]);
    end;
  end;
end;

function TWIZPayloadOverride.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if (not rbNotEncrypted.Checked) and (not rbEncryptedWithSender.Checked) and
    (not rbEncryptedWithRecipient.Checked) and (not rbEncryptedWithPassword.Checked) then
  begin
    message := 'you must select an encryption option for payload';
    Result := False;
    Exit;
  end;
end;

end.
