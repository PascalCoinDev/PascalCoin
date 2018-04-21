unit UWIZOperationPayload_Encryption;

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
  UWizard, UWIZOperationPayload_Content, UWIZOperationPayload_Password, UWIZModels;

type

  { TWIZOperationPayload_Encryption }

  TWIZOperationPayload_Encryption = class(TWizardForm<TWIZOperationsModel>)
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

{ TWIZOperationPayload_Encryption }

procedure TWIZOperationPayload_Encryption.OnNext;
begin
  if rbEncryptedWithRecipient.Checked then
  begin
    Model.Payload.PayloadEncryptionMode := akaEncryptWithSender;
  end
  else
  if rbEncryptedWithSender.Checked then
  begin
    Model.Payload.PayloadEncryptionMode := akaEncryptWithReceiver;
  end
  else
  if rbEncryptedWithPassword.Checked then
  begin
    Model.Payload.PayloadEncryptionMode := akaEncryptWithPassword;
  end
  else
  if rbNotEncrypted.Checked then
  begin
    Model.Payload.PayloadEncryptionMode := akaNotEncrypt;
  end;
  case Model.Payload.PayloadEncryptionMode of
    akaEncryptWithPassword:
    begin
      UpdatePath(ptInject, [TWIZOperationPayload_Password]);
    end
    else
    begin
      UpdatePath(ptInject, [TWIZOperationPayload_Content]);
    end;
  end;
end;

function TWIZOperationPayload_Encryption.Validate(out message: ansistring): boolean;
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

