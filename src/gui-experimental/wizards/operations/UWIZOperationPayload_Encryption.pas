unit UWIZOperationPayload_Encryption;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  StdCtrls, ExtCtrls, UWizard, UWIZOperation;

type

  { TWIZOperationPayload_Encryption }

  TWIZOperationPayload_Encryption = class(TWizardForm<TWIZOperationsModel>)
    grpPayload: TGroupBox;
    lblEncryptWithPassword: TLabel;
    lblNoEncryption: TLabel;
    lblEncryptWithRecipientKey: TLabel;
    lblEncryptWithSenderKey: TLabel;
    lblNote1: TLabel;
    paPayload: TPanel;
    rbEncryptedWithRecipient: TRadioButton;
    rbEncryptedWithSender: TRadioButton;
    rbEncryptedWithPassword: TRadioButton;
    rbNotEncrypted: TRadioButton;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts,
  UCoreObjects,
  UWIZOperationPayload_Content,
  UWIZOperationPayload_Password;

{ TWIZOperationPayload_Encryption }

procedure TWIZOperationPayload_Encryption.OnPresent;
begin
  case Model.ExecuteOperationType of
    omtChangeInfo:
    begin
      rbEncryptedWithRecipient.Enabled := False;
      lblEncryptWithRecipientKey.Enabled := False;
    end;
  end;
end;

procedure TWIZOperationPayload_Encryption.OnNext;
begin
  if rbEncryptedWithRecipient.Checked then
    Model.Payload.PayloadEncryptionMode := pemEncryptWithRecipient
  else
  if rbEncryptedWithSender.Checked then
    Model.Payload.PayloadEncryptionMode := pemEncryptWithSender
  else
  if rbEncryptedWithPassword.Checked then
    Model.Payload.PayloadEncryptionMode := pemEncryptWithPassword
  else
  if rbNotEncrypted.Checked then
    Model.Payload.PayloadEncryptionMode := pemNotEncrypt;
  case Model.Payload.PayloadEncryptionMode of
    pemEncryptWithPassword:
      UpdatePath(ptInject, [TWIZOperationPayload_Password])
    else
      UpdatePath(ptInject, [TWIZOperationPayload_Content]);
  end;
end;

function TWIZOperationPayload_Encryption.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if (not rbNotEncrypted.Checked) and (not rbEncryptedWithSender.Checked) and
    (not rbEncryptedWithRecipient.Checked) and (not rbEncryptedWithPassword.Checked) then
  begin
    message := 'You must select an encryption option for payload';
    Exit(False);
  end;

  case Model.ExecuteOperationType of
    omtChangeInfo:
    begin
      if rbEncryptedWithRecipient.Checked then
      begin
        message := 'This operation does not support this type of payload encryption';
        Exit(False);
      end;
    end;
  end;
end;

end.

