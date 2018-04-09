unit UWIZChangeAccountPrivateKey_TransactionPayload;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections,
  UWizard, UWIZChangeAccountPrivateKey, UWIZModels;

type

  { TWIZChangeAccountPrivateKey_TransactionPayload }

  TWIZChangeAccountPrivateKey_TransactionPayload = class(TWizardForm<TWIZOperationsModel>)
    edtPassword: TEdit;
    grpPayload: TGroupBox;
    Label1: TLabel;
    lblPassword: TLabel;
    mmoPayload: TMemo;
    paPayload: TPanel;
    rbEncryptedWithOldEC: TRadioButton;
    rbEncryptedWithEC: TRadioButton;
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

{ TWIZChangeAccountPrivateKey_TransactionPayload }

procedure TWIZChangeAccountPrivateKey_TransactionPayload.OnNext;
begin
  Model.ChangeAccountPrivateKeyModel.Payload := mmoPayload.Lines.Text;
  if rbEncryptedWithOldEC.Checked then
  begin
    Model.ChangeAccountPrivateKeyModel.PayloadEncryptionMode := akaEncryptWithSender;
  end
  else
  if rbEncryptedWithEC.Checked then
  begin
    Model.ChangeAccountPrivateKeyModel.PayloadEncryptionMode := akaEncryptWithReceiver;
  end
  else
  if rbEncryptedWithPassword.Checked then
  begin
    Model.ChangeAccountPrivateKeyModel.PayloadEncryptionMode := akaEncryptWithPassword;
  end
  else
  if rbNotEncrypted.Checked then
  begin
    Model.ChangeAccountPrivateKeyModel.PayloadEncryptionMode := akaNotEncrypt;
  end;
end;

function TWIZChangeAccountPrivateKey_TransactionPayload.Validate(out message: ansistring): boolean;
begin
  if (not rbNotEncrypted.Checked) and (not rbEncryptedWithEC.Checked) and
    (not rbEncryptedWithOldEC.Checked) and (not rbEncryptedWithPassword.Checked) then
  begin
    message := 'you must select an encryption option for payload';
    Result := False;
    Exit;
  end;
end;

end.
