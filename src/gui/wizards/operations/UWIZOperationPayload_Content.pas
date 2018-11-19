unit UWIZOperationPayload_Content;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections,
  UWizard, UWIZOperation, UWIZOperationSigner_Select, UCoreObjects;

type

  { TWIZOperationPayload_Content }

  TWIZOperationPayload_Content = class(TWizardForm<TWIZOperationsModel>)
    grpPayload: TGroupBox;
    Label1: TLabel;
    mmoPayload: TMemo;
    paPayload: TPanel;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UConst, UUserInterface;

{ TWIZOperationPayload_Content }

procedure TWIZOperationPayload_Content.OnPresent;
begin
  mmoPayload.SetFocus;
end;

procedure TWIZOperationPayload_Content.OnNext;
begin
  Model.Payload.Content := mmoPayload.Lines.Text;
  if Length(Model.Account.SelectedAccounts) > 1 then
    UpdatePath(ptInject, [TWIZOperationSigner_Select])
  else
  begin
    Model.Signer.SignerAccount := Model.Account.SelectedAccounts[0];
    Model.Signer.OperationSigningMode := akaPrimary;
  end;
end;

function TWIZOperationPayload_Content.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if Length(mmoPayload.Lines.Text) > CT_MaxPayloadSize then
  begin
    message := Format('Payload Size Is Larger Than Max Payload Size Which Is "%u"', [CT_MaxPayloadSize]);
    Result := False;
    Exit;
  end;
end;

end.
