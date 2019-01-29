unit UWIZDelistAccountFromSale_Options;

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
  SysUtils, StdCtrls, UWizard, UWIZOperation;

type

  { TWIZDelistAccountFromSale_Options }

  TWIZDelistAccountFromSale_Options = class(TWizardForm<TWIZOperationsModel>)
    chkAttachPayload: TCheckBox;
    chkChooseFee: TCheckBox;
    gbOptions: TGroupBox;

  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  USettings,
  UCoreObjects,
  UWIZOperationFee_Custom,
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

{ TWIZDelistAccountFromSale_Options }

procedure TWIZDelistAccountFromSale_Options.OnPresent;
begin
  if Model.Account.Count > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
end;

procedure TWIZDelistAccountFromSale_Options.OnNext;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;

  if chkChooseFee.Checked then
    UpdatePath(ptInject, [TWIZOperationFee_Custom])
  else
  begin
    Model.Fee.SingleOperationFee := TSettings.DefaultFee;
    if Model.Payload.HasPayload then
      UpdatePath(ptInject, [TWIZOperationPayload_Encryption])
    else
    begin
      UpdatePath(ptInject, [TWIZOperationSigner_Select]);
    end;
  end;
end;

function TWIZDelistAccountFromSale_Options.Validate(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
