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
  SysUtils, StdCtrls, ExtCtrls, UWizard, UWIZOperation;

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
  UConst,
  UCoreObjects,
  UWIZOperationSigner_Select;

{ TWIZOperationPayload_Content }

procedure TWIZOperationPayload_Content.OnPresent;
begin
  mmoPayload.SetFocus;
end;

procedure TWIZOperationPayload_Content.OnNext;
begin
  Model.Payload.Content := mmoPayload.Lines.Text;

  case Model.ExecuteOperationType of
    omtSendPasc, omtBuyAccount:
    begin
      // do nothing
    end;

    omtChangeInfo:
    begin
      if Model.Account.Count = 1 then
      begin
        UpdatePath(ptInject, [TWIZOperationSigner_Select]);
      end;
    end
    else
    begin
      UpdatePath(ptInject, [TWIZOperationSigner_Select]);
    end;
  end;

end;

function TWIZOperationPayload_Content.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if Length(mmoPayload.Lines.Text) > CT_MaxPayloadSize then
  begin
    message := Format('Payload Size Is Larger Than Max Payload Size Which Is "%u"', [CT_MaxPayloadSize]);
    Exit(False);
  end;
end;

end.
