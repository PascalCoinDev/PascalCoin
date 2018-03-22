unit UWIZSendPASC_Confirmation;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWizard, UWIZSendPASC;

type

  { TWIZSendPASC_Confirmation }

  TWIZSendPASC_Confirmation = class(TWizardForm<TWIZSendPASCModel>)
    Edit1: TEdit;

  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

{ TWIZSendPASC_Confirmation }

procedure TWIZSendPASC_Confirmation.OnPresent;
begin
  inherited OnPresent;
end;

procedure TWIZSendPASC_Confirmation.OnNext;
begin
  inherited OnNext;
end;

end.

