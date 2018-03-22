unit UWIZSendPASC_Completion;

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

  { TWIZSendPASC_Completion }

  TWIZSendPASC_Completion = class(TWizardForm<TWIZSendPASCModel>)
    Button1: TButton;


  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

{ TWIZSendPASC_Completion }

procedure TWIZSendPASC_Completion.OnPresent;
begin
  inherited OnPresent;
end;

procedure TWIZSendPASC_Completion.OnNext;
begin
  inherited OnNext;
end;

end.

