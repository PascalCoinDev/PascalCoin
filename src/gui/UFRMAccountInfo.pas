unit UFRMAccountInfo;

interface

{$I ..\config.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFRMAccountInfo = class(TApplicationForm)
    Label1: TLabel;
    lblAccount: TLabel;
    Label6: TLabel;
    lblBalance: TLabel;
    Label3: TLabel;
    ebPublicKey: TEdit;
    Label2: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

end.
