unit UFRMAbout;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, StdCtrls, Buttons;

type
  TFRMAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
    bbClose: TBitBtn;
    lblBuild: TLabel;
    lblProtocolVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses UFolderHelper, UConst;

{$R *.dfm}

procedure TFRMAbout.FormCreate(Sender: TObject);
Var fvi : TFileVersionInfo;
begin
  fvi := TFolderHelper.GetTFileVersionInfo(Application.ExeName);
  lblBuild.Caption :=  'Build: '+fvi.FileVersion;
  if CT_Protocol_Available>0 then begin
    lblProtocolVersion.Caption := Format('Protocol: %d (Available: %d)',[CT_Protocol_Version,CT_Protocol_Available]);
  end else begin
    lblProtocolVersion.Caption := Format('Protocol: %d',[CT_Protocol_Version]);
  end;
end;

end.
