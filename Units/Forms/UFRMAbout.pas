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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
    Procedure OpenURL(Url : String);
  public
    { Public declarations }
  end;

implementation

uses UFolderHelper, UConst, ShellApi;

{$R *.dfm}

procedure TFRMAbout.FormCreate(Sender: TObject);
Var fvi : TFileVersionInfo;
begin
  fvi := TFolderHelper.GetTFileVersionInfo(Application.ExeName);
  lblBuild.Caption :=  'Build: '+fvi.FileVersion;
  lblProtocolVersion.Caption := Format('BlockChain Protocol: %d (%d)  -  Net Protocol: %d (%d)',[CT_BlockChain_Protocol_Version,CT_BlockChain_Protocol_Available,
    CT_NetProtocol_Version, CT_NetProtocol_Available]);
end;

procedure TFRMAbout.Label4Click(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TFRMAbout.Label5Click(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TFRMAbout.OpenURL(Url: String);
begin
  shellexecute(0, 'open', pchar(URL), nil, nil, SW_SHOW)
end;

end.
