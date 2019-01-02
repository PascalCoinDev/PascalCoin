unit UFRMAbout;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  pngimage, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type

  { TFRMAbout }

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

uses
{$IFnDEF FPC}
  ShellApi,
{$ELSE}
{$ENDIF}
  UFolderHelper, UConst, UNode, UOpenSSL;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFRMAbout.FormCreate(Sender: TObject);
begin
  lblBuild.Caption :=  'Build: '+CT_ClientAppVersion+' OpenSSL: '+IntToHex(OpenSSLVersion,8)+' Compiler: '{$IFDEF FPC}+'FPC'{$IFDEF CPU32}+' 32b'{$ELSE}+' 64b'{$ENDIF}{$ELSE}+'Delphi'{$IFDEF CPU32BITS}+' 32b'{$ELSE}+' 64b'{$ENDIF}{$ENDIF};
  lblProtocolVersion.Caption := Format('BlockChain Protocol: %d (%d)  -  Net Protocol: %d (%d)',[TNode.Node.Bank.SafeBox.CurrentProtocol,CT_BlockChain_Protocol_Available,
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
  {$IFDEF FPC}
  OpenDocument(pchar(URL))
  {$ELSE}
  shellexecute(0, 'open', pchar(URL), nil, nil, SW_SHOW)
  {$ENDIF}
end;

end.
