unit UCTRLBanner;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}


interface

{$I ./../PascalCoin/config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, UConst;

type

  { TCTRLBanner }

  TCTRLBanner = class(TForm)
    cbLanguage: TComboBox;
    imBackground : TImage;
    imLogo: TImage;
    ilLogoImages: TImageList;
    lblClientVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses UCommon.UI;

{$R *.lfm}

{ TCTRLBanner }

procedure TCTRLBanner.FormCreate(Sender: TObject);
begin
  {$ifdef TESTNET}
  imLogo.SetImageListPicture(ilLogoImages, 1);
  {$else}
  imLogo.SetImageListPicture(ilLogoImages, 0);
  {$endif}
  lblClientVersion.Caption := CT_ClientAppVersion;
end;


end.

