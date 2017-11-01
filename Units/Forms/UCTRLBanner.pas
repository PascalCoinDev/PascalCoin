unit UCTRLBanner;

{$mode delphi}

interface

{$I ./../PascalCoin/config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus;

type

  { TForm1 }

  { TCTRLBanner }

  TCTRLBanner = class(TForm)
    imBackground : TImage;
    imLogo: TImage;
    ilLogoImages: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses UCommonUI;

{$R *.lfm}

{ TCTRLBanner }

procedure TCTRLBanner.FormCreate(Sender: TObject);
begin
  {$ifdef TESTNET}
  imLogo.SetImageListPicture(ilLogoImages, 0);
  {$else}
  imLogo.SetImageListPicture(ilLogoImages, 1);
  {$endif}
end;


end.

