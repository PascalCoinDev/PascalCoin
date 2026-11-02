unit UCTRLBanner;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Displays the wallet logo, version, node status and available languages.
  Language changes update the saved startup preference. }

{ Public types in this unit

  Type        | Purpose
  ------------+-------------------------------------------------------------------------
  TCTRLBanner | Displays wallet branding, node status and the language selector.
}

interface

{$I ..\config.inc}

uses
  Classes, SysUtils, {$IFDEF FPC}FileUtil,{$ENDIF} Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, UConst, UWalletLanguages;

type

  { TCTRLBanner }

  TCTRLBanner = class(TForm)
    cbLanguage: TComboBox;
    imBackground : TImage;
    imLogo: TImage;
    ilLogoImages: TImageList;
    lblClientVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
  private
    FLanguages: TArray<TWalletLanguage>;
    FRefreshingLanguage: Boolean;
    FNodeStatus: TLabel;
    procedure RefreshLanguage(Sender: TObject);
    procedure RefreshNodeStatus(Sender: TObject);
  public
    property NodeStatusLabel: TLabel read FNodeStatus;
  end;

implementation

uses Math, UCommon, UCommon.UI, UUserInterface, USettings, UCTRLSyncronization;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TCTRLBanner }

procedure TCTRLBanner.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  {$IFNDEF FPC}
  // VCL's label default depends on the current Windows theme.
  lblClientVersion.Transparent := True;
  {$ENDIF}
  {$ifdef TESTNET}
  imLogo.SetImageListPicture(ilLogoImages, 1);
  {$else}
  imLogo.SetImageListPicture(ilLogoImages, 0);
  {$endif}
  lblClientVersion.Caption := CT_ClientAppVersion;
  FNodeStatus := TLabel.Create(Self);
  FNodeStatus.Name := 'NodeStatus';
  FNodeStatus.Parent := Self;
  FNodeStatus.AutoSize := False;
  FNodeStatus.Alignment := taLeftJustify;
  FNodeStatus.Layout := tlCenter;
  FNodeStatus.Transparent := True;
  FNodeStatus.ParentFont := False;
  FNodeStatus.Font.Size := 16;
  FNodeStatus.Font.Style := [fsBold];
  FNodeStatus.Font.Color := clWhite;
  TUserInterface.StateChanged.Add(RefreshNodeStatus);
  TUserInterface.SynchronizationChanged.Add(RefreshNodeStatus);
  RefreshNodeStatus(Self);
  TUserInterface.AppStarted.Add(RefreshLanguage);
  FLanguages := TWalletLanguages.Available;
  FRefreshingLanguage := true;
  try
    cbLanguage.Items.Clear;
    for i := 0 to High(FLanguages) do
      cbLanguage.Items.Add(FLanguages[i].DisplayName);
    cbLanguage.Enabled := Length(FLanguages) > 1;
  finally
    FRefreshingLanguage := false;
  end;
  FormResize(Self);
end;

procedure TCTRLBanner.FormResize(Sender: TObject);
var
  LLeft, LTop, LStatusLeft, LStatusRight, LStatusWidth: Integer;
begin
  if (not Assigned(cbLanguage)) or (not Assigned(imLogo)) then Exit;
  // The logo's left inset scales with the form; use the same inset at the
  // opposite edge instead of retaining a design-time position after docking.
  LLeft := ClientWidth - cbLanguage.Width - imLogo.Left;
  if LLeft < 0 then LLeft := 0;
  LTop := (ClientHeight - cbLanguage.Height) div 2;
  if LTop < 0 then LTop := 0;
  cbLanguage.SetBounds(LLeft, LTop, cbLanguage.Width, cbLanguage.Height);
  if Assigned(FNodeStatus) then begin
    // Start status text after the brand and reserve the remaining width up to
    // the language selector, including after DPI changes and narrow resizes.
    LStatusLeft := lblClientVersion.Left + lblClientVersion.Width + imLogo.Left;
    LStatusRight := cbLanguage.Left - imLogo.Left;
    LStatusWidth := Max(0,LStatusRight-LStatusLeft);
    FNodeStatus.SetBounds(LStatusLeft,0,LStatusWidth,ClientHeight);
    FNodeStatus.Visible := LStatusWidth>0;
  end;
end;

procedure TCTRLBanner.FormDestroy(Sender: TObject);
begin
  TUserInterface.StateChanged.Remove(RefreshNodeStatus);
  TUserInterface.SynchronizationChanged.Remove(RefreshNodeStatus);
  TUserInterface.AppStarted.Remove(RefreshLanguage);
end;

procedure TCTRLBanner.RefreshNodeStatus(Sender: TObject);
begin
  if Assigned(FNodeStatus) then
    // Normal online operation needs no banner notice; retain other states.
    if TUserInterface.State = uisActive then FNodeStatus.Caption := ''
    else FNodeStatus.Caption := TCTRLSyncronization.CurrentNodeStatusText;
end;

procedure TCTRLBanner.RefreshLanguage(Sender: TObject);
var
  code: String;
  i: Integer;
begin
  FRefreshingLanguage := true;
  try
    code := TWalletLanguages.ReadPreference;
    cbLanguage.ItemIndex := -1;
    for i := 0 to High(FLanguages) do
      if SameText(FLanguages[i].Code, code) then begin
        cbLanguage.ItemIndex := i;
        break;
      end;
  finally
    FRefreshingLanguage := false;
  end;
end;

procedure TCTRLBanner.cbLanguageChange(Sender: TObject);
begin
  if FRefreshingLanguage or (not TUserInterface.Started) or
    (cbLanguage.ItemIndex < 0) then exit;
  if SameText(TWalletLanguages.ReadPreference, FLanguages[cbLanguage.ItemIndex].Code) then exit;
  TWalletLanguages.WritePreference(FLanguages[cbLanguage.ItemIndex].Code);
  TSettings.Save;
  TUserInterface.ShowInfo(Self, 'Interface language',
    'The interface language will change the next time you start PascalCoin.');
end;


end.

