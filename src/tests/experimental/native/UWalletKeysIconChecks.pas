unit UWalletKeysIconChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Checks the actual Wallet Keys button on a fresh, offline test wallet. The
  caller supplies a directory owned by its private-desktop test run.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes;

// Requires an empty wallet without a password and restores that state on exit.
// Saves each button through its native paint handler and returns pixel counts.
function CheckWalletKeysIcons(AOwner: TComponent; const AOutputDirectory: String): String;

implementation

uses
  Windows, Messages, SysUtils, Types, Forms, Graphics, UWallet, UConst, UFRMWalletKeys,
  UCoreUtils, UAccounts, UBaseTypes, UPCDataTypes,
  {$IFDEF FPC}LMessages,{$ENDIF}
  {$IFDEF FPC}Generics.Defaults{$ELSE}System.Generics.Defaults{$ENDIF};

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Wallet Keys icons: ' + AMessage);
end;

function CheckWalletKeysIcons(AOwner: TComponent; const AOutputDirectory: String): String;
var
  LForm: TFRMWalletKeys;
  LOutput: String;

  function CaptureButton: TBitmap;
  begin
    Result := TBitmap.Create;
    try
      Result.PixelFormat := pf24bit;
      Result.SetSize(LForm.btnLock.Width, LForm.btnLock.Height);
      Result.Canvas.Brush.Color := LForm.Color;
      Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
      // TGraphicControl's paint message uses the supplied DC and invokes the
      // real native speed-button Paint. LCL GetFormImage is black on a hidden
      // Windows desktop, so do not use a screen copy as pixel evidence.
      LForm.btnLock.Perform({$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF},
        WPARAM(Result.Canvas.Handle), 0);
    except
      Result.Free;
      raise;
    end;
  end;

  function CheckState(const AName, ACaption: String; AEnabled: Boolean;
    AImageIndex: Integer): Integer;
  var
    LWithIcon, LWithoutIcon: TBitmap;
    X, Y, LSavedIndex: Integer;
  begin
    // Deliver the same refresh message posted by the real wallet-change event.
    SendMessage(LForm.Handle, CM_PC_WalletChanged, 0, 0);
    Require(LForm.lblKeysEncrypted.Caption = ACaption, AName + ': incorrect state caption');
    Require(LForm.btnLock.Enabled = AEnabled, AName + ': incorrect enabled state');
    Require(LForm.ilIcons.Count = 2, AName + ': expected both original lock images');
    Require((LForm.ilIcons.Width = 64) and (LForm.ilIcons.Height = 64),
      AName + ': changed image dimensions');
    Require(LForm.btnLock.Images = LForm.ilIcons, AName + ': native image-list binding missing');
    Require(LForm.btnLock.ImageIndex = AImageIndex, AName + ': incorrect lock image');
    Require(LForm.btnLock.Glyph.Empty, AName + ': unexpected legacy bitmap glyph');
    Require(LForm.btnLock.NumGlyphs = 1, AName + ': unexpected glyph frame count');

    LWithIcon := CaptureButton;
    LWithoutIcon := nil;
    LSavedIndex := LForm.btnLock.ImageIndex;
    try
      LWithIcon.PixelFormat := pf24bit;
      LWithIcon.SaveToFile(LOutput + AName + '-button.bmp');
      // Compare against the same native button without its image. Keeping the
      // image-list binding intact avoids changing its native rendering owner.
      LForm.btnLock.ImageIndex := -1;
      LWithoutIcon := CaptureButton;
      LWithoutIcon.PixelFormat := pf24bit;
      Result := 0;
      for Y := 6 to LForm.btnLock.Height - 7 do
        for X := 6 to LForm.btnLock.Width - 7 do
          if LWithIcon.Canvas.Pixels[X, Y] <> LWithoutIcon.Canvas.Pixels[X, Y] then
            Inc(Result);
      Require(Result >= 100, AName + ': padlock is visually blank');
    finally
      LForm.btnLock.ImageIndex := LSavedIndex;
      LWithIcon.Free;
      LWithoutIcon.Free;
    end;
  end;

var
  LDisabled, LUnlocked, LLocked, LReopened: Integer;
  LKey: TAccountKey;
  LExpected: UInt32;
  I: Integer;
begin
  Require((TWallet.Keys.Count = 0) and not TWallet.Keys.HasPassword,
    'requires a fresh empty wallet without a password');
  LOutput := IncludeTrailingPathDelimiter(AOutputDirectory);
  Require(DirectoryExists(LOutput), 'output directory does not exist');
  // A native Delphi string hash may be negative. Exercise its high bit with
  // range checks enabled rather than relying on a randomly generated key.
  LKey := Default(TAccountKey);
  LKey.EC_OpenSSL_NID := CT_Default_EC_OpenSSL_NID;
  LExpected := 0;
  for I := 0 to 4095 do begin
    LKey.x.FromString('Native account hash regression ' + IntToStr(I));
    LExpected := UInt32(TEqualityComparer<AnsiString>.Default.GetHashCode(
      IntToStr(LKey.EC_OpenSSL_NID) + LKey.x.ToString + LKey.y.ToString));
    if (LExpected and $80000000) <> 0 then
      Break;
  end;
  Require((LExpected and $80000000) <> 0, 'high-bit string hash fixture not found');
  Require(TAccountKeyEqualityComparer.CalcHashCode(LKey) = LExpected,
    'account-key comparer did not preserve the signed string hash bits');
  LForm := TFRMWalletKeys.Create(AOwner);
  try
    LForm.Show;
    LDisabled := CheckState('wallet-keys-no-password', 'Wallet has no password', false, 1);
    TWallet.GenerateNewKey('Disposable native icon QA key', CT_Default_EC_OpenSSL_NID);
    TWallet.Keys.WalletPassword := 'Disposable native icon QA password';
    Require(TWallet.Keys.HasPassword and TWallet.Keys.IsValidPassword, 'encryption failed');
    LUnlocked := CheckState('wallet-keys-unlocked', 'Wallet is unlocked', true, 1);
    Require(TWallet.Keys.LockWallet, 'locking failed');
    LLocked := CheckState('wallet-keys-locked', 'Wallet is locked', true, 0);
    TWallet.Keys.WalletPassword := 'Disposable native icon QA password';
    Require(TWallet.Keys.IsValidPassword, 'unlocking failed');
    LReopened := CheckState('wallet-keys-unlocked-again', 'Wallet is unlocked', true, 1);
    Result := Format('WALLET KEYS ICONS: native images=2, 64x64, painted pixels disabled=%d, unlocked=%d, locked=%d, reopened=%d; high-bit hash=%s',
      [LDisabled, LUnlocked, LLocked, LReopened, IntToHex(LExpected, 8)]);
  finally
    LForm.Free;
    // Delete persists each removal in the owned QA file, including an error
    // raised while the encrypted wallet was locked.
    while TWallet.Keys.Count > 0 do
      TWallet.Keys.Delete(TWallet.Keys.Count - 1);
    TWallet.Keys.Clear;
    TWallet.Keys.WalletPassword := '';
    Require((TWallet.Keys.Count = 0) and not TWallet.Keys.HasPassword,
      'did not restore the original empty wallet');
  end;
end;

end.
