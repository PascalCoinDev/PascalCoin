unit UWalletFooterChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Native footer checks include the status panel's painted bevel. The status
  window's ClientRect alone includes that border and is not safe child space.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, Types, Controls, ComCtrls;

type
  TWalletFooterGeometry = record
    ClientRect, PanelRect, ContentRect, ToolbarRect: TRect;
    HorizontalBorder, VerticalBorder, PanelSpacing: Integer;
    function Description: String;
  end;

function ReadWalletFooterGeometry(AStatus: TStatusBar; AToolbar: TToolBar): TWalletFooterGeometry;
procedure RequireWalletFooterContent(const AGeometry: TWalletFooterGeometry; const ACheckpoint: String);
// Uses the real native control paint; returns protected border pixels changed
// by the toolbar. The caller can save a complete form image independently.
function CheckWalletFooterPaint(AStatus: TStatusBar; AToolbar: TToolBar;
  const AOutputFile: String): Integer;

implementation

uses
  Windows, Messages, CommCtrl, SysUtils, Forms, Graphics, Themes,
  {$IFDEF FPC}LMessages,{$ENDIF} Math;

function RectText(const ARect: TRect): String;
begin
  Result := Format('%d,%d,%d,%d', [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
end;

function TWalletFooterGeometry.Description: String;
begin
  Result := Format('client=%s panel=%s content=%s toolbar=%s borders=%d,%d,%d',
    [RectText(ClientRect), RectText(PanelRect), RectText(ContentRect), RectText(ToolbarRect),
     HorizontalBorder, VerticalBorder, PanelSpacing]);
end;

function ReadWalletFooterGeometry(AStatus: TStatusBar; AToolbar: TToolBar): TWalletFooterGeometry;
var
  LBorders: array[0..2] of Integer;
  LInset, LThemed: TRect;
  LDetails: TThemedElementDetails;
begin
  FillChar(LBorders, SizeOf(LBorders), 0);
  Result := Default(TWalletFooterGeometry);
  Result.ClientRect := AStatus.ClientRect;
  if AStatus.Panels.Count = 0 then raise Exception.Create('Footer has no native status panels');
  if SendMessage(AStatus.Handle, SB_GETRECT, AStatus.Panels.Count - 1, LPARAM(@Result.PanelRect)) = 0 then
    raise Exception.Create('Cannot query native last status panel');
  SendMessage(AStatus.Handle, SB_GETBORDERS, 0, LPARAM(@LBorders[0]));
  Result.HorizontalBorder := LBorders[0];
  Result.VerticalBorder := LBorders[1];
  Result.PanelSpacing := LBorders[2];
  Result.ContentRect := Result.PanelRect;
  {$IFDEF FPC}
  LDetails := ThemeServices.GetElementDetails(tsPane);
  LThemed := ThemeServices.ContentRect(0, LDetails, Result.PanelRect);
  {$ELSE}
  LDetails := StyleServices.GetElementDetails(tsPane);
  if not StyleServices.GetElementContentRect(0, LDetails, Result.PanelRect, LThemed) then
    LThemed := Result.PanelRect;
  {$ENDIF}
  IntersectRect(Result.ContentRect, Result.ContentRect, LThemed);
  // Both native widgetsets paint the pane bevel on SB_GETRECT before drawing
  // its contents; VCL's own owner-draw path explicitly insets this by one.
  if AStatus.Panels[AStatus.Panels.Count - 1].Bevel <> pbNone then begin
    LInset := Result.PanelRect;
    InflateRect(LInset, -1, -1);
    IntersectRect(Result.ContentRect, Result.ContentRect, LInset);
  end;
  GetWindowRect(AToolbar.Handle, Result.ToolbarRect);
  MapWindowPoints(0, AStatus.Handle, Result.ToolbarRect, 2);
end;

procedure RequireWalletFooterContent(const AGeometry: TWalletFooterGeometry; const ACheckpoint: String);
begin
  with AGeometry do
    if (ToolbarRect.Left < ContentRect.Left) or (ToolbarRect.Top < ContentRect.Top) or
      (ToolbarRect.Right > ContentRect.Right) or (ToolbarRect.Bottom > ContentRect.Bottom) then
      raise Exception.Create(ACheckpoint + ': footer paints over status-panel content/border; ' + Description);
end;

function PaintStatus(AStatus: TStatusBar; AToolbar: TToolBar): TBitmap;
var
  LOrigin: TPoint;
  {$IFNDEF FPC}LForm: TCustomForm; LWhole: TBitmap;{$ELSE}LSavedDC: Integer; LToolbar: TRect;{$ENDIF}
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.SetSize(AStatus.ClientWidth, AStatus.ClientHeight);
  Result.Canvas.Brush.Color := clFuchsia;
  Result.Canvas.FillRect(Types.Rect(0, 0, Result.Width, Result.Height));
  {$IFNDEF FPC}
  LForm := GetParentForm(AStatus);
  LOrigin := Types.Point(0, 0);
  MapWindowPoints(AStatus.Handle, LForm.Handle, LOrigin, 1);
  LWhole := LForm.GetFormImage;
  try
    LWhole.PixelFormat := pf24bit;
    Result.Canvas.CopyRect(Types.Rect(0, 0, Result.Width, Result.Height), LWhole.Canvas,
      Types.Rect(LOrigin.X, LOrigin.Y, LOrigin.X + Result.Width, LOrigin.Y + Result.Height));
  finally LWhole.Free; end;
  {$ELSE}
  // Native status painting omits the LCL toolbar's graphic children. Invoke
  // its real paint handler into the same DC at the native child coordinates.
  SendMessage(AStatus.Handle, WM_PRINT, WPARAM(Result.Canvas.Handle), PRF_CLIENT or PRF_ERASEBKGND);
  if AToolbar.Visible then begin
    GetWindowRect(AToolbar.Handle, LToolbar);
    MapWindowPoints(0, AStatus.Handle, LToolbar, 2);
    LSavedDC := SaveDC(Result.Canvas.Handle);
    try
      SetViewportOrgEx(Result.Canvas.Handle, LToolbar.Left, LToolbar.Top, nil);
      IntersectClipRect(Result.Canvas.Handle, 0, 0, AToolbar.Width, AToolbar.Height);
      AToolbar.Perform(LM_PAINT, WPARAM(Result.Canvas.Handle), 0);
    finally RestoreDC(Result.Canvas.Handle, LSavedDC); end;
  end;
  {$ENDIF}
end;

function CheckWalletFooterPaint(AStatus: TStatusBar; AToolbar: TToolBar;
  const AOutputFile: String): Integer;
var
  LGeometry: TWalletFooterGeometry;
  LWithToolbar, LWithoutToolbar: TBitmap;
  X, Y, LInsideChanges: Integer;
  LVisible: Boolean;
begin
  LGeometry := ReadWalletFooterGeometry(AStatus, AToolbar);
  LWithToolbar := PaintStatus(AStatus, AToolbar);
  LWithoutToolbar := nil;
  LVisible := AToolbar.Visible;
  try
    LWithToolbar.SaveToFile(AOutputFile);
    AToolbar.Visible := false;
    LWithoutToolbar := PaintStatus(AStatus, AToolbar);
    Result := 0; LInsideChanges := 0;
    for Y := 0 to LWithToolbar.Height - 1 do
      for X := Max(0, LGeometry.ToolbarRect.Left) to Min(LWithToolbar.Width - 1, LGeometry.ToolbarRect.Right - 1) do
        if LWithToolbar.Canvas.Pixels[X, Y] <> LWithoutToolbar.Canvas.Pixels[X, Y] then begin
          if (Y < LGeometry.ContentRect.Top) or (Y >= LGeometry.ContentRect.Bottom) then Inc(Result)
          else Inc(LInsideChanges);
        end;
    if LInsideChanges < 10 then
      raise Exception.Create('Footer paint capture did not include actual toolbar icons');
  finally
    AToolbar.Visible := LVisible;
    LWithToolbar.Free;
    LWithoutToolbar.Free;
  end;
end;

end.
