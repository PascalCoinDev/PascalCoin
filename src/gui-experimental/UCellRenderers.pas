unit UCellRenderers;

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

{ Formats account and operation values for visual-grid cells, including
  amounts, timestamps, payloads and operation hashes. }

{ Public types in this unit

  Type           | Purpose
  ---------------+-------------------------------------------------------------------------
  TCellRenderers | Formats and paints wallet account and operation cells.
}

interface

uses
   Classes, Types, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
   UVisualGrid, variants;

type

  { TCellRenderers }

  TCellRenderers = class
    // Data Sanitizers
    class function UnixTimeSanitizer(const CellData, RowData: Variant) : Variant;
    class function OperationTypeSanitizer(const CellData, RowData: Variant) : Variant;

    // Cell Renderers
    class procedure OperationTime(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure PASC_CheckAllBalance(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure PASC_CheckPendingBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure PASC(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure Payload(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure OPHASH(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);

    // General purpose
    class function OperationShortHash(const AOpHash : String) : String;
    class function OperationShortText(const OpType, OpSubType : Cardinal) : String;
    class function AccountKeyShortText(const AText : String) : String;
  end;

implementation

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  SysUtils, DateUtils, UCommon, UCommon.Data, UAccounts, UConst, UCoreUtils;

const
  CT_PASCBALANCE_POS_COL = clGreen;
  CT_PASCBALANCE_NEU_COL = clDefault;
  CT_PASCBALANCE_NEG_COL = clRed;
  CT_PASCBALANCE_0CONF_COL = clBlue;


procedure DrawRendererText(ACanvas: TCanvas; ARect: TRect; const AText: String; AAlignment: TAlignment);
{$IFNDEF FPC}
var LFlags: Cardinal; LText: String;
{$ENDIF}
begin
  {$IFDEF FPC}
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, AText, ACanvas.TextStyle);
  {$ELSE}
  LFlags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or DT_END_ELLIPSIS;
  case AAlignment of
    taRightJustify: LFlags := LFlags or DT_RIGHT;
    taCenter: LFlags := LFlags or DT_CENTER;
  end;
  LText := String(AText);
  DrawText(ACanvas.Handle, PChar(LText), Length(LText), ARect, LFlags);
  {$ENDIF}
end;
{ TCellRenderers }

class function TCellRenderers.UnixTimeSanitizer(const CellData, RowData: Variant) : Variant;
var uxTime : UInt64;
begin
  if NOT VarIsNumeric(CellData) then exit;
  uxTime := CellData;
  Result := FormatDateTime('yyy-mm-dd hh:nn:ss', UnixToDateTime(uxtime));
end;

class function TCellRenderers.OperationTypeSanitizer(const CellData, RowData: Variant) : Variant;
var LData : TDataRowData absolute RowData;
  LType, LSubType : Cardinal;
begin
 LType := LData.Data['Type'];
 LSubType := LData.Data['SubType'];
 Result := OperationShortText(LType, LSubType);
end;


class procedure TCellRenderers.OperationTime (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : UInt64;
  LRowData : TDataRowData;
  LStr : String;
begin
  if NOT VarIsNumeric(CellData) then exit;
  LValue := CellData;
  if LValue > 0 then
    LStr := FormatDateTime('yyy-mm-dd hh:nn:ss', UnixToDateTime(LValue))
  else begin
    LStr := 'PENDING';
    Canvas.Font.Color := CT_PASCBALANCE_0CONF_COL;
  end;
  DrawRendererText(Canvas, Rect, LStr, taLeftJustify);
  Handled := true;
end;

class procedure TCellRenderers.PASC_CheckAllBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LRowData : TDataRowData;
  LStr : String;
  LAllBalance : boolean;
begin
  LRowData := TDataRowData(RowData);
  if LRowData.HasData('AllBalance') AND TVariantTool.TryParseBool(LRowData['AllBalance'], LAllBalance) AND LAllBalance then begin
    Canvas.Font.Style := [fsBold];
    DrawRendererText(Canvas, Rect, 'ALL BALANCE', taRightJustify);
    Handled := true;
    exit;
  end;
  PASC(Sender, ACol, ARow, Canvas, Rect, State, CellData, RowData, Handled);
end;

class procedure TCellRenderers.PASC_CheckPendingBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LRowData : TDataRowData;
  LStr : String;
  LPending : boolean;
begin
  if NOT TVariantTool.IsNumeric(CellData) then
    exit;
  LValue := CellData;
  LRowData := TDataRowData(RowData);
  // Paint only the fetched row. A live node query here can wait behind disk
  // restore or a snapshot worker while Windows is dispatching WM_PAINT.
  LPending := false;
  // The explicit account snapshot flag takes precedence over UnixTime.
  if not (LRowData.HasData('IsPending') and
    TVariantTool.TryParseBool(LRowData['IsPending'], LPending)) then
    if LRowData.HasData('UnixTime') then
      LPending := LRowData['UnixTime'] = 0;

  if LPending then begin
    Canvas.Font.Color := CT_PASCBALANCE_0CONF_COL;
    LStr := '(' + TAccountComp.FormatMoney(LValue) + ')';
    DrawRendererText(Canvas, Rect, LStr, taRightJustify);
    Handled := true;
  end else PASC(Sender, ACol, ARow, Canvas, Rect, State, CellData, RowData, Handled);
end;

class procedure TCellRenderers.PASC (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LRowData : TDataRowData;
  LStr : String;
begin
  if NOT TVariantTool.IsNumeric(CellData) then
    exit;
  LValue := CellData;
  LRowData := TDataRowData(RowData);
  Canvas.Font.Color := IIF (LValue < 0, CT_PASCBALANCE_NEG_COL, IIF(LValue > 0, CT_PASCBALANCE_POS_COL, CT_PASCBALANCE_NEU_COL));
  LStr := TAccountComp.FormatMoney(LValue);
  DrawRendererText(Canvas, Rect, LStr, taRightJustify);
  Handled := true;
end;

class procedure TCellRenderers.Payload (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  {    s := opr.PrintablePayload;
  Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]); }

  Handled := False;
end;

class procedure TCellRenderers.OPHASH (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LRowData : TDataRowData;
  LStr : String;
begin
  if NOT VarIsStr(CellData) then
    exit;
  LStr := CellData;
  LRowData := TDataRowData(RowData);
 // Canvas.Font.Size := Canvas.Font.Size - 1;
  Canvas.Font.Color := clGrayText;
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  LStr := OperationShortHash(LStr);
  DrawRendererText(Canvas, Rect, LStr, taLeftJustify);
  Handled := true;
end;

{ KEY }
{   else if ABindingName = 'Key' then begin
     index := TWallet.Keys.AccountsKeyList.IndexOfAccountKey(AItem.accountInfo.accountKey);
     if index>=0 then
        Result := TWallet.Keys[index].Name
     else
         Result := TAccountComp.AccountPublicKeyExport(AItem.accountInfo.accountKey); }


class function TCellRenderers.OperationShortHash(const AOpHash : String) : String;
var
  len : SizeInt;
begin
 len := Length(AOpHash);
  if len > 8 then
    // Copy uses one-based offsets: retain the actual final four characters.
    result := Copy(AOpHash, 1, 4) + '...' + Copy(AOpHash, len - 3, 4)
  else
    result := AOpHash;
end;

class function TCellRenderers.OperationShortText(const OpType, OpSubType : Cardinal) : String;
begin
  Result := TCoreTool.GetOperationShortText(OpType, OpSubType);
end;

class function TCellRenderers.AccountKeyShortText(const AText : String) : String;
begin
 If Length(AText) > 20 then
   Result := Copy(AText, 1, 17) + '...'
 else
   Result := AText;
end;


end.
