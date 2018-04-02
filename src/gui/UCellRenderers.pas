unit UCellRenderers;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
   Classes, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
   UVisualGrid, variants;

type

  { TCellRenderers }

  TCellRenderers = class
    class procedure AccountNo(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure AccountName(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure Date_YYYYMMDDHHMMSS(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure PASCTransfer(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure PASCBalance(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure Payload(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure OPHASH(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
    class procedure SmallText(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
  end;

implementation

uses
  SysUtils, UCommon, UAccounts;

const
  CT_PASCBALANCE_POS_COL = clGreen;
  CT_PASCBALANCE_NEU_COL = clGrayText;
  CT_PASCBALANCE_NEG_COL = clRed;

{ TCellRenderers }

class procedure TCellRenderers.AccountNo (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
 LTextStyle: TTextStyle;
begin
 LTextStyle := Canvas.TextStyle;
 LTextStyle.Alignment:=taLeftJustify;
 Canvas.TextStyle:=LTextStyle;
 Canvas.TextRect(Rect, Rect.Left, Rect.Top, CellData, LTextStyle);
 Handled := true;
end;

class procedure TCellRenderers.AccountName (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
 LTextStyle: TTextStyle;
begin
 LTextStyle := Canvas.TextStyle;
 LTextStyle.Alignment:=taLeftJustify;
 Canvas.TextStyle:=LTextStyle;
 Canvas.Font.Bold := true;
 Canvas.TextRect(Rect, Rect.Left, Rect.Top, CellData, LTextStyle);
 Handled := true;
end;

class procedure TCellRenderers.Date_YYYYMMDDHHMMSS (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  Handled := False;
end;

class procedure TCellRenderers.PASCTransfer (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  Handled := False;
end;

class procedure TCellRenderers.PASCBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LNum : Int64;
  LTextStyle: TTextStyle;
begin
  if NOT VarIsNumeric(CellData) then
    exit;
  LNum := CellData;
  Canvas.Font.Color:= IIF (LNum < 0, CT_PASCBALANCE_NEG_COL, IIF(LNum > 0, CT_PASCBALANCE_POS_COL, CT_PASCBALANCE_NEU_COL));
  Canvas.Font.Style:=[fsBold];
  LTextStyle := Canvas.TextStyle;
  LTextStyle.Alignment:=taRightJustify;
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, TAccountComp.FormatMoney(LNum), LTextStyle);
  Handled := true;
end;

class procedure TCellRenderers.Payload (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  Handled := False;
end;

class procedure TCellRenderers.OPHASH (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  Handled := False;
end;

class procedure TCellRenderers.SmallText (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
 Handled := False;
end;

end.

