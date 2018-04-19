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
    class function OperationShortHash(const AOpHash : AnsiString) : AnsiString;
    class function OperationShortText(const OpType, OpSubType : DWord) : AnsiString;
    class function AccountKeyShortText(const AText : AnsiString) : AnsiString;
  end;

implementation

uses
  SysUtils, DateUtils, UCommon, UCommon.Data, UAccounts, UConst;

const
  CT_PASCBALANCE_POS_COL = clGreen;
  CT_PASCBALANCE_NEU_COL = clDefault;
  CT_PASCBALANCE_NEG_COL = clRed;
  CT_PASCBALANCE_0CONF_COL = clBlue;


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
  LType, LSubType : DWord;
begin
 LType := LData.Data['Type'];
 LSubType := LData.Data['SubType'];
 Result := OperationShortText(LType, LSubType);
end;


class procedure TCellRenderers.OperationTime (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : UInt64;
  LTextStyle: TTextStyle;
  LRowData : TDataRowData;
  LStr : AnsiString;
begin
  if NOT VarIsNumeric(CellData) then exit;
  LValue := CellData;
  if LValue > 0 then
    LStr := FormatDateTime('yyy-mm-dd hh:nn:ss', UnixToDateTime(LValue))
  else begin
    LStr := 'PENDING';
    Canvas.Font.Color := CT_PASCBALANCE_0CONF_COL;
  end;
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, LStr, Canvas.TextStyle);
  Handled := true;
end;

class procedure TCellRenderers.PASC_CheckAllBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LTextStyle: TTextStyle;
  LRowData : TDataRowData;
  LStr : AnsiString;
  LAllBalance : boolean;
begin
  LRowData := TDataRowData(RowData);
  if LRowData.HasData('AllBalance') AND TVariantTool.TryParseBool(LRowData['AllBalance'], LAllBalance) AND LAllBalance then begin
    Canvas.Font.Style := [fsBold];
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, 'ALL BALANCE', Canvas.TextStyle);
    Handled := true;
    exit;
  end else PASC(Sender, ACol, ARow, Canvas, Rect, State, CellData, RowData, Handled);
end;

class procedure TCellRenderers.PASC_CheckPendingBalance (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LTextStyle: TTextStyle;
  LRowData : TDataRowData;
  LStr : AnsiString;
  LAllBalance : boolean;
begin
  if NOT TVariantTool.IsNumeric(CellData) then
    exit;
  LValue := CellData;
  LRowData := TDataRowData(RowData);
  if LRowData.HasData('UnixTime')  AND (LRowData['UnixTime'] = 0) then begin
    Canvas.Font.Color := CT_PASCBALANCE_0CONF_COL;
    LStr := '(' + TAccountComp.FormatMoney(LValue) + ')';
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, LStr, Canvas.TextStyle);
    Handled := true;
    exit;
  end else PASC(Sender, ACol, ARow, Canvas, Rect, State, CellData, RowData, Handled);
end;

class procedure TCellRenderers.PASC (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LValue : Int64;
  LTextStyle: TTextStyle;
  LRowData : TDataRowData;
  LStr : AnsiString;
begin
  if NOT TVariantTool.IsNumeric(CellData) then
    exit;
  LValue := CellData;
  LRowData := TDataRowData(RowData);
  if LRowData.HasData('UnixTime')  AND (LRowData['UnixTime'] = 0) then begin
    Canvas.Font.Color := CT_PASCBALANCE_0CONF_COL;
    LStr := '('+TAccountComp.FormatMoney(LValue)+')';
  end else begin
    Canvas.Font.Color := IIF (LValue < 0, CT_PASCBALANCE_NEG_COL, IIF(LValue > 0, CT_PASCBALANCE_POS_COL, CT_PASCBALANCE_NEU_COL));
    LStr := TAccountComp.FormatMoney(LValue);
  end;
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, LStr, Canvas.TextStyle);
  Handled := true;
end;

class procedure TCellRenderers.Payload (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  {    s := opr.PrintablePayload;
  Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]); }

  Handled := False;
end;

class procedure TCellRenderers.OPHASH (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
begin
  Handled := False;
end;


{ KEY }
{   else if ABindingName = 'Key' then begin
     index := TWallet.Keys.AccountsKeyList.IndexOfAccountKey(AItem.accountInfo.accountKey);
     if index>=0 then
        Result := TWallet.Keys[index].Name
     else
         Result := TAccountComp.AccountPublicKeyExport(AItem.accountInfo.accountKey); }


class function TCellRenderers.OperationShortHash(const AOpHash : AnsiString) : AnsiString;
var
  len : SizeInt;
begin
 len := Length(AOpHash);
  if len > 8 then
    result := AOpHash.Substring(0, 4) + '...' + AOpHash.Substring(len - 4 - 1, 4)
  else
    result := AOpHash;
end;

class function TCellRenderers.OperationShortText(const OpType, OpSubType : DWord) : AnsiString;
begin
  case OpType of
    CT_PseudoOp_Reward: case OpSubType of
      0, CT_PseudoOpSubtype_Miner : result := 'Miner Reward';
      CT_PseudoOpSubtype_Developer : result := 'Developer Reward';
      else result := 'Unknown';
    end;
    CT_Op_Transaction: case OpSubType of
      CT_OpSubtype_TransactionSender: Result := 'Send';
      CT_OpSubtype_TransactionReceiver: Result := 'Receive';
      CT_OpSubtype_BuyTransactionBuyer: result := 'Buy Account Direct';
      CT_OpSubtype_BuyTransactionTarget: result := 'Purchased Account Direct';
      CT_OpSubtype_BuyTransactionSeller: result := 'Sold Account Direct';
      else result := 'Unknown';
    end;
    CT_Op_Changekey: Result := 'Change Key (legacy)';
    CT_Op_Recover: Result := 'Recover';
    CT_Op_ListAccountForSale: case OpSubType of
      CT_OpSubtype_ListAccountForPublicSale: result := 'For Sale';
      CT_OpSubtype_ListAccountForPrivateSale: result := 'Exclusive Sale';
      else result := 'Unknown';
    end;
    CT_Op_DelistAccount: result := 'Remove Sale';
    CT_Op_BuyAccount: case OpSubType of
      CT_OpSubtype_BuyAccountBuyer: result := 'Buy Account';
      CT_OpSubtype_BuyAccountTarget: result := 'Purchased Account';
      CT_OpSubtype_BuyAccountSeller: result := 'Sold Account';
      else result := 'Unknown';
    end;
    CT_Op_ChangeKeySigned: result :=  'Change Key';
    CT_Op_ChangeAccountInfo: result := 'Change Info';
    CT_Op_MultiOperation: case OpSubType of
      CT_OpSubtype_MultiOperation_Global: Result := 'Mixed-Transfer';
      CT_OpSubtype_MultiOperation_AccountInfo: Result := 'Mixed-Change';
    end;
    else result := 'Unknown';
  end;
end;

class function TCellRenderers.AccountKeyShortText(const AText : AnsiString) : AnsiString;
begin
 If Length(AText) > 20 then
   Result := AText.SubString(0, 17) + '...'
 else
   Result := AText;
end;


end.
