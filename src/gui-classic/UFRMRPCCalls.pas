unit UFRMRPCCalls;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016-2018 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

{$I ../config.inc}

uses
  {$IFnDEF FPC}
    Windows, AppEvnts,
  {$ELSE}
    LCLIntf, LCLType, LMessages, FileUtil,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ValEdit, ComCtrls, Buttons, UJSONFunctions, blcksock, httpsend,
  UConst,
  UAccounts, Grids;

type

  TInfoType = (info,call,response,error);

  { TFRMRPCCalls }

  TFRMRPCCalls = class(TForm)
    bbSendCommand: TBitBtn;
    ebMethod: TEdit;
    ebServerURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblLastCallResultState: TLabel;
    lblTopCaption: TLabel;
    mCalls: TMemo;
    mLastResult: TMemo;
    mJSONParams: TMemo;
    pageControl: TPageControl;
    pnlLastCall: TPanel;
    pnlInfo: TPanel;
    pnlInfoBottom: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tsJSONParams: TTabSheet;
    tsKeyNames: TTabSheet;
    vlKeyParams: TValueListEditor;
    procedure bbSendCommandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServerURL: String;
    procedure SetServerURL(AValue: String);
    procedure ShowCallInfo(infoType : TInfoType; value : String);
    procedure DoSendJSON(json : TPCJSONObject);
    function DecodeJSONResult(jsonString : String; out jsonResult : TPCJSONObject) : Boolean;
    Procedure UpdateLastCallResult(jsonResult : TPCJSONObject);
  public
    Property ServerURL : String read FServerURL write SetServerURL;
  end;

Const
  CT_TIntoType_Str : Array[TInfoType] of String = ('info','call','response','error');

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TFRMRPCCalls }

procedure TFRMRPCCalls.FormCreate(Sender: TObject);
begin
  FServerURL:='127.0.0.1:'+IntToStr(CT_JSONRPC_Port);
  ebMethod.Text:='';
  mJSONParams.Clear;
  mCalls.Clear;
  {$IFDEF FPC}vlKeyParams.Clear;{$ENDIF}
  vlKeyParams.TitleCaptions.Text:='Key'+#10+'Value';
  ebServerURL.Text:=FServerURL;
  pageControl.ActivePage:=tsKeyNames;
  lblLastCallResultState.Caption:='';
  mLastResult.Clear;
end;

procedure TFRMRPCCalls.SetServerURL(AValue: String);
begin
  if FServerURL=AValue then Exit;
  FServerURL:=AValue;
  ebServerURL.Text := Trim(AValue);
end;

procedure TFRMRPCCalls.bbSendCommandClick(Sender: TObject);
Var obj : TPCJSONObject;
  iRow : Integer;
  decJSON : TPCJSONData;
begin
  obj := TPCJSONObject.Create;
  try
    obj.GetAsVariant('jsonrpc').Value:='2.0';
    obj.GetAsVariant('id').Value:=100;
    obj.GetAsVariant('method').Value:=ebMethod.Text;
    //
    If (pageControl.ActivePage = tsKeyNames) then begin
      for iRow := 1 to vlKeyParams.RowCount-1 do begin
        if (vlKeyParams.Keys[iRow]<>'') then begin
          obj.GetAsObject('params').GetAsVariant( vlKeyParams.Keys[iRow] ).Value:= vlKeyParams.Values[vlKeyParams.Keys[iRow]];
        end;
      end;
    end else begin
      if (mJSONParams.Text<>'') then begin
        decJSON := TPCJSONData.ParseJSONValue(mJSONParams.Text);
        try
          if (decJSON is TPCJSONObject) then begin
            obj.GetAsObject('params').Assign(decJSON);
          end else if (decJSON is TPCJSONArray) then begin
            obj.GetAsArray('params').Assign(decJSON);
          end else if (decJSON is TPCJSONVariantValue) then begin
            obj.GetAsVariant('params').Assign(decJSON);
          end else begin
            Raise Exception.Create('Invalid JSON value: '+mJSONParams.Text);
          end;
        finally
          decJSON.Free;
        end;
      end;
    end;
    //
    DoSendJSON(obj);
  finally
    obj.Free;
  end;
end;

procedure TFRMRPCCalls.ShowCallInfo(infoType: TInfoType; value: String);
begin
  mCalls.Lines.Add(Format('%s [%s] %s',[FormatDateTime('hh:nn:ss.zzz',Now),CT_TIntoType_Str[infoType],value]));
end;

procedure TFRMRPCCalls.DoSendJSON(json: TPCJSONObject);
  function DoHttpPostBinary(const URL: string; const Data: TStream): Boolean;
  var
    HTTP: THTTPSend;
  begin
    HTTP := THTTPSend.Create;
    try
      HTTP.Protocol:='1.1';
      HTTP.Document.CopyFrom(Data, 0);
      HTTP.MimeType := 'Application/octet-stream';
      Result := HTTP.HTTPMethod('POST', URL);
      Data.Size := 0;
      if Result then
      begin
        Data.Seek(0, soFromBeginning);
        Data.CopyFrom(HTTP.Document, 0);
      end;
    finally
      HTTP.Free;
    end;
  end;
Var
  ms : TMemoryStream;
  s : String;
  jsonResult : TPCJSONObject;
begin
  ShowCallInfo(call,json.ToJSON(false));
  ms := TMemoryStream.Create;
  Try
    ms.Size := 0;
    TStreamOp.LoadStreamFromRaw(ms,TEncoding.ANSI.GetBytes(json.ToJSON(False)));
    If Not DoHttpPostBinary(ebServerURL.Text,ms) then ShowCallInfo(error,'no valid response from '+ebServerURL.Text);
    s := TEncoding.ANSI.GetString(TStreamOp.SaveStreamToRaw(ms));
    ShowCallInfo(response,s);
    if DecodeJSONResult(s,jsonResult) then begin
      Try
        UpdateLastCallResult(jsonResult);
      finally
        jsonResult.Free;
      end;
    end else begin
      ShowCallInfo(error,'Invalid JSON response');
      UpdateLastCallResult(Nil);
    end;
  finally
    ms.Free;
  end;
end;

function TFRMRPCCalls.DecodeJSONResult(jsonString: String; out jsonResult: TPCJSONObject): Boolean;
Var jsd : TPCJSONData;
begin
  jsonResult := Nil;
  Result := false;
  If (jsonString='') then Exit;
  jsd := TPCJSONData.ParseJSONValue(jsonString);
  Try
    if jsd is TPCJSONObject then jsonResult := jsd as TPCJSONObject;
  finally
    if not Assigned(jsonResult) then begin
      jsd.Free;
      Result := False;
    end else Result := True;
  end;
end;

procedure TFRMRPCCalls.UpdateLastCallResult(jsonResult: TPCJSONObject);
begin
  If Assigned(jsonResult) then begin
    if jsonResult.IndexOfName('result')>=0 then begin
      // Has a valid result:
      lblLastCallResultState.Font.Color:=clGreen;
      lblLastCallResultState.Caption:=Format('%s OK',[FormatDateTime('hh:nn:ss.zzz',Now)]);
      mLastResult.Lines.Text:=jsonResult.ToJSON(False);
    end else begin
      // Is an error
      lblLastCallResultState.Font.Color:=clRed;
      lblLastCallResultState.Caption:=Format('%s ERROR %d',[FormatDateTime('hh:nn:ss.zzz',Now),jsonResult.GetAsObject('error').AsInteger('code',0)]);
      mLastResult.Lines.Text:=jsonResult.ToJSON(False);
    end;
  end else begin
    lblLastCallResultState.Caption:='';
    mLastResult.Clear;
  end;
end;

end.

