unit UFRMNodesIp;

{$MODE Delphi}

interface

{$I ./../PascalCoin/config.inc}

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  UCommon.UI;

type
  TFRMNodesIp = class(TApplicationForm)
    memoNodesIp: TMemo;
    Label1: TLabel;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    cbTryOnlyWithThisServers: TCheckBox;
    procedure bbOkClick(Sender: TObject);
    procedure cbTryOnlyWithThisServersClick(Sender: TObject);
  private
    { Private declarations }
    procedure Refresh;
  protected
    procedure ActivateFirstTime; override;
  public
    { Public declarations }
  end;

implementation

uses
  UUserInterface, USettings, UNetProtocol, UNode, UConst;

{$R *.lfm}

{ TFRMNodesIp }

procedure TFRMNodesIp.ActivateFirstTime;
begin
  Refresh;
end;

procedure TFRMNodesIp.Refresh;
Var
  nsarr : TNodeServerAddressArray;
  i : Integer;
  ips : AnsiString;
  aux : AnsiString;
begin
  memoNodesIp.Clear;
  setlength(nsarr,0);
  ips := TSettings.TryConnectOnlyWithThisFixedServers;
  if ips <> '' then begin
    cbTryOnlyWithThisServers.Checked := true;
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
  end else begin
    cbTryOnlyWithThisServers.Checked := false;
    nsarr := TNetData.NetData.GetValidNodeServers(false,0);
  end;
  for i := low(nsarr) to high(nsarr) do begin
    aux := nsarr[i].ip;
    if nsarr[i].port>0 then aux := aux +':'+ InttoStr(nsarr[i].port);
    memoNodesIp.Lines.Add( aux );
  end;
  if cbTryOnlyWithThisServers.Checked then begin
    cbTryOnlyWithThisServers.Font.Color := clRed;
    cbTryOnlyWithThisServers.Font.Style := [fsBold];
  end else begin
    cbTryOnlyWithThisServers.ParentFont := true;
  end;
  setlength(nsarr,0);
end;

procedure TFRMNodesIp.bbOkClick(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  ips : AnsiString;
begin
  TNode.DecodeIpStringToNodeServerAddressArray(memoNodesIp.Lines.Text,nsarr);
  if (length(nsarr)=0) And (cbTryOnlyWithThisServers.Checked) then begin
    raise Exception.Create('No valid IP in list!');
  end;
  // Encode
  ips := TNode.EncodeNodeServerAddressArrayToIpString(nsarr);
  TSettings.PeerCache := ips;
  if cbTryOnlyWithThisServers.Checked then Begin
    TSettings.TryConnectOnlyWithThisFixedServers := ips;
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    Application.MessageBox(PChar('Restart application to take effect'),PChar(Application.Title),MB_OK);
  end else begin
    TSettings.TryConnectOnlyWithThisFixedServers := '';
    setlength(nsarr,0);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
  end;
  setlength(nsarr,0);
  ModalResult := MrOk;
end;

procedure TFRMNodesIp.cbTryOnlyWithThisServersClick(Sender: TObject);
begin
  if cbTryOnlyWithThisServers.Checked then begin
    cbTryOnlyWithThisServers.Font.Color := clRed;
    cbTryOnlyWithThisServers.Font.Style := [fsBold];
  end else begin
    cbTryOnlyWithThisServers.ParentFont := true;
  end;
  if cbTryOnlyWithThisServers.Checked then begin
    Application.MessageBox(PChar('ALERT:'+#10+
      'If "'+cbTryOnlyWithThisServers.Caption+'" is checked '+#10+
      'and no valid server found, you will be alone!')
      ,PChar(Application.Title),MB_OK+MB_ICONWARNING);
  end;
end;

end.
