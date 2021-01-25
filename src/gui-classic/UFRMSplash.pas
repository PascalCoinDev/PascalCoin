unit UFRMSplash;

{ Copyright (c) 2021 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ../config.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UAccounts, Buttons, ActnList,
  ExtCtrls, ComCtrls,
  UNode, UWallet, UNetProtocol, UPCDataTypes, UThread,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TSplashProcess = record
    processName : String;
    allowProgress : Boolean;
    allowCancel : Boolean;
    maxValue : Integer;
    currentValue : Integer;
  end;

  ESplashCancel = Class(EAbort);

  TFRMSplash = class(TForm)
    imgSplash: TImage;
    lblProcess: TLabel;
    pgProgress1: TProgressBar;
    bbCancel: TBitBtn;
    lblProcessActive: TLabel;
    procedure bbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FSplashProcesses : TList<TSplashProcess>;
    FDoCancel : Boolean;
  public
    { Public declarations }
    class procedure SplashStart(const AOwnerForm : TForm; const AProcess : String; AAllowProgress, AAllowCancel : Boolean; AMaxValue : Integer; ACurrentValue : Integer);
    class procedure SplashUpdate(const AProcess : String; ACurrentValue : Integer);
    class procedure SplashFinalize;
    class function SplashCount : Integer;
    procedure UpdateInfo;
  end;

implementation

{$R *.dfm}

var _FRM : TFRMSplash;

{ TFRMSplash }

procedure TFRMSplash.bbCancelClick(Sender: TObject);
begin
  FDoCancel := True;
end;

procedure TFRMSplash.FormCreate(Sender: TObject);
begin
  FDoCancel := False;
  lblProcess.Caption := '';
  lblProcessActive.Caption := '';
  pgProgress1.Visible := False;
  bbCancel.Enabled := False;
  bbCancel.Visible := False;
  bbCancel.Cancel := True;
  FSplashProcesses := TList<TSplashProcess>.Create;
end;

procedure TFRMSplash.FormDestroy(Sender: TObject);
begin
  FSplashProcesses.Free;
end;

class function TFRMSplash.SplashCount: Integer;
begin
  if Assigned(_FRM) then Result := _FRM.FSplashProcesses.Count
  else Result := 0;
end;

class procedure TFRMSplash.SplashFinalize;
begin
  if Not Assigned(_FRM) then raise Exception.Create('No splash!');
  if _FRM.FSplashProcesses.Count<=0 then raise Exception.Create('No splash count!');
  _FRM.FSplashProcesses.Delete(_FRM.FSplashProcesses.Count-1);
  if _FRM.FSplashProcesses.Count>0 then begin
    _FRM.UpdateInfo;
  end else begin
    FreeAndNil(_FRM);
  end;
end;

class procedure TFRMSplash.SplashStart(const AOwnerForm : TForm; const AProcess: String; AAllowProgress,
  AAllowCancel: Boolean; AMaxValue, ACurrentValue: Integer);
var Lprocess : TSplashProcess;
begin
  if Not Assigned(_FRM) then begin
    _FRM := TFRMSplash.Create(AOwnerForm);
    _FRM.bbCancel.Enabled := AAllowCancel;
    _FRM.bbCancel.Visible := AAllowCancel;
    _FRM.lblProcess.Caption := AProcess;
  end;
  Lprocess.processName := AProcess;
  Lprocess.allowProgress := AAllowProgress;
  Lprocess.allowCancel := AAllowCancel;
  Lprocess.maxValue := AMaxValue;
  Lprocess.currentValue := ACurrentValue;
  _FRM.FSplashProcesses.Add(Lprocess);
  _FRM.UpdateInfo;
  _FRM.Show;
  Application.ProcessMessages;
end;

class procedure TFRMSplash.SplashUpdate(const AProcess: String; ACurrentValue: Integer);
var Lprocess : TSplashProcess;
begin
  if Not Assigned(_FRM) then raise Exception.Create('No splash!');
  if _FRM.FSplashProcesses.Count<=0 then raise Exception.Create('No splash count!');
  Lprocess := _FRM.FSplashProcesses.Items[_FRM.FSplashProcesses.Count-1];
  if AProcess<>'' then Lprocess.processName := AProcess;
  Lprocess.currentValue := ACurrentValue;
  _FRM.FSplashProcesses.Items[_FRM.FSplashProcesses.Count-1] := Lprocess;
  _FRM.UpdateInfo;
end;

procedure TFRMSplash.UpdateInfo;
var Lprocess : TSplashProcess;
begin
  if FSplashProcesses.Count>0 then begin
    Lprocess := _FRM.FSplashProcesses.Items[_FRM.FSplashProcesses.Count-1];

  end else begin
    Lprocess.processName := '';
    Lprocess.allowProgress := false;
    Lprocess.allowCancel := false;
    Lprocess.maxValue := 100;
    Lprocess.currentValue := 0;
  end;
  lblProcessActive.Caption := Lprocess.processName;
  pgProgress1.Visible := Lprocess.allowProgress;
  pgProgress1.Min := 0;
  if Lprocess.maxValue>0 then
    pgProgress1.Max := Lprocess.maxValue;
  if (Lprocess.currentValue>=0) and (Lprocess.currentValue<=Lprocess.maxValue) then begin
    pgProgress1.Position := Lprocess.maxValue;
  end;
  Application.ProcessMessages;
  if FDoCancel then begin
    raise ESplashCancel.Create('Canceled process: '+Lprocess.processName);
  end;
end;

initialization
  _FRM := Nil;
finalization
  FreeAndNil(_FRM);
end.
