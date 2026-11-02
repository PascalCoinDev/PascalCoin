unit UWalletKeyNotificationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises the actual wallet key notification and native receiver lifetime.
  No key, wallet file, account balance or transaction is changed. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure CheckWalletKeyNotifications(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Classes, SysUtils, SyncObjs, Forms, UWallet, UCommon, UCoreObjects,
  UFRMAccountExplorer, UCellRenderers;

type
  TWalletKeyProducer = class(TThread)
  private
    FKeys: TWalletKeys;
  protected
    procedure Execute; override;
  public
    Completed: TEvent;
    ErrorText: String;
    constructor Create(AKeys: TWalletKeys);
    destructor Destroy; override;
  end;

constructor TWalletKeyProducer.Create(AKeys: TWalletKeys);
begin
  inherited Create(True);
  FKeys := AKeys;
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor TWalletKeyProducer.Destroy;
begin
  inherited Destroy;
  Completed.Free;
end;

procedure TWalletKeyProducer.Execute;
begin
  try
    try
      FKeys.OnChanged.Invoke(FKeys);
    except
      on E: Exception do
        ErrorText := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then
    raise Exception.Create(AMessage);
end;

procedure Pump;
begin
  CheckSynchronize(0);
  Application.ProcessMessages;
  Sleep(1);
end;

procedure NotifyFromWorker;
var
  LWorker: TWalletKeyProducer;
begin
  LWorker := TWalletKeyProducer.Create(TWallet.Keys);
  try
    LWorker.Start;
    // Do not pump: a synchronous UI wait or direct control update must be
    // observable before any posted notification is permitted to run.
    Require(LWorker.Completed.WaitFor(1000) = wrSignaled,
      'Wallet key notification blocked its background producer');
    Require(LWorker.ErrorText = '', 'Wallet key producer failed: ' + LWorker.ErrorText);
  finally
    LWorker.WaitFor;
    LWorker.Free;
  end;
end;

procedure CheckWalletKeyNotifications(AMain: TMockMainForm; const ALogPath: String);
const
  CT_Sentinel = 'key-notification must wait for the UI';
var
  LExplorer: TFRMAccountExplorer;
  LRetiringWallet: TMockWallet;
  LUntil: UInt64;
  LLog: TStringList;
begin
  Require(TCellRenderers.OperationShortHash('12345678') = '12345678',
    'An eight-character operation hash must remain unchanged');
  Require(TCellRenderers.OperationShortHash('123456789ABC') = '1234...9ABC',
    'Short operation hashes must retain their actual final four characters');
  LExplorer := TFRMAccountExplorer.Create(nil);
  LRetiringWallet := nil;
  LLog := TStringList.Create;
  try
    LExplorer.Show;
    AMain.MockWallet.HandleNeeded;
    LExplorer.HandleNeeded;
    AMain.MockWallet.cbAccounts.Items.Add(CT_Sentinel);
    LExplorer.cbMyPrivateKeys.Items.Add(CT_Sentinel);
    NotifyFromWorker;
    Require(AMain.MockWallet.cbAccounts.Items.IndexOf(CT_Sentinel) >= 0,
      'Background wallet notification mutated the main key combo before UI delivery');
    Require(LExplorer.cbMyPrivateKeys.Items.IndexOf(CT_Sentinel) >= 0,
      'Background wallet notification mutated the explorer key combo before UI delivery');
    LUntil := GetTickCount64 + 3000;
    repeat
      Pump;
    until ((AMain.MockWallet.cbAccounts.Items.IndexOf(CT_Sentinel) < 0) and
      (LExplorer.cbMyPrivateKeys.Items.IndexOf(CT_Sentinel) < 0)) or
      (GetTickCount64 >= LUntil);
    Require(AMain.MockWallet.cbAccounts.Items.IndexOf(CT_Sentinel) < 0,
      'Main wallet did not refresh on its posted key notification');
    Require(LExplorer.cbMyPrivateKeys.Items.IndexOf(CT_Sentinel) < 0,
      'Accounts Explorer did not refresh on its posted key notification');
    LLog.Add('PASS background key notification returns without UI pumping; both real key combos refresh only on UI delivery');

    // Queue another delivery, then destroy both types of receiver without
    // pumping. The native window lifetime must discard those pending messages.
    LRetiringWallet := TMockWallet.CreateForModel(nil, AMain.Model);
    LRetiringWallet.HandleNeeded;
    NotifyFromWorker;
    FreeAndNil(LExplorer);
    FreeAndNil(LRetiringWallet);
    LUntil := GetTickCount64 + 200;
    repeat
      Pump;
    until GetTickCount64 >= LUntil;
    LLog.Add('PASS pending background key messages are safe when wallet and explorer receivers are destroyed before delivery');
    LLog.SaveToFile(ALogPath);
  finally
    LRetiringWallet.Free;
    LExplorer.Free;
    LLog.Free;
  end;
end;

end.
