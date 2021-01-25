unit UFRMAskForAccount;

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
  {$IFNDEF FPC}System.Actions, System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UNode, UWallet, UNetProtocol, UPCDataTypes, UThread, UBaseTypes,
  UBlockChain;

type

  { TFRMAskForAccount }

  TFRMAskForAccount = class(TForm)
    ActionList: TActionList;
    actExecute: TAction;
    pnlBottom: TPanel;
    bbExecute: TBitBtn;
    bbCancel: TBitBtn;
    reDescription: TMemo;
    lblDoYouWantToSendAMessage: TLabel;
    ebMessageToSend: TEdit;
    imgAskForAccount: TImage;
    lblTitle: TLabel;
    procedure actExecuteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FNode : TNode;
    FNetData : TNetData;
    FWalletKeys : TWalletKeysExt;
    FNewPublicKey : TAccountKey;
  public
    { Public declarations }
    class function AskForAccount(AOwnerForm : TComponent; ANode : TNode; ANetData : TNetData; AWalletKeys : TWalletKeysExt; const ANewPubliKey : TAccountKey) : Boolean;
  end;


implementation

{$R *.dfm}

uses UFRMSplash, UPCTNetDataExtraMessages,
  UPCAbstractMemAccountKeys,
  UOrderedList;

procedure TFRMAskForAccount.actExecuteExecute(Sender: TObject);
var Ltc, Ltc2 : TTickCount;
  Lnodes, LtotalNodes : Integer;
  Lmempool : TPCOperationsComp;
  LOnsafebox,LOnMempool : Integer;
begin
  // Ask
  LOnSafebox := 0;
  LOnMempool := 0;
  LtotalNodes := 0;

  if (ebMessageToSend.Text='') then begin
    if (Application.MessageBox(PChar('Are you sure to send an EMPTY message to the blockchain?'),
      PChar(Application.Title),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON2)<>IdYes) then Exit;
  end;

  Try
    TFRMSplash.SplashStart(Self,'Ask for PASA',True,True,100,0);
    try
      TFRMSplash.SplashUpdate('Checking current state... '+ebMessageToSend.Text,10);
      FNode.GetAccountsAvailableByPublicKey(FNewPublicKey,LOnsafebox,LOnMempool);
      if (LOnMempool>0) then raise Exception.Create(Format('%d accounts are on the mempool and will be available after next block.'+#10+
         'Your wallet will have at least %d accounts, no need to ask for more!',[LOnMempool,LOnsafebox+LOnMempool]));
      if (LOnSafebox>0) or (LOnMempool>0) then raise Exception.Create(Format('Your wallet has at least %d accounts, no need to ask for more!',[LOnsafebox+LOnMempool]));
      TFRMSplash.SplashUpdate('Sending message to nodes... '+ebMessageToSend.Text,20);
      Ltc := TPlatform.GetTickCount;
      repeat
        TFRMSplash.SplashUpdate(Format('Asking to %d nodes',[FNetData.ConnectionsCountAll]),10);
        Lnodes := TPCTNetDataExtraMessages.AskForFreeAccount(FNewPublicKey,ebMessageToSend.Text);
        inc(LtotalNodes,Lnodes);
        //
        if (LNodes>0) then begin
          Ltc2 := TPlatform.GetTickCount;
          repeat
            TFRMSplash.SplashUpdate(Format('Asked to %d nodes (total %d) waiting for success...',[Lnodes,LtotalNodes]),40);
            sleep(100);
            FNode.GetAccountsAvailableByPublicKey(FNewPublicKey,LOnsafebox,LOnMempool);
          until (Application.Terminated) or (Self.ModalResult<>0) or (LOnsafebox>0) or (LOnMempool>0) or (TPlatform.GetElapsedMilliseconds(Ltc2)>(10000));
        end else sleep(500);
      until (Application.Terminated) or (Self.ModalResult<>0) or (LOnsafebox>0) or (LOnMempool>0)
        or (FNetData.ConnectionsCountAll<=0)
        or (TPlatform.GetElapsedMilliseconds(Ltc)>(60 * 60 * 1000)); // 1 hour
    finally
      TFRMSplash.SplashFinalize;
    end;
    if (LOnsafebox+LOnMempool)=0 then begin
      Application.MessageBox(PChar('No accounts have been detected...'),PChar(Application.Title),MB_ICONERROR+MB_OK);
      ModalResult := MrNone;
    end else begin
      Application.MessageBox(PChar(Format('Congratulations!'+#10+#10+'%d new accounts are on the way!'+#10+#10+'Total accounts will be %d',[LOnMempool,LOnsafebox+LOnMempool])),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
      ModalResult := MrOk;
    end;
  Except
    on E:Exception do begin
      if (E is ESplashCancel) and ((LOnsafebox+LOnMempool)>0) then begin
        ModalResult := MrOk;
      end else Raise;
    end;
  End;
end;

class function TFRMAskForAccount.AskForAccount(AOwnerForm: TComponent;
  ANode: TNode; ANetData: TNetData; AWalletKeys: TWalletKeysExt; const ANewPubliKey : TAccountKey): Boolean;
var FRM : TFRMAskForAccount;
begin
  TPCTNetDataExtraMessages.InitNetDataExtraMessages(ANode,ANetData,AWalletKeys);
  FRM := TFRMAskForAccount.Create(AOwnerForm);
  Try
    FRM.FNode := ANode;
    FRM.FNetData := ANetData;
    FRM.FWalletKeys := AWalletKeys;
    FRM.FNewPublicKey := ANewPubliKey;
    Result := FRM.ShowModal=MrOk;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMAskForAccount.FormCreate(Sender: TObject);
begin
  bbCancel.Cancel := True;
  bbCancel.ModalResult := MrCancel;
end;

end.
