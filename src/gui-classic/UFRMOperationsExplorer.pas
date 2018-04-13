unit UFRMOperationsExplorer;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ActnList, UAccounts, UBlockChain, UNode, UCrypto, UBaseTypes,
  UFileStorage, UWallet, UConst, UTxMultiOperation, UOpTransaction, URPC, UJSONFunctions;


type

  { TFRMOperationsExplorer }

  TFRMOperationsExplorer = class(TForm)
    ActExecuteOperation: TAction;
    ActAddTxToMultioperation: TAction;
    ActAddTx: TAction;
    ActAddOpChange: TAction;
    ActSign: TAction;
    ActRemove: TAction;
    ActSignMultioperation: TAction;
    ActRemoveAccountFromMultioperation: TAction;
    AddOpChangeToMultioperation: TAction;
    ActionList: TActionList;
    bbSign: TButton;
    bbAddTx: TButton;
    bbAddOpChange: TButton;
    bbExecuteOperation: TButton;
    bbRemove: TButton;
    bbRandom: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblTopCaption: TLabel;
    lbOperations: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MiRPCCalls: TMenuItem;
    MiActions: TMenuItem;
    MiDivider1: TMenuItem;
    MiImportOperationsFromTxt: TMenuItem;
    miLoadFromBlockchain: TMenuItem;
    MiLoadOperationsFromBlockchainFile: TMenuItem;
    MiTools: TMenuItem;
    mMenu: TMainMenu;
    mOperationInfo: TMemo;
    mOperationExport: TMemo;
    pnlInfoOptions: TPanel;
    pnlInfo: TPanel;
    pnlExport: TPanel;
    pnlOpInfo: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    splitter: TSplitter;
    procedure ActAddTxToMultioperationExecute(Sender: TObject);
    procedure ActAddTxToMultioperationUpdate(Sender: TObject);
    procedure ActExecuteOperationExecute(Sender: TObject);
    procedure ActExecuteOperationUpdate(Sender: TObject);
    procedure ActRemoveAccountFromMultioperationExecute(Sender: TObject);
    procedure ActRemoveAccountFromMultioperationUpdate(Sender: TObject);
    procedure ActSignMultioperationExecute(Sender: TObject);
    procedure ActSignMultioperationUpdate(Sender: TObject);
    procedure AddOpChangeToMultioperationExecute(Sender: TObject);
    procedure AddOpChangeToMultioperationUpdate(Sender: TObject);
    procedure bbAddOpChangeClick(Sender: TObject);
    procedure bbAddTxClick(Sender: TObject);
    procedure bbRandomClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure bbSignClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbOperationsClick(Sender: TObject);
    procedure MiImportOperationsFromTxtClick(Sender: TObject);
    procedure miLoadFromBlockchainClick(Sender: TObject);
    procedure MiLoadOperationsFromBlockchainFileClick(Sender: TObject);
    procedure MiRPCCallsClick(Sender: TObject);
  private
    FOperationsHashTree : TOperationsHashTree;
    FSourceNode: TNode;
    FSourceWalletKeys: TWalletKeys;
    procedure OnOperationsHashTreeChanged(Sender : TObject);
    procedure SetSourceNode(AValue: TNode);
    procedure DoLoadFromStorageAssistant(AStorage : TStorage);
    procedure SetSourceWalletKeys(AValue: TWalletKeys);
    procedure UpdateHashTreeList;
    procedure UpdateSelectedOperationInfo;
    Function GetSelectedOperation : TPCOperation;
    Function GetSelectedMultiOperation : TOpMultiOperation;
    procedure DoCheckWalletKeysPwd;
  public
    Property SourceNode : TNode read FSourceNode write SetSourceNode;
    Property SourceWalletKeys : TWalletKeys read FSourceWalletKeys write SetSourceWalletKeys;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses
{$IFDEF TESTNET}
   UFRMRandomOperations,
{$ENDIF}
   UFRMRPCCalls;


{ TFRMOperationsExplorer }

procedure TFRMOperationsExplorer.FormCreate(Sender: TObject);
begin
   FOperationsHashTree := TOperationsHashTree.Create;
   FOperationsHashTree.OnChanged:=OnOperationsHashTreeChanged;
   FSourceNode := Nil;
   FSourceWalletKeys:= Nil;
   {$IFDEF TESTNET}
   bbRandom.Visible:=True;
   bbRandom.Enabled:=True;
   {$ELSE}
   bbRandom.Visible:=False;
   bbRandom.Enabled:=False;
   {$ENDIF}
end;

procedure TFRMOperationsExplorer.bbAddTxClick(Sender: TObject);
Var op : TPCOperation;
  mop : TOpMultiOperation;
  i : Integer;
  aux : String;
  nSender,nReceiver,n_operation : Cardinal;
  amount : Int64;
  senders : TMultiOpSenders;
  receivers : TMultiOpReceivers;
label LBL_start_sender;
label LBL_start_receiver;
begin
  op := GetSelectedOperation;
  mop := Nil;
  If Assigned(op) then begin
    if (op is TOpMultiOperation) then mop := TOpMultiOperation(op);
  end;
  If Not Assigned(mop) then begin
    mop := TOpMultiOperation.Create;
    FOperationsHashTree.AddOperationToHashTree(mop);
    mop.Free;
    mop := FOperationsHashTree.GetOperation(FOperationsHashTree.OperationsCount-1) as TOpMultiOperation;
  end;
  Repeat
LBL_start_sender:
    aux := '';
    If Not InputQuery(Caption,Format('Multioperation: New sender account %d:',[length(senders)+1]),aux) then Break;
    If Not TAccountComp.AccountTxtNumberToAccountNumber(aux,nSender) then Break;

    if (mop.IndexOfAccountSender(nSender)>=0) Or (mop.IndexOfAccountReceiver(nSender,0)>=0) Or (mop.IndexOfAccountSender(nSender,0,senders)>=0) Or (mop.IndexOfAccountChanger(nSender)>=0) then begin
      Application.MessageBox(PChar(Format('Account %s is used!',[TAccountComp.AccountNumberToAccountTxtNumber(nSender)])),PChar(Caption),MB_ICONWARNING+MB_OK);
      Goto LBL_start_sender;
    end;

    n_operation := 0;
    amount := 0;
    If Assigned(FSourceNode) then begin
      If (nSender<FSourceNode.Bank.AccountsCount) then begin
        n_operation := FSourceNode.Bank.SafeBox.Account(nSender).n_operation+1;
        amount := FSourceNode.Bank.SafeBox.Account(nSender).balance;
      end;
    end;

    aux := TAccountComp.FormatMoney(amount);
    If Not InputQuery(Caption,Format('Multioperation sender (%s) amount to send:',[TAccountComp.AccountNumberToAccountTxtNumber(nSender)]),aux) then Break;
    If Not TAccountComp.TxtToMoney(aux,amount) then Break;
    If amount<=0 then Break;

    aux := IntToStr(n_operation);
    If Not InputQuery(Caption,'Sender n_operation + 1:',aux) then Break;
    n_operation:=StrToIntDef(aux,0);
    If n_operation<=0 then Break;

    SetLength(senders,length(senders)+1);
    senders[high(senders)] := CT_TMultiOpSender_NUL;
    senders[high(senders)].Account:=nSender;
    senders[high(senders)].Amount:=amount;
    senders[high(senders)].N_Operation:=n_operation;
    Application.MessageBox(PChar(FormaT('Multioperation new sender:%s Amount:%s N_Operation:%d - Total senders:%d',
       [TAccountComp.AccountNumberToAccountTxtNumber(nSender),TAccountComp.FormatMoney(amount),n_operation,length(senders)])),PChar(Caption),MB_ICONINFORMATION+MB_OK);
  until False;

  Repeat
LBL_start_receiver:
    aux := '';
    If Not InputQuery(Caption,Format('Multioperation: New receiver account %d:',[length(receivers)+1]),aux) then Break;
    If Not TAccountComp.AccountTxtNumberToAccountNumber(aux,nReceiver) then Break;

    amount := 1;
    aux := TAccountComp.FormatMoney(amount);
    If Not InputQuery(Caption,Format('Multioperation receiver (%s) amount to receive:',[TAccountComp.AccountNumberToAccountTxtNumber(nReceiver)]),aux) then Break;
    If Not TAccountComp.TxtToMoney(aux,amount) then Break;
    If amount<=0 then Break;

    SetLength(receivers,length(receivers)+1);
    receivers[high(receivers)] := CT_TMultiOpReceiver_NUL;
    receivers[high(receivers)].Account:=nReceiver;
    receivers[high(receivers)].Amount:=amount;
    Application.MessageBox(PChar(Format('Multioperation new receiver:%s Amount:%s - Total receivers:%d',
       [TAccountComp.AccountNumberToAccountTxtNumber(nReceiver),TAccountComp.FormatMoney(amount),length(receivers)])),PChar(Caption),MB_OK+MB_ICONINFORMATION);
  until False;

  If (length(senders)=0) and (length(receivers)=0) then Exit;

  If Application.MessageBox(PChar(Format('Add %d new senders and %d new receivers to Multioperation with %d senders and %d receivers?',[length(senders),length(receivers),length(mop.Data.txSenders),length(mop.Data.txReceivers)])),
    PChar(Caption),MB_YESNO+MB_ICONINFORMATION)<>IDYES then Exit;

  If Not mop.AddTx(senders,receivers,True) then begin
    Application.MessageBox(PChar('Cannot add senders and receivers!'),PChar(Caption),MB_ICONWARNING+MB_OK);
  end;
  UpdateSelectedOperationInfo;
end;

procedure TFRMOperationsExplorer.bbRandomClick(Sender: TObject);
{$IFDEF TESTNET}
Var FRM : TFRMRandomOperations;
{$ENDIF}
begin
  {$IFDEF TESTNET}
  FRM := TFRMRandomOperations.Create(Self);
  Try
    FRM.SourceNode := Self.SourceNode;
    FRM.SourceWalletKeys := Self.SourceWalletKeys as TWalletKeysExt;
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
  {$ENDIF}
end;

procedure TFRMOperationsExplorer.bbRemoveClick(Sender: TObject);
Var op : TPCOperation;
  mop, newMop : TOpMultiOperation;
  i,nDel, nAccount : Integer;
  aux : String;
  txs : TMultiOpSenders;
  txr : TMultiOpReceivers;
  chs : TMultiOpChangesInfo;
begin
  op := GetSelectedOperation;
  If Not Assigned(op) then exit;
  If Not (op is TOpMultiOperation) Then Raise Exception.Create('Is not a multioperation');
  mop := TOpMultiOperation(op);
  aux := '';
  If Not InputQuery(Caption,Format('Multioperation: Remove account sender/receiver/changer. Which account?',[]),aux) then Exit;
  nAccount := StrToIntDef(aux,-1);
  If nAccount<0 then Exit;
  newMop := TOpMultiOperation.Create;
  Try
    SetLength(txs,0);
    SetLength(txr,0);
    SetLength(chs,0);
    nDel := 0;
    For i:=low(mop.Data.txSenders) to high(mop.Data.txSenders) do begin
      If mop.Data.txSenders[i].Account <> nAccount then begin
        SetLength(txs,length(txs)+1);
        txs[high(txs)] := mop.Data.txSenders[i];
      end else inc(nDel);
    end;
    For i:=low(mop.Data.txReceivers) to high(mop.Data.txReceivers) do begin
      If mop.Data.txReceivers[i].Account <> nAccount then begin
        SetLength(txr,length(txr)+1);
        txr[high(txr)] := mop.Data.txReceivers[i];
      end else inc(nDel);
    end;
    For i:=low(mop.Data.changesInfo) to high(mop.Data.changesInfo) do begin
      If mop.Data.changesInfo[i].Account <> nAccount then begin
        SetLength(chs,length(chs)+1);
        chs[high(chs)] := mop.Data.changesInfo[i];
      end else inc(nDel);
    end;
    If (nDel)>0 then begin
      Application.MessageBox(PChar(Format('Removed %d times account %s',[nDel,TAccountComp.AccountNumberToAccountTxtNumber(nAccount)])),PChar(Caption),MB_OK+MB_ICONINFORMATION);
    end else begin
      Raise Exception.Create(Format('Account %s not found in multioperation',[TAccountComp.AccountNumberToAccountTxtNumber(nAccount)]));
    end;
    newMop.AddTx(txs,txr,True);
    newMop.AddChangeInfos(chs,True);
    FOperationsHashTree.AddOperationToHashTree(newMop);
  finally
    newMop.Free;
  end;

end;

procedure TFRMOperationsExplorer.bbSignClick(Sender: TObject);
Var op : TPCOperation;
  mop : TOpMultiOperation;
  i,j,n : Integer;
begin
  DoCheckWalletKeysPwd;
  If Not Assigned(FSourceNode) then Raise Exception.Create('No node to sign accounts');
  op := GetSelectedOperation;
  If Not Assigned(op) then exit;
  If Not (op is TOpMultiOperation) Then Raise Exception.Create('Is not a multioperation');
  mop := TOpMultiOperation(op);
  n := 0;
  For i:=0 to High(mop.Data.txSenders) do begin
    j := FSourceWalletKeys.IndexOfAccountKey(FSourceNode.Bank.SafeBox.Account(mop.Data.txSenders[i].Account).accountInfo.accountKey);
    If (j>=0) then begin
      // Can sign
      If (Assigned(FSourceWalletKeys.Key[j].PrivateKey)) then begin
        inc(n, mop.DoSignMultiOperationSigner(mop.Data.txSenders[i].Account,FSourceWalletKeys.Key[j].PrivateKey));
      end;
    end;
  end;
  For i:=0 to High(mop.Data.changesInfo) do begin
    j := FSourceWalletKeys.IndexOfAccountKey(FSourceNode.Bank.SafeBox.Account(mop.Data.changesInfo[i].Account).accountInfo.accountKey);
    If (j>=0) then begin
      // Can sign
      If (Assigned(FSourceWalletKeys.Key[j].PrivateKey)) then begin
        inc(n, mop.DoSignMultiOperationSigner(mop.Data.changesInfo[i].Account,FSourceWalletKeys.Key[j].PrivateKey));
      end;
    end;
  end;
  Application.MessageBox(PChar(Format('Signed %d accounts',[n])),PChar(Caption),MB_OK+MB_ICONINFORMATION);
  UpdateSelectedOperationInfo;
end;

procedure TFRMOperationsExplorer.Button1Click(Sender: TObject);
begin

end;

procedure TFRMOperationsExplorer.bbAddOpChangeClick(Sender: TObject);
Var op : TPCOperation;
  mop : TOpMultiOperation;
  i : Integer;
  aux : String;
  nAccount,n_operation : Cardinal;
  changes : TMultiOpChangesInfo;
  new_name,errors : AnsiString;
  new_type : Word;
  new_account_key : TAccountKey;
label LBL_start_changer;
begin
  op := GetSelectedOperation;
  mop := Nil;
  If Assigned(op) then begin
    if (op is TOpMultiOperation) then mop := TOpMultiOperation(op);
  end;
  If Not Assigned(mop) then begin
    mop := TOpMultiOperation.Create;
    FOperationsHashTree.AddOperationToHashTree(mop);
    mop.Free;
    mop := FOperationsHashTree.GetOperation(FOperationsHashTree.OperationsCount-1) as TOpMultiOperation;
  end;
  Repeat
LBL_start_changer:
    aux := '';
    If Not InputQuery(Caption,Format('Multioperation: New changer account %d:',[length(changes)+1]),aux) then Break;
    If Not TAccountComp.AccountTxtNumberToAccountNumber(aux,nAccount) then Break;

    if (mop.IndexOfAccountSender(nAccount)>=0) Or (mop.IndexOfAccountReceiver(nAccount,0)>=0) Or (mop.IndexOfAccountChanger(nAccount,0,changes)>=0) then begin
      Application.MessageBox(PChar(Format('Account %s is used!',[TAccountComp.AccountNumberToAccountTxtNumber(nAccount)])),PChar(Caption),MB_ICONWARNING+MB_OK);
      Goto LBL_start_changer;
    end;

    n_operation := 0;
    new_name := '';
    new_type := 0;
    new_account_key := CT_TECDSA_Public_Nul;
    If Assigned(FSourceNode) then begin
      If (nAccount<FSourceNode.Bank.AccountsCount) then begin
        n_operation := FSourceNode.Bank.SafeBox.Account(nAccount).n_operation+1;
        new_name:= FSourceNode.Bank.SafeBox.Account(nAccount).name;
        new_type:= FSourceNode.Bank.SafeBox.Account(nAccount).account_type;
        new_account_key := FSourceNode.Bank.SafeBox.Account(nAccount).accountInfo.accountKey;
      end;
    end;

    aux := IntToStr(n_operation);
    If Not InputQuery(Caption,'Changer n_operation + 1:',aux) then Break;
    n_operation:=StrToIntDef(aux,0);
    If n_operation<=0 then Break;

    errors := '';
    Repeat
      If (errors<>'') then Application.MessageBox(PChar(Format('Invalid name "%s": %s',[aux,errors])),PChar(Caption),MB_ICONERROR+MB_OK);
      aux := new_name;
      If Not InputQuery(Caption,Format('New name for account %s:',[TAccountComp.AccountNumberToAccountTxtNumber(nAccount)]),aux) then Break;
      aux := LowerCase(aux);
    Until (aux='') Or (TPCSafeBox.ValidAccountName(aux,errors));
    new_name := aux;

    aux := IntToStr(new_type);
    If Not InputQuery(Caption,Format('New type for account %s:',[TAccountComp.AccountNumberToAccountTxtNumber(nAccount)]),aux) then Break;
    new_type := StrToIntDef(aux,0);

    If new_account_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID then aux := TAccountComp.AccountPublicKeyExport(new_account_key)
    else aux := '';
    If Not InputQuery(Caption,Format('New public key for account %s:',[TAccountComp.AccountNumberToAccountTxtNumber(nAccount)]),aux) then begin
      If Not TAccountComp.AccountPublicKeyImport(aux,new_account_key,errors) then begin
        new_account_key := CT_TECDSA_Public_Nul;
        Application.MessageBox(PChar(errors),PChar(Caption),MB_ICONERROR+MB_OK);
      end;
    end;

    SetLength(changes,length(changes)+1);
    changes[high(changes)] := CT_TMultiOpChangeInfo_NUL;
    changes[high(changes)].Account:=nAccount;
    changes[high(changes)].Changes_type:=[account_name,account_type];
    changes[high(changes)].New_Name:=new_name;
    changes[high(changes)].New_Type:=new_type;
    If new_account_key.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID then begin
      changes[high(changes)].Changes_type:=changes[high(changes)].Changes_type + [public_key];
      changes[high(changes)].New_Accountkey := new_account_key;
    end;
    changes[high(changes)].N_Operation:=n_operation;
    Application.MessageBox(PChar(FormaT('Multioperation new changer account:%s N_Operation:%d Name:%s Type:%d - Total changes:%d',
       [TAccountComp.AccountNumberToAccountTxtNumber(nAccount),n_operation,new_name,new_type,length(changes)])),PChar(Caption),MB_ICONINFORMATION+MB_OK);
  until False;


  If (length(changes)=0) then Exit;

  If Application.MessageBox(PChar(Format('Add %d new changes to Multioperation with %d changes?',[length(changes),length(mop.Data.changesInfo)])),
    PChar(Caption),MB_YESNO+MB_ICONINFORMATION)<>IDYES then Exit;

  If Not mop.AddChangeInfos(changes,True) then begin
    Application.MessageBox(PChar('Cannot add changes!'),PChar(Caption),MB_ICONWARNING+MB_OK);
  end;
  UpdateSelectedOperationInfo;
end;

procedure TFRMOperationsExplorer.ActExecuteOperationUpdate(Sender: TObject);
Var obj : TObject;
begin
  obj := GetSelectedOperation;
  TAction(Sender).Enabled:= ((Assigned(obj)) And (Assigned(FSourceNode)));
end;

procedure TFRMOperationsExplorer.ActRemoveAccountFromMultioperationExecute(Sender: TObject);
begin
  bbRemoveClick(Nil);
end;

procedure TFRMOperationsExplorer.ActRemoveAccountFromMultioperationUpdate(Sender: TObject);
Var op : TOpMultiOperation;
begin
  op := GetSelectedMultiOperation;
  TAction(Sender).Enabled:= (Assigned(FSourceNode)) And (Assigned(op));
end;

procedure TFRMOperationsExplorer.ActSignMultioperationExecute(Sender: TObject);
begin
  bbSignClick(Nil);
end;

procedure TFRMOperationsExplorer.ActSignMultioperationUpdate(Sender: TObject);
Var op : TOpMultiOperation;
begin
  op := GetSelectedMultiOperation;
  TAction(Sender).Enabled:= (Assigned(FSourceNode)) And (Assigned(op));
end;

procedure TFRMOperationsExplorer.AddOpChangeToMultioperationExecute(Sender: TObject);
begin
  bbAddOpChangeClick(Nil);
end;

procedure TFRMOperationsExplorer.AddOpChangeToMultioperationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= True;
end;

procedure TFRMOperationsExplorer.ActExecuteOperationExecute(Sender: TObject);
Var op : TPCOperation;
  errors : AnsiString;
begin
  If Not Assigned(FSourceNode) then Raise Exception.Create('No node to Execute');
  op := GetSelectedOperation;
  If Not Assigned(op) then exit;
  If (FSourceNode.AddOperation(Nil,op,errors)) then begin
    Application.MessageBox(PChar('Operation executed!'+#10+Op.ToString),PChar(Caption),MB_OK+MB_ICONINFORMATION);
  end else begin
    Application.MessageBox(PChar('ERROR EXECUTING!'+#10+Op.ToString+#10+#10+'ERROR:'+#10+errors),PChar(Caption),MB_OK+MB_ICONERROR);
  end;
end;

procedure TFRMOperationsExplorer.ActAddTxToMultioperationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= True;
end;

procedure TFRMOperationsExplorer.ActAddTxToMultioperationExecute(Sender: TObject);
begin
  bbAddTxClick(Nil);
end;

procedure TFRMOperationsExplorer.FormDestroy(Sender: TObject);
begin
   FreeAndNil(FOperationsHashTree);
end;

procedure TFRMOperationsExplorer.lbOperationsClick(Sender: TObject);
begin
  UpdateSelectedOperationInfo;
end;

procedure TFRMOperationsExplorer.MiImportOperationsFromTxtClick(Sender: TObject);
Var i : Integer;
  raw : TRawBytes;
  aux : AnsiString;
  auxS : String;
  errors : AnsiString;
  opht : TOperationsHashTree;
  ms : TMemoryStream;
begin
  aux := '';
  If Not InputQuery(Caption,'Paste a RAW hexadecimal operations:',auxS) then exit;
  aux := auxS;
  If Not TCrypto.IsHexString(aux) then Raise Exception.Create('Invalid hexadecimal RAW');
  raw := TCrypto.HexaToRaw(aux);
  If Length(raw)=0 then Exit;
  ms := TMemoryStream.Create;
  opht := TOperationsHashTree.Create;
  Try
    ms.Write(raw[1],Length(raw));
    ms.Position:=0;
    If Not opht.LoadOperationsHashTreeFromStream(ms,false,0,Nil,errors) then Raise Exception.Create(errors);
    For i:=0 to opht.OperationsCount-1 do begin
      FOperationsHashTree.AddOperationToHashTree(opht.GetOperation(i));
    end;
  finally
    ms.Free;
    opht.Free;
  end;
end;

procedure TFRMOperationsExplorer.miLoadFromBlockchainClick(Sender: TObject);
begin
  If Not Assigned(FSourceNode) then raise Exception.Create('No blockchain');
  DoLoadFromStorageAssistant(FSourceNode.Bank.Storage);
end;

procedure TFRMOperationsExplorer.MiLoadOperationsFromBlockchainFileClick(Sender: TObject);
Var fn : String;
  od : TOpenDialog;
  filest : TFileStorage;
begin
  od := TOpenDialog.Create(Self);
  Try
    od.Filter:='Blockchain file (*.blocks)|*.blocks|All files (*.*)|*.*|';
    If od.Execute then begin
      filest := TFileStorage.Create(Self);
      Try
        filest.ReadOnly:=True;
        filest.SetBlockChainFile(od.FileName);
        filest.Initialize;
        DoLoadFromStorageAssistant(filest);
      finally
        filest.Free;
      end;
    end;
  finally
    od.Free;
  end;
end;

procedure TFRMOperationsExplorer.MiRPCCallsClick(Sender: TObject);
Var FRM : TFRMRPCCalls;
begin
  FRM := TFRMRPCCalls.Create(Self);
  Try
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TFRMOperationsExplorer.OnOperationsHashTreeChanged(Sender: TObject);
begin
  UpdateHashTreeList;
end;

procedure TFRMOperationsExplorer.SetSourceNode(AValue: TNode);
begin
  if FSourceNode=AValue then Exit;
  FSourceNode:=AValue;
  If Assigned(FSourceNode) then begin
    FOperationsHashTree.CopyFromHashTree(FSourceNode.Operations.OperationsHashTree);
  end else FOperationsHashTree.ClearHastThree;
end;

procedure TFRMOperationsExplorer.DoLoadFromStorageAssistant(AStorage: TStorage);
Var i : Integer;
  s,aux : String;
  opc : TPCOperationsComp;
begin
  s := Format('Input block from %d to %d',[AStorage.FirstBlock,AStorage.LastBlock]);
  aux := '';
  If Not InputQuery(Caption,s,aux) then Exit;
  i := StrToIntDef(aux,-1);
  If (i<AStorage.FirstBlock) Or (i>AStorage.LastBlock) then Raise Exception.Create(Format('Block %d does not exists',[i]));
  opc := TPCOperationsComp.Create(Nil);
  Try
    If Not AStorage.LoadBlockChainBlock(opc,i) then Raise Exception.Create(Format('Cannot load block %d',[i]));
    FOperationsHashTree.CopyFromHashTree(opc.OperationsHashTree);
  finally
    opc.Free;
  end;
end;

procedure TFRMOperationsExplorer.SetSourceWalletKeys(AValue: TWalletKeys);
begin
  if FSourceWalletKeys=AValue then Exit;
  FSourceWalletKeys:=AValue;
end;

procedure TFRMOperationsExplorer.UpdateHashTreeList;
var i,lasti : Integer;
begin
  lbOperations.Items.BeginUpdate;
  Try
    lasti := lbOperations.ItemIndex;
    lbOperations.Items.Clear;
    For i:=0 to FOperationsHashTree.OperationsCount-1 do begin
      lbOperations.Items.AddObject(Format('%d/%d %s',[i+1,FOperationsHashTree.OperationsCount,FOperationsHashTree.GetOperation(i).ToString]),FOperationsHashTree.GetOperation(i));
    end;
    If (lasti<=0) then lasti:=0;
    If (lasti>=lbOperations.Count) then lasti := lbOperations.Count-1;
    lbOperations.ItemIndex:=lasti;
  finally
    lbOperations.Items.EndUpdate;
  end;
  UpdateSelectedOperationInfo;

end;

procedure TFRMOperationsExplorer.UpdateSelectedOperationInfo;
Var op : TPCOperation;
  opht : TOperationsHashTree;
  i : Integer;
  l : TList;
  aux : String;
  raw : TRawBytes;
  ms : TMemoryStream;
  opr : TOperationResume;
  jsonObj : TPCJSONObject;
begin
  setLength(raw,0);
  If lbOperations.ItemIndex>=0 then begin
    If lbOperations.SelCount>1 then op:=Nil
    else op := TPCOperation( lbOperations.Items.Objects[lbOperations.ItemIndex] )
  end else op := Nil;
  mOperationInfo.Clear;
  mOperationExport.Clear;
  If Assigned(op) then begin
    mOperationInfo.Lines.Add(Format('%s',[op.ToString]));
    mOperationInfo.Lines.Add('');
    mOperationInfo.Lines.Add(Format('OpType:%d ClassName:%s',[op.OpType,op.ClassName]));
    l := TList.Create;
    Try
      op.AffectedAccounts(l); aux := '';
      For i:=0 to l.Count-1 do begin
        if (aux<>'') then aux := aux + ',';
        aux := aux + Format('%s',[TAccountComp.AccountNumberToAccountTxtNumber(PtrInt(l[i]))]);
      end;
      mOperationInfo.Lines.Add(Format('Affected accounts: %s',[aux]));
    finally
      l.Free;
    end;
    If (op.HasValidSignature) then begin
      mOperationInfo.Lines.Add(Format('Signed verified',[]));
    end;
    opht := TOperationsHashTree.Create;
    ms := TMemoryStream.Create;
    try
      opht.AddOperationToHashTree(op);
      opht.SaveOperationsHashTreeToStream(ms,false);
      mOperationInfo.Lines.Add(Format('Size: %.2f Kb (%d bytes)',[ms.Size/1024,ms.Size]));
      ms.Position:=0;
      SetLength(raw,ms.Size);
      ms.ReadBuffer(raw[1],ms.Size);
      mOperationExport.Lines.Text := TCrypto.ToHexaString(raw);
    finally
      ms.Free;
      opht.Free;
    end;
    If op.OperationToOperationResume(FSourceNode.Bank.BlocksCount,op,True,op.SignerAccount,opr) then begin
      jsonObj := TPCJSONObject.Create;
      Try
        TPascalCoinJSONComp.FillOperationObject(opr,FSourceNode.Bank.BlocksCount,jsonObj);
        mOperationInfo.Lines.Add('JSON:');
        mOperationInfo.Lines.Add(jsonObj.ToJSON(False));
      Finally
        jsonObj.Free;
      end;
    end;
  end else begin
    opht := TOperationsHashTree.Create;
    ms := TMemoryStream.Create;
    try
      For i:=0 to lbOperations.Count-1 do begin
        If lbOperations.Selected[i] then begin
          op := TPCOperation( lbOperations.Items.Objects[i] );
          opht.AddOperationToHashTree(op);
        end;
      end;
      opht.SaveOperationsHashTreeToStream(ms,false);
      mOperationInfo.Lines.Add(Format('Selected %d operations',[opht.OperationsCount]));
      mOperationInfo.Lines.Add(Format('Size: %.2f Kb (%d bytes)',[ms.Size/1024,ms.Size]));
      if (opht.OperationsCount>0) then begin
        ms.Position:=0;
        SetLength(raw,ms.Size);
        ms.ReadBuffer(raw[1],ms.Size);
        mOperationExport.Lines.Text := TCrypto.ToHexaString(raw);
        jsonObj := TPCJSONObject.Create;
        Try
          TPascalCoinJSONComp.FillOperationsHashTreeObject(opht,jsonObj);
          mOperationInfo.Lines.Add('JSON:');
          mOperationInfo.Lines.Add(jsonObj.ToJSON(False));
        Finally
          jsonObj.Free;
        end;
      end else mOperationExport.Lines.Clear;
    finally
      ms.Free;
      opht.Free;
    end;
  end;
end;

function TFRMOperationsExplorer.GetSelectedOperation: TPCOperation;
begin
  Result := Nil;
  if lbOperations.ItemIndex>=0 then begin
    Result := TPCOperation( lbOperations.Items.Objects[lbOperations.ItemIndex] );
  end;
end;

function TFRMOperationsExplorer.GetSelectedMultiOperation: TOpMultiOperation;
begin
  Result := Nil;
  if lbOperations.ItemIndex>=0 then begin
    If (lbOperations.Items.Objects[lbOperations.ItemIndex] is TOpMultiOperation) then begin
      Result := TOpMultiOperation( lbOperations.Items.Objects[lbOperations.ItemIndex] );
    end;
  end;
end;

procedure TFRMOperationsExplorer.DoCheckWalletKeysPwd;
Var s : String;
  errors : AnsiString;
begin
  If Not Assigned(FSourceWalletKeys) then Raise Exception.Create('No wallet keys');
  Try
    While (Not FSourceWalletKeys.IsValidPassword) do begin
      s := '';
      if Not InputQuery('Wallet password','Enter wallet password',s) then exit;
      FSourceWalletKeys.WalletPassword := s;
    end;
  finally
    If Not FSourceWalletKeys.IsValidPassword then Raise Exception.Create('Wallet is protected!');
  end;
end;

end.

