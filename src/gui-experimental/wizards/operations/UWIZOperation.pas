unit UWIZOperation;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Shares selected accounts, operation inputs, fees and payload state across
  wizard screens. Tracks wallet unlock ownership so only wizard-owned
  unlocks are relocked.
}

{ Public types in this unit

  Type                                               | Purpose
  ---------------------------------------------------+--------------------------------------------------------------------------
  TWalletUnlockScope                                 | Relocks a password-protected wallet only when it owns the unlock.
  TWIZOperationsModel                                | Owns the shared inputs and unlock scope for an operation wizard.
  TWIZOperationsModel.TAccountModel                  | Stores the selected source accounts and their count.
  TWIZOperationsModel.TBuyAccountModel               | Stores the purchase target, price and new owner key.
  TWIZOperationsModel.TChangeAccountInfoModel        | Stores name/type changes, approved targets and an optional fee payer.
  TWIZOperationsModel.TChangeAccountPrivateKeyModel  | Stores the selected wallet key for an account ownership change.
  TWIZOperationsModel.TChangeKeyModel                | Selects transfer to an imported key or an existing wallet key.
  TWIZOperationsModel.TEnlistAccountForSaleModel     | Stores sale price, beneficiary, buyer restrictions and lock height.
  TWIZOperationsModel.TFeeModel                      | Stores the default and selected fee per operation.
  TWIZOperationsModel.TOperationExecuteResultHandler | Receives an operation's source account, outcome and result text.
  TWIZOperationsModel.TPayloadModel                  | Stores payload content, encryption mode, password and encoded bytes.
  TWIZOperationsModel.TSendPASCModel                 | Stores transfer amount, recipient and specified-amount or send-all mode.
  TWIZOperationsModel.TSignerModel                   | Stores eligible signers, the selected signer and fee-paying mode.
  TWIZOperationsModel.TTransferAccountModel          | Stores the imported public key that will own transferred accounts.
}

interface

uses
  Classes, SysUtils, UAccounts, UWallet, UBaseTypes, UCoreObjects, UPCDataTypes;

type

  { Borrows the wallet keys and only relocks a password-protected wallet which
    this scope successfully unlocked. No wallet files or global wallet state
    are accessed until a caller supplies the keys. }

  TWalletUnlockScope = class(TComponent)
  private
    FWalletKeys: TWalletKeys;
    FOwnsUnlock: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function TryAcquire(AWalletKeys: TWalletKeys; AUnlock: TNotifyEvent; out AMessage: String): Boolean;
    procedure Release;
    property OwnsUnlock: Boolean read FOwnsUnlock;
  end;

  { TWIZOperationsModel }

  TWIZOperationsModel = class(TComponent)
    public type

      { TOperationExecuteResultHandler }

      TOperationExecuteResultHandler = procedure(const ASourceAccount: TAccount; AOpType: TExecuteOperationType; const AOpText: String; Result: boolean; const Message: String) of object;


      { TAccountModel }

      TAccountModel = class(TComponent)
      public
        SelectedAccounts: TArray<TAccount>;
        Count: Integer;
      end;

      { TSendPASCModel }

      TSendPASCModel = class(TComponent)
      public
        SingleAmountToSend: int64;
        DestinationAccount: TAccount;
        SendPASCMode: TSendPASCMode;
      end;

      { TChangeKeyModel }

      TChangeKeyModel = class(TComponent)
      public
        ChangeKeyMode: TChangeKeyMode;
      end;

      { TTransferAccountModel }

      TTransferAccountModel = class(TComponent)
      public
        AccountKey: TAccountKey;
      end;

      { TChangeAccountPrivateKeyModel }

      TChangeAccountPrivateKeyModel = class(TComponent)
      public
        SelectedIndex: integer;
        NewWalletKey: TWalletKey;
      end;

      { TWIZEnlistAccountForSaleModel }
      TEnlistAccountForSaleModel = class(TComponent)
      public
        SalePrice: int64;
        NewOwnerPublicKey: TAccountKey;
        LockedUntilBlock: cardinal;
        SellerAccount: TAccount;
        AccountSaleMode: TAccountSaleMode;
      end;

      { TFeeModel }

      TFeeModel = class(TComponent)
      public
        DefaultFee, SingleOperationFee: int64;
      end;

      { TSignerModel }

      TSignerModel = class(TComponent)
      public
        OperationSigningMode: TOperationSigningMode;
        SignerAccount: TAccount;
        SignerCandidates: TArray<TAccount>;
        SelectedIndex: integer;
      end;

      { TPayloadModel }

      TPayloadModel = class(TComponent)
      public
        HasPayload: boolean;
        Content, Password: string;
        PayloadEncryptionMode: TPayloadEncryptionMode;
        EncodedBytes: TRawBytes;
      end;

      { TChangeAccountInfoModel }

      TChangeAccountInfoModel = class(TComponent)
      public
        NewName: TRawBytes;
        NewType: Word;
        // Keep the user's original selection separate from approved updates.
        TargetAccounts: TArray<TAccount>;
        SkippedCount: Integer;
        SkippedAccountsApproved: Boolean;
        UseSingleFeePayer: Boolean;
        FeePayerSelected: Boolean;
        FeePayerAccount: TAccount;
      end;

      { TBuyAccountModel }

      TBuyAccountModel = class(TComponent)
      public
        AccountToBuy: TAccount;
        Amount: Int64;
        NewOwnerPublicKey: TAccountKey;
        SelectedIndex: integer;
      end;

    private
      FExecuteOperationType: TExecuteOperationType;
      FUnlockScope: TWalletUnlockScope;
      FAccount: TAccountModel;
      FSendPASC: TSendPASCModel;
      FChangeKey: TChangeKeyModel;
      FTransferAccount: TTransferAccountModel;
      FChangeAccountPrivateKey: TChangeAccountPrivateKeyModel;
      FEnlistAccountForSale: TEnlistAccountForSaleModel;
      FFee: TFeeModel;
      FSigner: TSignerModel;
      FPayload: TPayloadModel;
      FChangeAccountInfo: TChangeAccountInfoModel;
      FBuyAccount: TBuyAccountModel;
    public
      constructor Create(AOwner: TComponent; AType: TExecuteOperationType); overload;
      function TryUnlockWallet(AWalletKeys: TWalletKeys; AUnlock: TNotifyEvent; out AMessage: String): Boolean;
      procedure RelockWallet;
      { Filters a common fee payer against the same ownership, lock and total
        fee rules used by preparation. Secondary mode lets funded targets pay
        their own fee before falling back to the selected fee payer. }
      class function FilterSignerCandidates(const ACandidates, ATargets: TArray<TAccount>; ABlocksCount: Cardinal; AFee: Int64; AMode: TOperationSigningMode): TArray<TAccount>; static;
      property ExecuteOperationType: TExecuteOperationType read FExecuteOperationType;
      property Account: TAccountModel read FAccount;
      property SendPASC: TSendPASCModel read FSendPASC;
      property ChangeKey: TChangeKeyModel read FChangeKey;
      property TransferAccount: TTransferAccountModel read FTransferAccount;
      property ChangeAccountPrivateKey: TChangeAccountPrivateKeyModel read FChangeAccountPrivateKey;
      property EnlistAccountForSale: TEnlistAccountForSaleModel read FEnlistAccountForSale;
      property Fee: TFeeModel read FFee;
      property Signer: TSignerModel read FSigner;
      property Payload: TPayloadModel read FPayload;
      property ChangeAccountInfo: TChangeAccountInfoModel read FChangeAccountInfo;
      property BuyAccount: TBuyAccountModel read FBuyAccount;
      property UnlockScope: TWalletUnlockScope read FUnlockScope;
  end;

implementation

uses UConst, UCommon;

{ TWalletUnlockScope }

destructor TWalletUnlockScope.Destroy;
begin
  Release;
  inherited Destroy;
end;

procedure TWalletUnlockScope.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FWalletKeys) then
  begin
    FWalletKeys := nil;
    FOwnsUnlock := False;
  end;
  inherited Notification(AComponent, Operation);
end;

function TWalletUnlockScope.TryAcquire(AWalletKeys: TWalletKeys; AUnlock: TNotifyEvent; out AMessage: String): Boolean;
begin
  AMessage := '';
  if not Assigned(AWalletKeys) then
  begin
    AMessage := 'No wallet keys found.';
    Exit(False);
  end;
  if FWalletKeys <> AWalletKeys then
  begin
    Release;
    if Assigned(FWalletKeys) then
      FWalletKeys.RemoveFreeNotification(Self);
    FWalletKeys := AWalletKeys;
    FWalletKeys.FreeNotification(Self);
  end;
  if not AWalletKeys.HasPassword or AWalletKeys.IsValidPassword then
    Exit(True);

  // An external lock ends any previous unlock owned by this scope.
  FOwnsUnlock := False;
  try
    if Assigned(AUnlock) then
      AUnlock(Self);
  finally
    // Also retain ownership if a dialog callback raises after unlocking.
    FOwnsUnlock := Assigned(FWalletKeys) and FWalletKeys.HasPassword and FWalletKeys.IsValidPassword;
  end;
  Result := Assigned(FWalletKeys) and FWalletKeys.IsValidPassword;
  if not Result then
    AMessage := 'Wallet remains locked. Unlock it before executing the operation.';
end;

procedure TWalletUnlockScope.Release;
var
  LWalletKeys: TWalletKeys;
begin
  if not FOwnsUnlock then
    Exit;
  LWalletKeys := FWalletKeys;
  FOwnsUnlock := False;
  if Assigned(LWalletKeys) then
    LWalletKeys.LockWallet;
end;

{ TWIZOperationsModel }

constructor TWIZOperationsModel.Create(AOwner: TComponent; AType: TExecuteOperationType);
begin
  inherited Create(AOwner);
  FExecuteOperationType := AType;
  FUnlockScope := TWalletUnlockScope.Create(Self);
  FAccount := TAccountModel.Create(Self);
  FSendPASC := TSendPASCModel.Create(Self);
  FChangeKey := TChangeKeyModel.Create(Self);
  FTransferAccount := TTransferAccountModel.Create(Self);
  FChangeAccountPrivateKey := TChangeAccountPrivateKeyModel.Create(Self);
  FEnlistAccountForSale := TEnlistAccountForSaleModel.Create(Self);
  FFee := TFeeModel.Create(Self);
  FSigner := TSignerModel.Create(Self);
  FPayload := TPayloadModel.Create(Self);
  FChangeAccountInfo := TChangeAccountInfoModel.Create(Self);
  FBuyAccount := TBuyAccountModel.Create(Self);
end;

function TWIZOperationsModel.TryUnlockWallet(AWalletKeys: TWalletKeys; AUnlock: TNotifyEvent; out AMessage: String): Boolean;
begin
  Result := FUnlockScope.TryAcquire(AWalletKeys, AUnlock, AMessage);
end;

procedure TWIZOperationsModel.RelockWallet;
begin
  FUnlockScope.Release;
end;

class function TWIZOperationsModel.FilterSignerCandidates(const ACandidates, ATargets: TArray<TAccount>; ABlocksCount: Cardinal; AFee: Int64; AMode: TOperationSigningMode): TArray<TAccount>;
var
  LCandidate, LTarget: TAccount;
  LCandidateIdx, LTargetIdx: Integer;
  LRequiredFee, LFee: UInt64;
  LValid: Boolean;
begin
  Result := nil;
  if (AFee < 0) or (AFee > CT_MaxTransactionFee) or (Length(ATargets) = 0) then
    Exit;
  for LTargetIdx := Low(ATargets) to High(ATargets) do
    if TAccountComp.IsAccountLocked(ATargets[LTargetIdx].accountInfo, ABlocksCount) or
      TAccountComp.IsAccountBlockedByProtocol(ATargets[LTargetIdx].account, ABlocksCount) then
      Exit;
  LFee := UInt64(AFee);
  for LCandidateIdx := Low(ACandidates) to High(ACandidates) do
  begin
    LCandidate := ACandidates[LCandidateIdx];
    LValid := not TAccountComp.IsAccountLocked(LCandidate.accountInfo, ABlocksCount) and
      not TAccountComp.IsAccountBlockedByProtocol(LCandidate.account, ABlocksCount);
    LRequiredFee := 0;
    for LTargetIdx := Low(ATargets) to High(ATargets) do
    begin
      LTarget := ATargets[LTargetIdx];
      if (AMode = akaSecondary) and (LTarget.balance >= LFee) and
        (LTarget.account <> LCandidate.account) then
        Continue;
      if not TAccountComp.EqualAccountKeys(LCandidate.accountInfo.accountKey, LTarget.accountInfo.accountKey) then
        LValid := False;
      if LCandidate.balance - LRequiredFee < LFee then
      begin
        LValid := False;
        Break;
      end;
      Inc(LRequiredFee, LFee);
    end;
    if LValid then
      TArrayTool<TAccount>.Add(Result, LCandidate);
  end;
end;

end.
