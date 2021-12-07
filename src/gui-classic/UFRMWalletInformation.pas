unit UFRMWalletInformation;

interface

uses
  UFRMWallet, Classes;

type
  TFRMWalletInformation = class
  private

  public
    class procedure FillAccountInformation
    (
      const Strings: TStrings;
      const AccountNumber: Cardinal
    );

  end;



implementation

uses
  UPCDataTypes, UJSONFunctions, SysUtils, UAccounts, UBaseTypes, URPC;

class procedure TFRMWalletInformation.FillAccountInformation
(
  const Strings: TStrings;
  const AccountNumber: Cardinal
);
Var
  account : TAccount;
  s : String;
  LjsonObj : TPCJSONObject;
  vString : string;
begin
  if AccountNumber<0 then exit;
  account := FRMWallet.Node.GetMempoolAccount(AccountNumber);
  if
    Length(account.name)>0 then
    s:='Name: '+TEncoding.ANSI.GetString(account.name)
  else
    s:='';
  Strings.Add(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(AccountNumber),s,account.account_type]));
  Strings.Add('');
  Strings.Add(Format('Current balance: %s',[TAccountComp.FormatMoney(account.balance)]));
  Strings.Add('');
  Strings.Add(Format('Updated on block: %d  (%d blocks ago)',[account.GetLastUpdatedBlock,FRMWallet.Node.Bank.BlocksCount-account.GetLastUpdatedBlock]));
  Strings.Add(Format('Updated on block as active mode: %d  (%d blocks ago)',[account.updated_on_block_active_mode,FRMWallet.Node.Bank.BlocksCount-account.updated_on_block_active_mode]));
  Strings.Add(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(account.accountInfo.accountKey.EC_OpenSSL_NID)]));
  Strings.Add(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.accountKey)]));

  if Length(account.account_data)>0 then
  begin
    // Skybuck: string compilation fail: needs UBaseTypes otherwise don't compile.
    vString := account.account_data.ToHexaString;
    Strings.Add
    (
      Format
      (
        'Account Data: %s',
        [
          vString
        ]
      )
    )
  end else Strings.Add(Format('Account Data: (No data)',[]));
  Strings.Add(Format('Account Seal: %s',[account.account_seal.ToHexaString]));
  if TAccountComp.IsAccountForSale(account.accountInfo) then begin
    Strings.Add('');
    Strings.Add('** Account is for sale: **');
    Strings.Add(Format('Price: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
    Strings.Add(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForPrivateSale(account.accountInfo) then begin
      Strings.Add('');
      Strings.Add('** Private sale **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
      Strings.Add('');
      if TAccountComp.IsAccountLocked(account.accountInfo,FRMWallet.Node.Bank.BlocksCount) then begin
        Strings.Add(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,FRMWallet.Node.Bank.BlocksCount,account.accountInfo.locked_until_block-FRMWallet.Node.Bank.BlocksCount]));
      end else begin
        Strings.Add(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,FRMWallet.Node.Bank.BlocksCount]));
      end;
    end;
  end else if TAccountComp.IsAccountForSwap(account.accountInfo) then begin
    Strings.Add('');
    if TAccountComp.IsAccountForAccountSwap(account.accountInfo) then begin
      Strings.Add('** Account is for Atomic Account Swap: **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
    end else if TAccountComp.IsAccountForCoinSwap(account.accountInfo) then begin
      Strings.Add('** Account is for Atomic Coin Swap: **');
      Strings.Add(Format('Amount to swap: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
      Strings.Add(Format('Counterparty account: %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    end;
    Strings.Add(Format('Public secret to find: %s',[account.accountInfo.hashed_secret.ToHexaString]));
    Strings.Add('');
    if TAccountComp.IsAccountLocked(account.accountInfo,FRMWallet.Node.Bank.BlocksCount) then begin
      Strings.Add(Format('SWAP IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,FRMWallet.Node.Bank.BlocksCount,account.accountInfo.locked_until_block-FRMWallet.Node.Bank.BlocksCount]));
    end else begin
        Strings.Add(Format('SWAP IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,FRMWallet.Node.Bank.BlocksCount]));
    end;
  end;
  LjsonObj := TPCJSONObject.Create;
  Try
    TPascalCoinJSONComp.FillAccountObject(account,LjsonObj);
    Strings.Add('ACCOUNT JSON:');
    Strings.Add(LjsonObj.ToJSON(False));
  Finally
    LjsonObj.Free;
  end;

end;


end.
