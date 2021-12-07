unit UFRMWalletInformation;

interface

uses
  UFRMWallet, Classes, UBlockChain;

type
  TFRMWalletInformation = class
  private

  public
    class procedure FillAccountInformation
    (
      const Strings: TStrings;
      const AccountNumber: Cardinal
    );

    class procedure FillOperationInformation
    (
      const Strings: TStrings;
      const OperationResume: TOperationResume
    );
  end;

implementation

uses
  UPCDataTypes, UJSONFunctions, SysUtils, UAccounts, UBaseTypes, URPC,
  UEPasa, UTxMultiOperation, UCrypto, UEPasaDecoder;

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

class procedure TFRMWalletInformation.FillOperationInformation
(
  const Strings: TStrings;
  const OperationResume: TOperationResume
);
var i : Integer;
  jsonObj : TPCJSONObject;
  LEPASA : TEPasa;
begin
  If (not OperationResume.valid) then exit;
  If OperationResume.Block<FRMWallet.Node.Bank.BlocksCount then
    if (OperationResume.NOpInsideBlock>=0) then begin
      Strings.Add(Format('Block: %d/%d',[OperationResume.Block,OperationResume.NOpInsideBlock]))
    end else begin
      Strings.Add(Format('Block: %d',[OperationResume.Block]))
    end
  else Strings.Add('** Pending operation not included on blockchain **');
  Strings.Add(Format('%s',[OperationResume.OperationTxt]));
  If (OperationResume.isMultiOperation) then begin
    Strings.Add('Multioperation:');
    For i := 0 to High(OperationResume.Senders) do begin
      Strings.Add(Format('  Sender (%d/%d): %s %s PASC Payload(%d):%s',[i+1,length(OperationResume.Senders),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Senders[i].Account),TAccountComp.FormatMoney(OperationResume.Senders[i].Amount),OperationResume.Senders[i].Payload.payload_type,OperationResume.Senders[i].Payload.payload_raw.ToHexaString]));
    end;
    For i := 0 to High(OperationResume.Receivers) do begin
      Strings.Add(Format('  Receiver (%d/%d): %s %s PASC Payload(%d):%s',[i+1,length(OperationResume.Receivers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Receivers[i].Account),TAccountComp.FormatMoney(OperationResume.Receivers[i].Amount),OperationResume.Receivers[i].Payload.payload_type,OperationResume.Receivers[i].Payload.payload_raw.ToHexaString]));
    end;
    For i := 0 to High(OperationResume.Changers) do begin
      Strings.Add(Format('  Change info (%d/%d): %s [%s]',[i+1,length(OperationResume.Changers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Changers[i].Account),TOpMultiOperation.OpChangeAccountInfoTypesToText(OperationResume.Changers[i].Changes_type)]));
    end;

  end;
  Strings.Add(Format('OpType:%d Subtype:%d',[OperationResume.OpType,OperationResume.OpSubtype]));
  Strings.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash)]));
  If (Length(OperationResume.OperationHash_OLD)>0) then begin
    Strings.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash_OLD)]));
  end;
  if TEPasaDecoder.TryDecodeEPASA(OperationResume.DestAccount,OperationResume.OriginalPayload,FRMWallet.Node,FRMWallet.WalletKeys,Nil,LEPASA) then begin
    Strings.Add('EPASA: '+LEPASA.ToString);
  end else Strings.Add('No EPASA format');
  Strings.Add(Format('Payload type:%s length:%d',['0x'+IntToHex(OperationResume.OriginalPayload.payload_type,2), length(OperationResume.OriginalPayload.payload_raw)]));
  if (Length(OperationResume.OriginalPayload.payload_raw)>0) then begin
    If OperationResume.PrintablePayload<>'' then begin
      Strings.Add(Format('Payload (human): %s',[OperationResume.PrintablePayload]));
    end;
    Strings.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(OperationResume.OriginalPayload.payload_raw)]));
  end;
  If OperationResume.Balance>=0 then begin
    Strings.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(OperationResume.Balance)]));
  end;
  jsonObj := TPCJSONObject.Create;
  Try
    TPascalCoinJSONComp.FillOperationObject(OperationResume,FRMWallet.Node.Bank.BlocksCount,
      FRMWallet.Node,FRMWallet.WalletKeys,Nil,
      jsonObj);
    Strings.Add('OPERATION JSON:');
    Strings.Add(jsonObj.ToJSON(False));
  Finally
    jsonObj.Free;
  end;
end;


end.
