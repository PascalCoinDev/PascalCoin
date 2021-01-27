unit UPCRPCFindAccounts;

{ Copyright (c) 2019 by PascalCoin developers, orignal code by Albert Molina

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

{$I ./../config.inc}

Uses classes, SysUtils,
  UJSONFunctions, UAccounts, UBaseTypes, UOpTransaction, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  URPC, UCrypto, UWallet, UBlockChain, ULog, UPCOrderedLists;


Type
  TRPCFindAccounts = Class
  private
  public
    class function FindAccounts(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
  End;

implementation

uses UPCDataTypes;

{ TRPCFindAccounts }

class function TRPCFindAccounts.FindAccounts(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;


{  RPC "findaccounts"
### findaccounts
Find accounts by name/type and returns them as an array of "Account Object"

##### Params
- `name` : String - Name to search
- `namesearchtype` : String - One of those values
  - `exact` : `name` must match (DEFAULT OPTION)
  - `startswith` :
  - `not-startswith` :
  - `contains` :
  - `not-contains` :
  - `endswith` :
  - `not-endswith` :
- `exact` : Boolean (DEPRECATED, use `namesearchtype` instead, True by default) - If False and `name` has value will return accounts starting with `name`
- `type` : Integer - If has value, will return accounts with same type
- `min_balance`,`max_balance` : PASCURRENCY - If have value, will filter by current account balance
- `enc_pubkey` or `b58_pubkey` : HEXASTRING or String - Will return accounts with this public key. **NOTE:** When searching by public key the `start` param value is the position of indexed public keys list instead of accounts numbers
- `statustype` : String - One of those values
  - `all` : (Default option)
  - `for-sale`
  - `for-public-sale`
  - `for-private-sale`
  - `for-swap`
  - `for-account-swap`
  - `for-coin-swap`
  - `for-sale-swap`
  - `not-for-sale-swap`
- `listed` : Boolean (DEPRECATED, use `statustype` instead, False by default) - If True returns only for sale accounts
- `start` : Integer - Start account (by default, 0) - **NOTE:** Is the "start account number", when executing multiple calls you must set `start` value to the latest returned account number + 1 (Except if searching by public key, see below)
- `end` : Integer - End account (by default -1, equals to "no limit")
- `max` : Integer - Max of accounts returned in array (by default, 100)

}

type
  TSearchAccountNameType = (st_exact, st_startswith, st_contains, st_endswith, st_not_startswith, st_not_contains, st_not_endswith);
  TSearchAccountStatusType = (ss_all, ss_for_sale, ss_for_public_sale, ss_for_private_sale, ss_for_swap, ss_for_account_swap, ss_for_coin_swap, ss_for_sale_or_swap, ss_not_for_sale_or_swap);

  function _SearchValidName(const ASearchName : String; const AAccountName : String; ASearchType : TSearchAccountNameType) : Boolean;
  var i : Integer;
  begin
    if (ASearchName.Length=0) then Exit(True); // If nothing to search, allways TRUE
    // Here we know that ASearchName has a value
    if (AAccountName.Length=0) then Exit(False); // If account has NO NAME, allways FALSE
    if (ASearchType=st_exact) then begin
      Exit( AAccountName.Equals(ASearchName) );  // Must match
    end;

    i := AAccountName.IndexOf(ASearchName);
    Result :=
      ((i=0) and (ASearchType in [st_startswith])) // found at first position
      or
      ((i>=0) and (ASearchType in [st_contains])) // found in any pos
      or
      ((i=(AAccountName.Length-1)) and (ASearchType in [st_endswith])) // found at last position
      or
      ((i<0) and (ASearchType in [st_not_startswith, st_not_contains, st_not_endswith])) // not found and must not contain in any pos
      or
      ((i>=1) and (ASearchType in [st_not_startswith])) // not found at first position
      or
      ((i<(AAccountName.Length-1)) and (ASearchType in [st_not_endswith])); // not found at last position
  end;

var
  LAccountName : String;
  LSearchByNameType : TSearchAccountNameType;
  LAccountType : Integer;
  LSearchByStatusType : TSearchAccountStatusType;
  LAccountBalanceMin : Int64;
  LAccountBalanceMax : Int64;

  function _IsValidAccount(const AAccount : TAccount) : Boolean;
  begin
    if (LAccountType <> -1) AND (Integer(AAccount.account_type) <> LAccountType) then Exit(False);
    if (Not _SearchValidName(LAccountName,AAccount.name.ToString,LSearchByNameType)) then Exit(False);
    case LSearchByStatusType of
      ss_for_sale :        if Not TAccountComp.IsAccountForSale(AAccount.accountInfo) then Exit(False);
      ss_for_public_sale:  if Not TAccountComp.IsAccountForPublicSale(AAccount.accountInfo) then Exit(False);
      ss_for_private_sale: if Not TAccountComp.IsAccountForPrivateSale(AAccount.accountInfo) then Exit(False);
      ss_for_swap:         if Not TAccountComp.IsAccountForSwap(AAccount.accountInfo) then Exit(False);
      ss_for_account_swap: if Not TAccountComp.IsAccountForAccountSwap(AAccount.accountInfo) then Exit(False);
      ss_for_coin_swap:    if Not TAccountComp.IsAccountForCoinSwap(AAccount.accountInfo) then Exit(False);
      ss_for_sale_or_swap: if Not TAccountComp.IsAccountForSwap(AAccount.accountInfo) then Exit(False);
      ss_not_for_sale_or_swap: if TAccountComp.IsAccountForSwap(AAccount.accountInfo) then Exit(False);
    end;
    if ((LAccountBalanceMin>=0) AND (AAccount.balance<LAccountBalanceMin)) then Exit(False);
    if ((LAccountBalanceMax>=0) AND (AAccount.balance>LAccountBalanceMax)) then Exit(False);
    Result := True;
  end;

var
  LString : String;
  LAccountNumber : Integer;
  LRaw : TRawBytes;
  LSearchByPubkey : Boolean;
  LStart, LMax, LEnd : Integer;
  LAccountsNumbersList : TAccountsNumbersList;
  LAccount : TAccount;
  i : Integer;
  LErrors : String;
  LAccPubKey : TAccountKey;
  LOutput : TPCJSONArray;
  LStartsWith : TOrderedRawList;
  LAccountsList : TList<Integer>;
begin
  // Get Parameters
  Result := False;
  LAccountName := LowerCase(AInputParams.AsString('name', '')); // Convert to lowercase...
  if AInputParams.IndexOfName('namesearchtype')>=0 then begin
    LString := AInputParams.AsString('namesearchtype','');
    if (AnsiSameStr(LString,'exact')) then LSearchByNameType := st_exact
    else if (AnsiSameStr(LString,'startswith')) then LSearchByNameType := st_startswith
    else if (AnsiSameStr(LString,'not-startswith')) then LSearchByNameType := st_not_startswith
    else if (AnsiSameStr(LString,'contains')) then LSearchByNameType := st_contains
    else if (AnsiSameStr(LString,'not-contains')) then LSearchByNameType := st_not_contains
    else if (AnsiSameStr(LString,'endswith')) then LSearchByNameType := st_endswith
    else if (AnsiSameStr(LString,'not-endswith')) then LSearchByNameType := st_not_endswith
    else begin
      AErrorNum := CT_RPC_ErrNum_InvalidData;
      AErrorDesc := Format('Invalid "namesearchtype" value: "%s"',[LString]);
      Exit(False);
    end;
  end else begin
    if AInputParams.AsBoolean('exact',True) then LSearchByNameType := st_exact
    else LSearchByNameType := st_startswith;
  end;
  LAccountType := AInputParams.AsInteger('type', -1);
  LStart := AInputParams.AsInteger('start', 0);
  LEnd := AInputParams.AsInteger('end', -1);
  LMax := AInputParams.AsInteger('max', 100);
  if AInputParams.IndexOfName('statustype')>=0 then begin
    LString := AInputParams.AsString('statustype','all');
    if (AnsiSameStr(LString,'all')) then LSearchByStatusType := ss_all
    else if (AnsiSameStr(LString,'for-sale')) then LSearchByStatusType := ss_for_sale
    else if (AnsiSameStr(LString,'for-public-sale')) then LSearchByStatusType := ss_for_public_sale
    else if (AnsiSameStr(LString,'for-private-sale')) then LSearchByStatusType := ss_for_private_sale
    else if (AnsiSameStr(LString,'for-swap')) then LSearchByStatusType := ss_for_swap
    else if (AnsiSameStr(LString,'for-account-swap')) then LSearchByStatusType := ss_for_account_swap
    else if (AnsiSameStr(LString,'for-coin-swap')) then LSearchByStatusType := ss_for_coin_swap
    else if (AnsiSameStr(LString,'for-sale-swap')) then LSearchByStatusType := ss_for_sale_or_swap
    else if (AnsiSameStr(LString,'not-for-sale-swap')) then LSearchByStatusType := ss_not_for_sale_or_swap
    else begin
      AErrorNum := CT_RPC_ErrNum_InvalidData;
      AErrorDesc := Format('Invalid "statustype" value: "%s"',[LString]);
      Exit(False);
    end;
  end else begin
    if AInputParams.AsBoolean('listed',False) then LSearchByStatusType := ss_for_sale_or_swap
    else LSearchByStatusType := ss_all;
  end;

  LAccountBalanceMin := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('min_balance',-1));
  LAccountBalanceMax := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('max_balance',-1));

  // Validate Parameters
  if (Length(LAccountName)>0) And (LSearchByNameType = st_exact) then begin
    LRaw.FromString( LAccountName );
    if not ASender.Node.Bank.SafeBox.ValidAccountName(LRaw, LErrors) then begin
      AErrorNum := CT_RPC_ErrNum_InvalidAccountName;
      AErrorDesc := LErrors;
      exit;
    end;
  end;

  if LStart < 0 then begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := '"start" param must be >=0';
    exit;
  end;
  if LMax <= 0 then begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := '"max" param must be greater than zero';
    exit;
  end;

  if (LEnd<0) or (LEnd>=ASender.Node.Bank.AccountsCount) then begin
    LEnd := ASender.Node.Bank.AccountsCount - 1;
  end;

  // Declare return result (empty by default)
  LOutput := AJSONResponse.GetAsArray('result');

  // Search by accPubKey (if provided)
  If TPascalCoinJSONComp.CapturePubKey(AInputParams, '',LAccPubKey,LErrors) then begin
    // Must match accPubKey
    if (Not Assigned(ASender.Node.Bank.SafeBox.OrderedAccountKeysList)) then begin
      AErrorNum := CT_RPC_ErrNum_NotImplemented;
      AErrorDesc := 'Not allowed search by public key';
      Exit;
    end;
    LSearchByPubkey := True;
    LAccountsNumbersList := ASender.Node.Bank.SafeBox.OrderedAccountKeysList.GetAccountsUsingThisKey(LAccPubKey);
    if (Not Assigned(LAccountsNumbersList)) then begin
      // No account available with this pubkey, exit
      Result := True;
      Exit;
    end;
  end else LSearchByPubkey := False;
  // Search by name
  if ((Length(LAccountName)>0) AND (LSearchByNameType in [st_exact,st_startswith] )) then begin
    if (LSearchByNameType in [st_exact]) then begin
      LAccountNumber := ASender.Node.Bank.SafeBox.FindAccountByName(LAccountName);
      if LAccountNumber >= 0 then begin
        LAccount := ASender.Node.GetMempoolAccount(LAccountNumber);
        if (_IsValidAccount(LAccount)) and
          ((Not LSearchByPubkey) OR (TAccountComp.EqualAccountKeys(LAccPubKey,LAccount.accountInfo.accountKey))) then begin
           TPascalCoinJSONComp.FillAccountObject(LAccount,LOutput.GetAsObject(LOutput.Count));
        end;
      end;
    end else begin
      // Starts-with indexed
      LStartsWith := TOrderedRawList.Create;
      try
        LRaw.FromString(LAccountName);
        ASender.Node.Bank.SafeBox.FindAccountsStartingByName(LRaw,LStartsWith,LMax);
        for i := 0 to LStartsWith.Count-1 do begin
          LAccountNumber := LStartsWith.GetTag(i);
          LAccount := ASender.Node.GetMempoolAccount(LAccountNumber);
          if (_IsValidAccount(LAccount)) and
            ((Not LSearchByPubkey) OR (TAccountComp.EqualAccountKeys(LAccPubKey,LAccount.accountInfo.accountKey))) then begin
             TPascalCoinJSONComp.FillAccountObject(LAccount,LOutput.GetAsObject(LOutput.Count));
          end;
        end;
      finally
        LStartsWith.Free;
      end;
    end;
  end else begin
    // Search by type-forSale-balance
    if (LSearchByPubkey) then begin
      LAccountsList := TList<Integer>.Create;
      try
        LAccountsNumbersList.FillList(LStart,LEnd-LStart+1,LAccountsList);
        for i := 0 to LAccountsList.Count-1 do begin
          LAccount := ASender.Node.GetMempoolAccount( LAccountsList[i] );
          if (_IsValidAccount(LAccount)) then begin
            TPascalCoinJSONComp.FillAccountObject(LAccount,LOutput.GetAsObject(LOutput.Count));
            if LOutput.Count>=LMax then break;
          end;
        end;
      finally
        LAccountsList.Free;
      end;
    end else begin
      i := LStart;
      while (Not ASender.Terminated) And (i < LEnd) do begin
        LAccount := ASender.Node.GetMempoolAccount(i);

        if (_IsValidAccount(LAccount)) then begin
          TPascalCoinJSONComp.FillAccountObject(LAccount,LOutput.GetAsObject(LOutput.Count));
          if LOutput.Count>=LMax then break;
        end;
        inc(i);

      end;
    end;
  end;
  Result := True;
end;

initialization
  TRPCProcess.RegisterProcessMethod('findaccounts',TRPCFindAccounts.FindAccounts);
finalization
  TRPCProcess.UnregisterProcessMethod('findaccounts');
end.
