unit UWalletInfoTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

interface

uses Classes, SysUtils, fpcunit, testregistry, UCrypto, UBlockChain, UPCDataTypes;

type
  TWalletInfoTests = class(TTestCase)
  private
    FKey: TECPrivateKey;
    function Account: TAccount;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AccountDetailsIncludeDataSealAndJson;
    procedure PrivateSaleDetailsUseSaleState;
    procedure SwapDetailsIncludeHashAndCounterparty;
    procedure MultioperationDetailsIncludeAllParticipants;
    procedure ShortOperationHashesKeepTheirPrefixAndSuffix;
  end;

implementation

uses UAccounts, UConst, UCoreUtils, UBaseTypes, UCellRenderers;

procedure TWalletInfoTests.ShortOperationHashesKeepTheirPrefixAndSuffix;
begin
  AssertEquals('', TCellRenderers.OperationShortHash(''));
  AssertEquals('12345678', TCellRenderers.OperationShortHash('12345678'));
  AssertEquals('1234...6789', TCellRenderers.OperationShortHash('123456789'));
  AssertEquals('1234...9ABC', TCellRenderers.OperationShortHash('123456789ABC'));
end;

procedure TWalletInfoTests.SetUp;
begin
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

procedure TWalletInfoTests.TearDown;
begin
  FKey.Free;
end;

function TWalletInfoTests.Account: TAccount;
begin
  Result := CT_Account_NUL;
  Result.account := 123;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := FKey.PublicKey;
end;

procedure TWalletInfoTests.AccountDetailsIncludeDataSealAndJson;
var
  value: TAccount;
  text: String;
begin
  value := Account;
  value.account_data := TCrypto.HexaToRaw('00FF0102');
  value.account_seal := TCrypto.HexaToRaw('01020304');
  text := value.GetInfoText(Cardinal(0));
  AssertTrue(Pos('Account Data: ' + value.account_data.ToHexaString, text) > 0);
  AssertTrue(Pos('Account Seal: ' + value.account_seal.ToHexaString, text) > 0);
  AssertTrue(Pos('ACCOUNT JSON:', text) > 0);
  AssertTrue(Pos('"data"', text) > 0);
  AssertTrue(Pos('"seal"', text) > 0);
end;

procedure TWalletInfoTests.PrivateSaleDetailsUseSaleState;
var
  value: TAccount;
  text: String;
begin
  value := Account;
  value.accountInfo.state := as_ForSale;
  value.accountInfo.new_publicKey := FKey.PublicKey;
  value.accountInfo.price := 20000;
  value.accountInfo.locked_until_block := 50;
  text := value.GetInfoText(Cardinal(0));
  AssertTrue(Pos('** Private sale **', text) > 0);
  AssertTrue(Pos('New Base58 Public key:', text) > 0);
  AssertTrue(Pos('PURCHASE IS SECURE UNTIL BLOCK 50', text) > 0);
end;

procedure TWalletInfoTests.SwapDetailsIncludeHashAndCounterparty;
var
  value: TAccount;
  text: String;
begin
  value := Account;
  value.accountInfo.state := as_ForAtomicCoinSwap;
  value.accountInfo.price := 10000;
  value.accountInfo.account_to_pay := 456;
  value.accountInfo.hashed_secret := TCrypto.DoSha256(TEncoding.UTF8.GetBytes('Generated fixture secret'));
  text := value.GetInfoText(Cardinal(0));
  AssertTrue(Pos('Atomic Coin Swap', text) > 0);
  AssertTrue(Pos(TAccountComp.AccountNumberToAccountTxtNumber(456), text) > 0);
  AssertTrue(Pos(value.accountInfo.hashed_secret.ToHexaString, text) > 0);
  AssertTrue(Pos('"coin_swap"', text) > 0);
end;

procedure TWalletInfoTests.MultioperationDetailsIncludeAllParticipants;
var
  operation: TOperationResume;
  text: String;
begin
  operation := CT_TOperationResume_NUL;
  operation.valid := true;
  operation.isMultiOperation := true;
  SetLength(operation.Senders, 1);
  SetLength(operation.Receivers, 1);
  SetLength(operation.Changers, 1);
  operation.Senders[0].Account := 123;
  operation.Receivers[0].Account := 456;
  operation.Changers[0].Account := 789;
  text := operation.GetInfoText(Cardinal(0));
  AssertTrue(Pos('Multioperation: 1 senders, 1 receivers, 1 account changes', text) > 0);
  AssertTrue(Pos('OPERATION JSON:', text) > 0);
  AssertTrue(Pos('"senders"', text) > 0);
  AssertTrue(Pos('"receivers"', text) > 0);
  AssertTrue(Pos('"changers"', text) > 0);
  AssertTrue(Pos('789', text) > 0);
end;

initialization
  RegisterTest(TWalletInfoTests);
end.
