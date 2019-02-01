unit UCoreObjects;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

interface

uses
  UBaseTypes;

type

  { TExecuteOperationType }

  TExecuteOperationType = (omtAccount, omtSendPasc, omtChangeKey, omtTransferAccount, omtChangeAccountPrivateKey, omtAddKey, omtEnlistAccountForSale, omtDelistAccountFromSale, omtChangeInfo, omtBuyAccount);

  { TPayloadEncryptionMode }

  TPayloadEncryptionMode = (pemEncryptWithSender, pemEncryptWithRecipient, pemEncryptWithPassword, pemNotEncrypt);

  { TOperationSigningMode }

  TOperationSigningMode = (akaPrimary, akaSecondary);

  { TChangeKeyMode }

  TChangeKeyMode = (akaTransferAccountOwnership, akaChangeAccountPrivateKey);

  { TSendPASCMode }

  TSendPASCMode = (akaAllBalance, akaSpecifiedAmount);

  { TAccountSaleMode }
  TAccountSaleMode = (akaPublicSale, akaPrivateSale);


  { TBalanceSummary }

  TBalanceSummary = record
    TotalPASC : UInt64;
    TotalPASA : Cardinal;
  end;

  PBalanceSummary = ^TBalanceSummary;

  { TBlockSummary }

  TBlockSummary = record
    Block: cardinal;
    Timestamp: cardinal;
    BlockProtocolVersion,
    BlockProtocolAvailable: word;
    OperationsCount: integer;
    Volume: int64;
    Reward, Fee: int64;
    Target: cardinal;
    HashRateTargetKhs: int64;
    HashRateKhs: int64;
    MinerPayload: TRawBytes;
    PoW: TRawBytes;
    SafeBoxHash: TRawBytes;
    AccumulatedWork: UInt64;
    TimeAverage200: real;
    TimeAverage150: real;
    TimeAverage100: real;
    TimeAverage75: real;
    TimeAverage50: real;
    TimeAverage25: real;
    TimeAverage10: real;
  end;

const
  CT_BalanceSummary_Nil : TBalanceSummary = (
    TotalPASC : 0;
    TotalPASA : 0;
  );

  CT_BlockSummary_NUL: TBlockSummary = (
    Block: 0;
    Timestamp: 0;
    BlockProtocolVersion: 0;
    BlockProtocolAvailable: 0;
    OperationsCount: -1;
    Volume: -1;
    Reward: 0;
    Fee: 0;
    Target: 0;
    HashRateTargetKhs: 0;
    HashRateKhs: 0;
    MinerPayload: Nil;
    PoW: Nil;
    SafeBoxHash: Nil;
    AccumulatedWork: 0;
    TimeAverage200: 0;
    TimeAverage150: 0;
    TimeAverage100: 0;
    TimeAverage75: 0;
    TimeAverage50: 0;
    TimeAverage25: 0;
    TimeAverage10: 0
  );

implementation

end.

