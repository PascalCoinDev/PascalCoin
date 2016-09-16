unit UConst;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Uses ssl_const;

Const
  CT_Genesis_Magic_String_For_Old_Block_Hash :
    AnsiString =
    '(c) Albert Molina - Genesis block at same time than BitCoin Block 424720 Hash 000000000000000001cc41ff7846264718ef0a15f97f532a98277bd5f6820b89';

  CT_Zero_Block_Proof_of_work_in_Hexa =
    '00000003A29C32E84A539ADE24397D41D30116A6FAFEC17B7D9CED68A4238C92';


  CT_NetServer_Port = 4004;
  CT_AccountsPerBlock = 5;

  CT_NewLineSecondsAvg: Cardinal = 300; // 60*5=300 seconds -> 5 minutes avg
    //   -> 1 day = 86400 seconds -> 1 year = 31536000 seconds (aprox)
    //   Each year = 105120 new blocks (aprox)
    //   -> *5 accounts per block = 525600 new accounts each year (aprox)

  CT_FirstReward: UInt64 = 1000000; // 4 decimals... First reward = 100,0000
  CT_MinReward: UInt64 = 10000; // 4 decimals... Min reward = 1,0000
  CT_NewLineRewardDecrease: Cardinal = 420480; // Avg 4 year

  CT_WaitNewBlocksBeforeTransaction = 100;

  CT_RecoverFoundsWaitInactiveCount = 420480;  // After 4 years... if an account has no operations, money will be a reward for a miner!

  CT_MaxTransactionAmount = 1000000000000;
  CT_MaxTransactionFee = 100000000;
  CT_MaxWalletAmount = 10000000000000;
  //
  CT_MinCompactTarget: Cardinal = $19000000; // First compact target of block 0

  CT_CalcNewTargetBlocksAverage: Cardinal = 100;
  CT_MaxBlock : Cardinal = $FFFFFFFF;

  CT_MaxPayloadSize = 255; // Max payload size in bytes
  CT_MaxSecondsDifferenceOfNetworkNodes = 180; // 3 minutes. If a Node has a +- value difference, will be blacklisted

  CT_MaxServersConnected = 5;

  CT_MaxClientsConnected = 100;

  CT_BankToDiskEveryNBlocks = 1000;

  CT_Default_EC_OpenSSL_NID = NID_secp256k1;

  CT_BlockChain_Protocol_Version: Word = $0001; // Version 1
  CT_BlockChain_Protocol_Available: Word = $0000;

  // Build 1.0.4 - introducing NetProtocol versioning:
  CT_NetProtocol_Version: Word = $0001;
  // IMPORTANT NOTE!!!
  // NetProtocol_Available MUST BE allways >= NetProtocol_version
  CT_NetProtocol_Available: Word = $0002;  // Remember, >= NetProtocol_version !!!

  CT_MagicIdentificator: AnsiString = 'PascalCoin'; //

  // Value of Operations type in Protocol 1
  CT_Op_Transaction = $01;
  CT_Op_Changekey = $02;
  CT_Op_Recover = $03;

  CT_ClientAppVersion : AnsiString = '1.0.4';

  CT_Discover_IPs =  'bpascal1.dynamic-dns.net;bpascal2.dynamic-dns.net;pascalcoin1.ddns.net;pascalcoin2.ddns.net;pascalcoin1.dynamic-dns.net;pascalcoin1.dns1.us';

  CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');

implementation

end.
