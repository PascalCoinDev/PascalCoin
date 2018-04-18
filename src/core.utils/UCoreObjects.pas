unit UCoreObjects;

{ Copyright (c) 2018 by PascalCoin Project

  Contains common types for Core module.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: unit creator
}

{$mode delphi}

interface

uses
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain, UCommon,
  Generics.Collections, Generics.Defaults;

type

  { TKeySummary }

  TKeySummary = record
    Key : TAccountKey;
    TotalPASC : UInt64;
    TotalPASA : Cardinal;
    Accounts : TArray<TAccount>;  // TODO: change to PAccount later (optimized)
  end;

  { TUserSummary }

  TUserSummary = record
    Keys : TArray<TAccountKey>;
    Accounts : TArray<TAccount>;
    TotalPASC : UInt64;
    TotalPASA : Cardinal;
    Items : TArray<TKeySummary>;
  end;
  PUserSummary = ^TUserSummary;


const
  TKeySummary_Nil : TKeySummary = (
    Key : (EC_OpenSSL_NID:0;x:'';y:'');
    TotalPASC : 0;
    TotalPASA : 0;
    Accounts : nil;
  );

  TUserSummary_Nil : TUserSummary = (
    Keys : nil;
    Accounts : nil;
    TotalPASC : 0;
    TotalPASA : 0;
    Items : nil;
  );

implementation
end.

