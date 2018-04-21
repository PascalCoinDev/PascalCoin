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

  { TBalanceSummary }

  TBalanceSummary = record
    TotalPASC : UInt64;
    TotalPASA : Cardinal;
  end;


const
  CT_BalanceSummary_Nil : TBalanceSummary = (
    TotalPASC : 0;
    TotalPASA : 0;
  );

implementation
end.

