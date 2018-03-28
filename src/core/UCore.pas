unit UCore;

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
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain,
  Generics.Collections, Generics.Defaults;

type

TAccountKeyComparer = class (TComparer<TAccountKey>)
  function Compare(constref ALeft, ARight: T): Integer; override;
  class function DoCompare(constref ALeft, ARight : TAccountKey) : Integer; inline;
end;

TAccountKeyEqualityComparer = class(TEqualityComparer<TAccountKey>)
  public
    function Equals(constref ALeft, ARight: TAccountKey): Boolean; override;
    function GetHashCode(constref AValue: TAccountKey): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccountKey): Boolean;
    class function CalcHashCode(constref AValue: TAccountKey): UInt32;
end;

TAccountHelper = record helper for TAccount
  function GetAccountString : AnsiString;
  function GetInfoText(const ABank : TPCBank) : utf8string;
end;

TOperationResumeHelper = record helper for TOperationResume
  function GetPrintableOPHASH : AnsiString;
  function GetInfoText(const ABank : TPCBank) : utf8string;
end;

implementation

uses
  UCommon, UMemory;

{ TAccountKeyComparer }

function TAccountKeyComparer.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight);
end;

class function TAccountKeyComparer.DoCompare(constref ALeft, ARight : TAccountKey) : Integer;
begin
  Result := BinStrComp(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BinStrComp(ALeft.y, ARight.y);
end;

{ TAccountKeyEqualityComparer }

function TAccountKeyEqualityComparer.Equals(constref ALeft, ARight: TAccountKey): Boolean;
begin
  Result := TAccountKeyEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountKeyEqualityComparer.GetHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TAccountKeyEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountKeyEqualityComparer.AreEqual(constref ALeft, ARight: TAccountKey): Boolean;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight) = 0;
end;

class function TAccountKeyEqualityComparer.CalcHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TEqualityComparer<AnsiString>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x + AValue.y  );
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString : AnsiString;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetInfoText(const ABank : TPCBank) : utf8string;
var
  builder : TStrings;
  GC : TDisposables;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.name<>'','Name: '+Self.name,'') ,Self.account_type]));
   builder.Append('');
   builder.Append(Format('Current balance: %s',[TAccountComp.FormatMoney(Self.balance)]));
   builder.Append('');
   builder.Append(Format('Updated on block: %d  (%d blocks ago)',[Self.updated_block, ABank.BlocksCount-Self.updated_block]));
   builder.Append(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(Self.accountInfo.accountKey.EC_OpenSSL_NID)]));
   builder.Append(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(Self.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(Self.accountInfo) then begin
     builder.Append('');
     builder.Append('** Account is for sale: **');
     builder.Append(Format('Price: %s',[TAccountComp.FormatMoney(Self.accountInfo.price)]));
     builder.Append(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(Self.accountInfo) then begin
       builder.Append('');
       builder.Append('** Private sale **');
       builder.Append(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey)]));
       builder.Append('');
      if TAccountComp.IsAccountLocked(Self.accountInfo, ABank.BlocksCount) then begin
         builder.Append(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount,Self.accountInfo.locked_until_block - ABank.BlocksCount]));
      end else begin
         builder.Append(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount]));
      end;
    end;
  end;
  Result :=  builder.Text;
end;

{ TOperationResumeHelper }

function TOperationResumeHelper.GetPrintableOPHASH : AnsiString;
begin
  Result := TCrypto.ToHexaString(Self.OperationHash);
end;

function TOperationResumeHelper.GetInfoText(const ABank : TPCBank) : utf8string;
var
  builder : TStrings;
  GC : TDisposables;
begin
  If (not Self.valid) then exit;
  builder := GC.AddObject(TStringList.Create) as TStrings;
  If Self.Block < ABank.BlocksCount then
    if (Self.NOpInsideBlock>=0) then begin
      builder.Add(Format('Block: %d/%d',[Self.Block,Self.NOpInsideBlock]))
    end else begin
      builder.Add(Format('Block: %d',[Self.Block]))
    end
  else builder.Add('** Pending operation not included on blockchain **');
  builder.Add(Format('%s',[Self.OperationTxt]));
  builder.Add(Format('OpType:%d Subtype:%d',[Self.OpType,Self.OpSubtype]));
  builder.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(Self.OperationHash)]));
  If (Self.OperationHash_OLD<>'') then begin
    builder.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  end;
  if (Self.OriginalPayload<>'') then begin
    builder.Add(Format('Payload length:%d',[length(Self.OriginalPayload)]));
    If Self.PrintablePayload<>'' then begin
      builder.Add(Format('Payload (human): %s',[Self.PrintablePayload]));
    end;
    builder.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(Self.OriginalPayload)]));
  end;
  If Self.Balance>=0 then begin
    builder.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(Self.Balance)]));
  end;
  Result := builder.Text;
end;

end.

