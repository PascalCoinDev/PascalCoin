unit UPCAccountsOrdenations;

{ Copyright (c) 2016-2021 by Albert Molina

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

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ./../config.inc}

uses Classes, SysUtils,
  UAbstractMem,
  UAbstractMemBTree,
  UAbstractBTree,
  UPCDataTypes, UBaseTypes,
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

type

  TCallReturnAccount = function(AAccountNumber : Integer; var AAccount : TAccount) : Boolean of object;

  TAccountsOrderedByUpdatedBlock = Class
  private
    type
      TAccounstByUpdatedBlockBTree = Class({$IFDEF USE_ABSTRACTMEM}TAbstractMemBTree{$ELSE}TMemoryBTree<Integer>{$ENDIF})
      protected
        FCallReturnAccount : TCallReturnAccount;
        FSearching_AccountNumber : Integer;
        FSearching_UpdatedBlock : Integer;
        function DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer; override;
      public
        function NodeDataToString(const AData : TAbstractMemPosition) : String; override;
      End;
  private
    var
    FBTree : TAccounstByUpdatedBlockBTree;
    {$IFDEF USE_ABSTRACTMEM}
    FAbstractMem : TAbstractMem;
    {$ENDIF}
  public
    constructor Create({$IFDEF USE_ABSTRACTMEM}AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; {$ENDIF}ACallReturnAccount : TCallReturnAccount);
    destructor Destroy; override;
    function First(var AAccountNumber : Integer) : Boolean;
    function Next(var AAccountNumber : Integer) : Boolean;
    function Count : Integer;
    function Update(const AAccountNumber, AOldUpdatedBlock, ANewUpdatedBlock : Integer) : Boolean;
  End;

implementation

Uses UPCAbstractMemAccounts;

{ TAccountsOrderedByUpdatedBlock }

function TAccountsOrderedByUpdatedBlock.Count: Integer;
begin
  Result := FBTree.Count;
end;

constructor TAccountsOrderedByUpdatedBlock.Create({$IFDEF USE_ABSTRACTMEM}AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; {$ENDIF}ACallReturnAccount : TCallReturnAccount);
begin
  {$IFDEF USE_ABSTRACTMEM}
  FAbstractMem := AAbstractMem;
  FBTree := TAccounstByUpdatedBlockBTree.Create(FAbstractMem,AInitialZone,False,31);
  {$ELSE}
  FBTree := TAccounstByUpdatedBlockBTree.Create(Nil,False,31);
  {$ENDIF}
  FBTree.FSearching_AccountNumber := -1;
  FBTree.FSearching_UpdatedBlock := 0;
  FBTree.FCallReturnAccount := ACallReturnAccount;
end;

destructor TAccountsOrderedByUpdatedBlock.Destroy;
begin
  FBTree.Free;
  inherited;
end;

function TAccountsOrderedByUpdatedBlock.First(var AAccountNumber : Integer) : Boolean;
begin
  FBTree.Lock;
  Try
    FBTree.FSearching_AccountNumber := -1;
    Result := FBTree.FindLowest(AAccountNumber);
  Finally
    FBTree.Unlock;
  End;
end;

function TAccountsOrderedByUpdatedBlock.Next(var AAccountNumber : Integer): Boolean;
begin
  FBTree.Lock;
  Try
    FBTree.FSearching_AccountNumber := -1;
    Result := FBTree.FindSuccessor(AAccountNumber,AAccountNumber);
  Finally
    FBTree.Unlock;
  End;
end;

function TAccountsOrderedByUpdatedBlock.Update(const AAccountNumber, AOldUpdatedBlock, ANewUpdatedBlock: Integer): Boolean;
var
  Lampos : TAbstractMemPosition;
  Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LAccount : TAccount;
  LPosition : TAbstractMemPosition;
  LiPos : Integer;
begin
  FBTree.Lock;
  Try
    FBTree.FSearching_AccountNumber := AAccountNumber;
    FBTree.FSearching_UpdatedBlock := AOldUpdatedBlock;
    FBTree.Delete(AAccountNumber);
    FBTree.FSearching_UpdatedBlock := ANewUpdatedBlock;
    FBTree.Add(AAccountNumber);
  Finally
    FBTree.Unlock;
  End;
  Result := True;
end;

{ TAccountsOrderedByUpdatedBlock.TAccounstByUpdatedBlockBTree }

function TAccountsOrderedByUpdatedBlock.TAccounstByUpdatedBlockBTree.DoCompareData(
  const ALeftData, ARightData: TAbstractMemPosition): Integer;
var LLeftAccount, LRightAccount : TAccount;
begin
  if (ALeftData = ARightData) then Exit(0);

  FCallReturnAccount(ARightData,LRightAccount);
  if ((FSearching_AccountNumber>=0) And (ALeftData=FSearching_AccountNumber)) then begin
    Result := FSearching_UpdatedBlock - LRightAccount.updated_on_block_active_mode;
  end else begin
    FCallReturnAccount(ALeftData,LLeftAccount);
    Result := LLeftAccount.updated_on_block_active_mode - LRightAccount.updated_on_block_active_mode;
  end;
  if Result=0 then Result := ALeftData - ARightData;
end;

function TAccountsOrderedByUpdatedBlock.TAccounstByUpdatedBlockBTree.NodeDataToString(
  const AData: TAbstractMemPosition): String;
var LAccount : TAccount;
begin
  if FCallReturnAccount(AData,LAccount) then begin
    Result := Format('(Acc:%d Upd:%d)',[LAccount.account,LAccount.updated_on_block_active_mode]);
  end else Result := Format('(Pos:%d not found)',[AData]);
end;

end.
