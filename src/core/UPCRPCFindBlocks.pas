unit UPCRPCFindBlocks;

{ Copyright (c) 2020 by PascalCoin developers, orignal code by Albert Molina

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
  TRPCFindBlocks = Class
  private
  public
    class function FindBlocks(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
  End;

implementation

uses UPCDataTypes;

{ TRPCFindBlocks }

class function TRPCFindBlocks.FindBlocks(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;


{  RPC "findblocks"
### findblocks
Find blocks by name/type and returns them as an array of "Block Object"

##### Params
- `payload` : String - Name to search
- `payloadsearchtype` : String - One of those values
  - `exact` :
  - `startswith` : (DEFAULT OPTION)
  - `not-startswith` :
  - `contains` :
  - `not-contains` :
  - `endswith` :
  - `not-endswith` :
- `enc_pubkey` or `b58_pubkey` : HEXASTRING or String - Will return blocks with this public key.
- `start` : Integer - Start block (by default, 0)
- `end` : Integer - End block (by default -1, equals to "no limit")
- `max` : Integer - Max of accounts returned in array (by default, 100)

}

type
  TSearchBlockPayloadType = (st_exact, st_startswith, st_contains, st_endswith, st_not_startswith, st_not_contains, st_not_endswith);

  function _SearchValidPayload(const ASearch : String; const APayload : String; ASearchType : TSearchBlockPayloadType) : Boolean;
  var i : Integer;
  begin
    if (ASearch.Length=0) then Exit(True); // If nothing to search, allways TRUE
    // Here we know that ASearchName has a value
    if (APayload.Length=0) then Exit(False); // If account has NO NAME, allways FALSE
    if (ASearchType=st_exact) then begin
      Exit( APayload.Equals(ASearch) );  // Must match
    end;

    i := APayload.IndexOf(ASearch);
    Result :=
      ((i=0) and (ASearchType in [st_startswith])) // found at first position
      or
      ((i>=0) and (ASearchType in [st_contains])) // found in any pos
      or
      ((i=(APayload.Length-1)) and (ASearchType in [st_endswith])) // found at last position
      or
      ((i<0) and (ASearchType in [st_not_startswith, st_not_contains, st_not_endswith])) // not found and must not contain in any pos
      or
      ((i>=1) and (ASearchType in [st_not_startswith])) // not found at first position
      or
      ((i<(APayload.Length-1)) and (ASearchType in [st_not_endswith])); // not found at last position
  end;

var
  LPayload : String;
  LSearchByPayloadType : TSearchBlockPayloadType;
  LSearchByPubkey : Boolean;
  LPubKey : TAccountKey;

  function _IsValidBlock(const ABlock : TOperationBlock) : Boolean;
  begin
    if (Not _SearchValidPayload(LPayload,ABlock.block_payload.ToString,LSearchByPayloadType)) then Exit(False);
    if (LSearchByPubkey) then begin
      if Not (TAccountComp.EqualAccountKeys(LPubKey,ABlock.account_key)) then Exit(False);
    end;

    Result := True;
  end;

var
  LString : String;
  LAccountNumber : Integer;
  LRaw : TRawBytes;
  LStart, LEnd, LMax : Integer;
  LBlock : TOperationBlock;

  i : Integer;
  LErrors : String;
  LOutput : TPCJSONArray;
  LStartsWith : TOrderedRawList;
begin
  // Get Parameters
  Result := False;
  LPayload := LowerCase(AInputParams.AsString('payload', '')); // Convert to lowercase...
  if AInputParams.IndexOfName('payloadsearchtype')>=0 then begin
    LString := AInputParams.AsString('payloadsearchtype','');
    if (AnsiSameStr(LString,'exact')) then LSearchByPayloadType := st_exact
    else if (AnsiSameStr(LString,'startswith')) then LSearchByPayloadType := st_startswith
    else if (AnsiSameStr(LString,'not-startswith')) then LSearchByPayloadType := st_not_startswith
    else if (AnsiSameStr(LString,'contains')) then LSearchByPayloadType := st_contains
    else if (AnsiSameStr(LString,'not-contains')) then LSearchByPayloadType := st_not_contains
    else if (AnsiSameStr(LString,'endswith')) then LSearchByPayloadType := st_endswith
    else if (AnsiSameStr(LString,'not-endswith')) then LSearchByPayloadType := st_not_endswith
    else begin
      AErrorNum := CT_RPC_ErrNum_InvalidData;
      AErrorDesc := Format('Invalid "payloadsearchtype" value: "%s"',[LString]);
      Exit(False);
    end;
  end else begin
    LSearchByPayloadType := st_startswith;
  end;
  LStart := AInputParams.AsInteger('start', 0);
  LMax := AInputParams.AsInteger('max', 100);
  LEnd := AInputParams.AsInteger('end', -1);

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

  if (LEnd<0) or (LEnd>=ASender.Node.Bank.SafeBox.BlocksCount) then begin
    LEnd := ASender.Node.Bank.SafeBox.BlocksCount - 1;
  end;

  // Declare return result (empty by default)
  LOutput := AJSONResponse.GetAsArray('result');

  // Search by PubKey (if provided)
  If TPascalCoinJSONComp.CapturePubKey(AInputParams, '',LPubKey,LErrors) then begin
    LSearchByPubkey := True;
  end else LSearchByPubkey := False;
    //
  i := LStart;
  while (Not ASender.Terminated) And (i < LEnd) do begin
    LBlock := ASender.Node.Bank.SafeBox.GetBlockInfo(i);
    if (_IsValidBlock(LBlock)) then begin
      TPascalCoinJSONComp.FillBlockObject(i,ASender.Node,LOutput.GetAsObject(LOutput.Count));
      if LOutput.Count>=LMax then break;
    end;
    inc(i);
  end;
  Result := True;
end;

initialization
  TRPCProcess.RegisterProcessMethod('findblocks',TRPCFindBlocks.FindBlocks);
finalization
  TRPCProcess.UnregisterProcessMethod('findblocks');
end.
