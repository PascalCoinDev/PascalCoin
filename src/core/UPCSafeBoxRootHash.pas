unit UPCSafeBoxRootHash;

{ Copyright (c) 2019 by Albert Molina

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com> author of PIP-0030 (2019)

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


{ This unit implements the PIP-0030 proposed by Herman Schoenfeld for the Safebox
  https://github.com/PascalCoin/PascalCoin/blob/master/PIP/PIP-0030.md


  Is based in:
  Each "Account segment" is stored in a RAW memory buffer of type TBytesBuffer
  Each "Account segment" is a 32 bytes value, that contains a SHA256 of the
  information contained in a TBlockAccount:
    SHA256 of (
      TBlockAccount.blockchainInfo : TOperationBlock;
      TBlockAccount.accounts : Array[0..4] of TAccount;
    )
  On current PascalCoin source code, this "Account Segment" hash is stored
  in a TBytesBuffer where each 32 bytes are a iIndex of the "Account Segment" hash
  Example:
    - Number of "Account segments" = 1000 (that means 1000 blocks and 5000 accounts)
    - TBytesBuffer size = 32 * 1000 = 32000 bytes
    - index of "Account segment" position:
      - Position 0 -> 0*32 = 0
      - Position 123 -> 123 * 32 = 3936
      - Position 1002 -> Out of range (max = 1000-1 = 999)

  Calling "TPCSafeboxRootHash.CalcSafeBoxRootHash" will obtain a single 32 bytes
  value as described at PIP that is the "SafeBoxRoot"

  Calling "TPCSafeboxRootHash.GetProof" will return an array of 32 bytes value
  that is the proof of each level that must be hashed. The 0-index is the hash
  of the "Account Segment" to get Proof, and the last item of the array will be
  the "SafeBoxRoot" value

  Example:
    Account Segments:  01  02  03  04  05  06  07  08  09  = 9 items
      Level 2 process:   11      12       13      14   09  = 5 items
      Level 3 process:       21               22       09  = 3 items
      Level 4 process:              31                 09  = 2 items
      Level 5 process:                        41           = 1 item = SafeBoxRoot

   The "GetProof" of account segment 03 will be: 03 04 11 22 09 41
     - Note that first item 03 = same account to get proof of
     - Note that last item 41 = SafeBoxRoot

   The "GetProof" of account segment 09 will be: 09 09 09 09 31 41
     - Note that will repeat 09 value needed times one for each level (base + 3 levels)

  Calling "TPCSafeboxRootHash.CheckProof" will validate a previous "GetProof"
    - If the array is length = 1 then there was only 1 "Account Segment"
    - The array must be: length=1 or length>2 (length=2 not allowed)
      - Length 1=single account segment, so, equal to SafeBoxRoot
      - 2 accounts segments: Need 3 hashes: The base, sibling and SafeBoxRoot

  Speed tests:
    Made on 2019-05-21 with a Intel i5-4460 CPU
    - 315000 blocks (aka "Account segments") -> Aprox 3 years of PascalCoin Safebox (2016..2019)
      CalcSafeBoxRootHash -> 170 ms using OpenSSL library for 32 bits
    - 630000 Blocks -> Aprox 6 years of PascalCoin Safebox (2016..2022)
      CalcSafeBoxRootHash -> 350 ms using OpenSSL library for 32 bits

}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I config.inc}
uses
  Classes, SysUtils, UConst, UCrypto, SyncObjs, UThread, UBaseTypes,
  UPCOrderedLists, UPCDataTypes,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TProofLevels = Record
    Levels : Array of TRawBytes;
  End;

  TSafeboxHashCalcType = (sbh_Single_Sha256, sbh_Merkle_Root_Hash);

  { TBytesBuffer32Safebox is an extension of a TBytesBuffer that will
    automatically update and calc the SafeboxRootHash when
    SafeBoxHashCalcType = sbh_Merkle_Root_Hash

    This will increace speed because will only calculate modified
    blocks when used properly, mantaining integrity of the SafeBoxHash value

    When SafeBoxHashCalcType = sbh_Single_Sha256 (Default) then there is
    no change versus superclass type TBytesBuffer}

  TBytesBuffer32Safebox = Class(TBytesBuffer)
  private
    FNextLevelBytesBuffer : TBytesBuffer32Safebox;
    FSafeBoxHashCalcType: TSafeboxHashCalcType;
    procedure SetSafeBoxHashCalcType(const Value: TSafeboxHashCalcType);
  protected
    procedure NotifyUpdated(AStartPos, ACountBytes : Integer); override;
    procedure RedoNextLevelsForMerkleRootHash;
  public
    constructor Create(ADefaultIncrement : Integer); override;
    destructor Destroy; override;
    function GetSafeBoxHash : TRawBytes;
    property SafeBoxHashCalcType : TSafeboxHashCalcType read FSafeBoxHashCalcType write SetSafeBoxHashCalcType;
  End;

  TPCSafeboxRootHash = Class
    class function CalcNextLevelHash(ABlocksHashBuffer : TBytesBuffer; AStartIndex, ABlocksCount : Integer; ANextLevel : TBytesBuffer) : Boolean;
  public
    class function CalcSafeBoxRootHash(ABlocksHashBuffer : TBytesBuffer; AStartIndex, ABlocksCount : Integer) : TRawBytes; overload;
    class function CalcSafeBoxRootHash(ABlocksHashBuffer : TBytesBuffer) : TRawBytes; overload;
    class function GetProof(ABlocksHashBuffer : TBytesBuffer; ABlockIndex : Integer; var AProofLevels : TProofLevels) : Boolean;
    class function CheckProof(ABlockIndex, ATotalBlocks : Integer; const AProofLevels : TProofLevels) : Boolean;
  End;

implementation

{ TPCSafeboxRootHash }

class function TPCSafeboxRootHash.CalcNextLevelHash(ABlocksHashBuffer: TBytesBuffer; AStartIndex, ABlocksCount: Integer; ANextLevel: TBytesBuffer): Boolean;
var
  i, LLeft, LRight : Integer;
  LPByte : PByte;
  LSHA256  : TRawBytes;
  LTotalBlocks : Integer;
begin
  Assert((ABlocksHashBuffer.Length MOD 32)=0,'ABlocksHashBuffer invalid length ('+IntToStr(ABlocksHashBuffer.Length)+') not modulo 32 = 0');
  Assert((AStartIndex>=0) And (ABlocksCount>0) And (ABlocksHashBuffer.Length>0),Format('Invalid AStartIndex:%d or ACount:%d or Length:%d',[AStartIndex,ABlocksCount,ABlocksHashBuffer.Length]));

  LTotalBlocks := ABlocksHashBuffer.Length DIV 32;
  ANextLevel.Clear;

  if LTotalBlocks=1 then begin
    ANextLevel.CopyFrom(ABlocksHashBuffer);
    Exit(True);
  end;

  if (AStartIndex + ABlocksCount)>LTotalBlocks then Exit(False); // Invalid params

  for i := 0 to ((LTotalBlocks-1) DIV 2) do begin
    LLeft := i*64;
    LRight := (i+1)*64;
    LPByte := ABlocksHashBuffer.Memory;
    Inc(LPByte,LLeft);
    if (LRight>ABlocksHashBuffer.Length) then begin
      ANextLevel.Add(LPByte^,32);
    end else begin
      LSHA256 := TCrypto.DoSha256(PAnsiChar(LPByte),64);
      ANextLevel.Add(LSHA256);
    end;
  end;
  Result := True;
end;

class function TPCSafeboxRootHash.CalcSafeBoxRootHash(ABlocksHashBuffer: TBytesBuffer): TRawBytes;
begin
  Result := CalcSafeBoxRootHash(ABlocksHashBuffer,0,ABlocksHashBuffer.Length DIV 32);
end;

class function TPCSafeboxRootHash.CalcSafeBoxRootHash(ABlocksHashBuffer: TBytesBuffer; AStartIndex, ABlocksCount: Integer): TRawBytes;
  // PRE: The ABlocksHashBuffer has a length MODULO 32 = 0 and size > 0, because contains X blocks of 32 bytes each
  // each 32 bytes of ABlocksHashBuffer contains a SHA256 of TBlockAccount, extracted from TBlockAccount.block_hash

  function CalculateSafeBoxRootHash(APreviousLevelBuffer : TBytesBuffer) : TRawBytes;
    // PRE: APreviousLevelBuffer contains a set of 32 bytes
  var LNextLevel : TBytesBuffer;
    i, LLeft, LRight : Integer;
    LPByte : PByte;
    LSHA256  : TRawBytes;
    LTotalBlocks : Integer;
  begin
    LTotalBlocks := APreviousLevelBuffer.Length DIV 32;

    if (LTotalBlocks)=1 then begin
      SetLength(Result,32);
      Move(APreviousLevelBuffer.Memory^,Result[0],32);
      Exit;
    end;

    LNextLevel := TBytesBuffer.Create(APreviousLevelBuffer.Length DIV 2);
    try

      for i := 0 to ((LTotalBlocks-1) DIV 2) do begin
        LLeft := i*64;
        LRight := (i+1)*64;
        LPByte := APreviousLevelBuffer.Memory;
        Inc(LPByte,LLeft);
        if (LRight>APreviousLevelBuffer.Length) then begin
          LNextLevel.Add(LPByte^,32);
        end else begin
          LSHA256 := TCrypto.DoSha256(PAnsiChar(LPByte),64);
          LNextLevel.Add(LSHA256);
        end;
      end;

      Result := CalculateSafeBoxRootHash(LNextLevel)

    finally
      LNextLevel.Free;
    end;
  end;
var LHashBufferChunk : TBytesBuffer;
  LTotalBlocks : Integer;
  LPByte : PByte;
begin
  // Protection
  Assert((ABlocksHashBuffer.Length MOD 32)=0,'ABlocksHashBuffer invalid length ('+IntToStr(ABlocksHashBuffer.Length)+') not modulo 32 = 0');
  Assert((AStartIndex>=0) And (ABlocksCount>0) And (ABlocksHashBuffer.Length>0),Format('Invalid AStartIndex:%d or ACount:%d or Length:%d',[AStartIndex,ABlocksCount,ABlocksHashBuffer.Length]));

  LTotalBlocks := ABlocksHashBuffer.Length DIV 32;

  Assert((AStartIndex + ABlocksCount)<=LTotalBlocks,Format('Out of range AStartIndex:%d + ACount:%d > Blocks:%d',[AStartIndex,ABlocksCount,LTotalBlocks]));

  if (AStartIndex=0) And (ABlocksCount=LTotalBlocks) then begin
    Result := CalculateSafeBoxRootHash(ABlocksHashBuffer);
  end else begin
    LHashBufferChunk := TBytesBuffer.Create(LTotalBlocks*32);
    try
      LPByte := ABlocksHashBuffer.Memory;
      Inc(LPByte,32 * AStartIndex);
      LHashBufferChunk.Add(LPByte^, ABlocksCount*32);
      //
      Result := CalculateSafeBoxRootHash(LHashBufferChunk);
    finally
      LHashBufferChunk.Free;
    end;
  end;
end;

class function TPCSafeboxRootHash.CheckProof(ABlockIndex, ATotalBlocks: Integer; const AProofLevels: TProofLevels): Boolean;
var iLevel : Integer;
  LLevelItemsCount : Integer;
  LLevelItemIndex : Integer;
  LRawToHash,
  LPreviousHashedValue : TRawBytes;
begin
  // At least 1 level (single) or >2 levels where 0=leaf and Length-1 = RootHash
  if (Length(AProofLevels.Levels)=0) then Exit(False);
  if (Length(AProofLevels.Levels)=2) then Exit(False);


  Result := True;

  if (Length(AProofLevels.Levels)=1) then Exit(True); // If only 1 level, nothing to proof, is a single proof = True

  iLevel := 1;

  LLevelItemsCount := ATotalBlocks;
  LLevelItemIndex := ABlockIndex;
  SetLength(LRawToHash,32 * 2);
  LPreviousHashedValue := AProofLevels.Levels[0];
  while (iLevel<Length(AProofLevels.Levels)) do begin
    // Left or right?
    if (LLevelItemIndex MOD 2)=0 then begin
      // Even
      if (LLevelItemIndex+1<LLevelItemsCount) then begin
        Move(LPreviousHashedValue[0],LRawToHash[0],32);
        Move(AProofLevels.Levels[iLevel][0],LRawToHash[32],32);
        LPreviousHashedValue := TCrypto.DoSha256(LRawToHash);
      end
      else begin
        LPreviousHashedValue := Copy(LPreviousHashedValue);
      end;
    end else begin
      // Odd, always on right side
      Move(LPreviousHashedValue[0],LRawToHash[32],32);
      Move(AProofLevels.Levels[iLevel][0],LRawToHash[0],32);
      LPreviousHashedValue := TCrypto.DoSha256(LRawToHash);
    end;
    LLevelItemIndex := LLevelItemIndex DIV 2;
    LLevelItemsCount := ((LLevelItemsCount-1) DIV 2)+1;
    inc(iLevel);
  end;
  Result := TBaseType.Equals(LPreviousHashedValue,AProofLevels.Levels[High(AProofLevels.Levels)]);
end;

class function TPCSafeboxRootHash.GetProof(ABlocksHashBuffer: TBytesBuffer;
  ABlockIndex: Integer; var AProofLevels: TProofLevels): Boolean;
  // PRE: ABlockIndex is 0 indexed. Range 0..Total-1

  procedure AddToProofLevels(ABlockIndexToSave : Integer; const ABlocks : TBytesBuffer);
  var LPByte : PByte;
    LNewProof : TRawBytes;
  begin
    SetLength(LNewProof,32);
    LPByte := ABlocks.Memory;
    Inc(LPByte,ABlockIndexToSave * 32);
    Move(LPByte^,LNewProof[0],32);
    //
    SetLength(AProofLevels.Levels,Length(AProofLevels.Levels)+1);
    AProofLevels.Levels[High(AProofLevels.Levels)] := LNewProof;
  end;


  procedure GetLevelProof(APreviousLevelHashBuffer: TBytesBuffer; ALevelBlockIndex : Integer; var ALevels: TProofLevels);
    // PRE: At least we have 1 block

  var LTotalBlocks : Integer;
    LNextLevel : TBytesBuffer;
  begin
    LTotalBlocks := APreviousLevelHashBuffer.Length DIV 32;
    // Is top level?
    if LTotalBlocks=1 then begin
      // Include it at top
      AddToProofLevels(0, APreviousLevelHashBuffer);
      Exit;
    end;
    // Save current level
    // Even or Odd
    if (ALevelBlockIndex MOD 2)=0 then begin
      // Even = Left, put right one
      if ALevelBlockIndex+1<LTotalBlocks then begin
        AddToProofLevels(ALevelBlockIndex+1, APreviousLevelHashBuffer);
      end else begin
        // Last value, add itself
        AddToProofLevels(ALevelBlockIndex, APreviousLevelHashBuffer);
      end;
    end else begin
      // Odd = Right, put left one
      if (ALevelBlockIndex>0) then begin
        AddToProofLevels(ALevelBlockIndex-1, APreviousLevelHashBuffer);
      end else begin
        // First value, add itself
        AddToProofLevels(0, APreviousLevelHashBuffer);
      end;
    end;

    // Calculate next level
    LNextLevel := TBytesBuffer.Create(APreviousLevelHashBuffer.Length DIV 2);
    try
      CalcNextLevelHash(APreviousLevelHashBuffer,0,LTotalBlocks,LNextLevel);
      GetLevelProof(LNextLevel,(ALevelBlockIndex DIV 2),ALevels);
    finally
      LNextLevel.Free;
    end;
  end;

var LTotalBlocks : Integer;
begin
  // Protection
  Assert((ABlocksHashBuffer.Length MOD 32)=0,'ABlocksHashBuffer invalid length ('+IntToStr(ABlocksHashBuffer.Length)+') not modulo 32 = 0');
  // Init

  SetLength(AProofLevels.Levels,0);
  LTotalBlocks := ABlocksHashBuffer.Length DIV 32;
  Result := False;
  AProofLevels.Levels := Nil;
  if LTotalBlocks<=ABlockIndex then Exit(False);
  if LTotalBlocks=0 then Exit(False);
  // First
  Result := True;
  AddToProofLevels(ABlockIndex,ABlocksHashBuffer);
  if LTotalBlocks>1 then begin
    GetLevelProof(ABlocksHashBuffer,ABlockIndex,AProofLevels);
  end;
end;

{ TBytesBuffer32Safebox }

constructor TBytesBuffer32Safebox.Create(ADefaultIncrement: Integer);
begin
  FNextLevelBytesBuffer := Nil;
  FSafeBoxHashCalcType := sbh_Single_Sha256;
  inherited Create(ADefaultIncrement);
end;

destructor TBytesBuffer32Safebox.Destroy;
begin
  FreeAndNil(FNextLevelBytesBuffer);
  inherited;
end;

function TBytesBuffer32Safebox.GetSafeBoxHash: TRawBytes;
begin
  if (FSafeBoxHashCalcType = sbh_Single_Sha256) then begin
    if ((Self.Length MOD 32)=0) and (Self.Length>0) then begin
      Result := TCrypto.DoSha256(Self.Memory,Self.Length);
    end else begin
      Result := Nil;
    end;
  end else if (Self.Length=32) then begin
    System.SetLength(Result,32);
    Move(Self.Memory^,Result[0],32);
  end else if (Self.Length>32) and ((Self.Length MOD 32)=0) then begin
    if Not Assigned(FNextLevelBytesBuffer) then begin
      RedoNextLevelsForMerkleRootHash;
    end;
    Result := FNextLevelBytesBuffer.GetSafeBoxHash;
  end else begin
    Result := Nil;
  end;
end;

procedure TBytesBuffer32Safebox.NotifyUpdated(AStartPos, ACountBytes: Integer);
var LLevelItemIndex, LLevelItemsCount : Integer;
  LPByte : PByte;
  LSHA256 : TRawBytes;
begin
  inherited;
  if (FSafeBoxHashCalcType = sbh_Single_Sha256) or
    ((ACountBytes<>32) or ((AStartPos MOD 32)<>0)) or (Self.Length<64) or ((Self.Length MOD 32)<>0) then begin
    FreeAndNil(FNextLevelBytesBuffer);
  end else if Not Assigned(FNextLevelBytesBuffer) then begin
    // First time must "Redo"
    RedoNextLevelsForMerkleRootHash;
  end else begin
    LLevelItemIndex := AStartPos DIV 32;
    LLevelItemsCount := Self.Length DIV 32;
    LPByte := Self.Memory;
    inc(LPByte,AStartPos);

    // Left or right?
    if (LLevelItemIndex MOD 2)=0 then begin
      // Even, we are Left
      if (LLevelItemIndex+1<LLevelItemsCount) then begin
        LSHA256 := TCrypto.DoSha256(PAnsiChar(LPByte),64);
        FNextLevelBytesBuffer.Replace((AStartPos DIV 2),LSHA256);
      end
      else begin
        // No sheet on right, same value on next level
        FNextLevelBytesBuffer.Replace(AStartPos DIV 2,LPByte^,32);
      end;
    end else begin
      // Odd, is on right side
      Dec(LPByte,32);
      LSHA256 := TCrypto.DoSha256(PAnsiChar(LPByte),64);
      FNextLevelBytesBuffer.Replace(((AStartPos-32) DIV 2),LSHA256);
    end;
  end;
end;

procedure TBytesBuffer32Safebox.RedoNextLevelsForMerkleRootHash;
var i : Integer;
  LNextDefaultIncrement : Integer;
begin
  if (Self.Length<64) or ((Self.Length MOD 32)<>0) then begin
    FreeAndNil(FNextLevelBytesBuffer);
    Exit;
  end;
  if Not Assigned(FNextLevelBytesBuffer) then begin
    if (DefaultIncrement >= 64) then LNextDefaultIncrement := DefaultIncrement DIV 2
    else LNextDefaultIncrement := 32;
    FNextLevelBytesBuffer := TBytesBuffer32Safebox.Create(LNextDefaultIncrement);
    FNextLevelBytesBuffer.SafeBoxHashCalcType := Self.SafeBoxHashCalcType;
  end;
  for i := 0 to (((Self.Length+32) DIV 64)-1) do begin
    NotifyUpdated( (i*64), 32);
  end;
end;

procedure TBytesBuffer32Safebox.SetSafeBoxHashCalcType(const Value: TSafeboxHashCalcType);
begin
  if FSafeBoxHashCalcType=Value then Exit;
  FSafeBoxHashCalcType := Value;
  FreeAndNil(FNextLevelBytesBuffer);
end;

end.
