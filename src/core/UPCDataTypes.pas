unit UPCDataTypes;

{ Copyright (c) 2016-2019 by Albert Molina

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

uses
  Classes, SysUtils, UBaseTypes, UConst;

type

  TECPrivateKeyInfo = record
    EC_OpenSSL_NID : Word;
    EC_KEY_Ptr : Pointer;    // Used when compiled with $DEFINE Use_OpenSSL
    RAW_PrivKey : TRawBytes; // Used when compiled with $DEFINE Use_CryptoLib4Pascal
  end;

  { TECDSA_Public is a public key information }
  TECDSA_Public = record
     EC_OpenSSL_NID : Word;
     x: TRawBytes;
     y: TRawBytes;
     //
     procedure Clear;
     function GetSerializedLength : Integer;
     function ToSerialized : TBytes; overload;
     procedure ToSerialized(const AStream : TStream); overload;
     function FromSerialized(const ASerialized : TBytes) : Boolean; overload;
     function FromSerialized(const AStream : TStream) : Boolean; overload;
     function LoadFromTBytes(const ABytes : TBytes; var AStartIndex : Integer) : Boolean;
     function IsEqualTo(const ACompareTo : TECDSA_Public) : Boolean;
  end;

  { TECDSA_Public_Raw is a TECDSA_Public stored in a single TRawBytes
    Information will be:
    2 bytes for EC_OpenSSL_NID ++ 2 byte for x length ++ x RAW data ++ 2 byte for y length ++ y RAW data }
  TECDSA_Public_Raw = TRawBytes;

  { TECDSA_Public_Helper }

  TECDSA_Public_Helper = record helper for TECDSA_Public
     function ToRaw(var OECDSA_Public_Raw : TECDSA_Public_Raw) : Boolean;
     function FromRaw(const AECDSA_Public_Raw : TECDSA_Public_Raw) : Boolean;
  end;

  { TECDSA_SIG is a Eliptic Curve signature }
  TECDSA_SIG = record
     r: TRawBytes;
     s: TRawBytes;
  end;
  PECDSA_Public = ^TECDSA_Public; // Pointer to a TECDSA_SIG



  TAccountKey = TECDSA_Public;
  PAccountKey = ^TAccountKey;

  TAccountState = (as_Unknown, as_Normal, as_ForSale, as_ForAtomicAccountSwap, as_ForAtomicCoinSwap);

  { TAccountInfo }

  TAccountInfo = Record
    state : TAccountState;
    accountKey: TAccountKey;
    // Trade info, only when state=as_ForSale
    locked_until_block : Cardinal; // 0 = Not locked
    price : UInt64;                // 0 = invalid price
    account_to_pay : Cardinal;     // <> itself
    new_publicKey : TAccountKey;
    hashed_secret : TRawBytes;     // Hashed Secret for AtomicSwaps
    //
    procedure Clear;
    function ToSerialized : TBytes;
    function FromSerialized(const ASerialized : TBytes) : Boolean;
    function LoadFromTBytes(const ABytes : TBytes; var AStartIndex : Integer) : Boolean;
  end;

  TOperationBlock = Record
    block: Cardinal;
    account_key: TAccountKey;
    reward: UInt64;
    fee: UInt64;
    protocol_version: Word;     // Protocol version
    protocol_available: Word;   // Used to upgrade protocol
    timestamp: Cardinal;        // Timestamp creation
    compact_target: Cardinal;   // Target in compact form
    nonce: Cardinal;            // Random value to generate a new P-o-W
    block_payload : TRawBytes;  // RAW Payload that a miner can include to a blockchain
    initial_safe_box_hash: TRawBytes; // RAW Safe Box Hash value (32 bytes, it's a Sha256)
    operations_hash: TRawBytes; // RAW sha256 (32 bytes) of Operations
    proof_of_work: TRawBytes;   // RAW 32 bytes
    previous_proof_of_work: TRawBytes; // RAW 32 bytes
  end;

  { TAccount }

  TAccount = Record
    account: Cardinal;        // FIXED value. Account number
    accountInfo : TAccountInfo;
    balance: UInt64;          // Balance, always >= 0
    updated_on_block_passive_mode: Cardinal; // Number of block where was updated (active or passive mode)
    updated_on_block_active_mode: Cardinal; // Number of block where was used (active mode only)
    n_operation: Cardinal;    // count number of owner operations (when receive, this is not updated)
    name : TRawBytes;         // Protocol 2. Unique name
    account_type : Word;      // Protocol 2. Layer 2 use case
    account_data : TRawBytes; // Protocol 5. PIP-0024 RAW data information
    account_seal : TRawBytes;  // Protocol 5. PIP-0029 seal of data changes
    procedure Clear;
    function GetLastUpdatedBlock : Cardinal;
  End;
  PAccount = ^TAccount;

  TBlockAccount = Record
    blockchainInfo : TOperationBlock;
    accounts : Array[0..CT_AccountsPerBlock-1] of TAccount;
    block_hash: TRawBytes;   // Calculated on every block change (on create and on accounts updated)
    accumulatedWork : UInt64; // Accumulated work (previous + target) this value can be calculated.
  end;

  { TPCSafeBoxHeader }

  TPCSafeBoxHeader = Record
    protocol : Word;
    startBlock,
    endBlock,
    blocksCount : Cardinal;
    safeBoxHash : TRawBytes;
    function GetSavedBlocksCount : Integer;
    function IsAChunk : Boolean;
    function IsFullSafebox : Boolean;
    function ContainsFirstBlock : Boolean;
    function ContainsLastBlock : Boolean;
    function ToString : String;
  end;



const
  CT_AccountInfo_NUL : TAccountInfo = (state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);hashed_secret:Nil);
  CT_Account_NUL : TAccount = (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil);
  CT_BlockAccount_NUL : TBlockAccount = (
    blockchainInfo:(block:0;account_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);reward:0;fee:0;protocol_version:0;
    protocol_available:0;timestamp:0;compact_target:0;nonce:0;block_payload:Nil;initial_safe_box_hash:Nil;operations_hash:Nil;proof_of_work:Nil;previous_proof_of_work:Nil);
    accounts:(
    (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil),
    (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil),
    (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil),
    (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil),
    (account:0;accountInfo:(state:as_Unknown;accountKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);locked_until_block:0;price:0;account_to_pay:0;new_publicKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil));balance:0;updated_on_block_passive_mode:0;updated_on_block_active_mode:0;n_operation:0;name:Nil;account_type:0;account_data:Nil;account_seal:Nil)
    );
    block_hash:Nil;
    accumulatedWork:0);
  CT_PCSafeBoxHeader_NUL : TPCSafeBoxHeader = (protocol:0;startBlock:0;endBlock:0;blocksCount:0;safeBoxHash:Nil);


implementation

{ TECDSA_Public }

procedure TECDSA_Public.Clear;
begin
  Self.EC_OpenSSL_NID:=0;
  Self.x := Nil;
  Self.y := Nil;
end;

function TECDSA_Public.ToSerialized: TBytes;
var LPos : Integer;
begin
  SetLength(Result,2 + 2 + Length(Self.x) + 2 + Length(Self.y));
  Move(Self.EC_OpenSSL_NID,Result[0],2);
  LPos := 2;
  Self.x.SaveInsideTBytes(Result,LPos);
  Self.y.SaveInsideTBytes(Result,LPos);
end;

function TECDSA_Public.FromSerialized(const ASerialized: TBytes): Boolean;
var i : Integer;
begin
  i := 0;
  Result := LoadFromTBytes(ASerialized,i);
end;

function TECDSA_Public.FromSerialized(const AStream: TStream): Boolean;
begin
  if AStream.Read(Self.EC_OpenSSL_NID,2)<>2 then Exit(False);
  if Self.x.FromSerialized(AStream)<0 then Exit(False);
  if Self.y.FromSerialized(AStream)<0 then Exit(False);
  Result := True;
end;

function TECDSA_Public.GetSerializedLength: Integer;
begin
  Result := 2 + Self.x.GetSerializedLength + Self.y.GetSerializedLength;
end;

function TECDSA_Public.IsEqualTo(const ACompareTo: TECDSA_Public): Boolean;
begin
  Result := (Self.EC_OpenSSL_NID = ACompareTo.EC_OpenSSL_NID) and
    (Self.x.IsEqualTo(ACompareTo.x)) and (Self.y.IsEqualTo(ACompareTo.y));
end;

function TECDSA_Public.LoadFromTBytes(const ABytes: TBytes; var AStartIndex: Integer): Boolean;
begin
  Self.Clear;
  if (AStartIndex + 2 + 2 + 2 > Length(ABytes)) then Exit(False);
  Move(ABytes[AStartIndex],Self.EC_OpenSSL_NID,2);
  inc(AStartIndex,2);
  if Not Self.x.LoadFromTBytes(ABytes,AStartIndex) then Exit(False);
  if Not Self.y.LoadFromTBytes(ABytes,AStartIndex) then Exit(False);
end;

procedure TECDSA_Public.ToSerialized(const AStream: TStream);
begin
  AStream.Write(Self.EC_OpenSSL_NID,2);
  Self.x.ToSerialized(AStream);
  Self.y.ToSerialized(AStream);
end;

{ TAccountInfo }

procedure TAccountInfo.Clear;
begin
  Self.state := as_Unknown;
  Self.accountKey.Clear;
  Self.locked_until_block := 0;
  Self.price := 0;
  Self.account_to_pay := 0;
  Self.new_publicKey.Clear;
  Self.hashed_secret := Nil;
end;

function TAccountInfo.ToSerialized: TBytes;
var w : Word;
  LtotalLenght : Integer;
  Lacc_serialized, Lnew_serialized, Lsecret_serialized : TBytes;
begin
  Lacc_serialized := Self.accountKey.ToSerialized;
  case Self.state of
    as_Unknown: begin
      Result := Nil;
      Exit;
    end;
    as_Normal: Begin
      Result := Lacc_serialized;
      Exit;
    End;
    as_ForSale, as_ForAtomicAccountSwap, as_ForAtomicCoinSwap: begin
      Lnew_serialized := Self.new_publicKey.ToSerialized;
      Lsecret_serialized := Self.hashed_secret.ToSerialized;
      LtotalLenght := 2 + Length(Lacc_serialized) + 4 + 8 + 4 + Length(Lnew_serialized);
      case Self.state of
        as_ForSale: w := CT_AccountInfo_ForSale;
        as_ForAtomicAccountSwap: begin
          w := CT_AccountInfo_ForAccountSwap;
          inc(LtotalLenght,Length(Lsecret_serialized));
        end;
        as_ForAtomicCoinSwap: begin
          w := CT_AccountInfo_ForCoinSwap;
          inc(LtotalLenght,Length(Lsecret_serialized));
        end;
      end;
      SetLength(Result, LtotalLenght);
      Move(w,Result[0],2);
      Move(Lacc_serialized[0],Result[2],Length(Lacc_serialized));
      //
      Move(Self.locked_until_block,Result[2+Length(Lacc_serialized)],4);
      Move(Self.price,Result[2+Length(Lacc_serialized)+4],8);
      Move(Self.account_to_pay,Result[2+Length(Lacc_serialized)+4+8],4);
      //
      Move(Lnew_serialized[0],Result[2+Length(Lacc_serialized)+4+8+4], Length(Lnew_serialized));
      if (Self.state in [as_ForAtomicAccountSwap,as_ForAtomicCoinSwap]) then begin
        Move(Lsecret_serialized[0],Result[2+Length(Lacc_serialized)+4+8+4+Length(Lnew_serialized)],Length(Lsecret_serialized));
      end;
    end;
  end;
end;

function TAccountInfo.FromSerialized(const ASerialized: TBytes): Boolean;
var i : Integer;
begin
  i := 0;
  Result := LoadFromTBytes(ASerialized,i);
end;

function TAccountInfo.LoadFromTBytes(const ABytes: TBytes; var AStartIndex: Integer): Boolean;
var w : Word;
begin
  Self.Clear;
  if (AStartIndex + 2 > Length(ABytes)) then Exit(False);
  Move(ABytes[AStartIndex],w,2);
  case w of
    CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : Begin
      Self.state := as_Normal;
      Result := Self.accountKey.LoadFromTBytes(ABytes,AStartIndex);
    End;
    CT_AccountInfo_ForSale, CT_AccountInfo_ForAccountSwap, CT_AccountInfo_ForCoinSwap : Begin
      inc(AStartIndex,2);
      if Not Self.accountKey.LoadFromTBytes(ABytes,AStartIndex) then Exit(False);
      if (AStartIndex + 4 + 8 + 4 > Length(ABytes)) then Exit(False);
      Move(ABytes[AStartIndex],Self.locked_until_block,4);
      Move(ABytes[AStartIndex + 4],Self.price,8);
      Move(ABytes[AStartIndex + 4 + 8],Self.account_to_pay,4);
      inc(AStartIndex, 4+8+4);
      if Not Self.new_publicKey.LoadFromTBytes(ABytes,AStartIndex) then Exit(False);
      if Self.state in [as_ForAtomicAccountSwap,as_ForAtomicCoinSwap] then begin
        if Not Self.hashed_secret.LoadFromTBytes(ABytes,AStartIndex) then Exit(False);
      end;
      Result:=True;
    End;
  else
    raise Exception.Create('DEVELOP ERROR 20200318-1');
  end;
end;

{ TECDSA_Public_Helper }

function TECDSA_Public_Helper.ToRaw(var OECDSA_Public_Raw: TECDSA_Public_Raw): Boolean;
var l_length : Integer;
  l_bs : TBytesStream;
  l_w : Word;
begin
  if (Length(self.x)>65536) or (Length(self.y)>65536) then begin
    Result := False;
    SetLength(OECDSA_Public_Raw,0);
  end else begin
    l_length := 6 + Length(self.x) + Length(self.y);
    SetLength(OECDSA_Public_Raw,l_length);
    l_bs := TBytesStream.Create;
    try
      l_bs.Write(self.EC_OpenSSL_NID,2); // Write 2 bytes little endian with EC_OpenSSL_NID
      l_w := Length(self.x);
      l_bs.Write(l_w,2);                 // Write 2 bytes little endian for x length
      l_bs.WriteBuffer(self.x[Low(self.x)],l_w); // Write l_w bytes from x as RAW
      l_w := Length(self.y);
      l_bs.Write(l_w,2);                 // Write 2 bytes little endian for y length
      l_bs.WriteBuffer(self.y[Low(self.x)],l_w); // Write l_w bytes from y as RAW
      // Save to OECDSA_Public_Raw
      l_bs.Position:=0;
      l_bs.ReadBuffer(OECDSA_Public_Raw[Low(OECDSA_Public_Raw)],l_bs.Size);
    finally
      l_bs.Free;
    end;
    Result := True;
  end;
end;

function TECDSA_Public_Helper.FromRaw(const AECDSA_Public_Raw: TECDSA_Public_Raw): Boolean;
var l_bs : TBytesStream;
  l_w : Word;
begin
  Result := False; // Initial result to False
  if Length(AECDSA_Public_Raw)<6 then Exit; // Not minimum size: 2 + 2 + 0 + 2 + 0 = 6 bytes
  l_bs := TBytesStream.Create;
  try
    l_bs.WriteBuffer(AECDSA_Public_Raw[Low(AECDSA_Public_Raw)],Length(AECDSA_Public_Raw));
    l_bs.Read(self.EC_OpenSSL_NID,2); // Read 2 bytes little endian with EC_OpenSSL_NID
    l_bs.Read(l_w,2);                 // Read 2 bytes little endian with x length
    if (l_bs.Position + l_w) < l_bs.Size then Exit;
    SetLength(self.x,l_w);
    l_bs.ReadBuffer(self.x[Low(self.x)],l_w); // Read x as RAW
    l_bs.Read(l_w,2);                 // Read 2 bytes little endian with y length
    if (l_bs.Position + l_w - 1) <> l_bs.Size then Exit; // size must match
    SetLength(self.x,l_w);
    l_bs.ReadBuffer(self.y[Low(self.y)],l_w); // Read y as RAW
    Result := True;
  finally
    l_bs.Free;
  end;
end;

{ TAccount }

procedure TAccount.Clear;
begin
  Self := CT_Account_NUL;
end;

function TAccount.GetLastUpdatedBlock: Cardinal;
begin
  if (Self.updated_on_block_passive_mode>Self.updated_on_block_active_mode) then Result := Self.updated_on_block_passive_mode
  else Result := Self.updated_on_block_active_mode;
end;

{ TPCSafeBoxHeader }

function TPCSafeBoxHeader.GetSavedBlocksCount: Integer;
begin
  Result := Self.endBlock - Self.startBlock + 1;
end;

function TPCSafeBoxHeader.IsAChunk: Boolean;
begin
  Result := (Self.startBlock<>0) or (Self.endBlock+1<>Self.blocksCount);
end;

function TPCSafeBoxHeader.IsFullSafebox: Boolean;
begin
  Result := (Self.startBlock=0) and (Self.endBlock+1=Self.blocksCount);
end;

function TPCSafeBoxHeader.ContainsFirstBlock: Boolean;
begin
  Result := (Self.startBlock=0);
end;

function TPCSafeBoxHeader.ContainsLastBlock: Boolean;
begin
  Result := (Self.endBlock+1=Self.blocksCount);
end;

function TPCSafeBoxHeader.ToString: String;
begin
  if IsFullSafebox then begin
    Result := Format('Fulls SafeboxHeader from %d to %d (%d)',[Self.startBlock,Self.endBlock,Self.blocksCount])
  end else begin
    Result := Format('Chunk SafeboxHeader from %d to %d (%d of %d)',[Self.startBlock,Self.endBlock,Self.GetSavedBlocksCount,Self.blocksCount]);
  end;
end;



end.

