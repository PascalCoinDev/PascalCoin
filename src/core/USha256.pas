unit USha256;

{ Copyright (c) 2016 by PacalCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

interface

uses Sysutils, Classes;

type
  TSHA256HASH = array[0..7] of Cardinal;
  TChunk = array[0..15] of Cardinal;

function CalcDoubleSHA256(Msg: AnsiString): TSHA256HASH;
function CalcSHA256(Msg: AnsiString): TSHA256HASH; overload;
function CalcSHA256(Stream: TStream): TSHA256HASH; overload;
function SHA256ToStr(Hash: TSHA256HASH): String;


Function CanBeModifiedOnLastChunk(MessageTotalLength : Int64; var startBytePos : integer) : Boolean;
Procedure PascalCoinPrepareLastChunk(Const messageToHash : AnsiString; out stateForLastChunk : TSHA256HASH; out bufferForLastChunk : TChunk);
Function ExecuteLastChunk(const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal) : TSHA256HASH;
Function ExecuteLastChunkAndDoSha256(Const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal) : TSHA256HASH;
Procedure PascalCoinExecuteLastChunkAndDoSha256(Const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal; var ResultSha256 : AnsiString);
Function Sha256HashToRaw(Const hash : TSHA256HASH) : AnsiString; overload;
procedure Sha256HashToRaw(Const hash : TSHA256HASH; var raw : AnsiString); overload;

implementation

type
  PChunk = ^TChunk;

const
  k: array[0..63] of Cardinal = (
   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
   $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
   $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
   $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
   $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
   $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

{$IFDEF i386}

  {$IFDEF FPC}
    {$ASMMODE intel}
  {$ENDIF}

  function ror(x: Cardinal; y: Byte): Cardinal; assembler;
  asm
    mov cl,dl
    ror eax,cl
  end;

  function bswap(x: Cardinal): Cardinal; assembler;
  asm
    bswap eax
  end;

  function swap64(x: int64): int64; assembler;
  asm
    mov edx,dword ptr[x]
    mov eax,dword ptr[x+4]
    bswap edx
    bswap eax
  end;
{$ELSE}
  function ror(x: Cardinal; y: Byte): Cardinal;
  begin
    ror:=
      (x shr y) +
      (x shl (32-y));
  end;

  function bswap(x: Cardinal): Cardinal;
  begin
    bswap:=
      ((x and $000000FF) shl 24) +
      ((x and $0000FF00) shl  8) +
      ((x and $00FF0000) shr  8) +
      ((x and $FF000000) shr 24);
  end;

  function swap64(x: int64): int64;
  begin
    swap64:=
      ((x and $00000000000000FF) shl 56) +
      ((x and $000000000000FF00) shl 40) +
      ((x and $0000000000FF0000) shl 24) +
      ((x and $00000000FF000000) shl 8) +
      ((x and $000000FF00000000) shr 8) +
      ((x and $0000FF0000000000) shr 24) +
      ((x and $00FF000000000000) shr 40) +
      ((x and $FF00000000000000) shr 56);
  end;
{$ENDIF}

function CalcChunk(Hash: TSHA256HASH; const Chunk: TChunk): TSHA256HASH;
var
  i: Integer;
  s0, s1, maj, t1, t2, ch: Cardinal;
  w: array[0..63] of Cardinal;
begin
  for i:=0 to 15 do
    w[i]:= bswap(Chunk[i]);
  for i:= 16 to 63 do
  begin
    s0:=   ror(w[i-15],7) xor ror(w[i-15],18) xor (w[i-15] shr 3);
    s1:=   ror(w[i-2],17) xor ror(w[i-2],19) xor (w[i-2] shr 10);
    w[i]:= w[i-16] + s0 + w[i-7] + s1;
  end;
  for i:= 0 to 63 do
  begin
    s0:=  ror(Hash[0],2) xor ror(Hash[0],13) xor ror(Hash[0],22);
    maj:= (Hash[0] and Hash[1]) xor (Hash[0] and Hash[2]) xor (Hash[1] and Hash[2]);
    t2:=  s0 + maj;
    s1:=  ror(Hash[4],6) xor ror(Hash[4],11) xor ror(Hash[4],25);
    ch:=  (Hash[4] and Hash[5]) xor ((not Hash[4]) and Hash[6]);
    t1:=  Hash[7] + s1 + ch + k[i] + w[i];
    Hash[7]:= Hash[6];
    Hash[6]:= Hash[5];
    Hash[5]:= Hash[4];
    Hash[4]:= Hash[3] + t1;
    Hash[3]:= Hash[2];
    Hash[2]:= Hash[1];
    Hash[1]:= Hash[0];
    Hash[0]:= t1 + t2;
  end;
  Result:= Hash;
end;

function CalcSHA256(Msg: AnsiString): TSHA256HASH; overload;
var
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  try
    Stream.WriteBuffer(PAnsiChar(Msg)^,Length(Msg));
    Stream.Position:= 0;
    Result:= CalcSHA256(Stream);
  finally
    Stream.Free;
  end;
end;

function CalcDoubleSHA256(Msg: AnsiString): TSHA256HASH;
var
  Stream: TMemoryStream;
  m : AnsiString;
begin
  Stream:= TMemoryStream.Create;
  try
    Stream.WriteBuffer(PAnsiChar(Msg)^,Length(Msg));
    Stream.Position:= 0;
    m := Sha256HashToRaw(CalcSHA256(Stream));
    Stream.size := 0;
    Stream.WriteBuffer(m[1],32);
    Stream.Position:= 0;
    Result := CalcSHA256(Stream);
  finally
    Stream.Free;
  end;
End;

Const
  rSha256 : Array[0..7] of Cardinal =
   ($6a09e667,$bb67ae85,$3c6ef372,$a54ff53a,$510e527f,$9b05688c,$1f83d9ab,$5be0cd19);

function CalcSHA256(Stream: TStream): TSHA256HASH; overload;
var
  i,j,k: Integer;
  Size: int64;
  P: PAnsiChar;
  Chunk: PChunk;
  H: TSHA256HASH;
  PBuffer : Array[0..127] of byte;
begin
  Result[0]:= rSha256[0];
  Result[1]:= rSha256[1];
  Result[2]:= rSha256[2];
  Result[3]:= rSha256[3];
  Result[4]:= rSha256[4];
  Result[5]:= rSha256[5];
  Result[6]:= rSha256[6];
  Result[7]:= rSha256[7];
  Size:= 0;
  // Positioning P to buffer start
  P := @PBuffer[0];
  Chunk:= PChunk(P);
  // Fill
  FillChar(P^,64*2,#0);
  // Read first
  i:= Stream.Read(P^,64);
  while i = 64 do begin
    H:= CalcChunk(Result,Chunk^);
    for k:= 0 to 7 do
      Result[k]:= Result[k] + H[k];
    inc(Size,i);
    FillChar(P^,64*2,#0);
    i:= Stream.Read(P^,64);
  end;
  inc(Size,i);
  P[i]:= #$80;
  j:= i + 9;
  if j mod 64 > 0 then
   inc(j,64 - (j mod 64));
  Size:= swap64(Size*8);
  move(Size,P[j-8],8);
  for i:= 1 to j div 64 do
  begin
    H:= CalcChunk(Result,Chunk^);
    for k:= 0 to 7 do
      Result[k]:= Result[k] + H[k];
    inc(Chunk);
  end;
end;

function SHA256ToStr(Hash: TSHA256HASH): String;
var
  i: Integer;
begin
  Result:= EmptyStr;
  for i:= 0 to 6 do
    Result:= Result + IntToHex(Hash[i],8) + #32;
  Result:= Result + IntToHex(Hash[7],8);
end;

Function CanBeModifiedOnLastChunk(MessageTotalLength : Int64; var startBytePos : integer) : Boolean;
Begin
  { Sha256 process each round 512 bits (64 bytes)
    Timestamp and nOnce are last 8 bytes of digest message, so must be processed on last round
    last round, includes a reserved 9 bytes at the end :
     - 1 byte for $80 (1 bit left padded)
     - 8 bytes for length of digest message in bits
    Start byte pos can be a number between 0..63 - (9 reserved bytes) = 0..54
    Also, start byte must be MOD 4=0 because each value is 4 bytes in Sha256 calcs, so must discard last 4 bytes of left padded bit 0..51
    Finally: Value between 0..51 and (MOD 4=0)
    Valid values are: 0,4,8,12,16,20,24,28,32,36,40,44,48 = TOTAL 12 valid values of 64
    }
  startBytePos := (((((MessageTotalLength)*8)+72) MOD 512) DIV 8) - (8+9);
  Result := (startBytePos >= 0) And ((startBytePos MOD 4)=0) And (startBytePos<=48);
End;

Procedure PascalCoinPrepareLastChunk(Const messageToHash : AnsiString; out stateForLastChunk : TSHA256HASH; out bufferForLastChunk : TChunk);
var
  i,j,k,iPos: Integer;
  Size: int64;
  P: PAnsiChar;
  Chunk: PChunk;
  H: TSHA256HASH;
  PBuffer : Array[0..127] of byte;
begin
  //Will produce the TSHA256HASH ready for the last chunk
  stateForLastChunk[0]:= rSha256[0];
  stateForLastChunk[1]:= rSha256[1];
  stateForLastChunk[2]:= rSha256[2];
  stateForLastChunk[3]:= rSha256[3];
  stateForLastChunk[4]:= rSha256[4];
  stateForLastChunk[5]:= rSha256[5];
  stateForLastChunk[6]:= rSha256[6];
  stateForLastChunk[7]:= rSha256[7];
  Size:= 0;
  // Positioning P to buffer start
  P := @PBuffer[0];
  Chunk:= PChunk(P);
  iPos := 0;
  Repeat
    FillChar(P^,64*2,#0);
    i := length(messageToHash) - iPos;
    if (i > 64) then i:=64;
    Move(messageToHash[iPos+1],P[0],i);
    if (i = 64) then
    begin
      inc(iPos,i);
      H:= CalcChunk(stateForLastChunk,Chunk^);
      for k:= 0 to 7 do
        stateForLastChunk[k]:= stateForLastChunk[k] + H[k];
      inc(Size,i);
    end;
  Until i<>64;
  inc(Size,i);
  P[i]:= #$80;
  j:= i + 9;
  if j mod 64 > 0 then
   inc(j,64 - (j mod 64));
  Size:= swap64(Size*8);
  move(Size,P[j-8],8);
  if (j div 64)>1 then begin
    H:= CalcChunk(stateForLastChunk,Chunk^);
    for k:= 0 to 7 do
      stateForLastChunk[k]:= stateForLastChunk[k] + H[k];
    inc(Chunk);
  end;
  FillChar(bufferForLastChunk,64,#0);
  move(Chunk[0],bufferForLastChunk,64);
end;


Function ExecuteLastChunk(const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal) : TSHA256HASH;
Var
  bflc : TChunk;
  P : PAnsiChar;
  H: TSHA256HASH;
  k : Integer;
Begin
  move(bufferForLastChunk[0],bflc[0],16*4);
  P := @bflc[0];
  move(Timestamp,P[nPos],4);
  move(nOnce,P[nPos+4],4);
  H := CalcChunk(stateForLastChunk,bflc);
  for k:= 0 to 7 do
    Result[k]:= stateForLastChunk[k] + H[k];
End;

Function ExecuteLastChunkAndDoSha256(Const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal) : TSHA256HASH;
var
  i,k: Integer;
  Size: int64;
  P: PAnsiChar;
  Chunk: PChunk;
  H,HSwapped: TSHA256HASH;
  PBuffer : Array[0..127] of byte;
Begin
  H := ExecuteLastChunk(stateForLastChunk,bufferForLastChunk,nPos,nOnce,Timestamp);
  // Prepare for a SHA256 with a single chunk on 32 bytes
  Result[0]:= rSha256[0];
  Result[1]:= rSha256[1];
  Result[2]:= rSha256[2];
  Result[3]:= rSha256[3];
  Result[4]:= rSha256[4];
  Result[5]:= rSha256[5];
  Result[6]:= rSha256[6];
  Result[7]:= rSha256[7];
  Size:= 0;
  // Positioning P to buffer start
  P := @PBuffer[0];
  Chunk:= PChunk(P);
  FillChar(P^,64,#0);
  for i := 0 to 7 do begin
    HSwapped[i] := bSwap(H[i]);
  end;
  Move(HSwapped[0],P[0],32);
  // Adding 1 bit
  P[32]:= #$80;
  // Save size as bigendian to the end
  Size := swap64(32*8);
  move(Size,P[56],8);
  H:= CalcChunk(Result,Chunk^);
  for k:= 0 to 7 do
    Result[k]:= Result[k] + H[k];
End;

Procedure PascalCoinExecuteLastChunkAndDoSha256(Const stateForLastChunk : TSHA256HASH; const bufferForLastChunk : TChunk; nPos : Integer; nOnce,Timestamp : Cardinal; var ResultSha256 : AnsiString);
Var  H: TSHA256HASH;
Begin
  H := ExecuteLastChunkAndDoSha256(stateForLastChunk,bufferForLastChunk,nPos,nOnce,Timestamp);
  Sha256HashToRaw(H,ResultSha256);
End;

Function Sha256HashToRaw(Const hash : TSHA256HASH) : AnsiString;
var i: Integer;
  c : Cardinal;
begin
  SetLength(Result,32);
  for i:= 0 to 7 do begin
    c := hash[i];
    Result[4+(i*4)] := AnsiChar(c MOD 256);
    Result[3+(i*4)] := AnsiChar((c SHR 8) MOD 256);
    Result[2+(i*4)] := AnsiChar((c SHR 16) MOD 256);
    Result[1+(i*4)] := AnsiChar((c SHR 24) MOD 256);
  end;
End;

procedure Sha256HashToRaw(Const hash : TSHA256HASH; var raw : AnsiString); overload;
var i: Integer;
  c : Cardinal;
begin
  if (length(raw)<>32) then SetLength(raw,32);
  for i:= 0 to 7 do begin
    c := hash[i];
    raw[4+(i*4)] := AnsiChar(c MOD 256);
    raw[3+(i*4)] := AnsiChar((c SHR 8) MOD 256);
    raw[2+(i*4)] := AnsiChar((c SHR 16) MOD 256);
    raw[1+(i*4)] := AnsiChar((c SHR 24) MOD 256);
  end;
end;

end.
