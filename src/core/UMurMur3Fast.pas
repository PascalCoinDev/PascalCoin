unit UMurMur3Fast;

{ Copyright (c) 2019 by Albert Molina

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
  {$MODE delphi}
{$ENDIF}

interface

uses
SysUtils;

{ This unit contains a fast implementation of MurMur3 hash
  https://en.wikipedia.org/wiki/MurmurHash

  How to use:

  ### Single call:
  Call TMurMur3Fast.Hash(  using TBytes or a buffer reference, Optionally add a Initial Seed [0=Default] )

  ### Multiple calls:
  var M : TMurMur3Fast
  M.Init(0); // Init buffer and Seed, 0 by default
  for each bunch:
    M.Add(  )
  end for
  Result := M.ComputeFinal;

  ### Extra
  You can copy 2 TMurMur3Fast using "Clone"

}

Type
  TMurMur3Fast = Record
    MurMurPendingBuffer : TBytes;
    MurMurNextSeed : Integer;
    MurMurProcessedCount : Integer;
    procedure Init(ASeed : UInt32);
    procedure Add(Const AData : TBytes); overload;
    procedure Add(Const {$IFNDEF FPC}[ref]{$ENDIF} AData; ADataLen: Integer); overload;
    function ComputeFinal : UInt32;
    function Hash(Const AData : TBytes; ASeed : Integer = 0) : UInt32; overload;
    function Hash(Const {$IFNDEF FPC}[ref]{$ENDIF} AData; ADataLen: Integer; ASeed : Integer = 0) : UInt32; overload;
    function Clone : TMurMur3Fast;
  end;

Const
  CT_TMurMur3Fast_NUL : TMurMur3Fast = (MurMurPendingBuffer:Nil;MurMurNextSeed:0;MurMurProcessedCount:0);

implementation

{$pointermath on}

{

The "MurMurHash3" main assembler function (below) has been obtained from this public source:
https://stackoverflow.com/questions/30942918/is-there-any-delphi-implementation-of-murmurhash3

Possible author is JBontes (  https://github.com/JBontes/FastCode ) but original file "FastDefaults" does not contain this function. (Checked on 2019-10-01)

}

function MurmurHash3(const {$IFNDEF FPC}[ref]{$ENDIF} HashData; Len: integer; Seed: integer = 0): integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{$IFDEF no_assembler}
var
  i, Len2: integer;
  k: cardinal;
  remaining: cardinal;
  Data: PCardinal;
label
  case1, case2, case3, final;
begin
  Result:= Seed;
  Data:= @HashData;
  for i:= 0 to (Len shr 2) - 1 do begin
    k:= Data[i];
    k:= k * c1;
    k:= (k shl r1) or (k shr (32 - r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32 - r2));
    Result:= Result * m + n;
  end; {for i}
  Len2:= Len;
  remaining:= 0;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
    else goto final;
  end;
case3:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 16);
case2:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 8);
case1:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2]);
  remaining:= remaining * c1;
  remaining:= (remaining shl r1) or (remaining shr (32 - r1));
  remaining:= remaining * c2;
  Result:= Result xor remaining;
final:
  Result:= Result xor Len;

  Result:= Result xor (Result shr 16);
  Result:= Result * f1;
  Result:= Result xor (Result shr 13);
  Result:= Result * f2;
  Result:= Result xor (Result shr 16);
end;
{$ELSE}
{$REGION 'asm'}
{$IFDEF CPUx86}
{$IFDEF FPC}
  {$asmMode intel}
{$ENDIF}
  asm
    push EBX
    push EDI
    push ESI
    xchg ECX,EDX
    //EAX = data
    //ECX = count in bytes
    //EDX = seed
    mov  ESI,ECX
    shr  ECX,2
    jz @remaining_bytes
  @loop:
    mov  EDI,[EAX]
    imul EDI,EDI,c1
    rol  EDI,r1
    imul EDI,EDI,c2
    xor  EDX,EDI
    rol  EDX,r2
    lea  EDX,[EDX*4+EDX+n]
    lea  EAX,[EAX+4]
    dec  ECX
    jnz @loop
  @remaining_bytes:
    mov  ECX,ESI
    and  ECX,$3
    jz @finalization
    xor  EBX,EBX
    dec  ECX
    mov  BL,byte ptr [EAX+ECX]
    jz @process_remaining
    shl  EBX,8
    dec  ECX
    mov  BL,byte ptr [EAX+ECX]
    jz @process_remaining
    shl  EBX,8
    mov  BL,byte ptr [EAX]
  @process_remaining:
    imul EBX,EBX,c1
    rol  EBX,r1
    imul EBX,EBX,c2
    xor  EDX,EBX
  @finalization:
    xor  EDX,ESI
    mov  EAX,EDX
    shr  EDX,16
    xor  EDX,EAX
    imul EDX,EDX,f1
    mov  EAX,EDX
    shr  EDX,13
    xor  EDX,EAX
    imul EDX,EDX,f2
    mov  EAX,EDX
    shr  EDX,16
    xor  EAX,EDX
    pop  ESI
    pop  EDI
    pop  EBX
end;
{$ENDIF}
{$IFDEF CPUx64}
{$IFDEF FPC}
  {$asmMode intel}
{$ENDIF}
asm
  push RBX
  push RDI
  push RSI
  mov  RAX,RCX
  mov  RCX,RDX
  mov  RDX,R8
  //RAX = data
  //RCX = count in bytes
  //RDX = seed
  mov  ESI,ECX
  shr  ECX,2
  jz @remaining_bytes
@loop:
  mov  EDI, dword ptr [RAX]
  imul EDI,EDI,c1
  rol  EDI,r1
  imul EDI,EDI,c2
  xor  EDX,EDI
  rol  EDX,r2
  lea  EDX,dword ptr [EDX*4+EDX+n] // *5 + n
  lea  RAX,qword ptr [RAX+4]
  dec  ECX
  jnz @loop
@remaining_bytes:
  mov  ECX,ESI
  and  ECX,$3
  jz @finalization
  xor  RBX,RBX
  dec  ECX
  mov  BL,byte ptr [RAX+RCX]
  jz @process_remaining
  shl  EBX,8
  dec  ECX
  mov  BL,byte ptr [RAX+RCX]
  jz @process_remaining
  shl  EBX,8
  mov  BL,byte ptr [RAX]
@process_remaining:
  imul EBX,EBX,c1
  rol  EBX,r1
  imul EBX,EBX,c2
  xor  EDX,EBX
@finalization:
  xor  EDX,ESI
  mov  EAX,EDX
  shr  EDX,16
  xor  EDX,EAX
  imul EDX,EDX,f1
  mov  EAX,EDX
  shr  EDX,13
  xor  EDX,EAX
  imul EDX,EDX,f2
  mov  EAX,EDX
  shr  EDX,16
  xor  EAX,EDX
  pop  RSI
  pop  RDI
  pop  RBX
end;
{$ENDIF}
{$ENDREGION}
{$ENDIF}

{

  Next MurMurHash3 assembler functions are a modification of previous MurMurHash3 function

}

function MurmurHash3_Compute(const {$IFNDEF FPC}[ref]{$ENDIF} HashData; Len: integer; SeedOrLastCompute: integer = 0): integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{.$DEFINE no_assembler} // Active only for non assembler
{$IFDEF no_assembler}
var
  i, Len2: integer;
  k: cardinal;
  remaining: cardinal;
  Data: PCardinal;
label
  case1, case2, case3, final;
begin
  Result:= SeedOrLastCompute;
  Data:= @HashData;
  for i:= 0 to (Len shr 2) - 1 do begin
    k:= Data[i];
    k:= k * c1;
    k:= (k shl r1) or (k shr (32 - r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32 - r2));
    Result:= Result * m + n;
  end; {for i}
end;
{$ELSE}
{$REGION 'asm'}
{$IFDEF CPUx86}
  asm
    push EBX
    push EDI
    push ESI
    xchg ECX,EDX
    mov  ESI,ECX
    shr  ECX,2
    jz @return_seed
  @loop:
    mov  EDI,[EAX]
    imul EDI,EDI,c1
    rol  EDI,r1
    imul EDI,EDI,c2
    xor  EDX,EDI
    rol  EDX,r2
    lea  EDX,[EDX*4+EDX+n]
    lea  EAX,[EAX+4]
    dec  ECX
    jnz @loop
  @return_seed:
    mov EAX,EDX
    pop  ESI
    pop  EDI
    pop  EBX
end;
{$ENDIF}
{$IFDEF CPUx64}
asm
  push RBX
  push RDI
  push RSI
  mov  RAX,RCX
  mov  RCX,RDX
  mov  RDX,R8
  mov  ESI,ECX
  shr  ECX,2
  jz @return_seed
@loop:
  mov  EDI, dword ptr [RAX]
  imul EDI,EDI,c1
  rol  EDI,r1
  imul EDI,EDI,c2
  xor  EDX,EDI
  rol  EDX,r2
  lea  EDX,dword ptr [EDX*4+EDX+n] // *5 + n
  lea  RAX,qword ptr [RAX+4]
  dec  ECX
  jnz @loop
@return_seed:
  mov EAX,EDX
  pop  RSI
  pop  RDI
  pop  RBX
end;
{$ENDIF}
{$ENDREGION}
{$ENDIF}


function MurmurHash3_Final(const {$IFNDEF FPC}[ref]{$ENDIF} HashData; Len: integer; TotalLength : Integer; SeedOrLastCompute: integer = 0): integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
var
  i, Len2: integer;
  k: cardinal;
  remaining: cardinal;
  Data: PCardinal;
label
  case1, case2, case3, final;
begin
  assert( (Len>=0) and (Len<=3) , Format('Invalid Len %d on Final',[Len]));
  Result:= SeedOrLastCompute;
  Data:= @HashData;

  Len2:= Len;
  remaining:= 0;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
    else goto final;
  end;
case3:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 16);
case2:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2] shl 8);
case1:
  dec(Len2);
  inc(remaining, PByte(Data)[Len2]);
  remaining:= remaining * c1;
  remaining:= (remaining shl r1) or (remaining shr (32 - r1));
  remaining:= remaining * c2;
  Result:= Result xor remaining;
final:
  Result:= Result xor TotalLength;

  Result:= Result xor (Result shr 16);
  Result:= Result * f1;
  Result:= Result xor (Result shr 13);
  Result:= Result * f2;
  Result:= Result xor (Result shr 16);
end;

{ TMurMur3Data }

procedure TMurMur3Fast.Add(Const AData: TBytes);
begin
  Self.Add( AData[0], Length(AData) );
end;

procedure TMurMur3Fast.Add(const {$IFNDEF FPC}[ref]{$ENDIF} AData; ADataLen: Integer);
var LOffset, LPendingBytes : Integer;
  i : Integer;
  LDataRef : PByte;
begin
  Assert( Length(MurMurPendingBuffer) in [0..3] , Format('Invalid MurMurPendingBuffer length %d',[Length(MurMurPendingBuffer)]) );
  if ADataLen<=0 then Exit;
  // Pending from previous Add?
  if (Length(MurMurPendingBuffer)>0) and
    ((Length(MurMurPendingBuffer) + ADataLen)>=4) then begin
    i := Length(MurMurPendingBuffer);
    SetLength(Self.MurMurPendingBuffer,4);

    LOffset := 4 - i; // Offset will be 1..3
    Move(AData,Self.MurMurPendingBuffer[i],4-i);

    Self.MurMurNextSeed := MurmurHash3_Compute(Self.MurMurPendingBuffer[0],4,Self.MurMurNextSeed);

    SetLength(Self.MurMurPendingBuffer,0);
  end else LOffset := 0;
  // Process

  LDataRef := @AData;
  inc(LDataRef, LOffset);

  Self.MurMurNextSeed := MurmurHash3_Compute(LDataRef^, ((ADataLen-LOffset) DIV 4)*4, Self.MurMurNextSeed);

  // Save pending bytes
  LPendingBytes := ((ADataLen-LOffset) MOD 4);
  if LPendingBytes > 0 then begin
    SetLength(Self.MurMurPendingBuffer, Length(Self.MurMurPendingBuffer) + LPendingBytes);
    LDataRef := @AData;
    inc(LDataRef, ADataLen - LPendingBytes );
    Move(LDataRef^, Self.MurMurPendingBuffer[ Length(Self.MurMurPendingBuffer) - LPendingBytes ], LPendingBytes);
  end;
  Inc(Self.MurMurProcessedCount, ADataLen);
end;

function TMurMur3Fast.Clone: TMurMur3Fast;
begin
  Result := CT_TMurMur3Fast_NUL;
  Result.MurMurPendingBuffer := System.Copy(Self.MurMurPendingBuffer);
  Result.MurMurNextSeed := Self.MurMurNextSeed;
  Result.MurMurProcessedCount := Self.MurMurProcessedCount;
end;

function TMurMur3Fast.ComputeFinal: UInt32;
begin
  Result := MurmurHash3_Final( Self.MurMurPendingBuffer[0], Length(Self.MurMurPendingBuffer), Self.MurMurProcessedCount, Self.MurMurNextSeed );
end;

function TMurMur3Fast.Hash(const {$IFNDEF FPC}[ref]{$ENDIF} AData; ADataLen, ASeed: Integer): UInt32;
begin
  Result := MurmurHash3(AData,ADataLen,ASeed);
end;

function TMurMur3Fast.Hash(Const AData: TBytes; ASeed: Integer): UInt32;
begin
  Result := MurmurHash3(AData[0],Length(AData),ASeed);
end;

procedure TMurMur3Fast.Init(ASeed: UInt32);
begin
  SetLength(Self.MurMurPendingBuffer,0);
  Self.MurMurNextSeed := ASeed;
  Self.MurMurProcessedCount := 0;
end;

end.
