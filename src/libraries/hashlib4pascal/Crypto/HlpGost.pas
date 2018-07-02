unit HlpGost;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpBits,
  HlpConverters,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

type

  TGost = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private

    Fm_state, Fm_hash: THashLibUInt32Array;

    class var

      Fs_sbox1, Fs_sbox2, Fs_sbox3, Fs_sbox4: THashLibUInt32Array;

    procedure Compress(a_m: PCardinal);
    class constructor Gost();

  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;

  end;

implementation

{ TGost }

procedure TGost.Compress(a_m: PCardinal);
var
  u0, u1, u2, u3, u4, u5, u6, u7, v0, v1, v2, v3, v4, v5, v6, v7, w0, w1, w2,
    w3, w4, w5, w6, w7, key0, key1, key2, key3, key4, key5, key6, key7, r, l,
    t: UInt32;
  i: Int32;
  s: array [0 .. 7] of UInt32;
begin
  u0 := Fm_hash[0];
  u1 := Fm_hash[1];
  u2 := Fm_hash[2];
  u3 := Fm_hash[3];
  u4 := Fm_hash[4];
  u5 := Fm_hash[5];
  u6 := Fm_hash[6];
  u7 := Fm_hash[7];

  v0 := a_m[0];
  v1 := a_m[1];
  v2 := a_m[2];
  v3 := a_m[3];
  v4 := a_m[4];
  v5 := a_m[5];
  v6 := a_m[6];
  v7 := a_m[7];

  i := 0;

  while i < 8 do

  begin
    w0 := u0 xor v0;
    w1 := u1 xor v1;
    w2 := u2 xor v2;
    w3 := u3 xor v3;
    w4 := u4 xor v4;
    w5 := u5 xor v5;
    w6 := u6 xor v6;
    w7 := u7 xor v7;

    key0 := UInt32(Byte(w0)) or (UInt32(Byte(w2)) shl 8) or
      (UInt32(Byte(w4)) shl 16) or (UInt32(Byte(w6)) shl 24);
    key1 := UInt32(Byte(w0 shr 8)) or (w2 and $0000FF00) or
      ((w4 and $0000FF00) shl 8) or ((w6 and $0000FF00) shl 16);
    key2 := UInt32(Byte(w0 shr 16)) or ((w2 and $00FF0000) shr 8) or
      (w4 and $00FF0000) or ((w6 and $00FF0000) shl 8);
    key3 := (w0 shr 24) or ((w2 and $FF000000) shr 16) or
      ((w4 and $FF000000) shr 8) or (w6 and $FF000000);
    key4 := UInt32(Byte(w1)) or ((w3 and $000000FF) shl 8) or
      ((w5 and $000000FF) shl 16) or ((w7 and $000000FF) shl 24);
    key5 := UInt32(Byte(w1 shr 8)) or (w3 and $0000FF00) or
      ((w5 and $0000FF00) shl 8) or ((w7 and $0000FF00) shl 16);
    key6 := UInt32(Byte(w1 shr 16)) or ((w3 and $00FF0000) shr 8) or
      (w5 and $00FF0000) or ((w7 and $00FF0000) shl 8);
    key7 := (w1 shr 24) or ((w3 and $FF000000) shr 16) or
      ((w5 and $FF000000) shr 8) or (w7 and $FF000000);

    r := Fm_hash[i];
    l := Fm_hash[i + 1];

    t := key0 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key1 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key2 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key3 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key4 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key5 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key6 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key7 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key0 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key1 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key2 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key3 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key4 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key5 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key6 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key7 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key0 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key1 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key2 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key3 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key4 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key5 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key6 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key7 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key7 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key6 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key5 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key4 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key3 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key2 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key1 + r;
    l := l xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);
    t := key0 + l;
    r := r xor (Fs_sbox1[Byte(t)] xor Fs_sbox2[Byte(t shr 8)] xor Fs_sbox3
      [Byte(t shr 16)] xor Fs_sbox4[t shr 24]);

    t := r;
    r := l;
    l := t;

    s[i] := r;
    s[i + 1] := l;

    if (i = 6) then
      break;

    l := u0 xor u2;
    r := u1 xor u3;
    u0 := u2;
    u1 := u3;
    u2 := u4;
    u3 := u5;
    u4 := u6;
    u5 := u7;
    u6 := l;
    u7 := r;

    if (i = 2) then
    begin
      u0 := u0 xor $FF00FF00;
      u1 := u1 xor $FF00FF00;
      u2 := u2 xor $00FF00FF;
      u3 := u3 xor $00FF00FF;
      u4 := u4 xor $00FFFF00;
      u5 := u5 xor $FF0000FF;
      u6 := u6 xor $000000FF;
      u7 := u7 xor $FF00FFFF;
    end;

    l := v0;
    r := v2;
    v0 := v4;
    v2 := v6;
    v4 := l xor r;
    v6 := v0 xor r;
    l := v1;
    r := v3;
    v1 := v5;
    v3 := v7;
    v5 := l xor r;
    v7 := v1 xor r;

    System.Inc(i, 2);
  end;

  u0 := a_m[0] xor s[6];
  u1 := a_m[1] xor s[7];
  u2 := a_m[2] xor (s[0] shl 16) xor (s[0] shr 16) xor (s[0] and $FFFF)
    xor (s[1] and $FFFF) xor (s[1] shr 16) xor (s[2] shl 16)
    xor s[6] xor (s[6] shl 16) xor (s[7] and $FFFF0000) xor (s[7] shr 16);
  u3 := a_m[3] xor (s[0] and $FFFF) xor (s[0] shl 16) xor (s[1] and $FFFF)
    xor (s[1] shl 16) xor (s[1] shr 16) xor (s[2] shl 16) xor (s[2] shr 16)
    xor (s[3] shl 16) xor s[6] xor (s[6] shl 16) xor (s[6] shr 16)
    xor (s[7] and $FFFF) xor (s[7] shl 16) xor (s[7] shr 16);
  u4 := a_m[4] xor (s[0] and $FFFF0000) xor (s[0] shl 16) xor (s[0] shr 16)
    xor (s[1] and $FFFF0000) xor (s[1] shr 16) xor (s[2] shl 16)
    xor (s[2] shr 16) xor (s[3] shl 16) xor (s[3] shr 16) xor (s[4] shl 16)
    xor (s[6] shl 16) xor (s[6] shr 16) xor (s[7] and $FFFF) xor (s[7] shl 16)
    xor (s[7] shr 16);
  u5 := a_m[5] xor (s[0] shl 16) xor (s[0] shr 16) xor (s[0] and $FFFF0000)
    xor (s[1] and $FFFF) xor s[2] xor (s[2] shr 16) xor (s[3] shl 16)
    xor (s[3] shr 16) xor (s[4] shl 16) xor (s[4] shr 16) xor (s[5] shl 16)
    xor (s[6] shl 16) xor (s[6] shr 16) xor (s[7] and $FFFF0000)
    xor (s[7] shl 16) xor (s[7] shr 16);
  u6 := a_m[6] xor s[0] xor (s[1] shr 16) xor (s[2] shl 16)
    xor s[3] xor (s[3] shr 16) xor (s[4] shl 16) xor (s[4] shr 16)
    xor (s[5] shl 16) xor (s[5] shr 16) xor s[6] xor (s[6] shl 16)
    xor (s[6] shr 16) xor (s[7] shl 16);
  u7 := a_m[7] xor (s[0] and $FFFF0000) xor (s[0] shl 16) xor (s[1] and $FFFF)
    xor (s[1] shl 16) xor (s[2] shr 16) xor (s[3] shl 16)
    xor s[4] xor (s[4] shr 16) xor (s[5] shl 16) xor (s[5] shr 16)
    xor (s[6] shr 16) xor (s[7] and $FFFF) xor (s[7] shl 16) xor (s[7] shr 16);

  v0 := Fm_hash[0] xor (u1 shl 16) xor (u0 shr 16);
  v1 := Fm_hash[1] xor (u2 shl 16) xor (u1 shr 16);
  v2 := Fm_hash[2] xor (u3 shl 16) xor (u2 shr 16);
  v3 := Fm_hash[3] xor (u4 shl 16) xor (u3 shr 16);
  v4 := Fm_hash[4] xor (u5 shl 16) xor (u4 shr 16);
  v5 := Fm_hash[5] xor (u6 shl 16) xor (u5 shr 16);
  v6 := Fm_hash[6] xor (u7 shl 16) xor (u6 shr 16);
  v7 := Fm_hash[7] xor (u0 and $FFFF0000) xor (u0 shl 16) xor (u7 shr 16)
    xor (u1 and $FFFF0000) xor (u1 shl 16) xor (u6 shl 16)
    xor (u7 and $FFFF0000);

  Fm_hash[0] := (v0 and $FFFF0000) xor (v0 shl 16) xor (v0 shr 16)
    xor (v1 shr 16) xor (v1 and $FFFF0000) xor (v2 shl 16) xor (v3 shr 16)
    xor (v4 shl 16) xor (v5 shr 16) xor v5 xor (v6 shr 16) xor (v7 shl 16)
    xor (v7 shr 16) xor (v7 and $FFFF);
  Fm_hash[1] := (v0 shl 16) xor (v0 shr 16) xor (v0 and $FFFF0000)
    xor (v1 and $FFFF) xor v2 xor (v2 shr 16) xor (v3 shl 16) xor (v4 shr 16)
    xor (v5 shl 16) xor (v6 shl 16) xor v6 xor (v7 and $FFFF0000)
    xor (v7 shr 16);
  Fm_hash[2] := (v0 and $FFFF) xor (v0 shl 16) xor (v1 shl 16) xor (v1 shr 16)
    xor (v1 and $FFFF0000) xor (v2 shl 16) xor (v3 shr 16)
    xor v3 xor (v4 shl 16) xor (v5 shr 16) xor v6 xor (v6 shr 16)
    xor (v7 and $FFFF) xor (v7 shl 16) xor (v7 shr 16);
  Fm_hash[3] := (v0 shl 16) xor (v0 shr 16) xor (v0 and $FFFF0000)
    xor (v1 and $FFFF0000) xor (v1 shr 16) xor (v2 shl 16) xor (v2 shr 16)
    xor v2 xor (v3 shl 16) xor (v4 shr 16) xor v4 xor (v5 shl 16)
    xor (v6 shl 16) xor (v7 and $FFFF) xor (v7 shr 16);
  Fm_hash[4] := (v0 shr 16) xor (v1 shl 16) xor v1 xor (v2 shr 16)
    xor v2 xor (v3 shl 16) xor (v3 shr 16) xor v3 xor (v4 shl 16)
    xor (v5 shr 16) xor v5 xor (v6 shl 16) xor (v6 shr 16) xor (v7 shl 16);
  Fm_hash[5] := (v0 shl 16) xor (v0 and $FFFF0000) xor (v1 shl 16)
    xor (v1 shr 16) xor (v1 and $FFFF0000) xor (v2 shl 16)
    xor v2 xor (v3 shr 16) xor v3 xor (v4 shl 16) xor (v4 shr 16)
    xor v4 xor (v5 shl 16) xor (v6 shl 16) xor (v6 shr 16)
    xor v6 xor (v7 shl 16) xor (v7 shr 16) xor (v7 and $FFFF0000);
  Fm_hash[6] := v0 xor v2 xor (v2 shr 16) xor v3 xor (v3 shl 16)
    xor v4 xor (v4 shr 16) xor (v5 shl 16) xor (v5 shr 16)
    xor v5 xor (v6 shl 16) xor (v6 shr 16) xor v6 xor (v7 shl 16) xor v7;
  Fm_hash[7] := v0 xor (v0 shr 16) xor (v1 shl 16) xor (v1 shr 16)
    xor (v2 shl 16) xor (v3 shr 16) xor v3 xor (v4 shl 16)
    xor v4 xor (v5 shr 16) xor v5 xor (v6 shl 16) xor (v6 shr 16)
    xor (v7 shl 16) xor v7;

end;

constructor TGost.Create;
begin
  Inherited Create(32, 32);
  System.SetLength(Fm_state, 8);
  System.SetLength(Fm_hash, 8);

end;

procedure TGost.Finish;
var
  bits: UInt64;
  pad: THashLibByteArray;
  m_length: THashLibUInt32Array;
begin
  bits := Fm_processed_bytes * 8;

  if (Fm_buffer.Pos > 0) then
  begin
    System.SetLength(pad, 32 - Fm_buffer.Pos);
    TransformBytes(pad, 0, 32 - Fm_buffer.Pos);
  end;
  System.SetLength(m_length, 8);
  m_length[0] := UInt32(bits);
  m_length[1] := UInt32(bits shr 32);

  Compress(PCardinal(m_length));

  Compress(PCardinal(Fm_state));

end;

function TGost.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt32));
  TConverters.le32_copy(PCardinal(Fm_hash), 0, PByte(result), 0,
    System.Length(result));
end;

class constructor TGost.Gost;
var
  sbox: THashLibMatrixUInt32Array;

  i, a, b: Int32;
  ax, bx, cx, dx: UInt32;
begin
  sbox := THashLibMatrixUInt32Array.Create(THashLibUInt32Array.Create(4, 10, 9,
    2, 13, 8, 0, 14, 6, 11, 1, 12, 7, 15, 5, 3), THashLibUInt32Array.Create(14,
    11, 4, 12, 6, 13, 15, 10, 2, 3, 8, 1, 0, 7, 5, 9),
    THashLibUInt32Array.Create(5, 8, 1, 13, 10, 3, 4, 2, 14, 15, 12, 7, 6, 0, 9,
    11), THashLibUInt32Array.Create(7, 13, 10, 1, 0, 8, 9, 15, 14, 4, 6, 12, 11,
    2, 5, 3), THashLibUInt32Array.Create(6, 12, 7, 1, 5, 15, 13, 8, 4, 10, 9,
    14, 0, 3, 11, 2), THashLibUInt32Array.Create(4, 11, 10, 0, 7, 2, 1, 13, 3,
    6, 8, 5, 9, 12, 15, 14), THashLibUInt32Array.Create(13, 11, 4, 1, 3, 15, 5,
    9, 0, 10, 14, 7, 6, 8, 2, 12), THashLibUInt32Array.Create(1, 15, 13, 0, 5,
    7, 10, 4, 9, 2, 3, 14, 6, 11, 8, 12));

  System.SetLength(Fs_sbox1, 256);
  System.SetLength(Fs_sbox2, 256);
  System.SetLength(Fs_sbox3, 256);
  System.SetLength(Fs_sbox4, 256);

  i := 0;

  for a := 0 to 15 do

  begin
    ax := sbox[1, a] shl 15;
    bx := sbox[3, a] shl 23;
    cx := sbox[5, a];
    cx := TBits.RotateRight32(cx, 1);
    dx := sbox[7, a] shl 7;

    for b := 0 to 15 do

    begin
      Fs_sbox1[i] := ax or (sbox[0, b] shl 11);
      Fs_sbox2[i] := bx or (sbox[2, b] shl 19);
      Fs_sbox3[i] := cx or (sbox[4, b] shl 27);
      Fs_sbox4[i] := dx or (sbox[6, b] shl 3);
      System.Inc(i);
    end;

  end;

end;

procedure TGost.Initialize;
begin

  System.FillChar(Fm_state[0], System.Length(Fm_state) * System.SizeOf(UInt32),
    UInt32(0));
  System.FillChar(Fm_hash[0], System.Length(Fm_hash) * System.SizeOf(UInt32),
    UInt32(0));

  Inherited Initialize();

end;

procedure TGost.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  data, m: array [0 .. 7] of UInt32;
  c, a, b: UInt32;
  i: Int32;
begin

  c := 0;

  TConverters.le32_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  for i := 0 to 7 do
  begin
    a := data[i];
    m[i] := a;
    b := Fm_state[i];
    c := a + c + Fm_state[i];
    Fm_state[i] := c;
    if ((c < a) or (c < b)) then

      c := UInt32(1)
    else
      c := UInt32(0);

  end;

  Compress(@(m[0]));

  System.FillChar(m, System.SizeOf(m), 0);
  System.FillChar(data, System.SizeOf(data), 0);

end;

end.
