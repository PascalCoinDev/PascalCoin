{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpAesLightEngine;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIAesLightEngine,
  ClpIBlockCipher,
  ClpICipherParameters,
  ClpIKeyParameter,
  ClpCheck,
  ClpBits,
  ClpConverters,
  ClpCryptoLibTypes;

resourcestring
  SAESEngineNotInitialised = 'AES Engine not Initialised';
  SInputBuffertooShort = 'Input Buffer too Short';
  SOutputBuffertooShort = 'Output Buffer too Short';
  SInvalidParameterAESInit = 'Invalid Parameter Passed to AES Init - "%s"';
  SInvalidKeyLength = 'Key Length not 128/192/256 bits.';
  SInvalidOperation = 'Should Never Get Here';

type

  /// <summary>
  /// <para>
  /// an implementation of the AES (Rijndael), from FIPS-197.
  /// </para>
  /// <para>
  /// For further details see: <see href="http://csrc.nist.gov/encryption/aes/" />
  /// </para>
  /// <para>
  /// This implementation is based on optimizations from Dr. Brian
  /// Gladman's paper and C code at <see href="http://fp.gladman.plus.com/cryptography_technology/rijndael/" />
  /// </para>
  /// <para>
  /// This version uses no static tables at all and computes the values
  /// in each round.
  /// </para>
  /// <para>
  /// This file contains the slowest performance version with no static
  /// tables for round precomputation, but it has the smallest foot
  /// print.
  /// </para>
  /// </summary>
  TAesLightEngine = class sealed(TInterfacedObject, IAesLightEngine,
    IBlockCipher)

  strict private

  const
    // multiply four bytes in GF(2^8) by 'x' {02} in parallel //

    m1 = UInt32($80808080);
    m2 = UInt32($7F7F7F7F);
    m3 = UInt32($0000001B);
    m4 = UInt32($C0C0C0C0);
    m5 = UInt32($3F3F3F3F);
    BLOCK_SIZE = Int32(16);

    // The S box
    S: array [0 .. 255] of Byte = (99, 124, 119, 123, 242, 107, 111, 197, 48, 1,
      103, 43, 254, 215, 171, 118, 202, 130, 201, 125, 250, 89, 71, 240, 173,
      212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247, 204,
      52, 165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 150, 5, 154, 7,
      18, 128, 226, 235, 39, 178, 117, 9, 131, 44, 26, 27, 110, 90, 160, 82, 59,
      214, 179, 41, 227, 47, 132, 83, 209, 0, 237, 32, 252, 177, 91, 106, 203,
      190, 57, 74, 76, 88, 207, 208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2,
      127, 80, 60, 159, 168, 81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218,
      33, 16, 255, 243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126,
      61, 100, 93, 25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184,
      20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98,
      145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244,
      234, 101, 122, 174, 8, 186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116,
      31, 75, 189, 139, 138, 112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185,
      134, 193, 29, 158, 225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135,
      233, 206, 85, 40, 223, 140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45,
      15, 176, 84, 187, 22);

    // The inverse S-box
    Si: array [0 .. 255] of Byte = (82, 9, 106, 213, 48, 54, 165, 56, 191, 64,
      163, 158, 129, 243, 215, 251, 124, 227, 57, 130, 155, 47, 255, 135, 52,
      142, 67, 68, 196, 222, 233, 203, 84, 123, 148, 50, 166, 194, 35, 61, 238,
      76, 149, 11, 66, 250, 195, 78, 8, 46, 161, 102, 40, 217, 36, 178, 118, 91,
      162, 73, 109, 139, 209, 37, 114, 248, 246, 100, 134, 104, 152, 22, 212,
      164, 92, 204, 93, 101, 182, 146, 108, 112, 72, 80, 253, 237, 185, 218, 94,
      21, 70, 87, 167, 141, 157, 132, 144, 216, 171, 0, 140, 188, 211, 10, 247,
      228, 88, 5, 184, 179, 69, 6, 208, 44, 30, 143, 202, 63, 15, 2, 193, 175,
      189, 3, 1, 19, 138, 107, 58, 145, 17, 65, 79, 103, 220, 234, 151, 242,
      207, 206, 240, 180, 230, 115, 150, 172, 116, 34, 231, 173, 53, 133, 226,
      249, 55, 232, 28, 117, 223, 110, 71, 241, 26, 113, 29, 41, 197, 137, 111,
      183, 98, 14, 170, 24, 190, 27, 252, 86, 62, 75, 198, 210, 121, 32, 154,
      219, 192, 254, 120, 205, 90, 244, 31, 221, 168, 51, 136, 7, 199, 49, 177,
      18, 16, 89, 39, 128, 236, 95, 96, 81, 127, 169, 25, 181, 74, 13, 45, 229,
      122, 159, 147, 201, 156, 239, 160, 224, 59, 77, 174, 42, 245, 176, 200,
      235, 187, 60, 131, 83, 153, 97, 23, 43, 4, 126, 186, 119, 214, 38, 225,
      105, 20, 99, 85, 33, 12, 125);

    // vector used in calculating key schedule (powers of x in GF(256))
    Rcon: array [0 .. 29] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80,
      $1B, $36, $6C, $D8, $AB, $4D, $9A, $2F, $5E, $BC, $63, $C6, $97, $35, $6A,
      $D4, $B3, $7D, $FA, $EF, $C5, $91);

  var
    FROUNDS: Int32;
    FWorkingKey: TCryptoLibMatrixUInt32Array;
    FC0, FC1, FC2, FC3: UInt32;
    FforEncryption: Boolean;

    function GetAlgorithmName: String; virtual;
    function GetIsPartialBlockOkay: Boolean; virtual;
    function GetBlockSize(): Int32; virtual;

    /// <summary>
    /// <para>
    /// Calculate the necessary round keys
    /// </para>
    /// <para>
    /// The number of calculations depends on key size and block size
    /// </para>
    /// <para>
    /// AES specified a fixed block size of 128 bits and key sizes
    /// 128/192/256 bits
    /// </para>
    /// <para>
    /// This code is written assuming those are the only possible values
    /// </para>
    /// </summary>
    function GenerateWorkingKey(forEncryption: Boolean;
      const key: TCryptoLibByteArray): TCryptoLibMatrixUInt32Array;

    procedure UnPackBlock(const bytes: TCryptoLibByteArray; off: Int32); inline;
    procedure PackBlock(const bytes: TCryptoLibByteArray; off: Int32); inline;

    procedure EncryptBlock(const KW: TCryptoLibMatrixUInt32Array);

    procedure DecryptBlock(const KW: TCryptoLibMatrixUInt32Array);

    class function Shift(r: UInt32; Shift: Int32): UInt32; static; inline;
    class function FFmulX(x: UInt32): UInt32; static; inline;
    class function FFmulX2(x: UInt32): UInt32; static; inline;

    // The following defines provide alternative definitions of FFmulX that might
    // give improved performance if a fast 32-bit multiply is not available.
    //
    // private int FFmulX(int x) { int u = x & m1; u |= (u >> 1); return ((x & m2) << 1) ^ ((u >>> 3) | (u >>> 6)); }
    // private static final int  m4 = 0x1b1b1b1b;
    // private int FFmulX(int x) { int u = x & m1; return ((x & m2) << 1) ^ ((u - (u >>> 7)) & m4); }

    class function Mcol(x: UInt32): UInt32; static; inline;
    class function Inv_Mcol(x: UInt32): UInt32; static; inline;
    class function SubWord(x: UInt32): UInt32; static; inline;

  public
    /// <summary>
    /// initialise an AES cipher.
    /// </summary>
    /// <param name="forEncryption">
    /// whether or not we are for encryption.
    /// </param>
    /// <param name="parameters">
    /// the parameters required to set up the cipher.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); virtual;

    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32; virtual;

    procedure Reset(); virtual;

    property AlgorithmName: String read GetAlgorithmName;
    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

implementation

{ TAesLightEngine }

class function TAesLightEngine.Shift(r: UInt32; Shift: Int32): UInt32;
begin
  result := TBits.RotateRight32(r, Shift);
end;

class function TAesLightEngine.SubWord(x: UInt32): UInt32;
begin
  result := UInt32(S[x and 255]) or (UInt32(S[(x shr 8) and 255]) shl 8) or
    (UInt32(S[(x shr 16) and 255]) shl 16) or
    (UInt32(S[(x shr 24) and 255]) shl 24);
end;

class function TAesLightEngine.FFmulX(x: UInt32): UInt32;
begin
  result := ((x and m2) shl 1) xor (((x and m1) shr 7) * m3);
end;

class function TAesLightEngine.FFmulX2(x: UInt32): UInt32;
var
  t0, t1: UInt32;
begin
  t0 := (x and m5) shl 2;
  t1 := (x and m4);
  t1 := t1 xor (t1 shr 1);
  result := t0 xor (t1 shr 2) xor (t1 shr 5);
end;

class function TAesLightEngine.Mcol(x: UInt32): UInt32;
var
  t0, t1: UInt32;
begin
  t0 := Shift(x, 8);
  t1 := x xor t0;
  result := Shift(t1, 16) xor t0 xor FFmulX(t1);
end;

class function TAesLightEngine.Inv_Mcol(x: UInt32): UInt32;
var
  t0, t1: UInt32;
begin
  t0 := x;
  t1 := t0 xor Shift(t0, 8);
  t0 := t0 xor FFmulX(t1);
  t1 := t1 xor FFmulX2(t0);
  t0 := t0 xor (t1 xor Shift(t1, 16));
  result := t0;
end;

procedure TAesLightEngine.EncryptBlock(const KW: TCryptoLibMatrixUInt32Array);
var
  lkw: TCryptoLibUInt32Array;
  lt0, lt1, lt2, lr0, lr1, lr2, lr3: UInt32;
  lr: Int32;
begin
  lkw := KW[0];
  lt0 := FC0 xor lkw[0];
  lt1 := FC1 xor lkw[1];
  lt2 := FC2 xor lkw[2];

  lr3 := FC3 xor lkw[3];
  lr := 1;

  while (lr < FROUNDS - 1) do
  begin
    lkw := KW[lr];
    System.Inc(lr);
    lr0 := Mcol(UInt32(S[lt0 and 255]) xor ((UInt32(S[(lt1 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lt2 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lr3 shr 24) and 255])) shl 24)) xor lkw[0];
    lr1 := Mcol(UInt32(S[lt1 and 255]) xor ((UInt32(S[(lt2 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lr3 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lt0 shr 24) and 255])) shl 24)) xor lkw[1];
    lr2 := Mcol(UInt32(S[lt2 and 255]) xor ((UInt32(S[(lr3 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lt0 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lt1 shr 24) and 255])) shl 24)) xor lkw[2];
    lr3 := Mcol(UInt32(S[lr3 and 255]) xor ((UInt32(S[(lt0 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lt1 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lt2 shr 24) and 255])) shl 24)) xor lkw[3];
    lkw := KW[lr];
    System.Inc(lr);
    lt0 := Mcol(UInt32(S[lr0 and 255]) xor ((UInt32(S[(lr1 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lr2 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lr3 shr 24) and 255])) shl 24)) xor lkw[0];
    lt1 := Mcol(UInt32(S[lr1 and 255]) xor ((UInt32(S[(lr2 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lr3 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lr0 shr 24) and 255])) shl 24)) xor lkw[1];
    lt2 := Mcol(UInt32(S[lr2 and 255]) xor ((UInt32(S[(lr3 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lr0 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lr1 shr 24) and 255])) shl 24)) xor lkw[2];
    lr3 := Mcol(UInt32(S[lr3 and 255]) xor ((UInt32(S[(lr0 shr 8) and 255]))
      shl 8) xor ((UInt32(S[(lr1 shr 16) and 255])) shl 16)
      xor ((UInt32(S[(lr2 shr 24) and 255])) shl 24)) xor lkw[3];
  end;

  lkw := KW[lr];
  System.Inc(lr);
  lr0 := Mcol(UInt32(S[lt0 and 255]) xor ((UInt32(S[(lt1 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lt2 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr3 shr 24) and 255])) shl 24)) xor lkw[0];
  lr1 := Mcol(UInt32(S[lt1 and 255]) xor ((UInt32(S[(lt2 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lr3 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lt0 shr 24) and 255])) shl 24)) xor lkw[1];
  lr2 := Mcol(UInt32(S[lt2 and 255]) xor ((UInt32(S[(lr3 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lt0 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lt1 shr 24) and 255])) shl 24)) xor lkw[2];
  lr3 := Mcol(UInt32(S[lr3 and 255]) xor ((UInt32(S[(lt0 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lt1 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lt2 shr 24) and 255])) shl 24)) xor lkw[3];

  // the final round is a simple function of S

  lkw := KW[lr];
  FC0 := UInt32(S[lr0 and 255]) xor ((UInt32(S[(lr1 shr 8) and 255])) shl 8)
    xor ((UInt32(S[(lr2 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr3 shr 24) and 255])) shl 24) xor lkw[0];
  FC1 := UInt32(S[lr1 and 255]) xor ((UInt32(S[(lr2 shr 8) and 255])) shl 8)
    xor ((UInt32(S[(lr3 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr0 shr 24) and 255])) shl 24) xor lkw[1];
  FC2 := UInt32(S[lr2 and 255]) xor ((UInt32(S[(lr3 shr 8) and 255])) shl 8)
    xor ((UInt32(S[(lr0 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr1 shr 24) and 255])) shl 24) xor lkw[2];
  FC3 := UInt32(S[lr3 and 255]) xor ((UInt32(S[(lr0 shr 8) and 255])) shl 8)
    xor ((UInt32(S[(lr1 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr2 shr 24) and 255])) shl 24) xor lkw[3];
end;

procedure TAesLightEngine.DecryptBlock(const KW: TCryptoLibMatrixUInt32Array);
var
  lkw: TCryptoLibUInt32Array;
  lt0, lt1, lt2, lr0, lr1, lr2, lr3: UInt32;
  lr: Int32;
begin
  lkw := KW[FROUNDS];
  lt0 := FC0 xor lkw[0];
  lt1 := FC1 xor lkw[1];
  lt2 := FC2 xor lkw[2];

  lr3 := FC3 xor lkw[3];
  lr := FROUNDS - 1;

  while (lr > 1) do
  begin
    lkw := KW[lr];
    System.Dec(lr);
    lr0 := Inv_Mcol(UInt32(Si[lt0 and 255])
      xor ((UInt32(Si[(lr3 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lt2 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lt1 shr 24) and 255]) shl 24)) xor lkw[0];
    lr1 := Inv_Mcol(UInt32(Si[lt1 and 255])
      xor ((UInt32(Si[(lt0 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lr3 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lt2 shr 24) and 255]) shl 24)) xor lkw[1];
    lr2 := Inv_Mcol(UInt32(Si[lt2 and 255])
      xor ((UInt32(Si[(lt1 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lt0 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lr3 shr 24) and 255]) shl 24)) xor lkw[2];
    lr3 := Inv_Mcol(UInt32(Si[lr3 and 255])
      xor ((UInt32(Si[(lt2 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lt1 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lt0 shr 24) and 255]) shl 24)) xor lkw[3];
    lkw := KW[lr];
    System.Dec(lr);
    lt0 := Inv_Mcol(UInt32(Si[lr0 and 255])
      xor ((UInt32(Si[(lr3 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lr2 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lr1 shr 24) and 255]) shl 24)) xor lkw[0];
    lt1 := Inv_Mcol(UInt32(Si[lr1 and 255])
      xor ((UInt32(Si[(lr0 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lr3 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lr2 shr 24) and 255]) shl 24)) xor lkw[1];
    lt2 := Inv_Mcol(UInt32(Si[lr2 and 255])
      xor ((UInt32(Si[(lr1 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lr0 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lr3 shr 24) and 255]) shl 24)) xor lkw[2];
    lr3 := Inv_Mcol(UInt32(Si[lr3 and 255])
      xor ((UInt32(Si[(lr2 shr 8) and 255])) shl 8)
      xor ((UInt32(Si[(lr1 shr 16) and 255])) shl 16)
      xor (UInt32(Si[(lr0 shr 24) and 255]) shl 24)) xor lkw[3];
  end;

  lkw := KW[1];
  lr0 := Inv_Mcol(UInt32(Si[lt0 and 255]) xor ((UInt32(Si[(lr3 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lt2 shr 16) and 255])) shl 16)
    xor (UInt32(Si[(lt1 shr 24) and 255]) shl 24)) xor lkw[0];
  lr1 := Inv_Mcol(UInt32(Si[lt1 and 255]) xor ((UInt32(Si[(lt0 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lr3 shr 16) and 255])) shl 16)
    xor (UInt32(Si[(lt2 shr 24) and 255]) shl 24)) xor lkw[1];
  lr2 := Inv_Mcol(UInt32(Si[lt2 and 255]) xor ((UInt32(Si[(lt1 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lt0 shr 16) and 255])) shl 16)
    xor (UInt32(Si[(lr3 shr 24) and 255]) shl 24)) xor lkw[2];
  lr3 := Inv_Mcol(UInt32(Si[lr3 and 255]) xor ((UInt32(Si[(lt2 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lt1 shr 16) and 255])) shl 16)
    xor (UInt32(Si[(lt0 shr 24) and 255]) shl 24)) xor lkw[3];

  // the final round's table is a simple function of Si

  lkw := KW[0];
  FC0 := UInt32(Si[lr0 and 255]) xor ((UInt32(Si[(lr3 shr 8) and 255])) shl 8)
    xor ((UInt32(Si[(lr2 shr 16) and 255])) shl 16)
    xor ((UInt32(Si[(lr1 shr 24) and 255])) shl 24) xor lkw[0];
  FC1 := UInt32(Si[lr1 and 255]) xor ((UInt32(Si[(lr0 shr 8) and 255])) shl 8)
    xor ((UInt32(Si[(lr3 shr 16) and 255])) shl 16)
    xor ((UInt32(Si[(lr2 shr 24) and 255])) shl 24) xor lkw[1];
  FC2 := UInt32(Si[lr2 and 255]) xor ((UInt32(Si[(lr1 shr 8) and 255])) shl 8)
    xor ((UInt32(Si[(lr0 shr 16) and 255])) shl 16)
    xor ((UInt32(Si[(lr3 shr 24) and 255])) shl 24) xor lkw[2];
  FC3 := UInt32(Si[lr3 and 255]) xor ((UInt32(Si[(lr2 shr 8) and 255])) shl 8)
    xor ((UInt32(Si[(lr1 shr 16) and 255])) shl 16)
    xor ((UInt32(Si[(lr0 shr 24) and 255])) shl 24) xor lkw[3];
end;

function TAesLightEngine.GenerateWorkingKey(forEncryption: Boolean;
  const key: TCryptoLibByteArray): TCryptoLibMatrixUInt32Array;
var
  keyLen, KC, i, j: Int32;
  smallw: TCryptoLibUInt32Array;
  bigW: TCryptoLibMatrixUInt32Array;
  u, lrcon, t0, t1, t2, t3, t4, t5, t6, t7: UInt32;

begin
  keyLen := System.Length(key);
  if ((keyLen < 16) or (keyLen > 32) or ((keyLen and 7) <> 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidKeyLength);
  end;

  KC := keyLen shr 2; // KC := keyLen div 4;
  FROUNDS := KC + 6;
  // This is not always true for the generalized Rijndael that allows larger block sizes
  System.SetLength(bigW, FROUNDS + 1); // 4 words in a block

  for i := 0 to FROUNDS do
  begin
    System.SetLength(bigW[i], 4);
  end;

  case KC of
    4:
      begin
        t0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := t0;
        t1 := TConverters.ReadBytesAsUInt32LE(PByte(key), 4);
        bigW[0][1] := t1;
        t2 := TConverters.ReadBytesAsUInt32LE(PByte(key), 8);
        bigW[0][2] := t2;
        t3 := TConverters.ReadBytesAsUInt32LE(PByte(key), 12);
        bigW[0][3] := t3;

        for i := 1 to 10 do
        begin
          u := SubWord(Shift(t3, 8)) xor Rcon[i - 1];
          t0 := t0 xor u;
          bigW[i][0] := t0;
          t1 := t1 xor t0;
          bigW[i][1] := t1;
          t2 := t2 xor t1;
          bigW[i][2] := t2;
          t3 := t3 xor t2;
          bigW[i][3] := t3;
        end;
      end;

    6:
      begin
        t0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := t0;
        t1 := TConverters.ReadBytesAsUInt32LE(PByte(key), 4);
        bigW[0][1] := t1;
        t2 := TConverters.ReadBytesAsUInt32LE(PByte(key), 8);
        bigW[0][2] := t2;
        t3 := TConverters.ReadBytesAsUInt32LE(PByte(key), 12);
        bigW[0][3] := t3;
        t4 := TConverters.ReadBytesAsUInt32LE(PByte(key), 16);
        bigW[1][0] := t4;
        t5 := TConverters.ReadBytesAsUInt32LE(PByte(key), 20);
        bigW[1][1] := t5;

        lrcon := 1;
        u := SubWord(Shift(t5, 8)) xor lrcon;
        lrcon := lrcon shl 1;
        t0 := t0 xor u;
        bigW[1][2] := t0;
        t1 := t1 xor t0;
        bigW[1][3] := t1;
        t2 := t2 xor t1;
        bigW[2][0] := t2;
        t3 := t3 xor t2;
        bigW[2][1] := t3;
        t4 := t4 xor t3;
        bigW[2][2] := t4;
        t5 := t5 xor t4;
        bigW[2][3] := t5;

        i := 3;

        while i < 12 do

        begin
          u := SubWord(Shift(t5, 8)) xor lrcon;
          lrcon := lrcon shl 1;
          t0 := t0 xor u;
          bigW[i][0] := t0;
          t1 := t1 xor t0;
          bigW[i][1] := t1;
          t2 := t2 xor t1;
          bigW[i][2] := t2;
          t3 := t3 xor t2;
          bigW[i][3] := t3;
          t4 := t4 xor t3;
          bigW[i + 1][0] := t4;
          t5 := t5 xor t4;
          bigW[i + 1][1] := t5;
          u := SubWord(Shift(t5, 8)) xor lrcon;
          lrcon := lrcon shl 1;
          t0 := t0 xor u;
          bigW[i + 1][2] := t0;
          t1 := t1 xor t0;
          bigW[i + 1][3] := t1;
          t2 := t2 xor t1;
          bigW[i + 2][0] := t2;
          t3 := t3 xor t2;
          bigW[i + 2][1] := t3;
          t4 := t4 xor t3;
          bigW[i + 2][2] := t4;
          t5 := t5 xor t4;
          bigW[i + 2][3] := t5;
          System.Inc(i, 3);
        end;

        u := SubWord(Shift(t5, 8)) xor lrcon;
        t0 := t0 xor u;
        bigW[12][0] := t0;
        t1 := t1 xor t0;
        bigW[12][1] := t1;
        t2 := t2 xor t1;
        bigW[12][2] := t2;
        t3 := t3 xor t2;
        bigW[12][3] := t3;
      end;

    8:
      begin
        t0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := t0;
        t1 := TConverters.ReadBytesAsUInt32LE(PByte(key), 4);
        bigW[0][1] := t1;
        t2 := TConverters.ReadBytesAsUInt32LE(PByte(key), 8);
        bigW[0][2] := t2;
        t3 := TConverters.ReadBytesAsUInt32LE(PByte(key), 12);
        bigW[0][3] := t3;
        t4 := TConverters.ReadBytesAsUInt32LE(PByte(key), 16);
        bigW[1][0] := t4;
        t5 := TConverters.ReadBytesAsUInt32LE(PByte(key), 20);
        bigW[1][1] := t5;
        t6 := TConverters.ReadBytesAsUInt32LE(PByte(key), 24);
        bigW[1][2] := t6;
        t7 := TConverters.ReadBytesAsUInt32LE(PByte(key), 28);
        bigW[1][3] := t7;

        lrcon := 1;

        i := 2;

        while i < 14 do

        begin
          u := SubWord(Shift(t7, 8)) xor lrcon;
          lrcon := lrcon shl 1;
          t0 := t0 xor u;
          bigW[i][0] := t0;
          t1 := t1 xor t0;
          bigW[i][1] := t1;
          t2 := t2 xor t1;
          bigW[i][2] := t2;
          t3 := t3 xor t2;;
          bigW[i][3] := t3;
          u := SubWord(t3);
          t4 := t4 xor u;
          bigW[i + 1][0] := t4;
          t5 := t5 xor t4;
          bigW[i + 1][1] := t5;
          t6 := t6 xor t5;
          bigW[i + 1][2] := t6;
          t7 := t7 xor t6;
          bigW[i + 1][3] := t7;
          System.Inc(i, 2);
        end;

        u := SubWord(Shift(t7, 8)) xor lrcon;
        t0 := t0 xor u;
        bigW[14][0] := t0;
        t1 := t1 xor t0;
        bigW[14][1] := t1;
        t2 := t2 xor t1;
        bigW[14][2] := t2;
        t3 := t3 xor t2;;
        bigW[14][3] := t3;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes(@SInvalidOperation);
    end;
  end;

  if (not forEncryption) then
  begin
    for j := 1 to System.Pred(FROUNDS) do

    begin
      smallw := bigW[j];
      for i := 0 to System.Pred(4) do

      begin
        smallw[i] := Inv_Mcol(smallw[i]);
      end;
    end;
  end;

  result := bigW;

end;

function TAesLightEngine.GetAlgorithmName: String;
begin
  result := 'AES';
end;

function TAesLightEngine.GetBlockSize: Int32;
begin
  result := BLOCK_SIZE;
end;

function TAesLightEngine.GetIsPartialBlockOkay: Boolean;
begin
  result := false;
end;

procedure TAesLightEngine.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  keyParameter: IKeyParameter;
begin

  if not Supports(parameters, IKeyParameter, keyParameter) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidParameterAESInit,
      [(parameters as TObject).ToString]);
  end;

  FWorkingKey := GenerateWorkingKey(forEncryption, keyParameter.GetKey());

  FforEncryption := forEncryption;
end;

procedure TAesLightEngine.PackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  TConverters.ReadUInt32AsBytesLE(FC0, bytes, off);
  TConverters.ReadUInt32AsBytesLE(FC1, bytes, off + 4);
  TConverters.ReadUInt32AsBytesLE(FC2, bytes, off + 8);
  TConverters.ReadUInt32AsBytesLE(FC3, bytes, off + 12);
end;

procedure TAesLightEngine.UnPackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
begin
  FC0 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off);
  FC1 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 4);
  FC2 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 8);
  FC3 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 12);
end;

function TAesLightEngine.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if (FWorkingKey = Nil) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SAESEngineNotInitialised);
  end;

  TCheck.DataLength(input, inOff, 16, SInputBuffertooShort);
  TCheck.OutputLength(output, outOff, 16, SOutputBuffertooShort);

  UnPackBlock(input, inOff);

  if (FforEncryption) then
  begin
    EncryptBlock(FWorkingKey);
  end
  else
  begin
    DecryptBlock(FWorkingKey);
  end;

  PackBlock(output, outOff);

  result := BLOCK_SIZE;
end;

procedure TAesLightEngine.Reset;
begin
  // nothing to do.
end;

end.
