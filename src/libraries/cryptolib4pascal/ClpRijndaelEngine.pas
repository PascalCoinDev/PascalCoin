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

unit ClpRijndaelEngine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpCheck,
  ClpIBlockCipher,
  ClpIRijndaelEngine,
  ClpIKeyParameter,
  ClpICipherParameters,
  ClpCryptoLibTypes;

resourcestring
  SInputBuffertooShort = 'Input Buffer too Short';
  SOutputBuffertooShort = 'Output Buffer too Short';
  SUnsupportedBlock = 'Unknown Blocksize to Rijndael';
  SInvalidKeyLength = 'Key Length not 128/160/192/224/256 bits.';
  SRijndaelEngineNotInitialised = 'Rijndael Engine not Initialised';
  SInvalidParameterRijndaelInit =
    'Invalid Parameter Passed to Rijndael Init - "%s"';

type

  /// <summary>
  /// <para>
  /// an implementation of Rijndael, based on the documentation and
  /// reference implementation by Paulo Barreto, Vincent Rijmen, for v2.0
  /// August '99.
  /// </para>
  /// <para>
  /// Note: this implementation is based on information prior to readonly
  /// NIST publication.
  /// </para>
  /// </summary>
  TRijndaelEngine = class(TInterfacedObject, IRijndaelEngine, IBlockCipher)

  strict private
  const
    MAXROUNDS = Int32(14);
    MAXKC = Int32(256 div 4);

    Logtable: array [0 .. 255] of Byte = (0, 0, 25, 1, 50, 2, 26, 198, 75, 199,
      27, 104, 51, 238, 223, 3, 100, 4, 224, 14, 52, 141, 129, 239, 76, 113, 8,
      200, 248, 105, 28, 193, 125, 194, 29, 181, 249, 185, 39, 106, 77, 228,
      166, 114, 154, 201, 9, 120, 101, 47, 138, 5, 33, 15, 225, 36, 18, 240,
      130, 69, 53, 147, 218, 142, 150, 143, 219, 189, 54, 208, 206, 148, 19, 92,
      210, 241, 64, 70, 131, 56, 102, 221, 253, 48, 191, 6, 139, 98, 179, 37,
      226, 152, 34, 136, 145, 16, 126, 110, 72, 195, 163, 182, 30, 66, 58, 107,
      40, 84, 250, 133, 61, 186, 43, 121, 10, 21, 155, 159, 94, 202, 78, 212,
      172, 229, 243, 115, 167, 87, 175, 88, 168, 80, 244, 234, 214, 116, 79,
      174, 233, 213, 231, 230, 173, 232, 44, 215, 117, 122, 235, 22, 11, 245,
      89, 203, 95, 176, 156, 169, 81, 160, 127, 12, 246, 111, 23, 196, 73, 236,
      216, 67, 31, 45, 164, 118, 123, 183, 204, 187, 62, 90, 251, 96, 177, 134,
      59, 82, 161, 108, 170, 85, 41, 157, 151, 178, 135, 144, 97, 190, 220, 252,
      188, 149, 207, 205, 55, 63, 91, 209, 83, 57, 132, 60, 65, 162, 109, 71,
      20, 42, 158, 93, 86, 242, 211, 171, 68, 17, 146, 217, 35, 32, 46, 137,
      180, 124, 184, 38, 119, 153, 227, 165, 103, 74, 237, 222, 197, 49, 254,
      24, 13, 99, 140, 128, 192, 247, 112, 7);

    Alogtable: array [0 .. 510] of Byte = (0, 3, 5, 15, 17, 51, 85, 255, 26, 46,
      114, 150, 161, 248, 19, 53, 95, 225, 56, 72, 216, 115, 149, 164, 247, 2,
      6, 10, 30, 34, 102, 170, 229, 52, 92, 228, 55, 89, 235, 38, 106, 190, 217,
      112, 144, 171, 230, 49, 83, 245, 4, 12, 20, 60, 68, 204, 79, 209, 104,
      184, 211, 110, 178, 205, 76, 212, 103, 169, 224, 59, 77, 215, 98, 166,
      241, 8, 24, 40, 120, 136, 131, 158, 185, 208, 107, 189, 220, 127, 129,
      152, 179, 206, 73, 219, 118, 154, 181, 196, 87, 249, 16, 48, 80, 240, 11,
      29, 39, 105, 187, 214, 97, 163, 254, 25, 43, 125, 135, 146, 173, 236, 47,
      113, 147, 174, 233, 32, 96, 160, 251, 22, 58, 78, 210, 109, 183, 194, 93,
      231, 50, 86, 250, 21, 63, 65, 195, 94, 226, 61, 71, 201, 64, 192, 91, 237,
      44, 116, 156, 191, 218, 117, 159, 186, 213, 100, 172, 239, 42, 126, 130,
      157, 188, 223, 122, 142, 137, 128, 155, 182, 193, 88, 232, 35, 101, 175,
      234, 37, 111, 177, 200, 67, 197, 84, 252, 31, 33, 99, 165, 244, 7, 9, 27,
      45, 119, 153, 176, 203, 70, 202, 69, 207, 74, 222, 121, 139, 134, 145,
      168, 227, 62, 66, 198, 81, 243, 14, 18, 54, 90, 238, 41, 123, 141, 140,
      143, 138, 133, 148, 167, 242, 13, 23, 57, 75, 221, 124, 132, 151, 162,
      253, 28, 36, 108, 180, 199, 82, 246, 1, 3, 5, 15, 17, 51, 85, 255, 26, 46,
      114, 150, 161, 248, 19, 53, 95, 225, 56, 72, 216, 115, 149, 164, 247, 2,
      6, 10, 30, 34, 102, 170, 229, 52, 92, 228, 55, 89, 235, 38, 106, 190, 217,
      112, 144, 171, 230, 49, 83, 245, 4, 12, 20, 60, 68, 204, 79, 209, 104,
      184, 211, 110, 178, 205, 76, 212, 103, 169, 224, 59, 77, 215, 98, 166,
      241, 8, 24, 40, 120, 136, 131, 158, 185, 208, 107, 189, 220, 127, 129,
      152, 179, 206, 73, 219, 118, 154, 181, 196, 87, 249, 16, 48, 80, 240, 11,
      29, 39, 105, 187, 214, 97, 163, 254, 25, 43, 125, 135, 146, 173, 236, 47,
      113, 147, 174, 233, 32, 96, 160, 251, 22, 58, 78, 210, 109, 183, 194, 93,
      231, 50, 86, 250, 21, 63, 65, 195, 94, 226, 61, 71, 201, 64, 192, 91, 237,
      44, 116, 156, 191, 218, 117, 159, 186, 213, 100, 172, 239, 42, 126, 130,
      157, 188, 223, 122, 142, 137, 128, 155, 182, 193, 88, 232, 35, 101, 175,
      234, 37, 111, 177, 200, 67, 197, 84, 252, 31, 33, 99, 165, 244, 7, 9, 27,
      45, 119, 153, 176, 203, 70, 202, 69, 207, 74, 222, 121, 139, 134, 145,
      168, 227, 62, 66, 198, 81, 243, 14, 18, 54, 90, 238, 41, 123, 141, 140,
      143, 138, 133, 148, 167, 242, 13, 23, 57, 75, 221, 124, 132, 151, 162,
      253, 28, 36, 108, 180, 199, 82, 246, 1);

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

    rcon: array [0 .. 29] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80,
      $1B, $36, $6C, $D8, $AB, $4D, $9A, $2F, $5E, $BC, $63, $C6, $97, $35, $6A,
      $D4, $B3, $7D, $FA, $EF, $C5, $91);

    shifts0: array [0 .. 4, 0 .. 3] of Byte = ((0, 8, 16, 24), (0, 8, 16, 24),
      (0, 8, 16, 24), (0, 8, 16, 32), (0, 8, 24, 32));

    shifts1: array [0 .. 4, 0 .. 3] of Byte = ((0, 24, 16, 8), (0, 32, 24, 16),
      (0, 40, 32, 24), (0, 48, 40, 24), (0, 56, 40, 32));

  var
    FForEncryption: Boolean;
    FBC, FROUNDS, FBlockBits: Int32;
    FBC_MASK, FA0, FA1, FA2, FA3: UInt64;
    FShifts0SC, FShifts1SC: TCryptoLibByteArray;
    FWorkingKey: TCryptoLibMatrixUInt64Array;

    function GetAlgorithmName: String; virtual;
    function GetIsPartialBlockOkay: Boolean; virtual;

    /// <summary>
    /// multiply two elements of GF(2^m) needed for MixColumn and
    /// InvMixColumn
    /// </summary>
    function Mul0x2(b: Int32): Byte; inline;

    function Mul0x3(b: Int32): Byte; inline;

    function Mul0x9(b: Int32): Byte; inline;

    function Mul0xb(b: Int32): Byte; inline;

    function Mul0xd(b: Int32): Byte; inline;

    function Mul0xe(b: Int32): Byte; inline;

    /// <summary>
    /// xor corresponding text input and round key input bytes
    /// </summary>
    procedure KeyAddition(const rk: TCryptoLibUInt64Array); inline;

    /// <summary>
    /// rotate right custom
    /// </summary>
    function Shift(r: UInt64; Shift: Int32): UInt64; inline;

    /// <summary>
    /// Row 0 remains unchanged <br />The other three rows are shifted a
    /// variable amount
    /// </summary>
    procedure ShiftRow(const shiftsSC: TCryptoLibByteArray); inline;

    function ApplyS(r: UInt64; box: PByte): UInt64; inline;

    /// <summary>
    /// Replace every byte of the input by the byte at that place <br />in
    /// the nonlinear S-box
    /// </summary>
    procedure Substitution(box: PByte); inline;

    /// <summary>
    /// Mix the bytes of every column in a linear way
    /// </summary>
    procedure MixColumn();

    /// <summary>
    /// Mix the bytes of every column in a linear way <br />This is the
    /// opposite operation of Mixcolumn
    /// </summary>
    procedure InvMixColumn();

    /// <summary>
    /// Calculate the necessary round keys <br />The number of calculations
    /// depends on keyBits and blockBits
    /// </summary>
    function GenerateWorkingKey(const key: TCryptoLibByteArray)
      : TCryptoLibMatrixUInt64Array;

    procedure UnPackBlock(const bytes: TCryptoLibByteArray; off: Int32); inline;
    procedure PackBlock(const bytes: TCryptoLibByteArray; off: Int32); inline;

    procedure EncryptBlock(const rk: TCryptoLibMatrixUInt64Array);

    procedure DecryptBlock(const rk: TCryptoLibMatrixUInt64Array);

  public

    /// <summary>
    /// default constructor - 128 bit block size.
    /// </summary>
    constructor Create(); overload;

    /// <summary>
    /// basic constructor - set the cipher up for a given blocksize
    /// </summary>
    /// <param name="blockBits">
    /// the blocksize in bits, must be 128, 192, or 256.
    /// </param>
    constructor Create(blockBits: Int32); overload;

    /// <summary>
    /// initialise a Rijndael cipher.
    /// </summary>
    /// <param name="forEncryption">
    /// whether or not we are for encryption.
    /// </param>
    /// <param name="parameters">
    /// the parameters required to set up the cipher.
    /// </param>
    /// <exception cref="ClpCryptoLibTypes|EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); virtual;

    function ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32; virtual;

    procedure Reset(); virtual;

    function GetBlockSize(): Int32; virtual;

    property AlgorithmName: String read GetAlgorithmName;
    property IsPartialBlockOkay: Boolean read GetIsPartialBlockOkay;

  end;

implementation

{ TRijndaelEngine }

function TRijndaelEngine.Mul0x2(b: Int32): Byte;
begin
  if (b <> 0) then
  begin
    result := Alogtable[25 + (Logtable[b] and $FF)];
  end
  else
  begin
    result := 0;
  end;
end;

function TRijndaelEngine.Mul0x3(b: Int32): Byte;
begin
  if (b <> 0) then
  begin
    result := Alogtable[1 + (Logtable[b] and $FF)];
  end
  else
  begin
    result := 0;
  end;
end;

function TRijndaelEngine.Mul0x9(b: Int32): Byte;
begin
  if (b >= 0) then
  begin
    result := Alogtable[199 + b];
  end
  else
  begin
    result := 0;
  end;
end;

function TRijndaelEngine.Mul0xb(b: Int32): Byte;
begin
  if (b >= 0) then
  begin
    result := Alogtable[104 + b];
  end
  else
  begin
    result := 0;
  end;
end;

function TRijndaelEngine.Mul0xd(b: Int32): Byte;
begin
  if (b >= 0) then
  begin
    result := Alogtable[238 + b];
  end
  else
  begin
    result := 0;
  end;
end;

function TRijndaelEngine.Mul0xe(b: Int32): Byte;
begin
  if (b >= 0) then
  begin
    result := Alogtable[223 + b];
  end
  else
  begin
    result := 0;
  end;
end;

procedure TRijndaelEngine.KeyAddition(const rk: TCryptoLibUInt64Array);
begin
  FA0 := FA0 xor rk[0];
  FA1 := FA1 xor rk[1];
  FA2 := FA2 xor rk[2];
  FA3 := FA3 xor rk[3];
end;

function TRijndaelEngine.Shift(r: UInt64; Shift: Int32): UInt64;
begin
  result := (((r shr Shift) or (r shl (FBC - Shift)))) and FBC_MASK;
end;

procedure TRijndaelEngine.ShiftRow(const shiftsSC: TCryptoLibByteArray);
begin
  FA1 := Shift(FA1, shiftsSC[1]);
  FA2 := Shift(FA2, shiftsSC[2]);
  FA3 := Shift(FA3, shiftsSC[3]);
end;

function TRijndaelEngine.ApplyS(r: UInt64; box: PByte): UInt64;
var
  j: Int32;
begin
  result := 0;
  j := 0;
  while j < FBC do
  begin
    result := result or (UInt64(box[((r shr j) and $FF)] and $FF) shl j);
    System.Inc(j, 8);
  end;
end;

procedure TRijndaelEngine.Substitution(box: PByte);
begin
  FA0 := ApplyS(FA0, box);
  FA1 := ApplyS(FA1, box);
  FA2 := ApplyS(FA2, box);
  FA3 := ApplyS(FA3, box);
end;

procedure TRijndaelEngine.MixColumn;
var
  r0, r1, r2, r3: UInt64;
  a0, a1, a2, a3, j: Int32;
begin
  r0 := 0;
  r1 := 0;
  r2 := 0;
  r3 := 0;
  j := 0;
  while j < FBC do
  begin

    a0 := Int32((FA0 shr j) and $FF);
    a1 := Int32((FA1 shr j) and $FF);
    a2 := Int32((FA2 shr j) and $FF);
    a3 := Int32((FA3 shr j) and $FF);

    r0 := r0 or (UInt64(((Mul0x2(a0) xor Mul0x3(a1) xor a2 xor a3) and
      $FF)) shl j);

    r1 := r1 or (UInt64(((Mul0x2(a1) xor Mul0x3(a2) xor a3 xor a0) and
      $FF)) shl j);

    r2 := r2 or (UInt64(((Mul0x2(a2) xor Mul0x3(a3) xor a0 xor a1) and
      $FF)) shl j);

    r3 := r3 or (UInt64(((Mul0x2(a3) xor Mul0x3(a0) xor a1 xor a2) and
      $FF)) shl j);

    System.Inc(j, 8);
  end;

  FA0 := r0;
  FA1 := r1;
  FA2 := r2;
  FA3 := r3;
end;

procedure TRijndaelEngine.InvMixColumn;
var
  r0, r1, r2, r3: UInt64;
  a0, a1, a2, a3, j: Int32;
begin
  r0 := 0;
  r1 := 0;
  r2 := 0;
  r3 := 0;
  j := 0;
  while j < FBC do
  begin

    a0 := Int32((FA0 shr j) and $FF);
    a1 := Int32((FA1 shr j) and $FF);
    a2 := Int32((FA2 shr j) and $FF);
    a3 := Int32((FA3 shr j) and $FF);

    //
    // pre-lookup the log table
    //
    if (a0 <> 0) then
    begin
      a0 := (Logtable[a0 and $FF] and $FF);
    end
    else
    begin
      a0 := -1;
    end;

    if (a1 <> 0) then
    begin
      a1 := (Logtable[a1 and $FF] and $FF);
    end
    else
    begin
      a1 := -1;
    end;

    if (a2 <> 0) then
    begin
      a2 := (Logtable[a2 and $FF] and $FF);
    end
    else
    begin
      a2 := -1;
    end;

    if (a3 <> 0) then
    begin
      a3 := (Logtable[a3 and $FF] and $FF);
    end
    else
    begin
      a3 := -1;
    end;

    r0 := r0 or
      (UInt64(((Mul0xe(a0) xor Mul0xb(a1) xor Mul0xd(a2) xor Mul0x9(a3)) and
      $FF)) shl j);

    r1 := r1 or
      (UInt64(((Mul0xe(a1) xor Mul0xb(a2) xor Mul0xd(a3) xor Mul0x9(a0)) and
      $FF)) shl j);

    r2 := r2 or
      (UInt64(((Mul0xe(a2) xor Mul0xb(a3) xor Mul0xd(a0) xor Mul0x9(a1)) and
      $FF)) shl j);

    r3 := r3 or
      (UInt64(((Mul0xe(a3) xor Mul0xb(a0) xor Mul0xd(a1) xor Mul0x9(a2)) and
      $FF)) shl j);

    System.Inc(j, 8);
  end;

  FA0 := r0;
  FA1 := r1;
  FA2 := r2;
  FA3 := r3;
end;

function TRijndaelEngine.GenerateWorkingKey(const key: TCryptoLibByteArray)
  : TCryptoLibMatrixUInt64Array;
var
  KC, t, rconpointer, keyBits, i, index, j: Int32;
  tk: TCryptoLibMatrixByteArray;
  W: TCryptoLibMatrixUInt64Array;
begin
  rconpointer := 0;
  keyBits := System.Length(key) * 8;
  System.SetLength(tk, 4);
  for i := System.Low(tk) to System.High(tk) do
  begin
    System.SetLength(tk[i], MAXKC);
  end;

  System.SetLength(W, MAXROUNDS + 1);
  for i := System.Low(W) to System.High(W) do
  begin
    System.SetLength(W[i], 4);
  end;

  case keyBits of
    128:
      KC := 4;
    160:
      KC := 5;
    192:
      KC := 6;
    224:
      KC := 7;
    256:
      KC := 8
  else
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidKeyLength);
    end;
  end;

  if (keyBits >= FBlockBits) then
  begin
    FROUNDS := KC + 6;
  end
  else
  begin
    FROUNDS := (FBC div 8) + 6;
  end;

  //
  // copy the key into the processing area
  //
  index := 0;

  for i := 0 to System.Pred(System.Length(key)) do
  begin
    tk[i mod 4][i div 4] := key[index];
    System.Inc(index);
  end;

  t := 0;
  //
  // copy values into round key array
  //
  j := 0;

  while ((j < KC) and (t < ((FROUNDS + 1) * (FBC div 8)))) do
  begin
    for i := 0 to System.Pred(4) do
    begin
      W[t div (FBC div 8)][i] := W[t div (FBC div 8)][i] or
        (UInt64(tk[i][j] and $FF) shl ((t * 8) mod FBC));
    end;
    System.Inc(j);
    System.Inc(t);
  end;

  //
  // while not enough round key material calculated
  // calculate new values
  //
  while (t < ((FROUNDS + 1) * (FBC div 8))) do
  begin

    for i := 0 to System.Pred(4) do
    begin
      tk[i][0] := tk[i][0] xor (S[tk[(i + 1) mod 4][KC - 1] and $FF]);
    end;

    tk[0][0] := tk[0][0] xor Byte(rcon[rconpointer]);
    System.Inc(rconpointer);

    if (KC <= 6) then
    begin
      for j := 1 to System.Pred(KC) do
      begin
        for i := 0 to System.Pred(4) do
        begin
          tk[i][j] := tk[i][j] xor tk[i][j - 1];
        end;
      end;
    end
    else
    begin

      for j := 1 to System.Pred(4) do
      begin
        for i := 0 to System.Pred(4) do
        begin
          tk[i][j] := tk[i][j] xor tk[i][j - 1];
        end;
      end;

      for i := 0 to System.Pred(4) do
      begin
        tk[i][4] := tk[i][4] xor (S[tk[i][3] and $FF]);
      end;

      for j := 5 to System.Pred(KC) do
      begin
        for i := 0 to System.Pred(4) do
        begin
          tk[i][j] := tk[i][j] xor tk[i][j - 1];
        end;
      end;
    end;

    //
    // copy values into round key array
    //
    j := 0;

    while ((j < KC) and (t < ((FROUNDS + 1) * (FBC div 8)))) do
    begin
      for i := 0 to System.Pred(4) do
      begin
        W[t div (FBC div 8)][i] := W[t div (FBC div 8)][i] or
          (UInt64(tk[i][j] and $FF) shl ((t * 8) mod FBC));
      end;
      System.Inc(j);
      System.Inc(t);
    end;
  end;
  result := W;
end;

procedure TRijndaelEngine.PackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
var
  index, j: Int32;
begin
  index := off;
  j := 0;

  while j <> FBC do
  begin
    bytes[index] := Byte(FA0 shr j);
    System.Inc(index);
    bytes[index] := Byte(FA1 shr j);
    System.Inc(index);
    bytes[index] := Byte(FA2 shr j);
    System.Inc(index);
    bytes[index] := Byte(FA3 shr j);
    System.Inc(index);
    System.Inc(j, 8);
  end;
end;

procedure TRijndaelEngine.UnPackBlock(const bytes: TCryptoLibByteArray;
  off: Int32);
var
  index, j: Int32;
begin
  index := off;

  FA0 := UInt64(bytes[index] and $FF);
  System.Inc(index);
  FA1 := UInt64(bytes[index] and $FF);
  System.Inc(index);
  FA2 := UInt64(bytes[index] and $FF);
  System.Inc(index);
  FA3 := UInt64(bytes[index] and $FF);
  System.Inc(index);

  j := 8;

  while j <> FBC do
  begin
    FA0 := FA0 or (UInt64(bytes[index] and $FF) shl j);
    System.Inc(index);
    FA1 := FA1 or (UInt64(bytes[index] and $FF) shl j);
    System.Inc(index);
    FA2 := FA2 or (UInt64(bytes[index] and $FF) shl j);
    System.Inc(index);
    FA3 := FA3 or (UInt64(bytes[index] and $FF) shl j);
    System.Inc(index);
    System.Inc(j, 8);
  end;
end;

procedure TRijndaelEngine.EncryptBlock(const rk: TCryptoLibMatrixUInt64Array);
var
  r: Int32;
begin
  //
  // begin with a key addition
  //
  KeyAddition(rk[0]);

  //
  // ROUNDS-1 ordinary rounds
  //
  for r := 1 to System.Pred(FROUNDS) do
  begin
    Substitution(@(S[0]));
    ShiftRow(FShifts0SC);
    MixColumn();
    KeyAddition(rk[r]);
  end;

  //
  // Last round is special: there is no MixColumn
  //
  Substitution(@(S[0]));
  ShiftRow(FShifts0SC);
  KeyAddition(rk[FROUNDS]);
end;

procedure TRijndaelEngine.DecryptBlock(const rk: TCryptoLibMatrixUInt64Array);
var
  r: Int32;
begin
  // To decrypt: apply the inverse operations of the encrypt routine,
  // in opposite order
  //
  // (KeyAddition is an involution: it 's equal to its inverse)
  // (the inverse of Substitution with table S is Substitution with the inverse table of S)
  // (the inverse of Shiftrow is Shiftrow over a suitable distance)
  //

  // First the special round:
  // without InvMixColumn
  // with extra KeyAddition
  //
  KeyAddition(rk[FROUNDS]);
  Substitution(@(Si[0]));
  ShiftRow(FShifts1SC);

  //
  // ROUNDS-1 ordinary rounds
  //
  for r := System.Pred(FROUNDS) downto 1 do
  begin
    KeyAddition(rk[r]);
    InvMixColumn();
    Substitution(@(Si[0]));
    ShiftRow(FShifts1SC);
  end;

  //
  // End with the extra key addition
  //
  KeyAddition(rk[0]);
end;

constructor TRijndaelEngine.Create();
begin
  Create(128);
end;

constructor TRijndaelEngine.Create(blockBits: Int32);
begin
  Inherited Create();
  case blockBits of

    128:
      begin
        FBC := 32;
        FBC_MASK := $FFFFFFFF;
        System.SetLength(FShifts0SC, System.SizeOf(shifts0[0]));
        System.Move(shifts0[0], FShifts0SC[0], System.SizeOf(shifts0[0]));
        System.SetLength(FShifts1SC, System.SizeOf(shifts1[0]));
        System.Move(shifts1[0], FShifts1SC[0], System.SizeOf(shifts1[0]));
      end;

    160:
      begin
        FBC := 40;
        FBC_MASK := $FFFFFFFFFF;
        System.SetLength(FShifts0SC, System.SizeOf(shifts0[1]));
        System.Move(shifts0[1], FShifts0SC[0], System.SizeOf(shifts0[1]));
        System.SetLength(FShifts1SC, System.SizeOf(shifts1[1]));
        System.Move(shifts1[1], FShifts1SC[0], System.SizeOf(shifts1[1]));
      end;

    192:
      begin
        FBC := 48;
        FBC_MASK := $FFFFFFFFFFFF;
        System.SetLength(FShifts0SC, System.SizeOf(shifts0[2]));
        System.Move(shifts0[2], FShifts0SC[0], System.SizeOf(shifts0[2]));
        System.SetLength(FShifts1SC, System.SizeOf(shifts1[2]));
        System.Move(shifts1[2], FShifts1SC[0], System.SizeOf(shifts1[2]));
      end;

    224:
      begin
        FBC := 56;
        FBC_MASK := $FFFFFFFFFFFFFF;
        System.SetLength(FShifts0SC, System.SizeOf(shifts0[3]));
        System.Move(shifts0[3], FShifts0SC[0], System.SizeOf(shifts0[3]));
        System.SetLength(FShifts1SC, System.SizeOf(shifts1[3]));
        System.Move(shifts1[3], FShifts1SC[0], System.SizeOf(shifts1[3]));
      end;

    256:
      begin
        FBC := 64;
        FBC_MASK := UInt64($FFFFFFFFFFFFFFFF);
        System.SetLength(FShifts0SC, System.SizeOf(shifts0[4]));
        System.Move(shifts0[4], FShifts0SC[0], System.SizeOf(shifts0[4]));
        System.SetLength(FShifts1SC, System.SizeOf(shifts1[4]));
        System.Move(shifts1[4], FShifts1SC[0], System.SizeOf(shifts1[4]));
      end
  else
    begin
      raise EArgumentOutOfRangeCryptoLibException.CreateRes(@SUnsupportedBlock);
    end;

  end;

  FBlockBits := blockBits;
end;

function TRijndaelEngine.GetAlgorithmName: String;
begin
  result := 'Rijndael';
end;

function TRijndaelEngine.GetBlockSize: Int32;
begin
  result := FBC div 2;
end;

function TRijndaelEngine.GetIsPartialBlockOkay: Boolean;
begin
  result := False;
end;

procedure TRijndaelEngine.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  keyParameter: IKeyParameter;
begin
  if not Supports(parameters, IKeyParameter, keyParameter) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidParameterRijndaelInit, [(parameters as TObject).ToString]);
  end;

  FWorkingKey := GenerateWorkingKey(keyParameter.GetKey());

  FForEncryption := forEncryption;
end;

function TRijndaelEngine.ProcessBlock(const input: TCryptoLibByteArray;
  inOff: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if (FWorkingKey = Nil) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SRijndaelEngineNotInitialised);
  end;

  TCheck.DataLength(input, inOff, (FBC div 2), SInputBuffertooShort);
  TCheck.OutputLength(output, outOff, (FBC div 2), SOutputBuffertooShort);

  UnPackBlock(input, inOff);

  if (FForEncryption) then
  begin
    EncryptBlock(FWorkingKey);
  end
  else
  begin
    DecryptBlock(FWorkingKey);
  end;

  PackBlock(output, outOff);

  result := FBC div 2;
end;

procedure TRijndaelEngine.Reset;
begin

end;

end.
