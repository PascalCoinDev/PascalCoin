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

unit ClpAesEngine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIAesEngine,
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
  /// This version uses only one 256 word table for each, for a total of
  /// 2Kbytes, <br />adding 12 rotate operations per round to compute the
  /// values contained in the other tables from <br />the contents of the
  /// first.
  /// </para>
  /// <para>
  /// This file contains the middle performance version with 2Kbytes of
  /// static tables for round precomputation.
  /// </para>
  /// </summary>
  TAesEngine = class sealed(TInterfacedObject, IAesEngine, IBlockCipher)

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

    // precomputation tables of calculations for rounds
    T0: array [0 .. 255] of UInt32 = ($A56363C6, $847C7CF8, $997777EE,
      $8D7B7BF6, $0DF2F2FF, $BD6B6BD6, $B16F6FDE, $54C5C591, $50303060,
      $03010102, $A96767CE, $7D2B2B56, $19FEFEE7, $62D7D7B5, $E6ABAB4D,
      $9A7676EC, $45CACA8F, $9D82821F, $40C9C989, $877D7DFA, $15FAFAEF,
      $EB5959B2, $C947478E, $0BF0F0FB, $ECADAD41, $67D4D4B3, $FDA2A25F,
      $EAAFAF45, $BF9C9C23, $F7A4A453, $967272E4, $5BC0C09B, $C2B7B775,
      $1CFDFDE1, $AE93933D, $6A26264C, $5A36366C, $413F3F7E, $02F7F7F5,
      $4FCCCC83, $5C343468, $F4A5A551, $34E5E5D1, $08F1F1F9, $937171E2,
      $73D8D8AB, $53313162, $3F15152A, $0C040408, $52C7C795, $65232346,
      $5EC3C39D, $28181830, $A1969637, $0F05050A, $B59A9A2F, $0907070E,
      $36121224, $9B80801B, $3DE2E2DF, $26EBEBCD, $6927274E, $CDB2B27F,
      $9F7575EA, $1B090912, $9E83831D, $742C2C58, $2E1A1A34, $2D1B1B36,
      $B26E6EDC, $EE5A5AB4, $FBA0A05B, $F65252A4, $4D3B3B76, $61D6D6B7,
      $CEB3B37D, $7B292952, $3EE3E3DD, $712F2F5E, $97848413, $F55353A6,
      $68D1D1B9, $00000000, $2CEDEDC1, $60202040, $1FFCFCE3, $C8B1B179,
      $ED5B5BB6, $BE6A6AD4, $46CBCB8D, $D9BEBE67, $4B393972, $DE4A4A94,
      $D44C4C98, $E85858B0, $4ACFCF85, $6BD0D0BB, $2AEFEFC5, $E5AAAA4F,
      $16FBFBED, $C5434386, $D74D4D9A, $55333366, $94858511, $CF45458A,
      $10F9F9E9, $06020204, $817F7FFE, $F05050A0, $443C3C78, $BA9F9F25,
      $E3A8A84B, $F35151A2, $FEA3A35D, $C0404080, $8A8F8F05, $AD92923F,
      $BC9D9D21, $48383870, $04F5F5F1, $DFBCBC63, $C1B6B677, $75DADAAF,
      $63212142, $30101020, $1AFFFFE5, $0EF3F3FD, $6DD2D2BF, $4CCDCD81,
      $140C0C18, $35131326, $2FECECC3, $E15F5FBE, $A2979735, $CC444488,
      $3917172E, $57C4C493, $F2A7A755, $827E7EFC, $473D3D7A, $AC6464C8,
      $E75D5DBA, $2B191932, $957373E6, $A06060C0, $98818119, $D14F4F9E,
      $7FDCDCA3, $66222244, $7E2A2A54, $AB90903B, $8388880B, $CA46468C,
      $29EEEEC7, $D3B8B86B, $3C141428, $79DEDEA7, $E25E5EBC, $1D0B0B16,
      $76DBDBAD, $3BE0E0DB, $56323264, $4E3A3A74, $1E0A0A14, $DB494992,
      $0A06060C, $6C242448, $E45C5CB8, $5DC2C29F, $6ED3D3BD, $EFACAC43,
      $A66262C4, $A8919139, $A4959531, $37E4E4D3, $8B7979F2, $32E7E7D5,
      $43C8C88B, $5937376E, $B76D6DDA, $8C8D8D01, $64D5D5B1, $D24E4E9C,
      $E0A9A949, $B46C6CD8, $FA5656AC, $07F4F4F3, $25EAEACF, $AF6565CA,
      $8E7A7AF4, $E9AEAE47, $18080810, $D5BABA6F, $887878F0, $6F25254A,
      $722E2E5C, $241C1C38, $F1A6A657, $C7B4B473, $51C6C697, $23E8E8CB,
      $7CDDDDA1, $9C7474E8, $211F1F3E, $DD4B4B96, $DCBDBD61, $868B8B0D,
      $858A8A0F, $907070E0, $423E3E7C, $C4B5B571, $AA6666CC, $D8484890,
      $05030306, $01F6F6F7, $120E0E1C, $A36161C2, $5F35356A, $F95757AE,
      $D0B9B969, $91868617, $58C1C199, $271D1D3A, $B99E9E27, $38E1E1D9,
      $13F8F8EB, $B398982B, $33111122, $BB6969D2, $70D9D9A9, $898E8E07,
      $A7949433, $B69B9B2D, $221E1E3C, $92878715, $20E9E9C9, $49CECE87,
      $FF5555AA, $78282850, $7ADFDFA5, $8F8C8C03, $F8A1A159, $80898909,
      $170D0D1A, $DABFBF65, $31E6E6D7, $C6424284, $B86868D0, $C3414182,
      $B0999929, $772D2D5A, $110F0F1E, $CBB0B07B, $FC5454A8, $D6BBBB6D,
      $3A16162C);

    Tinv0: array [0 .. 255] of UInt32 = ($50A7F451, $5365417E, $C3A4171A,
      $965E273A, $CB6BAB3B, $F1459D1F, $AB58FAAC, $9303E34B, $55FA3020,
      $F66D76AD, $9176CC88, $254C02F5, $FCD7E54F, $D7CB2AC5, $80443526,
      $8FA362B5, $495AB1DE, $671BBA25, $980EEA45, $E1C0FE5D, $02752FC3,
      $12F04C81, $A397468D, $C6F9D36B, $E75F8F03, $959C9215, $EB7A6DBF,
      $DA595295, $2D83BED4, $D3217458, $2969E049, $44C8C98E, $6A89C275,
      $78798EF4, $6B3E5899, $DD71B927, $B64FE1BE, $17AD88F0, $66AC20C9,
      $B43ACE7D, $184ADF63, $82311AE5, $60335197, $457F5362, $E07764B1,
      $84AE6BBB, $1CA081FE, $942B08F9, $58684870, $19FD458F, $876CDE94,
      $B7F87B52, $23D373AB, $E2024B72, $578F1FE3, $2AAB5566, $0728EBB2,
      $03C2B52F, $9A7BC586, $A50837D3, $F2872830, $B2A5BF23, $BA6A0302,
      $5C8216ED, $2B1CCF8A, $92B479A7, $F0F207F3, $A1E2694E, $CDF4DA65,
      $D5BE0506, $1F6234D1, $8AFEA6C4, $9D532E34, $A055F3A2, $32E18A05,
      $75EBF6A4, $39EC830B, $AAEF6040, $069F715E, $51106EBD, $F98A213E,
      $3D06DD96, $AE053EDD, $46BDE64D, $B58D5491, $055DC471, $6FD40604,
      $FF155060, $24FB9819, $97E9BDD6, $CC434089, $779ED967, $BD42E8B0,
      $888B8907, $385B19E7, $DBEEC879, $470A7CA1, $E90F427C, $C91E84F8,
      $00000000, $83868009, $48ED2B32, $AC70111E, $4E725A6C, $FBFF0EFD,
      $5638850F, $1ED5AE3D, $27392D36, $64D90F0A, $21A65C68, $D1545B9B,
      $3A2E3624, $B1670A0C, $0FE75793, $D296EEB4, $9E919B1B, $4FC5C080,
      $A220DC61, $694B775A, $161A121C, $0ABA93E2, $E52AA0C0, $43E0223C,
      $1D171B12, $0B0D090E, $ADC78BF2, $B9A8B62D, $C8A91E14, $8519F157,
      $4C0775AF, $BBDD99EE, $FD607FA3, $9F2601F7, $BCF5725C, $C53B6644,
      $347EFB5B, $7629438B, $DCC623CB, $68FCEDB6, $63F1E4B8, $CADC31D7,
      $10856342, $40229713, $2011C684, $7D244A85, $F83DBBD2, $1132F9AE,
      $6DA129C7, $4B2F9E1D, $F330B2DC, $EC52860D, $D0E3C177, $6C16B32B,
      $99B970A9, $FA489411, $2264E947, $C48CFCA8, $1A3FF0A0, $D82C7D56,
      $EF903322, $C74E4987, $C1D138D9, $FEA2CA8C, $360BD498, $CF81F5A6,
      $28DE7AA5, $268EB7DA, $A4BFAD3F, $E49D3A2C, $0D927850, $9BCC5F6A,
      $62467E54, $C2138DF6, $E8B8D890, $5EF7392E, $F5AFC382, $BE805D9F,
      $7C93D069, $A92DD56F, $B31225CF, $3B99ACC8, $A77D1810, $6E639CE8,
      $7BBB3BDB, $097826CD, $F418596E, $01B79AEC, $A89A4F83, $656E95E6,
      $7EE6FFAA, $08CFBC21, $E6E815EF, $D99BE7BA, $CE366F4A, $D4099FEA,
      $D67CB029, $AFB2A431, $31233F2A, $3094A5C6, $C066A235, $37BC4E74,
      $A6CA82FC, $B0D090E0, $15D8A733, $4A9804F1, $F7DAEC41, $0E50CD7F,
      $2FF69117, $8DD64D76, $4DB0EF43, $544DAACC, $DF0496E4, $E3B5D19E,
      $1B886A4C, $B81F2CC1, $7F516546, $04EA5E9D, $5D358C01, $737487FA,
      $2E410BFB, $5A1D67B3, $52D2DB92, $335610E9, $1347D66D, $8C61D79A,
      $7A0CA137, $8E14F859, $893C13EB, $EE27A9CE, $35C961B7, $EDE51CE1,
      $3CB1477A, $59DFD29C, $3F73F255, $79CE1418, $BF37C773, $EACDF753,
      $5BAAFD5F, $146F3DDF, $86DB4478, $81F3AFCA, $3EC468B9, $2C342438,
      $5F40A3C2, $72C31D16, $0C25E2BC, $8B493C28, $41950DFF, $7101A839,
      $DEB30C08, $9CE4B4D8, $90C15664, $6184CB7B, $70B632D5, $745C6C48,
      $4257B8D0);

  var
    FROUNDS: Int32;
    FWorkingKey: TCryptoLibMatrixUInt32Array;
    FC0, FC1, FC2, FC3: UInt32;
    FforEncryption: Boolean;

    Fstate: array [0 .. 255] of Byte;

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

{ TAesEngine }

class function TAesEngine.Shift(r: UInt32; Shift: Int32): UInt32;
begin
  result := TBits.RotateRight32(r, Shift);
end;

class function TAesEngine.SubWord(x: UInt32): UInt32;
begin
  result := UInt32(S[x and 255]) or (UInt32(S[(x shr 8) and 255]) shl 8) or
    (UInt32(S[(x shr 16) and 255]) shl 16) or
    (UInt32(S[(x shr 24) and 255]) shl 24);
end;

procedure TAesEngine.DecryptBlock(const KW: TCryptoLibMatrixUInt32Array);
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
    lr0 := Tinv0[lt0 and 255] xor Shift(Tinv0[(lr3 shr 8) and 255], 24)
      xor Shift(Tinv0[(lt2 shr 16) and 255], 16)
      xor Shift(Tinv0[(lt1 shr 24) and 255], 8) xor lkw[0];
    lr1 := Tinv0[lt1 and 255] xor Shift(Tinv0[(lt0 shr 8) and 255], 24)
      xor Shift(Tinv0[(lr3 shr 16) and 255], 16)
      xor Shift(Tinv0[(lt2 shr 24) and 255], 8) xor lkw[1];
    lr2 := Tinv0[lt2 and 255] xor Shift(Tinv0[(lt1 shr 8) and 255], 24)
      xor Shift(Tinv0[(lt0 shr 16) and 255], 16)
      xor Shift(Tinv0[(lr3 shr 24) and 255], 8) xor lkw[2];
    lr3 := Tinv0[lr3 and 255] xor Shift(Tinv0[(lt2 shr 8) and 255], 24)
      xor Shift(Tinv0[(lt1 shr 16) and 255], 16)
      xor Shift(Tinv0[(lt0 shr 24) and 255], 8) xor lkw[3];
    lkw := KW[lr];
    System.Dec(lr);
    lt0 := Tinv0[lr0 and 255] xor Shift(Tinv0[(lr3 shr 8) and 255], 24)
      xor Shift(Tinv0[(lr2 shr 16) and 255], 16)
      xor Shift(Tinv0[(lr1 shr 24) and 255], 8) xor lkw[0];
    lt1 := Tinv0[lr1 and 255] xor Shift(Tinv0[(lr0 shr 8) and 255], 24)
      xor Shift(Tinv0[(lr3 shr 16) and 255], 16)
      xor Shift(Tinv0[(lr2 shr 24) and 255], 8) xor lkw[1];
    lt2 := Tinv0[lr2 and 255] xor Shift(Tinv0[(lr1 shr 8) and 255], 24)
      xor Shift(Tinv0[(lr0 shr 16) and 255], 16)
      xor Shift(Tinv0[(lr3 shr 24) and 255], 8) xor lkw[2];
    lr3 := Tinv0[lr3 and 255] xor Shift(Tinv0[(lr2 shr 8) and 255], 24)
      xor Shift(Tinv0[(lr1 shr 16) and 255], 16)
      xor Shift(Tinv0[(lr0 shr 24) and 255], 8) xor lkw[3];
  end;

  lkw := KW[1];
  lr0 := Tinv0[lt0 and 255] xor Shift(Tinv0[(lr3 shr 8) and 255], 24)
    xor Shift(Tinv0[(lt2 shr 16) and 255], 16)
    xor Shift(Tinv0[(lt1 shr 24) and 255], 8) xor lkw[0];
  lr1 := Tinv0[lt1 and 255] xor Shift(Tinv0[(lt0 shr 8) and 255], 24)
    xor Shift(Tinv0[(lr3 shr 16) and 255], 16)
    xor Shift(Tinv0[(lt2 shr 24) and 255], 8) xor lkw[1];
  lr2 := Tinv0[lt2 and 255] xor Shift(Tinv0[(lt1 shr 8) and 255], 24)
    xor Shift(Tinv0[(lt0 shr 16) and 255], 16)
    xor Shift(Tinv0[(lr3 shr 24) and 255], 8) xor lkw[2];
  lr3 := Tinv0[lr3 and 255] xor Shift(Tinv0[(lt2 shr 8) and 255], 24)
    xor Shift(Tinv0[(lt1 shr 16) and 255], 16)
    xor Shift(Tinv0[(lt0 shr 24) and 255], 8) xor lkw[3];

  // the final round's table is a simple function of Si so we don't use a whole other four tables for it

  lkw := KW[0];
  FC0 := UInt32(Si[lr0 and 255]) xor ((UInt32(Fstate[(lr3 shr 8) and 255]))
    shl 8) xor ((UInt32(Fstate[(lr2 shr 16) and 255])) shl 16)
    xor ((UInt32(Si[(lr1 shr 24) and 255])) shl 24) xor lkw[0];
  FC1 := UInt32(Fstate[lr1 and 255]) xor ((UInt32(Fstate[(lr0 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lr3 shr 16) and 255])) shl 16)
    xor ((UInt32(Fstate[(lr2 shr 24) and 255])) shl 24) xor lkw[1];
  FC2 := UInt32(Fstate[lr2 and 255]) xor ((UInt32(Si[(lr1 shr 8) and 255]))
    shl 8) xor ((UInt32(Si[(lr0 shr 16) and 255])) shl 16)
    xor ((UInt32(Fstate[(lr3 shr 24) and 255])) shl 24) xor lkw[2];
  FC3 := UInt32(Si[lr3 and 255]) xor ((UInt32(Fstate[(lr2 shr 8) and 255]))
    shl 8) xor ((UInt32(Fstate[(lr1 shr 16) and 255])) shl 16)
    xor ((UInt32(Fstate[(lr0 shr 24) and 255])) shl 24) xor lkw[3];

end;

procedure TAesEngine.EncryptBlock(const KW: TCryptoLibMatrixUInt32Array);
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
  while (lr < (FROUNDS - 1)) do
  begin
    lkw := KW[lr];
    System.Inc(lr);
    lr0 := T0[lt0 and 255] xor Shift(T0[(lt1 shr 8) and 255], 24)
      xor Shift(T0[(lt2 shr 16) and 255], 16)
      xor Shift(T0[(lr3 shr 24) and 255], 8) xor lkw[0];
    lr1 := T0[lt1 and 255] xor Shift(T0[(lt2 shr 8) and 255], 24)
      xor Shift(T0[(lr3 shr 16) and 255], 16)
      xor Shift(T0[(lt0 shr 24) and 255], 8) xor lkw[1];
    lr2 := T0[lt2 and 255] xor Shift(T0[(lr3 shr 8) and 255], 24)
      xor Shift(T0[(lt0 shr 16) and 255], 16)
      xor Shift(T0[(lt1 shr 24) and 255], 8) xor lkw[2];
    lr3 := T0[lr3 and 255] xor Shift(T0[(lt0 shr 8) and 255], 24)
      xor Shift(T0[(lt1 shr 16) and 255], 16)
      xor Shift(T0[(lt2 shr 24) and 255], 8) xor lkw[3];
    lkw := KW[lr];
    System.Inc(lr);
    lt0 := T0[lr0 and 255] xor Shift(T0[(lr1 shr 8) and 255], 24)
      xor Shift(T0[(lr2 shr 16) and 255], 16)
      xor Shift(T0[(lr3 shr 24) and 255], 8) xor lkw[0];
    lt1 := T0[lr1 and 255] xor Shift(T0[(lr2 shr 8) and 255], 24)
      xor Shift(T0[(lr3 shr 16) and 255], 16)
      xor Shift(T0[(lr0 shr 24) and 255], 8) xor lkw[1];
    lt2 := T0[lr2 and 255] xor Shift(T0[(lr3 shr 8) and 255], 24)
      xor Shift(T0[(lr0 shr 16) and 255], 16)
      xor Shift(T0[(lr1 shr 24) and 255], 8) xor lkw[2];
    lr3 := T0[lr3 and 255] xor Shift(T0[(lr0 shr 8) and 255], 24)
      xor Shift(T0[(lr1 shr 16) and 255], 16)
      xor Shift(T0[(lr2 shr 24) and 255], 8) xor lkw[3];
  end;

  lkw := KW[lr];
  System.Inc(lr);
  lr0 := T0[lt0 and 255] xor Shift(T0[(lt1 shr 8) and 255], 24)
    xor Shift(T0[(lt2 shr 16) and 255], 16) xor Shift(T0[(lr3 shr 24) and 255],
    8) xor lkw[0];
  lr1 := T0[lt1 and 255] xor Shift(T0[(lt2 shr 8) and 255], 24)
    xor Shift(T0[(lr3 shr 16) and 255], 16) xor Shift(T0[(lt0 shr 24) and 255],
    8) xor lkw[1];
  lr2 := T0[lt2 and 255] xor Shift(T0[(lr3 shr 8) and 255], 24)
    xor Shift(T0[(lt0 shr 16) and 255], 16) xor Shift(T0[(lt1 shr 24) and 255],
    8) xor lkw[2];
  lr3 := T0[lr3 and 255] xor Shift(T0[(lt0 shr 8) and 255], 24)
    xor Shift(T0[(lt1 shr 16) and 255], 16) xor Shift(T0[(lt2 shr 24) and 255],
    8) xor lkw[3];

  // the final round's table is a simple function of S so we don't use a whole other four tables for it

  lkw := KW[lr];
  FC0 := UInt32(S[lr0 and 255]) xor ((UInt32(S[(lr1 shr 8) and 255])) shl 8)
    xor ((UInt32(Fstate[(lr2 shr 16) and 255])) shl 16)
    xor ((UInt32(Fstate[(lr3 shr 24) and 255])) shl 24) xor lkw[0];
  FC1 := UInt32(Fstate[lr1 and 255]) xor ((UInt32(S[(lr2 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lr3 shr 16) and 255])) shl 16)
    xor ((UInt32(Fstate[(lr0 shr 24) and 255])) shl 24) xor lkw[1];
  FC2 := UInt32(Fstate[lr2 and 255]) xor ((UInt32(S[(lr3 shr 8) and 255]))
    shl 8) xor ((UInt32(S[(lr0 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr1 shr 24) and 255])) shl 24) xor lkw[2];
  FC3 := UInt32(Fstate[lr3 and 255]) xor ((UInt32(Fstate[(lr0 shr 8) and 255]))
    shl 8) xor ((UInt32(Fstate[(lr1 shr 16) and 255])) shl 16)
    xor ((UInt32(S[(lr2 shr 24) and 255])) shl 24) xor lkw[3];

end;

class function TAesEngine.FFmulX(x: UInt32): UInt32;
begin
  result := ((x and m2) shl 1) xor (((x and m1) shr 7) * m3);
end;

class function TAesEngine.FFmulX2(x: UInt32): UInt32;
var
  lt0, t1: UInt32;
begin
  lt0 := (x and m5) shl 2;
  t1 := (x and m4);
  t1 := t1 xor (t1 shr 1);
  result := lt0 xor (t1 shr 2) xor (t1 shr 5);
end;

class function TAesEngine.Inv_Mcol(x: UInt32): UInt32;
var
  lt0, t1: UInt32;
begin
  lt0 := x;
  t1 := lt0 xor Shift(lt0, 8);
  lt0 := lt0 xor FFmulX(t1);
  t1 := t1 xor FFmulX2(lt0);
  lt0 := lt0 xor (t1 xor Shift(t1, 16));
  result := lt0;
end;

function TAesEngine.GenerateWorkingKey(forEncryption: Boolean;
  const key: TCryptoLibByteArray): TCryptoLibMatrixUInt32Array;
var
  keyLen, KC, i, j: Int32;
  smallw: TCryptoLibUInt32Array;
  bigW: TCryptoLibMatrixUInt32Array;
  u, lrcon, lt0, t1, t2, t3, t4, t5, t6, t7: UInt32;

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
        lt0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := lt0;
        t1 := TConverters.ReadBytesAsUInt32LE(PByte(key), 4);
        bigW[0][1] := t1;
        t2 := TConverters.ReadBytesAsUInt32LE(PByte(key), 8);
        bigW[0][2] := t2;
        t3 := TConverters.ReadBytesAsUInt32LE(PByte(key), 12);
        bigW[0][3] := t3;

        for i := 1 to 10 do
        begin
          u := SubWord(Shift(t3, 8)) xor Rcon[i - 1];
          lt0 := lt0 xor u;
          bigW[i][0] := lt0;
          t1 := t1 xor lt0;
          bigW[i][1] := t1;
          t2 := t2 xor t1;
          bigW[i][2] := t2;
          t3 := t3 xor t2;
          bigW[i][3] := t3;
        end;
      end;

    6:
      begin
        lt0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := lt0;
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
        lt0 := lt0 xor u;
        bigW[1][2] := lt0;
        t1 := t1 xor lt0;
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
          lt0 := lt0 xor u;
          bigW[i][0] := lt0;
          t1 := t1 xor lt0;
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
          lt0 := lt0 xor u;
          bigW[i + 1][2] := lt0;
          t1 := t1 xor lt0;
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
        lt0 := lt0 xor u;
        bigW[12][0] := lt0;
        t1 := t1 xor lt0;
        bigW[12][1] := t1;
        t2 := t2 xor t1;
        bigW[12][2] := t2;
        t3 := t3 xor t2;
        bigW[12][3] := t3;
      end;

    8:
      begin
        lt0 := TConverters.ReadBytesAsUInt32LE(PByte(key), 0);
        bigW[0][0] := lt0;
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
          lt0 := lt0 xor u;
          bigW[i][0] := lt0;
          t1 := t1 xor lt0;
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
        lt0 := lt0 xor u;
        bigW[14][0] := lt0;
        t1 := t1 xor lt0;
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

function TAesEngine.GetAlgorithmName: String;
begin
  result := 'AES';
end;

function TAesEngine.GetBlockSize: Int32;
begin
  result := BLOCK_SIZE;
end;

function TAesEngine.GetIsPartialBlockOkay: Boolean;
begin
  result := false;
end;

procedure TAesEngine.Init(forEncryption: Boolean;
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

  if forEncryption then
  begin
    System.Move(S[System.Low(S)], Fstate[System.Low(Fstate)], System.SizeOf(S));
  end
  else
  begin
    System.Move(Si[System.Low(Si)], Fstate[System.Low(Fstate)],
      System.SizeOf(Si));
  end;

end;

procedure TAesEngine.PackBlock(const bytes: TCryptoLibByteArray; off: Int32);
begin
  TConverters.ReadUInt32AsBytesLE(FC0, bytes, off);
  TConverters.ReadUInt32AsBytesLE(FC1, bytes, off + 4);
  TConverters.ReadUInt32AsBytesLE(FC2, bytes, off + 8);
  TConverters.ReadUInt32AsBytesLE(FC3, bytes, off + 12);
end;

procedure TAesEngine.UnPackBlock(const bytes: TCryptoLibByteArray; off: Int32);
begin
  FC0 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off);
  FC1 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 4);
  FC2 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 8);
  FC3 := TConverters.ReadBytesAsUInt32LE(PByte(bytes), off + 12);
end;

function TAesEngine.ProcessBlock(const input: TCryptoLibByteArray; inOff: Int32;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
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

procedure TAesEngine.Reset;
begin
  // nothing to do.
end;

end.
