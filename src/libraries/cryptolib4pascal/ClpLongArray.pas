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

unit ClpLongArray;

{$I CryptoLib.inc}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Math,
  ClpBits,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SInvalidF2MFieldValue = 'Invalid F2M Field value, "bigInt"';

type
  TLongArray = record

  strict private

{$REGION 'Consts'}
  const

    /// <summary>
    /// This expands 8 bit indices into 16 bit contents (high bit 14), by
    /// inserting 0s between bits. In a binary field, this operation is the
    /// same as squaring an 8 bit number.
    /// </summary>
    INTERLEAVE2_TABLE: array [0 .. 255] of UInt16 = ($0000, $0001, $0004, $0005,
      $0010, $0011, $0014, $0015, $0040, $0041, $0044, $0045, $0050, $0051,
      $0054, $0055, $0100, $0101, $0104, $0105, $0110, $0111, $0114, $0115,
      $0140, $0141, $0144, $0145, $0150, $0151, $0154, $0155, $0400, $0401,
      $0404, $0405, $0410, $0411, $0414, $0415, $0440, $0441, $0444, $0445,
      $0450, $0451, $0454, $0455, $0500, $0501, $0504, $0505, $0510, $0511,
      $0514, $0515, $0540, $0541, $0544, $0545, $0550, $0551, $0554, $0555,
      $1000, $1001, $1004, $1005, $1010, $1011, $1014, $1015, $1040, $1041,
      $1044, $1045, $1050, $1051, $1054, $1055, $1100, $1101, $1104, $1105,
      $1110, $1111, $1114, $1115, $1140, $1141, $1144, $1145, $1150, $1151,
      $1154, $1155, $1400, $1401, $1404, $1405, $1410, $1411, $1414, $1415,
      $1440, $1441, $1444, $1445, $1450, $1451, $1454, $1455, $1500, $1501,
      $1504, $1505, $1510, $1511, $1514, $1515, $1540, $1541, $1544, $1545,
      $1550, $1551, $1554, $1555, $4000, $4001, $4004, $4005, $4010, $4011,
      $4014, $4015, $4040, $4041, $4044, $4045, $4050, $4051, $4054, $4055,
      $4100, $4101, $4104, $4105, $4110, $4111, $4114, $4115, $4140, $4141,
      $4144, $4145, $4150, $4151, $4154, $4155, $4400, $4401, $4404, $4405,
      $4410, $4411, $4414, $4415, $4440, $4441, $4444, $4445, $4450, $4451,
      $4454, $4455, $4500, $4501, $4504, $4505, $4510, $4511, $4514, $4515,
      $4540, $4541, $4544, $4545, $4550, $4551, $4554, $4555, $5000, $5001,
      $5004, $5005, $5010, $5011, $5014, $5015, $5040, $5041, $5044, $5045,
      $5050, $5051, $5054, $5055, $5100, $5101, $5104, $5105, $5110, $5111,
      $5114, $5115, $5140, $5141, $5144, $5145, $5150, $5151, $5154, $5155,
      $5400, $5401, $5404, $5405, $5410, $5411, $5414, $5415, $5440, $5441,
      $5444, $5445, $5450, $5451, $5454, $5455, $5500, $5501, $5504, $5505,
      $5510, $5511, $5514, $5515, $5540, $5541, $5544, $5545, $5550, $5551,
      $5554, $5555);

    /// <summary>
    /// This expands 7 bit indices into 21 bit contents (high bit 18),
    /// by inserting 0s between bits.
    /// </summary>
    INTERLEAVE3_TABLE: array [0 .. 127] of Int32 = ($00000, $00001, $00008,
      $00009, $00040, $00041, $00048, $00049, $00200, $00201, $00208, $00209,
      $00240, $00241, $00248, $00249, $01000, $01001, $01008, $01009, $01040,
      $01041, $01048, $01049, $01200, $01201, $01208, $01209, $01240, $01241,
      $01248, $01249, $08000, $08001, $08008, $08009, $08040, $08041, $08048,
      $08049, $08200, $08201, $08208, $08209, $08240, $08241, $08248, $08249,
      $09000, $09001, $09008, $09009, $09040, $09041, $09048, $09049, $09200,
      $09201, $09208, $09209, $09240, $09241, $09248, $09249, $40000, $40001,
      $40008, $40009, $40040, $40041, $40048, $40049, $40200, $40201, $40208,
      $40209, $40240, $40241, $40248, $40249, $41000, $41001, $41008, $41009,
      $41040, $41041, $41048, $41049, $41200, $41201, $41208, $41209, $41240,
      $41241, $41248, $41249, $48000, $48001, $48008, $48009, $48040, $48041,
      $48048, $48049, $48200, $48201, $48208, $48209, $48240, $48241, $48248,
      $48249, $49000, $49001, $49008, $49009, $49040, $49041, $49048, $49049,
      $49200, $49201, $49208, $49209, $49240, $49241, $49248, $49249);

    /// <summary>
    /// This expands 8 bit indices into 32 bit contents (high bit 28), by
    /// inserting 0s between bits.
    /// </summary>
    INTERLEAVE4_TABLE: array [0 .. 255] of Int32 = ($00000000, $00000001,
      $00000010, $00000011, $00000100, $00000101, $00000110, $00000111,
      $00001000, $00001001, $00001010, $00001011, $00001100, $00001101,
      $00001110, $00001111, $00010000, $00010001, $00010010, $00010011,
      $00010100, $00010101, $00010110, $00010111, $00011000, $00011001,
      $00011010, $00011011, $00011100, $00011101, $00011110, $00011111,
      $00100000, $00100001, $00100010, $00100011, $00100100, $00100101,
      $00100110, $00100111, $00101000, $00101001, $00101010, $00101011,
      $00101100, $00101101, $00101110, $00101111, $00110000, $00110001,
      $00110010, $00110011, $00110100, $00110101, $00110110, $00110111,
      $00111000, $00111001, $00111010, $00111011, $00111100, $00111101,
      $00111110, $00111111, $01000000, $01000001, $01000010, $01000011,
      $01000100, $01000101, $01000110, $01000111, $01001000, $01001001,
      $01001010, $01001011, $01001100, $01001101, $01001110, $01001111,
      $01010000, $01010001, $01010010, $01010011, $01010100, $01010101,
      $01010110, $01010111, $01011000, $01011001, $01011010, $01011011,
      $01011100, $01011101, $01011110, $01011111, $01100000, $01100001,
      $01100010, $01100011, $01100100, $01100101, $01100110, $01100111,
      $01101000, $01101001, $01101010, $01101011, $01101100, $01101101,
      $01101110, $01101111, $01110000, $01110001, $01110010, $01110011,
      $01110100, $01110101, $01110110, $01110111, $01111000, $01111001,
      $01111010, $01111011, $01111100, $01111101, $01111110, $01111111,
      $10000000, $10000001, $10000010, $10000011, $10000100, $10000101,
      $10000110, $10000111, $10001000, $10001001, $10001010, $10001011,
      $10001100, $10001101, $10001110, $10001111, $10010000, $10010001,
      $10010010, $10010011, $10010100, $10010101, $10010110, $10010111,
      $10011000, $10011001, $10011010, $10011011, $10011100, $10011101,
      $10011110, $10011111, $10100000, $10100001, $10100010, $10100011,
      $10100100, $10100101, $10100110, $10100111, $10101000, $10101001,
      $10101010, $10101011, $10101100, $10101101, $10101110, $10101111,
      $10110000, $10110001, $10110010, $10110011, $10110100, $10110101,
      $10110110, $10110111, $10111000, $10111001, $10111010, $10111011,
      $10111100, $10111101, $10111110, $10111111, $11000000, $11000001,
      $11000010, $11000011, $11000100, $11000101, $11000110, $11000111,
      $11001000, $11001001, $11001010, $11001011, $11001100, $11001101,
      $11001110, $11001111, $11010000, $11010001, $11010010, $11010011,
      $11010100, $11010101, $11010110, $11010111, $11011000, $11011001,
      $11011010, $11011011, $11011100, $11011101, $11011110, $11011111,
      $11100000, $11100001, $11100010, $11100011, $11100100, $11100101,
      $11100110, $11100111, $11101000, $11101001, $11101010, $11101011,
      $11101100, $11101101, $11101110, $11101111, $11110000, $11110001,
      $11110010, $11110011, $11110100, $11110101, $11110110, $11110111,
      $11111000, $11111001, $11111010, $11111011, $11111100, $11111101,
      $11111110, $11111111);

    /// <summary>
    /// This expands 7 bit indices into 35 bit contents (high bit 30), by
    /// inserting 0s between bits.
    /// </summary>
    INTERLEAVE5_TABLE: array [0 .. 127] of Int32 = ($00000000, $00000001,
      $00000020, $00000021, $00000400, $00000401, $00000420, $00000421,
      $00008000, $00008001, $00008020, $00008021, $00008400, $00008401,
      $00008420, $00008421, $00100000, $00100001, $00100020, $00100021,
      $00100400, $00100401, $00100420, $00100421, $00108000, $00108001,
      $00108020, $00108021, $00108400, $00108401, $00108420, $00108421,
      $02000000, $02000001, $02000020, $02000021, $02000400, $02000401,
      $02000420, $02000421, $02008000, $02008001, $02008020, $02008021,
      $02008400, $02008401, $02008420, $02008421, $02100000, $02100001,
      $02100020, $02100021, $02100400, $02100401, $02100420, $02100421,
      $02108000, $02108001, $02108020, $02108021, $02108400, $02108401,
      $02108420, $02108421, $40000000, $40000001, $40000020, $40000021,
      $40000400, $40000401, $40000420, $40000421, $40008000, $40008001,
      $40008020, $40008021, $40008400, $40008401, $40008420, $40008421,
      $40100000, $40100001, $40100020, $40100021, $40100400, $40100401,
      $40100420, $40100421, $40108000, $40108001, $40108020, $40108021,
      $40108400, $40108401, $40108420, $40108421, $42000000, $42000001,
      $42000020, $42000021, $42000400, $42000401, $42000420, $42000421,
      $42008000, $42008001, $42008020, $42008021, $42008400, $42008401,
      $42008420, $42008421, $42100000, $42100001, $42100020, $42100021,
      $42100400, $42100401, $42100420, $42100421, $42108000, $42108001,
      $42108020, $42108021, $42108400, $42108401, $42108420, $42108421);

    /// <summary>
    /// This expands 9 bit indices into 63 bit (long) contents (high bit
    /// 56), by inserting 0s between bits.
    /// </summary>
    INTERLEAVE7_TABLE: array [0 .. 511] of Int64 = (Int64($0000000000000000),
      Int64($0000000000000001), Int64($0000000000000080),
      Int64($0000000000000081), Int64($0000000000004000),
      Int64($0000000000004001), Int64($0000000000004080),
      Int64($0000000000004081), Int64($0000000000200000),
      Int64($0000000000200001), Int64($0000000000200080),
      Int64($0000000000200081), Int64($0000000000204000),
      Int64($0000000000204001), Int64($0000000000204080),
      Int64($0000000000204081), Int64($0000000010000000),
      Int64($0000000010000001), Int64($0000000010000080),
      Int64($0000000010000081), Int64($0000000010004000),
      Int64($0000000010004001), Int64($0000000010004080),
      Int64($0000000010004081), Int64($0000000010200000),
      Int64($0000000010200001), Int64($0000000010200080),
      Int64($0000000010200081), Int64($0000000010204000),
      Int64($0000000010204001), Int64($0000000010204080),
      Int64($0000000010204081), Int64($0000000800000000),
      Int64($0000000800000001), Int64($0000000800000080),
      Int64($0000000800000081), Int64($0000000800004000),
      Int64($0000000800004001), Int64($0000000800004080),
      Int64($0000000800004081), Int64($0000000800200000),
      Int64($0000000800200001), Int64($0000000800200080),
      Int64($0000000800200081), Int64($0000000800204000),
      Int64($0000000800204001), Int64($0000000800204080),
      Int64($0000000800204081), Int64($0000000810000000),
      Int64($0000000810000001), Int64($0000000810000080),
      Int64($0000000810000081), Int64($0000000810004000),
      Int64($0000000810004001), Int64($0000000810004080),
      Int64($0000000810004081), Int64($0000000810200000),
      Int64($0000000810200001), Int64($0000000810200080),
      Int64($0000000810200081), Int64($0000000810204000),
      Int64($0000000810204001), Int64($0000000810204080),
      Int64($0000000810204081), Int64($0000040000000000),
      Int64($0000040000000001), Int64($0000040000000080),
      Int64($0000040000000081), Int64($0000040000004000),
      Int64($0000040000004001), Int64($0000040000004080),
      Int64($0000040000004081), Int64($0000040000200000),
      Int64($0000040000200001), Int64($0000040000200080),
      Int64($0000040000200081), Int64($0000040000204000),
      Int64($0000040000204001), Int64($0000040000204080),
      Int64($0000040000204081), Int64($0000040010000000),
      Int64($0000040010000001), Int64($0000040010000080),
      Int64($0000040010000081), Int64($0000040010004000),
      Int64($0000040010004001), Int64($0000040010004080),
      Int64($0000040010004081), Int64($0000040010200000),
      Int64($0000040010200001), Int64($0000040010200080),
      Int64($0000040010200081), Int64($0000040010204000),
      Int64($0000040010204001), Int64($0000040010204080),
      Int64($0000040010204081), Int64($0000040800000000),
      Int64($0000040800000001), Int64($0000040800000080),
      Int64($0000040800000081), Int64($0000040800004000),
      Int64($0000040800004001), Int64($0000040800004080),
      Int64($0000040800004081), Int64($0000040800200000),
      Int64($0000040800200001), Int64($0000040800200080),
      Int64($0000040800200081), Int64($0000040800204000),
      Int64($0000040800204001), Int64($0000040800204080),
      Int64($0000040800204081), Int64($0000040810000000),
      Int64($0000040810000001), Int64($0000040810000080),
      Int64($0000040810000081), Int64($0000040810004000),
      Int64($0000040810004001), Int64($0000040810004080),
      Int64($0000040810004081), Int64($0000040810200000),
      Int64($0000040810200001), Int64($0000040810200080),
      Int64($0000040810200081), Int64($0000040810204000),
      Int64($0000040810204001), Int64($0000040810204080),
      Int64($0000040810204081), Int64($0002000000000000),
      Int64($0002000000000001), Int64($0002000000000080),
      Int64($0002000000000081), Int64($0002000000004000),
      Int64($0002000000004001), Int64($0002000000004080),
      Int64($0002000000004081), Int64($0002000000200000),
      Int64($0002000000200001), Int64($0002000000200080),
      Int64($0002000000200081), Int64($0002000000204000),
      Int64($0002000000204001), Int64($0002000000204080),
      Int64($0002000000204081), Int64($0002000010000000),
      Int64($0002000010000001), Int64($0002000010000080),
      Int64($0002000010000081), Int64($0002000010004000),
      Int64($0002000010004001), Int64($0002000010004080),
      Int64($0002000010004081), Int64($0002000010200000),
      Int64($0002000010200001), Int64($0002000010200080),
      Int64($0002000010200081), Int64($0002000010204000),
      Int64($0002000010204001), Int64($0002000010204080),
      Int64($0002000010204081), Int64($0002000800000000),
      Int64($0002000800000001), Int64($0002000800000080),
      Int64($0002000800000081), Int64($0002000800004000),
      Int64($0002000800004001), Int64($0002000800004080),
      Int64($0002000800004081), Int64($0002000800200000),
      Int64($0002000800200001), Int64($0002000800200080),
      Int64($0002000800200081), Int64($0002000800204000),
      Int64($0002000800204001), Int64($0002000800204080),
      Int64($0002000800204081), Int64($0002000810000000),
      Int64($0002000810000001), Int64($0002000810000080),
      Int64($0002000810000081), Int64($0002000810004000),
      Int64($0002000810004001), Int64($0002000810004080),
      Int64($0002000810004081), Int64($0002000810200000),
      Int64($0002000810200001), Int64($0002000810200080),
      Int64($0002000810200081), Int64($0002000810204000),
      Int64($0002000810204001), Int64($0002000810204080),
      Int64($0002000810204081), Int64($0002040000000000),
      Int64($0002040000000001), Int64($0002040000000080),
      Int64($0002040000000081), Int64($0002040000004000),
      Int64($0002040000004001), Int64($0002040000004080),
      Int64($0002040000004081), Int64($0002040000200000),
      Int64($0002040000200001), Int64($0002040000200080),
      Int64($0002040000200081), Int64($0002040000204000),
      Int64($0002040000204001), Int64($0002040000204080),
      Int64($0002040000204081), Int64($0002040010000000),
      Int64($0002040010000001), Int64($0002040010000080),
      Int64($0002040010000081), Int64($0002040010004000),
      Int64($0002040010004001), Int64($0002040010004080),
      Int64($0002040010004081), Int64($0002040010200000),
      Int64($0002040010200001), Int64($0002040010200080),
      Int64($0002040010200081), Int64($0002040010204000),
      Int64($0002040010204001), Int64($0002040010204080),
      Int64($0002040010204081), Int64($0002040800000000),
      Int64($0002040800000001), Int64($0002040800000080),
      Int64($0002040800000081), Int64($0002040800004000),
      Int64($0002040800004001), Int64($0002040800004080),
      Int64($0002040800004081), Int64($0002040800200000),
      Int64($0002040800200001), Int64($0002040800200080),
      Int64($0002040800200081), Int64($0002040800204000),
      Int64($0002040800204001), Int64($0002040800204080),
      Int64($0002040800204081), Int64($0002040810000000),
      Int64($0002040810000001), Int64($0002040810000080),
      Int64($0002040810000081), Int64($0002040810004000),
      Int64($0002040810004001), Int64($0002040810004080),
      Int64($0002040810004081), Int64($0002040810200000),
      Int64($0002040810200001), Int64($0002040810200080),
      Int64($0002040810200081), Int64($0002040810204000),
      Int64($0002040810204001), Int64($0002040810204080),
      Int64($0002040810204081), Int64($0100000000000000),
      Int64($0100000000000001), Int64($0100000000000080),
      Int64($0100000000000081), Int64($0100000000004000),
      Int64($0100000000004001), Int64($0100000000004080),
      Int64($0100000000004081), Int64($0100000000200000),
      Int64($0100000000200001), Int64($0100000000200080),
      Int64($0100000000200081), Int64($0100000000204000),
      Int64($0100000000204001), Int64($0100000000204080),
      Int64($0100000000204081), Int64($0100000010000000),
      Int64($0100000010000001), Int64($0100000010000080),
      Int64($0100000010000081), Int64($0100000010004000),
      Int64($0100000010004001), Int64($0100000010004080),
      Int64($0100000010004081), Int64($0100000010200000),
      Int64($0100000010200001), Int64($0100000010200080),
      Int64($0100000010200081), Int64($0100000010204000),
      Int64($0100000010204001), Int64($0100000010204080),
      Int64($0100000010204081), Int64($0100000800000000),
      Int64($0100000800000001), Int64($0100000800000080),
      Int64($0100000800000081), Int64($0100000800004000),
      Int64($0100000800004001), Int64($0100000800004080),
      Int64($0100000800004081), Int64($0100000800200000),
      Int64($0100000800200001), Int64($0100000800200080),
      Int64($0100000800200081), Int64($0100000800204000),
      Int64($0100000800204001), Int64($0100000800204080),
      Int64($0100000800204081), Int64($0100000810000000),
      Int64($0100000810000001), Int64($0100000810000080),
      Int64($0100000810000081), Int64($0100000810004000),
      Int64($0100000810004001), Int64($0100000810004080),
      Int64($0100000810004081), Int64($0100000810200000),
      Int64($0100000810200001), Int64($0100000810200080),
      Int64($0100000810200081), Int64($0100000810204000),
      Int64($0100000810204001), Int64($0100000810204080),
      Int64($0100000810204081), Int64($0100040000000000),
      Int64($0100040000000001), Int64($0100040000000080),
      Int64($0100040000000081), Int64($0100040000004000),
      Int64($0100040000004001), Int64($0100040000004080),
      Int64($0100040000004081), Int64($0100040000200000),
      Int64($0100040000200001), Int64($0100040000200080),
      Int64($0100040000200081), Int64($0100040000204000),
      Int64($0100040000204001), Int64($0100040000204080),
      Int64($0100040000204081), Int64($0100040010000000),
      Int64($0100040010000001), Int64($0100040010000080),
      Int64($0100040010000081), Int64($0100040010004000),
      Int64($0100040010004001), Int64($0100040010004080),
      Int64($0100040010004081), Int64($0100040010200000),
      Int64($0100040010200001), Int64($0100040010200080),
      Int64($0100040010200081), Int64($0100040010204000),
      Int64($0100040010204001), Int64($0100040010204080),
      Int64($0100040010204081), Int64($0100040800000000),
      Int64($0100040800000001), Int64($0100040800000080),
      Int64($0100040800000081), Int64($0100040800004000),
      Int64($0100040800004001), Int64($0100040800004080),
      Int64($0100040800004081), Int64($0100040800200000),
      Int64($0100040800200001), Int64($0100040800200080),
      Int64($0100040800200081), Int64($0100040800204000),
      Int64($0100040800204001), Int64($0100040800204080),
      Int64($0100040800204081), Int64($0100040810000000),
      Int64($0100040810000001), Int64($0100040810000080),
      Int64($0100040810000081), Int64($0100040810004000),
      Int64($0100040810004001), Int64($0100040810004080),
      Int64($0100040810004081), Int64($0100040810200000),
      Int64($0100040810200001), Int64($0100040810200080),
      Int64($0100040810200081), Int64($0100040810204000),
      Int64($0100040810204001), Int64($0100040810204080),
      Int64($0100040810204081), Int64($0102000000000000),
      Int64($0102000000000001), Int64($0102000000000080),
      Int64($0102000000000081), Int64($0102000000004000),
      Int64($0102000000004001), Int64($0102000000004080),
      Int64($0102000000004081), Int64($0102000000200000),
      Int64($0102000000200001), Int64($0102000000200080),
      Int64($0102000000200081), Int64($0102000000204000),
      Int64($0102000000204001), Int64($0102000000204080),
      Int64($0102000000204081), Int64($0102000010000000),
      Int64($0102000010000001), Int64($0102000010000080),
      Int64($0102000010000081), Int64($0102000010004000),
      Int64($0102000010004001), Int64($0102000010004080),
      Int64($0102000010004081), Int64($0102000010200000),
      Int64($0102000010200001), Int64($0102000010200080),
      Int64($0102000010200081), Int64($0102000010204000),
      Int64($0102000010204001), Int64($0102000010204080),
      Int64($0102000010204081), Int64($0102000800000000),
      Int64($0102000800000001), Int64($0102000800000080),
      Int64($0102000800000081), Int64($0102000800004000),
      Int64($0102000800004001), Int64($0102000800004080),
      Int64($0102000800004081), Int64($0102000800200000),
      Int64($0102000800200001), Int64($0102000800200080),
      Int64($0102000800200081), Int64($0102000800204000),
      Int64($0102000800204001), Int64($0102000800204080),
      Int64($0102000800204081), Int64($0102000810000000),
      Int64($0102000810000001), Int64($0102000810000080),
      Int64($0102000810000081), Int64($0102000810004000),
      Int64($0102000810004001), Int64($0102000810004080),
      Int64($0102000810004081), Int64($0102000810200000),
      Int64($0102000810200001), Int64($0102000810200080),
      Int64($0102000810200081), Int64($0102000810204000),
      Int64($0102000810204001), Int64($0102000810204080),
      Int64($0102000810204081), Int64($0102040000000000),
      Int64($0102040000000001), Int64($0102040000000080),
      Int64($0102040000000081), Int64($0102040000004000),
      Int64($0102040000004001), Int64($0102040000004080),
      Int64($0102040000004081), Int64($0102040000200000),
      Int64($0102040000200001), Int64($0102040000200080),
      Int64($0102040000200081), Int64($0102040000204000),
      Int64($0102040000204001), Int64($0102040000204080),
      Int64($0102040000204081), Int64($0102040010000000),
      Int64($0102040010000001), Int64($0102040010000080),
      Int64($0102040010000081), Int64($0102040010004000),
      Int64($0102040010004001), Int64($0102040010004080),
      Int64($0102040010004081), Int64($0102040010200000),
      Int64($0102040010200001), Int64($0102040010200080),
      Int64($0102040010200081), Int64($0102040010204000),
      Int64($0102040010204001), Int64($0102040010204080),
      Int64($0102040010204081), Int64($0102040800000000),
      Int64($0102040800000001), Int64($0102040800000080),
      Int64($0102040800000081), Int64($0102040800004000),
      Int64($0102040800004001), Int64($0102040800004080),
      Int64($0102040800004081), Int64($0102040800200000),
      Int64($0102040800200001), Int64($0102040800200080),
      Int64($0102040800200081), Int64($0102040800204000),
      Int64($0102040800204001), Int64($0102040800204080),
      Int64($0102040800204081), Int64($0102040810000000),
      Int64($0102040810000001), Int64($0102040810000080),
      Int64($0102040810000081), Int64($0102040810004000),
      Int64($0102040810004001), Int64($0102040810004080),
      Int64($0102040810004081), Int64($0102040810200000),
      Int64($0102040810200001), Int64($0102040810200080),
      Int64($0102040810200081), Int64($0102040810204000),
      Int64($0102040810204001), Int64($0102040810204080),
      Int64($0102040810204081));

    // For toString(); must have length 64
    ZEROES: String =
      '0000000000000000000000000000000000000000000000000000000000000000';

{$ENDREGION}

    // TODO make m fixed for the LongArray, and hence compute T once and for all

  var

    Fm_ints: TCryptoLibInt64Array;

    function GetLength: Int32; inline;

    function DegreeFrom(limit: Int32): Int32;
    function ResizedInts(newLen: Int32): TCryptoLibInt64Array; inline;
    procedure AddShiftedByBitsSafe(const other: TLongArray;
      otherDegree, bits: Int32);

    class function ShiftUp(const x: TCryptoLibInt64Array;
      xOff, count, shift: Int32): Int64; overload; static;
    class function ShiftUp(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count, shift: Int32): Int64;
      overload; static;
    class function BitLength(w: Int64): Int32; static;
    class function AddShiftedUp(const x: TCryptoLibInt64Array; xOff: Int32;
      const y: TCryptoLibInt64Array; yOff, count, shift: Int32): Int64;
      static; inline;
    class function AddShiftedDown(const x: TCryptoLibInt64Array; xOff: Int32;
      const y: TCryptoLibInt64Array; yOff, count, shift: Int32): Int64;
      static; inline;
    class procedure Add(const x: TCryptoLibInt64Array; xOff: Int32;
      const y: TCryptoLibInt64Array; yOff, count: Int32); overload;
      static; inline;
    class procedure Add(const x: TCryptoLibInt64Array; xOff: Int32;
      const y: TCryptoLibInt64Array; yOff: Int32; const z: TCryptoLibInt64Array;
      zOff, count: Int32); overload; static; inline;

    class procedure AddBoth(const x: TCryptoLibInt64Array; xOff: Int32;
      const y1: TCryptoLibInt64Array; y1Off: Int32;
      const y2: TCryptoLibInt64Array; y2Off, count: Int32); static; inline;

    class procedure Distribute(const x: TCryptoLibInt64Array;
      src, dst1, dst2, count: Int32); static; inline;

    class procedure FlipWord(const buf: TCryptoLibInt64Array; off, bit: Int32;
      word: Int64); static; inline;

    class function TestBit(const buf: TCryptoLibInt64Array; off, n: Int32)
      : Boolean; static; inline;

    class procedure FlipBit(const buf: TCryptoLibInt64Array; off, n: Int32);
      static; inline;

    class procedure MultiplyWord(a: Int64; const b: TCryptoLibInt64Array;
      bLen: Int32; const c: TCryptoLibInt64Array; cOff: Int32); static; inline;

    class function ReduceResult(const buf: TCryptoLibInt64Array;
      off, len, m: Int32; const ks: TCryptoLibInt32Array): TLongArray;
      static; inline;

    class function ReduceInPlace(const buf: TCryptoLibInt64Array;
      off, len, m: Int32; const ks: TCryptoLibInt32Array): Int32; static;

    class procedure ReduceBitWise(const buf: TCryptoLibInt64Array;
      off, BitLength, m: Int32; const ks: TCryptoLibInt32Array); static; inline;

    class procedure ReduceBit(const buf: TCryptoLibInt64Array;
      off, bit, m: Int32; const ks: TCryptoLibInt32Array); static; inline;

    class procedure ReduceWordWise(const buf: TCryptoLibInt64Array;
      off, len, toBit, m: Int32; const ks: TCryptoLibInt32Array); static;

    class procedure ReduceWord(const buf: TCryptoLibInt64Array; off, bit: Int32;
      word: Int64; m: Int32; const ks: TCryptoLibInt32Array); static; inline;

    class procedure ReduceVectorWise(const buf: TCryptoLibInt64Array;
      off, len, words, m: Int32; const ks: TCryptoLibInt32Array);
      static; inline;

    class procedure FlipVector(const x: TCryptoLibInt64Array; xOff: Int32;
      const y: TCryptoLibInt64Array; yOff, yLen, bits: Int32); static;

    class procedure SquareInPlace(const x: TCryptoLibInt64Array; xLen, m: Int32;
      const ks: TCryptoLibInt32Array); static; inline;

    class procedure Interleave(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count, width: Int32); static; inline;

    class procedure Interleave3(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count: Int32); overload;
      static; inline;

    class function Interleave3(x: Int64): Int64; overload; static; inline;

    class function Interleave3_21to63(x: Int32): Int64; static; inline;

    class procedure Interleave5(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count: Int32); overload;
      static; inline;

    class function Interleave5(x: Int64): Int64; overload; static; inline;

    class function Interleave3_13to65(x: Int32): Int64; static; inline;

    class procedure Interleave7(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count: Int32); overload;
      static; inline;

    class function Interleave7(x: Int64): Int64; overload; static; inline;

    class procedure Interleave2_n(const x: TCryptoLibInt64Array; xOff: Int32;
      const z: TCryptoLibInt64Array; zOff, count, rounds: Int32); overload;
      static; inline;

    class function Interleave2_n(x: Int64; rounds: Int32): Int64; overload;
      static; inline;

    class function Interleave4_16to64(x: Int32): Int64; static; inline;

    class function Interleave2_32to64(x: Int32): Int64; static; inline;

    class function Int64ToBin(input: Int64): string; static;

  public
{$REGION 'Consts'}
    const
    BitLengths: array [0 .. 255] of Byte = (0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
      4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8);

{$ENDREGION}
    constructor Create(intLen: Int32); overload;
    constructor Create(const ints: TCryptoLibInt64Array); overload;
    constructor Create(const ints: TCryptoLibInt64Array;
      off, len: Int32); overload;
    constructor Create(const bigInt: TBigInteger); overload;

    procedure CopyTo(const z: TCryptoLibInt64Array; zOff: Int32);

    function IsOne(): Boolean; inline;
    function IsZero(): Boolean; inline;
    function GetUsedLength(): Int32; inline;
    function GetUsedLengthFrom(from: Int32): Int32;
    function Degree(): Int32; inline;
    function ToBigInteger(): TBigInteger;
    function AddOne(): TLongArray; inline;
    procedure AddShiftedByWords(const other: TLongArray; words: Int32); inline;

    function TestBitZero(): Boolean;

    function ModMultiplyLD(const other: TLongArray; m: Int32;
      const ks: TCryptoLibInt32Array): TLongArray;

    function ModMultiply(const other: TLongArray; m: Int32;
      const ks: TCryptoLibInt32Array): TLongArray;

    function ModMultiplyAlt(const other: TLongArray; m: Int32;
      const ks: TCryptoLibInt32Array): TLongArray;

    function ModReduce(m: Int32; const ks: TCryptoLibInt32Array)
      : TLongArray; inline;

    function Multiply(const other: TLongArray; m: Int32;
      const ks: TCryptoLibInt32Array): TLongArray;

    procedure Reduce(m: Int32; const ks: TCryptoLibInt32Array); inline;

    function ModSquare(m: Int32; const ks: TCryptoLibInt32Array)
      : TLongArray; inline;

    function ModSquareN(n, m: Int32; const ks: TCryptoLibInt32Array)
      : TLongArray; inline;

    function Square(m: Int32; const ks: TCryptoLibInt32Array)
      : TLongArray; inline;

    function ModInverse(m: Int32; const ks: TCryptoLibInt32Array): TLongArray;

    function Copy(): TLongArray; inline;

    function Equals(const other: TLongArray): Boolean; inline;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
    inline;
{$ENDIF DELPHI}
    function ToString(): String; inline;
    property Length: Int32 read GetLength;

  end;

implementation

{ TLongArray }

class function TLongArray.TestBit(const buf: TCryptoLibInt64Array;
  off, n: Int32): Boolean;
var
  theInt, theBit: Int32;
  tester: Int64;
begin
  // theInt = n / 64
  theInt := Int32(UInt32(n) shr 6);
  // theBit = n % 64
  theBit := n and $3F;
  tester := Int64(1) shl theBit;
  Result := (buf[off + theInt] and tester) <> 0;
end;

class procedure TLongArray.FlipBit(const buf: TCryptoLibInt64Array;
  off, n: Int32);
var
  theInt, theBit: Int32;
  flipper: Int64;
begin
  // theInt = n / 64
  theInt := Int32(UInt32(n) shr 6);
  // theBit = n % 64
  theBit := n and $3F;
  flipper := Int64(1) shl theBit;
  buf[off + theInt] := buf[off + theInt] xor flipper;
end;

class procedure TLongArray.ReduceBit(const buf: TCryptoLibInt64Array;
  off, bit, m: Int32; const ks: TCryptoLibInt32Array);
var
  n, j: Int32;
begin
  FlipBit(buf, off, bit);
  n := bit - m;
  j := System.Length(ks);
  System.Dec(j);
  while (j >= 0) do
  begin
    FlipBit(buf, off, ks[j] + n);
    System.Dec(j);
  end;
  FlipBit(buf, off, n);
end;

class procedure TLongArray.ReduceBitWise(const buf: TCryptoLibInt64Array;
  off, BitLength, m: Int32; const ks: TCryptoLibInt32Array);
begin
  System.Dec(BitLength);
  while (BitLength >= m) do
  begin
    if (TestBit(buf, off, BitLength)) then
    begin
      ReduceBit(buf, off, BitLength, m, ks);
    end;
    System.Dec(BitLength);
  end;
end;

class procedure TLongArray.ReduceVectorWise(const buf: TCryptoLibInt64Array;
  off, len, words, m: Int32; const ks: TCryptoLibInt32Array);
var
  baseBit, j: Int32;
begin
  // /*
  // * NOTE: It's important we go from highest coefficient to lowest, because for the highest
  // * one (only) we allow the ranges to partially overlap, and therefore any changes must take
  // * effect for the subsequent lower coefficients.
  // */
  baseBit := (words shl 6) - m;
  j := System.Length(ks);
  System.Dec(j);
  while (j >= 0) do
  begin
    FlipVector(buf, off, buf, off + words, len - words, baseBit + ks[j]);
    System.Dec(j);
  end;
  FlipVector(buf, off, buf, off + words, len - words, baseBit);
end;

class function TLongArray.ReduceInPlace(const buf: TCryptoLibInt64Array;
  off, len, m: Int32; const ks: TCryptoLibInt32Array): Int32;
var
  mLen, numBits, excessBits, kLen, kMax, kNext, wordWiseLimit, vectorableWords,
    vectorWiseWords: Int32;
begin
  mLen := TBits.Asr32((m + 63), 6);
  if (len < mLen) then
  begin
    Result := len;
    Exit;
  end;

  numBits := Math.Min(len shl 6, (m shl 1) - 1); // TODO use actual degree?
  excessBits := (len shl 6) - numBits;
  while (excessBits >= 64) do
  begin
    System.Dec(len);
    excessBits := excessBits - 64;
  end;

  kLen := System.Length(ks);
  kMax := ks[kLen - 1];
  if kLen > 1 then
  begin
    kNext := ks[kLen - 2];
  end
  else
  begin
    kNext := 0;
  end;

  wordWiseLimit := Math.Max(m, kMax + 64);
  vectorableWords := TBits.Asr32((excessBits + Math.Min(numBits - wordWiseLimit,
    m - kNext)), 6);
  if (vectorableWords > 1) then
  begin
    vectorWiseWords := len - vectorableWords;
    ReduceVectorWise(buf, off, len, vectorWiseWords, m, ks);
    while (len > vectorWiseWords) do
    begin
      System.Dec(len);
      buf[off + len] := Int64(0);
    end;
    numBits := vectorWiseWords shl 6;
  end;

  if (numBits > wordWiseLimit) then
  begin
    ReduceWordWise(buf, off, len, wordWiseLimit, m, ks);
    numBits := wordWiseLimit;
  end;

  if (numBits > m) then
  begin
    ReduceBitWise(buf, off, numBits, m, ks);
  end;

  Result := mLen;
end;

class function TLongArray.Interleave2_32to64(x: Int32): Int64;
var
  r00, r32: Int32;
begin
  r00 := INTERLEAVE2_TABLE[x and $FF] or INTERLEAVE2_TABLE
    [(UInt32(x) shr 8) and $FF] shl 16;
  r32 := INTERLEAVE2_TABLE[(UInt32(x) shr 16) and $FF] or
    INTERLEAVE2_TABLE[UInt32(x) shr 24] shl 16;
  Result := (r32 and Int64($FFFFFFFF)) shl 32 or (r00 and Int64($FFFFFFFF));
end;

class procedure TLongArray.SquareInPlace(const x: TCryptoLibInt64Array;
  xLen, m: Int32; const ks: TCryptoLibInt32Array);
var
  pos: Int32;
  xVal: Int64;
begin
  pos := xLen shl 1;
  System.Dec(xLen);

  while (xLen >= 0) do
  begin
    xVal := x[xLen];
    System.Dec(pos);
    x[pos] := Interleave2_32to64(Int32(UInt64(xVal) shr 32));
    System.Dec(pos);
    x[pos] := Interleave2_32to64(Int32(xVal));
    System.Dec(xLen);
  end;
end;

class function TLongArray.Interleave3_21to63(x: Int32): Int64;
var
  r00, r21, r42: Int32;
begin
  r00 := INTERLEAVE3_TABLE[x and $7F];
  r21 := INTERLEAVE3_TABLE[(UInt32(x) shr 7) and $7F];
  r42 := INTERLEAVE3_TABLE[UInt32(x) shr 14];
  Result := (r42 and Int64($FFFFFFFF)) shl 42 or (r21 and Int64($FFFFFFFF))
    shl 21 or (r00 and Int64($FFFFFFFF));
end;

class function TLongArray.Interleave3(x: Int64): Int64;
var
  z: Int64;
begin
  z := x and (Int64(1) shl 63);
  Result := z or Interleave3_21to63(Int32(x) and $1FFFFF) or
    Interleave3_21to63(Int32(UInt64(x) shr 21) and $1FFFFF) shl 1 or
    Interleave3_21to63(Int32(UInt64(x) shr 42) and $1FFFFF) shl 2;
end;

class procedure TLongArray.Interleave3(const x: TCryptoLibInt64Array;
  xOff: Int32; const z: TCryptoLibInt64Array; zOff, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    z[zOff + I] := Interleave3(x[xOff + I]);
  end;
end;

class function TLongArray.Interleave3_13to65(x: Int32): Int64;
var
  r00, r35: Int32;
begin
  r00 := INTERLEAVE5_TABLE[x and $7F];
  r35 := INTERLEAVE5_TABLE[UInt32(x) shr 7];
  Result := (r35 and Int64($FFFFFFFF)) shl 35 or (r00 and Int64($FFFFFFFF));
end;

class function TLongArray.Interleave5(x: Int64): Int64;
begin
  Result := Interleave3_13to65(Int32(x) and $1FFF) or
    Interleave3_13to65(Int32(UInt64(x) shr 13) and $1FFF) shl 1 or
    Interleave3_13to65(Int32(UInt64(x) shr 26) and $1FFF) shl 2 or
    Interleave3_13to65(Int32(UInt64(x) shr 39) and $1FFF) shl 3 or
    Interleave3_13to65(Int32(UInt64(x) shr 52) and $1FFF) shl 4;
end;

class procedure TLongArray.Interleave5(const x: TCryptoLibInt64Array;
  xOff: Int32; const z: TCryptoLibInt64Array; zOff, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    z[zOff + I] := Interleave5(x[xOff + I]);
  end;
end;

class function TLongArray.Interleave7(x: Int64): Int64;
var
  z: Int64;
begin
  z := x and (Int64(1) shl 63);
  Result := z or INTERLEAVE7_TABLE[Int32(x) and $1FF] or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 9) and $1FF] shl 1 or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 18) and $1FF] shl 2 or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 27) and $1FF] shl 3 or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 36) and $1FF] shl 4 or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 45) and $1FF] shl 5 or
    INTERLEAVE7_TABLE[Int32(UInt64(x) shr 54) and $1FF] shl 6;
end;

class procedure TLongArray.Interleave7(const x: TCryptoLibInt64Array;
  xOff: Int32; const z: TCryptoLibInt64Array; zOff, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    z[zOff + I] := Interleave7(x[xOff + I]);
  end;
end;

class function TLongArray.Interleave4_16to64(x: Int32): Int64;
var
  r00, r32: Int32;
begin
  r00 := INTERLEAVE4_TABLE[x and $FF];
  r32 := INTERLEAVE4_TABLE[UInt32(x) shr 8];
  Result := (r32 and Int64($FFFFFFFF)) shl 32 or (r00 and Int64($FFFFFFFF));
end;

class function TLongArray.Interleave2_n(x: Int64; rounds: Int32): Int64;
begin
  while (rounds > 1) do
  begin
    rounds := rounds - 2;
    x := Interleave4_16to64(Int32(x) and $FFFF) or
      Interleave4_16to64(Int32(UInt64(x) shr 16) and $FFFF) shl 1 or
      Interleave4_16to64(Int32(UInt64(x) shr 32) and $FFFF) shl 2 or
      Interleave4_16to64(Int32(UInt64(x) shr 48) and $FFFF) shl 3;
  end;
  if (rounds > 0) then
  begin
    x := Interleave2_32to64(Int32(x)) or
      Interleave2_32to64(Int32(UInt64(x) shr 32)) shl 1;
  end;
  Result := x;
end;

class procedure TLongArray.Interleave2_n(const x: TCryptoLibInt64Array;
  xOff: Int32; const z: TCryptoLibInt64Array; zOff, count, rounds: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    z[zOff + I] := Interleave2_n(x[xOff + I], rounds);
  end;
end;

class procedure TLongArray.Add(const x: TCryptoLibInt64Array; xOff: Int32;
  const y: TCryptoLibInt64Array; yOff: Int32; const z: TCryptoLibInt64Array;
  zOff, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    z[zOff + I] := x[xOff + I] xor y[yOff + I];
  end;
end;

class procedure TLongArray.Add(const x: TCryptoLibInt64Array; xOff: Int32;
  const y: TCryptoLibInt64Array; yOff, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    x[xOff + I] := x[xOff + I] xor y[yOff + I];
  end;
end;

class procedure TLongArray.AddBoth(const x: TCryptoLibInt64Array; xOff: Int32;
  const y1: TCryptoLibInt64Array; y1Off: Int32; const y2: TCryptoLibInt64Array;
  y2Off, count: Int32);
var
  I: Int32;
begin
  for I := 0 to System.Pred(count) do
  begin
    x[xOff + I] := x[xOff + I] xor (y1[y1Off + I] xor y2[y2Off + I]);
  end;
end;

function TLongArray.GetUsedLength: Int32;
begin
  Result := GetUsedLengthFrom(System.Length(Fm_ints));
end;

function TLongArray.ResizedInts(newLen: Int32): TCryptoLibInt64Array;
begin
  System.SetLength(Result, newLen);
  System.Move(Fm_ints[0], Result[0], Math.Min(System.Length(Fm_ints), newLen) *
    System.SizeOf(Int64));
end;

function TLongArray.AddOne: TLongArray;
var
  resultLen: Int32;
  ints: TCryptoLibInt64Array;
begin
  if (System.Length(Fm_ints) = 0) then
  begin
    Result := TLongArray.Create(TCryptoLibInt64Array.Create(Int64(1)));
    Exit;
  end;

  resultLen := Math.Max(1, GetUsedLength());
  ints := ResizedInts(resultLen);
  ints[0] := ints[0] xor Int64(1);
  Result := TLongArray.Create(ints);
end;

class function TLongArray.AddShiftedUp(const x: TCryptoLibInt64Array;
  xOff: Int32; const y: TCryptoLibInt64Array; yOff, count, shift: Int32): Int64;
var
  shiftInv, I: Int32;
  prev, next: Int64;
begin
  shiftInv := 64 - shift;
  prev := 0;
  for I := 0 to System.Pred(count) do

  begin
    next := y[yOff + I];
    x[xOff + I] := x[xOff + I] xor ((next shl shift) or prev);
    prev := Int64(UInt64(next) shr shiftInv);
  end;
  Result := prev;
end;

procedure TLongArray.AddShiftedByBitsSafe(const other: TLongArray;
  otherDegree, bits: Int32);
var
  otherLen, words, shift: Int32;
  carry: Int64;
begin
  otherLen := Int32(UInt32(otherDegree + 63) shr 6);

  words := Int32(UInt32(bits) shr 6);
  shift := bits and $3F;

  if (shift = 0) then
  begin
    Add(Fm_ints, words, other.Fm_ints, 0, otherLen);
    Exit;
  end;

  carry := AddShiftedUp(Fm_ints, words, other.Fm_ints, 0, otherLen, shift);
  if (carry <> Int64(0)) then
  begin
    Fm_ints[otherLen + words] := Fm_ints[otherLen + words] xor carry;
  end;
end;

procedure TLongArray.AddShiftedByWords(const other: TLongArray; words: Int32);
var
  otherUsedLen, minLen: Int32;
begin
  otherUsedLen := other.GetUsedLength();
  if (otherUsedLen = 0) then
  begin
    Exit;
  end;

  minLen := otherUsedLen + words;
  if (minLen > System.Length(Fm_ints)) then
  begin
    Fm_ints := ResizedInts(minLen);
  end;

  Add(Fm_ints, words, other.Fm_ints, 0, otherUsedLen);
end;

class function TLongArray.AddShiftedDown(const x: TCryptoLibInt64Array;
  xOff: Int32; const y: TCryptoLibInt64Array; yOff, count, shift: Int32): Int64;
var
  shiftInv, I: Int32;
  prev, next: Int64;
begin
  shiftInv := 64 - shift;
  prev := 0;
  I := count;

  System.Dec(I);
  while I >= 0 do
  begin
    next := y[yOff + I];
    x[xOff + I] := x[xOff + I] xor (Int64(UInt64(next) shr shift) or prev);
    prev := next shl shiftInv;
    System.Dec(I);
  end;
  Result := prev;
end;

class function TLongArray.BitLength(w: Int64): Int32;
var
  u, b, t, k, v: Int32;
begin
  u := Int32(UInt64(w) shr 32);
  if (u = 0) then
  begin
    u := Int32(w);
    b := 0;
  end
  else
  begin
    b := 32;
  end;

  t := Int32(UInt32(u) shr 16);
  if (t = 0) then
  begin
    t := Int32(UInt32(u) shr 8);
    if (t = 0) then
    begin
      k := BitLengths[u];
    end
    else
    begin
      k := 8 + BitLengths[t];
    end;

  end
  else
  begin
    v := Int32(UInt32(t) shr 8);

    if (v = 0) then
    begin
      k := 16 + BitLengths[t];
    end
    else
    begin
      k := 24 + BitLengths[v];
    end;

  end;

  Result := b + k;
end;

function TLongArray.Copy: TLongArray;
begin
  Result := TLongArray.Create(System.Copy(Fm_ints));
end;

constructor TLongArray.Create(const ints: TCryptoLibInt64Array;
  off, len: Int32);
begin
  if ((off = 0) and (len = System.Length(ints))) then
  begin
    Fm_ints := ints;
  end
  else
  begin
    System.SetLength(Fm_ints, len);
    System.Move(ints[off], Fm_ints[0], len * System.SizeOf(Int64));
  end;
end;

constructor TLongArray.Create(const ints: TCryptoLibInt64Array);
begin
  Fm_ints := ints;
end;

constructor TLongArray.Create(intLen: Int32);
begin
  System.SetLength(Fm_ints, intLen);
end;

constructor TLongArray.Create(const bigInt: TBigInteger);
var
  barr: TCryptoLibByteArray;
  barrLen, barrStart, intLen, iarrJ, rem, barrI, I: Int32;
  temp: Int64;
  barrBarrI: UInt32;
begin
  if (not(bigInt.IsInitialized) or (bigInt.SignValue < 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidF2MFieldValue);
  end;

  if (bigInt.SignValue = 0) then
  begin
    Fm_ints := TCryptoLibInt64Array.Create(Int64(0));
    Exit;
  end;

  barr := bigInt.ToByteArray();
  barrLen := System.Length(barr);
  barrStart := 0;
  if (barr[0] = 0) then
  begin
    // First byte is 0 to enforce highest (=sign) bit is zero.
    // In this case ignore barr[0].
    System.Dec(barrLen);
    barrStart := 1;
  end;
  intLen := (barrLen + 7) div 8;
  System.SetLength(Fm_ints, intLen);

  iarrJ := intLen - 1;
  rem := (barrLen mod 8) + barrStart;
  temp := 0;
  barrI := barrStart;
  if (barrStart < rem) then
  begin
    while (barrI < rem) do
    begin
      temp := temp shl 8;
      barrBarrI := barr[barrI];
      temp := temp or barrBarrI;
      System.Inc(barrI);
    end;

    Fm_ints[iarrJ] := temp;
    System.Dec(iarrJ);

  end;

  while (iarrJ >= 0) do

  begin
    temp := 0;
    I := 0;
    while I < 8 do
    begin
      temp := temp shl 8;
      barrBarrI := barr[barrI];
      System.Inc(barrI);
      temp := temp or barrBarrI;
      System.Inc(I);
    end;
    Fm_ints[iarrJ] := temp;
    System.Dec(iarrJ);
  end;
end;

function TLongArray.Degree: Int32;
var
  I: Int32;
  w: Int64;
begin
  I := System.Length(Fm_ints);
  repeat
    if (I = 0) then
    begin
      Result := 0;
      Exit;
    end;
    System.Dec(I);
    w := Fm_ints[I];
  until (not(w = 0));

  Result := (I shl 6) + BitLength(w);
end;

function TLongArray.DegreeFrom(limit: Int32): Int32;
var
  I: Int32;
  w: Int64;
begin
  I := Int32((UInt32(limit) + 62) shr 6);
  repeat
    if (I = 0) then
    begin
      Result := 0;
      Exit;
    end;
    System.Dec(I);
    w := Fm_ints[I];
  until (not(w = 0));

  Result := (I shl 6) + BitLength(w);
end;

class procedure TLongArray.Distribute(const x: TCryptoLibInt64Array;
  src, dst1, dst2, count: Int32);
var
  I: Int32;
  v: Int64;
begin
  for I := 0 to System.Pred(count) do
  begin
    v := x[src + I];
    x[dst1 + I] := x[dst1 + I] xor v;
    x[dst2 + I] := x[dst2 + I] xor v;
  end;
end;

function TLongArray.Equals(const other: TLongArray): Boolean;
var
  usedLen, I: Int32;
begin
  usedLen := GetUsedLength();
  if (other.GetUsedLength() <> usedLen) then
  begin
    Result := false;
    Exit;
  end;

  for I := 0 to System.Pred(usedLen) do

  begin
    if (Fm_ints[I] <> other.Fm_ints[I]) then
    begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

class procedure TLongArray.FlipVector(const x: TCryptoLibInt64Array;
  xOff: Int32; const y: TCryptoLibInt64Array; yOff, yLen, bits: Int32);
var
  carry: Int64;
begin
  xOff := xOff + Int32(UInt32(bits) shr 6);
  bits := bits and $3F;

  if (bits = 0) then
  begin
    Add(x, xOff, y, yOff, yLen);
  end
  else
  begin
    carry := AddShiftedDown(x, xOff + 1, y, yOff, yLen, 64 - bits);
    x[xOff] := x[xOff] xor carry;
  end;
end;

class procedure TLongArray.FlipWord(const buf: TCryptoLibInt64Array;
  off, bit: Int32; word: Int64);
var
  n, shift: Int32;
begin
  n := off + Int32(UInt32(bit) shr 6);
  shift := bit and $3F;
  if (shift = 0) then
  begin
    buf[n] := buf[n] xor word;
  end
  else
  begin
    buf[n] := buf[n] xor (word shl shift);
    word := Int64(UInt64(word) shr (64 - shift));
    if (word <> 0) then
    begin
      System.Inc(n);
      buf[n] := buf[n] xor word;
    end;
  end;
end;

function TLongArray.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}

var
  usedLen, hash, I: Int32;
  mi: Int64;
begin
  usedLen := GetUsedLength();
  hash := 1;
  for I := 0 to System.Pred(usedLen) do

  begin
    mi := Fm_ints[I];
    hash := hash * 31;
    hash := hash xor Int32(mi);
    hash := hash * 31;
    hash := hash xor Int32(UInt64(mi) shr 32);
  end;
  Result := hash;
end;

function TLongArray.GetLength: Int32;
begin
  Result := System.Length(Fm_ints);
end;

function TLongArray.GetUsedLengthFrom(from: Int32): Int32;
var
  a: TCryptoLibInt64Array;
begin
  a := Fm_ints;
  from := Math.Min(from, System.Length(a));

  if (from < 1) then
  begin
    Result := 0;
    Exit;
  end;

  // Check if first element will act as sentinel
  if (a[0] <> 0) then
  begin
    System.Dec(from);
    while (a[from] = 0) do
    begin
      System.Dec(from);
    end;
    Result := from + 1;
    Exit;
  end;

  repeat

    System.Dec(from);
    if (a[from] <> 0) then
    begin
      Result := from + 1;
      Exit;
    end;
    System.Dec(from);

  until (not(from > 0));

  Result := 0;
end;

class procedure TLongArray.Interleave(const x: TCryptoLibInt64Array;
  xOff: Int32; const z: TCryptoLibInt64Array; zOff, count, width: Int32);
begin
  case width of
    3:
      Interleave3(x, xOff, z, zOff, count);

    5:
      Interleave5(x, xOff, z, zOff, count);

    7:
      Interleave7(x, xOff, z, zOff, count);

  else
    Interleave2_n(x, xOff, z, zOff, count, BitLengths[width] - 1);

  end;

end;

class function TLongArray.Int64ToBin(input: Int64): string;
var
  bits: TCryptoLibCharArray;
  I: Int32;
begin

  Result := '';

  System.SetLength(bits, System.SizeOf(Int64) * 8);

  I := 0;

  while (input <> 0) do
  begin
    if (input and 1) = 1 then
      bits[I] := '1'
    else
    begin
      bits[I] := '0';
    end;
    System.Inc(I);
    input := input shr 1;
  end;

  System.SetString(Result, PChar(@bits[0]), I);

  Result := ReverseString(Result);

end;

procedure TLongArray.CopyTo(const z: TCryptoLibInt64Array; zOff: Int32);
begin
  System.Move(Fm_ints[0], z[zOff], System.Length(Fm_ints) *
    System.SizeOf(Int64));
end;

function TLongArray.IsOne: Boolean;
var
  a: TCryptoLibInt64Array;
  I: Int32;
begin
  a := Fm_ints;
  if (a[0] <> Int64(1)) then
  begin
    Result := false;
    Exit;
  end;

  for I := 1 to System.Pred(System.Length(a)) do
  begin
    if (a[I] <> Int64(0)) then
    begin
      Result := false;
      Exit;
    end;
  end;

  Result := true;
end;

function TLongArray.IsZero: Boolean;
var
  a: TCryptoLibInt64Array;
  I: Int32;
begin
  a := Fm_ints;
  for I := 0 to System.Pred(System.Length(a)) do
  begin
    if (a[I] <> Int64(0)) then
    begin
      Result := false;
      Exit;
    end;
  end;

  Result := true;
end;

function TLongArray.ModInverse(m: Int32; const ks: TCryptoLibInt32Array)
  : TLongArray;
var
  uzDegree, t, b, duv1, dgg1, j, duv2, dgg2: Int32;
  uz, vz, g1z, g2z: TLongArray;
  uvDeg, ggDeg: TCryptoLibInt32Array;
  uv, gg: TCryptoLibGenericArray<TLongArray>;
begin
  // /*
  // * Inversion in F2m using the extended Euclidean algorithm
  // *
  // * Input: A nonzero polynomial a(z) of degree at most m-1
  // * Output: a(z)^(-1) mod f(z)
  // */

  Result := Default (TLongArray); // to make FixInsight Happy:)
  uzDegree := Degree();
  if (uzDegree = 0) then
  begin
    raise EInvalidOperationCryptoLibException.Create('');
  end;
  if (uzDegree = 1) then
  begin
    Result := Self;
    Exit;
  end;

  // u(z) := a(z)
  uz := Copy();

  t := TBits.Asr32((m + 63), 6);

  // v(z) := f(z)
  vz := TLongArray.Create(t);
  ReduceBit(vz.Fm_ints, 0, m, m, ks);

  // g1(z) := 1, g2(z) := 0
  g1z := TLongArray.Create(t);
  g1z.Fm_ints[0] := Int64(1);
  g2z := TLongArray.Create(t);

  uvDeg := TCryptoLibInt32Array.Create(uzDegree, m + 1);
  uv := TCryptoLibGenericArray<TLongArray>.Create(uz, vz);

  ggDeg := TCryptoLibInt32Array.Create(1, 0);
  gg := TCryptoLibGenericArray<TLongArray>.Create(g1z, g2z);

  b := 1;
  duv1 := uvDeg[b];
  dgg1 := ggDeg[b];
  j := duv1 - uvDeg[1 - b];

  while true do

  begin
    if (j < 0) then
    begin
      j := -j;
      uvDeg[b] := duv1;
      ggDeg[b] := dgg1;
      b := 1 - b;
      duv1 := uvDeg[b];
      dgg1 := ggDeg[b];
    end;

    uv[b].AddShiftedByBitsSafe(uv[1 - b], uvDeg[1 - b], j);

    duv2 := uv[b].DegreeFrom(duv1);
    if (duv2 = 0) then
    begin
      Result := gg[1 - b];
      Exit;
    end;

    dgg2 := ggDeg[1 - b];
    gg[b].AddShiftedByBitsSafe(gg[1 - b], dgg2, j);
    dgg2 := dgg2 + j;

    if (dgg2 > dgg1) then
    begin
      dgg1 := dgg2;
    end
    else if (dgg2 = dgg1) then
    begin
      dgg1 := gg[b].DegreeFrom(dgg1);
    end;

    j := j + (duv2 - duv1);
    duv1 := duv2;
  end;
end;

class procedure TLongArray.MultiplyWord(a: Int64; const b: TCryptoLibInt64Array;
  bLen: Int32; const c: TCryptoLibInt64Array; cOff: Int32);
var
  k: Int32;
  carry: Int64;
begin
  if ((a and Int64(1)) <> Int64(0)) then
  begin
    Add(c, cOff, b, 0, bLen);
  end;
  k := 1;
  a := Int64(UInt64(a) shr 1);
  while (a <> Int64(0)) do
  begin
    if ((a and Int64(1)) <> Int64(0)) then
    begin
      carry := AddShiftedUp(c, cOff, b, 0, bLen, k);
      if (carry <> Int64(0)) then
      begin
        c[cOff + bLen] := c[cOff + bLen] xor carry;
      end;
    end;
    System.Inc(k);
    a := Int64(UInt64(a) shr 1);
  end;
end;

class function TLongArray.ReduceResult(const buf: TCryptoLibInt64Array;
  off, len, m: Int32; const ks: TCryptoLibInt32Array): TLongArray;
var
  rLen: Int32;
begin
  rLen := ReduceInPlace(buf, off, len, m, ks);
  Result := TLongArray.Create(buf, off, rLen);
end;

function TLongArray.ModMultiply(const other: TLongArray; m: Int32;
  const ks: TCryptoLibInt32Array): TLongArray;
var
  aDeg, bDeg, tmp, aLen, bLen, cLen, bMax, tOff, I, MASK, aPos, cOff,
    u, v: Int32;
  a, b: TLongArray;
  c0, T0, T1, aMInts, c: TCryptoLibInt64Array;
  ti: TCryptoLibInt32Array;
  a0, aVal: Int64;
begin
  // /*
  // * Find out the degree of each argument and handle the zero cases
  // */
  aDeg := Degree();
  if (aDeg = 0) then
  begin
    Result := Self;
    Exit;
  end;
  bDeg := other.Degree();
  if (bDeg = 0) then
  begin
    Result := other;
    Exit;
  end;

  // /*
  // * Swap if necessary so that A is the smaller argument
  // */
  a := Self;
  b := other;
  if (aDeg > bDeg) then
  begin
    a := other;
    b := Self;
    tmp := aDeg;
    aDeg := bDeg;
    bDeg := tmp;
  end;

  // /*
  // * Establish the word lengths of the arguments and result
  // */
  aLen := Int32(UInt32(aDeg + 63) shr 6);
  bLen := Int32(UInt32(bDeg + 63) shr 6);
  cLen := Int32(UInt32(aDeg + bDeg + 62) shr 6);

  if (aLen = 1) then
  begin
    a0 := a.Fm_ints[0];
    if (a0 = Int64(1)) then
    begin
      Result := b;
      Exit;
    end;

    // /*
    // * Fast path for small A, with performance dependent only on the number of set bits
    // */
    System.SetLength(c0, cLen);
    MultiplyWord(a0, b.Fm_ints, bLen, c0, 0);

    // /*
    // * Reduce the raw answer against the reduction coefficients
    // */
    Result := ReduceResult(c0, 0, cLen, m, ks);
    Exit;
  end;

  // /*
  // * Determine if B will get bigger during shifting
  // */
  bMax := Int32(UInt32(bDeg + 7 + 63) shr 6);

  // /*
  // * Lookup table for the offset of each B in the tables
  // */

  System.SetLength(ti, 16);

  // /*
  // * Precompute table of all 4-bit products of B
  // */

  System.SetLength(T0, bMax shl 4);
  tOff := bMax;
  ti[1] := tOff;
  System.Move(b.Fm_ints[0], T0[tOff], bLen * System.SizeOf(Int64));

  for I := 2 to System.Pred(16) do
  begin
    tOff := tOff + bMax;
    ti[I] := tOff;
    if ((I and 1) = 0) then
    begin
      ShiftUp(T0, Int32(UInt32(tOff) shr 1), T0, tOff, bMax, 1);
    end
    else
    begin
      Add(T0, bMax, T0, tOff - bMax, T0, tOff, bMax);
    end;
  end;

  // /*
  // * Second table with all 4-bit products of B shifted 4 bits
  // */

  System.SetLength(T1, System.Length(T0));

  ShiftUp(T0, 0, T1, 0, System.Length(T0), 4);
  // ShiftUp(T0, bMax, T1, bMax, tOff, 4);

  aMInts := a.Fm_ints;
  System.SetLength(c, cLen shl 3);

  MASK := $F;

  // /*
  // * Lopez-Dahab (Modified) algorithm
  // */

  for aPos := 0 to System.Pred(aLen) do
  begin
    aVal := aMInts[aPos];
    cOff := aPos;
    while true do

    begin
      u := Int32(aVal) and MASK;
      aVal := Int64(UInt64(aVal) shr 4);
      v := Int32(aVal) and MASK;
      AddBoth(c, cOff, T0, ti[u], T1, ti[v], bMax);
      aVal := Int64(UInt64(aVal) shr 4);
      if (aVal = Int64(0)) then
      begin
        break;
      end;
      cOff := cOff + cLen;
    end;
  end;

  cOff := System.Length(c);
  cOff := cOff - cLen;
  while (cOff <> 0) do
  begin
    AddShiftedUp(c, cOff - cLen, c, cOff, cLen, 8);
    cOff := cOff - cLen;
  end;

  // /*
  // * Finally the raw answer is collected, reduce it against the reduction coefficients
  // */
  Result := ReduceResult(c, 0, cLen, m, ks);
end;

function TLongArray.ModMultiplyAlt(const other: TLongArray; m: Int32;
  const ks: TCryptoLibInt32Array): TLongArray;
var
  aDeg, bDeg, tmp, aLen, bLen, cLen, width, positions, top, banks, shifts, bMax,
    bTotal, stride, cTotal, I, bOff, bank, MASK, k, aPos, index, ciPos: Int32;
  a0, aVal: Int64;
  a, b: TLongArray;
  c0, c: TCryptoLibInt64Array;
  ci: TCryptoLibInt32Array;
begin
  // /*
  // * Find out the degree of each argument and handle the zero cases
  // */
  aDeg := Degree();
  if (aDeg = 0) then
  begin
    Result := Self;
    Exit;
  end;
  bDeg := other.Degree();
  if (bDeg = 0) then
  begin
    Result := other;
    Exit;
  end;

  // /*
  // * Swap if necessary so that A is the smaller argument
  // */
  a := Self;
  b := other;
  if (aDeg > bDeg) then
  begin
    a := other;
    b := Self;
    tmp := aDeg;
    aDeg := bDeg;
    bDeg := tmp;
  end;

  // /*
  // * Establish the word lengths of the arguments and result
  // */
  aLen := Int32(UInt32(aDeg + 63) shr 6);
  bLen := Int32(UInt32(bDeg + 63) shr 6);
  cLen := Int32(UInt32(aDeg + bDeg + 62) shr 6);

  if (aLen = 1) then
  begin
    a0 := a.Fm_ints[0];
    if (a0 = Int64(1)) then
    begin
      Result := b;
      Exit;
    end;

    // /*
    // * Fast path for small A, with performance dependent only on the number of set bits
    // */
    System.SetLength(c0, cLen);
    MultiplyWord(a0, b.Fm_ints, bLen, c0, 0);

    // /*
    // * Reduce the raw answer against the reduction coefficients
    // */
    Result := ReduceResult(c0, 0, cLen, m, ks);
    Exit;
  end;

  // /*
  // * Determine the parameters of the Interleaved window algorithm: the 'width' in bits to
  // * process together, the number of evaluation 'positions' implied by that width, and the
  // * 'top' position at which the regular window algorithm stops.
  // */

  // NOTE: width 4 is the fastest over the entire range of sizes used in current crypto
  // width = 1; positions = 64; top = 64; banks = 4;
  // width = 2; positions = 32; top = 64; banks = 4;
  // width = 3; positions = 21; top = 63; banks = 3;
  width := 4;
  positions := 16;
  top := 64;
  banks := 8;
  // width = 5; positions = 13; top = 65; banks = 7;
  // width = 7; positions = 9; top = 63; banks = 9;
  // width = 8; positions = 8; top = 64; banks = 8;

  // /*
  // * Determine if B will get bigger during shifting
  // */
  if top < 64 then
  begin
    shifts := positions;
  end
  else
  begin
    shifts := positions - 1;
  end;

  bMax := Int32(UInt32(bDeg + shifts + 63) shr 6);

  bTotal := bMax * banks;
  stride := width * banks;

  // /*
  // * Create a single temporary buffer, with an offset table to find the positions of things in it
  // */
  System.SetLength(ci, 1 shl width);
  cTotal := aLen;

  ci[0] := cTotal;
  cTotal := cTotal + bTotal;
  ci[1] := cTotal;

  for I := 2 to System.Pred(System.Length(ci)) do
  begin
    cTotal := cTotal + cLen;
    ci[I] := ci[I] + cTotal;
  end;
  cTotal := cTotal + cLen;

  // NOTE: Provide a safe dump for "high zeroes" since we are adding 'bMax' and not 'bLen'
  System.Inc(cTotal);

  System.SetLength(c, cTotal);

  // Prepare A in Interleaved form, according to the chosen width
  Interleave(a.Fm_ints, 0, c, 0, aLen, width);

  // Make a working copy of B, since we will be shifting it

  bOff := aLen;

  System.Move(b.Fm_ints[0], c[bOff], bLen * System.SizeOf(Int64));
  for bank := 1 to System.Pred(banks) do

  begin
    bOff := bOff + bMax;
    ShiftUp(c, aLen, c, bOff, bMax, bank);
  end;

  // /*
  // * The main loop analyzes the Interleaved windows in A, and for each non-zero window
  // * a single word-array XOR is performed to a carefully selected slice of 'c'. The loop is
  // * breadth-first, checking the lowest window in each word, then looping again for the
  // * next higher window position.
  // */
  MASK := (1 shl width) - 1;

  k := 0;
  while true do
  begin
    aPos := 0;
    repeat

      aVal := Int64(UInt64(c[aPos]) shr k);
      bank := 0;
      bOff := aLen;
      while true do

      begin
        index := Int32(aVal) and MASK;
        if (index <> 0) then
        begin
          // /*
          // * Add to a 'c' buffer based on the bit-pattern of 'index'. Since A is in
          // * Interleaved form, the bits represent the current B shifted by 0, 'positions',
          // * 'positions' * 2, ..., 'positions' * ('width' - 1)
          // */
          Add(c, aPos + ci[index], c, bOff, bMax);
        end;
        System.Inc(bank);
        if (bank = banks) then
        begin
          break;
        end;
        bOff := bOff + bMax;
        aVal := Int64(UInt64(aVal) shr width);
      end;

      System.Inc(aPos);
    until (not(aPos < aLen));

    k := k + stride;
    if (k >= top) then
    begin
      if (k >= 64) then
      begin
        break;
      end;

      // /*
      // * Adjustment for window setups with top == 63, the final bit (if any) is processed
      // * as the top-bit of a window
      // */
      k := 64 - width;
      MASK := MASK and (MASK shl (top - k));
    end;

    // /*
    // * After each position has been checked for all words of A, B is shifted up 1 place
    // */
    ShiftUp(c, aLen, bTotal, banks);
  end;

  ciPos := System.Length(ci);
  System.Dec(ciPos);
  while (ciPos > 1) do
  begin
    if ((ciPos and Int64(1)) = Int64(0)) then
    begin
      // /*
      // * For even numbers, shift contents and add to the half-position
      // */
      AddShiftedUp(c, ci[UInt32(ciPos) shr 1], c, ci[ciPos], cLen, positions);
    end
    else
    begin
      // /*
      // * For odd numbers, 'distribute' contents to the result and the next-lowest position
      // */
      Distribute(c, ci[ciPos], ci[ciPos - 1], ci[1], cLen);
    end;
    System.Dec(ciPos);
  end;

  // /*
  // * Finally the raw answer is collected, reduce it against the reduction coefficients
  // */
  Result := ReduceResult(c, ci[1], cLen, m, ks);
end;

function TLongArray.ModMultiplyLD(const other: TLongArray; m: Int32;
  const ks: TCryptoLibInt32Array): TLongArray;
var
  aDeg, bDeg, tmp, aLen, bLen, cLen, bMax, tOff, I, MASK, k, aVal, u,
    v, j: Int32;
  a, b: TLongArray;
  a0: Int64;
  c0, T0, T1, aMInts, c: TCryptoLibInt64Array;
  ti: TCryptoLibInt32Array;
begin
  // /*
  // * Find out the degree of each argument and handle the zero cases
  // */
  aDeg := Degree();
  if (aDeg = 0) then
  begin
    Result := Self;
    Exit;
  end;
  bDeg := other.Degree();
  if (bDeg = 0) then
  begin
    Result := other;
    Exit;
  end;

  // /*
  // * Swap if necessary so that A is the smaller argument
  // */
  a := Self;
  b := other;
  if (aDeg > bDeg) then
  begin
    a := other;
    b := Self;
    tmp := aDeg;
    aDeg := bDeg;
    bDeg := tmp;
  end;

  // /*
  // * Establish the word lengths of the arguments and result
  // */
  aLen := Int32(UInt32(aDeg + 63) shr 6);
  bLen := Int32(UInt32(bDeg + 63) shr 6);
  cLen := Int32(UInt32(aDeg + bDeg + 62) shr 6);

  if (aLen = 1) then
  begin
    a0 := a.Fm_ints[0];
    if (a0 = Int64(1)) then
    begin
      Result := b;
      Exit;
    end;

    // /*
    // * Fast path for small A, with performance dependent only on the number of set bits
    // */
    System.SetLength(c0, cLen);

    MultiplyWord(a0, b.Fm_ints, bLen, c0, 0);

    // /*
    // * Reduce the raw answer against the reduction coefficients
    // */
    Result := ReduceResult(c0, 0, cLen, m, ks);
    Exit;
  end;

  // /*
  // * Determine if B will get bigger during shifting
  // */
  bMax := Int32(UInt32(bDeg + 7 + 63) shr 6);

  // /*
  // * Lookup table for the offset of each B in the tables
  // */
  System.SetLength(ti, 16);

  // /*
  // * Precompute table of all 4-bit products of B
  // */
  System.SetLength(T0, bMax shl 4);
  tOff := bMax;
  ti[1] := tOff;
  System.Move(b.Fm_ints[0], T0[tOff], bLen * System.SizeOf(Int64));

  for I := 2 to System.Pred(16) do

  begin
    tOff := tOff + bMax;
    ti[I] := tOff;
    if ((I and 1) = 0) then
    begin
      ShiftUp(T0, Int32(UInt32(tOff) shr 1), T0, tOff, bMax, 1);
    end
    else
    begin
      Add(T0, bMax, T0, tOff - bMax, T0, tOff, bMax);
    end;
  end;

  // /*
  // * Second table with all 4-bit products of B shifted 4 bits
  // */
  System.SetLength(T1, System.Length(T0));
  ShiftUp(T0, 0, T1, 0, System.Length(T0), 4);
  // shiftUp(T0, bMax, T1, bMax, tOff, 4);

  aMInts := a.Fm_ints;
  System.SetLength(c, cLen);

  MASK := $F;

  // /*
  // * Lopez-Dahab algorithm
  // */

  k := 56;
  while k >= 0 do
  begin
    j := 1;
    while j < aLen do
    begin
      aVal := Int32(UInt64(aMInts[j]) shr k);
      u := aVal and MASK;
      v := Int32(UInt32(aVal) shr 4) and MASK;
      AddBoth(c, j - 1, T0, ti[u], T1, ti[v], bMax);
      System.Inc(j, 2);
    end;
    ShiftUp(c, 0, cLen, 8);
    System.Dec(k, 8);
  end;

  k := 56;
  while k >= 0 do
  begin
    j := 0;
    while j < aLen do
    begin
      aVal := Int32(UInt64(aMInts[j]) shr k);
      u := aVal and MASK;
      v := Int32(UInt32(aVal) shr 4) and MASK;
      AddBoth(c, j, T0, ti[u], T1, ti[v], bMax);
      System.Inc(j, 2);
    end;
    if (k > 0) then
    begin
      ShiftUp(c, 0, cLen, 8);
    end;
    System.Dec(k, 8);
  end;

  // /*
  // * Finally the raw answer is collected, reduce it against the reduction coefficients
  // */
  Result := ReduceResult(c, 0, cLen, m, ks);

end;

function TLongArray.ModReduce(m: Int32; const ks: TCryptoLibInt32Array)
  : TLongArray;
var
  buf: TCryptoLibInt64Array;
  rLen: Int32;
begin
  buf := System.Copy(Fm_ints);
  rLen := ReduceInPlace(buf, 0, System.Length(buf), m, ks);
  Result := TLongArray.Create(buf, 0, rLen);
end;

function TLongArray.ModSquare(m: Int32; const ks: TCryptoLibInt32Array)
  : TLongArray;
var
  len, _2len, pos: Int32;
  r: TCryptoLibInt64Array;
  mi: Int64;
begin
  len := GetUsedLength();
  if (len = 0) then
  begin
    Result := Self;
    Exit;
  end;

  _2len := len shl 1;
  System.SetLength(r, _2len);

  pos := 0;
  while (pos < _2len) do
  begin
    mi := Fm_ints[UInt32(pos) shr 1];

    r[pos] := Interleave2_32to64(Int32(mi));
    System.Inc(pos);
    r[pos] := Interleave2_32to64(Int32(UInt64(mi) shr 32));
    System.Inc(pos);
  end;

  Result := TLongArray.Create(r, 0, ReduceInPlace(r, 0,
    System.Length(r), m, ks));
end;

function TLongArray.ModSquareN(n, m: Int32; const ks: TCryptoLibInt32Array)
  : TLongArray;
var
  len, mLen: Int32;
  r: TCryptoLibInt64Array;
begin
  len := GetUsedLength();
  if (len = 0) then
  begin
    Result := Self;
    Exit;
  end;

  mLen := TBits.Asr32((m + 63), 6);

  System.SetLength(r, mLen shl 1);
  System.Move(Fm_ints[0], r[0], len * System.SizeOf(Int64));

  System.Dec(n);
  while (n >= 0) do
  begin
    SquareInPlace(r, len, m, ks);
    len := ReduceInPlace(r, 0, System.Length(r), m, ks);
    System.Dec(n);
  end;

  Result := TLongArray.Create(r, 0, len);
end;

function TLongArray.Multiply(const other: TLongArray; m: Int32;
  const ks: TCryptoLibInt32Array): TLongArray;
var
  aDeg, bDeg, tmp, aLen, bLen, cLen, bMax, tOff, I, MASK, aPos, cOff,
    u, v: Int32;
  a0, aVal: Int64;
  a, b: TLongArray;
  c0, T0, T1, aMInts, c: TCryptoLibInt64Array;
  ti: TCryptoLibInt32Array;
begin
  // /*
  // * Find out the degree of each argument and handle the zero cases
  // */

  aDeg := Degree();
  if (aDeg = 0) then
  begin
    Result := Self;
    Exit;
  end;
  bDeg := other.Degree();
  if (bDeg = 0) then
  begin
    Result := other;
    Exit;
  end;

  // /*
  // * Swap if necessary so that A is the smaller argument
  // */
  a := Self;
  b := other;
  if (aDeg > bDeg) then
  begin
    a := other;
    b := Self;
    tmp := aDeg;
    aDeg := bDeg;
    bDeg := tmp;
  end;

  // /*
  // * Establish the word lengths of the arguments and result
  // */
  aLen := Int32(UInt32(aDeg + 63) shr 6);
  bLen := Int32(UInt32(bDeg + 63) shr 6);
  cLen := Int32(UInt32(aDeg + bDeg + 62) shr 6);

  if (aLen = 1) then
  begin
    a0 := a.Fm_ints[0];
    if (a0 = Int64(1)) then
    begin
      Result := b;
      Exit;
    end;

    // /*
    // * Fast path for small A, with performance dependent only on the number of set bits
    // */
    System.SetLength(c0, cLen);
    MultiplyWord(a0, b.Fm_ints, bLen, c0, 0);

    // /*
    // * Reduce the raw answer against the reduction coefficients
    // */
    // return ReduceResult(c0, 0, cLen, m, ks);
    Result := TLongArray.Create(c0, 0, cLen);
    Exit;
  end;

  // /*
  // * Determine if B will get bigger during shifting
  // */
  bMax := Int32(UInt32(bDeg + 7 + 63) shr 6);

  // /*
  // * Lookup table for the offset of each B in the tables
  // */
  System.SetLength(ti, 16);

  // /*
  // * Precompute table of all 4-bit products of B
  // */
  System.SetLength(T0, bMax shl 4);

  tOff := bMax;
  ti[1] := tOff;
  System.Move(b.Fm_ints[0], T0[tOff], bLen * System.SizeOf(Int64));

  for I := 2 to System.Pred(16) do
  begin
    tOff := tOff + bMax;
    ti[I] := tOff;
    if ((I and 1) = 0) then
    begin
      ShiftUp(T0, Int32(UInt32(tOff) shr 1), T0, tOff, bMax, 1);
    end
    else
    begin
      Add(T0, bMax, T0, tOff - bMax, T0, tOff, bMax);
    end;
  end;

  // /*
  // * Second table with all 4-bit products of B shifted 4 bits
  // */
  System.SetLength(T1, System.Length(T0));

  ShiftUp(T0, 0, T1, 0, System.Length(T0), 4);
  // ShiftUp(T0, bMax, T1, bMax, tOff, 4);

  aMInts := a.Fm_ints;
  System.SetLength(c, cLen shl 3);

  MASK := $F;

  // /*
  // * Lopez-Dahab (Modified) algorithm
  // */

  for aPos := 0 to System.Pred(aLen) do

  begin
    aVal := aMInts[aPos];
    cOff := aPos;
    while true do

    begin
      u := Int32(aVal) and MASK;
      aVal := Int64(UInt64(aVal) shr 4);
      v := Int32(aVal) and MASK;
      AddBoth(c, cOff, T0, ti[u], T1, ti[v], bMax);
      aVal := Int64(UInt64(aVal) shr 4);
      if (aVal = Int64(0)) then
      begin
        break;
      end;
      cOff := cOff + cLen;
    end;
  end;

  cOff := System.Length(c);
  cOff := cOff - cLen;
  while (cOff <> 0) do
  begin
    AddShiftedUp(c, cOff - cLen, c, cOff, cLen, 8);
    cOff := cOff - cLen;
  end;

  // /*
  // * Finally the raw answer is collected, reduce it against the reduction coefficients
  // */
  // return ReduceResult(c, 0, cLen, m, ks);
  Result := TLongArray.Create(c, 0, cLen);
end;

procedure TLongArray.Reduce(m: Int32; const ks: TCryptoLibInt32Array);
var
  buf: TCryptoLibInt64Array;
  rLen: Int32;
begin
  buf := Fm_ints;
  rLen := ReduceInPlace(buf, 0, System.Length(buf), m, ks);
  if (rLen < System.Length(buf)) then
  begin
    System.SetLength(Fm_ints, rLen);
    System.Move(buf[0], Fm_ints[0], rLen * System.SizeOf(Int64));
  end;
end;

class procedure TLongArray.ReduceWord(const buf: TCryptoLibInt64Array;
  off, bit: Int32; word: Int64; m: Int32; const ks: TCryptoLibInt32Array);
var
  offset, j: Int32;
begin
  offset := bit - m;
  j := System.Length(ks);
  System.Dec(j);
  while (j >= 0) do
  begin
    FlipWord(buf, off, offset + ks[j], word);
    System.Dec(j);
  end;
  FlipWord(buf, off, offset, word);
end;

class procedure TLongArray.ReduceWordWise(const buf: TCryptoLibInt64Array;
  off, len, toBit, m: Int32; const ks: TCryptoLibInt32Array);
var
  toPos, partial: Int32;
  word: Int64;
begin
  toPos := Int32(UInt32(toBit) shr 6);
  System.Dec(len);
  while (len > toPos) do
  begin
    word := buf[off + len];
    if (word <> 0) then
    begin
      buf[off + len] := 0;
      ReduceWord(buf, off, (len shl 6), word, m, ks);
    end;
    System.Dec(len);
  end;

  partial := toBit and $3F;
  word := Int64(UInt64(buf[off + toPos]) shr partial);
  if (word <> 0) then
  begin
    buf[off + toPos] := buf[off + toPos] xor (word shl partial);
    ReduceWord(buf, off, toBit, word, m, ks);
  end;

end;

class function TLongArray.ShiftUp(const x: TCryptoLibInt64Array; xOff: Int32;
  const z: TCryptoLibInt64Array; zOff, count, shift: Int32): Int64;
var
  shiftInv, I: Int32;
  prev, next: Int64;
begin
  shiftInv := 64 - shift;
  prev := 0;
  for I := 0 to System.Pred(count) do

  begin
    next := x[xOff + I];
    z[zOff + I] := (next shl shift) or prev;
    prev := Int64(UInt64(next) shr shiftInv);
  end;
  Result := prev;
end;

class function TLongArray.ShiftUp(const x: TCryptoLibInt64Array;
  xOff, count, shift: Int32): Int64;
var
  shiftInv, I: Int32;
  prev, next: Int64;
begin
  shiftInv := 64 - shift;
  prev := 0;
  for I := 0 to System.Pred(count) do

  begin
    next := x[xOff + I];
    x[xOff + I] := (next shl shift) or prev;
    prev := Int64(UInt64(next) shr shiftInv);
  end;
  Result := prev;
end;

function TLongArray.Square(m: Int32; const ks: TCryptoLibInt32Array)
  : TLongArray;
var
  len, _2len, pos: Int32;
  r: TCryptoLibInt64Array;
  mi: Int64;
begin
  len := GetUsedLength();
  if (len = 0) then
  begin
    Result := Self;
    Exit;
  end;

  _2len := len shl 1;
  System.SetLength(r, _2len);

  pos := 0;
  while (pos < _2len) do
  begin
    mi := Fm_ints[UInt32(pos) shr 1];
    r[pos] := Interleave2_32to64(Int32(mi));
    System.Inc(pos);
    r[pos] := Interleave2_32to64(Int32(UInt64(mi) shr 32));
    System.Inc(pos);
  end;

  Result := TLongArray.Create(r, 0, System.Length(r));
end;

function TLongArray.TestBitZero: Boolean;
begin
  Result := (System.Length(Fm_ints) > 0) and ((Fm_ints[0] and Int64(1)) <> 0);
end;

function TLongArray.ToBigInteger: TBigInteger;
var
  usedLen, barrI, j, barrLen, iarrJ: Int32;
  highestInt, mi: Int64;
  temp, barr: TCryptoLibByteArray;
  trailingZeroBytesDone: Boolean;
  thisByte: Byte;
begin
  usedLen := GetUsedLength();
  if (usedLen = 0) then
  begin
    Result := TBigInteger.Zero;
    Exit;
  end;

  highestInt := Fm_ints[usedLen - 1];
  System.SetLength(temp, 8);
  barrI := 0;
  trailingZeroBytesDone := false;

  j := 7;
  while j >= 0 do
  begin
    thisByte := Byte(UInt64(highestInt) shr (8 * j));
    if (trailingZeroBytesDone or (thisByte <> 0)) then
    begin
      trailingZeroBytesDone := true;
      temp[barrI] := thisByte;
      System.Inc(barrI);
    end;
    System.Dec(j);
  end;

  barrLen := (8 * (usedLen - 1)) + barrI;
  System.SetLength(barr, barrLen);

  for j := 0 to System.Pred(barrI) do

  begin
    barr[j] := temp[j];
  end;
  // Highest value int is done now

  iarrJ := usedLen - 2;
  while (iarrJ >= 0) do
  begin
    mi := Fm_ints[iarrJ];
    j := 7;
    while (j >= 0) do
    begin
      barr[barrI] := Byte(UInt64(mi) shr (8 * j));
      System.Inc(barrI);
      System.Dec(j);
    end;
    System.Dec(iarrJ);
  end;
  Result := TBigInteger.Create(1, barr);
end;

function TLongArray.ToString: String;
var
  I, len: Int32;
  sl: TStringList;
  S, temp: String;
begin
  I := GetUsedLength();
  if (I = 0) then
  begin
    Result := '0';
    Exit;
  end;

  System.Dec(I);
  sl := TStringList.Create();
  sl.LineBreak := '';
  try
    sl.Add(TLongArray.Int64ToBin(Fm_ints[I]));

    System.Dec(I);

    while (I >= 0) do
    begin

      S := TLongArray.Int64ToBin(Fm_ints[I]);

      // Add leading zeroes, except for highest significant word
      len := System.Length(S);
      if (len < 64) then
      begin
        temp := System.Copy(ZEROES, len, System.Length(ZEROES) - (len - 1));
        sl.Add(temp);
      end;

      sl.Add(S);
      System.Dec(I);
    end;
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

end.
