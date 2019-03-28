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

unit ClpXSalsa20Engine;

{$I CryptoLib.inc}

interface

uses
  ClpIStreamCipher,
  ClpSalsa20Engine,
  ClpIXSalsa20Engine,
  ClpConverters,
  ClpCryptoLibTypes;

resourcestring
  SNullKeyReInit = '%s Doesn''t Support Re-Init with Null Key';
  SInvalidKeySize = '%s Requires a 256 bit Key';

type
  /// <summary>
  /// Implementation of Daniel J. Bernstein's XSalsa20 stream cipher - Salsa20 with an extended nonce.
  /// </summary>
  /// <remarks>
  /// XSalsa20 requires a 256 bit key, and a 192 bit nonce.
  /// </remarks>
  TXSalsa20Engine = class sealed(TSalsa20Engine, IXSalsa20Engine, IStreamCipher)

  strict protected
    function GetAlgorithmName: String; override;
    function GetNonceSize: Int32; override;
    /// <summary>
    /// XSalsa20 key generation: process 256 bit input key and 128 bits of the input nonce
    /// using a core Salsa20 function without input addition to produce 256 bit working key
    /// and use that with the remaining 64 bits of nonce to initialize a standard Salsa20 engine state.
    /// </summary>
    procedure SetKey(const keyBytes, ivBytes: TCryptoLibByteArray); override;

  end;

implementation

{ TXSalsa20Engine }

function TXSalsa20Engine.GetAlgorithmName: String;
begin
  result := 'XSalsa20';
end;

function TXSalsa20Engine.GetNonceSize: Int32;
begin
  result := 24;
end;

procedure TXSalsa20Engine.SetKey(const keyBytes, ivBytes: TCryptoLibByteArray);
var
  hsalsa20Out: TCryptoLibUInt32Array;
begin
  if (keyBytes = Nil) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SNullKeyReInit,
      [AlgorithmName]);
  end;

  if (System.Length(keyBytes) <> 32) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidKeySize,
      [AlgorithmName]);
  end;

  // Set key for HSalsa20
  Inherited SetKey(keyBytes, ivBytes);

  // Pack next 64 bits of IV into engine state instead of counter
  TConverters.le32_copy(PByte(ivBytes), 8 * System.SizeOf(Byte),
    PCardinal(FEngineState), 8 * System.SizeOf(UInt32),
    2 * System.SizeOf(UInt32));

  // Process engine state to generate Salsa20 key
  System.SetLength(hsalsa20Out, System.Length(FEngineState));
  SalsaCore(20, FEngineState, hsalsa20Out);

  // Set new key, removing addition in last round of salsaCore
  FEngineState[1] := hsalsa20Out[0] - FEngineState[0];
  FEngineState[2] := hsalsa20Out[5] - FEngineState[5];
  FEngineState[3] := hsalsa20Out[10] - FEngineState[10];
  FEngineState[4] := hsalsa20Out[15] - FEngineState[15];

  FEngineState[11] := hsalsa20Out[6] - FEngineState[6];
  FEngineState[12] := hsalsa20Out[7] - FEngineState[7];
  FEngineState[13] := hsalsa20Out[8] - FEngineState[8];
  FEngineState[14] := hsalsa20Out[9] - FEngineState[9];

  // Last 64 bits of input IV
  TConverters.le32_copy(PByte(ivBytes), 16 * System.SizeOf(Byte),
    PCardinal(FEngineState), 6 * System.SizeOf(UInt32),
    2 * System.SizeOf(UInt32));
end;

end.
