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

unit ClpChaChaEngine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIStreamCipher,
  ClpIChaChaEngine,
  ClpSalsa20Engine,
  ClpConverters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// Implementation of Daniel J. Bernstein's ChaCha stream cipher.
  /// </summary>
  TChaChaEngine = class sealed(TSalsa20Engine, IChaChaEngine, IStreamCipher)

  strict private
    /// <summary>
    /// ChaCha function.
    /// </summary>
    /// <param name="rounds">The number of ChaCha rounds to execute</param>
    /// <param name="input">The input words.</param>
    /// <param name="x">The ChaCha state to modify.</param>
    class procedure ChaChaCore(rounds: Int32;
      const input, x: TCryptoLibUInt32Array); static;

  strict protected
    function GetAlgorithmName: String; override;

    procedure AdvanceCounter(); override;
    procedure ResetCounter(); override;
    procedure SetKey(const keyBytes, ivBytes: TCryptoLibByteArray); override;
    procedure GenerateKeyStream(const output: TCryptoLibByteArray); override;

  public
    /// <summary>
    /// Creates a 20 rounds ChaCha engine.
    /// </summary>
    constructor Create(); overload;
    /// <summary>
    /// Creates a ChaCha engine with a specific number of rounds.
    /// </summary>
    /// <param name="rounds">the number of rounds (must be an even number).</param>
    constructor Create(rounds: Int32); overload;

  end;

implementation

{ TChaChaEngine }

procedure TChaChaEngine.AdvanceCounter;
begin
  System.Inc(FEngineState[12]);
  if (FEngineState[12] = 0) then
  begin
    System.Inc(FEngineState[13]);
  end;
end;

class procedure TChaChaEngine.ChaChaCore(rounds: Int32;
  const input, x: TCryptoLibUInt32Array);
var
  x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14,
    x15: UInt32;
  Idx: Int32;
begin
  if (System.Length(input) <> 16) then
  begin
    raise EArgumentCryptoLibException.Create('');
  end;
  if (System.Length(x) <> 16) then
  begin
    raise EArgumentCryptoLibException.Create('');
  end;
  if ((rounds mod 2) <> 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SRoundsMustbeEven);
  end;

  x00 := input[0];
  x01 := input[1];
  x02 := input[2];
  x03 := input[3];
  x04 := input[4];
  x05 := input[5];
  x06 := input[6];
  x07 := input[7];
  x08 := input[8];
  x09 := input[9];
  x10 := input[10];
  x11 := input[11];
  x12 := input[12];
  x13 := input[13];
  x14 := input[14];
  x15 := input[15];

  Idx := rounds;
  while Idx > 0 do
  begin

    x00 := x00 + x04;
    x12 := R(x12 xor x00, 16);
    x08 := x08 + x12;
    x04 := R(x04 xor x08, 12);
    x00 := x00 + x04;
    x12 := R(x12 xor x00, 8);
    x08 := x08 + x12;
    x04 := R(x04 xor x08, 7);
    x01 := x01 + x05;
    x13 := R(x13 xor x01, 16);
    x09 := x09 + x13;
    x05 := R(x05 xor x09, 12);
    x01 := x01 + x05;
    x13 := R(x13 xor x01, 8);
    x09 := x09 + x13;
    x05 := R(x05 xor x09, 7);
    x02 := x02 + x06;
    x14 := R(x14 xor x02, 16);
    x10 := x10 + x14;
    x06 := R(x06 xor x10, 12);
    x02 := x02 + x06;
    x14 := R(x14 xor x02, 8);
    x10 := x10 + x14;
    x06 := R(x06 xor x10, 7);
    x03 := x03 + x07;
    x15 := R(x15 xor x03, 16);
    x11 := x11 + x15;
    x07 := R(x07 xor x11, 12);
    x03 := x03 + x07;
    x15 := R(x15 xor x03, 8);
    x11 := x11 + x15;
    x07 := R(x07 xor x11, 7);
    x00 := x00 + x05;
    x15 := R(x15 xor x00, 16);
    x10 := x10 + x15;
    x05 := R(x05 xor x10, 12);
    x00 := x00 + x05;
    x15 := R(x15 xor x00, 8);
    x10 := x10 + x15;
    x05 := R(x05 xor x10, 7);
    x01 := x01 + x06;
    x12 := R(x12 xor x01, 16);
    x11 := x11 + x12;
    x06 := R(x06 xor x11, 12);
    x01 := x01 + x06;
    x12 := R(x12 xor x01, 8);
    x11 := x11 + x12;
    x06 := R(x06 xor x11, 7);
    x02 := x02 + x07;
    x13 := R(x13 xor x02, 16);
    x08 := x08 + x13;
    x07 := R(x07 xor x08, 12);
    x02 := x02 + x07;
    x13 := R(x13 xor x02, 8);
    x08 := x08 + x13;
    x07 := R(x07 xor x08, 7);
    x03 := x03 + x04;
    x14 := R(x14 xor x03, 16);
    x09 := x09 + x14;
    x04 := R(x04 xor x09, 12);
    x03 := x03 + x04;
    x14 := R(x14 xor x03, 8);
    x09 := x09 + x14;
    x04 := R(x04 xor x09, 7);

    System.Dec(Idx, 2);
  end;

  x[0] := x00 + input[0];
  x[1] := x01 + input[1];
  x[2] := x02 + input[2];
  x[3] := x03 + input[3];
  x[4] := x04 + input[4];
  x[5] := x05 + input[5];
  x[6] := x06 + input[6];
  x[7] := x07 + input[7];
  x[8] := x08 + input[8];
  x[9] := x09 + input[9];
  x[10] := x10 + input[10];
  x[11] := x11 + input[11];
  x[12] := x12 + input[12];
  x[13] := x13 + input[13];
  x[14] := x14 + input[14];
  x[15] := x15 + input[15];

end;

constructor TChaChaEngine.Create;
begin
  Inherited Create();
end;

constructor TChaChaEngine.Create(rounds: Int32);
begin
  Inherited Create(rounds);
end;

procedure TChaChaEngine.GenerateKeyStream(const output: TCryptoLibByteArray);
begin
  ChaChaCore(FRounds, FEngineState, Fx);
  TConverters.le32_copy(PCardinal(Fx), 0, PByte(output), 0,
    System.Length(Fx) * System.SizeOf(UInt32));
end;

function TChaChaEngine.GetAlgorithmName: String;
begin
  result := Format('ChaCha%d', [FRounds]);
end;

procedure TChaChaEngine.ResetCounter;
begin
  FEngineState[12] := 0;
  FEngineState[13] := 0;
end;

procedure TChaChaEngine.SetKey(const keyBytes, ivBytes: TCryptoLibByteArray);
begin
  if (keyBytes <> Nil) then
  begin
    if not(Byte(System.Length(keyBytes)) in [16, 32]) then
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidKeySize,
        [AlgorithmName]);
    end;

    PackTauOrSigma(System.Length(keyBytes), FEngineState, 0);

    // Key
    TConverters.le32_copy(PByte(keyBytes), 0, PCardinal(FEngineState),
      4 * System.SizeOf(UInt32), 4 * System.SizeOf(UInt32));
    TConverters.le32_copy(PByte(keyBytes), (System.Length(keyBytes) - 16) *
      System.SizeOf(Byte), PCardinal(FEngineState), 8 * System.SizeOf(UInt32),
      4 * System.SizeOf(UInt32));
  end;

  // IV
  TConverters.le32_copy(PByte(ivBytes), 0, PCardinal(FEngineState),
    14 * System.SizeOf(UInt32), 2 * System.SizeOf(UInt32));
end;

end.
