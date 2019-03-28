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

unit ClpSalsa20Engine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBits,
  ClpCheck,
  ClpIStreamCipher,
  ClpISalsa20Engine,
  ClpIKeyParameter,
  ClpICipherParameters,
  ClpIParametersWithIV,
  ClpConverters,
  ClpCryptoLibTypes;

resourcestring
  SInvalidRound = '"rounds" Must be a Positive, Even Number';
  SInvalidKeySize = '%s Requires 128 bit or 256 bit key';
  SMaxByteExceeded = '2^70 Byte Limit per IV; Change IV';
  SMaxByteExceededTwo = '2^70 byte limit per IV would be exceeded; Change IV';
  SEngineNotInitialized = '%s not Initialized';
  SInputBuffertooShort = 'Input Buffer too Short';
  SOutputBuffertooShort = 'Output Buffer too Short';
  SRoundsMustbeEven = 'Number of Rounds Must be Even';
{$IFNDEF _FIXINSIGHT_}
  SIVRequired = '%s Init Requires an IV, "parameters"';
  SInvalidIV = '%s Requires exactly %d bytes of IV';
  SInitError =
    '%s Init Parameters must Contain a KeyParameter (or null for Re-Init)';
  SKeyParameterNullForFirstInit =
    'KeyParameter can not be null for First Initialisation';
{$ENDIF}

type

  /// <summary>
  /// Implementation of Daniel J. Bernstein's Salsa20 stream cipher, Snuffle 2005
  /// </summary>
  TSalsa20Engine = class(TInterfacedObject, ISalsa20Engine, IStreamCipher)

  strict private
  const
    DEFAULT_ROUNDS = Int32(20);
    STATE_SIZE = Int32(16); // 16, 32 bit ints = 64 bytes
    // representation of 'expand 16-byte k' + 'expand 32-byte k' as an array of UInt32
    TAU_SIGMA: array [0 .. 7] of UInt32 = (1634760805, 824206446, 2036477238,
      1797285236, 1634760805, 857760878, 2036477234, 1797285236);

  var
    FIndex: Int32;
    // internal counter
    FCW0, FCW1, FCW2: UInt32;
    FKeyStream: TCryptoLibByteArray;
    FInitialised: Boolean;

    procedure ResetLimitCounter(); inline;
    function LimitExceeded(): Boolean; overload; inline;
    // this relies on the fact len will always be positive.
    function LimitExceeded(len: UInt32): Boolean; overload; inline;

  strict protected
  var
    FRounds: Int32;
    FEngineState, Fx: TCryptoLibUInt32Array;

    function GetAlgorithmName: String; virtual;
    function GetNonceSize: Int32; virtual;
    property NonceSize: Int32 read GetNonceSize;

    procedure AdvanceCounter(); virtual;
    procedure ResetCounter(); virtual;
    procedure SetKey(const keyBytes, ivBytes: TCryptoLibByteArray); virtual;
    procedure GenerateKeyStream(const output: TCryptoLibByteArray); virtual;

    /// <summary>
    /// Rotate left
    /// </summary>
    /// <param name="x">
    /// value to rotate
    /// </param>
    /// <param name="y">
    /// amount to rotate x
    /// </param>
    /// <returns>
    /// rotated x
    /// </returns>
    class function R(x: UInt32; y: Int32): UInt32; static; inline;
    class procedure PackTauOrSigma(keyLength: Int32;
      const state: TCryptoLibUInt32Array; stateOffset: Int32); static;
    class procedure SalsaCore(rounds: Int32;
      const input, x: TCryptoLibUInt32Array); static;

  public
    /// <summary>
    /// Creates a 20 round Salsa20 engine.
    /// </summary>
    constructor Create(); overload;
    /// <summary>
    /// Creates a Salsa20 engine with a specific number of rounds.
    /// </summary>
    /// <param name="rounds">the number of rounds (must be an even number).</param>
    constructor Create(rounds: Int32); overload;

{$IFNDEF _FIXINSIGHT_}
    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); virtual;
{$ENDIF}
    function ReturnByte(input: Byte): Byte; virtual;

    procedure ProcessBytes(const inBytes: TCryptoLibByteArray;
      inOff, len: Int32; const outBytes: TCryptoLibByteArray;
      outOff: Int32); virtual;

    procedure Reset(); virtual;

    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TSalsa20Engine }

constructor TSalsa20Engine.Create;
begin
  Create(DEFAULT_ROUNDS);
end;

procedure TSalsa20Engine.AdvanceCounter;
begin
  System.Inc(FEngineState[8]);
  if (FEngineState[8] = 0) then
  begin
    System.Inc(FEngineState[9]);
  end;
end;

constructor TSalsa20Engine.Create(rounds: Int32);
begin
  Inherited Create();
  if ((rounds <= 0) or ((rounds and 1) <> 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidRound);
  end;
  FRounds := rounds;
  FIndex := 0;
  FInitialised := false;
  System.SetLength(FEngineState, STATE_SIZE); // state
  System.SetLength(Fx, STATE_SIZE); // internal buffer
  System.SetLength(FKeyStream, STATE_SIZE * 4); // expanded state, 64 bytes
end;

procedure TSalsa20Engine.GenerateKeyStream(const output: TCryptoLibByteArray);
begin
  SalsaCore(FRounds, FEngineState, Fx);
  TConverters.le32_copy(PCardinal(Fx), 0, PByte(output), 0,
    System.Length(Fx) * System.SizeOf(UInt32));
end;

function TSalsa20Engine.GetAlgorithmName: String;
begin
  result := 'Salsa20';
  if (FRounds <> DEFAULT_ROUNDS) then
  begin
    result := Format('%s/%d', [result, FRounds]);
  end;
end;

function TSalsa20Engine.GetNonceSize: Int32;
begin
  result := 8;
end;

{$IFNDEF _FIXINSIGHT_}

procedure TSalsa20Engine.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  ivParams: IParametersWithIV;
  iv: TCryptoLibByteArray;
  keyParam: ICipherParameters;
begin
  (*
    * Salsa20 encryption and decryption is completely
    * symmetrical, so the 'forEncryption' is
    * irrelevant. (Like 90% of stream ciphers)
  *)

  if not Supports(parameters, IParametersWithIV, ivParams) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SIVRequired,
      [AlgorithmName]);
  end;

  iv := ivParams.GetIV();
  if ((iv = Nil) or (System.Length(iv) <> NonceSize)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidIV,
      [AlgorithmName, NonceSize]);
  end;

  keyParam := ivParams.parameters;
  if (keyParam = Nil) then
  begin
    if (not FInitialised) then
    begin
      raise EArgumentCryptoLibException.CreateResFmt
        (@SKeyParameterNullForFirstInit, [AlgorithmName]);
    end;

    SetKey(Nil, iv);
  end
  else if Supports(keyParam, IKeyParameter) then
  begin
    SetKey((keyParam as IKeyParameter).GetKey(), iv);
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInitError,
      [AlgorithmName]);
  end;

  Reset();
  FInitialised := true;
end;
{$ENDIF}

function TSalsa20Engine.LimitExceeded: Boolean;
begin
  System.Inc(FCW0);
  if (FCW0 = 0) then
  begin
    System.Inc(FCW1);
    if (FCW1 = 0) then
    begin
      System.Inc(FCW2);
      result := (FCW2 and $20) <> 0; // 2^(32 + 32 + 6)
      Exit;
    end;
  end;

  result := false;
end;

function TSalsa20Engine.LimitExceeded(len: UInt32): Boolean;
var
  Old: UInt32;
begin
  Old := FCW0;
  System.Inc(FCW0, len);
  if (FCW0 < Old) then
  begin
    System.Inc(FCW1);
    if (FCW1 = 0) then
    begin
      System.Inc(FCW2);
      result := (FCW2 and $20) <> 0; // 2^(32 + 32 + 6)
      Exit;
    end;
  end;

  result := false;
end;

class procedure TSalsa20Engine.PackTauOrSigma(keyLength: Int32;
  const state: TCryptoLibUInt32Array; stateOffset: Int32);
var
  tsOff: Int32;
begin
  tsOff := (keyLength - 16) div 4;
  state[stateOffset] := TAU_SIGMA[tsOff];
  state[stateOffset + 1] := TAU_SIGMA[tsOff + 1];
  state[stateOffset + 2] := TAU_SIGMA[tsOff + 2];
  state[stateOffset + 3] := TAU_SIGMA[tsOff + 3];
end;

procedure TSalsa20Engine.ProcessBytes(const inBytes: TCryptoLibByteArray;
  inOff, len: Int32; const outBytes: TCryptoLibByteArray; outOff: Int32);
var
  Idx: Int32;
begin
  if (not FInitialised) then
  begin
    raise EInvalidOperationCryptoLibException.CreateResFmt
      (@SEngineNotInitialized, [AlgorithmName]);
  end;

  TCheck.DataLength(inBytes, inOff, len, SInputBuffertooShort);
  TCheck.OutputLength(outBytes, outOff, len, SOutputBuffertooShort);

  if (LimitExceeded(UInt32(len))) then
  begin
    raise EMaxBytesExceededCryptoLibException.CreateRes(@SMaxByteExceededTwo);
  end;

  for Idx := 0 to System.Pred(len) do
  begin
    if (FIndex = 0) then
    begin
      GenerateKeyStream(FKeyStream);
      AdvanceCounter();
    end;
    outBytes[Idx + outOff] := Byte(FKeyStream[FIndex] xor inBytes[Idx + inOff]);
    FIndex := (FIndex + 1) and 63;
  end;
end;

class function TSalsa20Engine.R(x: UInt32; y: Int32): UInt32;
begin
  result := TBits.RotateLeft32(x, y);
end;

procedure TSalsa20Engine.ResetCounter;
begin
  FEngineState[8] := 0;
  FEngineState[9] := 0;
end;

procedure TSalsa20Engine.ResetLimitCounter;
begin
  FCW0 := 0;
  FCW1 := 0;
  FCW2 := 0;
end;

procedure TSalsa20Engine.Reset;
begin
  FIndex := 0;
  ResetLimitCounter();
  ResetCounter();
end;

function TSalsa20Engine.ReturnByte(input: Byte): Byte;
var
  output: Byte;
begin
  if (LimitExceeded()) then
  begin
    raise EMaxBytesExceededCryptoLibException.CreateRes(@SMaxByteExceeded);
  end;

  if (FIndex = 0) then
  begin
    GenerateKeyStream(FKeyStream);
    AdvanceCounter();
  end;

  output := Byte(FKeyStream[FIndex] xor input);
  FIndex := (FIndex + 1) and 63;

  result := output;
end;

class procedure TSalsa20Engine.SalsaCore(rounds: Int32;
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

    x04 := x04 xor (R((x00 + x12), 7));
    x08 := x08 xor (R((x04 + x00), 9));
    x12 := x12 xor (R((x08 + x04), 13));
    x00 := x00 xor (R((x12 + x08), 18));
    x09 := x09 xor (R((x05 + x01), 7));
    x13 := x13 xor (R((x09 + x05), 9));
    x01 := x01 xor (R((x13 + x09), 13));
    x05 := x05 xor (R((x01 + x13), 18));
    x14 := x14 xor (R((x10 + x06), 7));
    x02 := x02 xor (R((x14 + x10), 9));
    x06 := x06 xor (R((x02 + x14), 13));
    x10 := x10 xor (R((x06 + x02), 18));
    x03 := x03 xor (R((x15 + x11), 7));
    x07 := x07 xor (R((x03 + x15), 9));
    x11 := x11 xor (R((x07 + x03), 13));
    x15 := x15 xor (R((x11 + x07), 18));

    x01 := x01 xor (R((x00 + x03), 7));
    x02 := x02 xor (R((x01 + x00), 9));
    x03 := x03 xor (R((x02 + x01), 13));
    x00 := x00 xor (R((x03 + x02), 18));
    x06 := x06 xor (R((x05 + x04), 7));
    x07 := x07 xor (R((x06 + x05), 9));
    x04 := x04 xor (R((x07 + x06), 13));
    x05 := x05 xor (R((x04 + x07), 18));
    x11 := x11 xor (R((x10 + x09), 7));
    x08 := x08 xor (R((x11 + x10), 9));
    x09 := x09 xor (R((x08 + x11), 13));
    x10 := x10 xor (R((x09 + x08), 18));
    x12 := x12 xor (R((x15 + x14), 7));
    x13 := x13 xor (R((x12 + x15), 9));
    x14 := x14 xor (R((x13 + x12), 13));
    x15 := x15 xor (R((x14 + x13), 18));

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

procedure TSalsa20Engine.SetKey(const keyBytes, ivBytes: TCryptoLibByteArray);
var
  tsOff: Int32;
begin
  if (keyBytes <> Nil) then
  begin
    if not(Byte(System.Length(keyBytes)) in [16, 32]) then
    begin
      raise EArgumentCryptoLibException.CreateResFmt(@SInvalidKeySize,
        [AlgorithmName]);
    end;

    tsOff := (System.Length(keyBytes) - 16) div 4;
    FEngineState[0] := TAU_SIGMA[tsOff];
    FEngineState[5] := TAU_SIGMA[tsOff + 1];
    FEngineState[10] := TAU_SIGMA[tsOff + 2];
    FEngineState[15] := TAU_SIGMA[tsOff + 3];

    // Key
    TConverters.le32_copy(PByte(keyBytes), 0, PCardinal(FEngineState),
      1 * System.SizeOf(UInt32), 4 * System.SizeOf(UInt32));
    TConverters.le32_copy(PByte(keyBytes), (System.Length(keyBytes) - 16) *
      System.SizeOf(Byte), PCardinal(FEngineState), 11 * System.SizeOf(UInt32),
      4 * System.SizeOf(UInt32));
  end;

  // IV
  TConverters.le32_copy(PByte(ivBytes), 0, PCardinal(FEngineState),
    6 * System.SizeOf(UInt32), 2 * System.SizeOf(UInt32));
end;

end.
