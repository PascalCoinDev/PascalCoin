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

unit ClpBaseKdfBytesGenerator;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIDigest,
  ClpIKdfParameters,
  ClpIIso18033KdfParameters,
  ClpIDerivationFunction,
  ClpIDerivationParameters,
  ClpIBaseKdfBytesGenerator,
  ClpConverters,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooSmall = 'Output Buffer too Small';
  SOutputLengthTooLarge = 'Output Length too Large';
  SKDFParameterNotFound = 'KDF Parameters Required For KDF Generator';

type

  /// <summary>
  /// <para>
  /// Basic KDF generator for derived keys and ivs as defined by IEEE
  /// P1363a/ISO 18033
  /// </para>
  /// <para>
  /// This implementation is based on ISO 18033/P1363a.
  /// </para>
  /// </summary>
  TBaseKdfBytesGenerator = class(TInterfacedObject, IBaseKdfBytesGenerator,
    IDerivationFunction)

  strict protected
  var
    Fdigest: IDigest;
    FcounterStart: Int32;
    Fshared, Fiv: TCryptoLibByteArray;

    function GetDigest(): IDigest; virtual;

  public

    /// <summary>
    /// Construct a KDF Parameters generator.
    /// </summary>
    /// <param name="counterStart">
    /// value of counter.
    /// </param>
    /// <param name="digest">
    /// the digest to be used as the source of derived keys.
    /// </param>
    constructor Create(counterStart: Int32; const digest: IDigest);

    procedure Init(const parameters: IDerivationParameters); virtual;

    /// <summary>
    /// return the underlying digest.
    /// </summary>
    property digest: IDigest read GetDigest;

    /// <summary>
    /// fill len bytes of the output buffer with bytes generated from the
    /// derivation function.
    /// </summary>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the size of the request will cause an overflow.
    /// </exception>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if the out buffer is too small.
    /// </exception>
    function GenerateBytes(const output: TCryptoLibByteArray;
      outOff, length: Int32): Int32; virtual;

  end;

implementation

{ TBaseKdfBytesGenerator }

constructor TBaseKdfBytesGenerator.Create(counterStart: Int32;
  const digest: IDigest);
begin
  Inherited Create();
  FcounterStart := counterStart;
  Fdigest := digest;
end;

function TBaseKdfBytesGenerator.GenerateBytes(const output: TCryptoLibByteArray;
  outOff, length: Int32): Int32;
var
  outLen, cThreshold, i: Int32;
  oBytes: Int64;
  counterBase: UInt32;
  dig, C: TCryptoLibByteArray;
begin
  if ((System.length(output) - length) < outOff) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;

  oBytes := length;
  outLen := Fdigest.GetDigestSize;

  //
  // this is at odds with the standard implementation, the
  // maximum value should be hBits * (2^32 - 1) where hBits
  // is the digest output size in bits. We can't have an
  // array with a long index at the moment...
  //

  if (oBytes > ((Int64(2) shl 32) - 1)) then
  begin

    raise EArgumentCryptoLibException.CreateRes(@SOutputLengthTooLarge);
  end;

  cThreshold := Int32((oBytes + outLen - 1) div outLen);

  System.SetLength(dig, Fdigest.GetDigestSize);

  System.SetLength(C, 4);

  TConverters.ReadUInt32AsBytesBE(UInt32(FcounterStart), C, 0);

  counterBase := UInt32(FcounterStart and (not $FF));

  i := 0;
  while i < cThreshold do
  begin
    Fdigest.BlockUpdate(Fshared, 0, System.length(Fshared));
    Fdigest.BlockUpdate(C, 0, 4);

    if (Fiv <> Nil) then
    begin
      Fdigest.BlockUpdate(Fiv, 0, System.length(Fiv));
    end;

    Fdigest.DoFinal(dig, 0);

    if (length > outLen) then
    begin
      System.Move(dig[0], output[outOff], outLen * System.SizeOf(Byte));
      outOff := outOff + outLen;
      length := length - outLen;
    end
    else
    begin
      System.Move(dig[0], output[outOff], length * System.SizeOf(Byte));
    end;

    System.Inc(C[3]);
    if (C[3] = 0) then

    begin
      counterBase := counterBase + $100;
      TConverters.ReadUInt32AsBytesBE(counterBase, C, 0);
    end;

    System.Inc(i);
  end;

  Fdigest.Reset();

  result := Int32(oBytes);
end;

function TBaseKdfBytesGenerator.GetDigest: IDigest;
begin
  result := Fdigest;
end;

procedure TBaseKdfBytesGenerator.Init(const parameters: IDerivationParameters);
var
  Lparameters: IDerivationParameters;
  p1: IKdfParameters;
  p2: IIso18033KdfParameters;
begin
  Lparameters := parameters;

  if Supports(Lparameters, IKdfParameters, p1) then
  begin
    Fshared := p1.GetSharedSecret();
    Fiv := p1.GetIV();
  end
  else if Supports(Lparameters, IIso18033KdfParameters, p2) then
  begin
    Fshared := p2.GetSeed();
    Fiv := Nil;
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SKDFParameterNotFound);
  end;

end;

end.
