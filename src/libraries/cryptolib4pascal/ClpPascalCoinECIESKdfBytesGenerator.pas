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

unit ClpPascalCoinECIESKdfBytesGenerator;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIDigest,
  ClpBaseKdfBytesGenerator,
  ClpIDerivationParameters,
  ClpIKdfParameters,
  ClpIPascalCoinECIESKdfBytesGenerator,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooSmall = 'Output Buffer too Small';
  SKDFParameterNotFound = 'KDF Parameters Required For KDF Generator';
  SHashCannotNotProduceSufficientData =
    'Specified Hash Cannot Produce Sufficient Data for the Specified Operation.';

type

  /// <summary>
  /// <para>
  /// KDF generator for compatibility with existing PascalCoin Implementation
  /// </para>
  /// </summary>
  TPascalCoinECIESKdfBytesGenerator = class(TBaseKdfBytesGenerator,
    IPascalCoinECIESKdfBytesGenerator)

  strict protected
    function GetDigest(): IDigest; override;

  public

    /// <summary>
    /// Construct a PascalCoin compatible bytes generator.
    /// </summary>
    /// <param name="digest">
    /// the digest to be used as the source of derived keys.
    /// </param>
    constructor Create(const digest: IDigest);

    procedure Init(const parameters: IDerivationParameters); override;

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
      outOff, length: Int32): Int32; override;

  end;

implementation

{ TPascalCoinECIESKdfBytesGenerator }

constructor TPascalCoinECIESKdfBytesGenerator.Create(const digest: IDigest);
begin
  Inherited Create(0, digest);
end;

function TPascalCoinECIESKdfBytesGenerator.GenerateBytes
  (const output: TCryptoLibByteArray; outOff, length: Int32): Int32;
var
  outLen: Int32;
  oBytes: Int64;
  temp: TCryptoLibByteArray;
begin
  if ((System.length(output) - length) < outOff) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;

  oBytes := length;
  outLen := Fdigest.GetDigestSize;

  if (oBytes > outLen) then
  begin
    raise EDataLengthCryptoLibException.CreateRes
      (@SHashCannotNotProduceSufficientData);
  end;

  System.SetLength(temp, Fdigest.GetDigestSize);
  Fdigest.BlockUpdate(Fshared, 0, System.length(Fshared));
  Fdigest.DoFinal(temp, 0);

  System.Move(temp[0], output[outOff], length * System.SizeOf(Byte));

  Fdigest.Reset();

  result := oBytes;

end;

function TPascalCoinECIESKdfBytesGenerator.GetDigest: IDigest;
begin
  result := Fdigest;
end;

procedure TPascalCoinECIESKdfBytesGenerator.Init(const parameters
  : IDerivationParameters);
var
  Lparameters: IDerivationParameters;
  p1: IKdfParameters;
begin
  Lparameters := parameters;

  if Supports(Lparameters, IKdfParameters, p1) then
  begin
    Fshared := p1.GetSharedSecret();
    Fiv := p1.GetIV();
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SKDFParameterNotFound);
  end;
end;

end.
