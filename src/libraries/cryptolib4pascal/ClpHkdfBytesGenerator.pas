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

unit ClpHkdfBytesGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Math,
  SysUtils,
  ClpHMac,
  ClpIHMac,
  ClpIDigest,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpIHkdfParameters,
  ClpIHkdfBytesGenerator,
  ClpIDerivationFunction,
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

resourcestring
  SSizeTooBigHKDF = 'HKDF Cannot Generate More Than 255 Blocks of HashLen Size';
  SSizeTooBigHKDF2 = 'HKDF May Only Be Used For 255 * HashLen Bytes of Output';
  SInvalidParameterHKDF =
    'HKDF Parameters Required For "HkdfBytesGenerator", "parameters"';

type

  /// <summary>
  /// HMAC-based Extract-and-Expand Key Derivation Function (HKDF)
  /// implemented <br />according to IETF RFC 5869, May 2010 as specified by
  /// H. Krawczyk, IBM <br />Research &amp;amp; P. Eronen, Nokia. It uses a
  /// HMac internally to compute the OKM <br />(output keying material) and
  /// is likely to have better security properties <br />than KDF's based on
  /// just a hash function.
  /// </summary>
  THkdfBytesGenerator = class(TInterfacedObject, IDerivationFunction,
    IHkdfBytesGenerator)

  strict private
  var
    FhMacHash: IHMac;
    FhashLen, FgeneratedBytes: Int32;
    Finfo, FcurrentT: TCryptoLibByteArray;

    /// <summary>
    /// Performs the extract part of the key derivation function.
    /// </summary>
    /// <param name="salt">
    /// the salt to use
    /// </param>
    /// <param name="ikm">
    /// the input keying material
    /// </param>
    /// <returns>
    /// the PRK as KeyParameter
    /// </returns>
    function Extract(const salt, ikm: TCryptoLibByteArray): IKeyParameter;

    /// <summary>
    /// Performs the expand part of the key derivation function, using
    /// currentT <br />as input and output buffer.
    /// </summary>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if the total number of bytes generated is larger than the one
    /// specified by RFC 5869 (255 * HashLen)
    /// </exception>
    procedure ExpandNext();

  strict protected
    function GetDigest: IDigest; virtual;

  public

    /// <summary>
    /// Creates a HKDFBytesGenerator based on the given hash function.
    /// </summary>
    /// <param name="hash">
    /// the digest to be used as the source of generatedBytes bytes
    /// </param>
    constructor Create(const hash: IDigest);

    procedure Init(const parameters: IDerivationParameters); virtual;

    function GenerateBytes(const output: TCryptoLibByteArray;
      outOff, len: Int32): Int32; virtual;

    property Digest: IDigest read GetDigest;

  end;

implementation

{ THkdfBytesGenerator }

constructor THkdfBytesGenerator.Create(const hash: IDigest);
begin
  Inherited Create();
  FhMacHash := THMac.Create(hash);
  FhashLen := hash.GetDigestSize();
end;

procedure THkdfBytesGenerator.ExpandNext;
var
  n: Int32;
begin
  n := (FgeneratedBytes div FhashLen) + 1;
  if (n >= 256) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SSizeTooBigHKDF);
  end;
  // special case for T(0): T(0) is empty, so no update
  if (FgeneratedBytes <> 0) then
  begin
    FhMacHash.BlockUpdate(FcurrentT, 0, FhashLen);
  end;
  FhMacHash.BlockUpdate(Finfo, 0, System.Length(Finfo));
  FhMacHash.Update(Byte(n));
  FhMacHash.DoFinal(FcurrentT, 0);
end;

function THkdfBytesGenerator.Extract(const salt, ikm: TCryptoLibByteArray)
  : IKeyParameter;
var
  temp, prk: TCryptoLibByteArray;
begin
  if (salt = Nil) then
  begin
    System.SetLength(temp, FhashLen);
    // TODO check if hashLen is indeed same as HMAC size
    FhMacHash.Init(TKeyParameter.Create(temp) as IKeyParameter);
  end
  else
  begin
    FhMacHash.Init(TKeyParameter.Create(salt) as IKeyParameter);
  end;

  FhMacHash.BlockUpdate(ikm, 0, System.Length(ikm));

  System.SetLength(prk, FhashLen);
  FhMacHash.DoFinal(prk, 0);
  result := TKeyParameter.Create(prk);
end;

function THkdfBytesGenerator.GenerateBytes(const output: TCryptoLibByteArray;
  outOff, len: Int32): Int32;
var
  toGenerate, posInT, leftInT, toCopy: Int32;
begin
  if ((FgeneratedBytes + len) > (255 * FhashLen)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SSizeTooBigHKDF2);
  end;

  if (FgeneratedBytes mod FhashLen = 0) then
  begin
    ExpandNext();
  end;

  // copy what is left in the currentT (1..hash
  toGenerate := len;
  posInT := FgeneratedBytes mod FhashLen;
  leftInT := FhashLen - (FgeneratedBytes mod FhashLen);
  toCopy := Min(leftInT, toGenerate);
  System.Move(FcurrentT[posInT], output[outOff], toCopy);
  FgeneratedBytes := FgeneratedBytes + toCopy;
  toGenerate := toGenerate - toCopy;
  outOff := outOff + toCopy;

  while (toGenerate > 0) do
  begin
    ExpandNext();
    toCopy := Min(FhashLen, toGenerate);
    System.Move(FcurrentT[0], output[outOff], toCopy);
    FgeneratedBytes := FgeneratedBytes + toCopy;
    toGenerate := toGenerate - toCopy;
    outOff := outOff + toCopy;
  end;

  result := len;
end;

function THkdfBytesGenerator.GetDigest: IDigest;
begin
  result := FhMacHash.GetUnderlyingDigest();
end;

procedure THkdfBytesGenerator.Init(const parameters: IDerivationParameters);
var
  hkdfParameters: IHkdfParameters;
begin
  if (not Supports(parameters, IHkdfParameters, hkdfParameters)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidParameterHKDF);
  end;

  if (hkdfParameters.SkipExtract) then
  begin
    // use IKM directly as PRK
    FhMacHash.Init(TKeyParameter.Create(hkdfParameters.GetIkm())
      as IKeyParameter);
  end
  else
  begin
    FhMacHash.Init(Extract(hkdfParameters.GetSalt(), hkdfParameters.GetIkm()));
  end;

  Finfo := hkdfParameters.GetInfo();

  FgeneratedBytes := 0;
  System.SetLength(FcurrentT, FhashLen);
end;

end.
