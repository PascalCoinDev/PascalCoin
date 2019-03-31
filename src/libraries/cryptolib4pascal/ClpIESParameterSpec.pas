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

unit ClpIESParameterSpec;

{$I CryptoLib.inc}

interface

uses
  ClpIIESParameterSpec,
  ClpIAlgorithmParameterSpec,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// Parameter spec for an integrated encryptor, as in IEEE P1363a
  /// </summary>
  TIESParameterSpec = class sealed(TInterfacedObject, IAlgorithmParameterSpec,
    IIESParameterSpec)

  strict private
  var
    Fderivation, Fencoding, FNonce: TCryptoLibByteArray;
    FmacKeySize, FcipherKeySize: Int32;
    FusePointCompression: Boolean;

  strict private
    function GetDerivationV: TCryptoLibByteArray; inline;
    function GetEncodingV: TCryptoLibByteArray; inline;
    function GetMacKeySize: Int32; inline;
    function GetCipherKeySize: Int32; inline;
    function GetNonce: TCryptoLibByteArray; inline;
    function GetPointCompression: Boolean; inline;

  public

    /// <summary>
    /// Set the IES engine parameters.
    /// </summary>
    /// <param name="derivation">
    /// the optional derivation vector for the KDF.
    /// </param>
    /// <param name="encoding">
    /// the optional encoding vector for the KDF.
    /// </param>
    /// <param name="macKeySize">
    /// the key size (in bits) for the MAC.
    /// </param>
    /// <param name="CipherKeySize">
    /// the key size (in bits) for the block cipher.
    /// </param>
    /// <param name="Nonce">
    /// an IV to use initialising the block cipher.
    /// </param>
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      MacKeySize, CipherKeySize: Int32;
      const Nonce: TCryptoLibByteArray); overload;

    /// <summary>
    /// Set the IES engine parameters.
    /// </summary>
    /// <param name="derivation">
    /// the optional derivation vector for the KDF.
    /// </param>
    /// <param name="encoding">
    /// the optional encoding vector for the KDF.
    /// </param>
    /// <param name="macKeySize">
    /// the key size (in bits) for the MAC.
    /// </param>
    /// <param name="CipherKeySize">
    /// the key size (in bits) for the block cipher.
    /// </param>
    /// <param name="Nonce">
    /// an IV to use initialising the block cipher.
    /// </param>
    /// <param name="UsePointCompression">
    /// whether to use EC point compression or not (false by default)
    /// </param>
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      MacKeySize: Int32; CipherKeySize: Int32 = -1;
      const Nonce: TCryptoLibByteArray = Nil;
      UsePointCompression: Boolean = False); overload;

    /// <summary>
    /// Returns the derivation vector.
    /// </summary>
    /// <value>
    /// the derivation vector.
    /// </value>
    property DerivationV: TCryptoLibByteArray read GetDerivationV;

    /// <summary>
    /// Returns the encoding vector.
    /// </summary>
    /// <value>
    /// the encoding vector.
    /// </value>
    property EncodingV: TCryptoLibByteArray read GetEncodingV;

    /// <summary>
    /// Return the key size in bits for the MAC used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the MAC used with the message
    /// </value>
    property MacKeySize: Int32 read GetMacKeySize;

    /// <summary>
    /// Return the key size in bits for the block cipher used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the block cipher used with the message
    /// </value>
    property CipherKeySize: Int32 read GetCipherKeySize;

    /// <summary>
    /// Return the Nonce (IV) value to be associated with message.
    /// </summary>
    /// <value>
    /// block cipher IV for message.
    /// </value>
    property Nonce: TCryptoLibByteArray read GetNonce;

    /// <summary>
    /// Return the 'point compression' flag.
    /// </summary>
    /// <value>
    /// the point compression flag
    /// </value>
    property PointCompression: Boolean read GetPointCompression;

  end;

implementation

{ TIESParameterSpec }

constructor TIESParameterSpec.Create(const derivation,
  encoding: TCryptoLibByteArray; MacKeySize, CipherKeySize: Int32;
  const Nonce: TCryptoLibByteArray);
begin
  Create(derivation, encoding, MacKeySize, CipherKeySize, Nonce, False);
end;

constructor TIESParameterSpec.Create(const derivation,
  encoding: TCryptoLibByteArray; MacKeySize: Int32; CipherKeySize: Int32;
  const Nonce: TCryptoLibByteArray; UsePointCompression: Boolean);
begin
  Inherited Create();
  Fderivation := derivation;
  Fencoding := encoding;
  FmacKeySize := MacKeySize;
  FcipherKeySize := CipherKeySize;
  FNonce := System.Copy(Nonce);
  FusePointCompression := UsePointCompression;
end;

function TIESParameterSpec.GetCipherKeySize: Int32;
begin
  result := FcipherKeySize;
end;

function TIESParameterSpec.GetDerivationV: TCryptoLibByteArray;
begin
  result := System.Copy(Fderivation);
end;

function TIESParameterSpec.GetEncodingV: TCryptoLibByteArray;
begin
  result := System.Copy(Fencoding);
end;

function TIESParameterSpec.GetMacKeySize: Int32;
begin
  result := FmacKeySize;
end;

function TIESParameterSpec.GetNonce: TCryptoLibByteArray;
begin
  result := System.Copy(FNonce);
end;

function TIESParameterSpec.GetPointCompression: Boolean;
begin
  result := FusePointCompression;
end;

end.
