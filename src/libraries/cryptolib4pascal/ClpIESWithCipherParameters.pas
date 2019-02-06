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

unit ClpIESWithCipherParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIESParameters,
  ClpIIESParameters,
  ClpIIESWithCipherParameters,
  ClpCryptoLibTypes;

type

  TIESWithCipherParameters = class(TIESParameters, IIESParameters,
    IIESWithCipherParameters)

  strict private
  var
    Fnonce: TCryptoLibByteArray;
    FcipherKeySize: Int32;
    FusePointCompression: Boolean;

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
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      macKeySize, CipherKeySize: Int32); overload;

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
    /// <param name="nonce">
    /// an IV to use initialising the block cipher.
    /// </param>
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      macKeySize, CipherKeySize: Int32;
      const nonce: TCryptoLibByteArray); overload;

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
    /// <param name="nonce">
    /// an IV to use initialising the block cipher.
    /// </param>
    /// <param name="UsePointCompression">
    /// whether to use EC point compression or not (false by default)
    /// </param>
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      macKeySize, CipherKeySize: Int32; const nonce: TCryptoLibByteArray;
      UsePointCompression: Boolean); overload;

    /// <summary>
    /// Return the key size in bits for the block cipher used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the block cipher used with the message
    /// </value>
    property CipherKeySize: Int32 read GetCipherKeySize;

    /// <summary>
    /// Return the nonce (IV) value to be associated with message.
    /// </summary>
    /// <value>
    /// block cipher IV for message.
    /// </value>
    property nonce: TCryptoLibByteArray read GetNonce;

    /// <summary>
    /// Return the 'point compression' flag.
    /// </summary>
    /// <value>
    /// the point compression flag
    /// </value>
    property PointCompression: Boolean read GetPointCompression;
  end;

implementation

{ TIESWithCipherParameters }

function TIESWithCipherParameters.GetCipherKeySize: Int32;
begin
  Result := FcipherKeySize;
end;

function TIESWithCipherParameters.GetNonce: TCryptoLibByteArray;
begin
  Result := System.Copy(Fnonce);
end;

function TIESWithCipherParameters.GetPointCompression: Boolean;
begin
  Result := FusePointCompression;
end;

constructor TIESWithCipherParameters.Create(const derivation,
  encoding: TCryptoLibByteArray; macKeySize, CipherKeySize: Int32);
begin
  Create(derivation, encoding, macKeySize, CipherKeySize, Nil);
end;

constructor TIESWithCipherParameters.Create(const derivation,
  encoding: TCryptoLibByteArray; macKeySize, CipherKeySize: Int32;
  const nonce: TCryptoLibByteArray);
begin
  Create(derivation, encoding, macKeySize, CipherKeySize, nonce, false);
end;

constructor TIESWithCipherParameters.Create(const derivation,
  encoding: TCryptoLibByteArray; macKeySize, CipherKeySize: Int32;
  const nonce: TCryptoLibByteArray; UsePointCompression: Boolean);
begin
  Inherited Create(derivation, encoding, macKeySize);

  FcipherKeySize := CipherKeySize;
  Fnonce := System.Copy(nonce);
  FusePointCompression := UsePointCompression;
end;

end.
