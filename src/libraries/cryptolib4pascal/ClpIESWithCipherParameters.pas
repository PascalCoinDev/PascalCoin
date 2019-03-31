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
    FcipherKeySize: Int32;

    function GetCipherKeySize: Int32; inline;
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
      macKeySize, CipherKeySize: Int32);

    /// <summary>
    /// Return the key size in bits for the block cipher used with the message
    /// </summary>
    /// <value>
    /// the key size in bits for the block cipher used with the message
    /// </value>
    property CipherKeySize: Int32 read GetCipherKeySize;

  end;

implementation

{ TIESWithCipherParameters }

function TIESWithCipherParameters.GetCipherKeySize: Int32;
begin
  Result := FcipherKeySize;
end;

constructor TIESWithCipherParameters.Create(const derivation,
  encoding: TCryptoLibByteArray; macKeySize, CipherKeySize: Int32);
begin
  Inherited Create(derivation, encoding, macKeySize);
  FcipherKeySize := CipherKeySize;
end;

end.
