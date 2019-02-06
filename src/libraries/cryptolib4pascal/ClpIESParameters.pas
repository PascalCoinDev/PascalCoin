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

unit ClpIESParameters;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIIESParameters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// parameters for using an integrated cipher in stream mode.
  /// </summary>
  TIESParameters = class(TInterfacedObject, IIESParameters, ICipherParameters)

  strict private
  var
    Fderivation, Fencoding: TCryptoLibByteArray;
    FmacKeySize: Int32;
  strict protected
    function GetMacKeySize(): Int32; inline;
  public
    function GetDerivationV(): TCryptoLibByteArray; inline;
    function GetEncodingV(): TCryptoLibByteArray; inline;
    property MacKeySize: Int32 read GetMacKeySize;

    /// <param name="derivation">
    /// the derivation parameter for the KDF function.
    /// </param>
    /// <param name="encoding">
    /// the encoding parameter for the KDF function.
    /// </param>
    /// <param name="MacKeySize">
    /// the size of the MAC key (in bits).
    /// </param>
    constructor Create(const derivation, encoding: TCryptoLibByteArray;
      MacKeySize: Int32);
  end;

implementation

{ TIESParameters }

constructor TIESParameters.Create(const derivation,
  encoding: TCryptoLibByteArray; MacKeySize: Int32);
begin
  Inherited Create();
  Fderivation := derivation;
  Fencoding := encoding;
  FmacKeySize := MacKeySize;
end;

function TIESParameters.GetDerivationV: TCryptoLibByteArray;
begin
  result := System.Copy(Fderivation);
end;

function TIESParameters.GetEncodingV: TCryptoLibByteArray;
begin
  result := System.Copy(Fencoding);
end;

function TIESParameters.GetMacKeySize: Int32;
begin
  result := FmacKeySize;
end;

end.
