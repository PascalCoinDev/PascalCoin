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

unit ClpIDerivationFunction;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// base interface for general purpose byte derivation functions.
  /// </summary>
  IDerivationFunction = interface(IInterface)
    ['{A9DA624C-A58E-4588-9EA0-81BA5B13E47E}']

    procedure Init(const parameters: IDerivationParameters);

    function GetDigest(): IDigest;

    /// <value>
    /// return the message digest used as the basis for the function
    /// </value>
    property Digest: IDigest read GetDigest;

    /// <exception cref="EDataLengthCryptoLibException" />
    /// <exception cref="EArgumentCryptoLibException" />
    function GenerateBytes(const output: TCryptoLibByteArray;
      outOff, length: Int32): Int32;

  end;

implementation

end.
