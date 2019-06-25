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

unit ClpPbeParametersGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIPbeParametersGenerator;

type

  /// <summary>
  /// base class for all Password Based Encryption (Pbe) parameter generator
  /// classes.
  /// </summary>
  TPbeParametersGenerator = class abstract(TInterfacedObject,
    IPbeParametersGenerator)

  public

    procedure Clear(); virtual; abstract;

    /// <summary>
    /// Generate derived parameters for a key of length keySize.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String; keySize: Int32)
      : ICipherParameters; overload; virtual; abstract;

    /// <summary>
    /// Generate derived parameters for a key of length keySize and iv
    /// of length ivSize.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <param name="ivSize">
    /// the length, in bits, of the iv required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key and an iv.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String;
      keySize, ivSize: Int32): ICipherParameters; overload; virtual; abstract;

    /// <summary>
    /// Generate derived parameters for a key of length keySize,
    /// specifically <br />for use with a MAC.
    /// </summary>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedMacParameters(keySize: Int32): ICipherParameters;
      virtual; abstract;

  end;

implementation

end.
