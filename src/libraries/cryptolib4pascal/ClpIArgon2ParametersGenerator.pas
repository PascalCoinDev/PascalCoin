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

unit ClpIArgon2ParametersGenerator;

{$I CryptoLib.inc}

interface

uses
  HlpIHashInfo,
  HlpArgon2TypeAndVersion,
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
{$SCOPEDENUMS ON}
  TArgon2Type = HlpArgon2TypeAndVersion.TArgon2Type;
  TArgon2Version = HlpArgon2TypeAndVersion.TArgon2Version;
  TArgon2MemoryCostType = (a2mctMemoryAsKB, a2mctMemoryPowOfTwo);
{$SCOPEDENUMS OFF}

type
  IArgon2ParametersGenerator = interface(IInterface)

    ['{0AC3D3A8-9422-405F-B0EE-6B7AE0F64F74}']

    procedure Init(argon2Type: TArgon2Type; argon2Version: TArgon2Version;
      const password, salt, secret, additional: TCryptoLibByteArray;
      iterations, memory, parallelism: Int32;
      memoryCostType: TArgon2MemoryCostType);

    /// <returns>
    /// the password byte array.
    /// </returns>
    function GetPassword: TCryptoLibByteArray;

    /// <value>
    /// the password byte array.
    /// </value>
    property password: TCryptoLibByteArray read GetPassword;

    /// <returns>
    /// the Argon2 Parameter Builder Instance
    /// </returns>
    function GetArgon2ParametersBuilder: HlpIHashInfo.IArgon2ParametersBuilder;

    /// <returns>
    /// the Argon2 Parameter Builder Instance
    /// </returns>
    property Argon2ParametersBuilder: HlpIHashInfo.IArgon2ParametersBuilder
      read GetArgon2ParametersBuilder;

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
      : ICipherParameters; overload;

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
      keySize, ivSize: Int32): ICipherParameters; overload;

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

  end;

implementation

end.
