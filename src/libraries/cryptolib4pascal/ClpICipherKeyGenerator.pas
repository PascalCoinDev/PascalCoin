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

unit ClpICipherKeyGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIKeyGenerationParameters,
  ClpCryptoLibTypes;

type

  ICipherKeyGenerator = interface(IInterface)
    ['{084FE16F-7AEA-42C0-92BB-6CEC7923DE6F}']

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="parameters">
    /// the parameters to be used for key generation
    /// </param>
    procedure Init(const parameters: IKeyGenerationParameters);

    /// <summary>
    /// Generate a secret key.
    /// </summary>
    /// <returns>
    /// a byte array containing the key value.
    /// </returns>
    function GenerateKey: TCryptoLibByteArray;

    function GetdefaultStrength: Int32;

    property defaultStrength: Int32 read GetdefaultStrength;

  end;

implementation

end.
