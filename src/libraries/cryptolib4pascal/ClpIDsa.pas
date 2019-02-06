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

unit ClpIDsa;

{$I CryptoLib.inc}

interface

uses

  ClpBigInteger,
  ClpICipherParameters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// interface for classes implementing the Digital Signature Algorithm
  /// </summary>
  IDsa = interface(IInterface)

    ['{FD805337-5A3B-4595-B3A3-35B97F47E9E5}']

    function GetAlgorithmName: String;
    property AlgorithmName: String read GetAlgorithmName;

    // /**
    // * initialise the signer for signature generation or signature
    // * verification.
    // *
    // * @param forSigning true if we are generating a signature, false
    // * otherwise.
    // * @param param key parameters for signature generation.
    // */
    procedure Init(forSigning: Boolean; const parameters: ICipherParameters);

    // /**
    // * sign the passed in message (usually the output of a hash function).
    // *
    // * @param message the message to be signed.
    // * @return two big integers representing the r and s values respectively.
    // */
    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>;

    // /**
    // * verify the message message against the signature values r and s.
    // *
    // * @param message the message that was supposed to have been signed.
    // * @param r the r signature value.
    // * @param s the s signature value.
    // */
    function VerifySignature(const &message: TCryptoLibByteArray;
      const r, s: TBigInteger): Boolean;

  end;

implementation

end.
