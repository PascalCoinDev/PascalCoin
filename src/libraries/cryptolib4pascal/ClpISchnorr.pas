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

unit ClpISchnorr;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpBigInteger,
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
  /// <summary>
  /// interface for classes implementing the Schnorr Signature Algorithm
  /// </summary>
  ISchnorr = interface(IInterface)
    ['{B8065C83-1164-4DD0-B983-8CF840F120EF}']

    function GetAlgorithmName: String;
    property AlgorithmName: String read GetAlgorithmName;

    procedure Init(forSigning: Boolean; const parameters: ICipherParameters;
      const digest: IDigest);

    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>;

    function VerifySignature(const &message: TCryptoLibByteArray;
      const r, s: TBigInteger): Boolean;

  end;

implementation

end.
