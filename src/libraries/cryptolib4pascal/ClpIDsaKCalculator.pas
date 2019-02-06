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

unit ClpIDsaKCalculator;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpISecureRandom;

type

  /// <summary>
  /// Interface define calculators of K values for DSA/ECDSA.
  /// </summary>
  IDsaKCalculator = interface(IInterface)
    ['{FDD8ED8F-F59A-42BD-AF57-27602BC29010}']

    // /**
    // * Return true if this calculator is deterministic, false otherwise.
    // *
    // * @return true if deterministic, otherwise false.
    // */
    function GetIsDeterministic: Boolean;
    property IsDeterministic: Boolean read GetIsDeterministic;

    // /**
    // * Non-deterministic initialiser.
    // *
    // * @param n the order of the DSA group.
    // * @param random a source of randomness.
    // */
    procedure Init(const n: TBigInteger; const random: ISecureRandom); overload;

    // /**
    // * Deterministic initialiser.
    // *
    // * @param n the order of the DSA group.
    // * @param d the DSA private value.
    // * @param message the message being signed.
    // */
    procedure Init(const n, d: TBigInteger;
      const &message: TCryptoLibByteArray); overload;

    // /**
    // * Return the next valid value of K.
    // *
    // * @return a K value.
    // */
    function NextK(): TBigInteger;
  end;

implementation

end.
