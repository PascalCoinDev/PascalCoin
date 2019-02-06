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

unit ClpIBasicAgreement;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpICipherParameters;

type

  /// <summary>
  /// The basic interface that basic Diffie-Hellman implementations conforms
  /// to.
  /// </summary>
  IBasicAgreement = interface(IInterface)

    ['{4A36A62D-3E1F-49B4-A4CD-58348E5A837F}']

    /// <summary>
    /// initialise the agreement engine.
    /// </summary>
    procedure Init(const parameters: ICipherParameters);

    /// <summary>
    /// return the field size for the agreement algorithm in bytes.
    /// </summary>
    function GetFieldSize(): Int32;

    /// <summary>
    /// given a public key from a given party calculate the next message
    /// in the agreement sequence.
    /// </summary>
    function CalculateAgreement(const pubKey: ICipherParameters): TBigInteger;

  end;

implementation

end.
