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

unit ClpISigner;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
  ISigner = interface(IInterface)

    ['{8D9ACCFE-AB70-4983-AE4C-A61F935C7DD5}']

    // /**
    // * Return the name of the algorithm the signer implements.
    // *
    // * @return the name of the algorithm the signer implements.
    // */
    function GetAlgorithmName: String;
    property AlgorithmName: String read GetAlgorithmName;

    // /**
    // * Initialise the signer for signing or verification.
    // *
    // * @param forSigning true if for signing, false otherwise
    // * @param param necessary parameters.
    // */
    procedure Init(forSigning: Boolean; const parameters: ICipherParameters);

    // /**
    // * update the internal digest with the byte b
    // */
    procedure Update(input: Byte);

    // /**
    // * update the internal digest with the byte array in
    // */
    procedure BlockUpdate(const input: TCryptoLibByteArray;
      inOff, length: Int32);

    // /**
    // * Generate a signature for the message we've been loaded with using
    // * the key we were initialised with.
    // */
    function GenerateSignature(): TCryptoLibByteArray;
    // /**
    // * return true if the internal state represents the signature described
    // * in the passed in array.
    // */
    function VerifySignature(const signature: TCryptoLibByteArray): Boolean;

    // /**
    // * reset the internal state
    // */
    procedure Reset();

  end;

implementation

end.
