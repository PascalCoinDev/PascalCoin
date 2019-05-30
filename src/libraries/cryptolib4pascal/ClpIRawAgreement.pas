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

unit ClpIRawAgreement;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpCryptoLibTypes;

type
  IRawAgreement = interface(IInterface)
    ['{B3C55CE5-1F35-4C77-8CDB-757C07DBF4AA}']

    procedure Init(const parameters: ICipherParameters);

    function GetAgreementSize(): Int32;
    property AgreementSize: Int32 read GetAgreementSize;

    procedure CalculateAgreement(const publicKey: ICipherParameters;
      const buf: TCryptoLibByteArray; off: Int32);
  end;

implementation

end.
