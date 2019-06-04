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

unit ClpX25519Agreement;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIRawAgreement,
  ClpIX25519Agreement,
  ClpICipherParameters,
  ClpX25519PrivateKeyParameters,
  ClpIX25519PrivateKeyParameters,
  ClpIX25519PublicKeyParameters,
  ClpCryptoLibTypes;

resourcestring
  SWrongInitCipherParameter =
    'The Init Parameter does not Contain the Private Key';

type
  TX25519Agreement = class(TInterfacedObject, IX25519Agreement, IRawAgreement)

  strict protected
  var
    FPrivateKey: IX25519PrivateKeyParameters;

    function GetAgreementSize(): Int32; virtual;

  public

    procedure Init(const parameters: ICipherParameters); virtual;

    procedure CalculateAgreement(const publicKey: ICipherParameters;
      const buf: TCryptoLibByteArray; off: Int32);

    property AgreementSize: Int32 read GetAgreementSize;

  end;

implementation

{ TX25519Agreement }

function TX25519Agreement.GetAgreementSize: Int32;
begin
  result := TX25519PrivateKeyParameters.SecretSize;
end;

procedure TX25519Agreement.Init(const parameters: ICipherParameters);
begin
  if Supports(parameters, IX25519PrivateKeyParameters) then
  begin
    FPrivateKey := parameters as IX25519PrivateKeyParameters;
  end
  else
  begin
    raise EInvalidParameterCryptoLibException.CreateRes
      (@SWrongInitCipherParameter);
  end;
end;

procedure TX25519Agreement.CalculateAgreement(const publicKey
  : ICipherParameters; const buf: TCryptoLibByteArray; off: Int32);
begin
  FPrivateKey.GenerateSecret(publicKey as IX25519PublicKeyParameters, buf, off);
end;

end.
