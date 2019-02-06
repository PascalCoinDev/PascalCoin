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

unit ClpKeyEncoder;

{$I CryptoLib.inc}

interface

uses
  ClpIAsymmetricKeyParameter,
  ClpIECPublicKeyParameters,
  ClpIKeyEncoder,
  ClpCryptoLibTypes;

type
  TKeyEncoder = class(TInterfacedObject, IKeyEncoder)

  strict private
  var
    FUsePointCompression: Boolean;

  public
    constructor Create(usePointCompression: Boolean);
    function GetEncoded(const keyParameter: IAsymmetricKeyParameter)
      : TCryptoLibByteArray;

  end;

implementation

{ TKeyEncoder }

constructor TKeyEncoder.Create(usePointCompression: Boolean);
begin
  Inherited Create();
  FUsePointCompression := usePointCompression;
end;

function TKeyEncoder.GetEncoded(const keyParameter: IAsymmetricKeyParameter)
  : TCryptoLibByteArray;
begin
  Result := (keyParameter as IECPublicKeyParameters)
    .Q.GetEncoded(FUsePointCompression);
end;

end.
