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

unit ClpECIESPublicKeyParser;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpAsn1Objects,
  ClpIECDomainParameters,
  ClpECPublicKeyParameters,
  ClpIKeyParser,
  ClpIAsymmetricKeyParameter,
  ClpIECIESPublicKeyParser,
  ClpCryptoLibTypes;

resourcestring
  SSenderPublicKeyInvalid = 'Sender''s Public Key Invalid.';
  SSenderPublicKeyInvalidPointEncoding =
    'Sender''s Public Key has Invalid Point Encoding "%x"';

type
  TECIESPublicKeyParser = class sealed(TInterfacedObject, IECIESPublicKeyParser,
    IKeyParser)

  strict private
  var
    FecParams: IECDomainParameters;

  public
    function ReadKey(const Stream: TStream): IAsymmetricKeyParameter;
    constructor Create(const ecParams: IECDomainParameters);

  end;

implementation

{ TECIESPublicKeyParser }

constructor TECIESPublicKeyParser.Create(const ecParams: IECDomainParameters);
begin
  Inherited Create();
  FecParams := ecParams;
end;

function TECIESPublicKeyParser.ReadKey(const Stream: TStream)
  : IAsymmetricKeyParameter;
var
  v: TCryptoLibByteArray;
  first: Int32;
begin
  first := Stream.ReadByte;
  // Decode the public ephemeral key
  case first of
    $00: // infinity
      begin
        raise EIOCryptoLibException.CreateRes(@SSenderPublicKeyInvalid);
      end;

    $02, // compressed
    $03: // Byte length calculated as in ECPoint.getEncoded();
      begin
        System.SetLength(v, 1 + (FecParams.Curve.FieldSize + 7) div 8);
      end;

    $04, // uncompressed or
    $06, // hybrid
    $07: // Byte length calculated as in ECPoint.getEncoded();
      begin
        System.SetLength(v, 1 + (2 * ((FecParams.Curve.FieldSize + 7) div 8)));
      end
  else
    begin
      raise EIOCryptoLibException.CreateResFmt
        (@SSenderPublicKeyInvalidPointEncoding, [first]);
    end;

  end;

  v[0] := Byte(first);
  TStreamUtils.ReadFully(Stream, v, 1, System.length(v) - 1);

  result := TECPublicKeyParameters.Create(FecParams.Curve.DecodePoint(v),
    FecParams);
end;

end.
