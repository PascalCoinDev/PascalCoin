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

unit ClpX25519PublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpX25519,
  ClpAsymmetricKeyParameter,
  ClpIX25519PublicKeyParameters,
  ClpArrayUtils,
  ClpAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SEOFInPublicKey = 'EOF encountered in middle of X25519 public key';

type
  TX25519PublicKeyParameters = class sealed(TAsymmetricKeyParameter,
    IX25519PublicKeyParameters)

  strict private
  var
    FData: TCryptoLibByteArray;

  public

    const
    KeySize = Int32(TX25519.PointSize);

    constructor Create(const buf: TCryptoLibByteArray; off: Int32); overload;
    constructor Create(input: TStream); overload;

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32); inline;
    function GetEncoded(): TCryptoLibByteArray; inline;

    function Equals(const other: IX25519PublicKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TX25519PublicKeyParameters }

function TX25519PublicKeyParameters.GetEncoded: TCryptoLibByteArray;
begin
  result := System.Copy(FData);
end;

constructor TX25519PublicKeyParameters.Create(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  Inherited Create(false);
  System.SetLength(FData, KeySize);
  System.Move(buf[off], FData[0], KeySize * System.SizeOf(Byte));
end;

constructor TX25519PublicKeyParameters.Create(input: TStream);
begin
  Inherited Create(false);
  System.SetLength(FData, KeySize);
  if (KeySize <> TStreamUtils.ReadFully(input, FData)) then
  begin
    raise EEndOfStreamCryptoLibException.CreateRes(@SEOFInPublicKey);
  end;
end;

procedure TX25519PublicKeyParameters.Encode(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  System.Move(FData[0], buf[off], KeySize * System.SizeOf(Byte));
end;

function TX25519PublicKeyParameters.Equals(const other
  : IX25519PublicKeyParameters): Boolean;
begin
  if (other = Self as IX25519PublicKeyParameters) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TArrayUtils.ConstantTimeAreEqual(FData, other.GetEncoded())
end;

function TX25519PublicKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := TArrayUtils.GetArrayHashCode(FData);
end;

end.
