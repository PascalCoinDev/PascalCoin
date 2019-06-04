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

unit ClpX25519PrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpX25519,
  ClpISecureRandom,
  ClpAsymmetricKeyParameter,
  ClpIX25519PrivateKeyParameters,
  ClpIX25519PublicKeyParameters,
  ClpX25519PublicKeyParameters,
  ClpArrayUtils,
  ClpAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SEOFInPrivateKey = 'EOF encountered in middle of X25519 private key';
  SAgreementCalculationFailed = 'X25519 Agreement Failed';

type
  TX25519PrivateKeyParameters = class sealed(TAsymmetricKeyParameter,
    IX25519PrivateKeyParameters)

  strict private
  var
    FData: TCryptoLibByteArray;

  public

    const
    KeySize = Int32(TX25519.ScalarSize);
    SecretSize = Int32(TX25519.PointSize);

    constructor Create(const random: ISecureRandom); overload;
    constructor Create(const buf: TCryptoLibByteArray; off: Int32); overload;
    constructor Create(input: TStream); overload;

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32); inline;
    function GetEncoded(): TCryptoLibByteArray; inline;
    function GeneratePublicKey(): IX25519PublicKeyParameters; inline;
    procedure GenerateSecret(const publicKey: IX25519PublicKeyParameters;
      const buf: TCryptoLibByteArray; off: Int32);

    function Equals(const other: IX25519PrivateKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TX25519PrivateKeyParameters }

function TX25519PrivateKeyParameters.GeneratePublicKey
  : IX25519PublicKeyParameters;
var
  publicKey: TCryptoLibByteArray;
begin
  System.SetLength(publicKey, TX25519.PointSize);
  TX25519.GeneratePublicKey(FData, 0, publicKey, 0);
  result := TX25519PublicKeyParameters.Create(publicKey, 0);
end;

procedure TX25519PrivateKeyParameters.GenerateSecret(const publicKey
  : IX25519PublicKeyParameters; const buf: TCryptoLibByteArray; off: Int32);
var
  encoded: TCryptoLibByteArray;
begin
  System.SetLength(encoded, TX25519.PointSize);
  publicKey.Encode(encoded, 0);
  if (not(TX25519.CalculateAgreement(FData, 0, encoded, 0, buf, off))) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SAgreementCalculationFailed);
  end;
end;

function TX25519PrivateKeyParameters.GetEncoded: TCryptoLibByteArray;
begin
  result := System.Copy(FData);
end;

constructor TX25519PrivateKeyParameters.Create(const random: ISecureRandom);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  TX25519.GeneratePrivateKey(random, FData);
end;

constructor TX25519PrivateKeyParameters.Create(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  System.Move(buf[off], FData[0], KeySize * System.SizeOf(Byte));
end;

constructor TX25519PrivateKeyParameters.Create(input: TStream);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  if (KeySize <> TStreamUtils.ReadFully(input, FData)) then
  begin
    raise EEndOfStreamCryptoLibException.CreateRes(@SEOFInPrivateKey);
  end;
end;

procedure TX25519PrivateKeyParameters.Encode(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  System.Move(FData[0], buf[off], KeySize * System.SizeOf(Byte));
end;

function TX25519PrivateKeyParameters.Equals(const other
  : IX25519PrivateKeyParameters): Boolean;
begin
  if (other = Self as IX25519PrivateKeyParameters) then
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

function TX25519PrivateKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := TArrayUtils.GetArrayHashCode(FData);
end;

end.
