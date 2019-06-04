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

unit ClpEd25519Blake2BPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpEd25519Blake2B,
  ClpIEd25519Blake2B,
  ClpISecureRandom,
  ClpAsymmetricKeyParameter,
  ClpIEd25519Blake2BPrivateKeyParameters,
  ClpIEd25519Blake2BPublicKeyParameters,
  ClpEd25519Blake2BPublicKeyParameters,
  ClpArrayUtils,
  ClpAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SEOFInPrivateKey = 'EOF encountered in middle of Ed25519Blake2B private key';
  SUnsupportedAlgorithm = 'Unsupported Algorithm';
  SCtxNotNil = 'Ctx must be Nil for Ed25519 Algorithm';
  SMsgLen = 'MsgLen must be Equal to "PreHashSize" for Ed25519ph Algorithm';

type
  TEd25519Blake2BPrivateKeyParameters = class sealed(TAsymmetricKeyParameter,
    IEd25519Blake2BPrivateKeyParameters)

  strict private
  var
    FData: TCryptoLibByteArray;
    FEd25519Blake2BInstance: IEd25519Blake2B;

  public

    const
    KeySize = Int32(TEd25519Blake2B.SecretKeySize);
    SignatureSize = Int32(TEd25519Blake2B.SignatureSize);

    constructor Create(const random: ISecureRandom); overload;
    constructor Create(const buf: TCryptoLibByteArray; off: Int32); overload;
    constructor Create(input: TStream); overload;

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32); inline;
    function GetEncoded(): TCryptoLibByteArray; inline;
    function GeneratePublicKey(): IEd25519Blake2BPublicKeyParameters; inline;

    procedure Sign(algorithm: TEd25519Blake2B.TEd25519Algorithm;
      const publicKey: IEd25519Blake2BPublicKeyParameters;
      const ctx, msg: TCryptoLibByteArray; msgOff, msgLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32);

    function Equals(const other: IEd25519Blake2BPrivateKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TEd25519Blake2BPrivateKeyParameters }

function TEd25519Blake2BPrivateKeyParameters.GeneratePublicKey
  : IEd25519Blake2BPublicKeyParameters;
var
  publicKey: TCryptoLibByteArray;
begin
  System.SetLength(publicKey, TEd25519Blake2B.PublicKeySize);
  FEd25519Blake2BInstance.GeneratePublicKey(FData, 0, publicKey, 0);
  result := TEd25519Blake2BPublicKeyParameters.Create(publicKey, 0);
end;

function TEd25519Blake2BPrivateKeyParameters.GetEncoded: TCryptoLibByteArray;
begin
  result := System.Copy(FData);
end;

constructor TEd25519Blake2BPrivateKeyParameters.Create
  (const random: ISecureRandom);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  FEd25519Blake2BInstance := TEd25519Blake2B.Create();
  FEd25519Blake2BInstance.GeneratePrivateKey(random, FData);
end;

constructor TEd25519Blake2BPrivateKeyParameters.Create
  (const buf: TCryptoLibByteArray; off: Int32);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  FEd25519Blake2BInstance := TEd25519Blake2B.Create();
  System.Move(buf[off], FData[0], KeySize * System.SizeOf(Byte));
end;

constructor TEd25519Blake2BPrivateKeyParameters.Create(input: TStream);
begin
  Inherited Create(true);
  System.SetLength(FData, KeySize);
  FEd25519Blake2BInstance := TEd25519Blake2B.Create();
  if (KeySize <> TStreamUtils.ReadFully(input, FData)) then
  begin
    raise EEndOfStreamCryptoLibException.CreateRes(@SEOFInPrivateKey);
  end;
end;

procedure TEd25519Blake2BPrivateKeyParameters.Encode
  (const buf: TCryptoLibByteArray; off: Int32);
begin
  System.Move(FData[0], buf[off], KeySize * System.SizeOf(Byte));
end;

function TEd25519Blake2BPrivateKeyParameters.Equals
  (const other: IEd25519Blake2BPrivateKeyParameters): Boolean;
begin
  if (other = Self as IEd25519Blake2BPrivateKeyParameters) then
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

function TEd25519Blake2BPrivateKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32;
{$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := TArrayUtils.GetArrayHashCode(FData);
end;

procedure TEd25519Blake2BPrivateKeyParameters.Sign
  (algorithm: TEd25519Blake2B.TEd25519Algorithm;
  const publicKey: IEd25519Blake2BPublicKeyParameters;
  const ctx, msg: TCryptoLibByteArray; msgOff, msgLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  pk: TCryptoLibByteArray;
begin
  System.SetLength(pk, TEd25519Blake2B.PublicKeySize);

  if (publicKey = Nil) then
  begin
    FEd25519Blake2BInstance.GeneratePublicKey(FData, 0, pk, 0);
  end
  else
  begin
    publicKey.Encode(pk, 0);
  end;

  case algorithm of
    TEd25519Blake2B.TEd25519Algorithm.Ed25519:
      begin
        if (ctx <> Nil) then
        begin
          raise EArgumentCryptoLibException.CreateRes(@SCtxNotNil);
        end;

        FEd25519Blake2BInstance.Sign(FData, 0, pk, 0, msg, msgOff, msgLen,
          sig, sigOff);
      end;

    TEd25519Blake2B.TEd25519Algorithm.Ed25519ctx:
      begin
        FEd25519Blake2BInstance.Sign(FData, 0, pk, 0, ctx, msg, msgOff, msgLen,
          sig, sigOff);
      end;

    TEd25519Blake2B.TEd25519Algorithm.Ed25519ph:
      begin
        if (TEd25519Blake2B.PreHashSize <> msgLen) then
        begin
          raise EArgumentCryptoLibException.CreateRes(@SMsgLen);
        end;

        FEd25519Blake2BInstance.SignPrehash(FData, 0, pk, 0, ctx, msg, msgOff,
          sig, sigOff);
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnsupportedAlgorithm);
    end;

  end;
end;

end.
