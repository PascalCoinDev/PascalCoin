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

unit ClpDHAgreement;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpISecureRandom,
  ClpSecureRandom,
  ClpICipherParameters,
  ClpIDHAgreement,
  ClpIDHParameters,
  ClpDHKeyPairGenerator,
  ClpIDHKeyPairGenerator,
  ClpIDHPrivateKeyParameters,
  ClpIDHPublicKeyParameters,
  ClpDHKeyGenerationParameters,
  ClpIDHKeyGenerationParameters,
  ClpIAsymmetricKeyParameter,
  ClpIAsymmetricCipherKeyPair,
  ClpIParametersWithRandom,
  ClpCryptoLibTypes;

resourcestring
  SDHPublicKeyWrongParameter =
    'Diffie-Hellman Public Key has Wrong Parameters.';
  SNotDHPrivateKeyParameters = 'DHEngine Expects DHPrivateKeyParameters';
  SMessageNotInitialized = 'Message not Initialised';
  SSharedKeyInvalid = 'Shared Key Can''t be 1';
  SDHPublicKeyWeak = 'Diffie-Hellman Public Key is Weak';
  SDHPublicKeyNil = 'DH Public Key Parameter Cannot be Nil';

type
  /// <summary>
  /// <para>
  /// a Diffie-Hellman key exchange engine.
  /// </para>
  /// <para>
  /// note: This uses MTI/A0 key agreement in order to make the key
  /// agreement secure against passive attacks. If you're doing
  /// Diffie-Hellman and both parties have long term public keys you
  /// should look at using this. For further information have a look at
  /// RFC 2631.
  /// </para>
  /// <para>
  /// It's possible to extend this to more than two parties as well, for
  /// the moment that is left as an exercise for the reader.
  /// </para>
  /// </summary>
  TDHAgreement = class sealed(TInterfacedObject, IDHAgreement)

  strict private
  var
    Fkey: IDHPrivateKeyParameters;
    FdhParams: IDHParameters;
    FprivateValue: TBigInteger;
    Frandom: ISecureRandom;

  public
    /// <summary>
    /// initialise the agreement engine.
    /// </summary>
    procedure Init(const parameters: ICipherParameters);

    /// <summary>
    /// calculate our initial message.
    /// </summary>
    function CalculateMessage(): TBigInteger;

    /// <summary>
    /// given a message from a given party and the corresponding public key
    /// calculate the next message in the agreement sequence. In this case
    /// this will represent the shared secret.
    /// </summary>
    function CalculateAgreement(const pub: IDHPublicKeyParameters;
      const &message: TBigInteger): TBigInteger;

  end;

implementation

{ TDHAgreement }

function TDHAgreement.CalculateAgreement(const pub: IDHPublicKeyParameters;
  const &message: TBigInteger): TBigInteger;
var
  p, peerY: TBigInteger;
begin

  if (pub = Nil) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SDHPublicKeyNil);
  end;

  if (not &message.IsInitialized) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SMessageNotInitialized);
  end;

  if (not(pub.parameters.Equals(FdhParams))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SDHPublicKeyWrongParameter);
  end;

  p := FdhParams.p;

  peerY := pub.Y;

  if ((not peerY.IsInitialized) or (peerY.CompareTo(TBigInteger.One) <= 0) or
    (peerY.CompareTo(p.Subtract(TBigInteger.One)) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SDHPublicKeyWeak);
  end;

  result := peerY.ModPow(FprivateValue, p);

  if (result.Equals(TBigInteger.One)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SSharedKeyInvalid);
  end;

  result := &message.ModPow(Fkey.X, p).Multiply(result).&Mod(p);
end;

function TDHAgreement.CalculateMessage: TBigInteger;
var
  dhGen: IDHKeyPairGenerator;
  dhPair: IAsymmetricCipherKeyPair;
begin
  dhGen := TDHKeyPairGenerator.Create();

  dhGen.Init(TDHKeyGenerationParameters.Create(Frandom, FdhParams)
    as IDHKeyGenerationParameters);

  dhPair := dhGen.GenerateKeyPair();

  FprivateValue := (dhPair.Private as IDHPrivateKeyParameters).X;

  result := (dhPair.Public as IDHPublicKeyParameters).Y;
end;

procedure TDHAgreement.Init(const parameters: ICipherParameters);
var
  kParam: IAsymmetricKeyParameter;
  rParam: IParametersWithRandom;
begin
  if Supports(parameters, IParametersWithRandom, rParam) then
  begin
    Frandom := rParam.Random;
    kParam := rParam.parameters as IAsymmetricKeyParameter;
  end
  else
  begin
    Frandom := TSecureRandom.Create();
    kParam := parameters as IAsymmetricKeyParameter;
  end;

  if (not Supports(kParam, IDHPrivateKeyParameters)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNotDHPrivateKeyParameters);
  end;

  Fkey := kParam as IDHPrivateKeyParameters;
  FdhParams := Fkey.parameters;
end;

end.
