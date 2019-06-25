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

unit ClpDHBasicAgreement;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpICipherParameters,
  ClpIBasicAgreement,
  ClpIDHBasicAgreement,
  ClpIDHParameters,
  ClpIDHPrivateKeyParameters,
  ClpIDHPublicKeyParameters,
  ClpIParametersWithRandom,
  ClpCryptoLibTypes;

resourcestring
  SDHPublicKeyWrongParameter =
    'Diffie-Hellman Public Key has Wrong Parameters.';
  SNotDHPrivateKeyParameters = 'DHEngine Expects DHPrivateKeyParameters';
  SAlgorithmNotInitialized = 'Agreement Algorithm not Initialised';
  SSharedKeyInvalid = 'Shared Key Can''t be 1';
  SDHPublicKeyWeak = 'Diffie-Hellman Public Key is Weak';

type
  /// <summary>
  /// <para>
  /// a Diffie-Hellman key agreement class.
  /// </para>
  /// <para>
  /// note: This is only the basic algorithm, it doesn't take advantage
  /// of long term public keys if they are available. See the DHAgreement
  /// class for a "better" implementation.
  /// </para>
  /// </summary>
  TDHBasicAgreement = class sealed(TInterfacedObject, IDHBasicAgreement,
    IBasicAgreement)

  strict private
  var
    Fkey: IDHPrivateKeyParameters;
    FdhParams: IDHParameters;

  public
    /// <summary>
    /// initialise the agreement engine.
    /// </summary>
    procedure Init(const parameters: ICipherParameters);

    /// <summary>
    /// return the field size for the agreement algorithm in bytes.
    /// </summary>
    function GetFieldSize(): Int32;

    /// <summary>
    /// given a short term public key from a given party calculate the next
    /// message in the agreement sequence.
    /// </summary>
    function CalculateAgreement(const pubKey: ICipherParameters): TBigInteger;

  end;

implementation

{ TDHBasicAgreement }

function TDHBasicAgreement.CalculateAgreement(const pubKey: ICipherParameters)
  : TBigInteger;
var
  pub: IDHPublicKeyParameters;
  p, peerY: TBigInteger;
begin

  if (Fkey = Nil) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SAlgorithmNotInitialized);
  end;

  pub := pubKey as IDHPublicKeyParameters;

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

  result := peerY.ModPow(Fkey.X, p);

  if (result.Equals(TBigInteger.One)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SSharedKeyInvalid);
  end;
end;

function TDHBasicAgreement.GetFieldSize: Int32;
begin
  result := (Fkey.parameters.p.BitLength + 7) div 8;
end;

procedure TDHBasicAgreement.Init(const parameters: ICipherParameters);
var
  Lparameters: ICipherParameters;
begin
  Lparameters := parameters;
  if Supports(Lparameters, IParametersWithRandom) then
  begin
    Lparameters := (Lparameters as IParametersWithRandom).parameters;
  end;

  if (not Supports(Lparameters, IDHPrivateKeyParameters)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNotDHPrivateKeyParameters);
  end;

  Fkey := Lparameters as IDHPrivateKeyParameters;
  FdhParams := Fkey.parameters;
end;

end.
