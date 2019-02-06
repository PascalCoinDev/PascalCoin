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

unit ClpECDsaSigner;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpSecureRandom,
  ClpECAlgorithms,
  ClpECC,
  ClpIECC,
  ClpIParametersWithRandom,
  ClpIECPublicKeyParameters,
  ClpIECPrivateKeyParameters,
  ClpFixedPointCombMultiplier,
  ClpCryptoLibTypes,
  ClpICipherParameters,
  ClpIECDomainParameters,
  ClpISecureRandom,
  ClpRandomDsaKCalculator,
  ClpIECKeyParameters,
  ClpIDsaKCalculator,
  ClpECCurveConstants,
  ClpIDsaExt,
  ClpIECDsaSigner;

resourcestring
  SECPublicKeyNotFound = 'EC Public Key Required for Verification';
  SECPrivateKeyNotFound = 'EC Private Key Required for Signing';

type

  /// <summary>
  /// EC-DSA as described in X9.62
  /// </summary>
  TECDsaSigner = class(TInterfacedObject, IDsaExt, IECDsaSigner)

  strict private

    class var

      FEight: TBigInteger;

    class procedure Boot(); static;
    class constructor ECDsaSigner();

    class function GetEight: TBigInteger; static; inline;

    class property Eight: TBigInteger read GetEight;

    function GetOrder: TBigInteger; virtual;
    function GetAlgorithmName: String; virtual;

  strict protected
  var
    FkCalculator: IDsaKCalculator;
    Fkey: IECKeyParameters;
    Frandom: ISecureRandom;

    function CalculateE(const n: TBigInteger;
      const &message: TCryptoLibByteArray): TBigInteger; virtual;

    function CreateBasePointMultiplier(): IECMultiplier; virtual;

    function GetDenominator(coordinateSystem: Int32; const p: IECPoint)
      : IECFieldElement; virtual;

    function InitSecureRandom(needed: Boolean; const provided: ISecureRandom)
      : ISecureRandom; virtual;

  public

    /// <summary>
    /// Default configuration, random K values.
    /// </summary>
    constructor Create(); overload;

    /// <summary>
    /// Configuration with an alternate, possibly deterministic calculator of
    /// K.
    /// </summary>
    /// <param name="kCalculator">
    /// kCalculator a K value calculator.
    /// </param>
    constructor Create(const kCalculator: IDsaKCalculator); overload;

    property Order: TBigInteger read GetOrder;
    property AlgorithmName: String read GetAlgorithmName;

    procedure Init(forSigning: Boolean;
      const parameters: ICipherParameters); virtual;

    // // 5.3 pg 28
    // /**
    // * Generate a signature for the given message using the key we were
    // * initialised with. For conventional DSA the message should be a SHA-1
    // * hash of the message of interest.
    // *
    // * @param message the message that will be verified later.
    function GenerateSignature(const &message: TCryptoLibByteArray)
      : TCryptoLibGenericArray<TBigInteger>; virtual;

    // // 5.4 pg 29
    // /**
    // * return true if the value r and s represent a DSA signature for
    // * the passed in message (for standard DSA the message should be
    // * a SHA-1 hash of the real message to be verified).
    // */
    function VerifySignature(const &message: TCryptoLibByteArray;
      const r, s: TBigInteger): Boolean;

  end;

implementation

{ TECDsaSigner }

constructor TECDsaSigner.Create;
begin
  inherited Create();
  FkCalculator := TRandomDsaKCalculator.Create();
end;

class procedure TECDsaSigner.Boot;
begin
  FEight := TBigInteger.ValueOf(8);
end;

function TECDsaSigner.CalculateE(const n: TBigInteger;
  const &message: TCryptoLibByteArray): TBigInteger;
var
  messageBitLength: Int32;
  trunc: TBigInteger;
begin
  messageBitLength := System.Length(&message) * 8;
  trunc := TBigInteger.Create(1, &message);

  if (n.BitLength < messageBitLength) then
  begin
    trunc := trunc.ShiftRight(messageBitLength - n.BitLength);
  end;

  Result := trunc;
end;

constructor TECDsaSigner.Create(const kCalculator: IDsaKCalculator);
begin
  inherited Create();
  FkCalculator := kCalculator;
end;

function TECDsaSigner.CreateBasePointMultiplier: IECMultiplier;
begin
  Result := TFixedPointCombMultiplier.Create();
end;

class constructor TECDsaSigner.ECDsaSigner;
begin
  TECDsaSigner.Boot;
end;

function TECDsaSigner.GenerateSignature(const &message: TCryptoLibByteArray)
  : TCryptoLibGenericArray<TBigInteger>;
var
  ec: IECDomainParameters;
  basePointMultiplier: IECMultiplier;
  n, e, d, r, s, k: TBigInteger;
  p: IECPoint;
begin
  ec := Fkey.parameters;
  n := ec.n;
  e := CalculateE(n, &message);
  d := (Fkey as IECPrivateKeyParameters).d;

  if (FkCalculator.IsDeterministic) then
  begin
    FkCalculator.Init(n, d, &message);
  end
  else
  begin
    FkCalculator.Init(n, Frandom);
  end;

  basePointMultiplier := CreateBasePointMultiplier();

  // 5.3.2
  repeat // Generate s

    repeat // Generate r
      k := FkCalculator.NextK();

      p := basePointMultiplier.Multiply(ec.G, k).Normalize();

      // 5.3.3
      r := p.AffineXCoord.ToBigInteger().&Mod(n);
    until (not(r.SignValue = 0));

    s := k.ModInverse(n).Multiply(e.Add(d.Multiply(r))).&Mod(n);

  until (not(s.SignValue = 0));

  Result := TCryptoLibGenericArray<TBigInteger>.Create(r, s);
end;

function TECDsaSigner.GetAlgorithmName: String;
begin
  Result := 'ECDSA';
end;

function TECDsaSigner.GetDenominator(coordinateSystem: Int32; const p: IECPoint)
  : IECFieldElement;
begin
  case (coordinateSystem) of
    TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE, TECCurveConstants.COORD_SKEWED:
      begin
        Result := p.GetZCoord(0);
      end;

    TECCurveConstants.COORD_JACOBIAN,
      TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
      TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        Result := p.GetZCoord(0).Square();
      end
  else
    begin
      Result := Nil;
    end;
  end;
end;

class function TECDsaSigner.GetEight: TBigInteger;
begin
  Result := FEight;
end;

function TECDsaSigner.GetOrder: TBigInteger;
begin
  Result := Fkey.parameters.n;
end;

procedure TECDsaSigner.Init(forSigning: Boolean;
  const parameters: ICipherParameters);
var
  providedRandom: ISecureRandom;
  rParam: IParametersWithRandom;
  Lparameters: ICipherParameters;
begin
  providedRandom := Nil;
  Lparameters := parameters;

  if (forSigning) then
  begin

    if (Supports(Lparameters, IParametersWithRandom, rParam)) then
    begin
      providedRandom := rParam.random;
      Lparameters := rParam.parameters;
    end;

    if (not(Supports(Lparameters, IECPrivateKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPrivateKeyNotFound);
    end;

    Fkey := Lparameters as IECPrivateKeyParameters;
  end
  else
  begin
    if (not(Supports(Lparameters, IECPublicKeyParameters))) then
    begin
      raise EInvalidKeyCryptoLibException.CreateRes(@SECPublicKeyNotFound);
    end;

    Fkey := Lparameters as IECPublicKeyParameters;
  end;

  Frandom := InitSecureRandom((forSigning) and
    (not FkCalculator.IsDeterministic), providedRandom);
end;

function TECDsaSigner.InitSecureRandom(needed: Boolean;
  const provided: ISecureRandom): ISecureRandom;
begin
  if (not needed) then
  begin
    Result := Nil;
  end
  else
  begin
    if (provided <> Nil) then
    begin
      Result := provided;
    end
    else
    begin
      Result := TSecureRandom.Create();
    end;
  end;
end;

function TECDsaSigner.VerifySignature(const &message: TCryptoLibByteArray;
  const r, s: TBigInteger): Boolean;
var
  n, e, c, u1, u2, cofactor, v, Smallr: TBigInteger;
  G, Q, point: IECPoint;
  curve: IECCurve;
  d, X, RLocal: IECFieldElement;
begin
  n := Fkey.parameters.n;
  Smallr := r;

  // r and s should both in the range [1,n-1]
  if ((Smallr.SignValue < 1) or (s.SignValue < 1) or (Smallr.CompareTo(n) >= 0)
    or (s.CompareTo(n) >= 0)) then
  begin
    Result := false;
    Exit;
  end;

  e := CalculateE(n, &message);
  c := s.ModInverse(n);

  u1 := e.Multiply(c).&Mod(n);
  u2 := Smallr.Multiply(c).&Mod(n);

  G := Fkey.parameters.G;

  Q := (Fkey as IECPublicKeyParameters).Q;

  point := TECAlgorithms.SumOfTwoMultiplies(G, u1, Q, u2);

  if (point.IsInfinity) then
  begin
    Result := false;
    Exit;
  end;

  // /*
  // * If possible, avoid normalizing the point (to save a modular inversion in the curve field).
  // *
  // * There are ~cofactor elements of the curve field that reduce (modulo the group order) to 'r'.
  // * If the cofactor is known and small, we generate those possible field values and project each
  // * of them to the same "denominator" (depending on the particular projective coordinates in use)
  // * as the calculated point.X. If any of the projected values matches point.X, then we have:
  // *     (point.X / Denominator mod p) mod n == r
  // * as required, and verification succeeds.
  // *
  // * Based on an original idea by Gregory Maxwell (https://github.com/gmaxwell), as implemented in
  // * the libsecp256k1 project (https://github.com/bitcoin/secp256k1).
  // */
  curve := point.curve;
  if (curve <> Nil) then
  begin
    cofactor := curve.cofactor;
    if ((cofactor.IsInitialized) and (cofactor.CompareTo(Eight) <= 0)) then
    begin
      d := GetDenominator(curve.coordinateSystem, point);
      if ((d <> Nil) and (not d.IsZero)) then
      begin
        X := point.XCoord;
        while (curve.IsValidFieldElement(Smallr)) do
        begin
          RLocal := curve.FromBigInteger(Smallr).Multiply(d);
          if (RLocal.Equals(X)) then
          begin
            Result := True;
            Exit;
          end;
          Smallr := Smallr.Add(n);
        end;
        Result := false;
        Exit;
      end;
    end;
  end;

  v := point.Normalize().AffineXCoord.ToBigInteger().&Mod(n);
  Result := v.Equals(Smallr);
end;

end.
