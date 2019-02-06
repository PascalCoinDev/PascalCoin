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

unit ClpX9ECParameters;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpIPolynomialExtensionField,
  ClpIAsn1Objects,
  ClpIECC,
  ClpECAlgorithms,
  ClpX9ECC,
  ClpIX9ECC,
  ClpAsn1Objects,
  ClpIX9ECParameters;

resourcestring
  SUnSupportedCurveOne = 'Only trinomial and pentomial curves are supported';
  SUnSupportedCurveTwo = '"curve" is of an unsupported type';
  SBadVersion = 'Bad Version in X9ECParameters';

type
  /// **
  // * ASN.1 def for Elliptic-Curve ECParameters structure. See
  // * X9.62, for further details.
  // */
  TX9ECParameters = class sealed(TAsn1Encodable, IX9ECParameters)

  strict private
  var
    FfieldID: IX9FieldID;
    Fcurve: IECCurve;
    Fg: IX9ECPoint;
    Fn, Fh: TBigInteger;
    Fseed: TCryptoLibByteArray;

    function GetCurve: IECCurve; inline;
    function GetG: IECPoint; inline;
    function GetH: TBigInteger; inline;
    function GetN: TBigInteger; inline;
    function GetBaseEntry: IX9ECPoint; inline;
    function GetCurveEntry: IX9Curve; inline;
    function GetFieldIDEntry: IX9FieldID; inline;

  public

    constructor Create(const seq: IAsn1Sequence); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IX9ECPoint;
      const n, h: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger; const seed: TCryptoLibByteArray); overload;
    constructor Create(const curve: IECCurve; const g: IX9ECPoint;
      const n, h: TBigInteger; const seed: TCryptoLibByteArray); overload;

    property curve: IECCurve read GetCurve;
    property g: IECPoint read GetG;
    property n: TBigInteger read GetN;
    property h: TBigInteger read GetH;
    property CurveEntry: IX9Curve read GetCurveEntry;
    property FieldIDEntry: IX9FieldID read GetFieldIDEntry;
    property BaseEntry: IX9ECPoint read GetBaseEntry;

    function GetSeed(): TCryptoLibByteArray;

    function ToAsn1Object(): IAsn1Object; override;

    class function GetInstance(obj: TObject): IX9ECParameters; static;

  end;

implementation

{ TX9ECParameters }

function TX9ECParameters.GetBaseEntry: IX9ECPoint;
begin
  Result := Fg;
end;

function TX9ECParameters.GetFieldIDEntry: IX9FieldID;
begin
  Result := FfieldID;
end;

function TX9ECParameters.GetG: IECPoint;
begin
  Result := Fg.Point;
end;

function TX9ECParameters.GetH: TBigInteger;
begin
  Result := Fh;
end;

function TX9ECParameters.GetCurve: IECCurve;
begin
  Result := Fcurve;
end;

constructor TX9ECParameters.Create(const curve: IECCurve; const g: IX9ECPoint;
  const n, h: TBigInteger);
begin
  Create(curve, g, n, h, Nil);
end;

constructor TX9ECParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n: TBigInteger);
begin
  Create(curve, g, n, Default (TBigInteger), Nil);
end;

constructor TX9ECParameters.Create(const seq: IAsn1Sequence);
var
  x9c: IX9Curve;
  p: TObject;
  ecPoint: IX9ECPoint;
begin
  Inherited Create();
  if ((not(Supports(seq[0], IDerInteger))) or
    (not(seq[0] as IDerInteger).Value.Equals(TBigInteger.One))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SBadVersion);
  end;

  Fn := (seq[4] as IDerInteger).Value;

  if (seq.Count = 6) then
  begin
    Fh := (seq[5] as IDerInteger).Value;
  end;

  x9c := TX9Curve.Create(TX9FieldID.GetInstance(seq[1] as TAsn1Sequence), Fn,
    Fh, TAsn1Sequence.GetInstance(seq[2] as TAsn1Sequence));

  Fcurve := x9c.curve;
  p := seq[3] as TAsn1Sequence;

  ecPoint := p as TX9ECPoint;
  if (ecPoint <> Nil) then
  begin
    Fg := ecPoint;
  end
  else
  begin
    Fg := TX9ECPoint.Create(curve, p as TAsn1OctetString);
  end;

  Fseed := x9c.GetSeed();
end;

constructor TX9ECParameters.Create(const curve: IECCurve; const g: IX9ECPoint;
  const n, h: TBigInteger; const seed: TCryptoLibByteArray);
var
  exponents: TCryptoLibInt32Array;
  Field: IPolynomialExtensionField;
begin
  Inherited Create();
  Fcurve := curve;
  Fg := g;
  Fn := n;
  Fh := h;
  Fseed := seed;

  if (TECAlgorithms.IsFpCurve(curve)) then
  begin
    FfieldID := TX9FieldID.Create(curve.Field.Characteristic);
  end
  else if (TECAlgorithms.IsF2mCurve(curve)) then
  begin
    Field := curve.Field as IPolynomialExtensionField;
    exponents := Field.MinimalPolynomial.GetExponentsPresent();
    if (System.Length(exponents) = 3) then
    begin
      FfieldID := TX9FieldID.Create(exponents[2], exponents[1]);
    end
    else if (System.Length(exponents) = 5) then
    begin
      FfieldID := TX9FieldID.Create(exponents[4], exponents[1], exponents[2],
        exponents[3]);
    end
    else
    begin

      raise EArgumentCryptoLibException.CreateRes(@SUnSupportedCurveOne);
    end
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SUnSupportedCurveTwo);

  end;
end;

constructor TX9ECParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n, h: TBigInteger; const seed: TCryptoLibByteArray);
begin
  Create(curve, TX9ECPoint.Create(g) as IX9ECPoint, n, h, seed)
end;

constructor TX9ECParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n, h: TBigInteger);
begin
  Create(curve, g, n, h, Nil);
end;

function TX9ECParameters.GetCurveEntry: IX9Curve;
begin
  Result := TX9Curve.Create(Fcurve, Fseed);
end;

class function TX9ECParameters.GetInstance(obj: TObject): IX9ECParameters;
var
  instance: IX9ECParameters;
begin
  instance := obj as TX9ECParameters;
  if (instance <> Nil) then
  begin
    Result := instance;
    Exit;
  end;

  if (obj <> Nil) then
  begin
    Result := TX9ECParameters.Create(TAsn1Sequence.GetInstance(obj));
    Exit;
  end;

  Result := Nil;
end;

function TX9ECParameters.GetN: TBigInteger;
begin
  Result := Fn;
end;

function TX9ECParameters.GetSeed: TCryptoLibByteArray;
begin
  Result := Fseed;
end;

/// * *
// * Produce an object suitable for an Asn1OutputStream.
// * <pre>
// *  ECParameters ::= Sequence {
// *      version         Integer { ecpVer1(1) } (ecpVer1),
// *      fieldID         FieldID {{FieldTypes}},
// *      curve           X9Curve,
// *      base            X9ECPoint,
// *      order           Integer,
// *      cofactor        Integer OPTIONAL
// *  }
// * </pre>
// */

function TX9ECParameters.ToAsn1Object: IAsn1Object;
var
  v: IAsn1EncodableVector;
begin

  v := TAsn1EncodableVector.Create
    ([TDerInteger.Create(TBigInteger.One) as IDerInteger, FfieldID,
    TX9Curve.Create(Fcurve, Fseed) as IX9Curve, Fg, TDerInteger.Create(Fn)
    as IDerInteger]);

  if (Fh.IsInitialized) then
  begin
    v.Add([TDerInteger.Create(Fh) as IDerInteger]);
  end;

  Result := TDerSequence.Create(v);

end;

end.
