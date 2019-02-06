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

unit ClpX9ECC;

{$I CryptoLib.inc}

interface

uses
  ClpECC,
  ClpIECC,
  ClpIX9ECC,
  ClpCryptoLibTypes,
  ClpECAlgorithms,
  ClpBigInteger,
  ClpX9ObjectIdentifiers,
  ClpAsn1Objects,
  ClpIAsn1Objects;

resourcestring
  SInconsistentKValues = 'Inconsistent K Values';
  SCurveNil = 'Curve';
  SNotImplementedECCurve = 'This Type of ECCurve is not Implemented';
  SFieldIDNil = 'FieldID';
  SSeqNil = 'Seq';

type
  TX9IntegerConverter = class sealed(TObject)

  public

    class function GetByteLength(const fe: IECFieldElement): Int32; overload;
      static; inline;
    class function GetByteLength(const c: IECCurve): Int32; overload;
      static; inline;

    class function IntegerToBytes(const s: TBigInteger; qLength: Int32)
      : TCryptoLibByteArray; static;

  end;

type

  /// <summary>
  /// ASN.1 def for Elliptic-Curve Field ID structure. See X9.62, for further
  /// details.
  /// </summary>
  TX9FieldID = class(TAsn1Encodable, IX9FieldID)

  strict private
  var
    Fid: IDerObjectIdentifier;
    Fparameters: IAsn1Object;

    function GetIdentifier: IDerObjectIdentifier; inline;
    function GetParameters: IAsn1Object; inline;

    constructor Create(const seq: IAsn1Sequence); overload;

  public
    // /**
    // * Constructor for elliptic curves over prime fields
    // * <code>F<sub>2</sub></code>.
    // * @param primeP The prime <code>p</code> defining the prime field.
    // */
    constructor Create(const primeP: TBigInteger); overload;
    // /**
    // * Constructor for elliptic curves over binary fields
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param m  The exponent <code>m</code> of
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param k1 The integer <code>k1</code> where <code>x<sup>m</sup> +
    // * x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.
    // */
    constructor Create(m, k1: Int32); overload;
    // /**
    // * Constructor for elliptic curves over binary fields
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param m  The exponent <code>m</code> of
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param k1 The integer <code>k1</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.
    // * @param k2 The integer <code>k2</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.
    // * @param k3 The integer <code>k3</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>..
    // */
    constructor Create(m, k1, k2, k3: Int32); overload;

    property Identifier: IDerObjectIdentifier read GetIdentifier;

    property Parameters: IAsn1Object read GetParameters;

    /// <summary>
    /// <para>
    /// Produce a Der encoding of the following structure. <br />
    /// &lt;pre&gt;
    /// </para>
    /// <para>
    /// FieldID ::= Sequence { fieldType FIELD-ID.&amp;amp;id({IOSet}),
    /// parameters FIELD-ID.&amp;amp;Type({IOSet}{&amp;#64;fieldType})} <br />
    /// </para>
    /// <para>
    /// &lt;/pre&gt; <br />
    /// </para>
    /// </summary>
    function ToAsn1Object(): IAsn1Object; override;

    class function GetInstance(obj: TObject): IX9FieldID; static;

  end;

type

  /// <summary>
  /// Class for processing an ECFieldElement as a DER object.
  /// </summary>
  TX9FieldElement = class(TAsn1Encodable, IX9FieldElement)

  strict private
  var
    Ff: IECFieldElement;

    function GetValue: IECFieldElement; inline;

  public
    constructor Create(const f: IECFieldElement); overload;
    constructor Create(const p: TBigInteger; const s: IAsn1OctetString);
      overload; deprecated 'Will be removed';
    constructor Create(m, k1, k2, k3: Int32; const s: IAsn1OctetString);
      overload; deprecated 'Will be removed';

    // /**
    // * Produce an object suitable for an Asn1OutputStream.
    // * <pre>
    // *  FieldElement ::= OCTET STRING
    // * </pre>
    // * <p>
    // * <ol>
    // * <li> if <i>q</i> is an odd prime then the field element is
    // * processed as an Integer and converted to an octet string
    // * according to x 9.62 4.3.1.</li>
    // * <li> if <i>q</i> is 2<sup>m</sup> then the bit string
    // * contained in the field element is converted into an octet
    // * string with the same ordering padded at the front if necessary.
    // * </li>
    // * </ol>
    // * </p>
    // */
    function ToAsn1Object(): IAsn1Object; override;

    property Value: IECFieldElement read GetValue;

  end;

type
  // /**
  // * class for describing an ECPoint as a Der object.
  // */
  TX9ECPoint = class sealed(TAsn1Encodable, IX9ECPoint)

  strict private
  var
    Fencoding: IAsn1OctetString;
    Fc: IECCurve;
    Fp: IECPoint;

    function GetIsPointCompressed: Boolean; inline;
    function GetPoint: IECPoint; inline;

  public

    constructor Create(const p: IECPoint); overload;
    constructor Create(const p: IECPoint; compressed: Boolean); overload;
    constructor Create(const c: IECCurve;
      const encoding: TCryptoLibByteArray); overload;
    constructor Create(const c: IECCurve; const s: IAsn1OctetString); overload;

    property Point: IECPoint read GetPoint;
    property IsPointCompressed: Boolean read GetIsPointCompressed;

    function GetPointEncoding(): TCryptoLibByteArray; inline;

    // /**
    // * Produce an object suitable for an Asn1OutputStream.
    // * <pre>
    // *  ECPoint ::= OCTET STRING
    // * </pre>
    // * <p>
    // * Octet string produced using ECPoint.GetEncoded().</p>
    // */
    function ToAsn1Object(): IAsn1Object; override;

  end;

type

  /// <summary>
  /// ASN.1 def for Elliptic-Curve Curve structure. See X9.62, for further
  /// details.
  /// </summary>
  TX9Curve = class(TAsn1Encodable, IX9Curve)

  strict private
  var
    FSeed: TCryptoLibByteArray;
    FfieldIdentifier: IDerObjectIdentifier;
    Fcurve: IECCurve;

    function GetCurve: IECCurve; inline;

  public
    constructor Create(const curve: IECCurve); overload;
    constructor Create(const curve: IECCurve;
      const seed: TCryptoLibByteArray); overload;
    constructor Create(const fieldID: IX9FieldID; const seq: IAsn1Sequence);
      overload; deprecated 'Use constructor including order/cofactor';

    constructor Create(const fieldID: IX9FieldID;
      const order, cofactor: TBigInteger; const seq: IAsn1Sequence); overload;

    function GetSeed(): TCryptoLibByteArray; inline;

    property curve: IECCurve read GetCurve;

    /// <summary>
    /// <para>
    /// Produce an object suitable for an Asn1OutputStream. <br />
    /// &lt;pre&gt;
    /// </para>
    /// <para>
    /// Curve ::= Sequence { a FieldElement, b FieldElement, seed BIT
    /// STRING OPTIONAL }
    /// </para>
    /// <para>
    /// <br />&lt;/pre&gt;
    /// </para>
    /// </summary>
    function ToAsn1Object(): IAsn1Object; override;

  end;

implementation

{ TX9IntegerConverter }

class function TX9IntegerConverter.GetByteLength
  (const fe: IECFieldElement): Int32;
begin
  result := (fe.FieldSize + 7) div 8;
end;

class function TX9IntegerConverter.GetByteLength(const c: IECCurve): Int32;
begin
  result := (c.FieldSize + 7) div 8;
end;

class function TX9IntegerConverter.IntegerToBytes(const s: TBigInteger;
  qLength: Int32): TCryptoLibByteArray;
var
  bytes, tmp: TCryptoLibByteArray;
begin
  bytes := s.ToByteArrayUnsigned();

  if (qLength < System.Length(bytes)) then
  begin
    System.SetLength(tmp, qLength);
    System.Move(bytes[System.Length(bytes) - System.Length(tmp)], tmp[0],
      System.Length(tmp) * System.SizeOf(Byte));
    result := tmp;
    Exit;
  end;
  if (qLength > System.Length(bytes)) then
  begin
    System.SetLength(tmp, qLength);
    System.Move(bytes[0], tmp[System.Length(tmp) - System.Length(bytes)],
      System.Length(bytes) * System.SizeOf(Byte));
    result := tmp;
    Exit;
  end;

  result := bytes;
end;

{ TX9FieldID }

constructor TX9FieldID.Create(m, k1: Int32);
begin
  Create(m, k1, 0, 0);
end;

constructor TX9FieldID.Create(const primeP: TBigInteger);
begin
  Inherited Create();
  Fid := TX9ObjectIdentifiers.PrimeField;
  Fparameters := TDerInteger.Create(primeP);
end;

constructor TX9FieldID.Create(const seq: IAsn1Sequence);
begin
  Inherited Create();
  Fid := TDerObjectIdentifier.GetInstance(seq[0] as TAsn1Encodable);
  Fparameters := seq[1].ToAsn1Object();
end;

constructor TX9FieldID.Create(m, k1, k2, k3: Int32);
var
  fieldIdParams: IAsn1EncodableVector;
begin
  inherited Create();
  Fid := TX9ObjectIdentifiers.CharacteristicTwoField;

  fieldIdParams := TAsn1EncodableVector.Create
    ([TDerInteger.Create(m) as IDerInteger]);

  if (k2 = 0) then
  begin
    if (k3 <> 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInconsistentKValues);
    end;

    fieldIdParams.Add([TX9ObjectIdentifiers.TPBasis, TDerInteger.Create(k1)
      as IDerInteger]);
  end
  else
  begin
    if ((k2 <= k1) or (k3 <= k2)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInconsistentKValues);

    end;

    fieldIdParams.Add([TX9ObjectIdentifiers.PPBasis,
      TDerSequence.Create([TDerInteger.Create(k1) as IDerInteger,
      TDerInteger.Create(k2) as IDerInteger, TDerInteger.Create(k3)
      as IDerInteger])]);
  end;

  Fparameters := TDerSequence.Create(fieldIdParams);
end;

function TX9FieldID.GetIdentifier: IDerObjectIdentifier;
begin
  result := Fid;
end;

class function TX9FieldID.GetInstance(obj: TObject): IX9FieldID;
var
  x9FieldId: IX9FieldID;
begin
  x9FieldId := obj as TX9FieldID;
  if (x9FieldId <> Nil) then
  begin
    result := x9FieldId;
    Exit;
  end;
  if (obj = Nil) then
  begin
    result := Nil;
    Exit;
  end;
  result := TX9FieldID.Create(TAsn1Sequence.GetInstance(obj));
end;

function TX9FieldID.GetParameters: IAsn1Object;
begin
  result := Fparameters;
end;

function TX9FieldID.ToAsn1Object: IAsn1Object;
begin
  result := TDerSequence.Create([Fid, Fparameters]);
end;

{ TX9FieldElement }

constructor TX9FieldElement.Create(const p: TBigInteger;
  const s: IAsn1OctetString);
begin
  Create(TFpFieldElement.Create(p, TBigInteger.Create(1, s.GetOctets()))
    as IFpFieldElement)
end;

constructor TX9FieldElement.Create(const f: IECFieldElement);
begin
  Inherited Create();
  Ff := f;
end;

constructor TX9FieldElement.Create(m, k1, k2, k3: Int32;
  const s: IAsn1OctetString);
begin
  Create(TF2mFieldElement.Create(m, k1, k2, k3, TBigInteger.Create(1,
    s.GetOctets())) as IF2mFieldElement)
end;

function TX9FieldElement.GetValue: IECFieldElement;
begin
  result := Ff;
end;

function TX9FieldElement.ToAsn1Object: IAsn1Object;
var
  byteCount: Int32;
  paddedBigInteger: TCryptoLibByteArray;
begin
  byteCount := TX9IntegerConverter.GetByteLength(Ff);
  paddedBigInteger := TX9IntegerConverter.IntegerToBytes(Ff.ToBigInteger(),
    byteCount);

  result := TDerOctetString.Create(paddedBigInteger);
end;

{ TX9ECPoint }

constructor TX9ECPoint.Create(const c: IECCurve;
  const encoding: TCryptoLibByteArray);
begin
  inherited Create();
  Fc := c;
  Fencoding := TDerOctetString.Create(System.Copy(encoding));
end;

constructor TX9ECPoint.Create(const p: IECPoint; compressed: Boolean);
begin
  inherited Create();
  Fp := p.Normalize();
  Fencoding := TDerOctetString.Create(p.GetEncoded(compressed));
end;

constructor TX9ECPoint.Create(const p: IECPoint);
begin
  Create(p, false);
end;

constructor TX9ECPoint.Create(const c: IECCurve; const s: IAsn1OctetString);
begin
  Create(c, s.GetOctets());
end;

function TX9ECPoint.GetPointEncoding(): TCryptoLibByteArray;
begin
  result := Fencoding.GetOctets();
end;

function TX9ECPoint.GetIsPointCompressed: Boolean;
var
  octets: TCryptoLibByteArray;
begin
  octets := Fencoding.GetOctets();
  result := (octets <> Nil) and (System.Length(octets) > 0) and
    ((octets[0] = 2) or (octets[0] = 3));
end;

function TX9ECPoint.GetPoint: IECPoint;
begin
  if (Fp = Nil) then
  begin
    Fp := Fc.DecodePoint(Fencoding.GetOctets()).Normalize();
  end;

  result := Fp;
end;

function TX9ECPoint.ToAsn1Object: IAsn1Object;
begin
  result := Fencoding;
end;

{ TX9Curve }

constructor TX9Curve.Create(const curve: IECCurve;
  const seed: TCryptoLibByteArray);
begin
  Inherited Create();
  if (curve = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SCurveNil);
  end;

  Fcurve := curve;
  FSeed := System.Copy(seed);

  if (TECAlgorithms.IsFpCurve(curve)) then
  begin
    FfieldIdentifier := TX9ObjectIdentifiers.PrimeField;
  end
  else if (TECAlgorithms.IsF2mCurve(curve)) then
  begin
    FfieldIdentifier := TX9ObjectIdentifiers.CharacteristicTwoField;
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNotImplementedECCurve);
  end;
end;

constructor TX9Curve.Create(const curve: IECCurve);
begin
  Create(curve, Nil);
end;

constructor TX9Curve.Create(const fieldID: IX9FieldID;
  const seq: IAsn1Sequence);
begin
  Create(fieldID, Default (TBigInteger), Default (TBigInteger), seq);
end;

constructor TX9Curve.Create(const fieldID: IX9FieldID;
  const order, cofactor: TBigInteger; const seq: IAsn1Sequence);
var
  p, A, B: TBigInteger;
  Parameters: IDerSequence;
  representation: IDerObjectIdentifier;
  pentanomial: IDerSequence;
  m, k1, k2, k3: Int32;
begin
  Inherited Create();
  if (fieldID = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SFieldIDNil);
  end;
  if (seq = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SSeqNil);
  end;

  FfieldIdentifier := fieldID.Identifier;

  if (FfieldIdentifier.Equals(TX9ObjectIdentifiers.PrimeField)) then
  begin
    p := (fieldID.Parameters as IDerInteger).Value;
    A := TBigInteger.Create(1,
      TAsn1OctetString.GetInstance(seq[0] as TAsn1Encodable).GetOctets());
    B := TBigInteger.Create(1,
      TAsn1OctetString.GetInstance(seq[1] as TAsn1Encodable).GetOctets());
    Fcurve := TFpCurve.Create(p, A, B, order, cofactor);
  end
  else if (FfieldIdentifier.Equals(TX9ObjectIdentifiers.CharacteristicTwoField))
  then
  begin
    // Characteristic two field
    Parameters := fieldID.Parameters as IDerSequence;
    m := (Parameters[0] as IDerInteger).Value.Int32Value;
    representation := Parameters[1] as IDerObjectIdentifier;

    k2 := 0;
    k3 := 0;
    if (representation.Equals(TX9ObjectIdentifiers.TPBasis)) then
    begin
      // Trinomial basis representation
      k1 := (Parameters[2] as IDerInteger).Value.Int32Value;
    end
    else
    begin
      // Pentanomial basis representation
      pentanomial := Parameters[2] as IDerSequence;
      k1 := (pentanomial[0] as IDerInteger).Value.Int32Value;
      k2 := (pentanomial[1] as IDerInteger).Value.Int32Value;
      k3 := (pentanomial[2] as IDerInteger).Value.Int32Value;
    end;
    A := TBigInteger.Create(1,
      TAsn1OctetString.GetInstance(seq[0] as TAsn1Encodable).GetOctets());
    B := TBigInteger.Create(1,
      TAsn1OctetString.GetInstance(seq[1] as TAsn1Encodable).GetOctets());

    Fcurve := TF2mCurve.Create(m, k1, k2, k3, A, B, order, cofactor);
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNotImplementedECCurve);
  end;

  if (seq.Count = 3) then
  begin
    FSeed := (seq[2] as IDerBitString).GetBytes();
  end;

end;

function TX9Curve.GetCurve: IECCurve;
begin
  result := Fcurve;
end;

function TX9Curve.GetSeed: TCryptoLibByteArray;
begin
  result := System.Copy(FSeed);
end;

function TX9Curve.ToAsn1Object: IAsn1Object;
var
  v: IAsn1EncodableVector;
begin
  v := TAsn1EncodableVector.Create();

  if (FfieldIdentifier.Equals(TX9ObjectIdentifiers.PrimeField) or
    FfieldIdentifier.Equals(TX9ObjectIdentifiers.CharacteristicTwoField)) then
  begin
    v.Add([(TX9FieldElement.Create(Fcurve.A) as IX9FieldElement)
      .ToAsn1Object()]);
    v.Add([(TX9FieldElement.Create(Fcurve.B) as IX9FieldElement)
      .ToAsn1Object()]);
  end;

  if (FSeed <> Nil) then
  begin
    v.Add([TDerBitString.Create(FSeed) as IDerBitString]);
  end;

  result := TDerSequence.Create(v);
end;

end.
