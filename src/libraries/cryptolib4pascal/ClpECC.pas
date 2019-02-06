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

unit ClpECC;

{$I CryptoLib.inc}

interface

uses
  Classes,
  SyncObjs,
  SysUtils,
  Generics.Collections,
  ClpNat,
  ClpMod,
  ClpArrayUtils,
  ClpIPreCompCallback,
  ClpCryptoLibTypes,
  ClpBigInteger,
  ClpBigIntegers,
  ClpBits,
  ClpIGlvEndomorphism,
  ClpECAlgorithms,
  ClpLongArray,
  ClpGlvMultiplier,
  ClpWNafL2RMultiplier,
  ClpWTauNafMultiplier,
  ClpFiniteFields,
  ClpSetWeakRef,
  ClpECCurveConstants,
  ClpTnaf,
  ClpValidityPrecompInfo,
  ClpIValidityPrecompInfo,
  ClpIECC,
  ClpIFiniteField,
  ClpIPreCompInfo;

resourcestring
  SInvalidValue = 'Value Invalid in Fp Field Element, " x "';
  SInvalidValue2 = 'Value Invalid in F2m Field Element, "x"';
  SInvalidK2Value = 'k2 must be smaller than k3';
  SInvalidK2Value2 = 'k2 must be larger than 0';
  SInvalidFieldElement =
    'Field elements are not both instances of F2mFieldElement';
  SInvalidFieldElements =
    'Field elements are not elements of the same field F2m';
  SIncorrectRepresentation =
    'One of the F2m field elements has incorrect representation';
  SEvenValue = 'Even Value of Q';
  STraceInternalErrorCalculation = 'Internal Error in Trace Calculation';
  SHalfTraceUndefinedForM = 'Half-Trace Only Defined For Odd M';
  SUnSupportedCoordinateSystem = 'UnSupported Coordinate System';
  SCurrentCurve = 'Implementation returned Current Curve';
  SInvalidPointCoordinates = 'Invalid Point Coordinates';
  SInvalidAffineCoordinates = 'not valid for affine coordinates, "iso"';
  SInvalidPointOnCurve = 'must be non-null and on this curve, "point"';
  SInvalidPointOnCurve2 = 'Entries must be null or on this curve, "points"';
  SPointsNil = 'points';
  SInvalidRangeSpecified = 'Invalid Range Specified", "points"';
  SInvalidPointCompression = 'Invalid Point Compression';
  SInvalidK1 = 'k1 must be > 0';
  SInvalidK3 = 'k3 must be 0 if k2 == 0';
  SK2K1MisMatch = 'k2 must be > k1';
  SK3K2Mismatch = 'k3 must be > k2';
  SInvalidInfinityEncoding = 'Invalid Infinity Encoding, "encoded"';
  SInvalidPointEncoding = 'Invalid Point Encoding %u';
  SIncorrectLengthInfinityEncoding =
    'Incorrect Length for infinity encoding", "encoded"';
  SIncorrectLengthCompressedEncoding =
    'Incorrect Length for Compressed Encoding", "encoded"';
  SInvalidPoint = 'Invalid Point';
  SIncorrectLengthUnCompressedEncoding =
    'Incorrect Length for UnCompressed Encoding", "encoded"';
  SIncorrectLengthHybridEncoding =
    'Incorrect Length for Hybrid Encoding", "encoded"';
  SInConsistentYCoord =
    'Inconsistent Y Coordinate in Hybrid Encoding", "encoded"';
  SUnknownCoordSystem = 'Unknown Coordinate System';
  SPointNotInNormalForm = 'Point not in Normal Form';
  SNotProjectiveCoordSystem = 'Not a Projective Coordinate System';
  SCannotBeNegative = 'Cannot be Negative, "e"';
  SNilFieldElement = 'Exactly one of the Field Elements is Nil';

type
  TECFieldElement = class abstract(TInterfacedObject, IECFieldElement)

  strict protected

    function GetBitLength: Int32; virtual;
    function GetIsOne: Boolean; virtual;
    function GetIsZero: Boolean; virtual;

    function GetFieldName: String; virtual; abstract;
    function GetFieldSize: Int32; virtual; abstract;

  public

    constructor Create();
    destructor Destroy; override;

    function ToBigInteger(): TBigInteger; virtual; abstract;
    function Add(const b: IECFieldElement): IECFieldElement; virtual; abstract;
    function AddOne(): IECFieldElement; virtual; abstract;
    function Subtract(const b: IECFieldElement): IECFieldElement;
      virtual; abstract;
    function Multiply(const b: IECFieldElement): IECFieldElement;
      virtual; abstract;
    function Divide(const b: IECFieldElement): IECFieldElement;
      virtual; abstract;
    function Negate(): IECFieldElement; virtual; abstract;
    function Square(): IECFieldElement; virtual; abstract;
    function Invert(): IECFieldElement; virtual; abstract;
    function Sqrt(): IECFieldElement; virtual; abstract;

    function MultiplyMinusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; virtual;

    function MultiplyPlusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; virtual;

    function SquareMinusProduct(const x, y: IECFieldElement)
      : IECFieldElement; virtual;

    function SquarePlusProduct(const x, y: IECFieldElement)
      : IECFieldElement; virtual;

    function SquarePow(pow: Int32): IECFieldElement; virtual;

    function TestBitZero(): Boolean; virtual;

    function Equals(const other: IECFieldElement): Boolean;
      reintroduce; virtual;

    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    function ToString(): String; override;

    function GetEncoded(): TCryptoLibByteArray; virtual;

    property FieldName: string read GetFieldName;
    property FieldSize: Int32 read GetFieldSize;
    property BitLength: Int32 read GetBitLength;
    property IsOne: Boolean read GetIsOne;
    property IsZero: Boolean read GetIsZero;

  end;

type
  TAbstractFpFieldElement = class abstract(TECFieldElement,
    IAbstractFpFieldElement)

  end;

type
  TFpFieldElement = class(TAbstractFpFieldElement, IFpFieldElement)

  strict private
    Fq, Fr, Fx: TBigInteger;

    function GetQ: TBigInteger; inline;

    function CheckSqrt(const z: IECFieldElement): IECFieldElement; inline;
    function LucasSequence(const P, Q, K: TBigInteger)
      : TCryptoLibGenericArray<TBigInteger>;

  strict protected
    function ModAdd(const x1, x2: TBigInteger): TBigInteger; virtual;
    function ModDouble(const x: TBigInteger): TBigInteger; virtual;
    function ModHalf(const x: TBigInteger): TBigInteger; virtual;
    function ModHalfAbs(const x: TBigInteger): TBigInteger; virtual;
    function ModInverse(const x: TBigInteger): TBigInteger; virtual;
    function ModMult(const x1, x2: TBigInteger): TBigInteger; virtual;
    function ModReduce(const x: TBigInteger): TBigInteger; virtual;
    function ModSubtract(const x1, x2: TBigInteger): TBigInteger; virtual;

    /// <summary>
    /// return the field name for this field.
    /// </summary>
    /// <returns>
    /// return the string "Fp".
    /// </returns>
    function GetFieldName: String; override;
    function GetFieldSize: Int32; override;

  public
    constructor Create(const Q, x: TBigInteger); overload;
      deprecated 'Use ECCurve.FromBigInteger to construct field elements';

    constructor Create(const Q, r, x: TBigInteger); overload;

    destructor Destroy; override;

    function ToBigInteger(): TBigInteger; override;

    function Add(const b: IECFieldElement): IECFieldElement; override;
    function AddOne(): IECFieldElement; override;
    function Subtract(const b: IECFieldElement): IECFieldElement; override;

    function Multiply(const b: IECFieldElement): IECFieldElement; override;
    function Divide(const b: IECFieldElement): IECFieldElement; override;
    function Negate(): IECFieldElement; override;
    function Square(): IECFieldElement; override;

    function Invert(): IECFieldElement; override;

    /// <summary>
    /// return a sqrt root - the routine verifies that the calculation
    /// </summary>
    /// <returns>
    /// returns the right value - if none exists it returns null.
    /// </returns>
    function Sqrt(): IECFieldElement; override;

    function MultiplyMinusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;
    function MultiplyPlusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquareMinusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquarePlusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;

    property FieldName: string read GetFieldName;
    property FieldSize: Int32 read GetFieldSize;

    property Q: TBigInteger read GetQ;

    function Equals(const other: IFpFieldElement): Boolean; reintroduce; overload;

    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    class function CalculateResidue(const P: TBigInteger): TBigInteger; static;

  end;

type
  TAbstractF2mFieldElement = class abstract(TECFieldElement,
    IAbstractF2mFieldElement)

  public
    function Trace(): Int32; virtual;
    function HalfTrace(): IECFieldElement; virtual;

  end;

type
  /// **
  // * Class representing the Elements of the finite field
  // * <code>F<sub>2<sup>m</sup></sub></code> in polynomial basis (PB)
  // * representation. Both trinomial (Tpb) and pentanomial (Ppb) polynomial
  // * basis representations are supported. Gaussian normal basis (GNB)
  // * representation is not supported.
  // */
  TF2mFieldElement = class(TAbstractF2mFieldElement, IF2mFieldElement)

  strict private

  var
    Frepresentation, Fm: Int32;
    FKs: TCryptoLibInt32Array;
    Fx: TLongArray;
    // /**
    // * The exponent <code>m</code> of <code>F<sub>2<sup>m</sup></sub></code>.
    // */
    function GetM: Int32; inline;
    /// <summary>
    /// Tpb or Ppb.
    /// </summary>
    function GetRepresentation: Int32; inline;
    function GetKs: TCryptoLibInt32Array; inline;
    function GetX: TLongArray; inline;

    function GetK1: Int32; inline;
    function GetK2: Int32; inline;
    function GetK3: Int32; inline;

  strict protected

    function GetBitLength: Int32; override;
    function GetIsOne: Boolean; override;
    function GetIsZero: Boolean; override;

    function GetFieldName: String; override;
    function GetFieldSize: Int32; override;

  public

    const

    /// <summary>
    /// Indicates gaussian normal basis representation (GNB). Number
    /// chosen according to X9.62. GNB is not implemented at present. <br />
    /// </summary>
    Gnb = Int32(1);

    /// <summary>
    /// Indicates trinomial basis representation (Tpb). Number chosen
    /// according to X9.62. <br />
    /// </summary>
    Tpb = Int32(2);

    /// <summary>
    /// Indicates pentanomial basis representation (Ppb). Number chosen
    /// according to X9.62. <br />
    /// </summary>
    Ppb = Int32(3);

    // /**
    // * Constructor for Ppb.
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
    // * represents the reduction polynomial <code>f(z)</code>.
    // * @param x The BigInteger representing the value of the field element.
    // */
    constructor Create(m, k1, k2, k3: Int32; const x: TBigInteger); overload;
      deprecated 'Use ECCurve.FromBigInteger to construct field elements';
    // /**
    // * Constructor for Tpb.
    // * @param m  The exponent <code>m</code> of
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param k The integer <code>k</code> where <code>x<sup>m</sup> +
    // * x<sup>k</sup> + 1</code> represents the reduction
    // * polynomial <code>f(z)</code>.
    // * @param x The BigInteger representing the value of the field element.
    // */
    constructor Create(m, K: Int32; const x: TBigInteger); overload;
      deprecated 'Use ECCurve.FromBigInteger to construct field elements';

    constructor Create(m: Int32; const ks: TCryptoLibInt32Array;
      const x: TLongArray); overload;

    destructor Destroy; override;

    function TestBitZero(): Boolean; override;
    function ToBigInteger(): TBigInteger; override;

    function Add(const b: IECFieldElement): IECFieldElement; override;
    function AddOne(): IECFieldElement; override;
    function Subtract(const b: IECFieldElement): IECFieldElement; override;

    function Multiply(const b: IECFieldElement): IECFieldElement; override;
    function Divide(const b: IECFieldElement): IECFieldElement; override;
    function Negate(): IECFieldElement; override;
    function Square(): IECFieldElement; override;

    function Invert(): IECFieldElement; override;

    /// <summary>
    /// return a sqrt root - the routine verifies that the calculation
    /// </summary>
    /// <returns>
    /// returns the right value - if none exists it returns null.
    /// </returns>
    function Sqrt(): IECFieldElement; override;

    function MultiplyMinusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;
    function MultiplyPlusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquareMinusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquarePlusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquarePow(pow: Int32): IECFieldElement; override;

    function Equals(const other: IF2mFieldElement): Boolean; reintroduce; overload;

    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    // /**
    // * Checks, if the ECFieldElements <code>a</code> and <code>b</code>
    // * are elements of the same field <code>F<sub>2<sup>m</sup></sub></code>
    // * (having the same representation).
    // * @param a field element.
    // * @param b field element to be compared.
    // * @throws ArgumentException if <code>a</code> and <code>b</code>
    // * are not elements of the same field
    // * <code>F<sub>2<sup>m</sup></sub></code> (having the same
    // * representation).
    // */
    class procedure CheckFieldElements(const a, b: IECFieldElement); static;

    // /**
    // * @return the representation of the field
    // * <code>F<sub>2<sup>m</sup></sub></code>, either of
    // * {@link F2mFieldElement.Tpb} (trinomial
    // * basis representation) or
    // * {@link F2mFieldElement.Ppb} (pentanomial
    // * basis representation).
    // */
    property Representation: Int32 read GetRepresentation;

    // /**
    // * @return the degree <code>m</code> of the reduction polynomial
    // * <code>f(z)</code>.
    // */
    property m: Int32 read GetM;
    // /**
    // * @return Tpb: The integer <code>k</code> where <code>x<sup>m</sup> +
    // * x<sup>k</sup> + 1</code> represents the reduction polynomial
    // * <code>f(z)</code>.<br/>
    // * Ppb: The integer <code>k1</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    property k1: Int32 read GetK1;
    // /**
    // * @return Tpb: Always returns <code>0</code><br/>
    // * Ppb: The integer <code>k2</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    property k2: Int32 read GetK2;
    // /**
    // * @return Tpb: Always set to <code>0</code><br/>
    // * Ppb: The integer <code>k3</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    property k3: Int32 read GetK3;

    property ks: TCryptoLibInt32Array read GetKs;

    /// <summary>
    /// The <c>LongArray</c> holding the bits.
    /// </summary>
    property x: TLongArray read GetX;

    property FieldName: string read GetFieldName;
    property FieldSize: Int32 read GetFieldSize;
    property BitLength: Int32 read GetBitLength;
    property IsOne: Boolean read GetIsOne;
    property IsZero: Boolean read GetIsZero;

  end;

type

  /// <summary>
  /// Base class for an elliptic curve.
  /// </summary>
  TECCurve = class abstract(TInterfacedObject, IECCurve)

  strict private

    class procedure Boot(); static;
    class constructor CreateECCurve();
    class destructor DestroyECCurve();

  strict protected

    class var

      FLock: TCriticalSection;

  var
    Fm_field: IFiniteField;
    Fm_order, Fm_cofactor: TBigInteger;

    Fm_coord: Int32;

    Fm_endomorphism: IECEndomorphism;
    Fm_multiplier: IECMultiplier;
    Fm_a, Fm_b: IECFieldElement;

    constructor Create(const field: IFiniteField);

    procedure SetCoord(const Value: Int32); inline;
    procedure SetEndomorphism(const Value: IECEndomorphism); inline;
    procedure SetMultiplier(const Value: IECMultiplier); inline;
    function GetField: IFiniteField; virtual;
    function GetA: IECFieldElement; virtual;
    function GetB: IECFieldElement; virtual;
    function GetOrder: TBigInteger; virtual;
    function GetCofactor: TBigInteger; virtual;
    function GetCoordinateSystem: Int32; virtual;

    function GetFieldSize: Int32; virtual; abstract;
    function GetInfinity: IECPoint; virtual; abstract;

    function CloneCurve(): IECCurve; virtual; abstract;

    function CreateRawPoint(const x, y: IECFieldElement;
      withCompression: Boolean): IECPoint; overload; virtual; abstract;

    function CreateRawPoint(const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean): IECPoint; overload; virtual; abstract;

    function CreateDefaultMultiplier(): IECMultiplier; virtual;

    procedure CheckPoint(const point: IECPoint); virtual;

    procedure CheckPoints(const points: TCryptoLibGenericArray<IECPoint>);
      overload; virtual;

    procedure CheckPoints(const points: TCryptoLibGenericArray<IECPoint>;
      off, len: Int32); overload; virtual;

    function DecompressPoint(yTilde: Int32; const x1: TBigInteger): IECPoint;
      virtual; abstract;

  public

    type

    TConfig = class(TInterfacedObject, IConfig)

    strict protected
    var
      Fouter: IECCurve;
      Fcoord: Int32;
      Fendomorphism: IECEndomorphism;
      Fmultiplier: IECMultiplier;

    public
      constructor Create(const outer: IECCurve; coord: Int32;
        const endomorphism: IECEndomorphism;
        const multiplier: IECMultiplier); overload;

      destructor Destroy(); override;

      function SetCoordinateSystem(coord: Int32): IConfig; inline;
      function SetEndomorphism(const endomorphism: IECEndomorphism)
        : IConfig; inline;
      function SetMultiplier(const multiplier: IECMultiplier): IConfig; inline;
      function CreateCurve(): IECCurve;

    end;

  function FromBigInteger(const x: TBigInteger): IECFieldElement;
    virtual; abstract;
  function IsValidFieldElement(const x: TBigInteger): Boolean; virtual;
    abstract;

  function Configure(): IConfig; virtual;
  function ValidatePoint(const x, y: TBigInteger): IECPoint; overload; virtual;

  function ValidatePoint(const x, y: TBigInteger; withCompression: Boolean)
    : IECPoint; overload; virtual;
    deprecated 'Per-point compression property will be removed';

  /// <summary>
  /// Create a cache-safe lookup table for the specified sequence of points.
  /// All the points MUST <br />belong to this <c>ECCurve</c> instance, and
  /// MUST already be normalized.
  /// </summary>
  function CreateCacheSafeLookupTable(const points
    : TCryptoLibGenericArray<IECPoint>; off, len: Int32)
    : IECLookupTable; virtual;

  function CreatePoint(const x, y: TBigInteger): IECPoint; overload; virtual;

  function CreatePoint(const x, y: TBigInteger; withCompression: Boolean)
    : IECPoint; overload; virtual;
    deprecated 'Per-point compression property will be removed';

  function SupportsCoordinateSystem(coord: Int32): Boolean; virtual;

  function GetPreCompInfo(const point: IECPoint; const name: String)
    : IPreCompInfo; virtual;

  /// <summary>
  /// Compute a <c>PreCompInfo</c> for a point on this curve, under a given
  /// name. Used by <c>ECMultiplier</c> to save the precomputation for this <c>
  /// ECPoint</c> for use by subsequent multiplication.
  /// </summary>
  /// <param name="point">
  /// The <c>ECPoint</c> to store precomputations for.
  /// </param>
  /// <param name="name">
  /// A <c>String</c> used to index precomputations of different types.
  /// </param>
  /// <param name="callback">
  /// Called to calculate the <c>PreCompInfo</c>
  /// </param>
  function Precompute(const point: IECPoint; const name: String;
    const callback: IPreCompCallback): IPreCompInfo; virtual;

  function ImportPoint(const P: IECPoint): IECPoint; virtual;

  /// <summary>
  /// Normalization ensures that any projective coordinate is 1, and
  /// therefore that the x, y coordinates reflect those of the equivalent
  /// point in an affine coordinate system. Where more than one point is to
  /// be normalized, this method will generally be more efficient than
  /// normalizing each point separately.
  /// </summary>
  /// <param name="points">
  /// An array of points that will be updated in place with their normalized
  /// versions, where necessary
  /// </param>
  procedure NormalizeAll(const points: TCryptoLibGenericArray<IECPoint>);
    overload; virtual;

  /// <summary>
  /// Normalization ensures that any projective coordinate is 1, and
  /// therefore that the x, y coordinates reflect those of the equivalent
  /// point in an affine coordinate system. Where more than one point is to
  /// be normalized, this method will generally be more efficient than
  /// normalizing each point separately. An (optional) z-scaling factor can
  /// be applied; effectively each z coordinate is scaled by this value prior
  /// to normalization (but only one actual multiplication is needed).
  /// </summary>
  /// <param name="points">
  /// An array of points that will be updated in place with their normalized
  /// versions, where necessary
  /// </param>
  /// <param name="off">
  /// The start of the range of points to normalize
  /// </param>
  /// <param name="len">
  /// The length of the range of points to normalize
  /// </param>
  /// <param name="iso">
  /// The (optional) z-scaling factor - can be null
  /// </param>
  procedure NormalizeAll(const points: TCryptoLibGenericArray<IECPoint>;
    off, len: Int32; const iso: IECFieldElement); overload; virtual;

  function GetEndomorphism(): IECEndomorphism; virtual;

  /// <summary>
  /// Sets the default <c>ECMultiplier</c>, unless already set.
  /// </summary>
  function GetMultiplier(): IECMultiplier; virtual;

  /// <summary>
  /// Decode a point on this curve from its ASN.1 encoding. The different
  /// encodings are taken account of, including point compression for <br /><c>
  /// F</c><b>p</b> (X9.62 s 4.2.1 pg 17).
  /// </summary>
  /// <returns>
  /// The decoded point.
  /// </returns>
  function DecodePoint(const encoded: TCryptoLibByteArray): IECPoint; virtual;

  property coord: Int32 write SetCoord;
  property endomorphism: IECEndomorphism write SetEndomorphism;
  property multiplier: IECMultiplier write SetMultiplier;

  property FieldSize: Int32 read GetFieldSize;

  property Infinity: IECPoint read GetInfinity;

  property field: IFiniteField read GetField;

  property a: IECFieldElement read GetA;

  property b: IECFieldElement read GetB;

  property Order: TBigInteger read GetOrder;

  property Cofactor: TBigInteger read GetCofactor;

  property CoordinateSystem: Int32 read GetCoordinateSystem;

  function Equals(const other: IECCurve): Boolean; reintroduce;
  function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  destructor Destroy; override;

  class function GetAllCoordinateSystems(): TCryptoLibInt32Array;
    static; inline;

  end;

type
  TDefaultLookupTable = class(TInterfacedObject, IDefaultLookupTable,
    IECLookupTable)
  strict private
  var
    Fm_outer: IECCurve;
    Fm_table: TCryptoLibByteArray;
    Fm_size: Int32;

  public
    constructor Create(const outer: IECCurve; const table: TCryptoLibByteArray;
      size: Int32);
    function GetSize: Int32; virtual;
    function Lookup(index: Int32): IECPoint; virtual;
    property size: Int32 read GetSize;

  end;

type
  TAbstractFpCurve = class(TECCurve, IAbstractFpCurve)

  strict protected

    constructor Create(const Q: TBigInteger);
    function DecompressPoint(yTilde: Int32; const x1: TBigInteger)
      : IECPoint; override;

  public
    destructor Destroy; override;
    function IsValidFieldElement(const x: TBigInteger): Boolean; override;

  end;

type
  TDefaultF2mLookupTable = class(TInterfacedObject, IDefaultF2mLookupTable,
    IECLookupTable)
  strict private
  var
    Fm_outer: IF2mCurve;
    Fm_table: TCryptoLibInt64Array;
    Fm_size: Int32;

  public
    constructor Create(const outer: IF2mCurve;
      const table: TCryptoLibInt64Array; size: Int32);
    function GetSize: Int32; virtual;
    function Lookup(index: Int32): IECPoint; virtual;
    property size: Int32 read GetSize;

  end;

type
  TFpCurve = class(TAbstractFpCurve, IFpCurve)

  strict private
  const
    FP_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_JACOBIAN_MODIFIED);

  strict protected
  var
    Fm_q, Fm_r: TBigInteger;

    Fm_infinity: IFpPoint;

    constructor Create(const Q, r: TBigInteger; const a, b: IECFieldElement);
      overload; deprecated 'Use constructor taking order/cofactor';
    constructor Create(const Q, r: TBigInteger; const a, b: IECFieldElement;
      const Order, Cofactor: TBigInteger); overload;

    function GetQ: TBigInteger; virtual;
    function GetInfinity: IECPoint; override;
    function GetFieldSize: Int32; override;

    function CloneCurve(): IECCurve; override;
    function CreateRawPoint(const x, y: IECFieldElement;
      withCompression: Boolean): IECPoint; overload; override;

    function CreateRawPoint(const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean): IECPoint; overload; override;

  public
    constructor Create(const Q, a, b: TBigInteger); overload;
      deprecated 'Use constructor taking order/cofactor';
    constructor Create(const Q, a, b, Order, Cofactor: TBigInteger); overload;

    destructor Destroy; override;

    function FromBigInteger(const x: TBigInteger): IECFieldElement; override;
    function ImportPoint(const P: IECPoint): IECPoint; override;

    function SupportsCoordinateSystem(coord: Int32): Boolean; override;

    property Q: TBigInteger read GetQ;
    property Infinity: IECPoint read GetInfinity;
    property FieldSize: Int32 read GetFieldSize;

  end;

type
  TAbstractF2mCurve = class abstract(TECCurve, IAbstractF2mCurve)

  strict private

    /// <summary>
    /// The auxiliary values <c>s</c><b>0</b> and <c>s</c><b>1</b> used for
    /// partial modular reduction for Koblitz curves.
    /// </summary>
    Fsi: TCryptoLibGenericArray<TBigInteger>;

    class function BuildField(m, k1, k2, k3: Int32): IFiniteField; static;

  strict protected
    constructor Create(m, k1, k2, k3: Int32);

    /// <summary>
    /// Returns true if this is a Koblitz curve (ABC curve).
    /// </summary>
    /// <returns>
    /// true if this is a Koblitz curve (ABC curve), false otherwise
    /// </returns>
    function GetIsKoblitz: Boolean; virtual;

    function DecompressPoint(yTilde: Int32; const x1: TBigInteger)
      : IECPoint; override;

    // /**
    // * Solves a quadratic equation <code>z<sup>2</sup> + z = beta</code>(X9.62
    // * D.1.6) The other solution is <code>z + 1</code>.
    // *
    // * @param beta
    // *            The value to solve the qradratic equation for.
    // * @return the solution for <code>z<sup>2</sup> + z = beta</code> or
    // *         <code>null</code> if no solution exists.
    // */
    function SolveQuadraticEquation(const beta: IECFieldElement)
      : IECFieldElement;

  public

    destructor Destroy; override;

    function IsValidFieldElement(const x: TBigInteger): Boolean; override;

    function CreatePoint(const x, y: TBigInteger; withCompression: Boolean)
      : IECPoint; override;
      deprecated 'Per-point compression property will be removed';

    // /**
    // * @return the auxiliary values <code>s<sub>0</sub></code> and
    // * <code>s<sub>1</sub></code> used for partial modular reduction for
    // * Koblitz curves.
    // */
    function GetSi(): TCryptoLibGenericArray<TBigInteger>; virtual;

    property IsKoblitz: Boolean read GetIsKoblitz;

    class function Inverse(m: Int32; const ks: TCryptoLibInt32Array;
      const x: TBigInteger): TBigInteger; static; inline;

  end;

type
  // /**
  // * Elliptic curves over F2m. The Weierstrass equation is given by
  // * <code>y<sup>2</sup> + xy = x<sup>3</sup> + ax<sup>2</sup> + b</code>.
  // */
  TF2mCurve = class sealed(TAbstractF2mCurve, IF2mCurve)

  strict private
  const
    F2M_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_LAMBDA_PROJECTIVE);

  var
    // /**
    // * The exponent <code>m</code> of <code>F<sub>2<sup>m</sup></sub></code>.
    // */
    Fm: Int32;

    // /**
    // * TPB: The integer <code>k</code> where <code>x<sup>m</sup> +
    // * x<sup>k</sup> + 1</code> represents the reduction polynomial
    // * <code>f(z)</code>.<br/>
    // * PPB: The integer <code>k1</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    Fk1: Int32;

    // /**
    // * TPB: Always set to <code>0</code><br/>
    // * PPB: The integer <code>k2</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    Fk2: Int32;
    //
    // /**
    // * TPB: Always set to <code>0</code><br/>
    // * PPB: The integer <code>k3</code> where <code>x<sup>m</sup> +
    // * x<sup>k3</sup> + x<sup>k2</sup> + x<sup>k1</sup> + 1</code>
    // * represents the reduction polynomial <code>f(z)</code>.<br/>
    // */
    Fk3: Int32;

    /// <summary>
    /// The point at infinity on this curve.
    /// </summary>
    Fm_infinity: IF2mPoint;

    constructor Create(m, k1, k2, k3: Int32; const a, b: IECFieldElement;
      const Order, Cofactor: TBigInteger); overload;

    function GetM: Int32; inline;
    function GetK1: Int32; inline;
    function GetK2: Int32; inline;
    function GetK3: Int32; inline;

  strict protected
    function GetFieldSize: Int32; override;
    function GetInfinity: IECPoint; override;

    function CloneCurve(): IECCurve; override;
    function CreateDefaultMultiplier(): IECMultiplier; override;

    function CreateRawPoint(const x, y: IECFieldElement;
      withCompression: Boolean): IECPoint; overload; override;

    function CreateRawPoint(const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean): IECPoint; overload; override;

  public
    // /**
    // * Constructor for Trinomial Polynomial Basis (TPB).
    // * @param m  The exponent <code>m</code> of
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param k The integer <code>k</code> where <code>x<sup>m</sup> +
    // * x<sup>k</sup> + 1</code> represents the reduction
    // * polynomial <code>f(z)</code>.
    // * @param a The coefficient <code>a</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param b The coefficient <code>b</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // */
    constructor Create(m, K: Int32; const a, b: TBigInteger); overload;
      deprecated 'Use constructor taking order/cofactor';
    // /**
    // * Constructor for Trinomial Polynomial Basis (TPB).
    // * @param m  The exponent <code>m</code> of
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param k The integer <code>k</code> where <code>x<sup>m</sup> +
    // * x<sup>k</sup> + 1</code> represents the reduction
    // * polynomial <code>f(z)</code>.
    // * @param a The coefficient <code>a</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param b The coefficient <code>b</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param order The order of the main subgroup of the elliptic curve.
    // * @param cofactor The cofactor of the elliptic curve, i.e.
    // * <code>#E<sub>a</sub>(F<sub>2<sup>m</sup></sub>) = h * n</code>.
    // */
    constructor Create(m, K: Int32;
      const a, b, Order, Cofactor: TBigInteger); overload;

    // /**
    // * Constructor for Pentanomial Polynomial Basis (PPB).
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
    // * represents the reduction polynomial <code>f(z)</code>.
    // * @param a The coefficient <code>a</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param b The coefficient <code>b</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // */

    constructor Create(m, k1, k2, k3: Int32; const a, b: TBigInteger); overload;
      deprecated 'Use constructor taking order/cofactor';
    // /**
    // * Constructor for Pentanomial Polynomial Basis (PPB).
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
    // * represents the reduction polynomial <code>f(z)</code>.
    // * @param a The coefficient <code>a</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param b The coefficient <code>b</code> in the Weierstrass equation
    // * for non-supersingular elliptic curves over
    // * <code>F<sub>2<sup>m</sup></sub></code>.
    // * @param order The order of the main subgroup of the elliptic curve.
    // * @param cofactor The cofactor of the elliptic curve, i.e.
    // * <code>#E<sub>a</sub>(F<sub>2<sup>m</sup></sub>) = h * n</code>.
    // */
    constructor Create(m, k1, k2, k3: Int32;
      const a, b, Order, Cofactor: TBigInteger); overload;

    destructor Destroy; override;

    function SupportsCoordinateSystem(coord: Int32): Boolean; override;
    function FromBigInteger(const x: TBigInteger): IECFieldElement; override;

    /// <summary>
    /// Return true if curve uses a Trinomial basis.
    /// </summary>
    /// <returns>
    /// return true if curve Trinomial, false otherwise.
    /// </returns>
    function IsTrinomial(): Boolean; inline;

    function CreateCacheSafeLookupTable(const points
      : TCryptoLibGenericArray<IECPoint>; off, len: Int32)
      : IECLookupTable; override;

    property FieldSize: Int32 read GetFieldSize;
    property Infinity: IECPoint read GetInfinity;
    property m: Int32 read GetM;
    property k1: Int32 read GetK1;
    property k2: Int32 read GetK2;
    property k3: Int32 read GetK3;

  end;

type

  /// <summary>
  /// base class for points on elliptic curves.
  /// </summary>
  TECPoint = class abstract(TInterfacedObject, IECPoint)

  strict private

  type
    IValidityCallback = interface(IPreCompCallback)
      ['{FD571D52-9852-45A6-BD53-47765EB86F20}']

    end;

  type
    TValidityCallback = class(TInterfacedObject, IPreCompCallback,
      IValidityCallback)

    strict private
    var
      Fm_outer: IECPoint;
      Fm_decompressed, Fm_checkOrder: Boolean;

    public
      constructor Create(const outer: IECPoint;
        decompressed, checkOrder: Boolean);

      function Precompute(const existing: IPreCompInfo): IPreCompInfo;

    end;

  class constructor ECPoint();

  strict protected

    class var

      FEMPTY_ZS: TCryptoLibGenericArray<IECFieldElement>;

  var
    Fm_zs: TCryptoLibGenericArray<IECFieldElement>;
    Fm_withCompression: Boolean;
    Fm_curve: IECCurve;

    Fm_x, Fm_y: IECFieldElement;

    function GetIsInfinity: Boolean; inline;
    function GetIsCompressed: Boolean; inline;
    function GetpreCompTable: TDictionary<String, IPreCompInfo>; inline;
    procedure SetpreCompTable(const Value: TDictionary<String, IPreCompInfo>); inline;
    function GetCurve: IECCurve; virtual;
    function GetCurveCoordinateSystem: Int32; virtual;
    function GetAffineXCoord: IECFieldElement; virtual;
    function GetAffineYCoord: IECFieldElement; virtual;
    function GetXCoord: IECFieldElement; virtual;

    function GetCompressionYTilde: Boolean; virtual; abstract;

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;

    function SatisfiesOrder(): Boolean; virtual;
    function SatisfiesCurveEquation(): Boolean; virtual; abstract;
    function Detach(): IECPoint; virtual; abstract;

    function RawXCoord: IECFieldElement; inline;

    function RawYCoord: IECFieldElement; inline;

    function RawZCoords: TCryptoLibGenericArray<IECFieldElement>; inline;

    function CreateScaledPoint(const sx, sy: IECFieldElement)
      : IECPoint; virtual;

    procedure CheckNormalized(); virtual;

    property CurveCoordinateSystem: Int32 read GetCurveCoordinateSystem;

    property CompressionYTilde: Boolean read GetCompressionYTilde;

    class function GetInitialZCoords(const curve: IECCurve)
      : TCryptoLibGenericArray<IECFieldElement>; static;

  public
  var
    // Dictionary is (string -> PreCompInfo)
    Fm_preCompTable: TDictionary<String, IPreCompInfo>;

    function GetYCoord: IECFieldElement; virtual;
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;
    destructor Destroy; override;

    function GetDetachedPoint(): IECPoint; inline;
    function GetZCoord(index: Int32): IECFieldElement; virtual;
    function GetZCoords(): TCryptoLibGenericArray<IECFieldElement>; virtual;

    function IsNormalized(): Boolean; virtual;

    /// <summary>
    /// Normalization ensures that any projective coordinate is 1, and
    /// therefore that the x, y <br />coordinates reflect those of the
    /// equivalent point in an affine coordinate system.
    /// </summary>
    /// <returns>
    /// a new ECPoint instance representing the same point, but with
    /// normalized coordinates
    /// </returns>
    function Normalize(): IECPoint; overload; virtual;

    function Normalize(const zInv: IECFieldElement): IECPoint;
      overload; virtual;

    function ImplIsValid(decompressed, checkOrder: Boolean): Boolean;

    function IsValid(): Boolean; inline;
    function IsValidPartial(): Boolean; inline;

    function ScaleX(const scale: IECFieldElement): IECPoint; virtual;
    function ScaleY(const scale: IECFieldElement): IECPoint; virtual;

    function GetEncoded(): TCryptoLibByteArray; overload; virtual;
    function GetEncoded(compressed: Boolean): TCryptoLibByteArray; overload;
      virtual; abstract;

    function Add(const b: IECPoint): IECPoint; virtual; abstract;
    function Subtract(const b: IECPoint): IECPoint; virtual; abstract;
    function Negate(): IECPoint; virtual; abstract;
    function TimesPow2(e: Int32): IECPoint; virtual;

    function Twice(): IECPoint; virtual; abstract;
    function Multiply(b: TBigInteger): IECPoint; virtual; abstract;

    function TwicePlus(const b: IECPoint): IECPoint; virtual;

    function ThreeTimes(): IECPoint; virtual;

    function Equals(const other: IECPoint): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    function ToString(): String; override;

    property preCompTable: TDictionary<String, IPreCompInfo>
      read GetpreCompTable write SetpreCompTable;

    /// <summary>
    /// Returns the affine x-coordinate after checking that this point is
    /// normalized.
    /// </summary>
    /// <value>
    /// The affine x-coordinate of this point
    /// </value>
    /// <exception cref="ClpCryptoLibTypes|EInvalidOperationCryptoLibException">
    /// if the point is not normalized
    /// </exception>
    property AffineXCoord: IECFieldElement read GetAffineXCoord;
    /// <summary>
    /// Returns the affine y-coordinate after checking that this point is
    /// normalized.
    /// </summary>
    /// <value>
    /// The affine y-coordinate of this point
    /// </value>
    /// <exception cref="ClpCryptoLibTypes|EInvalidOperationCryptoLibException">
    /// if the point is not normalized
    /// </exception>
    property AffineYCoord: IECFieldElement read GetAffineYCoord;

    /// <summary>
    /// Returns the x-coordinate. <br />Caution: depending on the curve's
    /// coordinate system, this may not be the same value as in an <br />
    /// affine coordinate system; use Normalize() to get a point where the
    /// coordinates have their <br />affine values, or use AffineXCoord if
    /// you expect the point to already have been normalized.
    /// </summary>
    /// <value>
    /// the x-coordinate of this point
    /// </value>
    property XCoord: IECFieldElement read GetXCoord;
    /// <summary>
    /// Returns the y-coordinate. <br />Caution: depending on the curve's
    /// coordinate system, this may not be the same value as in an <br />
    /// affine coordinate system; use Normalize() to get a point where the
    /// coordinates have their <br />affine values, or use AffineYCoord if
    /// you expect the point to already have been normalized.
    /// </summary>
    /// <value>
    /// the y-coordinate of this point
    /// </value>
    property YCoord: IECFieldElement read GetYCoord;

    property curve: IECCurve read GetCurve;

    property IsInfinity: Boolean read GetIsInfinity;

    property IsCompressed: Boolean read GetIsCompressed;

  end;

type
  TECPointBase = class abstract(TECPoint, IECPointBase)

  strict protected

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

  public

    destructor Destroy; override;

    /// <summary>
    /// return the field element encoded with point compression. (S 4.3.6)
    /// </summary>
    function GetEncoded(compressed: Boolean): TCryptoLibByteArray; override;

    /// <summary>
    /// Multiplies this <c>ECPoint</c> by the given number.
    /// </summary>
    /// <param name="k">
    /// The multiplicator.
    /// </param>
    /// <returns>
    /// <c>k * this</c>
    /// </returns>
    function Multiply(K: TBigInteger): IECPoint; override;

  end;

type
  TAbstractFpPoint = class abstract(TECPointBase, IAbstractFpPoint)

  strict protected

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

    function GetCompressionYTilde(): Boolean; override;

    function SatisfiesCurveEquation(): Boolean; override;

    property CompressionYTilde: Boolean read GetCompressionYTilde;

  public

    destructor Destroy; override;
    function Subtract(const b: IECPoint): IECPoint; override;

  end;

type

  /// <summary>
  /// Elliptic curve points over Fp
  /// </summary>
  TFpPoint = class(TAbstractFpPoint, IFpPoint)

  strict protected

    function Detach(): IECPoint; override;

    function Two(const x: IECFieldElement): IECFieldElement; virtual;
    function Three(const x: IECFieldElement): IECFieldElement; virtual;
    function Four(const x: IECFieldElement): IECFieldElement; virtual;
    function Eight(const x: IECFieldElement): IECFieldElement; virtual;
    function DoubleProductFromSquares(const a, b, aSquared,
      bSquared: IECFieldElement): IECFieldElement; virtual;

    function CalculateJacobianModifiedW(const z: IECFieldElement;
      const ZSquared: IECFieldElement): IECFieldElement; virtual;

    function GetJacobianModifiedW(): IECFieldElement; virtual;

    function TwiceJacobianModified(calculateW: Boolean): IFpPoint; virtual;

  public

    /// <summary>
    /// Create a point which encodes without point compression.
    /// </summary>
    /// <param name="curve">
    /// curve the curve to use
    /// </param>
    /// <param name="x">
    /// affine x co-ordinate
    /// </param>
    /// <param name="y">
    /// affine y co-ordinate
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement);
      overload; deprecated 'Use ECCurve.CreatePoint to construct points';

    /// <summary>
    /// Create a point which encodes without point compression.
    /// </summary>
    /// <param name="curve">
    /// curve the curve to use
    /// </param>
    /// <param name="x">
    /// affine x co-ordinate
    /// </param>
    /// <param name="y">
    /// affine y co-ordinate
    /// </param>
    /// <param name="withCompression">
    /// if true encode with point compression
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;
      deprecated
      'Per-point compression property will be removed, see GetEncoded(boolean)';

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

    destructor Destroy; override;

    function GetZCoord(index: Int32): IECFieldElement; override;
    // B.3 pg 62
    function Add(const b: IECPoint): IECPoint; override;

    // B.3 pg 62
    function Twice(): IECPoint; override;

    function TwicePlus(const b: IECPoint): IECPoint; override;

    function ThreeTimes(): IECPoint; override;

    function TimesPow2(e: Int32): IECPoint; override;

    function Negate(): IECPoint; override;

  end;

type
  TAbstractF2mPoint = class abstract(TECPointBase, IAbstractF2mPoint)

  strict protected
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

    function SatisfiesOrder(): Boolean; override;
    function SatisfiesCurveEquation(): Boolean; override;

  public
    destructor Destroy; override;
    function ScaleX(const scale: IECFieldElement): IECPoint; override;
    function ScaleY(const scale: IECFieldElement): IECPoint; override;

    function Subtract(const b: IECPoint): IECPoint; override;

    function Tau(): IAbstractF2mPoint; virtual;

    function TauPow(pow: Int32): IAbstractF2mPoint; virtual;
  end;

type

  /// <summary>
  /// Elliptic curve points over F2m
  /// </summary>
  TF2mPoint = class(TAbstractF2mPoint, IF2mPoint)

  strict protected
    function GetCompressionYTilde: Boolean; override;
    function Detach(): IECPoint; override;
    property CompressionYTilde: Boolean read GetCompressionYTilde;
  public

    function GetYCoord: IECFieldElement; override;

    /// <param name="curve">
    /// base curve
    /// </param>
    /// <param name="x">
    /// x point
    /// </param>
    /// <param name="y">
    /// y point
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement);
      overload; deprecated 'Use ECCurve.CreatePoint to construct points';

    /// <param name="curve">
    /// base curve
    /// </param>
    /// <param name="x">
    /// x point
    /// </param>
    /// <param name="y">
    /// y point
    /// </param>
    /// <param name="withCompression">
    /// true if encode with point compression.
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;
      deprecated
      'Per-point compression property will be removed, see GetEncoded(boolean)';

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

    destructor Destroy; override;

    function Add(const b: IECPoint): IECPoint; override;

    function Twice(): IECPoint; override;

    function TwicePlus(const b: IECPoint): IECPoint; override;

    function Negate(): IECPoint; override;

    property YCoord: IECFieldElement read GetYCoord;
  end;

implementation

{ TF2mFieldElement }

function TF2mFieldElement.GetKs: TCryptoLibInt32Array;
begin
  result := FKs;
end;

function TF2mFieldElement.GetM: Int32;
begin
  result := Fm;
end;

function TF2mFieldElement.GetRepresentation: Int32;
begin
  result := Frepresentation;
end;

function TF2mFieldElement.GetX: TLongArray;
begin
  result := Fx;
end;

function TF2mFieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  iarrClone: TLongArray;
  bF2m: IF2mFieldElement;
begin
  // No check performed here for performance reasons. Instead the
  // elements involved are checked in ECPoint.F2m
  // checkFieldElements(this, b);
  iarrClone := Fx.Copy();
  bF2m := b as IF2mFieldElement;
  iarrClone.AddShiftedByWords(bF2m.x, 0);
  result := TF2mFieldElement.Create(Fm, FKs, iarrClone);
end;

function TF2mFieldElement.AddOne: IECFieldElement;
begin
  result := TF2mFieldElement.Create(Fm, FKs, Fx.AddOne());
end;

class procedure TF2mFieldElement.CheckFieldElements(const a,
  b: IECFieldElement);
var
  aF2m, bF2m: IF2mFieldElement;
begin
  if (not(Supports(a, IF2mFieldElement, aF2m)) or
    (not(Supports(b, IF2mFieldElement, bF2m)))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidFieldElement);
  end;

  if (aF2m.Representation <> bF2m.Representation) then
  begin
    // Should never occur
    raise EArgumentCryptoLibException.CreateRes(@SIncorrectRepresentation);
  end;

  if ((aF2m.m <> bF2m.m) or (not TArrayUtils.AreEqual(aF2m.ks, bF2m.ks))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidFieldElements);
  end;
end;

constructor TF2mFieldElement.Create(m, K: Int32; const x: TBigInteger);
begin
  Create(m, K, 0, 0, x);
end;

constructor TF2mFieldElement.Create(m, k1, k2, k3: Int32; const x: TBigInteger);
begin
  Inherited Create();
  if (not(x.IsInitialized) or (x.SignValue < 0) or (x.BitLength > m)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidValue2);
  end;

  if ((k2 = 0) and (k3 = 0)) then
  begin
    Frepresentation := Tpb;
    FKs := TCryptoLibInt32Array.Create(k1);
  end
  else
  begin
    if (k2 >= k3) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidK2Value);
    end;
    if (k2 <= 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidK2Value2);
    end;

    Frepresentation := Ppb;
    FKs := TCryptoLibInt32Array.Create(k1, k2, k3);
  end;

  Fm := m;
  Fx := TLongArray.Create(x);
end;

constructor TF2mFieldElement.Create(m: Int32; const ks: TCryptoLibInt32Array;
  const x: TLongArray);
begin
  Inherited Create();
  Fm := m;
  if (System.Length(ks) = 1) then
  begin
    Frepresentation := Tpb
  end
  else
  begin
    Frepresentation := Ppb;
  end;
  FKs := ks;
  Fx := x;
end;

destructor TF2mFieldElement.Destroy;
begin
  inherited Destroy;
end;

function TF2mFieldElement.Divide(const b: IECFieldElement): IECFieldElement;
var
  bInv: IECFieldElement;
begin
  // There may be more efficient implementations
  bInv := b.Invert();
  result := Multiply(bInv);
end;

function TF2mFieldElement.Equals(const other: IF2mFieldElement): Boolean;
begin
  if (other = Self as IF2mFieldElement) then
  begin
    result := true;
    Exit;
  end;
  if (Nil = other) then
  begin
    result := false;
    Exit;
  end;
  result := ((m = other.m) and (Representation = other.Representation) and
    TArrayUtils.AreEqual(ks, other.ks) and (x.Equals(other.x)));
end;

function TF2mFieldElement.GetBitLength: Int32;
begin
  result := Fx.Degree();
end;

function TF2mFieldElement.GetFieldName: String;
begin
  result := 'F2m';
end;

function TF2mFieldElement.GetFieldSize: Int32;
begin
  result := Fm;
end;

function TF2mFieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Fx.GetHashCode() xor Fm xor TArrayUtils.GetArrayHashCode(FKs);
end;

function TF2mFieldElement.GetIsOne: Boolean;
begin
  result := Fx.IsOne();
end;

function TF2mFieldElement.GetIsZero: Boolean;
begin
  result := Fx.IsZero();
end;

function TF2mFieldElement.GetK1: Int32;
begin
  result := FKs[0];
end;

function TF2mFieldElement.GetK2: Int32;
begin
  if (System.Length(FKs) >= 2) then
  begin
    result := FKs[1];
  end
  else
  begin
    result := 0;
  end;
end;

function TF2mFieldElement.GetK3: Int32;
begin
  if (System.Length(FKs) >= 3) then
  begin
    result := FKs[2];
  end
  else
  begin
    result := 0;
  end;
end;

function TF2mFieldElement.Invert: IECFieldElement;
begin
  result := TF2mFieldElement.Create(Fm, FKs, Fx.ModInverse(Fm, FKs));
end;

function TF2mFieldElement.Multiply(const b: IECFieldElement): IECFieldElement;
begin
  // Right-to-left comb multiplication in the LongArray
  // Input: Binary polynomials a(z) and b(z) of degree at most m-1
  // Output: c(z) = a(z) * b(z) mod f(z)

  // No check performed here for performance reasons. Instead the
  // elements involved are checked in ECPoint.F2m
  // checkFieldElements(this, b);
  result := TF2mFieldElement.Create(Fm, FKs,
    Fx.ModMultiply((b as IF2mFieldElement).x, Fm, FKs));
end;

function TF2mFieldElement.MultiplyMinusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := MultiplyPlusProduct(b, x, y);
end;

function TF2mFieldElement.MultiplyPlusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, bx, xx, yx, ab, xy: TLongArray;
begin
  ax := Fx;
  bx := (b as IF2mFieldElement).x;
  xx := (x as IF2mFieldElement).x;
  yx := (y as IF2mFieldElement).x;

  ab := ax.Multiply(bx, Fm, FKs);
  xy := xx.Multiply(yx, Fm, FKs);

  if ((ab.Equals(ax)) or (ab.Equals(bx))) then
  begin
    ab := ab.Copy();
  end;

  ab.AddShiftedByWords(xy, 0);
  ab.Reduce(Fm, FKs);

  result := TF2mFieldElement.Create(Fm, FKs, ab);
end;

function TF2mFieldElement.Negate: IECFieldElement;
begin
  // -x == x holds for all x in F2m
  result := Self as IECFieldElement;
end;

function TF2mFieldElement.Sqrt: IECFieldElement;
begin
  if ((Fx.IsZero()) or (Fx.IsOne())) then
  begin
    result := Self as IECFieldElement;
  end
  else
  begin
    result := SquarePow(Fm - 1);
  end;
end;

function TF2mFieldElement.Square: IECFieldElement;
begin
  result := TF2mFieldElement.Create(Fm, FKs, Fx.ModSquare(Fm, FKs));
end;

function TF2mFieldElement.SquareMinusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := SquarePlusProduct(x, y);
end;

function TF2mFieldElement.SquarePlusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, xx, yx, aa, xy: TLongArray;
begin
  ax := Fx;
  xx := (x as IF2mFieldElement).x;
  yx := (y as IF2mFieldElement).x;

  aa := ax.Square(Fm, FKs);
  xy := xx.Multiply(yx, Fm, FKs);

  if (aa.Equals(ax)) then
  begin
    aa := aa.Copy();
  end;

  aa.AddShiftedByWords(xy, 0);
  aa.Reduce(Fm, FKs);

  result := TF2mFieldElement.Create(Fm, FKs, aa);
end;

function TF2mFieldElement.SquarePow(pow: Int32): IECFieldElement;
begin
  if pow < 1 then
  begin
    result := Self as IECFieldElement
  end
  else
  begin
    result := TF2mFieldElement.Create(Fm, FKs, Fx.ModSquareN(pow, Fm, FKs));
  end;
end;

function TF2mFieldElement.Subtract(const b: IECFieldElement): IECFieldElement;
begin
  // Addition and subtraction are the same in F2m
  result := Add(b);
end;

function TF2mFieldElement.TestBitZero: Boolean;
begin
  result := Fx.TestBitZero();
end;

function TF2mFieldElement.ToBigInteger: TBigInteger;
begin
  result := Fx.ToBigInteger();
end;

{ TECFieldElement }

constructor TECFieldElement.Create;
begin
  Inherited Create();
end;

destructor TECFieldElement.Destroy;
begin
  inherited Destroy;
end;

function TECFieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  if (other = Self as IECFieldElement) then
  begin
    result := true;
    Exit;
  end;
  if (Nil = other) then
  begin
    result := false;
    Exit;
  end;
  result := ToBigInteger().Equals(other.ToBigInteger());
end;

function TECFieldElement.GetBitLength: Int32;
begin
  result := ToBigInteger().BitLength;
end;

function TECFieldElement.GetEncoded: TCryptoLibByteArray;
begin
  result := TBigIntegers.AsUnsignedByteArray((FieldSize + 7) div 8,
    ToBigInteger());
end;

function TECFieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := ToBigInteger().GetHashCode();
end;

function TECFieldElement.GetIsOne: Boolean;
begin
  result := BitLength = 1;
end;

function TECFieldElement.GetIsZero: Boolean;
begin
  result := 0 = ToBigInteger().SignValue;
end;

function TECFieldElement.MultiplyMinusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := Multiply(b).Subtract(x.Multiply(y));
end;

function TECFieldElement.MultiplyPlusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := Multiply(b).Add(x.Multiply(y));
end;

function TECFieldElement.SquareMinusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := Square().Subtract(x.Multiply(y));
end;

function TECFieldElement.SquarePlusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := Square().Add(x.Multiply(y));
end;

function TECFieldElement.SquarePow(pow: Int32): IECFieldElement;
var
  r: IECFieldElement;
  i: Int32;
begin
  r := Self as IECFieldElement;
  i := 0;
  while i < pow do
  begin
    r := r.Square();
    System.Inc(i);
  end;

  result := r;
end;

function TECFieldElement.TestBitZero: Boolean;
begin
  result := ToBigInteger().TestBit(0);
end;

function TECFieldElement.ToString: String;
begin
  result := ToBigInteger().ToString(16);
end;

{ TFpFieldElement }

function TFpFieldElement.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TFpFieldElement.GetFieldSize: Int32;
begin
  result := Q.BitLength;
end;

function TFpFieldElement.Add(const b: IECFieldElement): IECFieldElement;
begin
  result := TFpFieldElement.Create(Fq, Fr, ModAdd(Fx, b.ToBigInteger()));
end;

function TFpFieldElement.AddOne: IECFieldElement;
var
  x2: TBigInteger;
begin
  x2 := Fx.Add(TBigInteger.One);
  if (x2.CompareTo(Q) = 0) then
  begin
    x2 := TBigInteger.Zero;
  end;
  result := TFpFieldElement.Create(Fq, Fr, x2);
end;

class function TFpFieldElement.CalculateResidue(const P: TBigInteger)
  : TBigInteger;
var
  BitLength: Int32;
  firstWord: TBigInteger;
begin
  BitLength := P.BitLength;
  if (BitLength >= 96) then
  begin
    firstWord := P.ShiftRight(BitLength - 64);
    if (firstWord.Int64Value = Int64(-1)) then
    begin
      result := TBigInteger.One.ShiftLeft(BitLength).Subtract(P);
      Exit;
    end;
    if ((BitLength and 7) = 0) then
    begin
      result := TBigInteger.One.ShiftLeft(BitLength shl 1).Divide(P).Negate();
      Exit;
    end;
  end;
  result := Default (TBigInteger);
end;

function TFpFieldElement.CheckSqrt(const z: IECFieldElement): IECFieldElement;
begin
  if (z.Square().Equals(Self as IECFieldElement)) then
  begin
    result := z;
  end
  else
  begin
    result := Nil;
  end;
end;

constructor TFpFieldElement.Create(const Q, x: TBigInteger);
begin
  Create(Q, CalculateResidue(Q), x);
end;

constructor TFpFieldElement.Create(const Q, r, x: TBigInteger);
begin
  Inherited Create();
  if (not(x.IsInitialized) or (x.SignValue < 0) or (x.CompareTo(Q) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidValue);
  end;

  Fq := Q;
  Fr := r;
  Fx := x;
end;

destructor TFpFieldElement.Destroy;
begin
  inherited Destroy;
end;

function TFpFieldElement.Divide(const b: IECFieldElement): IECFieldElement;
begin
  result := TFpFieldElement.Create(Fq, Fr,
    ModMult(Fx, ModInverse(b.ToBigInteger())));
end;

function TFpFieldElement.Equals(const other: IFpFieldElement): Boolean;
begin
  if (other = Self as IFpFieldElement) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;

  result := (Q.Equals(other.Q) and (Inherited Equals(other)));
end;

function TFpFieldElement.GetFieldName: String;
begin
  result := 'Fp';
end;

function TFpFieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Q.GetHashCode() xor (Inherited GetHashCode());
end;

function TFpFieldElement.Invert: IECFieldElement;
begin
  // TODO Modular inversion can be faster for a (Generalized) Mersenne Prime.
  result := TFpFieldElement.Create(Fq, Fr, ModInverse(Fx));
end;

function TFpFieldElement.LucasSequence(const P, Q, K: TBigInteger)
  : TCryptoLibGenericArray<TBigInteger>;
var
  n, s, j: Int32;
  Uh, Vl, Vh, Ql, Qh: TBigInteger;
begin
  // TODO Research and apply "common-multiplicand multiplication here"

  n := K.BitLength;
  s := K.GetLowestSetBit();

{$IFDEF DEBUG}
  System.Assert(K.TestBit(s));
{$ENDIF DEBUG}
  Uh := TBigInteger.One;
  Vl := TBigInteger.Two;
  Vh := P;
  Ql := TBigInteger.One;
  Qh := TBigInteger.One;

  j := n - 1;

  while j >= s + 1 do
  begin
    Ql := ModMult(Ql, Qh);

    if (K.TestBit(j)) then
    begin
      Qh := ModMult(Ql, Q);
      Uh := ModMult(Uh, Vh);
      Vl := ModReduce(Vh.Multiply(Vl).Subtract(P.Multiply(Ql)));
      Vh := ModReduce(Vh.Multiply(Vh).Subtract(Qh.ShiftLeft(1)));
    end
    else
    begin
      Qh := Ql;
      Uh := ModReduce(Uh.Multiply(Vl).Subtract(Ql));
      Vh := ModReduce(Vh.Multiply(Vl).Subtract(P.Multiply(Ql)));
      Vl := ModReduce(Vl.Multiply(Vl).Subtract(Ql.ShiftLeft(1)));
    end;
    System.Dec(j);
  end;

  Ql := ModMult(Ql, Qh);
  Qh := ModMult(Ql, Q);
  Uh := ModReduce(Uh.Multiply(Vl).Subtract(Ql));
  Vl := ModReduce(Vh.Multiply(Vl).Subtract(P.Multiply(Ql)));
  Ql := ModMult(Ql, Qh);

  j := 1;

  while j <= s do
  begin
    Uh := ModMult(Uh, Vl);
    Vl := ModReduce(Vl.Multiply(Vl).Subtract(Ql.ShiftLeft(1)));
    Ql := ModMult(Ql, Ql);
    System.Inc(j);
  end;

  result := TCryptoLibGenericArray<TBigInteger>.Create(Uh, Vl);
end;

function TFpFieldElement.ModAdd(const x1, x2: TBigInteger): TBigInteger;
var
  x3: TBigInteger;
begin
  x3 := x1.Add(x2);
  if (x3.CompareTo(Q) >= 0) then
  begin
    x3 := x3.Subtract(Q);
  end;
  result := x3;
end;

function TFpFieldElement.ModDouble(const x: TBigInteger): TBigInteger;
var
  _2x: TBigInteger;
begin
  _2x := x.ShiftLeft(1);
  if (_2x.CompareTo(Q) >= 0) then
  begin
    _2x := _2x.Subtract(Q);
  end;
  result := _2x;
end;

function TFpFieldElement.ModHalf(const x: TBigInteger): TBigInteger;
var
  Lx: TBigInteger;
begin
  Lx := x;
  if (Lx.TestBit(0)) then
  begin
    Lx := Q.Add(Lx);
  end;
  result := Lx.ShiftRight(1);
end;

function TFpFieldElement.ModHalfAbs(const x: TBigInteger): TBigInteger;
var
  Lx: TBigInteger;
begin
  Lx := x;
  if (Lx.TestBit(0)) then
  begin
    Lx := Q.Subtract(Lx);
  end;
  result := Lx.ShiftRight(1);
end;

function TFpFieldElement.ModInverse(const x: TBigInteger): TBigInteger;
var
  bits, len: Int32;
  P, n, z: TCryptoLibUInt32Array;
begin
  bits := FieldSize;
  len := TBits.Asr32((bits + 31), 5);
  P := TNat.FromBigInteger(bits, Q);
  n := TNat.FromBigInteger(bits, x);
  z := TNat.Create(len);

  TMod.Invert(P, n, z);

  result := TNat.ToBigInteger(len, z);
end;

function TFpFieldElement.ModMult(const x1, x2: TBigInteger): TBigInteger;
begin
  result := ModReduce(x1.Multiply(x2));
end;

function TFpFieldElement.ModReduce(const x: TBigInteger): TBigInteger;
var
  negative, rIsOne: Boolean;
  qLen, d: Int32;
  qMod, u, v, mu, quot, bk1, Lx: TBigInteger;
begin
  Lx := x;
  if (not(Fr.IsInitialized)) then
  begin
    Lx := Lx.&Mod(Q);
  end
  else
  begin
    negative := Lx.SignValue < 0;
    if (negative) then
    begin
      Lx := Lx.Abs();
    end;
    qLen := Q.BitLength;
    if (Fr.SignValue > 0) then
    begin
      qMod := TBigInteger.One.ShiftLeft(qLen);
      rIsOne := Fr.Equals(TBigInteger.One);
      while (Lx.BitLength > (qLen + 1)) do
      begin
        u := Lx.ShiftRight(qLen);
        v := Lx.Remainder(qMod);
        if (not rIsOne) then
        begin
          u := u.Multiply(Fr);
        end;
        Lx := u.Add(v);
      end
    end
    else
    begin
      d := ((qLen - 1) and 31) + 1;
      mu := Fr.Negate();
      u := mu.Multiply(Lx.ShiftRight(qLen - d));
      quot := u.ShiftRight(qLen + d);
      v := quot.Multiply(Q);
      bk1 := TBigInteger.One.ShiftLeft(qLen + d);
      v := v.Remainder(bk1);
      Lx := Lx.Remainder(bk1);
      Lx := Lx.Subtract(v);
      if (Lx.SignValue < 0) then
      begin
        Lx := Lx.Add(bk1);
      end
    end;
    while (Lx.CompareTo(Q) >= 0) do
    begin
      Lx := Lx.Subtract(Q);
    end;
    if ((negative) and (Lx.SignValue <> 0)) then
    begin
      Lx := Q.Subtract(Lx);
    end;
  end;
  result := Lx;
end;

function TFpFieldElement.ModSubtract(const x1, x2: TBigInteger): TBigInteger;
var
  x3: TBigInteger;
begin
  x3 := x1.Subtract(x2);
  if (x3.SignValue < 0) then
  begin
    x3 := x3.Add(Q);
  end;
  result := x3;
end;

function TFpFieldElement.Multiply(const b: IECFieldElement): IECFieldElement;
begin
  result := TFpFieldElement.Create(Fq, Fr, ModMult(Fx, b.ToBigInteger()));
end;

function TFpFieldElement.MultiplyMinusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, bx, xx, yx, ab, xy: TBigInteger;
begin
  ax := Fx;
  bx := b.ToBigInteger();
  xx := x.ToBigInteger();
  yx := y.ToBigInteger();
  ab := ax.Multiply(bx);
  xy := xx.Multiply(yx);
  result := TFpFieldElement.Create(Fq, Fr, ModReduce(ab.Subtract(xy)));
end;

function TFpFieldElement.MultiplyPlusProduct(const b, x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, bx, xx, yx, ab, xy, sum: TBigInteger;
begin
  ax := Fx;
  bx := b.ToBigInteger();
  xx := x.ToBigInteger();
  yx := y.ToBigInteger();
  ab := ax.Multiply(bx);
  xy := xx.Multiply(yx);
  sum := ab.Add(xy);
  if ((Fr.IsInitialized) and (Fr.SignValue < 0) and
    (sum.BitLength > (Fq.BitLength shl 1))) then
  begin
    sum := sum.Subtract(Fq.ShiftLeft(Q.BitLength));
  end;
  result := TFpFieldElement.Create(Fq, Fr, ModReduce(sum));
end;

function TFpFieldElement.Negate: IECFieldElement;
begin
  if Fx.SignValue = 0 then
  begin
    result := Self as IECFieldElement
  end
  else
  begin
    result := TFpFieldElement.Create(Fq, Fr, Fq.Subtract(Fx));
  end;
end;

function TFpFieldElement.Sqrt: IECFieldElement;
var
  u, v, K, e, t1, t2, t3, t4, y, legendreExponent, x, fourX, qMinusOne,
    P: TBigInteger;
  tempRes: TCryptoLibGenericArray<TBigInteger>;
  CompareRes, ModReduceRes: Boolean;
begin
  if (IsZero or IsOne) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  if (not Fq.TestBit(0)) then
  begin
    raise ENotImplementedCryptoLibException.CreateRes(@SEvenValue);
  end;

  if (Fq.TestBit(1)) then // q == 4m + 3
  begin
    e := Fq.ShiftRight(2).Add(TBigInteger.One);
    result := CheckSqrt(TFpFieldElement.Create(Fq, Fr, Fx.ModPow(e, Fq))
      as IFpFieldElement);
    Exit;
  end;

  if (Fq.TestBit(2)) then // q == 8m + 5
  begin
    t1 := Fx.ModPow(Fq.ShiftRight(3), Fq);
    t2 := ModMult(t1, Fx);
    t3 := ModMult(t2, t1);

    if (t3.Equals(TBigInteger.One)) then
    begin
      result := CheckSqrt(TFpFieldElement.Create(Fq, Fr, t2)
        as IFpFieldElement);
      Exit;
    end;

    // TODO This is constant and could be precomputed
    t4 := TBigInteger.Two.ModPow(Fq.ShiftRight(2), Fq);

    y := ModMult(t2, t4);

    result := CheckSqrt(TFpFieldElement.Create(Fq, Fr, y) as IFpFieldElement);
    Exit;
  end;

  // q == 8m + 1

  legendreExponent := Fq.ShiftRight(1);
  if (not(Fx.ModPow(legendreExponent, Fq).Equals(TBigInteger.One))) then
  begin
    result := Nil;
    Exit;
  end;

  x := Fx;
  fourX := ModDouble(ModDouble(x));

  K := legendreExponent.Add(TBigInteger.One);
  qMinusOne := Fq.Subtract(TBigInteger.One);

  repeat

    repeat
      P := TBigInteger.Arbitrary(Fq.BitLength);

      CompareRes := P.CompareTo(Q) >= 0;
      ModReduceRes := (not ModReduce(P.Multiply(P).Subtract(fourX))
        .ModPow(legendreExponent, Q).Equals(qMinusOne));

    until ((not CompareRes) and (not ModReduceRes));

    tempRes := LucasSequence(P, x, K);
    u := tempRes[0];
    v := tempRes[1];

    if (ModMult(v, v).Equals(fourX)) then
    begin
      result := TFpFieldElement.Create(Fq, Fr, ModHalfAbs(v));
      Exit;
    end;

  until ((not u.Equals(TBigInteger.One)) or (not u.Equals(qMinusOne)));
  result := Nil;
end;

function TFpFieldElement.Square: IECFieldElement;
begin
  result := TFpFieldElement.Create(Fq, Fr, ModMult(Fx, Fx));
end;

function TFpFieldElement.SquareMinusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, xx, yx, aa, xy: TBigInteger;
begin
  ax := Fx;
  xx := x.ToBigInteger();
  yx := y.ToBigInteger();
  aa := ax.Multiply(ax);
  xy := xx.Multiply(yx);
  result := TFpFieldElement.Create(Fq, Fr, ModReduce(aa.Subtract(xy)));
end;

function TFpFieldElement.SquarePlusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, xx, yx, aa, xy, sum: TBigInteger;
begin
  ax := Fx;
  xx := x.ToBigInteger();
  yx := y.ToBigInteger();
  aa := ax.Multiply(ax);
  xy := xx.Multiply(yx);
  sum := aa.Add(xy);
  if ((Fr.IsInitialized) and (Fr.SignValue < 0) and
    (sum.BitLength > (Fq.BitLength shl 1))) then
  begin
    sum := sum.Subtract(Fq.ShiftLeft(Fq.BitLength));
  end;
  result := TFpFieldElement.Create(Fq, Fr, ModReduce(sum));
end;

function TFpFieldElement.Subtract(const b: IECFieldElement): IECFieldElement;
begin
  result := TFpFieldElement.Create(Fq, Fr, ModSubtract(Fx, b.ToBigInteger()));
end;

function TFpFieldElement.ToBigInteger: TBigInteger;
begin
  result := Fx;
end;

{ TAbstract2mFieldElement }

function TAbstractF2mFieldElement.HalfTrace: IECFieldElement;
var
  m, i: Int32;
  fe, ht: IECFieldElement;
begin
  m := FieldSize;
  if ((m and 1) = 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SHalfTraceUndefinedForM);
  end;

  fe := Self as IECFieldElement;
  ht := fe;
  i := 2;
  while i < m do
  begin
    fe := fe.SquarePow(2);
    ht := ht.Add(fe);
    System.Inc(i, 2);
  end;

  result := ht;
end;

function TAbstractF2mFieldElement.Trace: Int32;
var
  m, i: Int32;
  fe, tr: IECFieldElement;
begin
  m := FieldSize;
  fe := Self as IECFieldElement;
  tr := fe;

  i := 1;
  while i < m do
  begin
    fe := fe.Square();
    tr := tr.Add(fe);
    System.Inc(i);
  end;

  if (tr.IsZero) then
  begin
    result := 0;
    Exit;
  end;
  if (tr.IsOne) then
  begin
    result := 1;
    Exit;
  end;
  raise EArgumentCryptoLibException.CreateRes(@STraceInternalErrorCalculation);
end;

{ TECCurve }

class procedure TECCurve.Boot;
begin
  if FLock = Nil then
  begin
    FLock := TCriticalSection.Create;
  end;
end;

procedure TECCurve.CheckPoint(const point: IECPoint);
begin
  if ((point = Nil) or ((Self as IECCurve) <> point.curve)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointOnCurve);
  end;
end;

procedure TECCurve.CheckPoints(const points: TCryptoLibGenericArray<IECPoint>);
begin
  CheckPoints(points, 0, System.Length(points));
end;

procedure TECCurve.CheckPoints(const points: TCryptoLibGenericArray<IECPoint>;
  off, len: Int32);
var
  i: Int32;
  point: IECPoint;
begin
  if (points = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPointsNil);
  end;
  if ((off < 0) or (len < 0) or (off > (System.Length(points) - len))) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidRangeSpecified);
  end;

  for i := 0 to System.Pred(len) do

  begin
    point := points[off + i];
    if ((point <> Nil) and ((Self as IECCurve) <> point.curve)) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidPointOnCurve2);
    end;
  end;
end;

function TECCurve.Configure: IConfig;
begin
  result := TConfig.Create(Self as IECCurve, Self.Fm_coord,
    Self.Fm_endomorphism, Self.Fm_multiplier);
end;

constructor TECCurve.Create(const field: IFiniteField);
begin
  inherited Create();
  Fm_field := field;
end;

function TECCurve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  FE_BYTES, position, i, pxStart, pyStart, pxLen, pyLen: Int32;
  table, px, py: TCryptoLibByteArray;
  P: IECPoint;
begin
  FE_BYTES := (FieldSize + 7) div 8;
  System.SetLength(table, len * FE_BYTES * 2);
  position := 0;

  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    px := P.RawXCoord.ToBigInteger().ToByteArray();
    py := P.RawYCoord.ToBigInteger().ToByteArray();

    if System.Length(px) > FE_BYTES then
    begin
      pxStart := 1
    end
    else
    begin
      pxStart := 0
    end;

    pxLen := System.Length(px) - pxStart;

    if System.Length(py) > FE_BYTES then
    begin
      pyStart := 1
    end
    else
    begin
      pyStart := 0
    end;

    pyLen := System.Length(py) - pyStart;

    System.Move(px[pxStart], table[position + FE_BYTES - pxLen],
      pxLen * System.SizeOf(Byte));
    position := position + FE_BYTES;

    System.Move(py[pyStart], table[position + FE_BYTES - pyLen],
      pyLen * System.SizeOf(Byte));
    position := position + FE_BYTES;
  end;
  result := TDefaultLookupTable.Create(Self as IECCurve, table, len);
end;

function TECCurve.CreateDefaultMultiplier: IECMultiplier;
var
  glvEndomorphism: IGlvEndomorphism;
begin
  if (Supports(Fm_endomorphism, IGlvEndomorphism, glvEndomorphism)) then
  begin
    result := TGlvMultiplier.Create(Self as IECCurve, glvEndomorphism);
    Exit;
  end;

  result := TWNafL2RMultiplier.Create();
end;

class constructor TECCurve.CreateECCurve;
begin
  TECCurve.Boot;
end;

function TECCurve.CreatePoint(const x, y: TBigInteger): IECPoint;
begin
  result := CreatePoint(x, y, false);
end;

function TECCurve.CreatePoint(const x, y: TBigInteger; withCompression: Boolean)
  : IECPoint;
begin
  result := CreateRawPoint(FromBigInteger(x), FromBigInteger(y),
    withCompression);
end;

function TECCurve.DecodePoint(const encoded: TCryptoLibByteArray): IECPoint;
var
  x, y: TBigInteger;
  P: IECPoint;
  expectedLength, yTilde: Int32;
  ltype: Byte;
begin
  P := Nil;
  expectedLength := (FieldSize + 7) div 8;

  ltype := encoded[0];
  case ltype of
    $00: // infinity
      begin
        if (System.Length(encoded) <> 1) then
        begin
          raise EArgumentCryptoLibException.CreateRes
            (@SIncorrectLengthInfinityEncoding);
        end;

        P := Infinity;
      end;

    $02, // compressed
    $03: // compressed
      begin
        if (System.Length(encoded) <> (expectedLength + 1)) then
        begin
          raise EArgumentCryptoLibException.CreateRes
            (@SIncorrectLengthCompressedEncoding);
        end;

        yTilde := ltype and 1;
        x := TBigInteger.Create(1, encoded, 1, expectedLength);

        P := DecompressPoint(yTilde, x);
        // TODO Skip curve equation check?
        if ((not P.ImplIsValid(true, true))) then
        begin
          raise EArgumentCryptoLibException.CreateRes(@SInvalidPoint);
        end;
      end;

    $04: // uncompressed
      begin
        if (System.Length(encoded) <> ((2 * expectedLength) + 1)) then
        begin
          raise EArgumentCryptoLibException.CreateRes
            (@SIncorrectLengthUnCompressedEncoding);
        end;

        x := TBigInteger.Create(1, encoded, 1, expectedLength);
        y := TBigInteger.Create(1, encoded, 1 + expectedLength, expectedLength);

        P := ValidatePoint(x, y);
      end;

    $06, // hybrid
    $07: // hybrid
      begin
        if (System.Length(encoded) <> ((2 * expectedLength) + 1)) then
        begin
          raise EArgumentCryptoLibException.CreateRes
            (@SIncorrectLengthHybridEncoding);
        end;

        x := TBigInteger.Create(1, encoded, 1, expectedLength);
        y := TBigInteger.Create(1, encoded, 1 + expectedLength, expectedLength);

        if ((y.TestBit(0)) <> (ltype = $07)) then
        begin
          raise EArgumentCryptoLibException.CreateRes(@SInConsistentYCoord);
        end;

        P := ValidatePoint(x, y);
      end

  else
    begin
      raise EFormatCryptoLibException.CreateResFmt
        (@SInvalidPointEncoding, [ltype]);
    end;

  end;

  if ((ltype <> $00) and (P.IsInfinity)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidInfinityEncoding);
  end;

  result := P;
end;

destructor TECCurve.Destroy;
begin
  inherited Destroy;
end;

class destructor TECCurve.DestroyECCurve;
begin
  FLock.Free;
end;

function TECCurve.Equals(const other: IECCurve): Boolean;
begin
  if ((Self as IECCurve) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := (field as TObject).Equals(other.field as TObject) and
    (a.ToBigInteger().Equals(other.a.ToBigInteger())) and
    (b.ToBigInteger().Equals(other.b.ToBigInteger()));
end;

function TECCurve.GetA: IECFieldElement;
begin
  result := Fm_a;
end;

class function TECCurve.GetAllCoordinateSystems: TCryptoLibInt32Array;
begin
  result := TCryptoLibInt32Array.Create(TECCurveConstants.COORD_AFFINE,
    TECCurveConstants.COORD_HOMOGENEOUS, TECCurveConstants.COORD_JACOBIAN,
    TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
    TECCurveConstants.COORD_JACOBIAN_MODIFIED,
    TECCurveConstants.COORD_LAMBDA_AFFINE,
    TECCurveConstants.COORD_LAMBDA_PROJECTIVE, TECCurveConstants.COORD_SKEWED);
end;

function TECCurve.GetB: IECFieldElement;
begin
  result := Fm_b;
end;

function TECCurve.GetCofactor: TBigInteger;
begin
  result := Fm_cofactor;
end;

function TECCurve.GetCoordinateSystem: Int32;
begin
  result := Fm_coord;
end;

function TECCurve.GetEndomorphism: IECEndomorphism;
begin
  result := Fm_endomorphism;
end;

function TECCurve.GetField: IFiniteField;
begin
  result := Fm_field;
end;

function TECCurve.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := (field as TObject).GetHashCode()
    xor Int32(TBits.RotateLeft32(a.ToBigInteger().GetHashCode(), 8))
    xor Int32(TBits.RotateLeft32(b.ToBigInteger().GetHashCode(), 16));
end;

function TECCurve.GetMultiplier: IECMultiplier;
begin

  FLock.Acquire;
  try
    if (Fm_multiplier = Nil) then
    begin
      Fm_multiplier := CreateDefaultMultiplier();
    end;
    result := Fm_multiplier;
  finally
    FLock.Release;
  end;

end;

function TECCurve.GetOrder: TBigInteger;
begin
  result := Fm_order;
end;

function TECCurve.GetPreCompInfo(const point: IECPoint; const name: String)
  : IPreCompInfo;
var
  table: TDictionary<String, IPreCompInfo>;
begin
  CheckPoint(point);

  FLock.Acquire;
  try
    table := point.preCompTable;
    if table = Nil then
    begin
      result := Nil;
    end
    else
    begin
      table.TryGetValue(name, result);
    end;
  finally
    FLock.Release;
  end;
end;

function TECCurve.ImportPoint(const P: IECPoint): IECPoint;
var
  Lp: IECPoint;
begin
  if ((Self as IECCurve) = P.curve) then
  begin
    result := P;
    Exit;
  end;
  if (P.IsInfinity) then
  begin
    result := Infinity;
    Exit;
  end;

  // TODO Default behaviour could be improved if the two curves have the same coordinate system by copying any Z coordinates.
  Lp := P.Normalize();

  result := CreatePoint(Lp.XCoord.ToBigInteger(), Lp.YCoord.ToBigInteger(),
    Lp.IsCompressed);
end;

procedure TECCurve.NormalizeAll(const points: TCryptoLibGenericArray<IECPoint>;
  off, len: Int32; const iso: IECFieldElement);
var
  zs: TCryptoLibGenericArray<IECFieldElement>;
  indices: TCryptoLibInt32Array;
  count, i, j, index: Int32;
  P: IECPoint;
begin
  CheckPoints(points, off, len);
  case CoordinateSystem of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        if (iso <> Nil) then
        begin
          raise EArgumentCryptoLibException.CreateRes
            (@SInvalidAffineCoordinates);
        end;

        Exit;
      end;
  end;

  // /*
  // * Figure out which of the points actually need to be normalized
  // */
  System.SetLength(zs, len);
  System.SetLength(indices, len);

  count := 0;
  for i := 0 to System.Pred(len) do

  begin
    P := points[off + i];
    if ((P <> Nil) and ((iso <> Nil) or (not(P.IsNormalized())))) then
    begin
      zs[count] := P.GetZCoord(0);
      indices[count] := off + i;
      System.Inc(count);
    end;
  end;

  if (count = 0) then
  begin
    Exit;
  end;

  TECAlgorithms.MontgomeryTrick(zs, 0, count, iso);

  for j := 0 to System.Pred(count) do

  begin
    index := indices[j];
    points[index] := points[index].Normalize(zs[j]);
  end;

end;

procedure TECCurve.NormalizeAll(const points: TCryptoLibGenericArray<IECPoint>);
begin
  NormalizeAll(points, 0, System.Length(points), Nil);
end;

procedure TECCurve.SetCoord(const Value: Int32);
begin
  Fm_coord := Value;
end;

procedure TECCurve.SetEndomorphism(const Value: IECEndomorphism);
begin
  Fm_endomorphism := Value;
end;

procedure TECCurve.SetMultiplier(const Value: IECMultiplier);
begin
  Fm_multiplier := Value;
end;

function TECCurve.Precompute(const point: IECPoint; const name: String;
  const callback: IPreCompCallback): IPreCompInfo;
var
  table: TDictionary<String, IPreCompInfo>;
  existing: IPreCompInfo;
begin
  CheckPoint(point);

  FLock.Acquire;
  try
    table := point.preCompTable;
    if table = Nil then
    begin
      table := TDictionary<String, IPreCompInfo>.Create(4);
      point.preCompTable := table;
    end;

    table.TryGetValue(name, existing);

    result := callback.Precompute(existing);

    if (result <> existing) then
    begin
      table.AddOrSetValue(name, result);
    end;

  finally
    FLock.Release;
  end;
end;

function TECCurve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  result := coord = TECCurveConstants.COORD_AFFINE;
end;

function TECCurve.ValidatePoint(const x, y: TBigInteger;
  withCompression: Boolean): IECPoint;
var
  P: IECPoint;
begin
  P := CreatePoint(x, y, withCompression);
  if (not P.IsValid()) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointCoordinates);
  end;
  result := P;
end;

function TECCurve.ValidatePoint(const x, y: TBigInteger): IECPoint;
var
  P: IECPoint;
begin
  P := CreatePoint(x, y);
  if (not P.IsValid()) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointCoordinates);
  end;
  result := P;
end;

{ TECCurve.TConfig }

constructor TECCurve.TConfig.Create(const outer: IECCurve; coord: Int32;
  const endomorphism: IECEndomorphism; const multiplier: IECMultiplier);
begin
  Inherited Create();
  Fouter := outer;
  Fcoord := coord;
  Fendomorphism := endomorphism;
  Fmultiplier := multiplier;
end;

function TECCurve.TConfig.CreateCurve: IECCurve;
var
  c: IECCurve;
begin
  if (not Fouter.SupportsCoordinateSystem(Fcoord)) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes
      (@SUnSupportedCoordinateSystem);
  end;

  c := Fouter.CloneCurve();
  if (c = Fouter) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SCurrentCurve);
  end;

  c.coord := Fcoord;
  c.endomorphism := Fendomorphism;
  c.multiplier := Fmultiplier;

  result := c;
end;

destructor TECCurve.TConfig.Destroy;
begin
  inherited Destroy;
end;

function TECCurve.TConfig.SetCoordinateSystem(coord: Int32): IConfig;
begin
  Fcoord := coord;
  result := Self as IConfig;
end;

function TECCurve.TConfig.SetEndomorphism(const endomorphism
  : IECEndomorphism): IConfig;
begin
  Fendomorphism := endomorphism;
  result := Self as IConfig;
end;

function TECCurve.TConfig.SetMultiplier(const multiplier
  : IECMultiplier): IConfig;
begin
  Fmultiplier := multiplier;
  result := Self as IConfig;
end;

{ TAbstractFpCurve }

constructor TAbstractFpCurve.Create(const Q: TBigInteger);
begin
  Inherited Create(TFiniteFields.GetPrimeField(Q));
end;

function TAbstractFpCurve.DecompressPoint(yTilde: Int32; const x1: TBigInteger)
  : IECPoint;
var
  x, rhs, y: IECFieldElement;
begin
  x := FromBigInteger(x1);
  rhs := x.Square().Add(a).Multiply(x).Add(b);
  y := rhs.Sqrt();

  // /*
  // * If y is not a square, then we haven't got a point on the curve
  // */
  if (y = Nil) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointCompression);
  end;

  if (y.TestBitZero() <> (yTilde = 1)) then
  begin
    // Use the other root
    y := y.Negate();
  end;

  result := CreateRawPoint(x, y, true);
end;

destructor TAbstractFpCurve.Destroy;
begin
  inherited Destroy;
end;

function TAbstractFpCurve.IsValidFieldElement(const x: TBigInteger): Boolean;
begin
  result := (x.IsInitialized) and (x.SignValue >= 0) and
    (x.CompareTo(field.Characteristic) < 0);
end;

{ TFpCurve }

function TFpCurve.CloneCurve: IECCurve;
begin
  result := TFpCurve.Create(Fm_q, Fm_r, Fm_a, Fm_b, Fm_order, Fm_cofactor);
end;

constructor TFpCurve.Create(const Q, r: TBigInteger;
  const a, b: IECFieldElement; const Order, Cofactor: TBigInteger);
begin
  Inherited Create(Q);
  Fm_q := Q;
  Fm_r := r;
  Fm_infinity := TFpPoint.Create(Self as IECCurve, Nil, Nil, false);

  Fm_a := a;
  Fm_b := b;
  Fm_order := Order;
  Fm_cofactor := Cofactor;
  Fm_coord := FP_DEFAULT_COORDS;
end;

constructor TFpCurve.Create(const Q, r: TBigInteger;
  const a, b: IECFieldElement);
begin
  Create(Q, r, a, b, Default (TBigInteger), Default (TBigInteger));
end;

constructor TFpCurve.Create(const Q, a, b, Order, Cofactor: TBigInteger);
begin
  Inherited Create(Q);
  Fm_q := Q;
  Fm_r := TFpFieldElement.CalculateResidue(Q);
  Fm_infinity := TFpPoint.Create(Self as IECCurve, Nil, Nil, false);

  Fm_a := FromBigInteger(a);
  Fm_b := FromBigInteger(b);
  Fm_order := Order;
  Fm_cofactor := Cofactor;
  Fm_coord := FP_DEFAULT_COORDS;
end;

constructor TFpCurve.Create(const Q, a, b: TBigInteger);
begin
  Create(Q, a, b, Default (TBigInteger), Default (TBigInteger));
end;

function TFpCurve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TFpPoint.Create(Self as IECCurve, x, y, zs, withCompression);
end;

destructor TFpCurve.Destroy;
begin
  inherited Destroy;
end;

function TFpCurve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TFpPoint.Create(Self as IECCurve, x, y, withCompression);
end;

function TFpCurve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TFpFieldElement.Create(Fm_q, Fm_r, x);
end;

function TFpCurve.GetFieldSize: Int32;
begin
  result := Fm_q.BitLength;
end;

function TFpCurve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TFpCurve.GetQ: TBigInteger;
begin
  result := Fm_q;
end;

function TFpCurve.ImportPoint(const P: IECPoint): IECPoint;
begin
  if ((Self as IECCurve <> P.curve) and
    (CoordinateSystem = TECCurveConstants.COORD_JACOBIAN) and (not P.IsInfinity))
  then
  begin
    case P.curve.CoordinateSystem of
      TECCurveConstants.COORD_JACOBIAN,
        TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
        TECCurveConstants.COORD_JACOBIAN_MODIFIED:
        begin
          result := TFpPoint.Create(Self as IECCurve,
            FromBigInteger(P.RawXCoord.ToBigInteger()),
            FromBigInteger(P.RawYCoord.ToBigInteger()),
            TCryptoLibGenericArray<IECFieldElement>.Create
            (FromBigInteger(P.GetZCoord(0).ToBigInteger())), P.IsCompressed);
          Exit;
        end;
    end;
  end;

  result := (Inherited ImportPoint(P));
end;

function TFpCurve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_JACOBIAN,
      TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := true;
      end
  else
    begin
      result := false;
    end;
  end;
end;

{ TAbstractF2mCurve }

class function TAbstractF2mCurve.BuildField(m, k1, k2, k3: Int32): IFiniteField;
begin
  if (k1 = 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidK1);
  end;

  if (k2 = 0) then
  begin
    if (k3 <> 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidK3);
    end;

    result := TFiniteFields.GetBinaryExtensionField
      (TCryptoLibInt32Array.Create(0, k1, m));
    Exit;
  end;

  if (k2 <= k1) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SK2K1MisMatch);
  end;

  if (k3 <= k2) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SK3K2Mismatch);
  end;

  result := TFiniteFields.GetBinaryExtensionField(TCryptoLibInt32Array.Create(0,
    k1, k2, k3, m));
end;

constructor TAbstractF2mCurve.Create(m, k1, k2, k3: Int32);
begin
  Inherited Create(BuildField(m, k1, k2, k3));
end;

function TAbstractF2mCurve.CreatePoint(const x, y: TBigInteger;
  withCompression: Boolean): IECPoint;
var
  Lx, LY: IECFieldElement;
begin
  Lx := FromBigInteger(x);
  LY := FromBigInteger(y);

  case CoordinateSystem of
    TECCurveConstants.COORD_LAMBDA_AFFINE,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        if (Lx.IsZero) then
        begin
          if (not LY.Square().Equals(b)) then
          begin
            raise EArgumentCryptoLibException.Create('');
          end;
        end
        else
        begin
          // Y becomes Lambda (X + Y/X) here
          LY := LY.Divide(Lx).Add(Lx);
        end;
      end;
  end;

  result := CreateRawPoint(Lx, LY, withCompression);
end;

function TAbstractF2mCurve.DecompressPoint(yTilde: Int32; const x1: TBigInteger)
  : IECPoint;
var
  xp, yp, beta, z: IECFieldElement;
begin
  xp := FromBigInteger(x1);
  yp := Nil;
  if (xp.IsZero) then
  begin
    yp := b.Sqrt();
  end
  else
  begin
    beta := xp.Square().Invert().Multiply(b).Add(a).Add(xp);
    z := SolveQuadraticEquation(beta);

    if (z <> Nil) then
    begin
      if (z.TestBitZero() <> (yTilde = 1)) then
      begin
        z := z.AddOne();
      end;

      case CoordinateSystem of
        TECCurveConstants.COORD_LAMBDA_AFFINE,
          TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
          begin
            yp := z.Add(xp);
          end
      else
        begin
          yp := z.Multiply(xp);
        end;
      end;

    end;

  end;

  if (yp = Nil) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidPointCompression);
  end;

  result := CreateRawPoint(xp, yp, true);
end;

destructor TAbstractF2mCurve.Destroy;
begin
  inherited Destroy;
end;

function TAbstractF2mCurve.GetIsKoblitz: Boolean;
begin
  result := (Fm_order.IsInitialized) and (Fm_cofactor.IsInitialized) and
    (Fm_b.IsOne) and (Fm_a.IsZero or Fm_a.IsOne);
end;

function TAbstractF2mCurve.GetSi: TCryptoLibGenericArray<TBigInteger>;
begin
  if (Fsi = Nil) then
  begin

    FLock.Acquire;
    try
      if (Fsi = Nil) then
      begin
        Fsi := TTnaf.GetSi(Self as IAbstractF2mCurve);
      end;
    finally
      FLock.Release;
    end;
  end;
  result := Fsi;
end;

class function TAbstractF2mCurve.Inverse(m: Int32;
  const ks: TCryptoLibInt32Array; const x: TBigInteger): TBigInteger;
begin
  result := TLongArray.Create(x).ModInverse(m, ks).ToBigInteger();
end;

function TAbstractF2mCurve.IsValidFieldElement(const x: TBigInteger): Boolean;
begin
  result := (x.IsInitialized) and (x.SignValue >= 0) and
    (x.BitLength <= FieldSize);
end;

function TAbstractF2mCurve.SolveQuadraticEquation(const beta: IECFieldElement)
  : IECFieldElement;
var
  gamma, z, zeroElement, t, w, w2: IECFieldElement;
  m, i: Int32;
begin
  if (beta.IsZero) then
  begin
    result := beta;
    Exit;
  end;

  zeroElement := FromBigInteger(TBigInteger.Zero);

  m := FieldSize;

  repeat
    t := FromBigInteger(TBigInteger.Arbitrary(m));
    z := zeroElement;
    w := beta;
    i := 1;
    while i < m do
    begin
      w2 := w.Square();
      z := z.Square().Add(w2.Multiply(t));
      w := w2.Add(beta);
      System.Inc(i);
    end;

    if (not w.IsZero) then
    begin
      result := Nil;
      Exit;
    end;
    gamma := z.Square().Add(z);
  until (not(gamma.IsZero));

  result := z;
end;

{ TF2mCurve }

function TF2mCurve.GetFieldSize: Int32;
begin
  result := Fm;
end;

function TF2mCurve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TF2mCurve.GetK1: Int32;
begin
  result := Fk1;
end;

function TF2mCurve.GetK2: Int32;
begin
  result := Fk2;
end;

function TF2mCurve.GetK3: Int32;
begin
  result := Fk3;
end;

function TF2mCurve.GetM: Int32;
begin
  result := Fm;
end;

function TF2mCurve.IsTrinomial: Boolean;
begin
  result := (k2 = 0) and (k3 = 0);
end;

function TF2mCurve.CloneCurve: IECCurve;
begin
  result := TF2mCurve.Create(m, k1, k2, k3, Fm_a, Fm_b, Fm_order, Fm_cofactor);
end;

constructor TF2mCurve.Create(m, K: Int32; const a, b: TBigInteger);
begin
  Create(m, K, 0, 0, a, b, Default (TBigInteger), Default (TBigInteger));
end;

constructor TF2mCurve.Create(m, k1, k2, k3: Int32; const a, b: IECFieldElement;
  const Order, Cofactor: TBigInteger);
begin
  Inherited Create(m, k1, k2, k3);
  Fm := m;
  Fk1 := k1;
  Fk2 := k2;
  Fk3 := k3;
  Fm_order := Order;
  Fm_cofactor := Cofactor;

  Fm_infinity := TF2mPoint.Create(Self as IECCurve, Nil, Nil, false);
  Fm_a := a;
  Fm_b := b;
  Fm_coord := F2M_DEFAULT_COORDS;
end;

constructor TF2mCurve.Create(m, K: Int32;
  const a, b, Order, Cofactor: TBigInteger);
begin
  Create(m, K, 0, 0, a, b, Order, Cofactor);
end;

constructor TF2mCurve.Create(m, k1, k2, k3: Int32;
  const a, b, Order, Cofactor: TBigInteger);
begin
  Inherited Create(m, k1, k2, k3);
  Fm := m;
  Fk1 := k1;
  Fk2 := k2;
  Fk3 := k3;
  Fm_order := Order;
  Fm_cofactor := Cofactor;
  Fm_infinity := TF2mPoint.Create(Self as IECCurve, Nil, Nil, false);

  if (k1 = 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidK1);
  end;

  if (k2 = 0) then
  begin
    if (k3 <> 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidK3);
    end;
  end
  else
  begin
    if (k2 <= k1) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SK2K1MisMatch);
    end;

    if (k3 <= k2) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SK3K2Mismatch);
    end;
  end;

  Fm_a := FromBigInteger(a);
  Fm_b := FromBigInteger(b);
  Fm_coord := F2M_DEFAULT_COORDS;

end;

function TF2mCurve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  FE_LONGS, position, i: Int32;
  table: TCryptoLibInt64Array;
  P: IECPoint;
begin
  FE_LONGS := (m + 63) div 64;
  System.SetLength(table, len * FE_LONGS * 2);

  position := 0;

  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    (P.RawXCoord as IF2mFieldElement).x.CopyTo(table, position);
    position := position + FE_LONGS;
    (P.RawYCoord as IF2mFieldElement).x.CopyTo(table, position);
    position := position + FE_LONGS;
  end;

  result := TDefaultF2mLookupTable.Create(Self as IF2mCurve, table, len);
end;

constructor TF2mCurve.Create(m, k1, k2, k3: Int32; const a, b: TBigInteger);
begin
  Create(m, k1, k2, k3, a, b, Default (TBigInteger), Default (TBigInteger));
end;

function TF2mCurve.CreateDefaultMultiplier: IECMultiplier;
begin
  if (IsKoblitz) then
  begin
    result := TWTauNafMultiplier.Create();
    Exit;
  end;

  result := (Inherited CreateDefaultMultiplier());
end;

function TF2mCurve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TF2mPoint.Create(Self as IECCurve, x, y, withCompression);
end;

function TF2mCurve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TF2mPoint.Create(Self as IECCurve, x, y, zs, withCompression);
end;

destructor TF2mCurve.Destroy;
begin
  inherited Destroy;
end;

function TF2mCurve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TF2mFieldElement.Create(Fm, Fk1, Fk2, Fk3, x);
end;

function TF2mCurve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        result := true;
      end
  else
    begin
      result := false;
    end;
  end;
end;

{ TDefaultLookupTable }

constructor TDefaultLookupTable.Create(const outer: IECCurve;
  const table: TCryptoLibByteArray; size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TDefaultLookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TDefaultLookupTable.Lookup(index: Int32): IECPoint;
var
  FE_BYTES, position, i, j: Int32;
  x, y: TCryptoLibByteArray;
  MASK: Byte;
  XFieldElement, YFieldElement: IECFieldElement;
begin
  FE_BYTES := (Fm_outer.FieldSize + 7) div 8;
  System.SetLength(x, FE_BYTES);
  System.SetLength(y, FE_BYTES);

  position := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin

    MASK := Byte(TBits.Asr32((i xor index) - 1, 31));

    for j := 0 to System.Pred(FE_BYTES) do
    begin

      x[j] := x[j] xor Byte(Fm_table[position + j] and MASK);
      y[j] := y[j] xor Byte(Fm_table[position + FE_BYTES + j] and MASK);
    end;
    position := position + (FE_BYTES * 2);
  end;

  XFieldElement := Fm_outer.FromBigInteger(TBigInteger.Create(1, x));
  YFieldElement := Fm_outer.FromBigInteger(TBigInteger.Create(1, y));
  result := Fm_outer.CreateRawPoint(XFieldElement, YFieldElement, false);
end;

{ TDefaultF2mLookupTable }

constructor TDefaultF2mLookupTable.Create(const outer: IF2mCurve;
  const table: TCryptoLibInt64Array; size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TDefaultF2mLookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TDefaultF2mLookupTable.Lookup(index: Int32): IECPoint;
var
  FE_LONGS, position, m, i, j: Int32;
  ks: TCryptoLibInt32Array;
  x, y: TCryptoLibInt64Array;
  MASK: Int64;
  XFieldElement, YFieldElement: IECFieldElement;
begin
  m := Fm_outer.m;
  if Fm_outer.IsTrinomial() then
  begin
    ks := TCryptoLibInt32Array.Create(Fm_outer.k1);
  end
  else
  begin
    ks := TCryptoLibInt32Array.Create(Fm_outer.k1, Fm_outer.k2, Fm_outer.k3);
  end;

  FE_LONGS := (Fm_outer.m + 63) div 64;
  System.SetLength(x, FE_LONGS);
  System.SetLength(y, FE_LONGS);

  position := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin

    MASK := TBits.Asr32((i xor index) - 1, 31);

    for j := 0 to System.Pred(FE_LONGS) do
    begin

      x[j] := x[j] xor (Fm_table[position + j] and MASK);
      y[j] := y[j] xor (Fm_table[position + FE_LONGS + j] and MASK);
    end;
    position := position + (FE_LONGS * 2);
  end;

  XFieldElement := TF2mFieldElement.Create(m, ks, TLongArray.Create(x));
  YFieldElement := TF2mFieldElement.Create(m, ks, TLongArray.Create(y));
  result := Fm_outer.CreateRawPoint(XFieldElement, YFieldElement, false);
end;

{ TECPoint }

function TECPoint.GetIsCompressed: Boolean;
begin
  result := Fm_withCompression;
end;

function TECPoint.GetIsInfinity: Boolean;
begin
  // result := (Fm_x = Nil) and (Fm_y = Nil);
  result := (Fm_x = Nil) or (Fm_y = Nil) or
    ((System.Length(Fm_zs) > 0) and (Fm_zs[0].IsZero));
end;

function TECPoint.RawXCoord: IECFieldElement;
begin
  result := Fm_x;
end;

function TECPoint.RawYCoord: IECFieldElement;
begin
  result := Fm_y;
end;

function TECPoint.RawZCoords: TCryptoLibGenericArray<IECFieldElement>;
begin
  result := Fm_zs;
end;

function TECPoint.Normalize: IECPoint;
var
  Z1: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  case CurveCoordinateSystem of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        result := Self;
        Exit;
      end
  else
    begin

      Z1 := RawZCoords[0];
      if (Z1.IsOne) then
      begin
        result := Self;
        Exit;
      end;

      result := Normalize(Z1.Invert());
    end;
  end;
end;

function TECPoint.SatisfiesOrder: Boolean;
var
  n: TBigInteger;
begin
  if (TBigInteger.One.Equals(curve.GetCofactor())) then
  begin
    result := true;
    Exit;
  end;

  n := curve.GetOrder();

  // TODO Require order to be available for all curves

  result := (not(n.IsInitialized)) or TECAlgorithms.ReferenceMultiply
    (Self as IECPoint, n).IsInfinity;
end;

function TECPoint.ScaleX(const scale: IECFieldElement): IECPoint;
begin
  if IsInfinity then
  begin
    result := Self;
  end
  else
  begin
    result := curve.CreateRawPoint(RawXCoord.Multiply(scale), RawYCoord,
      RawZCoords, IsCompressed);
  end;
end;

function TECPoint.ScaleY(const scale: IECFieldElement): IECPoint;
begin
  if IsInfinity then
  begin
    result := Self;
  end
  else
  begin
    result := curve.CreateRawPoint(RawXCoord, RawYCoord.Multiply(scale),
      RawZCoords, IsCompressed);
  end;
end;

procedure TECPoint.SetpreCompTable(const Value
  : TDictionary<String, IPreCompInfo>);
begin
  Fm_preCompTable := Value;
end;

function TECPoint.ThreeTimes: IECPoint;
begin
  result := TwicePlus(Self);
end;

function TECPoint.TimesPow2(e: Int32): IECPoint;
var
  P: IECPoint;
begin
  if (e < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SCannotBeNegative);
  end;

  P := Self;
  System.Dec(e);
  while (e >= 0) do
  begin
    P := P.Twice();
    System.Dec(e);
  end;
  result := P;
end;

function TECPoint.ToString: String;
var
  sl: TStringList;
  i: Int32;
begin
  if (IsInfinity) then
  begin
    result := 'INF';
    Exit;
  end;

  sl := TStringList.Create();
  sl.LineBreak := '';
  try
    sl.Add('(');
    sl.Add(RawXCoord.ToString);
    sl.Add(',');
    sl.Add(RawYCoord.ToString);
    for i := 0 to System.Pred(System.Length(Fm_zs)) do
    begin
      sl.Add(',');
      sl.Add(Fm_zs[i].ToString);
    end;
    sl.Add(')');
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TECPoint.TwicePlus(const b: IECPoint): IECPoint;
begin
  result := Twice().Add(b);
end;

constructor TECPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  withCompression: Boolean);
begin
  Create(curve, x, y, GetInitialZCoords(curve), withCompression);
end;

procedure TECPoint.CheckNormalized;
begin
  if (not IsNormalized()) then
  begin
    raise EInvalidOperationCryptoLibException.CreateRes(@SPointNotInNormalForm);
  end;
end;

constructor TECPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create();
  // Fm_curve := curve;
  TSetWeakRef.SetWeakReference(@Fm_curve, curve);
  Fm_x := x;
  Fm_y := y;
  Fm_zs := zs;
  Fm_withCompression := withCompression;
end;

function TECPoint.CreateScaledPoint(const sx, sy: IECFieldElement): IECPoint;
begin
  result := curve.CreateRawPoint(RawXCoord.Multiply(sx), RawYCoord.Multiply(sy),
    IsCompressed);
end;

destructor TECPoint.Destroy;
begin
  TSetWeakRef.SetWeakReference(@Fm_curve, Nil);
  Fm_preCompTable.Free;
  inherited Destroy;
end;

class constructor TECPoint.ECPoint;
begin
  System.SetLength(FEMPTY_ZS, 0);
end;

function TECPoint.Equals(const other: IECPoint): Boolean;
var
  c1, c2: IECCurve;
  n1, n2, i1, i2: Boolean;
  p1, p2: IECPoint;
  points: TCryptoLibGenericArray<IECPoint>;
begin
  if ((Self as IECPoint) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;

  c1 := Self.curve;
  c2 := other.curve;
  n1 := (c1 = Nil);
  n2 := (c2 = Nil);
  i1 := IsInfinity;
  i2 := other.IsInfinity;

  if (i1 or i2) then
  begin
    result := (i1 and i2) and (n1 or n2 or c1.Equals(c2));
    Exit;
  end;

  p1 := Self as IECPoint;
  p2 := other;
  if (n1 and n2) then
  begin
    // Points with null curve are in affine form, so already normalized
  end
  else if (n1) then
  begin
    p2 := p2.Normalize();
  end
  else if (n2) then
  begin
    p1 := p1.Normalize();
  end
  else if (not c1.Equals(c2)) then
  begin
    result := false;
    Exit;
  end
  else
  begin
    // TODO Consider just requiring already normalized, to avoid silent performance degradation

    points := TCryptoLibGenericArray<IECPoint>.Create(Self, c1.ImportPoint(p2));

    // TODO This is a little strong, really only requires coZNormalizeAll to get Zs equal
    c1.NormalizeAll(points);

    p1 := points[0];
    p2 := points[1];
  end;

  result := p1.XCoord.Equals(p2.XCoord) and p1.YCoord.Equals(p2.YCoord);
end;

function TECPoint.GetEncoded: TCryptoLibByteArray;
begin
  result := GetEncoded(Fm_withCompression);
end;

function TECPoint.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}

var
  c: IECCurve;
  P: IECPoint;
  hc: Int32;
begin
  c := curve;
  if c = Nil then
  begin
    hc := 0;
  end
  else
  begin
    hc := not c.GetHashCode();
  end;

  if (not IsInfinity) then
  begin
    // TODO Consider just requiring already normalized, to avoid silent performance degradation

    P := Normalize();

    hc := hc xor (P.XCoord.GetHashCode() * 17);
    hc := hc xor (P.YCoord.GetHashCode() * 257);
  end;

  result := hc;
end;

class function TECPoint.GetInitialZCoords(const curve: IECCurve)
  : TCryptoLibGenericArray<IECFieldElement>;
var
  coord: Int32;
  One: IECFieldElement;
begin
  // Cope with null curve, most commonly used by implicitlyCa
  if curve = Nil then
  begin
    coord := TECCurveConstants.COORD_AFFINE;
  end
  else
  begin
    coord := curve.CoordinateSystem;
  end;

  case coord of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        result := FEMPTY_ZS;
        Exit;
      end;
  end;

  One := curve.FromBigInteger(TBigInteger.One);

  case coord of

    TECCurveConstants.COORD_HOMOGENEOUS, TECCurveConstants.COORD_JACOBIAN,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        result := TCryptoLibGenericArray<IECFieldElement>.Create(One);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY:
      begin
        result := TCryptoLibGenericArray<IECFieldElement>.Create(One, One, One);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := TCryptoLibGenericArray<IECFieldElement>.Create(One, curve.a);
        Exit;
      end

  else
    begin
      raise EArgumentCryptoLibException.CreateRes(@SUnknownCoordSystem);
    end;

  end;

end;

function TECPoint.GetpreCompTable: TDictionary<String, IPreCompInfo>;
begin
  result := Fm_preCompTable;
end;

function TECPoint.GetXCoord: IECFieldElement;
begin
  result := Fm_x;
end;

function TECPoint.GetYCoord: IECFieldElement;
begin
  result := Fm_y;
end;

function TECPoint.GetZCoord(index: Int32): IECFieldElement;
begin
  if ((index < 0) or (index >= System.Length(Fm_zs))) then
  begin
    result := Nil;
  end
  else
  begin
    result := Fm_zs[index];
  end;
end;

function TECPoint.GetZCoords: TCryptoLibGenericArray<IECFieldElement>;
var
  zsLen: Int32;
begin
  zsLen := System.Length(Fm_zs);
  if (zsLen = 0) then
  begin
    result := Fm_zs;
    Exit;
  end;
  System.SetLength(result, zsLen);
  result := System.Copy(Fm_zs, 0, zsLen);
end;

function TECPoint.ImplIsValid(decompressed, checkOrder: Boolean): Boolean;
var
  Validity: IValidityPrecompInfo;
  callback: IValidityCallback;
begin

  if (IsInfinity) then
  begin
    result := true;
    Exit;
  end;

  callback := TValidityCallback.Create(Self as IECPoint, decompressed,
    checkOrder);
  Validity := curve.Precompute(Self as IECPoint,
    TValidityPrecompInfo.PRECOMP_NAME, callback) as IValidityPrecompInfo;

  result := not(Validity.hasFailed());
end;

function TECPoint.IsNormalized: Boolean;
var
  coord: Int32;
begin
  coord := CurveCoordinateSystem;

  result := (coord = TECCurveConstants.COORD_AFFINE) or
    (coord = TECCurveConstants.COORD_LAMBDA_AFFINE) or (IsInfinity) or
    (RawZCoords[0].IsOne);
end;

function TECPoint.IsValid: Boolean;
begin
  result := ImplIsValid(false, true);
end;

function TECPoint.IsValidPartial: Boolean;
begin
  result := ImplIsValid(false, false);
end;

function TECPoint.Normalize(const zInv: IECFieldElement): IECPoint;
var
  zInv2, zInv3: IECFieldElement;
begin
  case CurveCoordinateSystem of
    TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        result := CreateScaledPoint(zInv, zInv);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN,
      TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
      TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        zInv2 := zInv.Square();
        zInv3 := zInv2.Multiply(zInv);
        result := CreateScaledPoint(zInv2, zInv3);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SNotProjectiveCoordSystem);
    end;

  end;
end;

function TECPoint.GetAffineXCoord: IECFieldElement;
begin
  CheckNormalized();
  result := XCoord;
end;

function TECPoint.GetAffineYCoord: IECFieldElement;
begin
  CheckNormalized();
  result := YCoord;
end;

function TECPoint.GetCurve: IECCurve;
begin
  result := Fm_curve;
end;

function TECPoint.GetCurveCoordinateSystem: Int32;
begin
  // Cope with null curve, most commonly used by implicitlyCa
  if Fm_curve = Nil then
  begin
    result := TECCurveConstants.COORD_AFFINE;
  end
  else
  begin
    result := Fm_curve.CoordinateSystem;
  end;
end;

function TECPoint.GetDetachedPoint: IECPoint;
begin
  result := Normalize().Detach();
end;

{ TF2mPoint }

constructor TF2mPoint.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

constructor TF2mPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNilFieldElement);
  end;

  if (x <> Nil) then
  begin
    // Check if x and y are elements of the same field
    TF2mFieldElement.CheckFieldElements(x, y);

    // Check if x and a are elements of the same field
    if (curve <> Nil) then
    begin
      TF2mFieldElement.CheckFieldElements(x, curve.a);
    end;
  end;
end;

function TF2mPoint.Add(const b: IECPoint): IECPoint;
var
  ecCurve: IECCurve;
  coord: Int32;
  x1, x2, Y1, Y2, dx, dy, L, x3, Y3, Z1, Z2, U1, V1, U2, V2, u, v, Vsq, Vcu, w,
    a, VSqZ2, uv, Z3, L3, L1, L2, S2, S1, ABZ2, AU1, AU2, bigB: IECFieldElement;
  Z1IsOne, Z2IsOne: Boolean;
  P: IECPoint;
begin
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  x1 := RawXCoord;
  x2 := b.RawXCoord;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        Y1 := RawYCoord;
        Y2 := b.RawYCoord;

        dx := x1.Add(x2);
        dy := Y1.Add(Y2);
        if (dx.IsZero) then
        begin
          if (dy.IsZero) then
          begin
            result := Twice();
            Exit;
          end;

          result := ecCurve.Infinity;
          Exit;
        end;

        L := dy.Divide(dx);

        x3 := L.Square().Add(L).Add(dx).Add(ecCurve.a);
        Y3 := L.Multiply(x1.Add(x3)).Add(x3).Add(Y1);

        result := TF2mPoint.Create(ecCurve, x3, Y3, IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        Y1 := RawYCoord;
        Z1 := RawZCoords[0];
        Y2 := b.RawYCoord;
        Z2 := b.RawZCoords[0];

        Z1IsOne := Z1.IsOne;
        U1 := Y2;
        V1 := x2;
        if (not Z1IsOne) then
        begin
          U1 := U1.Multiply(Z1);
          V1 := V1.Multiply(Z1);
        end;

        Z2IsOne := Z2.IsOne;
        U2 := Y1;
        V2 := x1;
        if (not Z2IsOne) then
        begin
          U2 := U2.Multiply(Z2);
          V2 := V2.Multiply(Z2);
        end;

        u := U1.Add(U2);
        v := V1.Add(V2);

        if (v.IsZero) then
        begin
          if (u.IsZero) then
          begin
            result := Twice();
            Exit;
          end;

          result := ecCurve.Infinity;
          Exit;
        end;

        Vsq := v.Square();
        Vcu := Vsq.Multiply(v);

        if Z1IsOne then
        begin
          w := Z2;
        end
        else if Z2IsOne then
        begin
          w := Z1;
        end
        else
        begin
          w := Z1.Multiply(Z2);
        end;

        uv := u.Add(v);
        a := uv.MultiplyPlusProduct(u, Vsq, ecCurve.a).Multiply(w).Add(Vcu);

        x3 := v.Multiply(a);
        if Z2IsOne then
        begin
          VSqZ2 := Vsq;
        end
        else
        begin
          VSqZ2 := Vsq.Multiply(Z2);
        end;

        Y3 := u.MultiplyPlusProduct(x1, v, Y1).MultiplyPlusProduct
          (VSqZ2, uv, a);
        Z3 := Vcu.Multiply(w);

        result := TF2mPoint.Create(ecCurve, x3, Y3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        if (x1.IsZero) then
        begin
          if (x2.IsZero) then
          begin
            result := ecCurve.Infinity;
            Exit;
          end;

          result := b.Add(Self);
          Exit;
        end;

        L1 := RawYCoord;
        Z1 := RawZCoords[0];
        L2 := b.RawYCoord;
        Z2 := b.RawZCoords[0];

        Z1IsOne := Z1.IsOne;
        U2 := x2;
        S2 := L2;
        if (not Z1IsOne) then
        begin
          U2 := U2.Multiply(Z1);
          S2 := S2.Multiply(Z1);
        end;

        Z2IsOne := Z2.IsOne;
        U1 := x1;
        S1 := L1;
        if (not Z2IsOne) then
        begin
          U1 := U1.Multiply(Z2);
          S1 := S1.Multiply(Z2);
        end;

        a := S1.Add(S2);
        bigB := U1.Add(U2);

        if (bigB.IsZero) then
        begin
          if (a.IsZero) then
          begin
            result := Twice();
            Exit;
          end;

          result := ecCurve.Infinity;
          Exit;
        end;

        if (x2.IsZero) then
        begin
          // TODO This can probably be optimized quite a bit
          P := Normalize();
          x1 := P.RawXCoord;
          Y1 := P.YCoord;

          Y2 := L2;
          L := Y1.Add(Y2).Divide(x1);

          x3 := L.Square().Add(L).Add(x1).Add(ecCurve.a);
          if (x3.IsZero) then
          begin
            result := TF2mPoint.Create(ecCurve, x3, ecCurve.b.Sqrt(),
              IsCompressed);
            Exit;
          end;

          Y3 := L.Multiply(x1.Add(x3)).Add(x3).Add(Y1);
          L3 := Y3.Divide(x3).Add(x3);
          Z3 := ecCurve.FromBigInteger(TBigInteger.One);
        end
        else
        begin
          bigB := bigB.Square();

          AU1 := a.Multiply(U1);
          AU2 := a.Multiply(U2);

          x3 := AU1.Multiply(AU2);
          if (x3.IsZero) then
          begin
            result := TF2mPoint.Create(ecCurve, x3, ecCurve.b.Sqrt(),
              IsCompressed);
            Exit;
          end;

          ABZ2 := a.Multiply(bigB);
          if (not Z2IsOne) then
          begin
            ABZ2 := ABZ2.Multiply(Z2);
          end;

          L3 := AU2.Add(bigB).SquarePlusProduct(ABZ2, L1.Add(Z1));

          Z3 := ABZ2;
          if (not Z1IsOne) then
          begin
            Z3 := Z3.Multiply(Z1);
          end;
        end;

        result := TF2mPoint.Create(ecCurve, x3, L3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;

end;

constructor TF2mPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

destructor TF2mPoint.Destroy;
begin
  inherited Destroy;
end;

function TF2mPoint.Detach: IECPoint;
begin
  result := TF2mPoint.Create(Nil, AffineXCoord, AffineYCoord, false);
end;

function TF2mPoint.GetCompressionYTilde: Boolean;
var
  Lx, LY: IECFieldElement;
begin
  Lx := RawXCoord;
  if (Lx.IsZero) then
  begin
    result := false;
    Exit;
  end;

  LY := RawYCoord;

  case CurveCoordinateSystem of
    TECCurveConstants.COORD_LAMBDA_AFFINE,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        // Y is actually Lambda (X + Y/X) here
        result := LY.TestBitZero() <> Lx.TestBitZero();
        Exit;
      end
  else
    begin
      result := LY.Divide(Lx).TestBitZero();
    end;
  end;

end;

function TF2mPoint.GetYCoord: IECFieldElement;
var
  coord: Int32;
  Lx, L, LY, z: IECFieldElement;
begin
  coord := CurveCoordinateSystem;

  case coord of
    TECCurveConstants.COORD_LAMBDA_AFFINE,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        Lx := RawXCoord;
        L := RawYCoord;

        if (IsInfinity or Lx.IsZero) then
        begin
          result := L;
          Exit;
        end;

        // Y is actually Lambda (X + Y/X) here; convert to affine value on the fly
        LY := L.Add(Lx).Multiply(Lx);
        if (TECCurveConstants.COORD_LAMBDA_PROJECTIVE = coord) then
        begin
          z := RawZCoords[0];
          if (not z.IsOne) then
          begin
            LY := LY.Divide(z);
          end;
        end;
        result := LY;
        Exit;
      end
  else
    begin
      result := RawYCoord;
    end;
  end;

end;

function TF2mPoint.Negate: IECPoint;
var
  Lx, LY, bigY, z, L: IECFieldElement;
  ecCurve: IECCurve;
  coord: Int32;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  Lx := RawXCoord;
  if (Lx.IsZero) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        bigY := RawYCoord;
        result := TF2mPoint.Create(ecCurve, Lx, bigY.Add(Lx), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        LY := RawYCoord;
        z := RawZCoords[0];
        result := TF2mPoint.Create(ecCurve, Lx, LY.Add(Lx),
          TCryptoLibGenericArray<IECFieldElement>.Create(z), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        L := RawYCoord;
        result := TF2mPoint.Create(ecCurve, Lx, L.AddOne(), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        // L is actually Lambda (X + Y/X) here
        L := RawYCoord;
        z := RawZCoords[0];
        result := TF2mPoint.Create(ecCurve, Lx, L.Add(z),
          TCryptoLibGenericArray<IECFieldElement>.Create(z), IsCompressed);
        Exit;
      end

  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;

end;

function TF2mPoint.Twice: IECPoint;
var
  ecCurve: IECCurve;
  x1, Y1, L1, x3, Y3, Z1, X1Z1, X1Sq, Y1Z1, s, v, vSquared, sv, h, Z3, L1Z1,
    Z1Sq, a, aZ1Sq, L3, t, b, t1, t2: IECFieldElement;
  coord: Int32;
  Z1IsOne: Boolean;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;

  x1 := RawXCoord;
  if (x1.IsZero) then
  begin
    // A point with X == 0 is it's own additive inverse
    result := ecCurve.Infinity;
    Exit;
  end;

  coord := ecCurve.CoordinateSystem;
  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        Y1 := RawYCoord;

        L1 := Y1.Divide(x1).Add(x1);

        x3 := L1.Square().Add(L1).Add(ecCurve.a);
        Y3 := x1.SquarePlusProduct(x3, L1.AddOne());

        result := TF2mPoint.Create(ecCurve, x3, Y3, IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        Y1 := RawYCoord;
        Z1 := RawZCoords[0];

        Z1IsOne := Z1.IsOne;

        if Z1IsOne then
        begin
          X1Z1 := x1;
        end
        else
        begin
          X1Z1 := x1.Multiply(Z1);
        end;

        if Z1IsOne then
        begin
          Y1Z1 := Y1;
        end
        else
        begin
          Y1Z1 := Y1.Multiply(Z1);
        end;

        X1Sq := x1.Square();
        s := X1Sq.Add(Y1Z1);
        v := X1Z1;
        vSquared := v.Square();
        sv := s.Add(v);
        h := sv.MultiplyPlusProduct(s, vSquared, ecCurve.a);

        x3 := v.Multiply(h);
        Y3 := X1Sq.Square().MultiplyPlusProduct(v, h, sv);
        Z3 := v.Multiply(vSquared);

        result := TF2mPoint.Create(ecCurve, x3, Y3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        L1 := RawYCoord;
        Z1 := RawZCoords[0];

        Z1IsOne := Z1.IsOne;
        if Z1IsOne then
        begin
          L1Z1 := L1;
        end
        else
        begin
          L1Z1 := L1.Multiply(Z1);
        end;

        if Z1IsOne then
        begin
          Z1Sq := Z1;
        end
        else
        begin
          Z1Sq := Z1.Square();
        end;

        a := ecCurve.a;

        if Z1IsOne then
        begin
          aZ1Sq := a;
        end
        else
        begin
          aZ1Sq := a.Multiply(Z1Sq);
        end;

        t := L1.Square().Add(L1Z1).Add(aZ1Sq);
        if (t.IsZero) then
        begin
          result := TF2mPoint.Create(ecCurve, t, ecCurve.b.Sqrt(),
            IsCompressed);
          Exit;
        end;

        x3 := t.Square();

        if Z1IsOne then
        begin
          Z3 := t;
        end
        else
        begin
          Z3 := t.Multiply(Z1Sq);
        end;

        b := ecCurve.b;

        if (b.BitLength < (TBits.Asr32(ecCurve.FieldSize, 1))) then
        begin
          t1 := L1.Add(x1).Square();

          if (b.IsOne) then
          begin
            t2 := aZ1Sq.Add(Z1Sq).Square();
          end
          else
          begin
            // TODO Can be calculated with one square if we pre-compute sqrt(b)
            t2 := aZ1Sq.SquarePlusProduct(b, Z1Sq.Square());
          end;
          L3 := t1.Add(t).Add(Z1Sq).Multiply(t1).Add(t2).Add(x3);
          if (a.IsZero) then
          begin
            L3 := L3.Add(Z3);
          end
          else if (not a.IsOne) then
          begin
            L3 := L3.Add(a.AddOne().Multiply(Z3));
          end
        end
        else
        begin

          if Z1IsOne then
          begin
            X1Z1 := x1;
          end
          else
          begin
            X1Z1 := x1.Multiply(Z1);
          end;
          L3 := X1Z1.SquarePlusProduct(t, L1Z1).Add(x3).Add(Z3);
        end;

        result := TF2mPoint.Create(ecCurve, x3, L3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;
  end;
end;

function TF2mPoint.TwicePlus(const b: IECPoint): IECPoint;
var
  ecCurve: IECCurve;
  x1, x2, Z2, L1, L2, Z1, X1Sq, L1Sq, Z1Sq, L1Z1, t, L2plus1, a, X2Z1Sq, bigB,
    x3, L3, Z3: IECFieldElement;
  coord: Int32;
begin
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Twice();
    Exit;
  end;

  ecCurve := curve;

  x1 := RawXCoord;
  if (x1.IsZero) then
  begin
    // A point with X == 0 is it's own additive inverse
    result := b;
    Exit;
  end;

  coord := ecCurve.CoordinateSystem;

  case coord of
    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin

        // NOTE: twicePlus() only optimized for lambda-affine argument
        x2 := b.RawXCoord;
        Z2 := b.RawZCoords[0];
        if ((x2.IsZero) or (not Z2.IsOne)) then
        begin
          result := Twice().Add(b);
          Exit;
        end;

        L1 := RawYCoord;
        Z1 := RawZCoords[0];
        L2 := b.RawYCoord;

        X1Sq := x1.Square();
        L1Sq := L1.Square();
        Z1Sq := Z1.Square();
        L1Z1 := L1.Multiply(Z1);

        t := ecCurve.a.Multiply(Z1Sq).Add(L1Sq).Add(L1Z1);
        L2plus1 := L2.AddOne();
        a := ecCurve.a.Add(L2plus1).Multiply(Z1Sq).Add(L1Sq)
          .MultiplyPlusProduct(t, X1Sq, Z1Sq);
        X2Z1Sq := x2.Multiply(Z1Sq);
        bigB := X2Z1Sq.Add(t).Square();

        if (bigB.IsZero) then
        begin
          if (a.IsZero) then
          begin
            result := b.Twice();
            Exit;
          end;

          result := ecCurve.Infinity;
          Exit;
        end;

        if (a.IsZero) then
        begin
          result := TF2mPoint.Create(ecCurve, a, ecCurve.b.Sqrt(),
            IsCompressed);
          Exit;
        end;

        x3 := a.Square().Multiply(X2Z1Sq);
        Z3 := a.Multiply(bigB).Multiply(Z1Sq);
        L3 := a.Add(bigB).Square().MultiplyPlusProduct(t, L2plus1, Z3);

        result := TF2mPoint.Create(ecCurve, x3, L3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end
  else
    begin
      result := Twice().Add(b);
      Exit;
    end;
  end;

end;

{ TECPointBase }

constructor TECPointBase.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
end;

constructor TECPointBase.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

destructor TECPointBase.Destroy;
begin
  inherited Destroy;
end;

function TECPointBase.GetEncoded(compressed: Boolean): TCryptoLibByteArray;
var
  normed: IECPoint;
  Lx, LY, PO: TCryptoLibByteArray;
begin
  if (IsInfinity) then
  begin
    System.SetLength(result, 1);
    Exit;
  end;

  normed := Normalize();

  Lx := normed.XCoord.GetEncoded();

  if (compressed) then
  begin
    System.SetLength(PO, System.Length(Lx) + 1);
    if normed.CompressionYTilde then
    begin
      PO[0] := Byte($03);
    end
    else
    begin
      PO[0] := Byte($02);
    end;

    System.Move(Lx[0], PO[1], System.Length(Lx) * System.SizeOf(Byte));

    result := PO;
    Exit;
  end;

  LY := normed.YCoord.GetEncoded();

  System.SetLength(PO, System.Length(Lx) + System.Length(LY) + 1);

  PO[0] := $04;

  System.Move(Lx[0], PO[1], System.Length(Lx) * System.SizeOf(Byte));
  System.Move(LY[0], PO[System.Length(Lx) + 1],
    System.Length(LY) * System.SizeOf(Byte));

  result := PO;

end;

function TECPointBase.Multiply(K: TBigInteger): IECPoint;
begin
  result := curve.GetMultiplier().Multiply(Self as IECPoint, K);
end;

{ TAbstractFpPoint }

function TAbstractFpPoint.GetCompressionYTilde: Boolean;
begin
  result := AffineYCoord.TestBitZero();
end;

constructor TAbstractFpPoint.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
end;

constructor TAbstractFpPoint.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

destructor TAbstractFpPoint.Destroy;
begin
  inherited Destroy;
end;

function TAbstractFpPoint.SatisfiesCurveEquation: Boolean;
var
  Lx, LY, a, b, lhs, rhs, z, Z2, Z3, Z4, Z6: IECFieldElement;
begin
  Lx := RawXCoord;
  LY := RawYCoord;
  a := curve.a;
  b := curve.b;
  lhs := LY.Square();

  case CurveCoordinateSystem of
    TECCurveConstants.COORD_AFFINE:
      begin
        // do nothing
      end;

    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        z := RawZCoords[0];
        if (not z.IsOne) then
        begin
          Z2 := z.Square();
          Z3 := z.Multiply(Z2);
          lhs := lhs.Multiply(z);
          a := a.Multiply(Z2);
          b := b.Multiply(Z3);
        end;
      end;

    TECCurveConstants.COORD_JACOBIAN,
      TECCurveConstants.COORD_JACOBIAN_CHUDNOVSKY,
      TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        z := RawZCoords[0];
        if (not z.IsOne) then
        begin
          Z2 := z.Square();
          Z4 := Z2.Square();
          Z6 := Z2.Multiply(Z4);
          a := a.Multiply(Z4);
          b := b.Multiply(Z6);
        end;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;

  rhs := Lx.Square().Add(a).Multiply(Lx).Add(b);
  result := lhs.Equals(rhs);
end;

function TAbstractFpPoint.Subtract(const b: IECPoint): IECPoint;
begin
  if (b.IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  // Add -b
  result := Add(b.Negate());
end;

{ TFpPoint }

function TFpPoint.Add(const b: IECPoint): IECPoint;
var
  ecCurve: IECCurve;
  coord: Int32;
  gamma, x1, x2, Y1, Y2, dx, dy, x3, Y3, Z1, Z2, U1, V1, U2, V2, u, v, w, a, Z3,
    S2, S1, vSquared, vCubed, vSquaredV2, Z1Squared, bigU2, Z3Squared, c, W1,
    w2, A1, Z1Cubed, Z2Squared, bigU1, h, r, HSquared, G, W3,
    Z2Cubed: IECFieldElement;
  zs: TCryptoLibGenericArray<IECFieldElement>;
  Z1IsOne, Z2IsOne: Boolean;
begin
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  if (Self as IECPoint = b) then
  begin
    result := Twice();
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  x1 := RawXCoord;
  Y1 := RawYCoord;
  x2 := b.RawXCoord;
  Y2 := b.RawYCoord;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin

        dx := x2.Subtract(x1);
        dy := Y2.Subtract(Y1);

        if (dx.IsZero) then
        begin
          if (dy.IsZero) then
          begin
            // this == b, i.e. this must be doubled
            result := Twice();
            Exit;
          end;

          // this == -b, i.e. the result is the point at infinity
          result := curve.Infinity;
          Exit;
        end;

        gamma := dy.Divide(dx);
        x3 := gamma.Square().Subtract(x1).Subtract(x2);
        Y3 := gamma.Multiply(x1.Subtract(x3)).Subtract(Y1);

        result := TFpPoint.Create(curve, x3, Y3, IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        Z1 := RawZCoords[0];
        Z2 := b.RawZCoords[0];

        Z1IsOne := Z1.IsOne;
        Z2IsOne := Z2.IsOne;

        if Z1IsOne then
        begin
          U1 := Y2;
        end
        else
        begin
          U1 := Y2.Multiply(Z1);
        end;

        if Z2IsOne then
        begin
          U2 := Y1;
        end
        else
        begin
          U2 := Y1.Multiply(Z2);
        end;

        u := U1.Subtract(U2);

        if Z1IsOne then
        begin
          V1 := x2;
        end
        else
        begin
          V1 := x2.Multiply(Z1);
        end;

        if Z2IsOne then
        begin
          V2 := x1;
        end
        else
        begin
          V2 := x1.Multiply(Z2);
        end;

        v := V1.Subtract(V2);

        // Check if b = this or b = -this
        if (v.IsZero) then
        begin
          if (u.IsZero) then
          begin
            // this = b, i.e. this must be doubled
            result := Twice();
            Exit;
          end;

          // this = -b, i.e. the result is the point at infinity
          result := ecCurve.Infinity;
          Exit;
        end;

        // TODO Optimize for when w = 1
        if Z1IsOne then
        begin
          w := Z2;
        end
        else if Z2IsOne then

        begin
          w := Z1;
        end
        else
        begin
          w := Z1.Multiply(Z2);
        end;

        vSquared := v.Square();
        vCubed := vSquared.Multiply(v);
        vSquaredV2 := vSquared.Multiply(V2);
        a := u.Square().Multiply(w).Subtract(vCubed).Subtract(Two(vSquaredV2));

        x3 := v.Multiply(a);
        Y3 := vSquaredV2.Subtract(a).MultiplyMinusProduct(u, U2, vCubed);
        Z3 := vCubed.Multiply(w);

        result := TFpPoint.Create(ecCurve, x3, Y3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN, TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        Z1 := RawZCoords[0];
        Z2 := b.RawZCoords[0];

        Z1IsOne := Z1.IsOne;

        x3 := Nil;
        Y3 := Nil;
        Z3 := Nil;
        Z3Squared := Nil;

        if ((not Z1IsOne) and (Z1.Equals(Z2))) then
        begin
          // TODO Make this available as public method coZAdd?

          dx := x1.Subtract(x2);
          dy := Y1.Subtract(Y2);
          if (dx.IsZero) then
          begin
            if (dy.IsZero) then
            begin
              result := Twice();
              Exit;
            end;
            result := ecCurve.Infinity;
            Exit;
          end;

          c := dx.Square();
          W1 := x1.Multiply(c);
          w2 := x2.Multiply(c);
          A1 := W1.Subtract(w2).Multiply(Y1);

          x3 := dy.Square().Subtract(W1).Subtract(w2);
          Y3 := W1.Subtract(x3).Multiply(dy).Subtract(A1);
          Z3 := dx;

          if (Z1IsOne) then
          begin
            Z3Squared := c;
          end
          else
          begin
            Z3 := Z3.Multiply(Z1);
          end
        end
        else
        begin

          if (Z1IsOne) then
          begin
            Z1Squared := Z1;
            bigU2 := x2;
            S2 := Y2;
          end
          else
          begin
            Z1Squared := Z1.Square();
            bigU2 := Z1Squared.Multiply(x2);
            Z1Cubed := Z1Squared.Multiply(Z1);
            S2 := Z1Cubed.Multiply(Y2);
          end;

          Z2IsOne := Z2.IsOne;

          if (Z2IsOne) then
          begin
            Z2Squared := Z2;
            bigU1 := x1;
            S1 := Y1;
          end
          else
          begin
            Z2Squared := Z2.Square();
            bigU1 := Z2Squared.Multiply(x1);
            Z2Cubed := Z2Squared.Multiply(Z2);
            S1 := Z2Cubed.Multiply(Y1);
          end;

          h := bigU1.Subtract(bigU2);
          r := S1.Subtract(S2);

          // Check if b == this or b == -this
          if (h.IsZero) then
          begin
            if (r.IsZero) then
            begin
              // this == b, i.e. this must be doubled
              result := Twice();
              Exit;
            end;

            // this == -b, i.e. the result is the point at infinity
            result := ecCurve.Infinity;
            Exit;
          end;

          HSquared := h.Square();
          G := HSquared.Multiply(h);
          v := HSquared.Multiply(bigU1);

          x3 := r.Square().Add(G).Subtract(Two(v));
          Y3 := v.Subtract(x3).MultiplyMinusProduct(r, G, S1);

          Z3 := h;
          if (not Z1IsOne) then
          begin
            Z3 := Z3.Multiply(Z1);
          end;
          if (not Z2IsOne) then
          begin
            Z3 := Z3.Multiply(Z2);
          end;

          // Alternative calculation of Z3 using fast square
          // X3 := four(X3);
          // Y3 := eight(Y3);
          // Z3 := doubleProductFromSquares(Z1, Z2, Z1Squared, Z2Squared).Multiply(H);

          if (Z3 = h) then
          begin
            Z3Squared := HSquared;
          end;
        end;

        if (coord = TECCurveConstants.COORD_JACOBIAN_MODIFIED) then
        begin
          // TODO If the result will only be used in a subsequent addition, we don't need W3
          W3 := CalculateJacobianModifiedW(Z3, Z3Squared);

          zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3, W3);
        end
        else
        begin
          zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3);
        end;

        result := TFpPoint.Create(ecCurve, x3, Y3, zs, IsCompressed);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;

end;

function TFpPoint.CalculateJacobianModifiedW(const z: IECFieldElement;
  const ZSquared: IECFieldElement): IECFieldElement;
var
  a4, w, a4Neg, LZSquared: IECFieldElement;
begin
  a4 := curve.a;
  LZSquared := ZSquared;
  if ((a4.IsZero) or (z.IsOne)) then
  begin
    result := a4;
    Exit;
  end;

  if (LZSquared = Nil) then
  begin
    LZSquared := z.Square();
  end;

  w := LZSquared.Square();
  a4Neg := a4.Negate();
  if (a4Neg.BitLength < a4.BitLength) then
  begin
    w := w.Multiply(a4Neg).Negate();
  end
  else
  begin
    w := w.Multiply(a4);
  end;
  result := w;
end;

constructor TFpPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

constructor TFpPoint.Create(const curve: IECCurve; const x, y: IECFieldElement;
  withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNilFieldElement);
  end;
end;

constructor TFpPoint.Create(const curve: IECCurve; const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

destructor TFpPoint.Destroy;
begin
  inherited Destroy;
end;

function TFpPoint.Detach: IECPoint;
begin
  result := TFpPoint.Create(Nil, AffineXCoord, AffineYCoord, false);
end;

function TFpPoint.DoubleProductFromSquares(const a, b, aSquared,
  bSquared: IECFieldElement): IECFieldElement;
begin
  // /*
  // * NOTE: If squaring in the field is faster than multiplication, then this is a quicker
  // * way to calculate 2.A.B, if A^2 and B^2 are already known.
  // */
  result := a.Add(b).Square().Subtract(aSquared).Subtract(bSquared);
end;

function TFpPoint.Eight(const x: IECFieldElement): IECFieldElement;
begin
  result := Four(Two(x));
end;

function TFpPoint.Four(const x: IECFieldElement): IECFieldElement;
begin
  result := Two(Two(x));
end;

function TFpPoint.GetJacobianModifiedW: IECFieldElement;
var
  ZZ: TCryptoLibGenericArray<IECFieldElement>;
  w: IECFieldElement;
begin
  ZZ := RawZCoords;
  w := ZZ[1];
  if (w = Nil) then
  begin
    // NOTE: Rarely, TwicePlus will result in the need for a lazy W1 calculation here
    w := CalculateJacobianModifiedW(ZZ[0], Nil);
    ZZ[1] := w;
  end;
  result := w;
end;

function TFpPoint.GetZCoord(index: Int32): IECFieldElement;
begin
  if ((index = 1) and (TECCurveConstants.COORD_JACOBIAN_MODIFIED =
    CurveCoordinateSystem)) then
  begin
    result := GetJacobianModifiedW();
    Exit;
  end;

  result := (Inherited GetZCoord(index));
end;

function TFpPoint.Negate: IECPoint;
var
  Lcurve: IECCurve;
  coord: Int32;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  Lcurve := curve;
  coord := Lcurve.CoordinateSystem;

  if (TECCurveConstants.COORD_AFFINE <> coord) then
  begin
    result := TFpPoint.Create(Lcurve, RawXCoord, RawYCoord.Negate(), RawZCoords,
      IsCompressed);
    Exit;
  end;

  result := TFpPoint.Create(Lcurve, RawXCoord, RawYCoord.Negate(),
    IsCompressed);
end;

function TFpPoint.Three(const x: IECFieldElement): IECFieldElement;
begin
  result := Two(x).Add(x);
end;

function TFpPoint.ThreeTimes: IECPoint;
var
  Y1, x1, _2Y1, Lx, z, LY, d, bigD, i, L1, L2, X4, Y4: IECFieldElement;
  ecCurve: IECCurve;
  coord: Int32;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  Y1 := RawYCoord;
  if (Y1.IsZero) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin

        x1 := RawXCoord;

        _2Y1 := Two(Y1);
        Lx := _2Y1.Square();
        z := Three(x1.Square()).Add(curve.a);
        LY := z.Square();

        d := Three(x1).Multiply(Lx).Subtract(LY);
        if (d.IsZero) then
        begin
          result := curve.Infinity;
          Exit;
        end;

        bigD := d.Multiply(_2Y1);
        i := bigD.Invert();
        L1 := d.Multiply(i).Multiply(z);
        L2 := Lx.Square().Multiply(i).Subtract(L1);

        X4 := (L2.Subtract(L1)).Multiply(L1.Add(L2)).Add(x1);
        Y4 := (x1.Subtract(X4)).Multiply(L2).Subtract(Y1);
        result := TFpPoint.Create(curve, X4, Y4, IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := TwiceJacobianModified(false).Add(Self);
        Exit;
      end
  else
    begin
      // NOTE: Be careful about recursions between TwicePlus and ThreeTimes
      result := Twice().Add(Self);
    end;

  end;

end;

function TFpPoint.TimesPow2(e: Int32): IECPoint;
var
  ecCurve: IECCurve;
  Y1, W1, x1, Z1, Z1Sq, X1Squared, m, _2Y1, _2Y1Squared, s, _4T, _8T, zInv,
    zInv2, zInv3: IECFieldElement;
  coord, i: Int32;
begin
  if (e < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SCannotBeNegative);
  end;
  if ((e = 0) or (IsInfinity)) then
  begin
    result := Self;
    Exit;
  end;
  if (e = 1) then
  begin
    result := Twice();
    Exit;
  end;

  ecCurve := curve;

  Y1 := RawYCoord;
  if (Y1.IsZero) then
  begin
    result := ecCurve.Infinity;
    Exit;
  end;

  coord := ecCurve.CoordinateSystem;

  W1 := ecCurve.a;
  x1 := RawXCoord;
  if RawZCoords = Nil then
  begin
    Z1 := ecCurve.FromBigInteger(TBigInteger.One);
  end
  else
  begin
    Z1 := RawZCoords[0];
  end;

  if (not Z1.IsOne) then
  begin
    case coord of
      TECCurveConstants.COORD_HOMOGENEOUS:
        begin
          Z1Sq := Z1.Square();
          x1 := x1.Multiply(Z1);
          Y1 := Y1.Multiply(Z1Sq);
          W1 := CalculateJacobianModifiedW(Z1, Z1Sq);
        end;
      TECCurveConstants.COORD_JACOBIAN:
        begin
          W1 := CalculateJacobianModifiedW(Z1, Nil);
        end;

      TECCurveConstants.COORD_JACOBIAN_MODIFIED:
        begin
          W1 := GetJacobianModifiedW();
        end;
    end;

  end;

  i := 0;
  while i < e do
  begin
    if (Y1.IsZero) then
    begin
      result := ecCurve.Infinity;
      Exit;
    end;

    X1Squared := x1.Square();
    m := Three(X1Squared);
    _2Y1 := Two(Y1);
    _2Y1Squared := _2Y1.Multiply(Y1);
    s := Two(x1.Multiply(_2Y1Squared));
    _4T := _2Y1Squared.Square();
    _8T := Two(_4T);

    if (not W1.IsZero) then
    begin
      m := m.Add(W1);
      W1 := Two(_8T.Multiply(W1));
    end;

    x1 := m.Square().Subtract(Two(s));
    Y1 := m.Multiply(s.Subtract(x1)).Subtract(_8T);
    if Z1.IsOne then
    begin
      Z1 := _2Y1;
    end
    else
    begin
      Z1 := _2Y1.Multiply(Z1);
    end;

    System.Inc(i);
  end;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        zInv := Z1.Invert();
        zInv2 := zInv.Square();
        zInv3 := zInv2.Multiply(zInv);

        result := TFpPoint.Create(ecCurve, x1.Multiply(zInv2),
          Y1.Multiply(zInv3), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        x1 := x1.Multiply(Z1);
        Z1 := Z1.Multiply(Z1.Square());
        result := TFpPoint.Create(ecCurve, x1, Y1,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z1), IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_JACOBIAN:
      begin
        result := TFpPoint.Create(ecCurve, x1, Y1,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z1), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := TFpPoint.Create(ecCurve, x1, Y1,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z1, W1), IsCompressed);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;

end;

function TFpPoint.Twice: IECPoint;
var
  ecCurve: IECCurve;
  Y1, x1, X1Squared, gamma, x3, Y3, Z1, w, s, t, b, _4B, h, _2s, _2t,
    _4sSquared, Z3, m, Y1Squared, a4, a4Neg, Z1Squared, Z1Pow4: IECFieldElement;
  coord: Int32;
  Z1IsOne: Boolean;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;

  Y1 := RawYCoord;

  if (Y1.IsZero) then
  begin
    result := ecCurve.Infinity;
    Exit;
  end;

  coord := ecCurve.CoordinateSystem;

  x1 := RawXCoord;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        X1Squared := x1.Square();
        gamma := Three(X1Squared).Add(curve.a).Divide(Two(Y1));
        x3 := gamma.Square().Subtract(Two(x1));
        Y3 := gamma.Multiply(x1.Subtract(x3)).Subtract(Y1);

        result := TFpPoint.Create(curve, x3, Y3, IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_HOMOGENEOUS:
      begin
        Z1 := RawZCoords[0];

        Z1IsOne := Z1.IsOne;

        // TODO Optimize for small negative a4 and -3
        w := ecCurve.a;
        if ((not w.IsZero) and (not Z1IsOne)) then
        begin
          w := w.Multiply(Z1.Square());
        end;
        w := w.Add(Three(x1.Square()));

        if Z1IsOne then
        begin
          s := Y1;
        end
        else
        begin
          s := Y1.Multiply(Z1);
        end;

        if Z1IsOne then
        begin
          t := Y1.Square();
        end
        else
        begin
          t := s.Multiply(Y1);
        end;

        b := x1.Multiply(t);
        _4B := Four(b);
        h := w.Square().Subtract(Two(_4B));

        _2s := Two(s);
        x3 := h.Multiply(_2s);
        _2t := Two(t);
        Y3 := _4B.Subtract(h).Multiply(w).Subtract(Two(_2t.Square()));

        if Z1IsOne then
        begin
          _4sSquared := Two(_2t);
        end
        else
        begin
          _4sSquared := _2s.Square();
        end;

        Z3 := Two(_4sSquared).Multiply(s);

        result := TFpPoint.Create(ecCurve, x3, Y3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN:
      begin
        Z1 := RawZCoords[0];

        Z1IsOne := Z1.IsOne;

        Y1Squared := Y1.Square();
        t := Y1Squared.Square();

        a4 := ecCurve.a;
        a4Neg := a4.Negate();

        if (a4Neg.ToBigInteger().Equals(TBigInteger.ValueOf(3))) then
        begin

          if Z1IsOne then
          begin
            Z1Squared := Z1;
          end
          else
          begin
            Z1Squared := Z1.Square();
          end;

          m := Three(x1.Add(Z1Squared).Multiply(x1.Subtract(Z1Squared)));
          s := Four(Y1Squared.Multiply(x1));
        end
        else
        begin
          X1Squared := x1.Square();
          m := Three(X1Squared);
          if (Z1IsOne) then
          begin
            m := m.Add(a4);
          end
          else if (not a4.IsZero) then
          begin

            if Z1IsOne then
            begin
              Z1Squared := Z1;
            end
            else
            begin
              Z1Squared := Z1.Square();
            end;

            Z1Pow4 := Z1Squared.Square();
            if (a4Neg.BitLength < a4.BitLength) then
            begin
              m := m.Subtract(Z1Pow4.Multiply(a4Neg));
            end
            else
            begin
              m := m.Add(Z1Pow4.Multiply(a4));
            end
          end;
          // S := two(doubleProductFromSquares(X1, Y1Squared, X1Squared, T));
          s := Four(x1.Multiply(Y1Squared));
        end;

        x3 := m.Square().Subtract(Two(s));
        Y3 := s.Subtract(x3).Multiply(m).Subtract(Eight(t));

        Z3 := Two(Y1);
        if (not Z1IsOne) then
        begin
          Z3 := Z3.Multiply(Z1);
        end;

        // Alternative calculation of Z3 using fast square
        // Z3 := doubleProductFromSquares(Y1, Z1, Y1Squared, Z1Squared);
        result := TFpPoint.Create(ecCurve, x3, Y3,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := TwiceJacobianModified(true);
        Exit;
      end
  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;
end;

function TFpPoint.TwiceJacobianModified(calculateW: Boolean): IFpPoint;
var
  x1, Y1, Z1, W1, X1Squared, m, _2Y1, _2Y1Squared, s, x3, _4T, _8T, Y3, W3,
    Z3: IECFieldElement;
begin
  x1 := RawXCoord;
  Y1 := RawYCoord;
  Z1 := RawZCoords[0];
  W1 := GetJacobianModifiedW();

  X1Squared := x1.Square();
  m := Three(X1Squared).Add(W1);
  _2Y1 := Two(Y1);
  _2Y1Squared := _2Y1.Multiply(Y1);
  s := Two(x1.Multiply(_2Y1Squared));
  x3 := m.Square().Subtract(Two(s));
  _4T := _2Y1Squared.Square();
  _8T := Two(_4T);
  Y3 := m.Multiply(s.Subtract(x3)).Subtract(_8T);

  if calculateW then
  begin
    W3 := Two(_8T.Multiply(W1));
  end
  else
  begin
    W3 := Nil;
  end;

  if Z1.IsOne then
  begin
    Z3 := _2Y1;
  end
  else
  begin
    Z3 := _2Y1.Multiply(Z1);
  end;

  result := TFpPoint.Create(curve, x3, Y3,
    TCryptoLibGenericArray<IECFieldElement>.Create(Z3, W3), IsCompressed);
end;

function TFpPoint.TwicePlus(const b: IECPoint): IECPoint;
var
  Y1, x1, x2, Y2, dx, dy, Lx, LY, d, i, L1, L2, X4, Y4, bigD: IECFieldElement;
  ecCurve: IECCurve;
  coord: Int32;

begin
  if (Self as IECPoint = b) then
  begin
    result := ThreeTimes();
    Exit;
  end;
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Twice();
    Exit;
  end;

  Y1 := RawYCoord;
  if (Y1.IsZero) then
  begin
    result := b;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  case coord of
    TECCurveConstants.COORD_AFFINE:
      begin
        x1 := RawXCoord;
        x2 := b.RawXCoord;
        Y2 := b.RawYCoord;

        dx := x2.Subtract(x1);
        dy := Y2.Subtract(Y1);

        if (dx.IsZero) then
        begin
          if (dy.IsZero) then
          begin
            // this == b i.e. the result is 3P
            result := ThreeTimes();
            Exit;
          end;

          // this == -b, i.e. the result is P
          result := Self;
          Exit;
        end;

        // / * * Optimized calculation of 2 p + Q, as described
        // in " Trading Inversions for * Multiplications
        // in Elliptic curve Cryptography ", by Ciet, Joye, Lauter,
        // Montgomery. * /

        Lx := dx.Square();
        LY := dy.Square();
        d := Lx.Multiply(Two(x1).Add(x2)).Subtract(LY);
        if (d.IsZero) then
        begin
          result := curve.Infinity;
          Exit;
        end;

        bigD := d.Multiply(dx);
        i := bigD.Invert();
        L1 := d.Multiply(i).Multiply(dy);
        L2 := Two(Y1).Multiply(Lx).Multiply(dx).Multiply(i).Subtract(L1);
        X4 := (L2.Subtract(L1)).Multiply(L1.Add(L2)).Add(x2);
        Y4 := (x1.Subtract(X4)).Multiply(L2).Subtract(Y1);

        result := TFpPoint.Create(curve, X4, Y4, IsCompressed);
        Exit;
      end;
    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      begin
        result := TwiceJacobianModified(false).Add(b);
        Exit;
      end
  else
    begin
      result := Twice().Add(b);
      Exit;
    end;
  end;

end;

function TFpPoint.Two(const x: IECFieldElement): IECFieldElement;
begin
  result := x.Add(x);
end;

{ TAbstractF2mPoint }

constructor TAbstractF2mPoint.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
end;

constructor TAbstractF2mPoint.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

destructor TAbstractF2mPoint.Destroy;
begin
  inherited Destroy;
end;

function TAbstractF2mPoint.SatisfiesCurveEquation: Boolean;
var
  z, Z2, Z3, x, LY, a, b, lhs, rhs, L, x2, Z4: IECFieldElement;
  ecCurve: IECCurve;
  coord: Int32;
  ZIsOne: Boolean;
begin
  ecCurve := curve;
  x := RawXCoord;
  LY := RawYCoord;
  a := ecCurve.a;
  b := ecCurve.b;

  coord := ecCurve.CoordinateSystem;
  if (coord = TECCurveConstants.COORD_LAMBDA_PROJECTIVE) then
  begin
    z := RawZCoords[0];
    ZIsOne := z.IsOne;

    if (x.IsZero) then
    begin
      // NOTE: For x == 0, we expect the affine-y instead of the lambda-y
      lhs := LY.Square();
      rhs := b;
      if (not ZIsOne) then
      begin
        Z2 := z.Square();
        rhs := rhs.Multiply(Z2);
      end
    end
    else
    begin
      L := LY;
      x2 := x.Square();
      if (ZIsOne) then
      begin
        lhs := L.Square().Add(L).Add(a);
        rhs := x2.Square().Add(b);
      end
      else
      begin
        Z2 := z.Square();
        Z4 := Z2.Square();
        lhs := L.Add(z).MultiplyPlusProduct(L, a, Z2);
        // TODO If sqrt(b) is precomputed this can be simplified to a single square
        rhs := x2.SquarePlusProduct(b, Z4);
      end;
      lhs := lhs.Multiply(x2);
    end
  end
  else
  begin
    lhs := LY.Add(x).Multiply(LY);

    case coord of
      TECCurveConstants.COORD_AFFINE:
        begin
          // do nothing;
        end;

      TECCurveConstants.COORD_HOMOGENEOUS:
        begin
          z := RawZCoords[0];
          if (not z.IsOne) then
          begin
            Z2 := z.Square();
            Z3 := z.Multiply(Z2);
            lhs := lhs.Multiply(z);
            a := a.Multiply(z);
            b := b.Multiply(Z3);
          end;
        end

    else
      begin
        raise EInvalidOperationCryptoLibException.CreateRes
          (@SUnSupportedCoordinateSystem);
      end;

    end;

    rhs := x.Add(a).Multiply(x.Square()).Add(b);
  end;

  result := lhs.Equals(rhs);
end;

function TAbstractF2mPoint.SatisfiesOrder: Boolean;
var
  Cofactor: TBigInteger;
  n: IECPoint;
  x, rhs, lambda, w, t: IECFieldElement;
  Lcurve: IECCurve;
begin
  Lcurve := curve;
  Cofactor := Lcurve.GetCofactor();
  if (TBigInteger.Two.Equals(Cofactor)) then
  begin
    // /*
    // *  Check that the trace of (X + A) is 0, then there exists a solution to L^2 + L = X + A,
    // *  and so a halving is possible, so this point is the double of another.
    // */
    n := Normalize();
    x := n.AffineXCoord;
    rhs := x.Add(Lcurve.a);
    result := (rhs as IAbstractF2mFieldElement).Trace() = 0;
    Exit;
  end;
  if (TBigInteger.Four.Equals(Cofactor)) then
  begin
    // /*
    // * Solve L^2 + L = X + A to find the half of this point, if it exists (fail if not).
    // * Generate both possibilities for the square of the half-point's x-coordinate (w),
    // * and check if Tr(w + A) == 0 for at least one; then a second halving is possible
    // * (see comments for cofactor 2 above), so this point is four times another.
    // *
    // * Note: Tr(x^2) == Tr(x).
    // */
    n := Normalize();
    x := n.AffineXCoord;
    lambda := (Lcurve as IAbstractF2mCurve).SolveQuadraticEquation
      (x.Add(curve.a));
    if (lambda = Nil) then
    begin
      result := false;
      Exit;
    end;
    w := x.Multiply(lambda).Add(n.AffineYCoord);
    t := w.Add(Lcurve.a);
    result := ((t as IAbstractF2mFieldElement).Trace() = 0) or
      ((t.Add(x) as IAbstractF2mFieldElement).Trace() = 0);
    Exit;
  end;

  result := Inherited SatisfiesOrder();
end;

function TAbstractF2mPoint.ScaleX(const scale: IECFieldElement): IECPoint;
var
  Lx, L, x2, L2, z, Z2: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  case CurveCoordinateSystem of
    TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        // Y is actually Lambda (X + Y/X) here
        Lx := RawXCoord;
        L := RawYCoord;

        x2 := Lx.Multiply(scale);
        L2 := L.Add(Lx).Divide(scale).Add(x2);

        result := curve.CreateRawPoint(Lx, L2, RawZCoords, IsCompressed);
        Exit;
      end;

    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        // Y is actually Lambda (X + Y/X) here
        Lx := RawXCoord;
        L := RawYCoord;
        z := RawZCoords[0];

        // We scale the Z coordinate also, to avoid an inversion
        x2 := Lx.Multiply(scale.Square());
        L2 := L.Add(Lx).Add(x2);
        Z2 := z.Multiply(scale);

        result := curve.CreateRawPoint(Lx, L2,
          TCryptoLibGenericArray<IECFieldElement>.Create(Z2), IsCompressed);
        Exit;
      end
  else
    begin
      result := (Inherited ScaleX(scale));
    end;

  end;

end;

function TAbstractF2mPoint.ScaleY(const scale: IECFieldElement): IECPoint;
var
  Lx, L, L2: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  case CurveCoordinateSystem of
    TECCurveConstants.COORD_LAMBDA_AFFINE,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        Lx := RawXCoord;
        L := RawYCoord;

        // Y is actually Lambda (X + Y/X) here
        L2 := L.Add(Lx).Multiply(scale).Add(Lx);

        result := curve.CreateRawPoint(Lx, L2, RawZCoords, IsCompressed);
        Exit;
      end
  else
    begin
      result := (Inherited ScaleY(scale));
    end;
  end;

end;

function TAbstractF2mPoint.Subtract(const b: IECPoint): IECPoint;
begin
  if (b.IsInfinity) then
  begin
    result := Self;
  end;

  // Add -b
  result := Add(b.Negate());
end;

function TAbstractF2mPoint.Tau: IAbstractF2mPoint;
var
  ecCurve: IECCurve;
  coord: Int32;
  x1, Y1, Z1: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  x1 := RawXCoord;

  case coord of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        Y1 := RawYCoord;
        result := ecCurve.CreateRawPoint(x1.Square(), Y1.Square(), IsCompressed)
          as IAbstractF2mPoint;
        Exit;
      end;

    TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        Y1 := RawYCoord;
        Z1 := RawZCoords[0];
        result := ecCurve.CreateRawPoint(x1.Square(), Y1.Square(),
          TCryptoLibGenericArray<IECFieldElement>.Create(Z1.Square()),
          IsCompressed) as IAbstractF2mPoint;
        Exit;
      end

  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;
end;

function TAbstractF2mPoint.TauPow(pow: Int32): IAbstractF2mPoint;
var
  ecCurve: IECCurve;
  coord: Int32;
  x1, Y1, Z1: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  ecCurve := curve;
  coord := ecCurve.CoordinateSystem;

  x1 := RawXCoord;

  case coord of
    TECCurveConstants.COORD_AFFINE, TECCurveConstants.COORD_LAMBDA_AFFINE:
      begin
        Y1 := RawYCoord;
        result := ecCurve.CreateRawPoint(x1.SquarePow(pow), Y1.SquarePow(pow),
          IsCompressed) as IAbstractF2mPoint;
        Exit;
      end;

    TECCurveConstants.COORD_HOMOGENEOUS,
      TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      begin
        Y1 := RawYCoord;
        Z1 := RawZCoords[0];
        result := ecCurve.CreateRawPoint(x1.SquarePow(pow), Y1.SquarePow(pow),
          TCryptoLibGenericArray<IECFieldElement>.Create(Z1.SquarePow(pow)),
          IsCompressed) as IAbstractF2mPoint;
        Exit;
      end

  else
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SUnSupportedCoordinateSystem);
    end;

  end;
end;

{ TECPoint.TValidityCallback }

constructor TECPoint.TValidityCallback.Create(const outer: IECPoint;
  decompressed, checkOrder: Boolean);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_decompressed := decompressed;
  Fm_checkOrder := checkOrder;
end;

function TECPoint.TValidityCallback.Precompute(const existing: IPreCompInfo)
  : IPreCompInfo;
var
  info: IValidityPrecompInfo;
begin
  if (not(Supports(existing, IValidityPrecompInfo, info))) then
  begin
    info := TValidityPrecompInfo.Create();
  end;

  if (info.hasFailed()) then
  begin
    result := info;
    Exit;
  end;
  if (not(info.hasCurveEquationPassed())) then
  begin
    if (not(Fm_decompressed) and not(Fm_outer.SatisfiesCurveEquation())) then
    begin
      info.reportFailed();
      result := info;
      Exit;
    end;
    info.reportCurveEquationPassed();
  end;

  if ((Fm_checkOrder) and (not(info.HasOrderPassed()))) then
  begin
    if (not(Fm_outer.SatisfiesOrder())) then
    begin
      info.reportFailed();
      result := info;
      Exit;
    end;
    info.reportOrderPassed();
  end;
  result := info;
end;

end.
