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

unit ClpEd25519;

{$I CryptoLib.inc}

interface

uses
  SyncObjs,
  ClpNat,
  ClpBits,
  ClpNat256,
  ClpInterleave,
  ClpDigestUtilities,
  ClpISecureRandom,
  ClpIDigest,
  ClpIEd25519,
  ClpX25519Field,
  ClpConverters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SInvalidCtx = 'ctx';
  SDigestError = 'Digest Produced Insufficient Data';

type
  TEd25519 = class(TInterfacedObject, IEd25519)

  strict private

  type
    TPointAccum = record
    private
      Fx, Fy, Fz, Fu, Fv: TCryptoLibInt32Array;

      function GetX: TCryptoLibInt32Array; inline;
      procedure SetX(const value: TCryptoLibInt32Array); inline;
      function GetY: TCryptoLibInt32Array; inline;
      procedure SetY(const value: TCryptoLibInt32Array); inline;
      function GetZ: TCryptoLibInt32Array; inline;
      procedure SetZ(const value: TCryptoLibInt32Array); inline;
      function GetU: TCryptoLibInt32Array; inline;
      procedure SetU(const value: TCryptoLibInt32Array); inline;
      function GetV: TCryptoLibInt32Array; inline;
      procedure SetV(const value: TCryptoLibInt32Array); inline;

    public
      property X: TCryptoLibInt32Array read GetX write SetX;
      property Y: TCryptoLibInt32Array read GetY write SetY;
      property Z: TCryptoLibInt32Array read GetZ write SetZ;
      property U: TCryptoLibInt32Array read GetU write SetU;
      property V: TCryptoLibInt32Array read GetV write SetV;

      class function CreatePointAccum(): TPointAccum; static;
    end;

  type
    TPointAffine = record
    private
      Fx, Fy: TCryptoLibInt32Array;

      function GetX: TCryptoLibInt32Array; inline;
      procedure SetX(const value: TCryptoLibInt32Array); inline;
      function GetY: TCryptoLibInt32Array; inline;
      procedure SetY(const value: TCryptoLibInt32Array); inline;

    public
      property X: TCryptoLibInt32Array read GetX write SetX;
      property Y: TCryptoLibInt32Array read GetY write SetY;

      class function CreatePointAffine(): TPointAffine; static;
    end;

  type
    TPointExt = record
    private
      Fx, Fy, Fz, Ft: TCryptoLibInt32Array;

      function GetX: TCryptoLibInt32Array; inline;
      procedure SetX(const value: TCryptoLibInt32Array); inline;
      function GetY: TCryptoLibInt32Array; inline;
      procedure SetY(const value: TCryptoLibInt32Array); inline;
      function GetZ: TCryptoLibInt32Array; inline;
      procedure SetZ(const value: TCryptoLibInt32Array); inline;
      function GetT: TCryptoLibInt32Array; inline;
      procedure SetT(const value: TCryptoLibInt32Array); inline;

    public
      property X: TCryptoLibInt32Array read GetX write SetX;
      property Y: TCryptoLibInt32Array read GetY write SetY;
      property Z: TCryptoLibInt32Array read GetZ write SetZ;
      property T: TCryptoLibInt32Array read GetT write SetT;

      class function CreatePointExt(): TPointExt; static;
    end;

  type
    TPointPrecomp = record
    private
      Fypx_h, Fymx_h, Fxyd: TCryptoLibInt32Array;

      function GetYpx_h: TCryptoLibInt32Array; inline;
      procedure SetYpx_h(const value: TCryptoLibInt32Array); inline;
      function GetYmx_h: TCryptoLibInt32Array; inline;
      procedure SetYmx_h(const value: TCryptoLibInt32Array); inline;
      function GetXyd: TCryptoLibInt32Array; inline;
      procedure SetXyd(const value: TCryptoLibInt32Array); inline;

    public
      property Ypx_h: TCryptoLibInt32Array read GetYpx_h write SetYpx_h;
      property Ymx_h: TCryptoLibInt32Array read GetYmx_h write SetYmx_h;
      property Xyd: TCryptoLibInt32Array read GetXyd write SetXyd;

      class function CreatePointPrecomp(): TPointPrecomp; static;
    end;

  const
    M28L = Int64($0FFFFFFF);
    M32L = Int64($FFFFFFFF);

    PointBytes = Int32(32);
    ScalarUints = Int32(8);
    ScalarBytes = Int32(ScalarUints * 4);

    L0 = Int32($FCF5D3ED); // L0:26/--
    L1 = Int32($012631A6); // L1:24/22
    L2 = Int32($079CD658); // L2:27/--
    L3 = Int32($FF9DEA2F); // L3:23/--
    L4 = Int32($000014DF); // L4:12/11

    WnafWidthBase = Int32(7);

    PrecompBlocks = Int32(8);
    PrecompTeeth = Int32(4);
    PrecompSpacing = Int32(8);
    PrecompPoints = Int32(1 shl (PrecompTeeth - 1));
    PrecompMask = Int32(PrecompPoints - 1);

  class var

    FDom2Prefix: TCryptoLibByteArray;
    FP, FL: TCryptoLibUInt32Array;
    FB_x, FB_y, FC_d, FC_d2, FC_d4: TCryptoLibInt32Array;
    FPrecompLock: TCriticalSection;
    // TODO[ed25519] Convert to PointPrecomp
    FPrecompBaseTable: TCryptoLibGenericArray<TPointExt>;
    FPrecompBase: TCryptoLibInt32Array;

    class function CalculateS(const r, k, s: TCryptoLibByteArray)
      : TCryptoLibByteArray; static;

    class function CheckContextVar(const ctx: TCryptoLibCustomByteArrayBuffer;
      phflag: Byte): Boolean; static; inline;

    class function CheckPointVar(const p: TCryptoLibByteArray): Boolean;
      static; inline;

    class function CheckScalarVar(const s: TCryptoLibByteArray): Boolean;
      static; inline;

    class function Decode24(const bs: TCryptoLibByteArray; off: Int32): UInt32;
      static; inline;

    class function Decode32(const bs: TCryptoLibByteArray; off: Int32): UInt32;
      overload; static; inline;

    class procedure Decode32(const bs: TCryptoLibByteArray; bsOff: Int32;
      const n: TCryptoLibUInt32Array; nOff, nLen: Int32); overload;
      static; inline;

    class function DecodePointVar(const p: TCryptoLibByteArray; pOff: Int32;
      ANegate: Boolean; var r: TPointAffine): Boolean; static;

    class procedure DecodeScalar(const k: TCryptoLibByteArray; kOff: Int32;
      const n: TCryptoLibUInt32Array); static; inline;

    class procedure Dom2(const d: IDigest; phflag: Byte;
      const ctx: TCryptoLibCustomByteArrayBuffer); static; inline;

    class procedure Encode24(n: UInt32; const bs: TCryptoLibByteArray;
      off: Int32); static; inline;

    class procedure Encode32(n: UInt32; const bs: TCryptoLibByteArray;
      off: Int32); static; inline;

    class procedure Encode56(n: UInt64; const bs: TCryptoLibByteArray;
      off: Int32); static; inline;

    class procedure EncodePoint(var p: TPointAccum;
      const r: TCryptoLibByteArray; rOff: Int32); static;

    class function GetWindow4(const X: TCryptoLibUInt32Array; n: Int32): Int32;
      static; inline;

    class function GetWnaf(const n: TCryptoLibUInt32Array; width: Int32)
      : TCryptoLibShortIntArray; static;

    class procedure ImplSign(const d: IDigest;
      const h, s, pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
      const m: TCryptoLibByteArray; mOff, mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload; static;

    procedure ImplSign(const sk: TCryptoLibByteArray; skOff: Int32;
      const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
      const m: TCryptoLibByteArray; mOff: Int32; mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    procedure ImplSign(const sk: TCryptoLibByteArray; skOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
      const m: TCryptoLibByteArray; mOff: Int32; mLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32); overload;

    function ImplVerify(const sig: TCryptoLibByteArray; sigOff: Int32;
      const pk: TCryptoLibByteArray; pkOff: Int32;
      const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
      const m: TCryptoLibByteArray; mOff, mLen: Int32): Boolean;

    class procedure PointAdd(var p: TPointExt; var r: TPointAccum);
      overload; static;

    class procedure PointAdd(var p, r: TPointExt); overload; static;

    class procedure PointAddVar(negate: Boolean; var p: TPointExt;
      var r: TPointAccum); overload; static;

    class procedure PointAddVar(negate: Boolean; var p, q, r: TPointExt);
      overload; static;

    class procedure PointAddPrecomp(var p: TPointPrecomp; var r: TPointAccum);
      overload; static;

    class function PointCopy(var p: TPointAffine): TPointExt; overload;
      static; inline;

    class function PointCopy(var p: TPointAccum): TPointExt; overload;
      static; inline;

    class function PointCopy(var p: TPointExt): TPointExt; overload;
      static; inline;

    class procedure PointCopy(var p: TPointAffine; var r: TPointAccum);
      overload; static; inline;

    class procedure PointCopy(var p, r: TPointExt); overload; static; inline;

    class procedure PointDouble(var r: TPointAccum); static;

    class procedure PointExtendXY(var p: TPointAccum); overload; static; inline;

    class procedure PointExtendXY(var p: TPointExt); overload; static; inline;

    class procedure PointLookup(block, index: Int32; var p: TPointPrecomp);
      overload; static;

    class procedure PointLookup(const X: TCryptoLibUInt32Array; n: Int32;
      const table: TCryptoLibInt32Array; var r: TPointExt); overload; static;

    class procedure PointLookup(const table: TCryptoLibInt32Array; index: Int32;
      var r: TPointExt); overload; static;

    class function PointPrecomp(var p: TPointAffine; count: Int32)
      : TCryptoLibInt32Array; static;

    class function PointPrecompVar(var p: TPointExt; count: Int32)
      : TCryptoLibGenericArray<TPointExt>; static;

    class procedure PointSetNeutral(var p: TPointAccum); overload;
      static; inline;

    class procedure PointSetNeutral(var p: TPointExt); overload; static; inline;

    class procedure PruneScalar(const n: TCryptoLibByteArray; nOff: Int32;
      const r: TCryptoLibByteArray); static; inline;

    class function ReduceScalar(const n: TCryptoLibByteArray)
      : TCryptoLibByteArray; static;

    class procedure ScalarMultBase(const k: TCryptoLibByteArray;
      var r: TPointAccum); static;

    class procedure ScalarMultBaseEncoded(const k, r: TCryptoLibByteArray;
      rOff: Int32); static; inline;

    class procedure ScalarMultStrausVar(const nb, np: TCryptoLibUInt32Array;
      var p: TPointAffine; var r: TPointAccum); static;

    class function ValidateDigestOutputSize(const ADigest: IDigest)
      : TCryptoLibByteArray; static; inline;

    class function ConstructCustomByteArrayBufferContext
      (const AData: TCryptoLibByteArray; AIsNil: Boolean; ALength: Int32)
      : TCryptoLibCustomByteArrayBuffer; static; inline;

    class procedure Boot(); static;
    class constructor CreateEd25519();
    class destructor DestroyEd25519();

  strict protected

    function CreateDigest(): IDigest; virtual;

  public

    const
    PreHashSize = Int32(64);
    PublicKeySize = Int32(PointBytes);
    SecretKeySize = Int32(32);
    SignatureSize = Int32(PointBytes + ScalarBytes);

  type
{$SCOPEDENUMS ON}
    TEd25519Algorithm = (Ed25519 = 0, Ed25519ctx = 1, Ed25519ph = 2);
{$SCOPEDENUMS OFF}
  constructor Create();

  function CreatePreHash(): IDigest; inline;

  procedure GeneratePrivateKey(const random: ISecureRandom;
    const k: TCryptoLibByteArray); inline;

  procedure GeneratePublicKey(const sk: TCryptoLibByteArray; skOff: Int32;
    pk: TCryptoLibByteArray; pkOff: Int32);

  class procedure Precompute(); static;

  // NOTE: Only for use by X25519
  class procedure ScalarMultBaseYZ(const k: TCryptoLibByteArray; kOff: Int32;
    const Y, Z: TCryptoLibInt32Array); static; inline;

  procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
    const m: TCryptoLibByteArray; mOff, mLen: Int32;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
    mOff, mLen: Int32; const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
    const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure Sign(const sk: TCryptoLibByteArray; skOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32;
    const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
    const ctx, ph: TCryptoLibByteArray; phOff: Int32;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32;
    const ctx, ph: TCryptoLibByteArray; phOff: Int32;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
    const ctx: TCryptoLibByteArray; const ph: IDigest;
    const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  procedure SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32; const ctx: TCryptoLibByteArray;
    const ph: IDigest; const sig: TCryptoLibByteArray; sigOff: Int32); overload;

  function Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
    mOff, mLen: Int32): Boolean; overload;

  function Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32;
    const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32): Boolean; overload;

  function VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32;
    const ctx, ph: TCryptoLibByteArray; phOff: Int32): Boolean; overload;

  function VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
    const pk: TCryptoLibByteArray; pkOff: Int32; const ctx: TCryptoLibByteArray;
    const ph: IDigest): Boolean; overload;

  class procedure ScalarMult(const k: TCryptoLibByteArray; var p: TPointAffine;
    var r: TPointAccum); static;

  end;

implementation

{ TEd25519.TPointExt }

class function TEd25519.TPointExt.CreatePointExt(): TPointExt;
begin
  result := Default (TPointExt);
  result.Fx := TX25519Field.Create();
  result.Fy := TX25519Field.Create();
  result.Fz := TX25519Field.Create();
  result.Ft := TX25519Field.Create();
end;

function TEd25519.TPointExt.GetT: TCryptoLibInt32Array;
begin
  result := Ft;
end;

function TEd25519.TPointExt.GetX: TCryptoLibInt32Array;
begin
  result := Fx;
end;

function TEd25519.TPointExt.GetY: TCryptoLibInt32Array;
begin
  result := Fy;
end;

function TEd25519.TPointExt.GetZ: TCryptoLibInt32Array;
begin
  result := Fz;
end;

procedure TEd25519.TPointExt.SetT(const value: TCryptoLibInt32Array);
begin
  Ft := value;
end;

procedure TEd25519.TPointExt.SetX(const value: TCryptoLibInt32Array);
begin
  Fx := value;
end;

procedure TEd25519.TPointExt.SetY(const value: TCryptoLibInt32Array);
begin
  Fy := value;
end;

procedure TEd25519.TPointExt.SetZ(const value: TCryptoLibInt32Array);
begin
  Fz := value;
end;

{ TEd25519.TPointAccum }

class function TEd25519.TPointAccum.CreatePointAccum(): TPointAccum;
begin
  result := Default (TPointAccum);
  result.Fx := TX25519Field.Create();
  result.Fy := TX25519Field.Create();
  result.Fz := TX25519Field.Create();
  result.Fu := TX25519Field.Create();
  result.Fv := TX25519Field.Create();
end;

function TEd25519.TPointAccum.GetU: TCryptoLibInt32Array;
begin
  result := Fu;
end;

function TEd25519.TPointAccum.GetV: TCryptoLibInt32Array;
begin
  result := Fv;
end;

function TEd25519.TPointAccum.GetX: TCryptoLibInt32Array;
begin
  result := Fx;
end;

function TEd25519.TPointAccum.GetY: TCryptoLibInt32Array;
begin
  result := Fy;
end;

function TEd25519.TPointAccum.GetZ: TCryptoLibInt32Array;
begin
  result := Fz;
end;

procedure TEd25519.TPointAccum.SetU(const value: TCryptoLibInt32Array);
begin
  Fu := value;
end;

procedure TEd25519.TPointAccum.SetV(const value: TCryptoLibInt32Array);
begin
  Fv := value;
end;

procedure TEd25519.TPointAccum.SetX(const value: TCryptoLibInt32Array);
begin
  Fx := value;
end;

procedure TEd25519.TPointAccum.SetY(const value: TCryptoLibInt32Array);
begin
  Fy := value;
end;

procedure TEd25519.TPointAccum.SetZ(const value: TCryptoLibInt32Array);
begin
  Fz := value;
end;

{ TEd25519.TPointPrecomp }

class function TEd25519.TPointPrecomp.CreatePointPrecomp(): TPointPrecomp;
begin
  result := Default (TPointPrecomp);
  result.Fypx_h := TX25519Field.Create();
  result.Fymx_h := TX25519Field.Create();
  result.Fxyd := TX25519Field.Create();
end;

function TEd25519.TPointPrecomp.GetXyd: TCryptoLibInt32Array;
begin
  result := Fxyd;
end;

function TEd25519.TPointPrecomp.GetYmx_h: TCryptoLibInt32Array;
begin
  result := Fymx_h;
end;

function TEd25519.TPointPrecomp.GetYpx_h: TCryptoLibInt32Array;
begin
  result := Fypx_h;
end;

procedure TEd25519.TPointPrecomp.SetXyd(const value: TCryptoLibInt32Array);
begin
  Fxyd := value;
end;

procedure TEd25519.TPointPrecomp.SetYmx_h(const value: TCryptoLibInt32Array);
begin
  Fymx_h := value;
end;

procedure TEd25519.TPointPrecomp.SetYpx_h(const value: TCryptoLibInt32Array);
begin
  Fypx_h := value;
end;

{ TEd25519 }

class procedure TEd25519.Boot;
begin
  // 'SigEd25519 no Ed25519 collisions' as ByteArray using ASCII Encoding
  FDom2Prefix := TCryptoLibByteArray.Create(83, 105, 103, 69, 100, 50, 53, 53,
    49, 57, 32, 110, 111, 32, 69, 100, 50, 53, 53, 49, 57, 32, 99, 111, 108,
    108, 105, 115, 105, 111, 110, 115);

  FP := TCryptoLibUInt32Array.Create($FFFFFFED, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $7FFFFFFF);
  FL := TCryptoLibUInt32Array.Create($5CF5D3ED, $5812631A, $A2F79CD6, $14DEF9DE,
    $00000000, $00000000, $00000000, $10000000);

  FB_x := TCryptoLibInt32Array.Create($0325D51A, $018B5823, $007B2C95,
    $0304A92D, $00D2598E, $01D6DC5C, $01388C7F, $013FEC0A, $029E6B72,
    $0042D26D);
  FB_y := TCryptoLibInt32Array.Create($02666658, $01999999, $00666666,
    $03333333, $00CCCCCC, $02666666, $01999999, $00666666, $03333333,
    $00CCCCCC);
  FC_d := TCryptoLibInt32Array.Create($035978A3, $02D37284, $018AB75E,
    $026A0A0E, $0000E014, $0379E898, $01D01E5D, $01E738CC, $03715B7F,
    $00A406D9);
  FC_d2 := TCryptoLibInt32Array.Create($02B2F159, $01A6E509, $01156EBD,
    $00D4141D, $0001C029, $02F3D130, $03A03CBB, $01CE7198, $02E2B6FF,
    $00480DB3);
  FC_d4 := TCryptoLibInt32Array.Create($0165E2B2, $034DCA13, $002ADD7A,
    $01A8283B, $00038052, $01E7A260, $03407977, $019CE331, $01C56DFF,
    $00901B67);

  if FPrecompLock = Nil then
  begin
    FPrecompLock := TCriticalSection.Create;
  end;
end;

class function TEd25519.Decode32(const bs: TCryptoLibByteArray;
  off: Int32): UInt32;
begin
  // UInt32 n := bs[off];
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 8);
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 16);
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 24);
  // result := n;
  result := TConverters.ReadBytesAsUInt32LE(PByte(bs), off);
end;

class procedure TEd25519.Decode32(const bs: TCryptoLibByteArray; bsOff: Int32;
  const n: TCryptoLibUInt32Array; nOff, nLen: Int32);
begin
  // int32 i;
  // for i := 0 to System.Pred(nLen) do
  // begin
  // n[nOff + i] := Decode32(bs, bsOff + (i * 4));
  // end;
  TConverters.le32_copy(PByte(bs), bsOff * System.SizeOf(Byte), PCardinal(n),
    nOff * System.SizeOf(UInt32), nLen * System.SizeOf(UInt32));
end;

class procedure TEd25519.DecodeScalar(const k: TCryptoLibByteArray; kOff: Int32;
  const n: TCryptoLibUInt32Array);
begin
  Decode32(k, kOff, n, 0, ScalarUints);
end;

class procedure TEd25519.Encode24(n: UInt32; const bs: TCryptoLibByteArray;
  off: Int32);
begin
  bs[off] := Byte(n);
  System.Inc(off);
  bs[off] := Byte(n shr 8);
  System.Inc(off);
  bs[off] := Byte(n shr 16);
end;

class procedure TEd25519.Encode32(n: UInt32; const bs: TCryptoLibByteArray;
  off: Int32);
begin
  // bs[  off] := Byte(n      );
  // System.Inc(off);
  // bs[off] := Byte(n shr  8);
  // System.Inc(off);
  // bs[off] := Byte(n shr  16);
  // System.Inc(off);
  // bs[off] := Byte(n shr  24);
  TConverters.ReadUInt32AsBytesLE(n, bs, off);
end;

class procedure TEd25519.Encode56(n: UInt64; const bs: TCryptoLibByteArray;
  off: Int32);
begin
  Encode32(UInt32(n), bs, off);
  Encode24(UInt32(n shr 32), bs, off + 4);
end;

class function TEd25519.CalculateS(const r, k, s: TCryptoLibByteArray)
  : TCryptoLibByteArray;
var
  T, U, V: TCryptoLibUInt32Array;
  i: Int32;
begin
  System.SetLength(T, ScalarUints * 2);
  DecodeScalar(r, 0, T);
  System.SetLength(U, ScalarUints);
  DecodeScalar(k, 0, U);
  System.SetLength(V, ScalarUints);
  DecodeScalar(s, 0, V);

  TNat256.MulAddTo(U, V, T);

  System.SetLength(result, ScalarBytes * 2);

  for i := 0 to System.Pred(System.Length(T)) do
  begin
    Encode32(T[i], result, i * 4);
  end;
  result := ReduceScalar(result);
end;

class function TEd25519.CheckContextVar(const ctx
  : TCryptoLibCustomByteArrayBuffer; phflag: Byte): Boolean;
begin
  result := ((ctx.IsNil) and (phflag = $00)) or
    ((not ctx.IsNil) and (ctx.Length < 256));
end;

class function TEd25519.CheckPointVar(const p: TCryptoLibByteArray): Boolean;
var
  T: TCryptoLibUInt32Array;
begin
  System.SetLength(T, 8);
  Decode32(p, 0, T, 0, 8);
  T[7] := T[7] and $7FFFFFFF;
  result := not(TNat256.Gte(T, FP));
end;

class function TEd25519.CheckScalarVar(const s: TCryptoLibByteArray): Boolean;
var
  n: TCryptoLibUInt32Array;
begin
  System.SetLength(n, ScalarUints);
  DecodeScalar(s, 0, n);
  result := not(TNat256.Gte(n, FL));
end;

constructor TEd25519.Create;
begin
  Inherited Create();
end;

function TEd25519.CreateDigest: IDigest;
begin
  result := TDigestUtilities.GetDigest('SHA-512');
end;

class constructor TEd25519.CreateEd25519;
begin
  TEd25519.Boot();
end;

function TEd25519.CreatePreHash: IDigest;
begin
  result := CreateDigest();
end;

class function TEd25519.Decode24(const bs: TCryptoLibByteArray;
  off: Int32): UInt32;
var
  n: UInt32;
begin
  n := bs[off];
  System.Inc(off);
  n := n or (UInt32(bs[off]) shl 8);
  System.Inc(off);
  n := n or (UInt32(bs[off]) shl 16);
  result := n;
end;

class procedure TEd25519.PointExtendXY(var p: TPointAccum);
begin
  TX25519Field.One(p.Z);
  TX25519Field.Copy(p.X, 0, p.U, 0);
  TX25519Field.Copy(p.Y, 0, p.V, 0);
end;

class procedure TEd25519.PointExtendXY(var p: TPointExt);
begin
  TX25519Field.One(p.Z);
  TX25519Field.Mul(p.X, p.Y, p.T);
end;

class procedure TEd25519.PointLookup(const table: TCryptoLibInt32Array;
  index: Int32; var r: TPointExt);
var
  off: Int32;
begin
  off := TX25519Field.SIZE * 4 * index;

  TX25519Field.Copy(table, off, r.X, 0);
  off := off + TX25519Field.SIZE;
  TX25519Field.Copy(table, off, r.Y, 0);
  off := off + TX25519Field.SIZE;
  TX25519Field.Copy(table, off, r.Z, 0);
  off := off + TX25519Field.SIZE;
  TX25519Field.Copy(table, off, r.T, 0);
end;

class function TEd25519.GetWindow4(const X: TCryptoLibUInt32Array;
  n: Int32): Int32;
var
  w, b: Int32;
begin
  w := TBits.Asr32(n, 3);
  b := (n and 7) shl 2;
  result := (X[w] shr b) and 15;
end;

class procedure TEd25519.PointLookup(const X: TCryptoLibUInt32Array; n: Int32;
  const table: TCryptoLibInt32Array; var r: TPointExt);
var
  w, LSign, abs, i, off, cond: Int32;
begin
  w := GetWindow4(X, n);

  LSign := (TBits.Asr32(w, (PrecompTeeth - 1))) xor 1;
  abs := (w xor -LSign) and PrecompMask;

{$IFDEF DEBUG}
  System.Assert((LSign = 0) or (LSign = 1));
  System.Assert((abs <= 0) and (abs < PrecompPoints));
{$ENDIF DEBUG}
  i := 0;
  off := 0;

  while i < PrecompPoints do
  begin
    cond := TBits.Asr32(((i xor abs) - 1), 31);
    TX25519Field.CMov(cond, table, off, r.X, 0);
    off := off + TX25519Field.SIZE;
    TX25519Field.CMov(cond, table, off, r.Y, 0);
    off := off + TX25519Field.SIZE;
    TX25519Field.CMov(cond, table, off, r.Z, 0);
    off := off + TX25519Field.SIZE;
    TX25519Field.CMov(cond, table, off, r.T, 0);
    off := off + TX25519Field.SIZE;
    System.Inc(i);
  end;

  TX25519Field.CNegate(LSign, r.X);
  TX25519Field.CNegate(LSign, r.T);
end;

class function TEd25519.DecodePointVar(const p: TCryptoLibByteArray;
  pOff: Int32; ANegate: Boolean; var r: TPointAffine): Boolean;
var
  py: TCryptoLibByteArray;
  U, V: TCryptoLibInt32Array;
  x_0: Int32;
begin
  py := TArrayUtils.CopyOfRange(p, pOff, pOff + PointBytes);
  if (not CheckPointVar(py)) then
  begin
    result := false;
    Exit;
  end;

  x_0 := (py[PointBytes - 1] and $80) shr 7;
  py[PointBytes - 1] := py[PointBytes - 1] and $7F;

  TX25519Field.Decode(py, 0, r.Y);

  U := TX25519Field.Create();
  V := TX25519Field.Create();

  TX25519Field.Sqr(r.Y, U);
  TX25519Field.Mul(FC_d, U, V);
  TX25519Field.SubOne(U);
  TX25519Field.AddOne(V);

  if (not(TX25519Field.SqrtRatioVar(U, V, r.X))) then
  begin
    result := false;
    Exit;
  end;

  TX25519Field.Normalize(r.X);
  if ((x_0 = 1) and (TX25519Field.IsZeroVar(r.X))) then
  begin
    result := false;
    Exit;
  end;

  if (ANegate xor (x_0 <> (r.X[0] and 1))) then
  begin
    TX25519Field.negate(r.X, r.X);
  end;

  result := true;
end;

class destructor TEd25519.DestroyEd25519;
begin
  FPrecompLock.Free;
end;

class procedure TEd25519.Dom2(const d: IDigest; phflag: Byte;
  const ctx: TCryptoLibCustomByteArrayBuffer);
begin
  if (not(ctx.IsNil)) then
  begin
    d.BlockUpdate(FDom2Prefix, 0, System.Length(FDom2Prefix));
    d.Update(phflag);
    d.Update(Byte(ctx.Length));
    if ctx.Data <> Nil then
    begin
      d.BlockUpdate(ctx.Data, 0, ctx.Length);
    end;
  end;
end;

class procedure TEd25519.EncodePoint(var p: TPointAccum;
  const r: TCryptoLibByteArray; rOff: Int32);
var
  X, Y: TCryptoLibInt32Array;
begin
  X := TX25519Field.Create();
  Y := TX25519Field.Create();

  TX25519Field.Inv(p.Z, Y);
  TX25519Field.Mul(p.X, Y, X);
  TX25519Field.Mul(p.Y, Y, Y);
  TX25519Field.Normalize(X);
  TX25519Field.Normalize(Y);

  TX25519Field.Encode(Y, r, rOff);
  r[rOff + PointBytes - 1] := r[rOff + PointBytes - 1] or
    Byte((X[0] and 1) shl 7);
end;

procedure TEd25519.GeneratePrivateKey(const random: ISecureRandom;
  const k: TCryptoLibByteArray);
begin
  random.NextBytes(k);
end;

class procedure TEd25519.PruneScalar(const n: TCryptoLibByteArray; nOff: Int32;
  const r: TCryptoLibByteArray);
begin
  System.Move(n[nOff], r[0], ScalarBytes * System.SizeOf(Byte));

  r[0] := r[0] and $F8;
  r[ScalarBytes - 1] := r[ScalarBytes - 1] and $7F;
  r[ScalarBytes - 1] := r[ScalarBytes - 1] or $40;
end;

class procedure TEd25519.ScalarMultBaseEncoded(const k, r: TCryptoLibByteArray;
  rOff: Int32);
var
  p: TPointAccum;
begin
  p := TPointAccum.CreatePointAccum();
  ScalarMultBase(k, p);
  EncodePoint(p, r, rOff);
end;

procedure TEd25519.GeneratePublicKey(const sk: TCryptoLibByteArray;
  skOff: Int32; pk: TCryptoLibByteArray; pkOff: Int32);
var
  d: IDigest;
  h, s: TCryptoLibByteArray;
begin
  d := CreateDigest();
  System.SetLength(h, d.GetDigestSize());

  d.BlockUpdate(sk, skOff, SecretKeySize);
  d.DoFinal(h, 0);

  System.SetLength(s, ScalarBytes);

  PruneScalar(h, 0, s);

  ScalarMultBaseEncoded(s, pk, pkOff);
end;

class function TEd25519.GetWnaf(const n: TCryptoLibUInt32Array; width: Int32)
  : TCryptoLibShortIntArray;
var
  T: TCryptoLibUInt32Array;
  ws: TCryptoLibShortIntArray;
  i, tPos, j: Int32;
  c, next, pow2, mask, LSign, carry, word, word16, bit, digit: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert((n[ScalarUints - 1] shr 28) = 0);
{$ENDIF DEBUG}
  System.SetLength(T, ScalarUints * 2);

  c := 0;
  tPos := System.Length(T);
  i := ScalarUints;
  System.Dec(i);
  while (i >= 0) do
  begin
    next := n[i];
    System.Dec(tPos);
    T[tPos] := (next shr 16) or (c shl 16);
    System.Dec(tPos);
    c := next;
    T[tPos] := c;
    System.Dec(i);
  end;

  System.SetLength(ws, 253);

  pow2 := UInt32(1) shl width;
  mask := pow2 - UInt32(1);
  LSign := pow2 shr 1;

  carry := UInt32(0);
  j := 0;

  for i := 0 to System.Pred(System.Length(T)) do
  begin
    word := T[i];
    while (j < 16) do
    begin
      word16 := word shr j;
      bit := word16 and UInt32(1);

      if (bit = carry) then
      begin
        System.Inc(j);
        continue;
      end;

      digit := (word16 and mask) + carry;
      carry := digit and LSign;
      digit := digit - (carry shl 1);
      carry := carry shr (width - 1);

      ws[(i shl 4) + j] := ShortInt(digit);

      j := j + width;
    end;
    System.Dec(j, 16);
  end;

{$IFDEF DEBUG}
  System.Assert(carry = 0);
{$ENDIF DEBUG}
  result := ws;
end;

procedure TEd25519.ImplSign(const sk: TCryptoLibByteArray; skOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
  const m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  d: IDigest;
  h, s: TCryptoLibByteArray;
begin
  if (not CheckContextVar(ctx, phflag)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidCtx);
  end;

  d := CreateDigest();
  System.SetLength(h, d.GetDigestSize());

  d.BlockUpdate(sk, skOff, SecretKeySize);
  d.DoFinal(h, 0);

  System.SetLength(s, ScalarBytes);
  PruneScalar(h, 0, s);

  ImplSign(d, h, s, pk, pkOff, ctx, phflag, m, mOff, mLen, sig, sigOff);

end;

procedure TEd25519.ImplSign(const sk: TCryptoLibByteArray; skOff: Int32;
  const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
  const m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  d: IDigest;
  h, s, pk: TCryptoLibByteArray;
begin
  if (not CheckContextVar(ctx, phflag)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidCtx);
  end;

  d := CreateDigest();
  System.SetLength(h, d.GetDigestSize());

  d.BlockUpdate(sk, skOff, SecretKeySize);
  d.DoFinal(h, 0);

  System.SetLength(s, ScalarBytes);
  PruneScalar(h, 0, s);

  System.SetLength(pk, PointBytes);
  ScalarMultBaseEncoded(s, pk, 0);

  ImplSign(d, h, s, pk, 0, ctx, phflag, m, mOff, mLen, sig, sigOff);
end;

class procedure TEd25519.ImplSign(const d: IDigest;
  const h, s, pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
  const m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  r, bigR, k, bigS: TCryptoLibByteArray;
begin
  Dom2(d, phflag, ctx);
  d.BlockUpdate(h, ScalarBytes, ScalarBytes);
  d.BlockUpdate(m, mOff, mLen);
  d.DoFinal(h, 0);

  r := ReduceScalar(h);
  System.SetLength(bigR, PointBytes);
  ScalarMultBaseEncoded(r, bigR, 0);

  Dom2(d, phflag, ctx);
  d.BlockUpdate(bigR, 0, PointBytes);
  d.BlockUpdate(pk, pkOff, PointBytes);
  d.BlockUpdate(m, mOff, mLen);
  d.DoFinal(h, 0);

  k := ReduceScalar(h);
  bigS := CalculateS(r, k, s);

  System.Move(bigR[0], sig[sigOff], PointBytes * System.SizeOf(Byte));
  System.Move(bigS[0], sig[sigOff + PointBytes],
    ScalarBytes * System.SizeOf(Byte));
end;

function TEd25519.ImplVerify(const sig: TCryptoLibByteArray; sigOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx: TCryptoLibCustomByteArrayBuffer; phflag: Byte;
  const m: TCryptoLibByteArray; mOff, mLen: Int32): Boolean;
var
  r, s, h, k, check: TCryptoLibByteArray;
  nS, nA: TCryptoLibUInt32Array;
  pA: TPointAffine;
  pR: TPointAccum;
  d: IDigest;
begin
  if (not CheckContextVar(ctx, phflag)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidCtx);
  end;

  r := TArrayUtils.CopyOfRange(sig, sigOff, sigOff + PointBytes);
  s := TArrayUtils.CopyOfRange(sig, sigOff + PointBytes,
    sigOff + SignatureSize);

  if (not CheckPointVar(r)) then
  begin
    result := false;
    Exit;
  end;

  if (not CheckScalarVar(s)) then
  begin
    result := false;
    Exit;
  end;

  pA := TPointAffine.CreatePointAffine();
  if (not DecodePointVar(pk, pkOff, true, pA)) then
  begin
    result := false;
    Exit;
  end;

  d := CreateDigest();
  System.SetLength(h, d.GetDigestSize());

  Dom2(d, phflag, ctx);
  d.BlockUpdate(r, 0, PointBytes);
  d.BlockUpdate(pk, pkOff, PointBytes);
  d.BlockUpdate(m, mOff, mLen);
  d.DoFinal(h, 0);

  k := ReduceScalar(h);

  System.SetLength(nS, ScalarUints);
  DecodeScalar(s, 0, nS);

  System.SetLength(nA, ScalarUints);

  DecodeScalar(k, 0, nA);

  pR := TPointAccum.CreatePointAccum();
  ScalarMultStrausVar(nS, nA, pA, pR);

  System.SetLength(check, PointBytes);

  EncodePoint(pR, check, 0);

  result := TArrayUtils.ConstantTimeAreEqual(check, r);
end;

class procedure TEd25519.PointAddPrecomp(var p: TPointPrecomp;
  var r: TPointAccum);
var
  bigA, bigB, bigC, bigD, bigE, bigF, bigG: TCryptoLibInt32Array;
begin
  bigA := TX25519Field.Create();
  bigB := TX25519Field.Create();
  bigC := TX25519Field.Create();
  bigD := r.U;
  bigE := TX25519Field.Create();
  bigF := TX25519Field.Create();
  bigG := r.V;

  TX25519Field.Apm(r.Y, r.X, bigB, bigA);
  TX25519Field.Mul(bigA, p.Ymx_h, bigA);
  TX25519Field.Mul(bigB, p.Ypx_h, bigB);
  TX25519Field.Mul(r.U, r.V, bigC);
  TX25519Field.Mul(bigC, p.Xyd, bigC);
  TX25519Field.Apm(bigB, bigA, bigG, bigD);
  TX25519Field.Apm(r.Z, bigC, bigF, bigE);
  TX25519Field.carry(bigF);
  TX25519Field.Mul(bigD, bigE, r.X);
  TX25519Field.Mul(bigF, bigG, r.Y);
  TX25519Field.Mul(bigE, bigF, r.Z);
end;

class procedure TEd25519.PointAdd(var p: TPointExt; var r: TPointAccum);
var
  A, b, c, d, E, F, G, h: TCryptoLibInt32Array;
begin
  A := TX25519Field.Create();
  b := TX25519Field.Create();
  c := TX25519Field.Create();
  d := TX25519Field.Create();
  E := r.U;
  F := TX25519Field.Create();
  G := TX25519Field.Create();
  h := r.V;

  TX25519Field.Apm(r.Y, r.X, b, A);
  TX25519Field.Apm(p.Y, p.X, d, c);
  TX25519Field.Mul(A, c, A);
  TX25519Field.Mul(b, d, b);
  TX25519Field.Mul(r.U, r.V, c);
  TX25519Field.Mul(c, p.T, c);
  TX25519Field.Mul(c, FC_d2, c);
  TX25519Field.Mul(r.Z, p.Z, d);
  TX25519Field.Add(d, d, d);
  TX25519Field.Apm(b, A, h, E);
  TX25519Field.Apm(d, c, G, F);
  TX25519Field.carry(G);
  TX25519Field.Mul(E, F, r.X);
  TX25519Field.Mul(G, h, r.Y);
  TX25519Field.Mul(F, G, r.Z);
end;

class procedure TEd25519.PointAdd(var p, r: TPointExt);
var
  A, b, c, d, E, F, G, h: TCryptoLibInt32Array;
begin
  A := TX25519Field.Create();
  b := TX25519Field.Create();
  c := TX25519Field.Create();
  d := TX25519Field.Create();
  E := TX25519Field.Create();
  F := TX25519Field.Create();
  G := TX25519Field.Create();
  h := TX25519Field.Create();

  TX25519Field.Apm(p.Y, p.X, b, A);
  TX25519Field.Apm(r.Y, r.X, d, c);
  TX25519Field.Mul(A, c, A);
  TX25519Field.Mul(b, d, b);
  TX25519Field.Mul(p.T, r.T, c);
  TX25519Field.Mul(c, FC_d2, c);
  TX25519Field.Mul(p.Z, r.Z, d);
  TX25519Field.Add(d, d, d);
  TX25519Field.Apm(b, A, h, E);
  TX25519Field.Apm(d, c, G, F);
  TX25519Field.carry(G);
  TX25519Field.Mul(E, F, r.X);
  TX25519Field.Mul(G, h, r.Y);
  TX25519Field.Mul(F, G, r.Z);
  TX25519Field.Mul(E, h, r.T);
end;

class procedure TEd25519.PointAddVar(negate: Boolean; var p, q, r: TPointExt);
var
  bigA, bigB, bigC, bigD, bigE, bigF, bigG, bigH, c, d, F,
    G: TCryptoLibInt32Array;
begin
  bigA := TX25519Field.Create();
  bigB := TX25519Field.Create();
  bigC := TX25519Field.Create();
  bigD := TX25519Field.Create();
  bigE := TX25519Field.Create();
  bigF := TX25519Field.Create();
  bigG := TX25519Field.Create();
  bigH := TX25519Field.Create();

  if (negate) then
  begin
    c := bigD;
    d := bigC;
    F := bigG;
    G := bigF;
  end
  else
  begin
    c := bigC;
    d := bigD;
    F := bigF;
    G := bigG;
  end;

  TX25519Field.Apm(p.Y, p.X, bigB, bigA);
  TX25519Field.Apm(q.Y, q.X, d, c);
  TX25519Field.Mul(bigA, bigC, bigA);
  TX25519Field.Mul(bigB, bigD, bigB);
  TX25519Field.Mul(p.T, q.T, bigC);
  TX25519Field.Mul(bigC, FC_d2, bigC);
  TX25519Field.Mul(p.Z, q.Z, bigD);
  TX25519Field.Add(bigD, bigD, bigD);
  TX25519Field.Apm(bigB, bigA, bigH, bigE);
  TX25519Field.Apm(bigD, bigC, G, F);
  TX25519Field.carry(G);
  TX25519Field.Mul(bigE, bigF, r.X);
  TX25519Field.Mul(bigG, bigH, r.Y);
  TX25519Field.Mul(bigF, bigG, r.Z);
  TX25519Field.Mul(bigE, bigH, r.T);
end;

class procedure TEd25519.PointAddVar(negate: Boolean; var p: TPointExt;
  var r: TPointAccum);
var
  bigA, bigB, bigC, bigD, bigE, bigF, bigG, bigH, c, d, F,
    G: TCryptoLibInt32Array;
begin
  bigA := TX25519Field.Create();
  bigB := TX25519Field.Create();
  bigC := TX25519Field.Create();
  bigD := TX25519Field.Create();
  bigE := r.U;
  bigF := TX25519Field.Create();
  bigG := TX25519Field.Create();
  bigH := r.V;

  if (negate) then
  begin
    c := bigD;
    d := bigC;
    F := bigG;
    G := bigF;
  end
  else
  begin
    c := bigC;
    d := bigD;
    F := bigF;
    G := bigG;
  end;

  TX25519Field.Apm(r.Y, r.X, bigB, bigA);
  TX25519Field.Apm(p.Y, p.X, d, c);
  TX25519Field.Mul(bigA, bigC, bigA);
  TX25519Field.Mul(bigB, bigD, bigB);
  TX25519Field.Mul(r.U, r.V, bigC);
  TX25519Field.Mul(bigC, p.T, bigC);
  TX25519Field.Mul(bigC, FC_d2, bigC);
  TX25519Field.Mul(r.Z, p.Z, bigD);
  TX25519Field.Add(bigD, bigD, bigD);
  TX25519Field.Apm(bigB, bigA, bigH, bigE);
  TX25519Field.Apm(bigD, bigC, G, F);
  TX25519Field.carry(G);
  TX25519Field.Mul(bigE, bigF, r.X);
  TX25519Field.Mul(bigG, bigH, r.Y);
  TX25519Field.Mul(bigF, bigG, r.Z);
end;

class function TEd25519.PointCopy(var p: TPointAffine): TPointExt;
var
  r: TPointExt;
begin
  r := TPointExt.CreatePointExt();
  TX25519Field.Copy(p.X, 0, r.X, 0);
  TX25519Field.Copy(p.Y, 0, r.Y, 0);
  PointExtendXY(r);
  result := r;
end;

class function TEd25519.PointCopy(var p: TPointAccum): TPointExt;
var
  r: TPointExt;
begin
  r := TPointExt.CreatePointExt();
  TX25519Field.Copy(p.X, 0, r.X, 0);
  TX25519Field.Copy(p.Y, 0, r.Y, 0);
  TX25519Field.Copy(p.Z, 0, r.Z, 0);
  TX25519Field.Mul(p.U, p.V, r.T);
  result := r;
end;

class function TEd25519.PointCopy(var p: TPointExt): TPointExt;
var
  r: TPointExt;
begin
  r := TPointExt.CreatePointExt();
  PointCopy(p, r);
  result := r;
end;

class procedure TEd25519.PointDouble(var r: TPointAccum);
var
  bigA, bigB, bigC, bigD, bigE, bigF, bigG: TCryptoLibInt32Array;
begin
  bigA := TX25519Field.Create();
  bigB := TX25519Field.Create();
  bigC := TX25519Field.Create();
  bigD := r.U;
  bigE := TX25519Field.Create();
  bigF := TX25519Field.Create();
  bigG := r.V;

  TX25519Field.Sqr(r.X, bigA);
  TX25519Field.Sqr(r.Y, bigB);
  TX25519Field.Sqr(r.Z, bigC);
  TX25519Field.Add(bigC, bigC, bigC);
  TX25519Field.Apm(bigA, bigB, bigG, bigF);
  TX25519Field.Add(r.X, r.Y, bigD);
  TX25519Field.Sqr(bigD, bigD);
  TX25519Field.Sub(bigG, bigD, bigD);
  TX25519Field.Add(bigC, bigF, bigE);
  TX25519Field.carry(bigE);
  TX25519Field.Mul(bigD, bigE, r.X);
  TX25519Field.Mul(bigF, bigG, r.Y);
  TX25519Field.Mul(bigE, bigF, r.Z);
end;

class procedure TEd25519.PointLookup(block, index: Int32; var p: TPointPrecomp);
var
  off, i, cond: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((0 <= block) and (block < PrecompBlocks));
  System.Assert((0 <= index) and (index < PrecompPoints));
{$ENDIF DEBUG}
  off := block * PrecompPoints * 3 * TX25519Field.SIZE;

  for i := 0 to System.Pred(PrecompPoints) do
  begin
    cond := TBits.Asr32(((i xor index) - 1), 31);
    TX25519Field.CMov(cond, FPrecompBase, off, p.Ypx_h, 0);
    off := off + TX25519Field.SIZE;
    TX25519Field.CMov(cond, FPrecompBase, off, p.Ymx_h, 0);
    off := off + TX25519Field.SIZE;
    TX25519Field.CMov(cond, FPrecompBase, off, p.Xyd, 0);
    off := off + TX25519Field.SIZE;
  end;
end;

class function TEd25519.PointPrecomp(var p: TPointAffine; count: Int32)
  : TCryptoLibInt32Array;
var
  q, d: TPointExt;
  table: TCryptoLibInt32Array;
  off, i: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(count > 0);
{$ENDIF DEBUG}
  q := PointCopy(p);
  d := PointCopy(q);
  PointAdd(q, d);

  table := TX25519Field.createTable(count * 4);
  off := 0;

  i := 0;

  while (true) do
  begin
    TX25519Field.Copy(q.X, 0, table, off);
    off := off + TX25519Field.SIZE;
    TX25519Field.Copy(q.Y, 0, table, off);
    off := off + TX25519Field.SIZE;
    TX25519Field.Copy(q.Z, 0, table, off);
    off := off + TX25519Field.SIZE;
    TX25519Field.Copy(q.T, 0, table, off);
    off := off + TX25519Field.SIZE;

    System.Inc(i);

    if (i = count) then
    begin
      break;
    end;

    PointAdd(d, q);

    System.Inc(i);
  end;

  result := table;
end;

class function TEd25519.PointPrecompVar(var p: TPointExt; count: Int32)
  : TCryptoLibGenericArray<TPointExt>;
var
  d: TPointExt;
  table: TCryptoLibGenericArray<TPointExt>;
  i: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(count > 0);
{$ENDIF DEBUG}
  d := TPointExt.CreatePointExt();
  PointAddVar(false, p, p, d);

  System.SetLength(table, count);

  table[0] := PointCopy(p);
  for i := 1 to System.Pred(count) do
  begin
    table[i] := TPointExt.CreatePointExt();
    PointAddVar(false, table[i - 1], d, table[i]);
  end;
  result := table;
end;

class procedure TEd25519.PointSetNeutral(var p: TPointAccum);
begin
  TX25519Field.Zero(p.X);
  TX25519Field.One(p.Y);
  TX25519Field.One(p.Z);
  TX25519Field.Zero(p.U);
  TX25519Field.One(p.V);
end;

class procedure TEd25519.PointSetNeutral(var p: TPointExt);
begin
  TX25519Field.Zero(p.X);
  TX25519Field.One(p.Y);
  TX25519Field.One(p.Z);
  TX25519Field.Zero(p.T);
end;

class procedure TEd25519.Precompute;
var
  bigB: TPointExt;
  p: TPointAccum;
  X, Y: TCryptoLibInt32Array;
  off, b, T, s, k, SIZE, j, i: Int32;
  // ds, points: TCryptoLibGenericArray<TPointExt>;
  ds: array [0 .. (PrecompTeeth - 1)] of TPointExt;
  points: array [0 .. (PrecompPoints - 1)] of TPointExt;
  sum, q: TPointExt;
  r: TPointPrecomp;
begin
  FPrecompLock.Acquire;
  try

    if (FPrecompBase <> Nil) then
    begin
      Exit;
    end;

    // Precomputed table for the base point in verification ladder
    bigB := TPointExt.CreatePointExt();
    TX25519Field.Copy(FB_x, 0, bigB.X, 0);
    TX25519Field.Copy(FB_y, 0, bigB.Y, 0);
    PointExtendXY(bigB);

    FPrecompBaseTable := PointPrecompVar(bigB, 1 shl (WnafWidthBase - 2));

    p := TPointAccum.CreatePointAccum();
    TX25519Field.Copy(FB_x, 0, p.X, 0);
    TX25519Field.Copy(FB_y, 0, p.Y, 0);
    PointExtendXY(p);

    FPrecompBase := TX25519Field.createTable(PrecompBlocks * PrecompPoints * 3);

    off := 0;
    for b := 0 to System.Pred(PrecompBlocks) do
    begin
      // System.SetLength(ds, PrecompTeeth); // **

      sum := TPointExt.CreatePointExt();
      PointSetNeutral(sum);

      for T := 0 to System.Pred(PrecompTeeth) do
      begin
        q := PointCopy(p);
        PointAddVar(true, sum, q, sum);
        PointDouble(p);

        ds[T] := PointCopy(p);

        if ((b + T) <> (PrecompBlocks + PrecompTeeth - 2)) then
        begin
          s := 1;
          while s < PrecompSpacing do
          begin
            PointDouble(p);
            System.Inc(s);
          end;
        end;
      end;

      // System.SetLength(points, PrecompPoints); // **

      k := 0;
      points[k] := sum;
      System.Inc(k);

      for T := 0 to System.Pred(PrecompTeeth - 1) do
      begin
        SIZE := 1 shl T;
        j := 0;
        while j < SIZE do
        begin
          points[k] := TPointExt.CreatePointExt();
          PointAddVar(false, points[k - SIZE], ds[T], points[k]);
          System.Inc(k);
          System.Inc(j);
        end;

      end;
{$IFDEF DEBUG}
      System.Assert(k = PrecompPoints);
{$ENDIF DEBUG}
      for i := 0 to System.Pred(PrecompPoints) do
      begin
        q := points[i];

        X := TX25519Field.Create();
        Y := TX25519Field.Create();

        TX25519Field.Add(q.Z, q.Z, X);
        // TODO[ed25519] Batch inversion
        TX25519Field.Inv(X, Y);
        TX25519Field.Mul(q.X, Y, X);
        TX25519Field.Mul(q.Y, Y, Y);

        r := TPointPrecomp.CreatePointPrecomp();
        TX25519Field.Apm(Y, X, r.Ypx_h, r.Ymx_h);
        TX25519Field.Mul(X, Y, r.Xyd);
        TX25519Field.Mul(r.Xyd, FC_d4, r.Xyd);

        TX25519Field.Normalize(r.Ypx_h);
        TX25519Field.Normalize(r.Ymx_h);
        // TX25519Field.Normalize(r.xyd);

        TX25519Field.Copy(r.Ypx_h, 0, FPrecompBase, off);
        off := off + TX25519Field.SIZE;
        TX25519Field.Copy(r.Ymx_h, 0, FPrecompBase, off);
        off := off + TX25519Field.SIZE;
        TX25519Field.Copy(r.Xyd, 0, FPrecompBase, off);
        off := off + TX25519Field.SIZE;
      end;
    end;

{$IFDEF DEBUG}
    System.Assert(off = System.Length(FPrecompBase));
{$ENDIF DEBUG}
  finally
    FPrecompLock.Release;
  end;
end;

class function TEd25519.ReduceScalar(const n: TCryptoLibByteArray)
  : TCryptoLibByteArray;
var
  x00, x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, x11, x12, x13, x14,
    x15, x16, x17, x18, T: Int64;
begin
  x00 := Decode32(n, 0) and Int64(M32L); // x00:32/--
  x01 := (Decode24(n, 4) shl 4) and Int64(M32L); // x01:28/--
  x02 := Decode32(n, 7) and Int64(M32L); // x02:32/--
  x03 := (Decode24(n, 11) shl 4) and Int64(M32L); // x03:28/--
  x04 := Decode32(n, 14) and Int64(M32L); // x04:32/--
  x05 := (Decode24(n, 18) shl 4) and Int64(M32L); // x05:28/--
  x06 := Decode32(n, 21) and Int64(M32L); // x06:32/--
  x07 := (Decode24(n, 25) shl 4) and Int64(M32L); // x07:28/--
  x08 := Decode32(n, 28) and Int64(M32L); // x08:32/--
  x09 := (Decode24(n, 32) shl 4) and Int64(M32L); // x09:28/--
  x10 := Decode32(n, 35) and Int64(M32L); // x10:32/--
  x11 := (Decode24(n, 39) shl 4) and Int64(M32L); // x11:28/--
  x12 := Decode32(n, 42) and Int64(M32L); // x12:32/--
  x13 := (Decode24(n, 46) shl 4) and Int64(M32L); // x13:28/--
  x14 := Decode32(n, 49) and Int64(M32L); // x14:32/--
  x15 := (Decode24(n, 53) shl 4) and Int64(M32L); // x15:28/--
  x16 := Decode32(n, 56) and Int64(M32L); // x16:32/--
  x17 := (Decode24(n, 60) shl 4) and Int64(M32L); // x17:28/--
  x18 := n[63] and Int64($FF); // x18:08/--

  // x18 := x18 + (x17 shr  28); x17 := x17 and Int64(M28L);
  x09 := x09 - (x18 * L0); // x09:34/28
  x10 := x10 - (x18 * L1); // x10:33/30
  x11 := x11 - (x18 * L2); // x11:35/28
  x12 := x12 - (x18 * L3); // x12:32/31
  x13 := x13 - (x18 * L4); // x13:28/21

  x17 := x17 + TBits.Asr64(x16, 28); // x17:28/--
  x16 := x16 and Int64(M28L); // x16:28/--
  x08 := x08 - (x17 * L0); // x08:54/32
  x09 := x09 - (x17 * L1); // x09:52/51
  x10 := x10 - (x17 * L2); // x10:55/34
  x11 := x11 - (x17 * L3); // x11:51/36
  x12 := x12 - (x17 * L4); // x12:41/--

  // x16 := x16 + (x15 shr  28); x15 := x15 and Int64(M28L);
  x07 := x07 - (x16 * L0); // x07:54/28
  x08 := x08 - (x16 * L1); // x08:54/53
  x09 := x09 - (x16 * L2); // x09:55/53
  x10 := x10 - (x16 * L3); // x10:55/52
  x11 := x11 - (x16 * L4); // x11:51/41

  x15 := x15 + TBits.Asr64(x14, 28); // x15:28/--
  x14 := x14 and Int64(M28L); // x14:28/--
  x06 := x06 - (x15 * L0); // x06:54/32
  x07 := x07 - (x15 * L1); // x07:54/53
  x08 := x08 - (x15 * L2); // x08:56/--
  x09 := x09 - (x15 * L3); // x09:55/54
  x10 := x10 - (x15 * L4); // x10:55/53

  // x14 := x14 + (x13 shr  28); x13 := x13 and Int64(M28L);
  x05 := x05 - (x14 * L0); // x05:54/28
  x06 := x06 - (x14 * L1); // x06:54/53
  x07 := x07 - (x14 * L2); // x07:56/--
  x08 := x08 - (x14 * L3); // x08:56/51
  x09 := x09 - (x14 * L4); // x09:56/--

  x13 := x13 + TBits.Asr64(x12, 28); // x13:28/22
  x12 := x12 and Int64(M28L); // x12:28/--
  x04 := x04 - (x13 * L0); // x04:54/49
  x05 := x05 - (x13 * L1); // x05:54/53
  x06 := x06 - (x13 * L2); // x06:56/--
  x07 := x07 - (x13 * L3); // x07:56/52
  x08 := x08 - (x13 * L4); // x08:56/52

  x12 := x12 + TBits.Asr64(x11, 28); // x12:28/24
  x11 := x11 and Int64(M28L); // x11:28/--
  x03 := x03 - (x12 * L0); // x03:54/49
  x04 := x04 - (x12 * L1); // x04:54/51
  x05 := x05 - (x12 * L2); // x05:56/--
  x06 := x06 - (x12 * L3); // x06:56/52
  x07 := x07 - (x12 * L4); // x07:56/53

  x11 := x11 + TBits.Asr64(x10, 28); // x11:29/--
  x10 := x10 and Int64(M28L); // x10:28/--
  x02 := x02 - (x11 * L0); // x02:55/32
  x03 := x03 - (x11 * L1); // x03:55/--
  x04 := x04 - (x11 * L2); // x04:56/55
  x05 := x05 - (x11 * L3); // x05:56/52
  x06 := x06 - (x11 * L4); // x06:56/53

  x10 := x10 + TBits.Asr64(x09, 28); // x10:29/--
  x09 := x09 and Int64(M28L); // x09:28/--
  x01 := x01 - (x10 * L0); // x01:55/28
  x02 := x02 - (x10 * L1); // x02:55/54
  x03 := x03 - (x10 * L2); // x03:56/55
  x04 := x04 - (x10 * L3); // x04:57/--
  x05 := x05 - (x10 * L4); // x05:56/53

  x08 := x08 + TBits.Asr64(x07, 28); // x08:56/53
  x07 := x07 and Int64(M28L); // x07:28/--
  x09 := x09 + TBits.Asr64(x08, 28); // x09:29/25
  x08 := x08 and Int64(M28L); // x08:28/--

  T := TBits.Asr64(x08, 27) and Int64(1);
  x09 := x09 + T; // x09:29/26

  x00 := x00 - (x09 * L0); // x00:55/53
  x01 := x01 - (x09 * L1); // x01:55/54
  x02 := x02 - (x09 * L2); // x02:57/--
  x03 := x03 - (x09 * L3); // x03:57/--
  x04 := x04 - (x09 * L4); // x04:57/42

  x01 := x01 + TBits.Asr64(x00, 28);
  x00 := x00 and Int64(M28L);
  x02 := x02 + TBits.Asr64(x01, 28);
  x01 := x01 and Int64(M28L);
  x03 := x03 + TBits.Asr64(x02, 28);
  x02 := x02 and Int64(M28L);
  x04 := x04 + TBits.Asr64(x03, 28);
  x03 := x03 and Int64(M28L);
  x05 := x05 + TBits.Asr64(x04, 28);
  x04 := x04 and Int64(M28L);
  x06 := x06 + TBits.Asr64(x05, 28);
  x05 := x05 and Int64(M28L);
  x07 := x07 + TBits.Asr64(x06, 28);
  x06 := x06 and Int64(M28L);
  x08 := x08 + TBits.Asr64(x07, 28);
  x07 := x07 and Int64(M28L);
  x09 := TBits.Asr64(x08, 28);
  x08 := x08 and Int64(M28L);

  x09 := x09 - T;

{$IFDEF DEBUG}
  System.Assert((x09 = Int64(0)) or (x09 = -Int64(1)));
{$ENDIF DEBUG}
  x00 := x00 + (x09 and L0);
  x01 := x01 + (x09 and L1);
  x02 := x02 + (x09 and L2);
  x03 := x03 + (x09 and L3);
  x04 := x04 + (x09 and L4);

  x01 := x01 + TBits.Asr64(x00, 28);
  x00 := x00 and Int64(M28L);
  x02 := x02 + TBits.Asr64(x01, 28);
  x01 := x01 and Int64(M28L);
  x03 := x03 + TBits.Asr64(x02, 28);
  x02 := x02 and Int64(M28L);
  x04 := x04 + TBits.Asr64(x03, 28);
  x03 := x03 and Int64(M28L);
  x05 := x05 + TBits.Asr64(x04, 28);
  x04 := x04 and Int64(M28L);
  x06 := x06 + TBits.Asr64(x05, 28);
  x05 := x05 and Int64(M28L);
  x07 := x07 + TBits.Asr64(x06, 28);
  x06 := x06 and Int64(M28L);
  x08 := x08 + TBits.Asr64(x07, 28);
  x07 := x07 and Int64(M28L);

  System.SetLength(result, ScalarBytes);
  Encode56(UInt64(x00 or (x01 shl 28)), result, 0);
  Encode56(UInt64(x02 or (x03 shl 28)), result, 7);
  Encode56(UInt64(x04 or (x05 shl 28)), result, 14);
  Encode56(UInt64(x06 or (x07 shl 28)), result, 21);
  Encode32(UInt32(x08), result, 28);
end;

class procedure TEd25519.ScalarMult(const k: TCryptoLibByteArray;
  var p: TPointAffine; var r: TPointAccum);
var
  n: TCryptoLibUInt32Array;
  table: TCryptoLibInt32Array;
  q: TPointExt;
  w, c1, c2: Int32;
begin
  Precompute();
  System.SetLength(n, ScalarUints);

  DecodeScalar(k, 0, n);

{$IFDEF DEBUG}
  System.Assert((n[0] and 7) = 0);
  System.Assert((TBits.Asr32(n[ScalarUints - 1], 30)) = 1);
{$ENDIF DEBUG}
  TNat.ShiftDownBits(ScalarUints, n, 3, 1);

  c1 := TNat.CAdd(ScalarUints, ((not n[0]) and 1), n, FL, n);
{$IFOPT C+}
  System.Assert(c1 = 0);
{$ENDIF}
  c2 := TNat.ShiftDownBit(ScalarUints, n, 0);
{$IFOPT C+}
  System.Assert(c2 = (1 shl 31));
{$ENDIF}
{$IFDEF DEBUG}
  System.Assert((TBits.Asr32(n[ScalarUints - 1], 28)) = 1);
{$ENDIF DEBUG}
  PointCopy(p, r);

  table := PointPrecomp(p, 8);

  q := TPointExt.CreatePointExt();

  // Replace first 4 doublings (2^4 * P) with 1 addition (P + 15 * P)
  PointLookup(table, 7, q);
  PointAdd(q, r);

  w := 62;
  while (true) do
  begin
    PointLookup(n, w, table, q);
    PointAdd(q, r);

    PointDouble(r);
    PointDouble(r);
    PointDouble(r);

    System.Dec(w);
    if (w < 0) then
    begin
      break;
    end;

    PointDouble(r);
  end;
end;

class procedure TEd25519.ScalarMultBase(const k: TCryptoLibByteArray;
  var r: TPointAccum);
var
  n: TCryptoLibUInt32Array;
  w, c1, c2: UInt32;
  i, cOff, b, LSign, abs: Int32;
  p: TPointPrecomp;
begin
  Precompute();

  PointSetNeutral(r);
  System.SetLength(n, ScalarUints);
  DecodeScalar(k, 0, n);

  // Recode the scalar into signed-digit form, then group comb bits in each block

  c1 := TNat.CAdd(ScalarUints, not(Int32(n[0])) and 1, n, FL, n);
{$IFOPT C+}
  System.Assert(c1 = 0);
{$ENDIF}
  c2 := TNat.ShiftDownBit(ScalarUints, n, UInt32(1));
{$IFOPT C+}
  System.Assert(c2 = (UInt32(1) shl 31));
{$ENDIF}
  for i := 0 to System.Pred(ScalarUints) do
  begin
    n[i] := TInterleave.Shuffle2(n[i]);
  end;

  p := TPointPrecomp.CreatePointPrecomp();

  cOff := (PrecompSpacing - 1) * PrecompTeeth;
  while true do

  begin
    for b := 0 to System.Pred(PrecompBlocks) do
    begin
      w := n[b] shr cOff;
      LSign := Int32(w shr (PrecompTeeth - 1)) and 1;
      abs := (Int32(w) xor -LSign) and PrecompMask;

{$IFDEF DEBUG}
      System.Assert((LSign = 0) or (LSign = 1));
      System.Assert((0 <= abs) and (abs < PrecompPoints));
{$ENDIF DEBUG}
      PointLookup(b, abs, p);

      TX25519Field.CSwap(LSign, p.Ypx_h, p.Ymx_h);
      TX25519Field.CNegate(LSign, p.Xyd);

      PointAddPrecomp(p, r);
    end;
    cOff := (cOff - PrecompTeeth);
    if (cOff < 0) then
    begin
      break;
    end;

    PointDouble(r);
  end;
end;

class procedure TEd25519.ScalarMultBaseYZ(const k: TCryptoLibByteArray;
  kOff: Int32; const Y, Z: TCryptoLibInt32Array);
var
  n: TCryptoLibByteArray;
  p: TPointAccum;
begin
  System.SetLength(n, ScalarBytes);
  PruneScalar(k, kOff, n);

  p := TPointAccum.CreatePointAccum();
  ScalarMultBase(n, p);
  TX25519Field.Copy(p.Y, 0, Y, 0);
  TX25519Field.Copy(p.Z, 0, Z, 0);
end;

class procedure TEd25519.ScalarMultStrausVar(const nb,
  np: TCryptoLibUInt32Array; var p: TPointAffine; var r: TPointAccum);
var
  width, bit, wb, wp, LSign, index: Int32;
  ws_b, ws_p: TCryptoLibShortIntArray;
  tp: TCryptoLibGenericArray<TPointExt>;
  temp: TPointExt;
begin
  Precompute();

  width := 5;

  ws_b := GetWnaf(nb, WnafWidthBase);
  ws_p := GetWnaf(np, width);

  temp := PointCopy(p);
  tp := PointPrecompVar(temp, 1 shl (width - 2));

  PointSetNeutral(r);

  bit := 252;

  while (true) do
  begin
    wb := ws_b[bit];
    if (wb <> 0) then
    begin
      LSign := TBits.Asr32(wb, 31);
      index := TBits.Asr32((wb xor LSign), 1);

      PointAddVar((LSign <> 0), FPrecompBaseTable[index], r);
    end;

    wp := ws_p[bit];
    if (wp <> 0) then
    begin
      LSign := TBits.Asr32(wp, 31);
      index := TBits.Asr32((wp xor LSign), 1);

      PointAddVar((LSign <> 0), tp[index], r);
    end;

    System.Dec(bit);

    if (bit < 0) then
    begin
      break;
    end;

    PointDouble(r);
  end;
end;

class function TEd25519.ValidateDigestOutputSize(const ADigest: IDigest)
  : TCryptoLibByteArray;
begin
  System.SetLength(result, PreHashSize);
  if (PreHashSize <> ADigest.DoFinal(result, 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SDigestError);
  end;
end;

class function TEd25519.ConstructCustomByteArrayBufferContext
  (const AData: TCryptoLibByteArray; AIsNil: Boolean; ALength: Int32)
  : TCryptoLibCustomByteArrayBuffer;
begin
  result := Default (TCryptoLibCustomByteArrayBuffer);
  result.Data := AData;
  result.IsNil := AIsNil;
  result.Length := ALength;
end;

procedure TEd25519.Sign(const sk: TCryptoLibByteArray; skOff: Int32;
  const m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
begin
  phflag := $00;
  ImplSign(sk, skOff, EmptyBytesNil, phflag, m, mOff, mLen, sig, sigOff);
end;

procedure TEd25519.Sign(const sk: TCryptoLibByteArray; skOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
  mOff, mLen: Int32; const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
begin
  phflag := $00;
  ImplSign(sk, skOff, pk, pkOff, EmptyBytesNil, phflag, m, mOff, mLen,
    sig, sigOff);
end;

procedure TEd25519.Sign(const sk: TCryptoLibByteArray; skOff: Int32;
  const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $00;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, LCtx, phflag, m, mOff, mLen, sig, sigOff);
end;

procedure TEd25519.Sign(const sk: TCryptoLibByteArray; skOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $00;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, pk, pkOff, LCtx, phflag, m, mOff, mLen, sig, sigOff);
end;

procedure TEd25519.SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
  const ctx, ph: TCryptoLibByteArray; phOff: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, LCtx, phflag, ph, phOff, PreHashSize, sig, sigOff);
end;

procedure TEd25519.SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx, ph: TCryptoLibByteArray; phOff: Int32;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, pk, pkOff, LCtx, phflag, ph, phOff, PreHashSize,
    sig, sigOff);
end;

procedure TEd25519.SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
  const ctx: TCryptoLibByteArray; const ph: IDigest;
  const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  m: TCryptoLibByteArray;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  m := ValidateDigestOutputSize(ph);
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, LCtx, phflag, m, 0, System.Length(m), sig, sigOff);
end;

procedure TEd25519.SignPreHash(const sk: TCryptoLibByteArray; skOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32; const ctx: TCryptoLibByteArray;
  const ph: IDigest; const sig: TCryptoLibByteArray; sigOff: Int32);
var
  phflag: Byte;
  m: TCryptoLibByteArray;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  m := ValidateDigestOutputSize(ph);
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  ImplSign(sk, skOff, pk, pkOff, LCtx, phflag, m, 0, System.Length(m),
    sig, sigOff);
end;

function TEd25519.Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32; const m: TCryptoLibByteArray;
  mOff, mLen: Int32): Boolean;
var
  phflag: Byte;
begin
  phflag := $00;
  result := ImplVerify(sig, sigOff, pk, pkOff, EmptyBytesNil, phflag, m,
    mOff, mLen);
end;

function TEd25519.Verify(const sig: TCryptoLibByteArray; sigOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx, m: TCryptoLibByteArray; mOff, mLen: Int32): Boolean;
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $00;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  result := ImplVerify(sig, sigOff, pk, pkOff, LCtx, phflag, m, mOff, mLen);
end;

function TEd25519.VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32;
  const ctx, ph: TCryptoLibByteArray; phOff: Int32): Boolean;
var
  phflag: Byte;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  result := ImplVerify(sig, sigOff, pk, pkOff, LCtx, phflag, ph, phOff,
    PreHashSize);
end;

function TEd25519.VerifyPreHash(const sig: TCryptoLibByteArray; sigOff: Int32;
  const pk: TCryptoLibByteArray; pkOff: Int32; const ctx: TCryptoLibByteArray;
  const ph: IDigest): Boolean;
var
  phflag: Byte;
  m: TCryptoLibByteArray;
  LCtx: TCryptoLibCustomByteArrayBuffer;
begin
  phflag := $01;
  m := ValidateDigestOutputSize(ph);
  LCtx := ConstructCustomByteArrayBufferContext(ctx, false, System.Length(ctx));
  result := ImplVerify(sig, sigOff, pk, pkOff, LCtx, phflag, m, 0,
    System.Length(m));
end;

class procedure TEd25519.PointCopy(var p: TPointAffine; var r: TPointAccum);
begin
  TX25519Field.Copy(p.X, 0, r.X, 0);
  TX25519Field.Copy(p.Y, 0, r.Y, 0);
  PointExtendXY(r);
end;

class procedure TEd25519.PointCopy(var p, r: TPointExt);
begin
  TX25519Field.Copy(p.X, 0, r.X, 0);
  TX25519Field.Copy(p.Y, 0, r.Y, 0);
  TX25519Field.Copy(p.Z, 0, r.Z, 0);
  TX25519Field.Copy(p.T, 0, r.T, 0);
end;

{ TEd25519.TPointAffine }

function TEd25519.TPointAffine.GetX: TCryptoLibInt32Array;
begin
  result := Fx;
end;

function TEd25519.TPointAffine.GetY: TCryptoLibInt32Array;
begin
  result := Fy;
end;

procedure TEd25519.TPointAffine.SetX(const value: TCryptoLibInt32Array);
begin
  Fx := value;
end;

procedure TEd25519.TPointAffine.SetY(const value: TCryptoLibInt32Array);
begin
  Fy := value;
end;

class function TEd25519.TPointAffine.CreatePointAffine: TPointAffine;
begin
  result := Default (TPointAffine);
  result.Fx := TX25519Field.Create();
  result.Fy := TX25519Field.Create();
end;

end.
