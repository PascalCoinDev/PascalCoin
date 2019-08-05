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

unit ClpECDomainParameters;

{$I CryptoLib.inc}

interface

uses
  SyncObjs,
  ClpBigInteger,
  ClpECAlgorithms,
  ClpIECC,
  ClpCryptoLibTypes,
  ClpIECDomainParameters;

resourcestring
  SCurveNil = 'Curve Cannot be Nil';
  SScalarNil = 'Scalar Cannot be Nil';
  SGNil = 'G Cannot be Nil';
  SBigIntegerNotInitialized = 'BigInteger Not Initialized "%s"';
  SQNil = 'Q Cannot be Nil';
  SQInfinity = 'Point at Infinity "Q"';
  SQPointNotOnCurve = 'Point Not on Curve "Q"';
  SScalarInvalidRange = 'Scalar is not in the Interval [1, n - 1]';

type

  TECDomainParameters = class sealed(TInterfacedObject, IECDomainParameters)

  strict private

  var
    FLock: TCriticalSection;
    Fcurve: IECCurve;
    Fseed: TCryptoLibByteArray;
    Fg: IECPoint;
    Fn, Fh, FhInv: TBigInteger;

    function GetCurve: IECCurve; inline;
    function GetG: IECPoint; inline;
    function GetH: TBigInteger; inline;
    function GetN: TBigInteger; inline;
    function GetHInv: TBigInteger; inline;
    function GetSeed: TCryptoLibByteArray; inline;

  public

    class function ValidatePublicPoint(const c: IECCurve; const q: IECPoint)
      : IECPoint; overload; static;

    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger; const seed: TCryptoLibByteArray); overload;

    function ValidatePrivateScalar(const d: TBigInteger): TBigInteger;
    function ValidatePublicPoint(const q: IECPoint): IECPoint; overload;

    destructor Destroy; override;

    property curve: IECCurve read GetCurve;
    property g: IECPoint read GetG;
    property n: TBigInteger read GetN;
    property h: TBigInteger read GetH;
    property HInv: TBigInteger read GetHInv;
    property seed: TCryptoLibByteArray read GetSeed;
    function Equals(const other: IECDomainParameters): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TECDomainParameters }

class function TECDomainParameters.ValidatePublicPoint(const c: IECCurve;
  const q: IECPoint): IECPoint;
begin
  if (q = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SQNil);

  result := TECAlgorithms.ImportPoint(c, q).Normalize();

  if (result.IsInfinity) then
    raise EArgumentCryptoLibException.CreateRes(@SQInfinity);

  if (not(result.IsValid())) then
    raise EArgumentCryptoLibException.CreateRes(@SQPointNotOnCurve);

end;

function TECDomainParameters.GetCurve: IECCurve;
begin
  result := Fcurve;
end;

function TECDomainParameters.GetG: IECPoint;
begin
  result := Fg;
end;

function TECDomainParameters.GetH: TBigInteger;
begin
  result := Fh;
end;

function TECDomainParameters.GetN: TBigInteger;
begin
  result := Fn;
end;

function TECDomainParameters.GetHInv: TBigInteger;
begin
  FLock.Acquire;
  try
    if (not(FhInv.IsInitialized)) then
    begin
      FhInv := h.ModInverse(n);
    end;
    result := FhInv;
  finally
    FLock.Release;
  end;
end;

function TECDomainParameters.GetSeed: TCryptoLibByteArray;
begin
  result := System.Copy(Fseed);
end;

constructor TECDomainParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n: TBigInteger);
begin
  Create(curve, g, n, TBigInteger.One, Nil);
end;

constructor TECDomainParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n, h: TBigInteger);
begin
  Create(curve, g, n, h, Nil);
end;

constructor TECDomainParameters.Create(const curve: IECCurve; const g: IECPoint;
  const n, h: TBigInteger; const seed: TCryptoLibByteArray);
begin
  if (curve = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SCurveNil);
  if (g = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SGNil);

  if (not n.IsInitialized) then
    raise EArgumentNilCryptoLibException.CreateResFmt
      (@SBigIntegerNotInitialized, ['n']);

  FLock := TCriticalSection.Create;

  // we can't check for (not (h.IsInitialized)) here as h is optional in X9.62 as it is not required for ECDSA

  Fcurve := curve;
  Fg := ValidatePublicPoint(curve, g);
  Fn := n;
  Fh := h;
  FhInv := Default (TBigInteger);

  Fseed := System.Copy(seed);

end;

destructor TECDomainParameters.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TECDomainParameters.ValidatePublicPoint(const q: IECPoint): IECPoint;
begin
  result := ValidatePublicPoint(curve, q);
end;

function TECDomainParameters.ValidatePrivateScalar(const d: TBigInteger)
  : TBigInteger;
begin
  if (not(d.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SScalarNil);
  end;

  if ((d.CompareTo(TBigInteger.One) < 0) or (d.CompareTo(n) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SScalarInvalidRange);
  end;
  result := d;
end;

function TECDomainParameters.Equals(const other: IECDomainParameters): Boolean;
begin

  if (other = Self as IECDomainParameters) then
  begin
    result := True;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;

  result := curve.Equals(other.curve) and g.Equals(other.g) and
    n.Equals(other.n);

  if h.IsInitialized then
  begin
    result := result and h.Equals(other.h);
  end;
end;

function TECDomainParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := 4;
  result := result * 257;
  result := result xor Fcurve.GetHashCode();
  result := result * 257;
  result := result xor Fg.GetHashCode();
  result := result * 257;
  result := result xor Fn.GetHashCode();

  if h.IsInitialized then
  begin
    result := result * 257;
    result := result xor Fh.GetHashCode();
  end;
end;

end.
