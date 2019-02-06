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
  SGNil = 'G Cannot be Nil';
  SBigIntegerNotInitialized = 'BigInteger Not Initialized "%s"';
  SQNil = 'Q Cannot be Nil';
  SQInfinity = 'Point at Infinity "Q"';
  SQPointNotOnCurve = 'Point Not on Curve "Q"';

type

  TECDomainParameters = class sealed(TInterfacedObject, IECDomainParameters)

  strict private

    class var

      FLock: TCriticalSection;

  var
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

    class procedure Boot(); static;
    class constructor CreateECDomainParameters();
    class destructor DestroyECDomainParameters();

  public

    class function Validate(const c: IECCurve; const q: IECPoint)
      : IECPoint; static;

    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger); overload;
    constructor Create(const curve: IECCurve; const g: IECPoint;
      const n, h: TBigInteger; const seed: TCryptoLibByteArray); overload;

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

class function TECDomainParameters.Validate(const c: IECCurve;
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

class procedure TECDomainParameters.Boot;
begin
  if FLock = Nil then
  begin
    FLock := TCriticalSection.Create;
  end;
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

  // we can't check for (not (h.IsInitialized)) here as h is optional in X9.62 as it is not required for ECDSA

  Fcurve := curve;
  Fg := Validate(curve, g);
  Fn := n;
  Fh := h;
  FhInv := Default (TBigInteger);

  Fseed := System.Copy(seed);

end;

class constructor TECDomainParameters.CreateECDomainParameters;
begin
  TECDomainParameters.Boot;
end;

class destructor TECDomainParameters.DestroyECDomainParameters;
begin
  FLock.Free;
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
    n.Equals(other.n) and h.Equals(other.h);

end;

function TECDomainParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := curve.GetHashCode();
  result := result * 37;
  result := result xor g.GetHashCode();
  result := result * 37;
  result := result xor n.GetHashCode();
  result := result * 37;
  result := result xor h.GetHashCode();
end;

end.
