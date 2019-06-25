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

unit ClpDHDomainParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHDomainParameters,
  ClpIDHValidationParams,
  ClpDHValidationParams,
  ClpAsn1Objects,
  ClpIAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SPNil = 'P Cannot be Nil';
  SGNil = 'G Cannot be Nil';
  SQNil = 'Q Cannot be Nil';
  SJNil = 'J Cannot be Nil';
  SBadSequenceSize = 'Bad Sequence Size "seq": %d';
  SInvalidDHDomainParameters = 'Invalid DHDomainParameters: %s';

type
  TDHDomainParameters = class(TAsn1Encodable, IDHDomainParameters)

  strict private
  var
    Fp, Fg, Fq, Fj: IDerInteger;
    FvalidationParams: IDHValidationParams;

    function GetP: IDerInteger; inline;
    function GetG: IDerInteger; inline;
    function GetQ: IDerInteger; inline;
    function GetJ: IDerInteger; inline;
    function GetValidationParams: IDHValidationParams; inline;

    constructor Create(const seq: IAsn1Sequence); overload;

    class function GetNext(const e: TCryptoLibGenericArray<IAsn1Encodable>;
      var Idx: Int32): IAsn1Encodable; static; inline;

  public
    constructor Create(const p, g, q, j: IDerInteger;
      const validationParams: IDHValidationParams); overload;

    function ToAsn1Object(): IAsn1Object; override;

    property p: IDerInteger read GetP;

    property g: IDerInteger read GetG;

    property q: IDerInteger read GetQ;

    property j: IDerInteger read GetJ;

    property validationParams: IDHValidationParams read GetValidationParams;

    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDHDomainParameters; overload; static; inline;

    class function GetInstance(obj: TObject): IDHDomainParameters; overload;
      static; inline;

  end;

implementation

{ TDHDomainParameters }

function TDHDomainParameters.GetP: IDerInteger;
begin
  result := Fp;
end;

function TDHDomainParameters.GetG: IDerInteger;
begin
  result := Fg;
end;

function TDHDomainParameters.GetJ: IDerInteger;
begin
  result := Fj;
end;

function TDHDomainParameters.GetQ: IDerInteger;
begin
  result := Fq;
end;

function TDHDomainParameters.GetValidationParams: IDHValidationParams;
begin
  result := FvalidationParams;
end;

class function TDHDomainParameters.GetNext
  (const e: TCryptoLibGenericArray<IAsn1Encodable>; var Idx: Int32)
  : IAsn1Encodable;
begin
  if Idx <= (System.Length(e) - 1) then
  begin
    result := e[Idx];
    System.Inc(Idx);
  end
  else
  begin
    result := Nil;
  end;
end;

constructor TDHDomainParameters.Create(const seq: IAsn1Sequence);
var
  e: TCryptoLibGenericArray<IAsn1Encodable>;
  next: IAsn1Encodable;
  Idx: Int32;
begin
  Inherited Create();
  if (seq.Count < 3) or (seq.Count > 5) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SBadSequenceSize,
      [seq.Count]);
  end;

  Idx := 0;
  e := seq.GetEnumerable();

  Fp := TDerInteger.GetInstance(GetNext(e, Idx) as TAsn1Encodable);

  Fg := TDerInteger.GetInstance(GetNext(e, Idx) as TAsn1Encodable);

  Fq := TDerInteger.GetInstance(GetNext(e, Idx) as TAsn1Encodable);

  next := GetNext(e, Idx);

  if ((next <> Nil) and ((next as TAsn1Encodable) is TDerInteger)) then
  begin
    Fj := TDerInteger.GetInstance(next as TAsn1Encodable);
    next := GetNext(e, Idx);
  end;

  if (next <> Nil) then
  begin
    FvalidationParams := TDHValidationParams.GetInstance
      (next.ToAsn1Object() as TAsn1Object);
  end;

end;

constructor TDHDomainParameters.Create(const p, g, q, j: IDerInteger;
  const validationParams: IDHValidationParams);
begin
  Inherited Create();

  if (p = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPNil);
  end;

  if (g = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SGNil);
  end;

  if (q = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SQNil);
  end;

  if (j = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SJNil);
  end;

  Fp := p;
  Fg := g;
  Fq := q;
  Fj := j;
  FvalidationParams := validationParams;
end;

class function TDHDomainParameters.GetInstance(obj: TObject)
  : IDHDomainParameters;
begin
  if ((obj = Nil) or (obj is TDHDomainParameters)) then
  begin
    result := obj as TDHDomainParameters;
    Exit;
  end;

  if (obj is TAsn1Sequence) then
  begin
    result := TDHDomainParameters.Create(obj as TAsn1Sequence);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SInvalidDHDomainParameters,
    [obj.ToString]);
end;

class function TDHDomainParameters.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDHDomainParameters;
begin
  result := GetInstance(TAsn1Sequence.GetInstance(obj, isExplicit)
    as TAsn1Sequence);
end;

function TDHDomainParameters.ToAsn1Object: IAsn1Object;
var
  v: IAsn1EncodableVector;
begin
  v := TAsn1EncodableVector.Create([p, g, q]);

  if (Fj <> Nil) then
  begin
    v.Add([Fj]);
  end;

  if (FvalidationParams <> Nil) then
  begin
    v.Add([FvalidationParams]);
  end;

  result := TDerSequence.Create(v);
end;

end.
