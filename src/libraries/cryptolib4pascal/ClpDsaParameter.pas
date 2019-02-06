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

unit ClpDsaParameter;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIDsaParameter,
  ClpAsn1Objects,
  ClpIAsn1Objects,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SBadSequenceSize = 'Bad Sequence Size "seq": %d';
  SInvalidDsaParameter = 'Invalid DsaParameter: %s';

type
  TDsaParameter = class(TAsn1Encodable, IDsaParameter)

  strict private
  var
    Fp, Fq, Fg: IDerInteger;

    function GetG: TBigInteger; inline;
    function GetP: TBigInteger; inline;
    function GetQ: TBigInteger; inline;

    constructor Create(const seq: IAsn1Sequence); overload;

  public
    constructor Create(const p, q, g: TBigInteger); overload;

    function ToAsn1Object(): IAsn1Object; override;

    property p: TBigInteger read GetP;
    property q: TBigInteger read GetQ;
    property g: TBigInteger read GetG;

    class function GetInstance(const obj: IAsn1TaggedObject;
      explicitly: Boolean): IDsaParameter; overload; static; inline;

    class function GetInstance(obj: TObject): IDsaParameter; overload;
      static; inline;

  end;

implementation

{ TDsaParameter }

function TDsaParameter.GetP: TBigInteger;
begin
  result := Fp.PositiveValue;
end;

function TDsaParameter.GetQ: TBigInteger;
begin
  result := Fq.PositiveValue;
end;

function TDsaParameter.GetG: TBigInteger;
begin
  result := Fg.PositiveValue;
end;

function TDsaParameter.ToAsn1Object: IAsn1Object;
begin
  result := TDerSequence.Create([Fp, Fq, Fg]);
end;

constructor TDsaParameter.Create(const seq: IAsn1Sequence);
begin
  Inherited Create();
  if (seq.Count <> 3) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SBadSequenceSize,
      [seq.Count]);
  end;

  Fp := TDerInteger.GetInstance(seq[0] as TAsn1Encodable);
  Fq := TDerInteger.GetInstance(seq[1] as TAsn1Encodable);
  Fg := TDerInteger.GetInstance(seq[2] as TAsn1Encodable);
end;

constructor TDsaParameter.Create(const p, q, g: TBigInteger);
begin
  Inherited Create();
  Fp := TDerInteger.Create(p);
  Fq := TDerInteger.Create(q);
  Fg := TDerInteger.Create(g);
end;

class function TDsaParameter.GetInstance(obj: TObject): IDsaParameter;
begin
  if ((obj = Nil) or (obj is TDsaParameter)) then
  begin
    result := obj as TDsaParameter;
    Exit;
  end;

  if (obj is TAsn1Sequence) then
  begin
    result := TDsaParameter.Create(obj as TAsn1Sequence);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SInvalidDsaParameter,
    [obj.ToString]);
end;

class function TDsaParameter.GetInstance(const obj: IAsn1TaggedObject;
  explicitly: Boolean): IDsaParameter;
begin
  result := GetInstance(TAsn1Sequence.GetInstance(obj, explicitly)
    as TAsn1Sequence);
end;

end.
