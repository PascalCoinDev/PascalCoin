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

unit ClpDHValidationParams;

{$I CryptoLib.inc}

interface

uses
  ClpIDHValidationParams,
  ClpAsn1Objects,
  ClpIAsn1Objects,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SSeedNil = 'Seed Cannot be Nil';
  SPGenCounterNil = 'PGenCounter Cannot be Nil';
  SBadSequenceSize = 'Bad Sequence Size "seq": %d';
  SInvalidDHValidationParams = 'Invalid DHValidationParams: %s';

type
  TDHValidationParams = class(TAsn1Encodable, IDHValidationParams)

  strict private
  var
    Fseed: IDerBitString;
    FpgenCounter: IDerInteger;

    function GetSeed: IDerBitString; inline;
    function GetPGenCounter: IDerInteger; inline;

    constructor Create(const seq: IAsn1Sequence); overload;

  public
    constructor Create(const seed: IDerBitString;
      const pgenCounter: IDerInteger); overload;

    function ToAsn1Object(): IAsn1Object; override;

    property seed: IDerBitString read GetSeed;

    property pgenCounter: IDerInteger read GetPGenCounter;

    class function GetInstance(const obj: IAsn1TaggedObject;
      isExplicit: Boolean): IDHValidationParams; overload; static; inline;

    class function GetInstance(obj: TObject): IDHValidationParams; overload;
      static; inline;

  end;

implementation

{ TDHValidationParams }

function TDHValidationParams.GetPGenCounter: IDerInteger;
begin
  result := FpgenCounter;
end;

function TDHValidationParams.GetSeed: IDerBitString;
begin
  result := Fseed;
end;

function TDHValidationParams.ToAsn1Object: IAsn1Object;
begin
  result := TDerSequence.Create([Fseed, FpgenCounter]);
end;

constructor TDHValidationParams.Create(const seq: IAsn1Sequence);
begin
  Inherited Create();
  if (seq.Count <> 2) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SBadSequenceSize,
      [seq.Count]);
  end;

  Fseed := TDerBitString.GetInstance(seq[0] as TAsn1Encodable);
  FpgenCounter := TDerInteger.GetInstance(seq[1] as TAsn1Encodable);
end;

constructor TDHValidationParams.Create(const seed: IDerBitString;
  const pgenCounter: IDerInteger);
begin
  Inherited Create();

  if (seed = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SSeedNil);
  end;

  if (pgenCounter = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPGenCounterNil);
  end;

  Fseed := seed;
  FpgenCounter := pgenCounter;
end;

class function TDHValidationParams.GetInstance(obj: TObject)
  : IDHValidationParams;
begin
  if ((obj = Nil) or (obj is TDHValidationParams)) then
  begin
    result := obj as TDHValidationParams;
    Exit;
  end;

  if (obj is TAsn1Sequence) then
  begin
    result := TDHValidationParams.Create(obj as TAsn1Sequence);
    Exit;
  end;

  raise EArgumentCryptoLibException.CreateResFmt(@SInvalidDHValidationParams,
    [obj.ToString]);
end;

class function TDHValidationParams.GetInstance(const obj: IAsn1TaggedObject;
  isExplicit: Boolean): IDHValidationParams;
begin
  result := GetInstance(TAsn1Sequence.GetInstance(obj, isExplicit)
    as TAsn1Sequence);
end;

end.
