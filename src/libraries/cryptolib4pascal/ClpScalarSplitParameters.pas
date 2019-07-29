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

unit ClpScalarSplitParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIScalarSplitParameters,
  ClpCryptoLibTypes;

resourcestring
  SInvalidParameters = '"%s" must consist of exactly 2 (initialized) values';

type
  TScalarSplitParameters = class sealed(TInterfacedObject,
    IScalarSplitParameters)

  strict private
    function GetG1: TBigInteger; inline;
    function GetG2: TBigInteger; inline;
    function GetV1A: TBigInteger; inline;
    function GetV1B: TBigInteger; inline;
    function GetV2A: TBigInteger; inline;
    function GetV2B: TBigInteger; inline;
    function GetBits: Int32; inline;

    class procedure CheckVector(const v: TCryptoLibGenericArray<TBigInteger>;
      const name: String); static;

  strict protected
    Fg1, Fg2, Fv1A, Fv1B, Fv2A, Fv2B: TBigInteger;
    Fbits: Int32;

  public
    constructor Create(const v1, v2: TCryptoLibGenericArray<TBigInteger>;
      const g1, g2: TBigInteger; bits: Int32);

    property g1: TBigInteger read GetG1;
    property g2: TBigInteger read GetG2;
    property V1A: TBigInteger read GetV1A;
    property V1B: TBigInteger read GetV1B;
    property V2A: TBigInteger read GetV2A;
    property V2B: TBigInteger read GetV2B;
    property bits: Int32 read GetBits;

  end;

implementation

{ TScalarSplitParameters }

class procedure TScalarSplitParameters.CheckVector
  (const v: TCryptoLibGenericArray<TBigInteger>; const name: String);
begin
  if ((v = Nil) or (System.length(v) <> 2) or (not v[0].IsInitialized) or
    (not v[1].IsInitialized)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidParameters, [name]);
  end;
end;

constructor TScalarSplitParameters.Create(const v1,
  v2: TCryptoLibGenericArray<TBigInteger>; const g1, g2: TBigInteger;
  bits: Int32);
begin
  CheckVector(v1, 'v1');
  CheckVector(v2, 'v2');

  Fv1A := v1[0];
  Fv1B := v1[1];
  Fv2A := v2[0];
  Fv2B := v2[1];
  Fg1 := g1;
  Fg2 := g2;
  Fbits := bits;
end;

function TScalarSplitParameters.GetG1: TBigInteger;
begin
  Result := Fg1;
end;

function TScalarSplitParameters.GetG2: TBigInteger;
begin
  Result := Fg2;
end;

function TScalarSplitParameters.GetV1A: TBigInteger;
begin
  Result := Fv1A;
end;

function TScalarSplitParameters.GetV1B: TBigInteger;
begin
  Result := Fv1B;
end;

function TScalarSplitParameters.GetV2A: TBigInteger;
begin
  Result := Fv2A;
end;

function TScalarSplitParameters.GetV2B: TBigInteger;
begin
  Result := Fv2B;
end;

function TScalarSplitParameters.GetBits: Int32;
begin
  Result := Fbits;
end;

end.
