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

unit ClpGlvTypeBParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIGlvTypeBParameters,
  ClpCryptoLibTypes;

type
  TGlvTypeBParameters = class sealed(TInterfacedObject, IGlvTypeBParameters)

  strict private
    function GetBeta: TBigInteger; inline;
    function GetBits: Int32; inline;
    function GetG1: TBigInteger; inline;
    function GetG2: TBigInteger; inline;
    function GetLambda: TBigInteger; inline;
    function GetV1: TCryptoLibGenericArray<TBigInteger>; inline;
    function GetV2: TCryptoLibGenericArray<TBigInteger>; inline;
  strict protected
    Fm_beta, Fm_lambda: TBigInteger;
    Fm_v1, Fm_v2: TCryptoLibGenericArray<TBigInteger>;
    Fm_g1, Fm_g2: TBigInteger;
    Fm_bits: Int32;

  public
    constructor Create(const beta, lambda: TBigInteger;
      const v1, v2: TCryptoLibGenericArray<TBigInteger>;
      const g1, g2: TBigInteger; bits: Int32);

    destructor Destroy; override;

    property beta: TBigInteger read GetBeta;
    property lambda: TBigInteger read GetLambda;
    property v1: TCryptoLibGenericArray<TBigInteger> read GetV1;
    property v2: TCryptoLibGenericArray<TBigInteger> read GetV2;
    property g1: TBigInteger read GetG1;
    property g2: TBigInteger read GetG2;
    property bits: Int32 read GetBits;

  end;

implementation

{ TGlvTypeBParameters }

constructor TGlvTypeBParameters.Create(const beta, lambda: TBigInteger;
  const v1, v2: TCryptoLibGenericArray<TBigInteger>; const g1, g2: TBigInteger;
  bits: Int32);
begin
  Fm_beta := beta;
  Fm_lambda := lambda;
  Fm_v1 := v1;
  Fm_v2 := v2;
  Fm_g1 := g1;
  Fm_g2 := g2;
  Fm_bits := bits;
end;

destructor TGlvTypeBParameters.Destroy;
begin
  inherited Destroy;
end;

function TGlvTypeBParameters.GetBeta: TBigInteger;
begin
  Result := Fm_beta;
end;

function TGlvTypeBParameters.GetBits: Int32;
begin
  Result := Fm_bits;
end;

function TGlvTypeBParameters.GetG1: TBigInteger;
begin
  Result := Fm_g1;
end;

function TGlvTypeBParameters.GetG2: TBigInteger;
begin
  Result := Fm_g2;
end;

function TGlvTypeBParameters.GetLambda: TBigInteger;
begin
  Result := Fm_lambda;
end;

function TGlvTypeBParameters.GetV1: TCryptoLibGenericArray<TBigInteger>;
begin
  Result := Fm_v1;
end;

function TGlvTypeBParameters.GetV2: TCryptoLibGenericArray<TBigInteger>;
begin
  Result := Fm_v2;
end;

end.
