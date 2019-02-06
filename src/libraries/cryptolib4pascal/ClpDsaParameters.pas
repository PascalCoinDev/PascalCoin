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

unit ClpDsaParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIDsaParameters,
  ClpIDsaValidationParameters,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SPUnInitialized = '"P" Cannot Be Uninitialized';
  SQUnInitialized = '"Q" Cannot Be Uninitialized';
  SGUnInitialized = '"G" Cannot Be Uninitialized';

type
  TDsaParameters = class(TInterfacedObject, ICipherParameters, IDsaParameters)

  strict private
  var
    Fp, Fq, Fg: TBigInteger;
    Fvalidation: IDsaValidationParameters;

    function GetG: TBigInteger; inline;
    function GetP: TBigInteger; inline;
    function GetQ: TBigInteger; inline;
    function GetValidationParameters: IDsaValidationParameters; inline;

  public

    /// <summary>
    /// Creates a new DSAParameter with the specified parameter values.
    /// </summary>
    /// <param name="p">
    /// the prime.
    /// </param>
    /// <param name="q">
    /// the sub-prime.
    /// </param>
    /// <param name="g">
    /// the base.
    /// </param>
    constructor Create(const p, q, g: TBigInteger); overload;
    /// <summary>
    /// Creates a new DSAParameter with the specified parameter values.
    /// </summary>
    /// <param name="p">
    /// the prime.
    /// </param>
    /// <param name="q">
    /// the sub-prime.
    /// </param>
    /// <param name="g">
    /// the base.
    /// </param>
    /// <param name="parameters">
    /// dsa validation parameters (this includes the seed, counter and usage
    /// index)
    /// </param>
    constructor Create(const p, q, g: TBigInteger;
      const parameters: IDsaValidationParameters); overload;

    function Equals(const other: IDsaParameters): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property p: TBigInteger read GetP;
    property q: TBigInteger read GetQ;
    property g: TBigInteger read GetG;
    property ValidationParameters: IDsaValidationParameters
      read GetValidationParameters;

  end;

implementation

{ TDsaParameters }

function TDsaParameters.GetG: TBigInteger;
begin
  result := Fg;
end;

function TDsaParameters.GetP: TBigInteger;
begin
  result := Fp;
end;

function TDsaParameters.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TDsaParameters.GetValidationParameters: IDsaValidationParameters;
begin
  result := Fvalidation;
end;

constructor TDsaParameters.Create(const p, q, g: TBigInteger);
begin
  Create(p, q, g, Nil);
end;

constructor TDsaParameters.Create(const p, q, g: TBigInteger;
  const parameters: IDsaValidationParameters);
begin
  Inherited Create();
  if (not(p.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPUnInitialized);
  end;

  if (not(q.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SQUnInitialized);
  end;

  if (not(g.IsInitialized)) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SGUnInitialized);
  end;

  Fp := p;
  Fq := q;
  Fg := g;
  Fvalidation := parameters;
end;

function TDsaParameters.Equals(const other: IDsaParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDsaParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := p.Equals(other.p) and q.Equals(other.q) and g.Equals(other.g);
end;

function TDsaParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := p.GetHashCode() xor q.GetHashCode() xor g.GetHashCode();
end;

end.
