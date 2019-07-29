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
  ClpIScalarSplitParameters,
  ClpCryptoLibTypes;

type
  TGlvTypeBParameters = class sealed(TInterfacedObject, IGlvTypeBParameters)

  strict private
  var
    Fbeta, Flambda: TBigInteger;
    FsplitParams: IScalarSplitParameters;

    function GetLambda: TBigInteger; inline;
    function GetBeta: TBigInteger; inline;
    function GetSplitParams: IScalarSplitParameters; inline;

  public

    constructor Create(const beta, lambda: TBigInteger;
      const splitParams: IScalarSplitParameters);

    property lambda: TBigInteger read GetLambda;
    property beta: TBigInteger read GetBeta;
    property splitParams: IScalarSplitParameters read GetSplitParams;

  end;

implementation

{ TGlvTypeBParameters }

constructor TGlvTypeBParameters.Create(const beta, lambda: TBigInteger;
  const splitParams: IScalarSplitParameters);
begin
  Inherited Create();
  Fbeta := beta;
  Flambda := lambda;
  FsplitParams := splitParams;
end;

function TGlvTypeBParameters.GetBeta: TBigInteger;
begin
  Result := Fbeta;
end;

function TGlvTypeBParameters.GetLambda: TBigInteger;
begin
  Result := Flambda;
end;

function TGlvTypeBParameters.GetSplitParams: IScalarSplitParameters;
begin
  Result := FsplitParams;
end;

end.
