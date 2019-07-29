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

unit ClpGlvTypeAParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIGlvTypeAParameters,
  ClpIScalarSplitParameters,
  ClpCryptoLibTypes;

type
  TGlvTypeAParameters = class sealed(TInterfacedObject, IGlvTypeAParameters)

  strict private
  var
    FI, Flambda: TBigInteger;
    FsplitParams: IScalarSplitParameters;

    function GetLambda: TBigInteger; inline;
    function GetI: TBigInteger; inline;
    function GetSplitParams: IScalarSplitParameters; inline;

  public

    constructor Create(const I, lambda: TBigInteger;
      const splitParams: IScalarSplitParameters);

    property lambda: TBigInteger read GetLambda;
    property I: TBigInteger read GetI;
    property splitParams: IScalarSplitParameters read GetSplitParams;

  end;

implementation

{ TGlvTypeAParameters }

constructor TGlvTypeAParameters.Create(const I, lambda: TBigInteger;
  const splitParams: IScalarSplitParameters);
begin
  Inherited Create();
  FI := I;
  Flambda := lambda;
  FsplitParams := splitParams;
end;

function TGlvTypeAParameters.GetI: TBigInteger;
begin
  Result := FI;
end;

function TGlvTypeAParameters.GetLambda: TBigInteger;
begin
  Result := Flambda;
end;

function TGlvTypeAParameters.GetSplitParams: IScalarSplitParameters;
begin
  Result := FsplitParams;
end;

end.
