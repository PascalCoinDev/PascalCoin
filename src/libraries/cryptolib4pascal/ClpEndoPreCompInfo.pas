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

unit ClpEndoPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIPreCompInfo,
  ClpIEndoPreCompInfo;

type
  TEndoPreCompInfo = class sealed(TInterfacedObject, IPreCompInfo,
    IEndoPreCompInfo)

  strict private
  var
    FEndomorphism: IECEndomorphism;
    FMappedPoint: IECPoint;

    function GetEndomorphism: IECEndomorphism; inline;
    procedure SetEndomorphism(const value: IECEndomorphism); inline;

    function GetMappedPoint: IECPoint; inline;
    procedure SetMappedPoint(const value: IECPoint); inline;

  public

    property Endomorphism: IECEndomorphism read GetEndomorphism
      write SetEndomorphism;
    property MappedPoint: IECPoint read GetMappedPoint write SetMappedPoint;
  end;

implementation

{ TEndoPreCompInfo }

function TEndoPreCompInfo.GetEndomorphism: IECEndomorphism;
begin
  result := FEndomorphism;
end;

function TEndoPreCompInfo.GetMappedPoint: IECPoint;
begin
  result := FMappedPoint;
end;

procedure TEndoPreCompInfo.SetEndomorphism(const value: IECEndomorphism);
begin
  FEndomorphism := value;
end;

procedure TEndoPreCompInfo.SetMappedPoint(const value: IECPoint);
begin
  FMappedPoint := value;
end;

end.
