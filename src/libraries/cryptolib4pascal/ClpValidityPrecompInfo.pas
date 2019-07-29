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

unit ClpValidityPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIPreCompInfo,
  ClpIValidityPreCompInfo;

type
  TValidityPreCompInfo = class(TInterfacedObject, IPreCompInfo,
    IValidityPreCompInfo)

  strict private

  var
    Ffailed, FcurveEquationPassed, ForderPassed: Boolean;

  public

    const
    PRECOMP_NAME = 'bc_validity';

    function HasFailed(): Boolean; inline;
    procedure ReportFailed(); inline;
    function HasCurveEquationPassed(): Boolean; inline;
    procedure ReportCurveEquationPassed(); inline;
    function HasOrderPassed(): Boolean; inline;
    procedure ReportOrderPassed(); inline;

    constructor Create();

  end;

implementation

{ TValidityPreCompInfo }

constructor TValidityPreCompInfo.Create;
begin
  Inherited Create();
  Ffailed := False;
  FcurveEquationPassed := False;
  ForderPassed := False;
end;

function TValidityPreCompInfo.HasCurveEquationPassed: Boolean;
begin
  result := FcurveEquationPassed;
end;

function TValidityPreCompInfo.HasFailed: Boolean;
begin
  result := Ffailed;
end;

function TValidityPreCompInfo.HasOrderPassed: Boolean;
begin
  result := ForderPassed;
end;

procedure TValidityPreCompInfo.ReportCurveEquationPassed;
begin
  FcurveEquationPassed := True;
end;

procedure TValidityPreCompInfo.ReportFailed;
begin
  Ffailed := True;
end;

procedure TValidityPreCompInfo.ReportOrderPassed;
begin
  ForderPassed := True;
end;

end.
