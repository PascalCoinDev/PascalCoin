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

unit ClpValidityPrecompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIPreCompInfo,
  ClpIValidityPrecompInfo;

type
  TValidityPrecompInfo = class(TInterfacedObject, IPreCompInfo,
    IValidityPrecompInfo)

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

{ TValidityPrecompInfo }

constructor TValidityPrecompInfo.Create;
begin
  Inherited Create();
  Ffailed := False;
  FcurveEquationPassed := False;
  ForderPassed := False;
end;

function TValidityPrecompInfo.HasCurveEquationPassed: Boolean;
begin
  result := FcurveEquationPassed;
end;

function TValidityPrecompInfo.HasFailed: Boolean;
begin
  result := Ffailed;
end;

function TValidityPrecompInfo.HasOrderPassed: Boolean;
begin
  result := ForderPassed;
end;

procedure TValidityPrecompInfo.ReportCurveEquationPassed;
begin
  FcurveEquationPassed := True;
end;

procedure TValidityPrecompInfo.ReportFailed;
begin
  Ffailed := True;
end;

procedure TValidityPrecompInfo.ReportOrderPassed;
begin
  ForderPassed := True;
end;

end.
