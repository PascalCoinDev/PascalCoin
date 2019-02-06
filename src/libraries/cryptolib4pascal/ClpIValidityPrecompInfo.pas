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

unit ClpIValidityPrecompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIPreCompInfo;

type
  IValidityPrecompInfo = interface(IPreCompInfo)

    ['{2339F5CA-A4B3-4E95-B358-4D4F4CA97EB3}']

    function HasFailed(): Boolean;
    procedure ReportFailed();
    function HasCurveEquationPassed(): Boolean;
    procedure ReportCurveEquationPassed();
    function HasOrderPassed(): Boolean;
    procedure ReportOrderPassed();

  end;

implementation

end.
