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

unit ClpIPolynomialExtensionField;

{$I CryptoLib.inc}

interface

uses
  ClpIPolynomial,
  ClpIExtensionField;

type

  IPolynomialExtensionField = interface(IExtensionField)
    ['{40B4388E-7014-4E15-9E93-9F5173F7F7E0}']

    function GetMinimalPolynomial: IPolynomial;
    property MinimalPolynomial: IPolynomial read GetMinimalPolynomial;

  end;

implementation

end.
