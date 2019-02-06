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

unit ClpIExtensionField;

{$I CryptoLib.inc}

interface

uses
  ClpIFiniteField;

type

  IExtensionField = interface(IFiniteField)
    ['{262CB0BB-E070-4B0A-971B-33F4357BC4B5}']

    function GetDegree: Int32;
    function GetSubfield: IFiniteField;
    property Degree: Int32 read GetDegree;
    property Subfield: IFiniteField read GetSubfield;

  end;

implementation

end.
