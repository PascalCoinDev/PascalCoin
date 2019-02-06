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

unit ClpIFixedPointPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIECC,
  ClpIPreCompInfo;

type
  IFixedPointPreCompInfo = interface(IPreCompInfo)
    ['{FD2E7BE8-D353-4229-981A-744A50EE9F7F}']

    function GetWidth: Int32;
    procedure SetWidth(const Value: Int32);
    function GetLookupTable: IECLookupTable;
    procedure SetLookupTable(const Value: IECLookupTable);
    function GetOffset: IECPoint;
    procedure SetOffset(const Value: IECPoint);

    property Offset: IECPoint read GetOffset write SetOffset;

    property LookupTable: IECLookupTable read GetLookupTable
      write SetLookupTable;
    property Width: Int32 read GetWidth write SetWidth;

  end;

implementation

end.
