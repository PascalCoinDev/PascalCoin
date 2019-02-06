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

unit ClpIFiniteField;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger;

type

  IFiniteField = interface(IInterface)
    ['{54DEBFF8-A0DF-4406-8F0C-FB1D0BD2619B}']

    function GetCharacteristic: TBigInteger;
    function GetDimension: Int32;
    property Characteristic: TBigInteger read GetCharacteristic;
    property Dimension: Int32 read GetDimension;

  end;

implementation

end.
