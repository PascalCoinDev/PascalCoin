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

unit ClpIDsaParameterGenerationParameters;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpISecureRandom;

type
  IDsaParameterGenerationParameters = interface(IInterface)
    ['{52ACCC72-7FF6-4934-81E5-F616BEB0EE04}']

    function GetL: Int32;
    property L: Int32 read GetL;

    function GetN: Int32;
    property N: Int32 read GetN;

    function GetUsageIndex: Int32;
    property usageIndex: Int32 read GetUsageIndex;

    function GetCertainty: Int32;
    property certainty: Int32 read GetCertainty;

    function GetRandom: ISecureRandom;
    property random: ISecureRandom read GetRandom;
  end;

implementation

end.
