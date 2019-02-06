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

unit ClpIX9ECParametersHolder;

{$I CryptoLib.inc}

interface

uses
  ClpIX9ECParameters;

type
  IX9ECParametersHolder = interface(IInterface)
    ['{F24A1BB5-8A39-45A4-9BEF-68DD0EE79E0D}']

    function CreateParameters(): IX9ECParameters;
    function GetParameters: IX9ECParameters;
    property Parameters: IX9ECParameters read GetParameters;

  end;

implementation

end.
