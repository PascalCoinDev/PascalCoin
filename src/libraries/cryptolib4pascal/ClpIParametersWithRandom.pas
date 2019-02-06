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

unit ClpIParametersWithRandom;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpICipherParameters;

type
  IParametersWithRandom = interface(ICipherParameters)

    ['{7528E638-E8DD-4B5E-ADF9-9495A9507087}']

    function GetRandom: ISecureRandom;
    function GetParameters: ICipherParameters;

    property random: ISecureRandom read GetRandom;

    property parameters: ICipherParameters read GetParameters;

  end;

implementation

end.
