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

unit ClpIDsaKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaParameters,
  ClpIAsymmetricKeyParameter;

type
  IDsaKeyParameters = interface(IAsymmetricKeyParameter)
    ['{1E3454DF-DC9F-4EA0-91DA-0768A77387C5}']

    function GetParameters: IDsaParameters;

    function Equals(const other: IDsaKeyParameters): Boolean;
    property parameters: IDsaParameters read GetParameters;

  end;

implementation

end.
