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

unit ClpIDHKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIAsn1Objects,
  ClpIAsymmetricKeyParameter;

type
  IDHKeyParameters = interface(IAsymmetricKeyParameter)
    ['{53834D98-B75A-4607-BA38-3CD9DE3B3CF4}']

    function GetParameters: IDHParameters;
    function GetAlgorithmOid: IDerObjectIdentifier;

    function Equals(const other: IDHKeyParameters): Boolean; overload;
    property parameters: IDHParameters read GetParameters;
    property AlgorithmOid: IDerObjectIdentifier read GetAlgorithmOid;

  end;

implementation

end.
