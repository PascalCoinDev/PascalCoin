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

unit ClpIPkcs5S2ParametersGenerator;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIPbeParametersGenerator,
  ClpIDigest;

type
  IPkcs5S2ParametersGenerator = interface(IPbeParametersGenerator)

    ['{AD345DB8-2341-4C56-B401-23444C2A81BA}']

    function GetDigest: IDigest;

    /// <value>
    /// the underlying digest.
    /// </value>
    property digest: IDigest read GetDigest;

  end;

implementation

end.
