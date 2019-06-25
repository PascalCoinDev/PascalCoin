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

unit ClpIDHDomainParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHValidationParams,
  ClpIAsn1Objects;

type
  IDHDomainParameters = interface(IAsn1Encodable)
    ['{18288135-B71F-48B4-8595-57AAB9092FC8}']

    function GetP: IDerInteger;
    property p: IDerInteger read GetP;

    function GetG: IDerInteger;
    property g: IDerInteger read GetG;

    function GetQ: IDerInteger;
    property q: IDerInteger read GetQ;

    function GetJ: IDerInteger;
    property j: IDerInteger read GetJ;

    function GetValidationParams: IDHValidationParams;
    property validationParams: IDHValidationParams read GetValidationParams;

  end;

implementation

end.
