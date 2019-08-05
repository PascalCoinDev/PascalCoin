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

unit ClpIECKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIECDomainParameters,
  ClpIKeyGenerationParameters;

type
  IECKeyGenerationParameters = interface(IKeyGenerationParameters)
    ['{B9343CA3-9274-4812-9FFC-2CC27486261E}']

    function GetDomainParameters: IECDomainParameters;
    property domainParameters: IECDomainParameters read GetDomainParameters;
  end;

implementation

end.
