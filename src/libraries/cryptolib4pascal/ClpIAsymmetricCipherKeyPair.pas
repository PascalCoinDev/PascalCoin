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

unit ClpIAsymmetricCipherKeyPair;

{$I CryptoLib.inc}

interface

uses
  ClpIAsymmetricKeyParameter;

type
  IAsymmetricCipherKeyPair = interface(IInterface)
    ['{66AD23CF-CE95-49C3-B9F0-83153174E9D7}']

    function GetPrivate: IAsymmetricKeyParameter;
    function GetPublic: IAsymmetricKeyParameter;

    /// <summary>
    /// return the public key parameters.
    /// </summary>
    property &Public: IAsymmetricKeyParameter read GetPublic;

    /// <summary>
    /// return the private key parameters.
    /// </summary>
    property &Private: IAsymmetricKeyParameter read GetPrivate;

  end;

implementation

end.
