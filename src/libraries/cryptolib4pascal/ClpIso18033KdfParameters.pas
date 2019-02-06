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

unit ClpIso18033KdfParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIIso18033KdfParameters,
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// parameters for Key derivation functions for ISO-18033
  /// </summary>
  TIso18033KdfParameters = class(TInterfacedObject, IIso18033KdfParameters,
    IDerivationParameters)

  strict private
    Fseed: TCryptoLibByteArray;

  public
    function GetSeed(): TCryptoLibByteArray; inline;

    constructor Create(const seed: TCryptoLibByteArray);
  end;

implementation

{ TIso18033KdfParameters }

constructor TIso18033KdfParameters.Create(const seed: TCryptoLibByteArray);
begin
  Inherited Create();
  Fseed := seed;
end;

function TIso18033KdfParameters.GetSeed: TCryptoLibByteArray;
begin
  result := Fseed;
end;

end.
