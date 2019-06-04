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

unit ClpEd25519Blake2B;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpEd25519,
  ClpDigestUtilities,
  ClpIEd25519Blake2B;

type
  TEd25519Blake2B = class sealed(TEd25519, IEd25519Blake2B)

  strict protected

    function CreateDigest(): IDigest; override;

  end;

implementation

{ TEd25519Blake2B }

function TEd25519Blake2B.CreateDigest: IDigest;
begin
  result := TDigestUtilities.GetDigest('BLAKE2B-512');
end;

end.
