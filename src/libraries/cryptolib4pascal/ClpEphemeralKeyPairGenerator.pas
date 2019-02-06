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

unit ClpEphemeralKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpEphemeralKeyPair,
  ClpIEphemeralKeyPair,
  ClpIEphemeralKeyPairGenerator,
  ClpIAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPairGenerator,
  ClpIKeyEncoder;

type
  TEphemeralKeyPairGenerator = class sealed(TInterfacedObject,
    IEphemeralKeyPairGenerator)

  strict private
  var
    Fgen: IAsymmetricCipherKeyPairGenerator;
    FkeyEncoder: IKeyEncoder;

  public
    function Generate(): IEphemeralKeyPair; inline;
    constructor Create(const gen: IAsymmetricCipherKeyPairGenerator;
      const keyEncoder: IKeyEncoder);
  end;

implementation

{ TEphemeralKeyPairGenerator }

constructor TEphemeralKeyPairGenerator.Create
  (const gen: IAsymmetricCipherKeyPairGenerator; const keyEncoder: IKeyEncoder);
begin
  Inherited Create();
  Fgen := gen;
  FkeyEncoder := keyEncoder;
end;

function TEphemeralKeyPairGenerator.Generate: IEphemeralKeyPair;
var
  eph: IAsymmetricCipherKeyPair;
begin
  eph := Fgen.generateKeyPair();
  // Encode the ephemeral public key
  result := TEphemeralKeyPair.Create(eph, FkeyEncoder);
end;

end.
