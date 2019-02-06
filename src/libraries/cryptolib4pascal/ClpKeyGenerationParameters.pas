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

unit ClpKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIKeyGenerationParameters,
  ClpCryptoLibTypes,
  ClpISecureRandom;

resourcestring
  SRandomNil = 'Random Class Instance Can''t Be Nil';
  SInvalidStrength = 'Strength Must Be Greater Than Zero But Was Given %d';

type
  /// <summary>
  /// The base class for parameters to key generators.
  /// </summary>
  TKeyGenerationParameters = class(TInterfacedObject, IKeyGenerationParameters)

  strict private
  var
    FRandom: ISecureRandom;
    FStrength: Int32;

  strict protected

    /// <returns>
    /// return the generators random source.
    /// </returns>
    function GetRandom: ISecureRandom;

    /// <returns>
    /// return the strength of the keys this generator produces (in bits).
    /// </returns>
    function GetStrength: Int32;

  public

    /// <value>
    /// return the random source associated with this generator.
    /// </value>
    property Random: ISecureRandom read GetRandom;

    /// <value>
    /// return the bit strength for keys produced by this generator.
    /// </value>
    property Strength: Int32 read GetStrength;

    /// <summary>
    /// initialise the generator with a source of randomness and a strength
    /// (in bits).
    /// </summary>
    /// <param name="Random">
    /// the random byte source.
    /// </param>
    /// <param name="Strength">
    /// the size, in bits, of the keys we want to produce.
    /// </param>
    constructor Create(const Random: ISecureRandom; Strength: Int32);

  end;

implementation

{ TKeyGenerationParameters }

constructor TKeyGenerationParameters.Create(const Random: ISecureRandom;
  Strength: Int32);
begin
  if (Random = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SRandomNil);

  if (Strength < 1) then
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidStrength,
      [Strength]);

  FRandom := Random;
  FStrength := Strength;
end;

function TKeyGenerationParameters.GetRandom: ISecureRandom;
begin
  Result := FRandom;
end;

function TKeyGenerationParameters.GetStrength: Int32;
begin
  Result := FStrength;
end;

end.
