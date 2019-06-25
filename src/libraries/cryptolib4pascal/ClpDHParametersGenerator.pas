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

unit ClpDHParametersGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpDHParameters,
  ClpIDHParameters,
  ClpIDHParametersGenerator,
  ClpDHParametersHelper,
  ClpBigInteger,
  ClpCryptoLibTypes;

type
  TDHParametersGenerator = class(TInterfacedObject, IDHParametersGenerator)

  strict private
  var
    Fcertainty, Fsize: Int32;
    Frandom: ISecureRandom;

  public

    procedure Init(size, certainty: Int32; const random: ISecureRandom);

    /// <summary>
    /// <para>
    /// which Generates the p and g values from the given parameters,
    /// returning the DHParameters object.
    /// </para>
    /// <para>
    /// Note: can take a while...
    /// </para>
    /// </summary>
    function GenerateParameters(): IDHParameters; virtual;

  end;

implementation

{ TDHParametersGenerator }

function TDHParametersGenerator.GenerateParameters: IDHParameters;
var
  safePrimes: TCryptoLibGenericArray<TBigInteger>;
  p, q, g: TBigInteger;
begin
  //
  // find a safe prime p where p = 2*q + 1, where p and q are prime.
  //
  safePrimes := TDHParametersHelper.GenerateSafePrimes(Fsize,
    Fcertainty, Frandom);

  p := safePrimes[0];
  q := safePrimes[1];
  g := TDHParametersHelper.SelectGenerator(p, q, Frandom);

  result := TDHParameters.Create(p, g, q, TBigInteger.Two, Nil);
end;

procedure TDHParametersGenerator.Init(size, certainty: Int32;
  const random: ISecureRandom);
begin
  Fsize := size;
  Fcertainty := certainty;
  Frandom := random;
end;

end.
