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

unit ClpDsaParameterGenerationParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIDsaParameterGenerationParameters;

type
  TDsaParameterGenerationParameters = class(TInterfacedObject,
    IDsaParameterGenerationParameters)

  strict private
  var
    Fl, Fn, Fcertainty, FusageIndex: Int32;
    Frandom: ISecureRandom;

  strict protected

    function GetL: Int32; virtual;
    function GetN: Int32; virtual;
    function GetCertainty: Int32; virtual;
    function GetUsageIndex: Int32; virtual;
    function GetRandom: ISecureRandom; virtual;

  public

    const
    DigitalSignatureUsage = Int32(1);
    KeyEstablishmentUsage = Int32(2);

    /// <summary>
    /// Construct without a usage index, this will do a random construction
    /// of G.
    /// </summary>
    /// <param name="L">
    /// desired length of prime P in bits (the effective key size).
    /// </param>
    /// <param name="N">
    /// desired length of prime Q in bits.
    /// </param>
    /// <param name="certainty">
    /// certainty level for prime number generation.
    /// </param>
    /// <param name="random">
    /// the source of randomness to use.
    /// </param>
    constructor Create(L, N, certainty: Int32;
      const random: ISecureRandom); overload;

    /// <summary>
    /// Construct without a usage index, this will do a random construction
    /// of G.
    /// </summary>
    /// <param name="L">
    /// desired length of prime P in bits (the effective key size).
    /// </param>
    /// <param name="N">
    /// desired length of prime Q in bits.
    /// </param>
    /// <param name="certainty">
    /// certainty level for prime number generation.
    /// </param>
    /// <param name="random">
    /// the source of randomness to use.
    /// </param>
    /// <param name="usageIndex">
    /// a valid usage index.
    /// </param>
    constructor Create(L, N, certainty: Int32; const random: ISecureRandom;
      usageIndex: Int32); overload;

    property L: Int32 read GetL;
    property N: Int32 read GetN;
    property usageIndex: Int32 read GetUsageIndex;
    property certainty: Int32 read GetCertainty;
    property random: ISecureRandom read GetRandom;

  end;

implementation

{ TDsaParameterGenerationParameters }

constructor TDsaParameterGenerationParameters.Create(L, N, certainty: Int32;
  const random: ISecureRandom);
begin
  Create(L, N, certainty, random, -1);
end;

constructor TDsaParameterGenerationParameters.Create(L, N, certainty: Int32;
  const random: ISecureRandom; usageIndex: Int32);
begin
  Inherited Create();
  Fl := L;
  Fn := N;
  Fcertainty := certainty;
  Frandom := random;
  FusageIndex := usageIndex;
end;

function TDsaParameterGenerationParameters.GetL: Int32;
begin
  result := Fl;
end;

function TDsaParameterGenerationParameters.GetN: Int32;
begin
  result := Fn;
end;

function TDsaParameterGenerationParameters.GetCertainty: Int32;
begin
  result := Fcertainty;
end;

function TDsaParameterGenerationParameters.GetUsageIndex: Int32;
begin
  result := FusageIndex;
end;

function TDsaParameterGenerationParameters.GetRandom: ISecureRandom;
begin
  result := Frandom;
end;

end.
