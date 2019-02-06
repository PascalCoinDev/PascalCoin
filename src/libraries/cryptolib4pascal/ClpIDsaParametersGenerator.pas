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

unit ClpIDsaParametersGenerator;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIDsaParameters,
  ClpIDsaParameterGenerationParameters;

type
  IDsaParametersGenerator = interface(IInterface)
    ['{EB5A601B-2267-4485-A519-A80751FC39EA}']

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="size">
    /// size of the key (range 2^512 -&amp;gt; 2^1024 - 64 bit increments)
    /// </param>
    /// <param name="certainty">
    /// measure of robustness of prime (for FIPS 186-2 compliance this should
    /// be at least 80).
    /// </param>
    /// <param name="random">
    /// random byte source.
    /// </param>
    procedure Init(size, certainty: Int32;
      const random: ISecureRandom); overload;

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="size">
    /// size of the key (range 2^512 -&amp;gt; 2^1024 - 64 bit increments)
    /// </param>
    /// <param name="certainty">
    /// measure of robustness of prime (for FIPS 186-2 compliance this should
    /// be at least 80).
    /// </param>
    /// <param name="iterations">
    /// iterations
    /// </param>
    /// <param name="random">
    /// random byte source.
    /// </param>
    procedure Init(size, certainty, iterations: Int32;
      const random: ISecureRandom); overload;

    /// <summary>
    /// <para>
    /// Initialise the key generator for DSA 2.
    /// </para>
    /// <para>
    /// Use this init method if you need to generate parameters for DSA 2
    /// keys.
    /// </para>
    /// </summary>
    /// <param name="params">
    /// DSA 2 key generation parameters.
    /// </param>
    procedure Init(const params: IDsaParameterGenerationParameters); overload;

    /// <summary>
    /// <para>
    /// which generates the p and g values from the given parameters,
    /// returning the DSAParameters object.
    /// </para>
    /// <para>
    /// Note: can take a while...
    /// </para>
    /// </summary>
    /// <returns>
    /// a generated DSA parameters object.
    /// </returns>
    function GenerateParameters(): IDsaParameters;

  end;

implementation

end.
