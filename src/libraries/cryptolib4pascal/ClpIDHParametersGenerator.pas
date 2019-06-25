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

unit ClpIDHParametersGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIDHParameters;

type
  IDHParametersGenerator = interface(IInterface)
    ['{ECE2C3CF-4DA4-450B-BB37-2C100BC72FF6}']

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
    function GenerateParameters(): IDHParameters;

  end;

implementation

end.
