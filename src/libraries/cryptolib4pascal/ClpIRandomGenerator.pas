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

unit ClpIRandomGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  /// <remarks>Generic interface for objects generating random bytes.</remarks>
  IRandomGenerator = interface(IInterface)
    ['{1CC92157-EEC8-4300-8254-655AF791E4CA}']

    /// <summary>Add more seed material to the generator.</summary>
    /// <param name="seed">A byte array to be mixed into the generator's state.</param>
    procedure AddSeedMaterial(const seed: TCryptoLibByteArray); overload;

    /// <summary>Add more seed material to the generator.</summary>
    /// <param name="seed">A long value to be mixed into the generator's state.</param>
    procedure AddSeedMaterial(seed: Int64); overload;

    /// <summary>Fill byte array with random values.</summary>
    /// <param name="bytes">Array to be filled.</param>
    procedure NextBytes(const bytes: TCryptoLibByteArray); overload;

    /// <summary>Fill byte array with random values.</summary>
    /// <param name="bytes">Array to receive bytes.</param>
    /// <param name="start">Index to start filling at.</param>
    /// <param name="len">Length of segment to fill.</param>
    procedure NextBytes(const bytes: TCryptoLibByteArray;
      start, len: Int32); overload;
  end;

implementation

end.
