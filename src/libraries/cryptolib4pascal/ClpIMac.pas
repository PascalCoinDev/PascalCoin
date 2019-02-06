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

unit ClpIMac;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpICipherParameters,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// The base interface for implementations of message authentication codes
  /// (MACs).
  /// </summary>
  IMac = interface(IInterface)
    ['{3273EF2F-AE51-4878-B55C-5F801DB85A74}']

    /// <summary>
    /// returns the name of the algorithm the MAC implements.
    /// </summary>
    function GetAlgorithmName: string;

    function GetUnderlyingDigest: IDigest;

    /// <summary>
    /// the block size for this MAC (in bytes).
    /// </summary>
    function GetMacSize: Int32;

    /// <summary>
    /// add a single byte to the mac for processing.
    /// </summary>
    /// <param name="input">
    /// the byte to be processed.
    /// </param>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the MAC is not initialised.
    /// </exception>
    procedure Update(input: Byte);

    /// <param name="input">
    /// the array containing the input.
    /// </param>
    /// <param name="inOff">
    /// the index in the array the data begins at.
    /// </param>
    /// <param name="len">
    /// the length of the input starting at inOff.
    /// </param>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the MAC is not initialised.
    /// </exception>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough data in in.
    /// </exception>
    procedure BlockUpdate(const input: TCryptoLibByteArray; inOff, len: Int32);

    /// <summary>
    /// Initialise the MAC.
    /// </summary>
    /// <param name="parameters">
    /// the key and other data required by the MAC.
    /// </param>
    /// <exception cref="EArgumentCryptoLibException">
    /// if the parameters argument is inappropriate.
    /// </exception>
    procedure Init(const parameters: ICipherParameters);

    /// <summary>
    /// Compute the final stage of the MAC writing the output to the output
    /// parameter starting from the outOff parameter
    /// </summary>
    /// <param name="output">
    /// the array the MAC is to be written to.
    /// </param>
    /// <param name="outOff">
    /// the offset into the output buffer the output is to start at.
    /// </param>
    /// <exception cref="EDataLengthCryptoLibException">
    /// if there isn't enough space in output.
    /// </exception>
    /// <exception cref="EInvalidOperationCryptoLibException">
    /// if the MAC is not initialised.
    /// </exception>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;

    /// <summary>
    /// Compute the final stage of the MAC writing the output to result
    /// </summary>
    /// <returns>
    /// the computed MAC result
    /// </returns>
    function DoFinal: TCryptoLibByteArray; overload;

    /// <summary>
    /// Reset the MAC. At the end of resetting the MAC should be in the <br />
    /// in the same state it was after the last init (if there was one).
    /// </summary>
    procedure Reset();

    /// <summary>
    /// the name of the algorithm the MAC implements.
    /// </summary>
    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

end.
