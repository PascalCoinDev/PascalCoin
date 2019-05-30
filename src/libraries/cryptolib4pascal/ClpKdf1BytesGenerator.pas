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

unit ClpKdf1BytesGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpBaseKdfBytesGenerator,
  ClpIKdf1BytesGenerator;

type

  /// <summary>
  /// <para>
  /// KDF1 generator for derived keys and ivs as defined by IEEE
  /// P1363a/ISO 18033
  /// </para>
  /// <para>
  /// This implementation is based on IEEE P1363/ISO 18033.
  /// </para>
  /// </summary>
  TKdf1BytesGenerator = class(TBaseKdfBytesGenerator, IKdf1BytesGenerator)

  public

    /// <summary>
    /// Construct a KDF1 bytes generator.
    /// </summary>
    /// <param name="digest">
    /// the digest to be used as the source of derived keys.
    /// </param>
    constructor Create(const digest: IDigest);

  end;

implementation

{ TKdf1BytesGenerator }

constructor TKdf1BytesGenerator.Create(const digest: IDigest);
begin
  Inherited Create(0, digest);
end;

end.
