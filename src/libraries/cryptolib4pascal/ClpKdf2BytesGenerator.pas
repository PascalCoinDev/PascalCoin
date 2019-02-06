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

unit ClpKdf2BytesGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIDigest,
  ClpBaseKdfBytesGenerator,
  ClpIKdf2BytesGenerator;

type

  /// <summary>
  /// <para>
  /// KDF2 generator for derived keys and ivs as defined by IEEE
  /// P1363a/ISO 18033
  /// </para>
  /// <para>
  /// This implementation is based on IEEE P1363/ISO 18033.
  /// </para>
  /// </summary>
  TKdf2BytesGenerator = class(TBaseKdfBytesGenerator, IKdf2BytesGenerator)

  public

    /// <summary>
    /// Construct a KDF2 bytes generator. Generates key material according to
    /// IEEE P1363 or ISO 18033 depending on the initialisation.
    /// </summary>
    /// <param name="digest">
    /// the digest to be used as the source of derived keys.
    /// </param>
    constructor Create(const digest: IDigest);

  end;

implementation

{ TKdf2BytesGenerator }

constructor TKdf2BytesGenerator.Create(const digest: IDigest);
begin
  Inherited Create(1, digest);
end;

end.
