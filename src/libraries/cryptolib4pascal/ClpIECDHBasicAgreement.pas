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

unit ClpIECDHBasicAgreement;

{$I CryptoLib.inc}

interface

uses
  ClpIBasicAgreement;

type

  /// <summary>
  /// P1363 7.2.1 ECSVDP-DH <br />ECSVDP-DH is Elliptic Curve Secret Value
  /// Derivation Primitive, <br />Diffie-Hellman version. It is based on the
  /// work of [DH76], [Mil86], <br />and [Kob87]. This primitive derives a
  /// shared secret value from one <br />party's private key and another
  /// party's public key, where both have <br />the same set of EC domain
  /// parameters. If two parties correctly <br />execute this primitive, they
  /// will produce the same output. This <br />primitive can be invoked by a
  /// scheme to derive a shared secret key; <br />specifically, it may be
  /// used with the schemes ECKAS-DH1 and <br />DL/ECKAS-DH2. It assumes that
  /// the input keys are valid (see also <br />Section 7.2.2). <br />
  /// </summary>
  IECDHBasicAgreement = interface(IBasicAgreement)

    ['{99566C8A-DF5D-4F60-859A-583DE6260A28}']

  end;

implementation

end.
