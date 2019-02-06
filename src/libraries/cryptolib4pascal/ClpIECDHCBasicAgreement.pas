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

unit ClpIECDHCBasicAgreement;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIBasicAgreement;

type

  /// <summary>
  /// P1363 7.2.2 ECSVDP-DHC <br /><br />ECSVDP-DHC is Elliptic Curve Secret
  /// Value Derivation Primitive, <br />Diffie-Hellman version with cofactor
  /// multiplication. It is based on <br />the work of [DH76], [Mil86],
  /// [Kob87], [LMQ98] and [Kal98a]. This <br />primitive derives a shared
  /// secret value from one party's private key <br />and another party's
  /// public key, where both have the same set of EC <br />domain parameters.
  /// If two parties correctly execute this primitive, <br />they will
  /// produce the same output. This primitive can be invoked by a <br />
  /// scheme to derive a shared secret key; specifically, it may be used <br />
  /// with the schemes ECKAS-DH1 and DL/ECKAS-DH2. It does not assume the <br />
  /// validity of the input public key (see also Section 7.2.1). <br /><br />
  /// Note: As stated P1363 compatibility mode with ECDH can be preset, and <br />
  /// in this case the implementation doesn't have a ECDH compatibility mode <br />
  /// (if you want that just use ECDHBasicAgreement and note they both
  /// implement <br />BasicAgreement!). <br />
  /// </summary>
  IECDHCBasicAgreement = interface(IBasicAgreement)

    ['{5938300B-2722-49D4-9486-98EDD5C34239}']

  end;

implementation

end.
