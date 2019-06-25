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

unit ClpIDHBasicAgreement;

{$I CryptoLib.inc}

interface

uses
  ClpIBasicAgreement;

type
  /// <summary>
  /// <para>
  /// a Diffie-Hellman key agreement class.
  /// </para>
  /// <para>
  /// note: This is only the basic algorithm, it doesn't take advantage
  /// of long term public keys if they are available. See the DHAgreement
  /// class for a "better" implementation.
  /// </para>
  /// </summary>
  IDHBasicAgreement = interface(IBasicAgreement)

    ['{913FB2D8-1DDB-4E68-8C67-2BDD796BE3A1}']

  end;

implementation

end.
