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

unit ClpIGlvTypeBEndomorphism;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIGlvEndomorphism;

type
  IGlvTypeBEndomorphism = interface(IGlvEndomorphism)
    ['{4F285F6A-F627-4873-9F4C-FBC7A7B83A9C}']

    function CalculateB(const k, g: TBigInteger; t: Int32): TBigInteger;

  end;

implementation

end.
