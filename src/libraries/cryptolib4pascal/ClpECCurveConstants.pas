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

unit ClpECCurveConstants;

{$I CryptoLib.inc}

interface

type
  TECCurveConstants = class sealed(TObject)

  public

    const
    COORD_AFFINE = Int32(0);
    COORD_HOMOGENEOUS = Int32(1);
    COORD_JACOBIAN = Int32(2);
    COORD_JACOBIAN_CHUDNOVSKY = Int32(3);
    COORD_JACOBIAN_MODIFIED = Int32(4);
    COORD_LAMBDA_AFFINE = Int32(5);
    COORD_LAMBDA_PROJECTIVE = Int32(6);
    COORD_SKEWED = Int32(7);

  end;

implementation

end.
