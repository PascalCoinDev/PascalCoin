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

unit ClpZTauElement;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIZTauElement;

type
  /// **
  // * Class representing an element of <code><b>Z</b>[&#964;]</code>. Let
  // * <code>&#955;</code> be an element of <code><b>Z</b>[&#964;]</code>. Then
  // * <code>&#955;</code> is given as <code>&#955; = u + v&#964;</code>. The
  // * components <code>u</code> and <code>v</code> may be used directly, there
  // * are no accessor methods.
  // * Immutable class.
  // */
  TZTauElement = class(TInterfacedObject, IZTauElement)

  strict private
  var
    Fu, Fv: TBigInteger;

    function GetU: TBigInteger; inline;
    function GetV: TBigInteger; inline;

  public
    // /**
    // * The &quot;real&quot; part of <code>&#955;</code>.
    // */
    property U: TBigInteger read GetU;
    // /**
    // * The &quot;<code>&#964;</code>-adic&quot; part of <code>&#955;</code>.
    // */
    property V: TBigInteger read GetV;

    // /**
    // * Constructor for an element <code>&#955;</code> of
    // * <code><b>Z</b>[&#964;]</code>.
    // * @param u The &quot;real&quot; part of <code>&#955;</code>.
    // * @param v The &quot;<code>&#964;</code>-adic&quot; part of
    // * <code>&#955;</code>.
    // */
    constructor Create(const U, V: TBigInteger);

  end;

implementation

{ TZTauElement }

constructor TZTauElement.Create(const U, V: TBigInteger);
begin
  inherited Create();
  Fu := U;
  Fv := V;
end;

function TZTauElement.GetU: TBigInteger;
begin
  Result := Fu;
end;

function TZTauElement.GetV: TBigInteger;
begin
  Result := Fv;
end;

end.
