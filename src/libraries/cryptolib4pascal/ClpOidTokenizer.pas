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

unit ClpOidTokenizer;

{$I CryptoLib.inc}

interface

uses
  StrUtils,
  ClpIOidTokenizer;

type

  /// <summary>
  /// class for breaking up an Oid into it's component tokens
  /// </summary>
  TOidTokenizer = class sealed(TInterfacedObject, IOidTokenizer)

  strict private
  var
    Foid: String;
    Findex: Int32;

    function GetHasMoreTokens: Boolean; inline;

  public
    constructor Create(const oid: String);

    function NextToken(): String; inline;

    property HasMoreTokens: Boolean read GetHasMoreTokens;

  end;

implementation

{ TOidTokenizer }

constructor TOidTokenizer.Create(const oid: String);
begin
  Foid := oid;
  Findex := 1;
end;

function TOidTokenizer.GetHasMoreTokens: Boolean;
begin
  result := Findex <> 0;
end;

function TOidTokenizer.NextToken: String;
var
  endPoint: Int32;
  LNextToken, lastToken: string;
begin
  if (Findex = 0) then
  begin
    result := '';
    Exit;
  end;

  endPoint := PosEx('.', Foid, Findex);

  if (endPoint = 0) then
  begin
    lastToken := System.Copy(Foid, Findex, System.Length(Foid) - (Findex - 1));
    Findex := 0;
    result := lastToken;
    Exit;
  end;

  LNextToken := System.Copy(Foid, Findex, endPoint - Findex);
  Findex := endPoint + 1;
  result := LNextToken;
end;

end.
