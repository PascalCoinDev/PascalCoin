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

unit ClpCheck;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  TCheck = class sealed(TObject)

  public
    class procedure DataLength(condition: Boolean; const msg: String); overload;
    class procedure DataLength(const buf: TCryptoLibByteArray; off, len: Int32;
      const msg: String); overload;
    class procedure OutputLength(const buf: TCryptoLibByteArray;
      off, len: Int32; const msg: String); overload;

  end;

implementation

{ TCheck }

class procedure TCheck.DataLength(condition: Boolean; const msg: String);
begin
  if condition then
  begin
    raise EDataLengthCryptoLibException.Create(msg);
  end;
end;

class procedure TCheck.DataLength(const buf: TCryptoLibByteArray;
  off, len: Int32; const msg: String);
begin
  if ((off + len) > System.Length(buf)) then
  begin
    raise EDataLengthCryptoLibException.Create(msg);
  end;
end;

class procedure TCheck.OutputLength(const buf: TCryptoLibByteArray;
  off, len: Int32; const msg: String);
begin
  if ((off + len) > System.Length(buf)) then
  begin
    raise EOutputLengthCryptoLibException.Create(msg);
  end;
end;

end.
