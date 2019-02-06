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

unit ClpKeyParameter;

{$I CryptoLib.inc}

interface

uses
  ClpIKeyParameter,
  ClpICipherParameters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SKeyNil = 'Key Cannot be Nil';
  SInvalidKeyOffSet = 'Invalid Key OffSet';
  SInvalidKeyLength = 'Invalid Key Length';

type
  TKeyParameter = class sealed(TInterfacedObject, IKeyParameter,
    ICipherParameters)

  strict private
  var
    Fkey: TCryptoLibByteArray;

  public
    constructor Create(const key: TCryptoLibByteArray); overload;
    constructor Create(const key: TCryptoLibByteArray;
      keyOff, keyLen: Int32); overload;
    destructor Destroy; override;
    function GetKey(): TCryptoLibByteArray; inline;

  end;

implementation

{ TKeyParameter }

constructor TKeyParameter.Create(const key: TCryptoLibByteArray);
begin
  Inherited Create();

  if (key = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SKeyNil);
  end;
  Fkey := System.Copy(key);
end;

constructor TKeyParameter.Create(const key: TCryptoLibByteArray;
  keyOff, keyLen: Int32);
begin
  Inherited Create();

  if (key = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SKeyNil);
  end;

  if ((keyOff < 0) or (keyOff > System.Length(key))) then
  begin
    raise EArgumentOutOfRangeCryptoLibException.CreateRes(@SInvalidKeyOffSet);
  end;

  if ((keyLen < 0) or (keyLen > (System.Length(key) - keyOff))) then
  begin
    raise EArgumentOutOfRangeCryptoLibException.CreateRes(@SInvalidKeyLength);
  end;

  System.SetLength(Fkey, keyLen);
  System.Move(key[keyOff], Fkey[0], keyLen);

end;

destructor TKeyParameter.Destroy;
begin
  TArrayUtils.Fill(Fkey, 0, System.Length(Fkey), Byte(0));
  inherited Destroy;
end;

function TKeyParameter.GetKey: TCryptoLibByteArray;
begin
  result := System.Copy(Fkey);
end;

end.
