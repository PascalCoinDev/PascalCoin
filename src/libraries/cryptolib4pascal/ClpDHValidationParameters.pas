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

unit ClpDHValidationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHValidationParameters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SSeedNil = '"Seed" Cannot Be Nil';

type
  TDHValidationParameters = class(TInterfacedObject, IDHValidationParameters)
  strict private
  var
    Fseed: TCryptoLibByteArray;
    Fcounter: Int32;

    function GetCounter: Int32; virtual;
    function GetSeed: TCryptoLibByteArray; virtual;

  public
    constructor Create(const seed: TCryptoLibByteArray; counter: Int32);

    function Equals(const other: IDHValidationParameters): Boolean; reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property counter: Int32 read GetCounter;
    property seed: TCryptoLibByteArray read GetSeed;
  end;

implementation

{ TDHValidationParameters }

constructor TDHValidationParameters.Create(const seed: TCryptoLibByteArray;
  counter: Int32);
begin
  Inherited Create();
  if (seed = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SSeedNil);
  end;

  Fseed := System.Copy(seed);
  Fcounter := counter;
end;

function TDHValidationParameters.Equals(const other
  : IDHValidationParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDHValidationParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := (counter = other.counter) and TArrayUtils.AreEqual(seed,
    other.seed);
end;

function TDHValidationParameters.GetCounter: Int32;
begin
  result := Fcounter;
end;

function TDHValidationParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := counter xor TArrayUtils.GetArrayHashCode(seed);
end;

function TDHValidationParameters.GetSeed: TCryptoLibByteArray;
begin
  result := System.Copy(Fseed);
end;

end.
