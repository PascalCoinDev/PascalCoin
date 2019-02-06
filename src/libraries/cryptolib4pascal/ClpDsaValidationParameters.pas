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

unit ClpDsaValidationParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIDsaValidationParameters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SSeedNil = '"Seed" Cannot Be Nil';

type
  TDsaValidationParameters = class(TInterfacedObject, IDsaValidationParameters)
  strict private
  var
    Fseed: TCryptoLibByteArray;
    Fcounter, FusageIndex: Int32;

    function GetCounter: Int32; virtual;
    function GetUsageIndex: Int32; virtual;
    function GetSeed: TCryptoLibByteArray; virtual;

  public
    constructor Create(const seed: TCryptoLibByteArray;
      counter: Int32); overload;
    constructor Create(const seed: TCryptoLibByteArray;
      counter, usageIndex: Int32); overload;

    function Equals(const other: IDsaValidationParameters): Boolean;
      reintroduce;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property counter: Int32 read GetCounter;
    property usageIndex: Int32 read GetUsageIndex;
    property seed: TCryptoLibByteArray read GetSeed;
  end;

implementation

{ TDsaValidationParameters }

constructor TDsaValidationParameters.Create(const seed: TCryptoLibByteArray;
  counter: Int32);
begin
  Create(seed, counter, -1);
end;

constructor TDsaValidationParameters.Create(const seed: TCryptoLibByteArray;
  counter, usageIndex: Int32);
begin
  Inherited Create();
  if (seed = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SSeedNil);
  end;

  Fseed := System.Copy(seed);
  Fcounter := counter;
  FusageIndex := usageIndex;
end;

function TDsaValidationParameters.Equals(const other
  : IDsaValidationParameters): Boolean;
begin
  if other = Nil then
  begin
    result := False;
    Exit;
  end;
  if ((Self as IDsaValidationParameters) = other) then
  begin
    result := True;
    Exit;
  end;
  result := (counter = other.counter) and TArrayUtils.AreEqual(seed,
    other.seed);
end;

function TDsaValidationParameters.GetCounter: Int32;
begin
  result := Fcounter;
end;

function TDsaValidationParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := counter xor TArrayUtils.GetArrayHashCode(seed);
end;

function TDsaValidationParameters.GetSeed: TCryptoLibByteArray;
begin
  result := System.Copy(Fseed);
end;

function TDsaValidationParameters.GetUsageIndex: Int32;
begin
  result := FusageIndex;
end;

end.
