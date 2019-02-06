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

unit ClpRandomNumberGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpOSRandom,
  ClpPcgRandomMinimal,
  ClpIRandomNumberGenerator;

resourcestring
  SUnknownAlgorithm = 'Unknown Random Generation Algorithm Requested';

type
  TRandomNumberGenerator = class abstract(TInterfacedObject,
    IRandomNumberGenerator)

  public

    type
{$SCOPEDENUMS ON}
    TRandomNumberGeneratorMode = (rngmOS = 0, rngmPCG = 1);
{$SCOPEDENUMS OFF}
  class function CreateRNG(): IRandomNumberGenerator; overload; static;

  class function CreateRNG(rngMode: TRandomNumberGeneratorMode)
    : IRandomNumberGenerator; overload; static;

  procedure GetBytes(const data: TCryptoLibByteArray); virtual; abstract;

  procedure GetNonZeroBytes(const data: TCryptoLibByteArray); virtual; abstract;

  end;

type
  TOSRandomNumberGenerator = class sealed(TRandomNumberGenerator,
    IOSRandomNumberGenerator)

  public
    constructor Create();

    procedure GetBytes(const data: TCryptoLibByteArray); override;

    procedure GetNonZeroBytes(const data: TCryptoLibByteArray); override;

  end;

type
  TPCGRandomNumberGenerator = class sealed(TRandomNumberGenerator,
    IPCGRandomNumberGenerator)

  public
    constructor Create();

    procedure GetBytes(const data: TCryptoLibByteArray); override;

    procedure GetNonZeroBytes(const data: TCryptoLibByteArray); override;

  end;

implementation

{ TRandomNumberGenerator }

class function TRandomNumberGenerator.CreateRNG: IRandomNumberGenerator;
begin
  result := TRandomNumberGenerator.CreateRNG(TRandomNumberGeneratorMode.rngmOS);
end;

class function TRandomNumberGenerator.CreateRNG
  (rngMode: TRandomNumberGeneratorMode): IRandomNumberGenerator;
begin

  case rngMode of
    TRandomNumberGeneratorMode.rngmOS:
      begin
        result := TOSRandomNumberGenerator.Create();
        Exit;
      end;

    TRandomNumberGeneratorMode.rngmPCG:
      begin
        result := TPCGRandomNumberGenerator.Create();
        Exit;
      end

  else
    begin
      raise EArgumentCryptoLibException.CreateRes(@SUnknownAlgorithm);
    end;

  end;

end;

{ TOSRandomNumberGenerator }

constructor TOSRandomNumberGenerator.Create;
begin
  inherited Create();
end;

procedure TOSRandomNumberGenerator.GetBytes(const data: TCryptoLibByteArray);
begin
  TOSRandom.GetBytes(data);
end;

procedure TOSRandomNumberGenerator.GetNonZeroBytes
  (const data: TCryptoLibByteArray);
begin
  TOSRandom.GetNonZeroBytes(data);
end;

{ TPCGRandomNumberGenerator }

constructor TPCGRandomNumberGenerator.Create;
begin
  inherited Create();
end;

procedure TPCGRandomNumberGenerator.GetBytes(const data: TCryptoLibByteArray);
var
  i: Int64;
begin
  i := System.Length(data);
  while i > 0 do
  begin
    data[i - 1] := Byte(TPcg.NextInt(System.Low(Int32), System.High(Int32)));
    System.Dec(i);
  end;

end;

procedure TPCGRandomNumberGenerator.GetNonZeroBytes
  (const data: TCryptoLibByteArray);
var
  i: Int64;
  val: Byte;
begin
  i := System.Length(data);
  while i > 0 do
  begin
    repeat
      val := Byte(TPcg.NextUInt32(System.Low(UInt32), System.High(UInt32)));
    until (not(val = 0));

    data[i - 1] := val;
    System.Dec(i);
  end;

end;

end.
