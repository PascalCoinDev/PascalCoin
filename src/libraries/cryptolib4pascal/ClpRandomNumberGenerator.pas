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
  ClpAESPRNGRandom,
  ClpIRandomNumberGenerator;

resourcestring
  SUnknownAlgorithm = 'Unknown Random Generation Algorithm Requested';
  SRandomNumberGeneratorOutputBufferNil =
    'Random Number Generator Output Buffer Cannot Be Nil';

type
  TRandomNumberGenerator = class abstract(TInterfacedObject,
    IRandomNumberGenerator)

  strict protected
    class procedure ValidateOutputBufferNotNull(const ABuffer
      : TCryptoLibByteArray); static; inline;

  public

    type
{$SCOPEDENUMS ON}
    TRandomNumberGeneratorMode = (rngmOS = 0, rngmAES = 1);
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
  TAESPRNGRandomNumberGenerator = class sealed(TRandomNumberGenerator,
    IAESPRNGRandomNumberGenerator)

  public
    constructor Create();

    procedure GetBytes(const data: TCryptoLibByteArray); override;

    procedure GetNonZeroBytes(const data: TCryptoLibByteArray); override;

  end;

implementation

{ TRandomNumberGenerator }

class procedure TRandomNumberGenerator.ValidateOutputBufferNotNull
  (const ABuffer: TCryptoLibByteArray);
begin
  if ABuffer = Nil then
  begin
    raise EArgumentNilCryptoLibException.CreateRes
      (@SRandomNumberGeneratorOutputBufferNil);
  end;
end;

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

    TRandomNumberGeneratorMode.rngmAES:
      begin
        result := TAESPRNGRandomNumberGenerator.Create();
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
  ValidateOutputBufferNotNull(data);
  TOSRandom.GetBytes(data);
end;

procedure TOSRandomNumberGenerator.GetNonZeroBytes
  (const data: TCryptoLibByteArray);
begin
  ValidateOutputBufferNotNull(data);
  TOSRandom.GetNonZeroBytes(data);
end;

{ TAESPRNGRandomNumberGenerator }

constructor TAESPRNGRandomNumberGenerator.Create;
begin
  inherited Create();
end;

procedure TAESPRNGRandomNumberGenerator.GetBytes
  (const data: TCryptoLibByteArray);
begin
  ValidateOutputBufferNotNull(data);
  TAESPRNGRandom.GetBytes(data);
end;

procedure TAESPRNGRandomNumberGenerator.GetNonZeroBytes
  (const data: TCryptoLibByteArray);
begin
  ValidateOutputBufferNotNull(data);
  TAESPRNGRandom.GetNonZeroBytes(data);
end;

end.
