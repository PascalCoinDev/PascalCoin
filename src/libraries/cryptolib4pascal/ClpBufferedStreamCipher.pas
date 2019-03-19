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

unit ClpBufferedStreamCipher;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpIStreamCipher,
  ClpICipherParameters,
  ClpIParametersWithRandom,
  ClpIBufferedStreamCipher,
  ClpBufferedCipherBase,
  ClpCryptoLibTypes;

resourcestring
  SCipherNil = 'Cipher Instance Cannot be Nil';

type
  TBufferedStreamCipher = class(TBufferedCipherBase, IBufferedStreamCipher)

  strict private
  var
    FCipher: IStreamCipher;

  strict protected
    function GetAlgorithmName: String; override;

  public
    constructor Create(const cipher: IStreamCipher);

    procedure Init(forEncryption: Boolean;
      const parameters: ICipherParameters); override;

    function GetBlockSize(): Int32; override;

    function GetOutputSize(inputLen: Int32): Int32; override;

    function GetUpdateOutputSize(inputLen: Int32): Int32; override;

    function ProcessByte(input: Byte): TCryptoLibByteArray; overload; override;
    function ProcessByte(input: Byte; const output: TCryptoLibByteArray;
      outOff: Int32): Int32; overload; override;

    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, Length: Int32): TCryptoLibByteArray; overload; override;
    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, Length: Int32; const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload; override;

    function DoFinal(): TCryptoLibByteArray; overload; override;
    function DoFinal(const input: TCryptoLibByteArray; inOff, Length: Int32)
      : TCryptoLibByteArray; overload; override;

    procedure Reset(); override;

    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TBufferedStreamCipher }

constructor TBufferedStreamCipher.Create(const cipher: IStreamCipher);
begin
  Inherited Create();
  if (cipher = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SCipherNil);
  end;

  FCipher := cipher;
end;

function TBufferedStreamCipher.GetAlgorithmName: String;
begin
  result := FCipher.AlgorithmName;
end;

function TBufferedStreamCipher.GetBlockSize: Int32;
begin
  result := 0;
end;

function TBufferedStreamCipher.GetOutputSize(inputLen: Int32): Int32;
begin
  result := inputLen;
end;

function TBufferedStreamCipher.GetUpdateOutputSize(inputLen: Int32): Int32;
begin
  result := inputLen;
end;

procedure TBufferedStreamCipher.Init(forEncryption: Boolean;
  const parameters: ICipherParameters);
var
  LParameters: ICipherParameters;
begin
  LParameters := parameters;
  if Supports(LParameters, IParametersWithRandom) then
  begin
    LParameters := (LParameters as IParametersWithRandom).parameters;
  end;
  FCipher.Init(forEncryption, LParameters);
end;

function TBufferedStreamCipher.ProcessByte(input: Byte;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  if (outOff >= System.Length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;
  output[outOff] := FCipher.ReturnByte(input);
  result := 1;
end;

function TBufferedStreamCipher.ProcessByte(input: Byte): TCryptoLibByteArray;
begin
  result := TCryptoLibByteArray.Create(FCipher.ReturnByte(input));
end;

function TBufferedStreamCipher.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, Length: Int32; const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
begin
  if (Length < 1) then
  begin
    result := 0;
    Exit;
  end;

  if (Length > 0) then
  begin
    FCipher.ProcessBytes(input, inOff, Length, output, outOff);
  end;

  result := Length;
end;

function TBufferedStreamCipher.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, Length: Int32): TCryptoLibByteArray;
begin
  if (Length < 1) then
  begin
    result := Nil;
    Exit;
  end;
  System.SetLength(result, Length);
  FCipher.ProcessBytes(input, inOff, Length, result, 0);
end;

function TBufferedStreamCipher.DoFinal: TCryptoLibByteArray;
begin
  Reset();
  result := EmptyBuffer;
end;

function TBufferedStreamCipher.DoFinal(const input: TCryptoLibByteArray;
  inOff, Length: Int32): TCryptoLibByteArray;
begin
  if (Length < 1) then
  begin
    result := EmptyBuffer;
    Exit;
  end;
  result := ProcessBytes(input, inOff, Length);
  Reset();
end;

procedure TBufferedStreamCipher.Reset;
begin
  FCipher.Reset();
end;

end.
