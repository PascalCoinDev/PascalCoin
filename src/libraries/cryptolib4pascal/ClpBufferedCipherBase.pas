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

unit ClpBufferedCipherBase;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpIBufferedCipher,
  ClpICipherParameters,
  ClpIBufferedCipherBase,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooSmall = 'Output Buffer too Short';
  SInvalidBufferSize = '"BufferSize" Must Be Greater Than Zero';
  SInputOutputStreamSame =
    'Input and Output Streams Must not Point to the Same Stream Instance';
  SUnAssignedInputStream = 'Input Stream Is Unassigned';
  SUnAssignedOutputStream = 'Output Stream Is Unassigned';
  SPositionOutOfRange = 'Current Position Is Out Of Range';
  SStreamPositionOutOfRange =
    'Stream Position (or Stream Length to Process) Is Out Of Range';

type
  TBufferedCipherBase = class abstract(TInterfacedObject, IBufferedCipherBase,
    IBufferedCipher)

  strict private

    class function GetEmptyBuffer: TCryptoLibByteArray; static; inline;

  var
    FBufferSize: Int32;
    FOnProgress: TBufferedCipherProgressEvent;

  const
    BUFFER_SIZE = Int32(64 * 1024); // 64Kb

  strict protected

    function GetBufferSize: Int32; inline;
    procedure SetBufferSize(value: Int32); inline;
    function GetOnProgress: TBufferedCipherProgressEvent; inline;
    procedure SetOnProgress(const value: TBufferedCipherProgressEvent); inline;
    procedure DoProgress(AProcessed, ATotal: Int64); virtual;

    class property EmptyBuffer: TCryptoLibByteArray read GetEmptyBuffer;

  public

    constructor Create();

    procedure Init(forEncryption: Boolean; const parameters: ICipherParameters);
      virtual; abstract;

    function GetBlockSize(): Int32; virtual; abstract;

    function GetOutputSize(inputLen: Int32): Int32; virtual; abstract;
    function GetUpdateOutputSize(inputLen: Int32): Int32; virtual; abstract;

    function ProcessByte(input: Byte): TCryptoLibByteArray; overload;
      virtual; abstract;

    function ProcessByte(input: Byte; const output: TCryptoLibByteArray;
      outOff: Int32): Int32; overload; virtual;

    function ProcessBytes(const input: TCryptoLibByteArray)
      : TCryptoLibByteArray; overload; virtual;

    function ProcessBytes(const input: TCryptoLibByteArray;
      inOff, length: Int32): TCryptoLibByteArray; overload; virtual; abstract;

    function ProcessBytes(const input, output: TCryptoLibByteArray;
      outOff: Int32): Int32; overload; virtual;

    function ProcessBytes(const input: TCryptoLibByteArray; inOff: Int32;
      length: Int32; const output: TCryptoLibByteArray; outOff: Int32): Int32;
      overload; virtual;

    procedure ProcessStream(const inputStream, outputStream: TStream;
      length: Int64); overload; virtual;

    procedure ProcessStream(const inputStream: TStream; inPos: Int64;
      const outputStream: TStream; outPos: Int64; length: Int64);
      overload; virtual;

    function DoFinal(): TCryptoLibByteArray; overload; virtual; abstract;

    function DoFinal(const input: TCryptoLibByteArray): TCryptoLibByteArray;
      overload; virtual;

    function DoFinal(const input: TCryptoLibByteArray; inOff, length: Int32)
      : TCryptoLibByteArray; overload; virtual; abstract;

    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32): Int32;
      overload; virtual;

    function DoFinal(const input, output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload; virtual;

    function DoFinal(const input: TCryptoLibByteArray; inOff, length: Int32;
      const output: TCryptoLibByteArray; outOff: Int32): Int32;
      overload; virtual;

    procedure Reset(); virtual; abstract;

    function GetAlgorithmName: String; virtual; abstract;
    property AlgorithmName: String read GetAlgorithmName;

    /// <summary>
    /// property for determining the buffer size to use for stream based
    /// encryption/decryption.
    /// </summary>
    property BufferSize: Int32 read GetBufferSize write SetBufferSize;
    property OnProgress: TBufferedCipherProgressEvent read GetOnProgress
      write SetOnProgress;

  end;

implementation

{ TBufferedCipherBase }

procedure TBufferedCipherBase.DoProgress(AProcessed, ATotal: Int64);
begin
  if System.Assigned(FOnProgress) then
  begin
    FOnProgress(AProcessed, ATotal);
  end;
end;

function TBufferedCipherBase.GetOnProgress: TBufferedCipherProgressEvent;
begin
  result := FOnProgress;
end;

procedure TBufferedCipherBase.SetOnProgress(const value
  : TBufferedCipherProgressEvent);
begin
  FOnProgress := value;
end;

constructor TBufferedCipherBase.Create;
begin
  Inherited Create();
  FBufferSize := BUFFER_SIZE;
end;

function TBufferedCipherBase.GetBufferSize: Int32;
begin
  result := FBufferSize;
end;

procedure TBufferedCipherBase.SetBufferSize(value: Int32);
begin
  if value > 0 then
  begin
    FBufferSize := value;
  end
  else
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidBufferSize);
  end;
end;

function TBufferedCipherBase.DoFinal(const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  outBytes: TCryptoLibByteArray;
begin
  outBytes := DoFinal();
  if ((outOff + System.length(outBytes)) > System.length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;
  System.Move(outBytes[0], output[outOff], System.length(outBytes));
  result := System.length(outBytes);
end;

function TBufferedCipherBase.DoFinal(const input: TCryptoLibByteArray)
  : TCryptoLibByteArray;
begin
  result := DoFinal(input, 0, System.length(input));
end;

function TBufferedCipherBase.DoFinal(const input: TCryptoLibByteArray;
  inOff, length: Int32; const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  len: Int32;
begin
  len := ProcessBytes(input, inOff, length, output, outOff);
  len := len + DoFinal(output, outOff + len);
  result := len;
end;

function TBufferedCipherBase.DoFinal(const input, output: TCryptoLibByteArray;
  outOff: Int32): Int32;
begin
  result := DoFinal(input, 0, System.length(input), output, outOff);
end;

class function TBufferedCipherBase.GetEmptyBuffer: TCryptoLibByteArray;
begin
  result := Nil;
end;

function TBufferedCipherBase.ProcessByte(input: Byte;
  const output: TCryptoLibByteArray; outOff: Int32): Int32;
var
  outBytes: TCryptoLibByteArray;
begin
  outBytes := ProcessByte(input);
  if (outBytes = Nil) then
  begin
    result := 0;
    Exit;
  end;
  if ((outOff + System.length(outBytes)) > System.length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;
  System.Move(outBytes[0], output[outOff], System.length(outBytes));
  result := System.length(outBytes);
end;

function TBufferedCipherBase.ProcessBytes(const input: TCryptoLibByteArray)
  : TCryptoLibByteArray;
begin
  result := ProcessBytes(input, 0, System.length(input));
end;

function TBufferedCipherBase.ProcessBytes(const input: TCryptoLibByteArray;
  inOff, length: Int32; const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  outBytes: TCryptoLibByteArray;
begin
  outBytes := ProcessBytes(input, inOff, length);
  if (outBytes = Nil) then
  begin
    result := 0;
    Exit;
  end;
  if ((outOff + System.length(outBytes)) > System.length(output)) then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooSmall);
  end;
  System.Move(outBytes[0], output[outOff], System.length(outBytes));
  result := System.length(outBytes);
end;

procedure TBufferedCipherBase.ProcessStream(const inputStream: TStream;
  inPos: Int64; const outputStream: TStream; outPos, length: Int64);
var
  LBufferSize, readed: Int32;
  total: Int64;
  data, tempRes: TCryptoLibByteArray;
begin
  total := 0;
  if ((inPos < 0) or (outPos < 0) or (length <= 0)) then
  begin
    raise EIndexOutOfRangeCryptoLibException.CreateRes
      (@SStreamPositionOutOfRange);
  end;

  if inputStream = Nil then
  begin
    raise EStreamCryptoLibException.CreateRes(@SUnAssignedInputStream);
  end;

  if outputStream = Nil then
  begin
    raise EStreamCryptoLibException.CreateRes(@SUnAssignedOutputStream);
  end;

  if inputStream = outputStream then
  begin
    raise EStreamCryptoLibException.CreateRes(@SInputOutputStreamSame);
  end;

  if ((inputStream.Position + length) > inputStream.Size) then
  begin
    raise EIndexOutOfRangeCryptoLibException.CreateRes(@SPositionOutOfRange);
  end;

  if (inputStream.Position >= inputStream.Size) then
  begin
    Exit;
  end;

  if BufferSize > inputStream.Size then // Sanity Check
  begin
    LBufferSize := BUFFER_SIZE;
  end
  else
  begin
    LBufferSize := BufferSize;
  end;

  System.SetLength(data, LBufferSize);

  inputStream.Position := inPos;
  outputStream.Position := outPos;

  DoProgress(0, length);

  while true do
  begin
    readed := inputStream.Read(data[0], LBufferSize);

    if ((total + Int64(readed)) >= length) then
    begin
      tempRes := ProcessBytes(data, 0, Int32(length - total));
      DoProgress(total + readed, length);
      if (tempRes <> Nil) then
      begin
        outputStream.Write(tempRes[0], System.length(tempRes));
      end;
      break;
    end
    else
    begin
      tempRes := ProcessBytes(data, 0, readed);
      if (tempRes <> Nil) then
      begin
        outputStream.Write(tempRes[0], System.length(tempRes));
      end;
      total := total + readed;
    end;
    DoProgress(total, length);
  end;

  tempRes := DoFinal();
  if (tempRes <> Nil) then
  begin
    outputStream.Write(tempRes[0], System.length(tempRes));
  end;

end;

procedure TBufferedCipherBase.ProcessStream(const inputStream,
  outputStream: TStream; length: Int64);
begin
  ProcessStream(inputStream, 0, outputStream, 0, length);
end;

function TBufferedCipherBase.ProcessBytes(const input,
  output: TCryptoLibByteArray; outOff: Int32): Int32;
begin
  result := ProcessBytes(input, 0, System.length(input), output, outOff);
end;

end.
