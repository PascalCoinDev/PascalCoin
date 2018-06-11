unit HlpMultipleTransformNonBlock;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF HAS_UNITSCOPE}
  HlpHashLibTypes,
  HlpHash,
  HlpIHashInfo,
  HlpIHashResult;

type

  TMultipleTransformNonBlock = class abstract(THash, INonBlockHash)

  strict private
    FBuffer: TMemoryStream;

    function Aggregate(): THashLibByteArray;

  strict protected
    function ComputeAggregatedBytes(a_data: THashLibByteArray): IHashResult;
      virtual; abstract;

  public
    constructor Create(a_hash_size, a_block_size: Int32);
    destructor Destroy; override;
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function ComputeBytes(a_data: THashLibByteArray): IHashResult; override;

  end;

implementation

{ TMultipleTransformNonBlock }

function TMultipleTransformNonBlock.Aggregate: THashLibByteArray;
begin
  FBuffer.Position := 0;
  System.SetLength(result, FBuffer.Size);
  FBuffer.Read(result[0], FBuffer.Size);
end;

constructor TMultipleTransformNonBlock.Create(a_hash_size, a_block_size: Int32);
begin
  Inherited Create(a_hash_size, a_block_size);
  FBuffer := TMemoryStream.Create();
end;

destructor TMultipleTransformNonBlock.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TMultipleTransformNonBlock.Initialize;
begin
  FBuffer.Clear;
  FBuffer.SetSize(0);
end;

procedure TMultipleTransformNonBlock.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  FBuffer.Write(a_data[a_index], a_length);
end;

function TMultipleTransformNonBlock.TransformFinal: IHashResult;
begin
  result := ComputeAggregatedBytes(Aggregate());
  Initialize();
end;

function TMultipleTransformNonBlock.ComputeBytes(a_data: THashLibByteArray)
  : IHashResult;
begin
  Initialize();
  result := ComputeAggregatedBytes(a_data);
end;

end.
