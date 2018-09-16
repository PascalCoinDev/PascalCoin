unit HlpNullDigest;

{$I HashLib.inc}

interface

uses
  Classes,
  SysUtils,
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type
  TNullDigest = class sealed(THash, ITransformBlock)
  strict private
  var
    FbOut: TMemoryStream;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TNullDigest }

function TNullDigest.Clone(): IHash;
var
  HashInstance: TNullDigest;
begin
  HashInstance := TNullDigest.Create();
  FbOut.Position := 0;
  HashInstance.FbOut.CopyFrom(FbOut, FbOut.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TNullDigest.Create;
begin
  Inherited Create(-1, -1); // Dummy State
  FbOut := TMemoryStream.Create();
end;

destructor TNullDigest.Destroy;
begin
  FbOut.Free;
  inherited Destroy;
end;

procedure TNullDigest.Initialize;
begin
  FbOut.Position := 0;
  FbOut.Size := 0;
  HashSize := 0;
  BlockSize := 0;
end;

procedure TNullDigest.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  FbOut.Write(a_data[a_index], a_length);
  HashSize := Int32(FbOut.Size);
end;

function TNullDigest.TransformFinal: IHashResult;
var
  res: THashLibByteArray;
begin
  FbOut.Position := 0;
  System.SetLength(res, FbOut.Size);
  FbOut.Read(res[0], FbOut.Size);
  result := THashResult.Create(res);
  Initialize();
end;

end.
