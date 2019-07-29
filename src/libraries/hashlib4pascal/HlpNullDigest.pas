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
    FOut: TMemoryStream;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TNullDigest }

function TNullDigest.Clone(): IHash;
var
  LHashInstance: TNullDigest;
begin
  LHashInstance := TNullDigest.Create();
  FOut.Position := 0;
  LHashInstance.FOut.CopyFrom(FOut, FOut.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TNullDigest.Create;
begin
  Inherited Create(-1, -1); // Dummy State
  FOut := TMemoryStream.Create();
end;

destructor TNullDigest.Destroy;
begin
  FOut.Free;
  inherited Destroy;
end;

procedure TNullDigest.Initialize;
begin
  FOut.Position := 0;
  FOut.Size := 0;
  HashSize := 0;
  BlockSize := 0;
end;

procedure TNullDigest.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  if AData <> Nil then
  begin
    FOut.Write(AData[AIndex], ALength);
    HashSize := Int32(FOut.Size);
  end;
end;

function TNullDigest.TransformFinal: IHashResult;
var
  LResult: THashLibByteArray;
begin
  try
    if FOut.Size > 0 then
    begin
      FOut.Position := 0;
      System.SetLength(LResult, FOut.Size);
      FOut.Read(LResult[0], FOut.Size);
    end;
    result := THashResult.Create(LResult);
  finally
    Initialize();
  end;
end;

end.
