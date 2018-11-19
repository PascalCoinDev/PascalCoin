unit HlpHMACNotBuildInAdapter;

{$I HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF HAS_UNITSCOPE}
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpIHashResult,
  HlpNullable;

type

  THMACNotBuildInAdapter = class sealed(THash, IHMAC, IHMACNotBuildIn, IWithKey,
    ICrypto, ICryptoNotBuildIn)

  strict private

    Fm_hash: IHash;
    Fm_opad, Fm_ipad, Fm_key: THashLibByteArray;
    Fm_blocksize: Int32;

  strict protected

    function GetName: String; override;

    function GetKey(): THashLibByteArray;
    function GetKeyLength(): TNullableInteger;
    procedure SetKey(const value: THashLibByteArray);
    procedure UpdatePads();

  public

    constructor Create(const a_underlyingHash: IHash);
    procedure Initialize(); override;
    function TransformFinal(): IHashResult; override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function Clone(): IHash; override;
    property Key: THashLibByteArray read GetKey write SetKey;
    property Name: String read GetName;
    property KeyLength: TNullableInteger read GetKeyLength;

  end;

implementation

{ THMACNotBuildInAdapter }

function THMACNotBuildInAdapter.Clone(): IHash;
var
  HmacInstance: THMACNotBuildInAdapter;
begin
  HmacInstance := THMACNotBuildInAdapter.Create(Fm_hash.Clone());
  HmacInstance.Fm_opad := System.Copy(Fm_opad);
  HmacInstance.Fm_ipad := System.Copy(Fm_ipad);
  HmacInstance.Fm_key := System.Copy(Fm_key);
  HmacInstance.Fm_blocksize := Fm_blocksize;
  result := HmacInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THMACNotBuildInAdapter.Create(const a_underlyingHash: IHash);
begin
  Inherited Create(a_underlyingHash.HashSize, a_underlyingHash.BlockSize);
  Fm_hash := a_underlyingHash;
  Fm_blocksize := Fm_hash.BlockSize;
  System.SetLength(Fm_key, 0);
  System.SetLength(Fm_ipad, Fm_blocksize);
  System.SetLength(Fm_opad, Fm_blocksize);
end;

function THMACNotBuildInAdapter.GetKey: THashLibByteArray;
begin
  result := System.Copy(Fm_key);
end;

function THMACNotBuildInAdapter.GetKeyLength: TNullableInteger;
begin
  result := Nil;
end;

procedure THMACNotBuildInAdapter.SetKey(const value: THashLibByteArray);
begin
  if (value = Nil) then
  begin

    System.SetLength(Fm_key, 0);
  end
  else
  begin
    Fm_key := System.Copy(value);
  end;
end;

procedure THMACNotBuildInAdapter.UpdatePads;
var
  LKey: THashLibByteArray;
  Idx, LBlockSize: Int32;
begin
  LBlockSize := Fm_blocksize;
  if (System.Length(Key) > LBlockSize) then
  begin
    LKey := Fm_hash.ComputeBytes(Key).GetBytes();
  end
  else
  begin
    LKey := Key;
  end;

  System.FillChar(Fm_ipad[0], LBlockSize * System.SizeOf(Byte), Byte($36));
  System.FillChar(Fm_opad[0], LBlockSize * System.SizeOf(Byte), Byte($5C));

  Idx := 0;
  while (Idx < System.Length(LKey)) and (Idx < LBlockSize) do
  begin
    Fm_ipad[Idx] := Fm_ipad[Idx] xor LKey[Idx];
    Fm_opad[Idx] := Fm_opad[Idx] xor LKey[Idx];
    System.Inc(Idx);
  end;

end;

procedure THMACNotBuildInAdapter.Initialize;
begin
  Fm_hash.Initialize();
  UpdatePads();
  Fm_hash.TransformBytes(Fm_ipad);
end;

function THMACNotBuildInAdapter.TransformFinal: IHashResult;
begin
  result := Fm_hash.TransformFinal();
  Fm_hash.TransformBytes(Fm_opad);
  Fm_hash.TransformBytes(result.GetBytes());
  result := Fm_hash.TransformFinal();
  Initialize();

end;

procedure THMACNotBuildInAdapter.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF}
  Fm_hash.TransformBytes(a_data, a_index, a_length);
end;

function THMACNotBuildInAdapter.GetName: String;
begin
  result := Format('%s(%s)', ['THMAC', Fm_hash.Name]);

end;

end.
