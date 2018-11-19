unit HlpSHA2_224;

{$I HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpSHA2_256Base,
  HlpIHash,
  HlpConverters;

type
  TSHA2_224 = class sealed(TSHA2_256Base)

  strict protected
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA2_224 }

function TSHA2_224.Clone(): IHash;
var
  HashInstance: TSHA2_224;
begin
  HashInstance := TSHA2_224.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA2_224.Create;
begin
  Inherited Create(28);
end;

function TSHA2_224.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 7 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(Fm_state), 0, PByte(result), 0,
    System.Length(result));
end;

procedure TSHA2_224.Initialize;
begin
  Fm_state[0] := $C1059ED8;
  Fm_state[1] := $367CD507;
  Fm_state[2] := $3070DD17;
  Fm_state[3] := $F70E5939;
  Fm_state[4] := $FFC00B31;
  Fm_state[5] := $68581511;
  Fm_state[6] := $64F98FA7;
  Fm_state[7] := $BEFA4FA4;

  Inherited Initialize();

end;

end.
