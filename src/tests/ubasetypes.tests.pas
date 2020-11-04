unit UBaseTypes.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit,
  testregistry,
  UBaseTypes;

type

  { TBytesBufferTest }

  TBytesBufferTest = class(TTestCase)
    published
      procedure Test_SaveToStream;
  end;

implementation

{ TBytesBufferTest }

procedure TBytesBufferTest.Test_SaveToStream;
var Lbb, Lbb2 : TBytesBuffer;
  LStream : TStream;
  LBuffer : TBytes;
  i : Integer;
begin
  SetLength(LBuffer,1000 + Random(1000) );
  for i:= 0 to High(LBuffer) do begin
    LBuffer[i] := Random(250)+1;
  end;
  Lbb := TBytesBuffer.Create(Random(1000)+100);
  Lbb.Add(LBuffer);
  Lbb2 := TBytesBuffer.CreateCopy(Lbb);
  LStream := TMemoryStream.Create;
  try
    Lbb.SaveToStream(LStream);
    Self.AssertEquals('T1',0,Lbb.Compare(Lbb2));
    Self.AssertEquals('T2',0,Lbb.Compare(LStream));
    Lbb2.Clear;
    Lbb2.LoadFromStream(LStream);
    Self.AssertEquals('T3',0,Lbb.Compare(Lbb2));
  finally
    Lbb.Free;
    Lbb2.Free;
  end;
end;


initialization
  Randomize;
  RegisterTest(TBytesBufferTest);
end.


