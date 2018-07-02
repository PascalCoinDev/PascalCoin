unit HlpBlake2BTreeConfig;

{$I ..\..\Include\HashLib.inc}

interface

uses
  HlpIBlake2BTreeConfig;

type

  TBlake2BTreeConfig = class sealed(TInterfacedObject, IBlake2BTreeConfig)

  strict private

    FIntermediateHashSize, FMaxHeight, FFanOut: Int32;
    FLeafSize: Int64;

    function GetIntermediateHashSize: Int32; inline;
    procedure SetIntermediateHashSize(value: Int32); inline;
    function GetMaxHeight: Int32; inline;
    procedure SetMaxHeight(value: Int32); inline;
    function GetLeafSize: Int64; inline;
    procedure SetLeafSize(value: Int64); inline;
    function GetFanOut: Int32; inline;
    procedure SetFanOut(value: Int32); inline;

  public
    constructor Create();
    property IntermediateHashSize: Int32 read GetIntermediateHashSize
      write SetIntermediateHashSize;
    property MaxHeight: Int32 read GetMaxHeight write SetMaxHeight;

    property LeafSize: Int64 read GetLeafSize write SetLeafSize;
    property FanOut: Int32 read GetFanOut write SetFanOut;

    class function CreateInterleaved(parallelism: Int32)
      : IBlake2BTreeConfig; static;

  end;

implementation

{ TBlake2BTreeConfig }

constructor TBlake2BTreeConfig.Create;
begin
  IntermediateHashSize := 64;
end;

class function TBlake2BTreeConfig.CreateInterleaved(parallelism: Int32)
  : IBlake2BTreeConfig;
begin
  result := TBlake2BTreeConfig.Create();
  result.FanOut := parallelism;
  result.MaxHeight := 2;
  result.IntermediateHashSize := 64;
end;

function TBlake2BTreeConfig.GetFanOut: Int32;
begin
  result := FFanOut;
end;

function TBlake2BTreeConfig.GetIntermediateHashSize: Int32;
begin
  result := FIntermediateHashSize;
end;

function TBlake2BTreeConfig.GetLeafSize: Int64;
begin
  result := FLeafSize;
end;

function TBlake2BTreeConfig.GetMaxHeight: Int32;
begin
  result := FMaxHeight;
end;

procedure TBlake2BTreeConfig.SetFanOut(value: Int32);
begin
  FFanOut := value;
end;

procedure TBlake2BTreeConfig.SetIntermediateHashSize(value: Int32);
begin
  FIntermediateHashSize := value;
end;

procedure TBlake2BTreeConfig.SetLeafSize(value: Int64);
begin
  FLeafSize := value;
end;

procedure TBlake2BTreeConfig.SetMaxHeight(value: Int32);
begin
  FMaxHeight := value;
end;

end.
