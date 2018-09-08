unit HlpBlake2STreeConfig;

{$I HashLib.inc}

interface

uses
  HlpIBlake2STreeConfig;

type

  TBlake2STreeConfig = class sealed(TInterfacedObject, IBlake2STreeConfig)

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
      : IBlake2STreeConfig; static;

  end;

implementation

{ TBlake2STreeConfig }

class function TBlake2STreeConfig.CreateInterleaved(parallelism: Int32)
  : IBlake2STreeConfig;
begin
  result := TBlake2STreeConfig.Create();
  result.FanOut := parallelism;
  result.MaxHeight := 2;
  result.IntermediateHashSize := 32;
end;

function TBlake2STreeConfig.GetFanOut: Int32;
begin
  result := FFanOut;
end;

function TBlake2STreeConfig.GetIntermediateHashSize: Int32;
begin
  result := FIntermediateHashSize;
end;

function TBlake2STreeConfig.GetLeafSize: Int64;
begin
  result := FLeafSize;
end;

function TBlake2STreeConfig.GetMaxHeight: Int32;
begin
  result := FMaxHeight;
end;

procedure TBlake2STreeConfig.SetFanOut(value: Int32);
begin
  FFanOut := value;
end;

procedure TBlake2STreeConfig.SetIntermediateHashSize(value: Int32);
begin
  FIntermediateHashSize := value;
end;

procedure TBlake2STreeConfig.SetLeafSize(value: Int64);
begin
  FLeafSize := value;
end;

procedure TBlake2STreeConfig.SetMaxHeight(value: Int32);
begin
  FMaxHeight := value;
end;

constructor TBlake2STreeConfig.Create;
begin
  Inherited Create();
  FIntermediateHashSize := 32;
end;

end.
