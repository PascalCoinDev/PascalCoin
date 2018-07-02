unit HlpIBlake2BTreeConfig;

{$I ..\..\Include\HashLib.inc}

interface

type
  IBlake2BTreeConfig = interface(IInterface)
    ['{3EFB1A70-4478-4375-BAF6-EF17B3673DA8}']
    function GetIntermediateHashSize: Int32;
    procedure SetIntermediateHashSize(value: Int32);
    property IntermediateHashSize: Int32 read GetIntermediateHashSize
      write SetIntermediateHashSize;
    function GetMaxHeight: Int32;
    procedure SetMaxHeight(value: Int32);
    property MaxHeight: Int32 read GetMaxHeight write SetMaxHeight;
    function GetLeafSize: Int64;
    procedure SetLeafSize(value: Int64);
    property LeafSize: Int64 read GetLeafSize write SetLeafSize;
    function GetFanOut: Int32;
    procedure SetFanOut(value: Int32);
    property FanOut: Int32 read GetFanOut write SetFanOut;

  end;

implementation

end.
