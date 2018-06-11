unit HlpIBlake2STreeConfig;

{$I ..\..\Include\HashLib.inc}

interface

type
  IBlake2STreeConfig = interface(IInterface)
    ['{9B0E3927-6C85-46C4-8EFE-609D1FF24030}']
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
