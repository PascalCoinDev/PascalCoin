unit HlpIBlake2BTreeConfig;

{$I HashLib.inc}

interface

type
  IBlake2BTreeConfig = interface(IInterface)
    ['{3EFB1A70-4478-4375-BAF6-EF17B3673DA8}']

    function GetFanOut: Byte;
    procedure SetFanOut(AValue: Byte);
    property FanOut: Byte read GetFanOut write SetFanOut;

    function GetMaxDepth: Byte;
    procedure SetMaxDepth(AValue: Byte);
    property MaxDepth: Byte read GetMaxDepth write SetMaxDepth;

    function GetNodeDepth: Byte;
    procedure SetNodeDepth(AValue: Byte);
    property NodeDepth: Byte read GetNodeDepth write SetNodeDepth;

    function GetInnerHashSize: Byte;
    procedure SetInnerHashSize(AValue: Byte);
    property InnerHashSize: Byte read GetInnerHashSize write SetInnerHashSize;

    function GetLeafSize: UInt32;
    procedure SetLeafSize(AValue: UInt32);
    property LeafSize: UInt32 read GetLeafSize write SetLeafSize;

    function GetNodeOffset: UInt64;
    procedure SetNodeOffset(AValue: UInt64);
    property NodeOffset: UInt64 read GetNodeOffset write SetNodeOffset;

    function GetIsLastNode: Boolean;
    procedure SetIsLastNode(AValue: Boolean);
    property IsLastNode: Boolean read GetIsLastNode write SetIsLastNode;

  end;

implementation

end.
