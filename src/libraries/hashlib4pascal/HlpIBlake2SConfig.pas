unit HlpIBlake2SConfig;

{$I HashLib.inc}

interface

uses
  HlpHashLibTypes;

type
  IBlake2SConfig = interface(IInterface)
    ['{C78DE94A-0290-467D-BE26-D0AD1639076C}']
    function GetPersonalisation: THashLibByteArray;
    procedure SetPersonalisation(const AValue: THashLibByteArray);
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    function GetSalt: THashLibByteArray;
    procedure SetSalt(const AValue: THashLibByteArray);
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    function GetKey: THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);
    property Key: THashLibByteArray read GetKey write SetKey;
    function GetHashSize: Int32;
    procedure SetHashSize(AValue: Int32);
    property HashSize: Int32 read GetHashSize write SetHashSize;

  end;

implementation

end.
