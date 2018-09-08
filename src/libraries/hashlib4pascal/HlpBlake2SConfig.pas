unit HlpBlake2SConfig;

{$I HashLib.inc}

interface

uses
  HlpIBlake2SConfig,
  HlpHashSize,
  HlpHashLibTypes;

resourcestring
  SInvalidHashSize =
    'BLAKE2S HashSize must be restricted to one of the following [16, 20, 28, 32], "%d"';

type

  TBlake2SConfig = class sealed(TInterfacedObject, IBlake2SConfig)

  strict private

  var

    FHashSize: Int32;
    FPersonalisation, FSalt, FKey: THashLibByteArray;

    procedure ValidateHashSize(AHashSize: Int32); inline;

    function GetPersonalisation: THashLibByteArray; inline;
    procedure SetPersonalisation(const value: THashLibByteArray); inline;

    function GetSalt: THashLibByteArray; inline;
    procedure SetSalt(const value: THashLibByteArray); inline;

    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const value: THashLibByteArray); inline;

    function GetHashSize: Int32; inline;
    procedure SetHashSize(value: Int32); inline;

  public
    constructor Create(AHashSize: THashSize = THashSize.hsHashSize256);
    property Personalisation: THashLibByteArray read GetPersonalisation
      write SetPersonalisation;
    property Salt: THashLibByteArray read GetSalt write SetSalt;
    property Key: THashLibByteArray read GetKey write SetKey;
    property HashSize: Int32 read GetHashSize write SetHashSize;

  end;

implementation

{ TBlake2SConfig }

procedure TBlake2SConfig.ValidateHashSize(AHashSize: Int32);
begin
  if not((AHashSize) in [16, 20, 28, 32]) then
  begin
    raise EArgumentHashLibException.CreateResFmt(@SInvalidHashSize,
      [Int32(AHashSize)]);
  end;
end;

function TBlake2SConfig.GetHashSize: Int32;
begin
  result := FHashSize;
end;

function TBlake2SConfig.GetKey: THashLibByteArray;
begin
  result := FKey;
end;

function TBlake2SConfig.GetPersonalisation: THashLibByteArray;
begin
  result := FPersonalisation;
end;

function TBlake2SConfig.GetSalt: THashLibByteArray;
begin
  result := FSalt;
end;

procedure TBlake2SConfig.SetHashSize(value: Int32);
begin
  ValidateHashSize(value);
  FHashSize := value;
end;

procedure TBlake2SConfig.SetKey(const value: THashLibByteArray);
begin
  FKey := value;
end;

procedure TBlake2SConfig.SetPersonalisation(const value: THashLibByteArray);
begin
  FPersonalisation := value;
end;

procedure TBlake2SConfig.SetSalt(const value: THashLibByteArray);
begin
  FSalt := value;
end;

constructor TBlake2SConfig.Create(AHashSize: THashSize);
var
  LHashSize: Int32;
begin
  Inherited Create();
  LHashSize := Int32(AHashSize);
  ValidateHashSize(LHashSize);
  FHashSize := LHashSize;
end;

end.
