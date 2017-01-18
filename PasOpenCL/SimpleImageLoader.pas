(********************************************)
(*                                          *)
(*                DelphiCL                  *)
(*                                          *)
(*      created by      : Maksym Tymkovych  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.07              *)
(*      file name    : SimpleImageLoader.pas*)
(*      last modify     : 10.12.11          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2011*****)
unit SimpleImageLoader;

interface

uses
  CL_platform,
  CL,
  Graphics;

type
  PArrayByte = ^TArrayByte;
  TArrayByte = array [0..0] of TCL_uchar4;
  TImageLoader = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FPtr: PArrayByte;
    FFormat: TCL_image_format;
    procedure SetPointer(const Ptr: Pointer);
    function GetPointer: Pointer;
    function GetFormat: PCL_image_format;
  public
    constructor Create(const FileName: String); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure SaveToFile(const FileName: String);
    procedure Resize(const Width, Height: Integer);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pointer: Pointer read GetPointer write SetPointer;
    property Format: PCL_image_format read GetFormat;
  end;

implementation

{ TImageLoader }

constructor TImageLoader.Create(const FileName: String);
var
  bmp: Graphics.TBitmap;
  i,j: Integer;
  row: PArrayByte;
begin
  inherited Create;
  bmp := TBitmap.Create;
  bmp.LoadFromFile(FileName);
  FWidth := bmp.Width;
  FHeight := bmp.Height;
  bmp.PixelFormat := pf32bit;
  FPtr:=GetMemory(FWidth*FHeight*4);
  for i:=0 to FHeight-1 do
  begin
    row := bmp.ScanLine[i];
    for j:=0 to FWidth-1 do
    begin
        FPtr^[i*FWidth+j].u8 := row^[j].u8;
    end;
  end;
  FFormat.Image_channel_order := CL_BGRA; //BMP - BGRA file
  FFormat.Image_channel_data_type := CL_UNSIGNED_INT8;
  bmp.Free;
end;

constructor TImageLoader.Create;
begin
  inherited Create;

  FFormat.Image_channel_order := CL_BGRA; //BMP - BGRA file
  FFormat.Image_channel_data_type := CL_UNSIGNED_INT8;
end;

destructor TImageLoader.Destroy;
begin
  Dispose(FPtr);
  inherited;
end;

procedure TImageLoader.SaveToFile(const FileName: String);
var
  bmp: TBitmap;
  i,j: integer;
  row: PArrayByte;
begin
  bmp:=TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;
  bmp.PixelFormat := pf32bit;
    for i:=0 to FHeight-1 do
    begin
      row := bmp.ScanLine[i];
      for j:=0 to FWidth-1 do
      begin
        row^[j].u8 := FPtr^[(i*FWidth+j)].u8;
      end;
    end;
  bmp.SaveToFile(FileName);
  bmp.Free;
end;

function TImageLoader.GetFormat: PCL_image_format;
begin
  Result := @FFormat;
end;

function TImageLoader.GetPointer: Pointer;
begin
  Result := FPtr;
end;

procedure TImageLoader.SetPointer(const Ptr: Pointer);
begin
  FPtr := Ptr;
end;

procedure TImageLoader.Resize(const Width, Height: Integer);
begin
  Dispose(FPtr);
  FPtr := GetMemory(FWidth * FHeight * 4);
end;

end.
