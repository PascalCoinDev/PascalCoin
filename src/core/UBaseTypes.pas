unit UBaseTypes;

{ Copyright (c) 2017 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

Type
  // Raw data in a maximum 65k bytes
  TDynRawBytes = Array of Byte;
  // Raw data in a maximum 256 bytes
  T256RawBytes = TDynRawBytes;

  // Fixed 32 bytes length (or empty)
  T32Bytes = Array[0..31] of byte;

  TRawBytes = AnsiString;

  { TBytesBuffer }

  TBytesBuffer = Class
  private
    FBytes : TBytes;
    FDefaultIncrement: Integer;
    FUsedBytes : Integer;
    procedure IncreaseSize(newSize : Integer);
    procedure SetDefaultIncrement(AValue: Integer);
  public
    constructor Create(ADefaultIncrement : Integer);
    constructor CreateCopy(ABytesBuffer : TBytesBuffer);
    destructor Destroy; override;
    function Length : Integer;
    function Add(const buffer : TBytes) : Integer; overload;
    function Add(var buffer; bufferSize : Integer) : Integer; overload;
    function Replace(startPos : Integer; const buffer : TBytes) : Integer; overload;
    function Replace(startPos : Integer; var buffer; bufferSize : Integer) : Integer; overload;
    property DefaultIncrement : Integer read FDefaultIncrement write SetDefaultIncrement;
    function Compare(ABytesBuffer : TBytesBuffer) : Integer;
    procedure SetLength(ANewLength : Integer);
    function Memory : Pointer;
    procedure Clear;
    procedure CopyFrom(ABytesBuffer : TBytesBuffer);
  end;


  { TBaseType }

  TBaseType = Class
  public
    class procedure T32BytesToRawBytes(const source : T32Bytes; var dest : TDynRawBytes); overload;
    class function T32BytesToRawBytes(const source : T32Bytes) : TDynRawBytes; overload;
    class function TRawBytesTo32Left0Padded(const source : TDynRawBytes) : T32Bytes;
    class function Copy(const source : T32bytes; start, length : Integer) : ShortString; overload;
    class function Copy(const source : T256RawBytes; var dest : T256RawBytes) : ShortString; overload;
    class function To256RawBytes(const source : TRawBytes) : T256RawBytes; overload;
    class procedure To256RawBytes(const source : TRawBytes; var dest : T256RawBytes); overload;
    class function ToRawBytes(const source : T256RawBytes) : TRawBytes; overload;
    class procedure ToRawBytes(const source : T256RawBytes; var dest: TRawBytes); overload;
    class function ToRawBytes(const source : T32Bytes) : TRawBytes; overload;
    class procedure ToRawBytes(const source : T32Bytes; var dest: TRawBytes); overload;
    class function To32Bytes(const source : TRawBytes) : T32Bytes; overload;
    class procedure To32Bytes(const source : TRawBytes; var dest: T32Bytes); overload;
    class procedure Fill0(var dest : T32Bytes);
    class function IsEmpty(const value : T32Bytes) : Boolean;
    class procedure Concat(const addBytes : T32Bytes; var target : TDynRawBytes); overload;
    class procedure Concat(const leftBytes,rightBytes : T32Bytes; var target : TDynRawBytes); overload;
    class function Equals(const v1,v2 : T32Bytes) : Boolean; overload;
    class function Equals(const v1,v2 : TDynRawBytes) : Boolean; overload;
    class function Higher(const vHigh,vLow : T32Bytes) : Boolean;
    class function Compare(const leftBytes,rightBytes : T32Bytes) : Integer;
    // Herman functions moved from "Common"
    { Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
    class function BinStrComp(const Str1, Str2 : AnsiString): Integer;
  end;

  // TickCount is platform specific (32 or 64 bits)
  TTickCount = {$IFDEF CPU64}QWord{$ELSE}Cardinal{$ENDIF};


  TPlatform = Class
  public
    class function GetTickCount : TTickCount;
    class function GetElapsedMilliseconds(Const previousTickCount : TTickCount) : Int64;
  End;

  //HS -- removed, TNotifyManyEvent in UCommon.pas
  {TNotifyEventToMany = Class
  private
    FList : Array of TNotifyEvent;
  public
    function IndexOf(search : TNotifyEvent) : Integer;
    procedure Add(newNotifyEvent : TNotifyEvent);
    procedure Remove(removeNotifyEvent : TNotifyEvent);
    procedure Invoke(sender : TObject);
    function Count : Integer;
    procedure Delete(index : Integer);
    Constructor Create;
  End;}


implementation

{$IFNDEF FPC}
Uses windows;
{$ENDIF}

{ TBaseType }

{$IFnDEF FPC}
procedure FillByte(var X; count : Integer; value : Byte);
begin
  FillChar(X,count,value);
end;
{$ENDIF}

class procedure TBaseType.T32BytesToRawBytes(const source: T32Bytes; var dest: TDynRawBytes);
begin
  SetLength(dest,32);
  Move(source[0],dest[0],32);
end;

class function TBaseType.T32BytesToRawBytes(const source: T32Bytes): TDynRawBytes;
begin
  T32BytesToRawBytes(source,Result);
end;

class function TBaseType.TRawBytesTo32Left0Padded(const source: TDynRawBytes): T32Bytes;
var i : Integer;
begin
  FillByte(Result,32,0);
  i := 0;
  while (i<32) And (i<=high(source)) do begin
    Result[i+32-length(source)] := source[i];
    inc(i);
  end;
end;

class function TBaseType.Copy(const source: T32bytes; start, length: Integer): ShortString;
begin
  if (length+start)>32 then raise Exception.Create('ERROR DEV 20170601-1');
  SetLength(Result,length);
  move(source[start],Result[1],length);
end;

class function TBaseType.Copy(const source: T256RawBytes; var dest: T256RawBytes): ShortString;
var i : Integer;
begin
  SetLength(dest,length(source));
  for i:=0 to high(dest) do begin
    dest[i] := source[i];
  end;
end;

class function TBaseType.To256RawBytes(const source: TRawBytes): T256RawBytes;
begin
  SetLength(Result,length(source));
  move(source[1],Result[0],length(source));
end;

class procedure TBaseType.To256RawBytes(const source: TRawBytes; var dest: T256RawBytes);
begin
  SetLength(dest,length(source));
  move(source[1],dest[0],length(source));
end;

class function TBaseType.ToRawBytes(const source: T256RawBytes): TRawBytes;
begin
  SetLength(Result,length(source));
  move(source[0],Result[1],length(source));
end;

class procedure TBaseType.ToRawBytes(const source: T256RawBytes; var dest: TRawBytes);
begin
  SetLength(dest,length(source));
  move(source[0],dest[1],length(source));
end;

class function TBaseType.ToRawBytes(const source: T32Bytes): TRawBytes;
begin
  SetLength(Result,length(source));
  move(source[0],Result[1],length(source));
end;

class procedure TBaseType.ToRawBytes(const source: T32Bytes; var dest: TRawBytes);
begin
  SetLength(dest,length(source));
  move(source[0],dest[1],length(source));
end;

class function TBaseType.To32Bytes(const source: TRawBytes): T32Bytes;
begin
  To32Bytes(source,Result);
end;

class procedure TBaseType.To32Bytes(const source: TRawBytes; var dest: T32Bytes);
var i : Integer;
begin
  FillByte(dest[0],32,0);
  i := length(source);
  if (i>32) then i:=32;
  move(source[1],dest[0],i);
end;

class procedure TBaseType.Fill0(var dest: T32Bytes);
begin
  FillByte(dest[0],32,0);
end;

class function TBaseType.IsEmpty(const value: T32Bytes): Boolean;
Var i : Integer;
begin
  For i:=0 to 31 do begin
    if value[i]<>0 then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

class procedure TBaseType.Concat(const addBytes: T32Bytes; var target: TDynRawBytes);
begin
  SetLength(target,length(target)+32);
  move(addBytes,target[length(target)-32],32);
end;

class procedure TBaseType.Concat(const leftBytes, rightBytes: T32Bytes; var target: TDynRawBytes);
begin
  SetLength(target,64);
  move(leftBytes,target[0],32);
  move(rightBytes,target[32],32);
end;

class function TBaseType.Equals(const v1, v2: T32Bytes) : Boolean;
Var i : Integer;
begin
  for i:=0 to 31 do begin
    If v1[i]<>v2[i] then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

class function TBaseType.Equals(const v1, v2: TDynRawBytes): Boolean;
Var i : Integer;
begin
  If Length(v1)<>Length(v2) then begin
    Result := False;
    Exit;
  end;
  for i:=0 to high(v1) do begin
    If v1[i]<>v2[i] then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

class function TBaseType.Higher(const vHigh, vLow: T32Bytes): Boolean;
Var i : Integer;
begin
  for i:=0 to 31 do begin
    If vHigh[i]<vLow[i] then begin
      Result := False;
      Exit;
    end else if vHigh[i]>vLow[i] then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False; // No higher, equal
end;

class function TBaseType.BinStrComp(const Str1, Str2: AnsiString): Integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if (Str1Len < Str2Len) then
     Result := -1
   else if (Str1Len > Str2Len) then
     Result := 1
   else begin
     Result := 0;
     for i:= 1 to Str1Len do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
End;

class function TBaseType.Compare(const leftBytes, rightBytes: T32Bytes): Integer;
var i : Integer;
begin
  for i:=0 to 31 do begin
    Result := leftBytes[i] - rightBytes[i];
    if Result<>0 then exit;
  end;
end;

{ TPlatform }

class function TPlatform.GetElapsedMilliseconds(const previousTickCount: TTickCount): Int64;
begin
  Result := (Self.GetTickCount - previousTickCount);
end;

class function TPlatform.GetTickCount: TTickCount;
begin
  Result := {$IFDEF CPU64}GetTickCount64{$ELSE}{$IFNDEF FPC}Windows.{$ELSE}SysUtils.{$ENDIF}GetTickCount{$ENDIF};
end;

{ TBytesBuffer }

procedure TBytesBuffer.IncreaseSize(newSize: Integer);
var actSize, auxSize : Integer;
begin
  actSize := System.Length(FBytes);
  If actSize<newSize then begin
    auxSize := actSize;
    repeat
      auxSize := auxSize + FDefaultIncrement;
    until auxSize >= newSize;
    System.SetLength(FBytes,auxSize);
    FillByte(FBytes[actSize],auxSize - actSize +1,0);
  end;
end;

procedure TBytesBuffer.SetDefaultIncrement(AValue: Integer);
begin
  if AValue<0 then FDefaultIncrement:=1024
  else if AValue>(1024*1024) then FDefaultIncrement := 1024*1024
  else FDefaultIncrement:=AValue;
end;

procedure TBytesBuffer.SetLength(ANewLength: Integer);
begin
  if ANewLength<0 then raise Exception.Create(Format('Invalid new Length value %d at %s',[ANewLength,ClassName]));
  IncreaseSize(ANewLength);
  FUsedBytes := ANewLength;
end;

function TBytesBuffer.Add(var buffer; bufferSize: Integer): Integer;
begin
  Result := Replace(Length,buffer,bufferSize);
end;

function TBytesBuffer.Add(const buffer: TBytes): Integer;
begin
  Result := Replace(Length,buffer);
end;

procedure TBytesBuffer.Clear;
begin
  System.SetLength(FBytes,0);
  FUsedBytes := 0;
end;

function TBytesBuffer.Compare(ABytesBuffer: TBytesBuffer): Integer;
var i : Integer;
begin
  Result := 0;
  if ABytesBuffer=Self then Exit;
  if Length<ABytesBuffer.Length then Result := -1
  else if Length>ABytesBuffer.Length then Result := 1
  else begin
    i := 0;
    while (Result=0) And (i<(Length)) do begin
      Result := Self.FBytes[i] - ABytesBuffer.FBytes[i];
      inc(i);
    end;
  end;
end;

procedure TBytesBuffer.CopyFrom(ABytesBuffer: TBytesBuffer);
begin
  System.SetLength(FBytes,System.Length(ABytesBuffer.FBytes));
  Move(ABytesBuffer.FBytes[0],FBytes[0],System.Length(FBytes));
  FUsedBytes := ABytesBuffer.FUsedBytes;
  FDefaultIncrement := ABytesBuffer.FDefaultIncrement;
end;

constructor TBytesBuffer.Create(ADefaultIncrement: Integer);
begin
  System.SetLength(FBytes,0);
  FUsedBytes:=0;
  SetDefaultIncrement(ADefaultIncrement);
end;

constructor TBytesBuffer.CreateCopy(ABytesBuffer: TBytesBuffer);
begin
  Create(ABytesBuffer.DefaultIncrement);
  CopyFrom(ABytesBuffer);
end;

destructor TBytesBuffer.Destroy;
begin
  System.SetLength(FBytes,0);
  FUsedBytes:=0;
  inherited Destroy;
end;

function TBytesBuffer.Length: Integer;
begin
  Result := FUsedBytes;
end;

function TBytesBuffer.Memory: Pointer;
begin
  Result := addr(FBytes[0]);
end;

function TBytesBuffer.Replace(startPos: Integer; var buffer; bufferSize : Integer): Integer;
begin
  IncreaseSize(startPos+1+bufferSize);
  Move(buffer,FBytes[startPos],bufferSize);
  if (startPos + bufferSize)>FUsedBytes then FUsedBytes := (startPos + bufferSize);
  Result := FUsedBytes;
end;

function TBytesBuffer.Replace(startPos: Integer; const buffer: TBytes): Integer;
begin
  Result := Replace(startPos,buffer[0],System.Length(buffer));
end;


end.

