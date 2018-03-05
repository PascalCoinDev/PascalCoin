unit UBaseTypes;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2017 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

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
  end;

implementation

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

class function TBaseType.Compare(const leftBytes, rightBytes: T32Bytes): Integer;
var i : Integer;
begin
  for i:=0 to 31 do begin
    Result := leftBytes[i] - rightBytes[i];
    if Result<>0 then exit;
  end;
end;

end.

