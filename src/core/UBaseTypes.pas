unit UBaseTypes;

{ Copyright (c) 2017-19 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.


  This Unit will define basic Types of data for PascalCoin
  This unit must not need other units from PascalCoin, is a "top" unit

}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

{$I ./../config.inc}

Type
  {$IFDEF NO_ANSISTRING}
    // When Delphi + Android, then no ANSISTRING available
    PAnsiChar = PByte;
    PPAnsiChar = ^PAnsiChar;
  {$ENDIF}


  // Raw data in a maximum 65k bytes
  TDynRawBytes = TBytes;
  // Raw data in a maximum 256 bytes
  T256RawBytes = TDynRawBytes;

  // Fixed 32 bytes length (or empty)
  T32Bytes = Array[0..31] of byte;

  // Fixed 20 bytes length
  T20Bytes = Array[0..19] of byte;

  TRawBytes = TBytes;

  { TRawBytesHelper }

  TRawBytesHelper = record helper for TRawBytes
    function ToString : String; // Returns a String type
    function ToPrintable : String; // Returns a printable string with chars from #32..#126, other chars will be printed as #126 "~"
    function ToHexaString : String; // Returns an Hexastring, so each byte will be printed as an hexadecimal (double size)
    procedure FromString(const AValue : String); // Will store a RAW bytes assuming each char of the string is a byte -> ALERT: Do not use when the String contains chars encoded with multibyte character set!
    function Add(const ARawValue : TRawBytes) : TRawBytes; // Will concat a new RawBytes value to current value
    function IsEmpty : Boolean; // Will return TRUE when Length = 0
    function IsEqualTo(const ACompareTo : TRawBytes) : Boolean;
    //
    procedure FromStream(AStream : TStream); overload;
    procedure FromStream(AStream : TStream; AStartPos, ALength : Integer); overload;
    //
    procedure SaveInsideTBytes(var ADestToWrite: TBytes; var AStartPosition: integer);    // AStartPosition will update to next position
    function LoadFromTBytes(const ASource: TBytes; var AStartPosition: integer): boolean; // AStartPosition will update to next position
    function ToSerialized : TBytes; overload;
    procedure ToSerialized(const AStream : TStream); overload;
    function FromSerialized(const ASerialized : TBytes; ACheckLength : Integer = 0) : Boolean; overload;
    function FromSerialized(const AStream : TStream; ACheckLength : Integer = 0) : Integer; overload;
    function GetSerializedLength: integer; // 2 + Length
  end;


  { TBytesBuffer }

  TBytesBuffer = Class
  private
    FBytes : TBytes;
    FDefaultIncrement: Integer;
    FUsedBytes : Integer;
    procedure IncreaseSize(newSize : Integer);
    procedure SetDefaultIncrement(AValue: Integer);
  protected
    procedure NotifyUpdated(AStartPos, ACountBytes : Integer); virtual;
  public
    constructor Create(ADefaultIncrement : Integer); virtual;
    constructor CreateCopy(ABytesBuffer : TBytesBuffer);
    destructor Destroy; override;
    function Length : Integer;
    function Add(const buffer : TBytes) : Integer; overload;
    function Add(const buffer; bufferSize : Integer) : Integer; overload;
    function Replace(startPos : Integer; const buffer : TBytes) : Integer; overload;
    function Replace(startPos : Integer; const buffer; bufferSize : Integer) : Integer; overload;
    property DefaultIncrement : Integer read FDefaultIncrement write SetDefaultIncrement;
    function Compare(ABytesBuffer : TBytesBuffer) : Integer; overload;
    function Compare(AStream : TStream) : Integer; overload;
    procedure SetLength(ANewLength : Integer);
    function Memory : Pointer;
    function MemoryLength : Integer;
    procedure Clear;
    procedure CopyFrom(ABytesBuffer : TBytesBuffer);
    function Capture(AStartPos, ALength : Integer) : TBytes;
    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);
  end;


  { TBaseType }

  TBaseType = Class
  public
    class procedure T32BytesToRawBytes(const source : T32Bytes; var dest : TDynRawBytes); overload;
    class function T32BytesToRawBytes(const source : T32Bytes) : TDynRawBytes; overload;
    class procedure T20BytesToRawBytes(const source : T20Bytes; var dest : TDynRawBytes); overload;
    class function T20BytesToRawBytes(const source : T20Bytes) : TDynRawBytes; overload;
    class function TRawBytesTo32Left0Padded(const source : TDynRawBytes) : T32Bytes;
    class function To256RawBytes(const source : TRawBytes) : T256RawBytes; overload;
    class procedure To256RawBytes(const source : TRawBytes; var dest : T256RawBytes); overload;
    class function ToRawBytes(const source : T256RawBytes) : TRawBytes; overload;
    class procedure ToRawBytes(const source : T256RawBytes; var dest: TRawBytes); overload;
    class function ToRawBytes(const source : T32Bytes) : TRawBytes; overload;
    class procedure ToRawBytes(const source : T32Bytes; var dest: TRawBytes); overload;
    class function To32Bytes(const source : TRawBytes) : T32Bytes; overload;
    class procedure To32Bytes(const source : TRawBytes; var dest: T32Bytes); overload;
    class function To20Bytes(const source : TRawBytes) : T20Bytes; overload;
    class procedure To20Bytes(const source : TRawBytes; var dest: T20Bytes); overload;
    class procedure Fill0(var dest : T32Bytes);
    class function IsEmpty(const value : T32Bytes) : Boolean;
    class procedure Concat(const addBytes : T32Bytes; var target : TDynRawBytes); overload;
    class procedure Concat(const leftBytes,rightBytes : T32Bytes; var target : TDynRawBytes); overload;
    class procedure Concat(const leftBytes,rightBytes : TDynRawBytes; var target : TDynRawBytes); overload;
    class function Equals(const v1,v2 : T32Bytes) : Boolean; overload;
    class function Equals(const v1,v2 : TDynRawBytes) : Boolean; overload;
    class function Higher(const vHigh,vLow : T32Bytes) : Boolean;
    class function Compare(const leftBytes,rightBytes : T32Bytes) : Integer; overload;
    class function FindIn(const subst, target : TRawBytes) : Integer;
    class function StartsWith(const subst, target : TRawBytes) : Boolean;
    // Herman functions moved from "Common"
    { Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
    class function BinStrComp(const Str1, Str2 : TRawBytes): Integer;
  end;

  // TickCount is platform specific (32 or 64 bits)
  TTickCount = {$IFDEF CPU64}QWord{$ELSE}Cardinal{$ENDIF};


  TPlatform = Class
  public
    class function GetTickCount : TTickCount;
    class function GetElapsedMilliseconds(Const previousTickCount : TTickCount) : Int64;
  End;

{$IFDEF NO_ANSISTRING}
function StrPas(ptrAnsiChar : PAnsiChar) : String;
{$ENDIF}

implementation

{$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
Uses windows;
  {$ENDIF}
{$ENDIF}

{$IFDEF NO_ANSISTRING}
function StrPas(ptrAnsiChar : PAnsiChar) : String;
var LRaw : TRawBytes;
  LPtr : PByte;
  LByte : Byte;
begin
  SetLength(LRaw,0);
  LPtr := ptrAnsiChar;
  repeat
    LByte := Byte(LPtr^);
    if LByte<>0 then begin
      SetLength(LRaw,Length(LRaw)+1);
      LRaw[High(LRaw)] := LByte;
    end;
    LPtr := LPtr + 1;
  until (LByte=0);
  Result := LRaw.ToString;
end;
{$ENDIF}

{ TRawBytesHelper }

function TRawBytesHelper.ToPrintable: String;
var i,inc_i : Integer;
  rbs : RawByteString; //
begin
  SetLength(rbs,Length(Self));
  inc_i := Low(rbs) - Low(Self);
  for i:=Low(Self) to High(Self) do begin
    if (Self[i] in [32..126]) then move(Self[i],rbs[i+inc_i],1)
    else rbs[i+inc_i] := Chr(126);
  end;
  Result := rbs;
end;

function TRawBytesHelper.ToString: String;
begin
  if Length(Self)>0 then begin
    Result := TEncoding.ANSI.GetString(Self);
  end else Result := '';
end;

function TRawBytesHelper.FromSerialized(const ASerialized: TBytes; ACheckLength : Integer = 0): Boolean;
var Lsize: integer;
begin
  if (Length(ASerialized)<2) then Exit(False);
  Lsize := 0;
  Move(ASerialized[0],Lsize,2);
  if (2 + Lsize > Length(ASerialized)) then Exit(False);
  SetLength(Self,Lsize);
  Move(ASerialized[2],Self[0],Lsize);
  Result := True;
end;

function TRawBytesHelper.FromSerialized(const AStream: TStream; ACheckLength : Integer = 0): Integer;
Var w: Word;
begin
  if AStream.Size - AStream.Position < 2 then begin
    SetLength(Self,0);
    Result := -1;
    Exit;
  end;
  AStream.Read(w, 2);
  if (AStream.Size - AStream.Position < w) OR ((ACheckLength > 0) AND (w <> ACheckLength)) then begin
    AStream.Position := AStream.Position - 2; // Go back!
    SetLength(Self,0);
    Result := -1;
    Exit;
  end;
  SetLength(Self, w);
  if (w>0) then begin
    AStream.ReadBuffer(Self[0], w);
  end;
  Result := w+2;
end;

procedure TRawBytesHelper.FromStream(AStream: TStream; AStartPos, ALength: Integer);
begin
  System.SetLength(Self,ALength);
  AStream.Position := AStartPos;
  AStream.Read(Self[0],ALength);
end;

procedure TRawBytesHelper.SaveInsideTBytes(var ADestToWrite: TBytes; var AStartPosition: integer);
var Lsize: integer;
begin
  if (AStartPosition + Length(Self) + 2) > Length(ADestToWrite) then begin
    SetLength(ADestToWrite,AStartPosition + Length(Self) + 2);
  end;
  Lsize := Length(Self);
  Move(Lsize, ADestToWrite[AStartPosition], 2);
  Move(Self[0], ADestToWrite[AStartPosition + 2], Lsize);
  Inc(AStartPosition, 2 + Lsize);
end;

function TRawBytesHelper.LoadFromTBytes(const ASource: TBytes; var AStartPosition: integer): boolean;
var Lsize: integer;
begin
  Lsize := 0;
  if (AStartPosition + 2 > Length(ASource)) then Exit(False);
  Move(ASource[AStartPosition],Lsize,2);
  if (AStartPosition + 2 + Lsize > Length(ASource)) then Exit(False);
  SetLength(Self,Lsize);
  Move(ASource[AStartPosition + 2],Self[0],Lsize);
  inc(AStartPosition, 2 + Lsize);
  Result := True;
end;

function TRawBytesHelper.ToSerialized: TBytes;
var Lsize: integer;
begin
  LSize := Length(Self);
  if LSize>65536 then raise Exception.Create('Cannot serialize TBytes due high length '+IntToStr(Length(Self)));
  SetLength(Result, LSize + 2);
  Move(Lsize, Result[0], 2);
  Move(Self[0], Result[2], Lsize);
end;

procedure TRawBytesHelper.ToSerialized(const AStream: TStream);
var LSize : Integer;
begin
  LSize := Length(Self);
  if LSize>65536 then raise Exception.Create('Cannot serialize TBytes due high length '+IntToStr(Length(Self)));
  AStream.Write(LSize,2);
  AStream.Write(Self[0],LSize);
end;

function TRawBytesHelper.GetSerializedLength: integer;
begin
  Result := 2 + Length(Self);
end;

procedure TRawBytesHelper.FromStream(AStream: TStream);
begin
  Self.FromStream(AStream,0,AStream.Size);
end;

procedure TRawBytesHelper.FromString(const AValue: String);
var i : Integer;
begin
  SetLength(Self,Length(AValue));
  for i := 0 to Length(AValue)-1 do begin
    Self[i] := Byte(AValue.Chars[i]);
  end;
end;

function TRawBytesHelper.Add(const ARawValue: TRawBytes): TRawBytes;
var iNext : Integer;
begin
  iNext := Length(Self);
  SetLength(Self,Length(Self)+Length(ARawValue));
  move(ARawValue[0],Self[iNext],Length(ARawValue));
  Result := Self;
end;

function TRawBytesHelper.IsEmpty: Boolean;
begin
  Result := Length(Self)=0;
end;

function TRawBytesHelper.IsEqualTo(const ACompareTo: TRawBytes): Boolean;
begin
  if (Length(Self)=Length(ACompareTo)) and (Length(Self)>0) then
    Result := (CompareMem(@Self[0],@ACompareTo[0],Length(Self)))
  else Result := False;
end;

function TRawBytesHelper.ToHexaString: String;
Var i : Integer;
  rbs : RawByteString;
  raw_as_hex : TRawBytes;
begin
  SetLength(raw_as_hex,length(Self)*2);
  for i := Low(Self) to High(Self) do begin
    rbs := IntToHex(Self[i],2);
    move(rbs[Low(rbs)],raw_as_hex[i*2],1);
    move(rbs[Low(rbs)+1],raw_as_hex[(i*2)+1],1);
  end;
  Result := raw_as_hex.ToString;
end;

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

class function TBaseType.T20BytesToRawBytes(const source: T20Bytes): TDynRawBytes;
begin
  T20BytesToRawBytes(source,Result);
end;

class procedure TBaseType.T20BytesToRawBytes(const source: T20Bytes; var dest: TDynRawBytes);
begin
  SetLength(dest,20);
  Move(source[0],dest[0],20);
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

class function TBaseType.To256RawBytes(const source: TRawBytes): T256RawBytes;
begin
  SetLength(Result,Length(source));
  move(source[Low(source)],Result[0],Length(source));
end;

class procedure TBaseType.To20Bytes(const source: TRawBytes; var dest: T20Bytes);
var i : Integer;
begin
  FillByte(dest[0],20,0);
  i := Length(source);
  if (i>20) then i:=20;
  move(source[Low(source)],dest[0],i);
end;

class function TBaseType.To20Bytes(const source: TRawBytes): T20Bytes;
begin
  To20Bytes(source,Result);
end;

class procedure TBaseType.To256RawBytes(const source: TRawBytes; var dest: T256RawBytes);
begin
  SetLength(dest,Length(source));
  move(source[Low(source)],dest[0],Length(source));
end;

class function TBaseType.ToRawBytes(const source: T256RawBytes): TRawBytes;
begin
  SetLength(Result,Length(source));
  move(source[0],Result[Low(Result)],Length(source));
end;

class procedure TBaseType.ToRawBytes(const source: T256RawBytes; var dest: TRawBytes);
begin
  SetLength(dest,Length(source));
  move(source[0],dest[Low(dest)],Length(source));
end;

class function TBaseType.ToRawBytes(const source: T32Bytes): TRawBytes;
begin
  SetLength(Result,Length(source));
  move(source[0],Result[Low(Result)],Length(source));
end;

class procedure TBaseType.ToRawBytes(const source: T32Bytes; var dest: TRawBytes);
begin
  SetLength(dest,Length(source));
  move(source[0],dest[Low(dest)],Length(source));
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
  move(source[Low(source)],dest[0],i);
end;

class procedure TBaseType.Fill0(var dest: T32Bytes);
begin
  FillByte(dest[0],32,0);
end;

class function TBaseType.FindIn(const subst, target: TRawBytes): Integer;
var iSubs, iTarget : Integer;
begin
  Result := -1;
  for iTarget := 0 to (High(target)-Length(subst)+1) do begin
    iSubs := 0;
    while (iSubs<=High(subst)) and (target[iSubs+iTarget]=subst[iSubs]) do inc(iSubs);
    if (iSubs>High(subst)) then begin
      Result := iTarget;
      Exit;
    end;
  end;
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

class function TBaseType.StartsWith(const subst, target: TRawBytes): Boolean;
var i : Integer;
begin
  if (Length(target)<Length(subst)) then begin
    Exit(False);
  end;
  i := Low(subst);
  while (i<=High(subst)) and (i<=High(target)) and (target[i]=subst[i]) do inc(i);
  Result := (i>High(subst));
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

class procedure TBaseType.Concat(const leftBytes, rightBytes: TDynRawBytes; var target: TDynRawBytes);
begin
  SetLength(target,Length(leftBytes)+Length(rightBytes));
  move(leftBytes[Low(leftBytes)],target[0],Length(leftBytes));
  move(rightBytes[Low(rightBytes)],target[High(leftBytes)+1],Length(rightBytes));
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
begin
  If Length(v1)<>Length(v2) then begin
    Result := False;
  end else if Length(v1)=0 then begin
    Result := True;
  end else begin
    Result := CompareMem(@v1[Low(v1)],@v2[Low(v2)],Length(v1));
  end;
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

class function TBaseType.BinStrComp(const Str1, Str2: TRawBytes): Integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if ((Str1Len=0) and (Str2Len=0)) or (@Str1[Low(Str1)] = @Str2[Low(Str2)]) then
     Result := 0
   else begin
     Result := 0;
     i := 0;
     while (i<Length(Str1)) and (i<Length(Str2)) do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         Break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         Break;
       end;
       inc(i);
     end;
     if (Result=0) then begin
       if (Str1Len < Str2Len) then
         Result := -1
       else if (Str1Len > Str2Len) then
         Result := 1;
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
  Result := {$IFDEF CPU64}GetTickCount64{$ELSE}
   {$IFDEF FPC}SysUtils.GetTickCount{$ELSE}
     {$IFDEF MSWINDOWS}Windows.GetTickCount{$ELSE}
     TThread.GetTickCount;
     {$ENDIF}
   {$ENDIF}
  {$ENDIF}
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
    FillByte(FBytes[actSize],auxSize - actSize ,0);
  end;
end;

procedure TBytesBuffer.SaveToStream(AStream: TStream);
begin
  AStream.Write(FBytes[0],Self.Length);
end;

procedure TBytesBuffer.SetDefaultIncrement(AValue: Integer);
begin
  if AValue<=0 then FDefaultIncrement:=1024
  else if AValue>(100*1024*1024) then FDefaultIncrement := (100*1024*1024)
  else FDefaultIncrement:=AValue;
end;

procedure TBytesBuffer.SetLength(ANewLength: Integer);
var LLastUsed : Integer;
begin
  if ANewLength<0 then raise Exception.Create(Format('Invalid new Length value %d at %s',[ANewLength,ClassName]));
  IncreaseSize(ANewLength);
  LLastUsed := FUsedBytes;
  FUsedBytes := ANewLength;
  if FUsedBytes>=LLastUsed then begin
    NotifyUpdated(LLastUsed,FUsedBytes-LLastUsed);
  end else begin
    NotifyUpdated(0,FUsedBytes);
  end;
end;

function TBytesBuffer.Add(const buffer; bufferSize: Integer): Integer;
begin
  Result := Replace(Length,buffer,bufferSize);
end;

function TBytesBuffer.Add(const buffer: TBytes): Integer;
begin
  Result := Replace(Length,buffer);
end;

function TBytesBuffer.Capture(AStartPos, ALength: Integer): TBytes;
var LLength : Integer;
begin
  if AStartPos+ALength <= Self.Length then LLength := ALength
  else LLength := Self.Length - AStartPos;
  if (LLength<0) or (LLength>Self.Length) or (AStartPos<0) or (ALength<0) then raise Exception.Create(Format('Invalid Capture start %d length %d for a %d buffer',
    [AStartPos,ALength,Self.Length]));
  System.SetLength(Result,LLength);
  if LLength>0 then
    Move(FBytes[AStartPos],Result[0],LLength);
end;

procedure TBytesBuffer.Clear;
begin
  System.SetLength(FBytes,0);
  FUsedBytes := 0;
  NotifyUpdated(0,FUsedBytes);
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

function TBytesBuffer.Compare(AStream: TStream): Integer;
var Lbb : TBytesBuffer;
begin
  Lbb := TBytesBuffer.Create(DefaultIncrement);
  try
    Lbb.LoadFromStream(AStream);
    Result := Compare(Lbb);
  finally
    Lbb.Free;
  end;
end;

procedure TBytesBuffer.CopyFrom(ABytesBuffer: TBytesBuffer);
begin
  System.SetLength(FBytes,System.Length(ABytesBuffer.FBytes));
  Move(ABytesBuffer.FBytes[0],FBytes[0],System.Length(FBytes));
  FUsedBytes := ABytesBuffer.FUsedBytes;
  FDefaultIncrement := ABytesBuffer.FDefaultIncrement;
  NotifyUpdated(0,FUsedBytes);
end;

constructor TBytesBuffer.Create(ADefaultIncrement: Integer);
begin
  System.SetLength(FBytes,0);
  FUsedBytes:=0;
  SetDefaultIncrement(ADefaultIncrement);
  NotifyUpdated(0,FUsedBytes);
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

procedure TBytesBuffer.LoadFromStream(AStream: TStream);
begin
  AStream.Position := 0;
  IncreaseSize(Self.Length + AStream.Size);
  AStream.Read(FBytes[FUsedBytes],AStream.Size);
  SetLength(Self.Length + AStream.Size);
end;

function TBytesBuffer.Memory: Pointer;
begin
  Result := addr(FBytes[0]);
end;

function TBytesBuffer.MemoryLength: Integer;
begin
  Result := System.Length(FBytes);
end;

procedure TBytesBuffer.NotifyUpdated(AStartPos, ACountBytes: Integer);
begin
  //
end;

function TBytesBuffer.Replace(startPos: Integer; const buffer; bufferSize : Integer): Integer;
begin
  if bufferSize>0 then begin
    IncreaseSize(startPos+1+bufferSize);
    Move(buffer,FBytes[startPos],bufferSize);
    if (startPos + bufferSize)>FUsedBytes then FUsedBytes := (startPos + bufferSize);
    NotifyUpdated(startPos,bufferSize);
  end;
  Result := FUsedBytes;
end;

function TBytesBuffer.Replace(startPos: Integer; const buffer: TBytes): Integer;
begin
  Result := Replace(startPos,buffer[0],System.Length(buffer));
end;


end.

