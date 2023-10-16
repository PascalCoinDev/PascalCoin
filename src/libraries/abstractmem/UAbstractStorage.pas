unit UAbstractStorage;

{
  This file is part of AbstractMem framework

  Copyright (C) 2023 Albert Molina - bpascalblockchain@gmail.com

  https://github.com/PascalCoinDev/

  *** BEGIN LICENSE BLOCK *****

  The contents of this files are subject to the Mozilla Public License Version
  2.0 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Albert Molina.

  See ConfigAbstractMem.inc file for more info

  ***** END LICENSE BLOCK *****
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  SyncObjs,
  UAbstractMem,
  UFileMem,
  UAbstractMemTList,
  UAbstractMemBTree;

{$I ./ConfigAbstractMem.inc }

type
  EAbstractStorage = Class(Exception);

  TStorageStructType = (
    ss_Unknown,
    ss_Buffer,
    ss_TAbstractMemBTree,
    ss_TAbstractMemBTreeData,
    ss_TAbstractMemBTreeDataIndex,
    ss_TAbstractMemTList,
    ss_TAbstractMemTListT,
    ss_TAbstractMemOrderedTListT
    );

  TStorageStructInfo = record
    Name : String;
    AMPosition : TAbstractMemPosition;
    StructType : TStorageStructType;
    ClassName : String;
    procedure Clear;
    function GetInformation(const AAbstractMem : TAbstractMem; out ACount : Integer; out AInfo : String) : Boolean;
  end;

  TAbstractStorage = Class
  public
    type
      TIsStorage = (is_not_initialized, is_empty, is_initialized, is_not);
      TOnInitalizeStorage = procedure(ASender : TAbstractStorage; ACurrentVersion, AUpdateToVersion : Integer) of object;
      TStorageStructInformation = Class(TAbstractMemBTreeData<TStorageStructInfo>)
      protected
        function LoadData(const APosition : TAbstractMemPosition) : TStorageStructInfo; override;
        function SaveData(const AData : TStorageStructInfo) : TAMZone; override;
      public
        procedure AddStorageStruct(const AName : String; AAMPosition : TAbstractMemPosition; AStructType : TStorageStructType; AObject : TObject);
        function GetStorageStruct(const AName : String) : TStorageStructInfo;
        function GetStorageStructAMZone(const AName : String) : TAMZone;
        function HasStorageStruct(const AName : String; out AStorageStruct : TStorageStructInfo) : Boolean;
      End;
  private
    FFileName : String;
    FAbstractMem : TAbstractMem;
    FStorageVersion: Integer;
    FStorageName: String;
    FStorageStructPosition : TAbstractMemPosition;
    FStorageStructInformation : TStorageStructInformation;
    FIsStorage : TIsStorage;
  protected
    procedure DoInitialize(AClearContent : Boolean; const ANewStorageName : String; ANewStorageVersion : Integer; AIs64Bits : Boolean; AMemUnitsSize : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
  public
    Constructor Create(const AFileName : String; AReadOnly : Boolean; const AStorageName : String; const AStorageVersion : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
    Destructor Destroy; override;
    property AbstractMem : TAbstractMem read FAbstractMem;
    property StorageName : String read FStorageName;
    property StorageVersion : Integer read FStorageVersion;
    procedure Init(const AStorageName : String; AStorageVersion : Integer; AIs64Bits : Boolean; AMemUnitsSize : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
    property StorageStructInformation : TStorageStructInformation read FStorageStructInformation;
    procedure Analize(const AInfo : TStrings);
  End;

  TBytesStorage = Class
  private
    FBytes: TBytes;
    FPosition : Integer;
    procedure CheckRead(ASize : Integer);
    function CanRead(ASize : Integer) : Boolean;
    procedure NeedWrite(AAmount : Integer);
  public
    Constructor Create(ALength : Integer); overload;
    Constructor Create(ABytes : TBytes); overload;
    Class Function Load(const AAbstractMem : TAbstractMem; const APosition : TAbstractMemPosition) : TBytesStorage;
    Class Function ReadFirstData(const AAbstractMem : TAbstractMem) : TBytesStorage;
    property Bytes : TBytes read FBytes;
    property Position : Integer read FPosition write FPosition;
    function Size : Integer;
    //
    function ReadString : String;
    function ReadByte : Byte;
    function ReadUInt16 : UInt16;
    function ReadUInt32 : UInt32;
    function ReadUInt64 : UInt64;
    function ReadStringDef(const ADefault : String) : String;
    function ReadIntDef(ABytesCount : Integer; ADefault : Int64) : Int64;
    function ReadUIntDef(ABytesCount : Integer; ADefault : UInt64) : UInt64;
    //
    function WriteString(const AValue : String) : TBytesStorage;
    function WriteByte(const AValue : Byte) : TBytesStorage;
    function WriteUInt16(const AValue : UInt16) : TBytesStorage;
    function WriteUInt32(const AValue : UInt32) : TBytesStorage;
    function WriteUInt64(const AValue : UInt64) : TBytesStorage;
    function Save(const AAbstractMem : TAbstractMem) : TAMZone;
  End;

implementation

function TStorageStructInformation_Comparer(const ALeft, ARight: TStorageStructInfo): Integer;
begin
  Result := AnsiCompareText(ALeft.Name.Trim,ARight.Name.Trim);
end;

{ TAbstractStorage }

procedure TAbstractStorage.Analize(const AInfo: TStrings);
var s : string;
  i : Integer;
  ss, ssOld : TStorageStructInfo;
begin
  AInfo.BeginUpdate;
  try
    AInfo.Add(Format('%s name:"%s" version:%d',[ClassName,Self.StorageName,Self.StorageVersion]));
    if AbstractMem.Is64Bits then s:='64bits' else s:='32bits';
    AInfo.Add(Format('AbstractMem %s %d bytes per unit total size %d',[s,AbstractMem.MemUnitsSize,AbstractMem.MaxAvailablePos]));
    AInfo.Add(Format('StorageStructs: %d',[StorageStructInformation.Count]));
    if StorageStructInformation.FindDataLowest(ss) then begin
      repeat
        if ss.GetInformation(AbstractMem,i,s) then begin
          AInfo.Add(Format('StorageStruct: %s class %s count %d %s',[ss.Name,ss.ClassName,i,s]));
        end;
        ssOld := ss;
      until not StorageStructInformation.FindDataSuccessor(ssOld,ss);
    end;
  finally
    AInfo.EndUpdate;
  end;
end;

constructor TAbstractStorage.Create(const AFileName : String; AReadOnly : Boolean;
  const AStorageName : String; const AStorageVersion : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
begin
  FIsStorage := is_not_initialized;
  FAbstractMem := Nil;
  FFileName := AFileName;
  FStorageVersion := 0;
  FStorageName := '';
  FStorageStructPosition := 0;
  FStorageStructInformation := Nil;
  if (FFileName<>'') then begin
    FAbstractMem := TFileMem.Create(FFileName,AReadOnly);
  end else FAbstractMem := TMem.Create(0,AReadOnly);
  DoInitialize(False,AStorageName,AStorageVersion,False,0,AOnInitalizeStorage);
end;

destructor TAbstractStorage.Destroy;
begin
  FStorageStructInformation.Free;
  FAbstractMem.Free;
  inherited;
end;

procedure TAbstractStorage.DoInitialize(AClearContent : Boolean; const ANewStorageName : String; ANewStorageVersion : Integer;
  AIs64Bits : Boolean; AMemUnitsSize : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
const
  CT_HEADER = 'TAbstractStorage';
  CT_VERSION = 1;

var LfdZone, LssZone : TAMZone;
  i : Integer;
  LBytesStorage : TBytesStorage;
  s : String;
begin
  if AClearContent then begin
    FreeAndNil(FStorageStructInformation);
    AbstractMem.ClearContent(AIs64Bits,AMemUnitsSize);
    //
    LfdZone := AbstractMem.New(Length(CT_HEADER)+Length(ANewStorageName)+50);
    LssZone := AbstractMem.New(TStorageStructInformation.MinAbstractMemInitialPositionSize(AbstractMem));
    FStorageStructPosition := LssZone.position;
    LBytesStorage := TBytesStorage.Create(LfdZone.size);
    try
      LBytesStorage.WriteString(CT_HEADER).WriteUInt32(CT_VERSION).WriteUInt64(FStorageStructPosition).
        WriteString(ANewStorageName).WriteUInt32(ANewStorageVersion);
      AbstractMem.Write(LfdZone.position,LBytesStorage.Bytes[0],LBytesStorage.Size);
    finally
      LBytesStorage.Free;
    end;
    FStorageStructInformation := TStorageStructInformation.Create(AbstractMem,LssZone,False,7,TStorageStructInformation_Comparer);
    FStorageStructInformation.AddStorageStruct(FStorageStructInformation.ClassName,FStorageStructPosition,ss_TAbstractMemBTreeData,FStorageStructInformation);
    if Assigned(AOnInitalizeStorage) then begin
      AOnInitalizeStorage(Self,0,ANewStorageVersion);
    end;
  end;
  //
  FreeAndNil(FStorageStructInformation);
  //
  FIsStorage := is_not_initialized;
  LBytesStorage := TBytesStorage.ReadFirstData(AbstractMem);
  Try
    if LBytesStorage.Size>0 then begin
      FIsStorage := is_not;
      s := LBytesStorage.ReadStringDef('');
      i := LBytesStorage.ReadIntDef(4,0);
      FIsStorage := is_not;
      if (s=CT_HEADER) and (i = CT_VERSION) then begin
        FStorageStructPosition := LBytesStorage.ReadUInt64;
        if FAbstractMem.GetUsedZoneInfo(FStorageStructPosition,True,LfdZone) then begin
          FStorageName := LBytesStorage.ReadString;
          FStorageVersion := LBytesStorage.ReadUInt32;
          FStorageStructInformation := TStorageStructInformation.Create(FAbstractMem,LfdZone,False,7,
            TStorageStructInformation_Comparer);
          FIsStorage := is_initialized;
          if Assigned(AOnInitalizeStorage) and (Not AbstractMem.ReadOnly) and (FStorageName=ANewStorageName) and (FStorageVersion<ANewStorageVersion) then begin
            AOnInitalizeStorage(Self,FStorageVersion,ANewStorageVersion);
          end;
        end else FStorageStructPosition := 0;
      end;
    end else begin
      FIsStorage := is_empty;
    end;
  Finally
    LBytesStorage.Free;
  End;
end;


procedure TAbstractStorage.Init(const AStorageName: String;
  AStorageVersion: Integer; AIs64Bits : Boolean; AMemUnitsSize : Integer; AOnInitalizeStorage : TOnInitalizeStorage);
begin
  DoInitialize(True,AStorageName,AStorageVersion,AIs64Bits,AMemUnitsSize,AOnInitalizeStorage);
end;

{ TAbstractStorage.TStorageStructInformation }

procedure TAbstractStorage.TStorageStructInformation.AddStorageStruct(
  const AName: String; AAMPosition: TAbstractMemPosition;
  AStructType: TStorageStructType; AObject : TObject);
var ss : TStorageStructInfo;
begin
  ss.Clear;
  ss.Name := AName;
  ss.AMPosition := AAMPosition;
  ss.StructType := AStructType;
  if Assigned(AObject) then ss.ClassName := AObject.ClassName;
  if not Self.AddData(ss) then raise EAbstractStorage.Create(Format('Cannot add StorageStruct "%s"',[AName]));
end;

function TAbstractStorage.TStorageStructInformation.GetStorageStruct(
  const AName: String): TStorageStructInfo;
begin
  if not HasStorageStruct(AName,Result) then raise EAbstractStorage.Create(Format('Cannot find storage struct "%s"',[AName]));
end;

function TAbstractStorage.TStorageStructInformation.GetStorageStructAMZone(
  const AName: String): TAMZone;
var ss : TStorageStructInfo;
begin
  ss := GetStorageStruct(AName);
  if not Self.AbstractMem.GetUsedZoneInfo(ss.AMPosition,True,Result) then raise EAbstractStorage.Create(Format('Cannot find AMZone for %s',[AName]));
end;

function TAbstractStorage.TStorageStructInformation.HasStorageStruct(
  const AName: String; out AStorageStruct: TStorageStructInfo): Boolean;
var ss : TStorageStructInfo;
begin
  ss.Clear;
  ss.Name := AName;
  Result := FindData(ss,AStorageStruct);
end;

function TAbstractStorage.TStorageStructInformation.LoadData(
  const APosition: TAbstractMemPosition): TStorageStructInfo;
var LBytesStorage : TBytesStorage;
begin
  Result.Clear;
  LBytesStorage := TBytesStorage.Load(AbstractMem,APosition);
  Try
    Result.Name := LBytesStorage.ReadString;
    Result.AMPosition := LBytesStorage.ReadUInt64;
    Result.StructType := TStorageStructType(LBytesStorage.ReadByte);
    Result.ClassName := LBytesStorage.ReadString;
  Finally
    LBytesStorage.Free;
  End;
end;

function TAbstractStorage.TStorageStructInformation.SaveData(
  const AData: TStorageStructInfo): TAMZone;
var LBytesStorage : TBytesStorage;
begin
  LBytesStorage := TBytesStorage.Create(0);
  try
    LBytesStorage.WriteString(AData.Name);
    LBytesStorage.WriteUInt64(AData.AMPosition);
    LBytesStorage.WriteByte(Byte(AData.StructType));
    LBytesStorage.WriteString(AData.ClassName);
    Result := LBytesStorage.Save(AbstractMem);
  finally
    LBytesStorage.Free;
  end;
end;

{ TStorageStructInfo }

procedure TStorageStructInfo.Clear;
begin
  Self.Name := '';
  Self.AMPosition := 0;
  Self.StructType := ss_Unknown;
  Self.ClassName := '';
end;

function TStorageStructInfo.GetInformation(const AAbstractMem: TAbstractMem;
  out ACount: Integer; out AInfo : String): Boolean;
var amz : TAMZone;
  obj : TObject;
  allowduplicates : boolean;
  order : Integer;
begin
  Result := False;
  ACount := 0;
  AInfo := '';
  if not AAbstractMem.GetUsedZoneInfo(self.AMPosition,true,amz) then Exit;
  case Self.StructType of
    ss_Buffer: begin
      ACount := amz.size;
      Result := True;
    end;
    ss_TAbstractMemBTree, ss_TAbstractMemBTreeData, ss_TAbstractMemBTreeDataIndex : begin
      if TAbstractMemBTree.GetInfo(AAbstractMem,amz,allowduplicates,order,ACount) then begin
        AInfo := Format('Order %d',[order]);
        if allowduplicates then AInfo := AInfo + ' with duplicates' else AInfo := AInfo + ' without duplicates';
        Result := True;
      end;
    end;
    ss_TAbstractMemTList: begin
      obj := TAbstractMemTList.Create(AAbstractMem,amz,3,false);
      try
        ACount := TAbstractMemTList(obj).Count;
        Result := True;
      finally
        obj.Free;
      end;
    end;
    ss_TAbstractMemTListT, ss_TAbstractMemOrderedTListT: begin
      obj := TAbstractMemTListBaseAbstract<Integer>.Create(AAbstractMem,amz,3,false);
      try
        ACount := TAbstractMemTListBaseAbstract<Integer>(obj).Count;
        Result := True;
      finally
        obj.Free;
      end;
    end
  end;

end;

{ TBytesStorage }

function TBytesStorage.CanRead(ASize: Integer): Boolean;
begin
  Result := (ASize>=0) and ((FPosition + ASize)<Length(FBytes));
end;

procedure TBytesStorage.CheckRead(ASize: Integer);
begin
  if (ASize<=0) or ((FPosition + ASize)>Length(FBytes)) then raise EAbstractStorage.Create(Format('Canot %s.Read %d bytes (pos %d/%d)',[Self.ClassName,ASize,FPosition,Length(FBytes)]));
end;

constructor TBytesStorage.Create(ABytes: TBytes);
begin
  SetLength(FBytes,Length(ABytes));
  FPosition := 0;
  Move(ABytes[0],FBytes[0],Length(ABytes));
end;

constructor TBytesStorage.Create(ALength : Integer);
begin
  if ALength<0 then ALength := 0;
  SetLength(FBytes,ALength);
  FPosition := 0;
end;

class function TBytesStorage.Load(const AAbstractMem: TAbstractMem;
  const APosition: TAbstractMemPosition): TBytesStorage;
var LZone : TAMZone;
begin
  if Not AAbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractStorage.Create(Format('%s.Load Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  Result := TBytesStorage.Create(LZone.size);
  Try
    if AAbstractMem.Read(LZone.position, Result.FBytes[0], LZone.size )<>LZone.size then
      raise EAbstractStorage.Create(Format('%s.Load Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  Except
    Result.Free;
    Raise;
  End;
end;

procedure TBytesStorage.NeedWrite(AAmount: Integer);
begin
  if FPosition+AAmount > Length(FBytes) then begin
    SetLength(FBytes,FPosition+AAmount);
  end;
end;

function TBytesStorage.ReadByte: Byte;
begin
  Result := 0;
  CheckRead(1);
  Move(FBytes[FPosition],Result,1);
  inc(FPosition,1);
end;

class function TBytesStorage.ReadFirstData(
  const AAbstractMem: TAbstractMem): TBytesStorage;
var LfdZone : TAMZone;
begin
  Result := TBytesStorage.Create(0);
  if not AAbstractMem.ReadFirstData(LfdZone,Result.FBytes) then Exit;
end;

function TBytesStorage.ReadIntDef(ABytesCount: Integer;
  ADefault: Int64): Int64;
begin
  if CanRead(ABytesCount) then begin
    Result := 0;
    Move(FBytes[FPosition],Result,ABytesCount);
    inc(FPosition,ABytesCount);
  end else Result := ADefault;
end;

function TBytesStorage.ReadString: String;
var LPos : Integer;
  l : Integer;
begin
  LPos := FPosition;
  try
    l := ReadUInt16;
    if (l<0) then raise EAbstractStorage.Create(Format('%s.ReadString Invalid Length %d for String',[Self.ClassName, l]));
    CheckRead(l);
    Result := TEncoding.ANSI.GetString(FBytes,FPosition,l);
    inc(FPosition,l);
  Except
    FPosition := LPos;
    Raise;
  end;
end;

function TBytesStorage.ReadStringDef(const ADefault: String): String;
var LPos : Integer;
  l : Int64;
begin
  LPos := FPosition;
  l := Integer(ReadIntDef(2,-1));
  if (l<0) or (Not CanRead(l)) then begin
    Result := ADefault;
    FPosition := LPos;
  end else begin
    Result := TEncoding.ANSI.GetString(FBytes,FPosition,Integer(l));
    inc(FPosition,Integer(l));
  end;
end;

function TBytesStorage.ReadUInt16: UInt16;
begin
  Result := 0;
  CheckRead(2);
  Move(FBytes[FPosition],Result,2);
  inc(FPosition,2);
end;

function TBytesStorage.ReadUInt32: UInt32;
begin
  Result := 0;
  CheckRead(4);
  Move(FBytes[FPosition],Result,4);
  inc(FPosition,4);
end;

function TBytesStorage.Save(const AAbstractMem: TAbstractMem): TAMZone;
begin
  Result := AAbstractMem.New(Self.Size);
  AAbstractMem.Write(Result.position,Self.FBytes[0],Self.Size);
end;

function TBytesStorage.Size: Integer;
begin
  Result := Length(FBytes);
end;

function TBytesStorage.ReadUInt64: UInt64;
begin
  Result := 0;
  CheckRead(8);
  Move(FBytes[FPosition],Result,8);
  inc(FPosition,8);
end;

function TBytesStorage.ReadUIntDef(ABytesCount: Integer;
  ADefault: UInt64): UInt64;
begin
  Result := ReadIntDef(ABytesCount,ADefault);
end;

function TBytesStorage.WriteByte(const AValue: Byte) : TBytesStorage;
begin
  NeedWrite(1);
  Move(AValue,FBytes[FPosition],1);
  inc(FPosition,1);
  Result := Self;
end;

function TBytesStorage.WriteString(const AValue: String) : TBytesStorage;
var Lb : TBytes;
begin
  WriteUInt16(AValue.Length);
  if Length(AValue)>0 then begin
    NeedWrite(Length(AValue));
    Lb := TEncoding.ANSI.GetBytes(AValue);
    Move(Lb[0],FBytes[FPosition],Length(AValue));
    inc(FPosition,Length(AValue));
  end;
  Result := Self;
end;

function TBytesStorage.WriteUInt16(const AValue: UInt16) : TBytesStorage;
begin
  NeedWrite(2);
  Move(AValue,FBytes[FPosition],2);
  inc(FPosition,2);
  Result := Self;
end;

function TBytesStorage.WriteUInt32(const AValue: UInt32) : TBytesStorage;
begin
  NeedWrite(4);
  Move(AValue,FBytes[FPosition],4);
  inc(FPosition,4);
  Result := Self;
end;

function TBytesStorage.WriteUInt64(const AValue: UInt64) : TBytesStorage;
begin
  NeedWrite(8);
  Move(AValue,FBytes[FPosition],8);
  inc(FPosition,8);
  Result := Self;
end;

end.
