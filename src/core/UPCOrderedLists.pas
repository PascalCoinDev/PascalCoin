unit UPCOrderedLists;

{ Copyright (c) 2016-2019 by Albert Molina

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
  Classes, SysUtils, UBaseTypes,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

Type
  TCardinalsArray = Array of Cardinal;

  // Maintans a Cardinal ordered (without duplicates) list with TRawData each

  { TOrderedCardinalList }

  TOrderedCardinalList = Class
  private
    FOrderedList : TList<Cardinal>;
    FDisabledsCount : Integer;
    FModifiedWhileDisabled : Boolean;
    FOnListChanged: TNotifyEvent;
    Procedure NotifyChanged;
  public
    Constructor Create;
    Destructor Destroy; override;
    Function Add(Value : Cardinal) : Integer;
    Procedure Remove(Value : Cardinal);
    Procedure Clear;
    Function Get(index : Integer) : Cardinal;
    Function Count : Integer;
    Function Find(const Value: Cardinal; var Index: Integer): Boolean;
    Property OnListChanged : TNotifyEvent read FOnListChanged write FOnListChanged;
    Procedure CopyFrom(Sender : TOrderedCardinalList);
    Procedure Disable;
    Procedure Enable;
    Function ToArray : TCardinalsArray;
  End;


  // Maintans a Cardinal ordered (without duplicates) list with TRawData each

  { TOrderedCardinalListWithRaw }

  TOrderedCardinalListWithRaw = Class
  private
    FList : TList<Pointer>;
    Function Find(value : Cardinal; var Index: Integer): Boolean;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Function Add(const Value: Cardinal; const RawData : TRawBytes) : Integer;
    Function Count : Integer;
    Function GetCardinal(index : Integer) : Cardinal;
    function GetRaw(index : Integer) : TRawBytes;
    Procedure Delete(index : Integer);
    Function IndexOf(value : Cardinal) : Integer;
    Function IndexOfRaw(const RawData : TRawBytes) : Integer;
  end;

  // Maintains a TRawBytes (AnsiString) list ordered to quick search withoud duplicates

  { TOrderedRawList }

  TOrderedRawList = Class
  private
    FList : TList<Pointer>;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Function Add(Const RawData : TRawBytes; tagValue : Integer = 0) : Integer;
    Procedure Remove(Const RawData : TRawBytes);
    Function Count : Integer;
    Function Get(index : Integer) : TRawBytes;
    Procedure Delete(index : Integer);
    procedure SetTag(Const RawData : TRawBytes; newTagValue : Integer);
    function GetTag(Const RawData : TRawBytes) : Integer; overload;
    function GetTag(index : Integer) : Integer; overload;
    Function IndexOf(Const RawData : TRawBytes) : Integer;
    Procedure CopyFrom(master : TOrderedRawList);
    Function Find(const RawData: TRawBytes; var Index: Integer): Boolean;
  End;



implementation

{ TOrderedCardinalList }

function TOrderedCardinalList.Add(Value: Cardinal): Integer;
var nc : Integer;
begin
  if Find(Value,Result) then exit
  else begin
    nc := FOrderedList.Capacity;
    if (nc <= FOrderedList.Count) then begin
      nc := nc SHL 1;
      if (nc > FOrderedList.Capacity) then begin
        {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Increase capacity from %d to %d for %s',[FOrderedList.Capacity,nc,IntToHex(Integer(FOrderedList),8)]));{$ENDIF}
        FOrderedList.Capacity:=nc;
      end;
    end;
    FOrderedList.Insert(Result,(Value));
    NotifyChanged;
  end;
end;

procedure TOrderedCardinalList.Clear;
begin
  FOrderedList.Clear;
  NotifyChanged;
end;

procedure TOrderedCardinalList.CopyFrom(Sender: TOrderedCardinalList);
Var i : Integer;
begin
  if Self=Sender then exit;
  Disable;
  Try
    Clear;
    FOrderedList.Capacity:=Sender.FOrderedList.Capacity;
    for I := 0 to Sender.Count - 1 do begin
      Add(Sender.Get(i));
    end;
  Finally
    Enable;
  End;
end;

function TOrderedCardinalList.Count: Integer;
begin
  Result := FOrderedList.Count;
end;

constructor TOrderedCardinalList.Create;
begin
  FOrderedList := TList<Cardinal>.Create;
  FDisabledsCount := 0;
  FModifiedWhileDisabled := false;
end;

destructor TOrderedCardinalList.Destroy;
begin
  FOrderedList.Free;
  inherited;
end;

procedure TOrderedCardinalList.Disable;
begin
  inc(FDisabledsCount);
end;

procedure TOrderedCardinalList.Enable;
begin
  if FDisabledsCount<=0 then raise Exception.Create('Dev error. Invalid disabled counter');
  dec(FDisabledsCount);
  if (FDisabledsCount=0) And (FModifiedWhileDisabled) then NotifyChanged;
end;

function TOrderedCardinalList.Find(const Value: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FOrderedList.Count - 1;
  // Optimization when inserting always a ordered list
  if (H>0) then begin
    C := Int64(FOrderedList[H]) - Int64(Value);
    if (C<0) then begin
      Index := H+1;
      Exit;
    end else if (C=0) then begin
      Index := H;
      Result := True;
      Exit;
    end;
  end;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64(FOrderedList[I]) - Int64(Value);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedCardinalList.Get(index: Integer): Cardinal;
begin
  Result := Cardinal(FOrderedList[index]);
end;

procedure TOrderedCardinalList.NotifyChanged;
begin
  if FDisabledsCount>0 then begin
    FModifiedWhileDisabled := true;
    exit;
  end;
  FModifiedWhileDisabled := false;
  if Assigned(FOnListChanged) then FOnListChanged(Self);
end;

procedure TOrderedCardinalList.Remove(Value: Cardinal);
Var i : Integer;
begin
  if Find(Value,i) then begin
    FOrderedList.Delete(i);
    NotifyChanged;
  end;
end;

Function TOrderedCardinalList.ToArray : TCardinalsArray;
var i : integer;
begin
  SetLength(Result, self.Count);
  for i := 0 to self.Count - 1 do
    Result[i] := Self.Get(i);
end;

{ TOrderedRawList }

Type TRawListData = Record
    RawData : TRawBytes;
    tag : Integer;
  End;
  PRawListData = ^TRawListData;

function TOrderedRawList.Add(const RawData: TRawBytes; tagValue : Integer = 0) : Integer;
Var P : PRawListData;
begin
  if Find(RawData,Result) then begin
    PRawListData(FList[Result])^.tag := tagValue;
  end else begin
    New(P);
    P^.RawData := RawData;
    P^.tag := tagValue;
    FList.Insert(Result,P);
  end;
end;

procedure TOrderedRawList.Remove(const RawData: TRawBytes);
Var i : Integer;
begin
  i := IndexOf(RawData);
  If i>=0 then Delete(i);
end;

procedure TOrderedRawList.Clear;
Var P : PRawListData;
  i : Integer;
begin
  for i := FList.Count - 1 downto 0 do begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
end;

function TOrderedRawList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TOrderedRawList.Create;
begin
  FList := TList<Pointer>.Create;
end;

procedure TOrderedRawList.Delete(index: Integer);
Var P : PRawListData;
begin
  P := PRawListData(FList[index]);
  FList.Delete(index);
  Dispose(P);
end;

destructor TOrderedRawList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;


function TOrderedRawList.Find(const RawData: TRawBytes; var Index: Integer): Boolean;
var L, H, I: Integer;
  c : Integer;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    c := TBaseType.BinStrComp(PRawListData(FList[i])^.RawData,RawData);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedRawList.Get(index: Integer): TRawBytes;
begin
  Result := PRawListData(FList[index])^.RawData;
end;

function TOrderedRawList.GetTag(index: Integer): Integer;
begin
  Result := PRawListData(FList[index])^.tag;
end;

function TOrderedRawList.GetTag(const RawData: TRawBytes): Integer;
Var i : Integer;
begin
  if Not Find(RawData,i) then begin
    Result := 0;
  end else begin
    Result := PRawListData(FList[i])^.tag;
  end;
end;

function TOrderedRawList.IndexOf(const RawData: TRawBytes): Integer;
begin
  if Not Find(RawData,Result) then Result := -1;
end;

procedure TOrderedRawList.CopyFrom(master: TOrderedRawList);
Var i : Integer;
begin
  If master=Self then Exit;
  Clear;
  For i:=0 to master.Count-1 do begin
    Add(master.Get(i),master.GetTag(i));
  end;
end;

procedure TOrderedRawList.SetTag(const RawData: TRawBytes; newTagValue: Integer);
begin
  Add(RawData,newTagValue);
end;

{ TOrderedCardinalListWithRaw }

Type TCardinalListData = Record
    value : Cardinal;
    rawData : TRawBytes;
  End;
  PCardinalListData = ^TCardinalListData;

function TOrderedCardinalListWithRaw.Find(value: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  c : Integer;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    c := Int64(PCardinalListData(FList[I])^.value) - Int64(Value);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

constructor TOrderedCardinalListWithRaw.Create;
begin
  FList := TList<Pointer>.Create;
end;

destructor TOrderedCardinalListWithRaw.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TOrderedCardinalListWithRaw.Clear;
Var i : Integer;
  P : PCardinalListData;
begin
  for i:=0 to FList.Count-1 do begin
    P := FList[i];
    SetLength(P^.rawData,0);
    Dispose(P);
  end;
  FList.Clear;
end;

function TOrderedCardinalListWithRaw.Add(const Value: Cardinal; const RawData: TRawBytes): Integer;
Var P : PCardinalListData;
begin
  If Find(Value,Result) then begin
    P := FList[Result];
    P^.rawData:=RawData;
  end else begin
    New(P);
    P^.value:=Value;
    P^.rawData:=rawData;
    FList.Insert(Result,P);
  end;
end;

function TOrderedCardinalListWithRaw.Count: Integer;
begin
  Result := FList.Count;
end;

function TOrderedCardinalListWithRaw.GetCardinal(index: Integer): Cardinal;
begin
  Result := PCardinalListData(FList[index])^.value;
end;

function TOrderedCardinalListWithRaw.GetRaw(index: Integer): TRawBytes;
begin
  Result := PCardinalListData(FList[index])^.rawData;
end;

procedure TOrderedCardinalListWithRaw.Delete(index: Integer);
Var P : PCardinalListData;
begin
  P := PCardinalListData( FList[index] );
  FList.Delete(index);
  Dispose(P);
end;

function TOrderedCardinalListWithRaw.IndexOf(value: Cardinal): Integer;
begin
  If Not Find(value,Result) then Result := -1;
end;

function TOrderedCardinalListWithRaw.IndexOfRaw(const RawData: TRawBytes): Integer;
begin
  For Result := 0 to FList.Count-1 do begin
    If TBaseType.BinStrComp( PCardinalListData( FList[Result] )^.rawData , RawData ) = 0 then Exit;
  end;
  Result := -1;
end;

end.

