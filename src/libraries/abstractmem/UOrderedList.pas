unit UOrderedList;

{
  This file is part of AbstractMem framework

  Copyright (C) 2020-2021 Albert Molina - bpascalblockchain@gmail.com

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

{$ifdef FPC}
  {$mode DELPHI}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils
  // NOTE ABOUT FREEPASCAL (2020-03-10)
  // Current version 3.0.4 does not contain valid support for Generics, using Generics from this:
  // https://github.com/PascalCoinDev/PascalCoin/tree/master/src/libraries/generics.collections
  // (Download and set folder as a "units include folder" in compiler options)
  {$IFNDEF FPC},System.Generics.Collections,System.Generics.Defaults{$ELSE},Generics.Collections,Generics.Defaults{$ENDIF};

{$I ./ConfigAbstractMem.inc }

type
  {$IFDEF FPC}
  TComparison<T> = function(const Left, Right: T): Integer;
  {$ENDIF}

  TOrderedList<T> = Class
  private
    FOnCompare: TComparison<T>;
    FAllowDuplicates : Boolean;
    FOrderedList : TList<T>;
  public
    Constructor Create(AAllowDuplicates : Boolean; const AOnCompareMethod: TComparison<T>); virtual;
    Destructor Destroy; override;

    Function Add(const AValue : T) : Integer; virtual;
    Procedure Remove(const AValue : T; ARemoveDuplicates : Boolean = False); virtual;
    Procedure Clear; virtual;
    Procedure Delete(index : Integer); virtual;

    Function Get(index : Integer) : T;
    Function Count : Integer;
    Function Find(const AValue: T; out Index: Integer): Boolean;
    function FindPrecessor(const AValue : T; out Index : Integer) : Boolean;
    function FindSuccessor(const AValue : T; out Index : Integer) : Boolean;
    Function IndexOf(const AValue: T) : Integer;
    property AllowDuplicates : Boolean read FAllowDuplicates;
    property OnComparer : TComparison<T> read FOnCompare;
    property Items[index:Integer] : T read Get; default;
  End;

// Default Ordered functions
function TComparison_Integer(const ALeft, ARight: Integer): Integer;
function TComparison_Cardinal(const ALeft, ARight: Cardinal): Integer;
function TComparison_Word(const ALeft, ARight: Word): Integer;
function TComparison_Byte(const ALeft, ARight: Byte): Integer;
function TComparison_Pointer(const ALeft, ARight: Pointer): Integer;
function TComparison_String(const ALeft, ARight: String): Integer;

implementation

function TComparison_Integer(const ALeft, ARight: Integer): Integer;
begin
  Result := ALeft - ARight;
end;
function TComparison_Cardinal(const ALeft, ARight: Cardinal): Integer;
begin
  Result := ALeft - ARight;
end;
function TComparison_Word(const ALeft, ARight: Word): Integer;
begin
  Result := ALeft - ARight;
end;
function TComparison_Byte(const ALeft, ARight: Byte): Integer;
begin
  Result := ALeft - ARight;
end;
function TComparison_Pointer(const ALeft, ARight: Pointer): Integer;
begin
{$IFNDEF FPC}
  Result := NativeInt(ALeft) - NativeInt(ARight);
{$ELSE}
  Result := PtrInt(ALeft) - PtrInt(ARight);
{$ENDIF}
end;
function TComparison_String(const ALeft, ARight: String): Integer;
begin
  Result := CompareText(ALeft,ARight);
end;

{ TOrderedList<T> }

function TOrderedList<T>.Add(const AValue: T): Integer;
var
  LFound : Boolean;
begin
  LFound := Find(AValue,Result);
  if (LFound and FAllowDuplicates) or (Not LFound) then begin
    FOrderedList.Insert(Result,AValue);
  end else Result := -1;
end;

procedure TOrderedList<T>.Clear;
begin
  FOrderedList.Clear;
end;

function TOrderedList<T>.Count: Integer;
begin
  Result := FOrderedList.Count;
end;

constructor TOrderedList<T>.Create(AAllowDuplicates: Boolean;
  const AOnCompareMethod: TComparison<T>);
begin
  FOnCompare := AOnCompareMethod;
  FAllowDuplicates := AAllowDuplicates;
  FOrderedList := TList<T>.Create;
  inherited Create;
end;

procedure TOrderedList<T>.Delete(index: Integer);
begin
  FOrderedList.Delete(index);
end;

destructor TOrderedList<T>.Destroy;
begin
  Clear;
  FOrderedList.Free;
  inherited;
end;

function TOrderedList<T>.Find(const AValue: T; out Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FOrderedList.Count - 1;
  // Optimization when inserting always a ordered list
  if (H>0) then begin
    C := FOnCompare(FOrderedList[H],AValue);
    if (C<0) then begin
      Index := H+1;
      Exit;
    end else if (C=0) then begin
      Index := H; // When equals, insert to the left
      Result := True;
      Exit;
    end;
  end;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := FOnCompare(FOrderedList[I],AValue);
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

function TOrderedList<T>.FindPrecessor(const AValue: T; out Index: Integer): Boolean;
begin
  if Find(AValue,Index) then begin
    if (Index>0) then begin
      Dec(Index);
      Result := True;
    end else Result := False;
  end else Result := False;
end;

function TOrderedList<T>.FindSuccessor(const AValue: T; out Index: Integer): Boolean;
begin
  if Find(AValue,Index) then begin
    if (Index+1<Count) then begin
      Inc(Index);
      Result := True;
    end else Result := False;
  end else Result := False;
end;

function TOrderedList<T>.Get(index: Integer): T;
begin
  Result := FOrderedList[index];
end;

function TOrderedList<T>.IndexOf(const AValue: T): Integer;
begin
  if Not Find(AValue,Result) then Result := -1;
end;

procedure TOrderedList<T>.Remove(const AValue: T; ARemoveDuplicates : Boolean = False);
var i : Integer;
begin
  while Find(AValue,i) do begin
    FOrderedList.Delete(i);
    if (Not FAllowDuplicates) or (Not ARemoveDuplicates) then Exit; // No need to continue while
  end;
end;

initialization

finalization

end.






