unit UAbstractMemTList;

{
  This file is part of AbstractMem framework

  Copyright (C) 2020 Albert Molina - bpascalblockchain@gmail.com

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
  Classes, SysUtils,
  SyncObjs,
  UAbstractMem,
  // NOTE ABOUT FREEPASCAL (2020-03-10)
  // Current version 3.0.4 does not contain valid support for Generics, using Generics from this:
  // https://github.com/PascalCoinDev/PascalCoin/tree/master/src/libraries/generics.collections
  // (Download and set folder as a "units include folder" in compiler options)
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

{$I ./ConfigAbstractMem.inc }

type
  EAbstractMemTList = Class(Exception);

  TAbstractMemTList = Class
  private
    FAbstractMem : TAbstractMem;
    FInitialZone : TAMZone; // Initial zone contains "magic signature", "elements of each block" and "first block pointer", must be at least 16 bytes size

    FElementsOfEachBlock : Integer;
    FFirstBlockPointer : TAbstractMemPosition;
    FNextElementPosition : Integer;

    FUseCache : Boolean;
    FCacheData : TBytes;
    FCacheUpdated : Boolean;
    FCacheDataLoaded : Boolean;
    FCacheDataUsedBytes : Integer;

    function GetPosition(AIndex: Integer): TAbstractMemPosition;
    procedure SetPosition(AIndex: Integer; const Value: TAbstractMemPosition);

    function UseCacheData : Boolean;
    Procedure CheckInitialized;
    procedure GetPointerTo(AIndex : Integer; AAllowIncrease : Boolean; out APreviousBlockPointer, ABlockPointer : TAbstractMemPosition; out AIndexInBlock : Integer);
    procedure AddRange(AIndexStart, AInsertCount : Integer);
    procedure RemoveRange(AIndexStart, ARemoveCount : Integer);
    procedure LoadElements(AIndexStart : Integer; var AElements : TBytes);
    procedure SetUseCache(const Value: Boolean);
    procedure Initialize(const AInitialZone : TAMZone; ADefaultElementsPerBlock : Integer);
  protected
    FAbstractMemTListLock : TCriticalSection;
  public
    Constructor Create(AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; ADefaultElementsPerBlock : Integer; AUseCache : Boolean); virtual;
    destructor Destroy; override;

    procedure FlushCache;

    Function Add(const APosition : TAbstractMemPosition) : Integer;

    Procedure Clear;
    Procedure Dispose;

    Procedure Delete(index : Integer);
    Procedure Insert(AIndex : Integer; const APosition : TAbstractMemPosition);

    property Position[AIndex : Integer] : TAbstractMemPosition read GetPosition write SetPosition;

    Function Count : Integer;
    property AbstractMem : TAbstractMem read FAbstractMem;
    property InitialiZone : TAMZone read FInitialZone;
    property UseCache : Boolean read FUseCache write SetUseCache;
    procedure LockList;
    procedure UnlockList;
  End;

  TAbstractMemTListBaseAbstract<T> = Class
  private
    FAbstractMem: TAbstractMem;
    function GetInitialZone: TAMZone;
    function GetUseCache : Boolean;
    procedure SetUseCache(const Value: Boolean);
  protected
    FList : TAbstractMemTList;
    // POSSIBLE OVERRIDE METHODS
    function GetItem(index : Integer) : T; virtual;
    procedure SetItem(index : Integer; const AItem : T); virtual;
    function ToString(const AItem : T) : String; overload; virtual;
    // ABSTRACT METHODS NEED TO OVERRIDE
    procedure LoadFrom(const ABytes : TBytes; var AItem : T); virtual; abstract;
    procedure SaveTo(const AItem : T; AIsAddingItem : Boolean; var ABytes : TBytes); virtual; abstract;
  public
    Constructor Create(AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; ADefaultElementsPerBlock : Integer; AUseCache : Boolean); virtual;
    Destructor Destroy; override;

    Function Add(const AItem : T) : Integer; virtual;

    function Count : Integer;
    procedure Delete(index : Integer); virtual;

    procedure FlushCache;
    Procedure Clear;
    Procedure Dispose;
    property AbstractMem : TAbstractMem read FAbstractMem;
    property InitialiZone : TAMZone read GetInitialZone;
    property UseCache : Boolean read GetUseCache write SetUseCache;
  End;


  TAbstractMemTList<T> = Class(TAbstractMemTListBaseAbstract<T>)
  public
    property Item[index : Integer] : T read GetItem write SetItem;
  End;

  { TAbstractMemOrderedTList }

  TAbstractMemOrderedTList<T> = Class(TAbstractMemTListBaseAbstract<T>)
  private
    FAllowDuplicates : Boolean;
  protected
    // ABSTRACT METHODS NEED TO OVERRIDE
    function Compare(const ALeft, ARight : T) : Integer; virtual; abstract;
  public
    Constructor Create(AAbstractMem : TAbstractMem; const AInitialZone : TAMZone; ADefaultElementsPerBlock : Integer; AAllowDuplicates, AUseCache : Boolean); reintroduce;
    function Find(const AItemToFind : T; out AIndex : Integer) : Boolean;
    Function Add(const AItem : T) : Integer; reintroduce;
    property Item[index : Integer] : T read GetItem;
    function IndexOf(const AItem : T) : Integer;
    property AllowDuplicates : Boolean read FAllowDuplicates;
    function Get(index : Integer) : T;
  End;

const
  CT_AbstractMemTList_HeaderSize = 16;
    // [0] 4 for magic
    // [4] 4 for elements of each block
    // [8] 4 for next element (counter)
    // [12] 4 for first block position

implementation

{ TAbstractMemTList }

const
  CT_AbstractMemTList_Magic = 'ABML'; // DO NOT LOCALIZE MUST BE 4 BYTES LENGTH

function TAbstractMemTList.Add(const APosition: TAbstractMemPosition): Integer;
begin
  FAbstractMemTListLock.Acquire;
  Try
  Result := FNextElementPosition;
  Insert(FNextElementPosition,APosition);
  Finally
    FAbstractMemTListLock.Release;
  End;
end;

procedure TAbstractMemTList.AddRange(AIndexStart, AInsertCount: Integer);
var LElements : TBytes;
  LBlockPointer,LPreviousBlockPointer : TAbstractMemPosition;
  LIndexInBlock, i, j, n : Integer;
begin
  CheckInitialized;
  if (AIndexStart<0) or (AInsertCount<=0) or (AIndexStart>FNextElementPosition) then raise EAbstractMemTList.Create(Format('%s AddRange %d..%d out of range 0..%d',[ClassName,AIndexStart,AIndexStart+AInsertCount,FNextElementPosition-1]));
  if (UseCacheData) then begin
    if (Length(FCacheData)-FCacheDataUsedBytes)< (AInsertCount*4) then begin
      // Increase
      if (FElementsOfEachBlock>AInsertCount) then i := FElementsOfEachBlock
      else i := AInsertCount;
      SetLength(FCacheData,Length(FCacheData) + (i * 4));
    end;
    FCacheUpdated := True;
    Inc(FCacheDataUsedBytes,(AInsertCount*4));
    Move(FCacheData[AIndexStart*4],FCacheData[(AIndexStart+AInsertCount)*4],FCacheDataUsedBytes-((AIndexStart+AInsertCount)*4));
    Inc(FNextElementPosition,AInsertCount);
    Exit;
  end;
  //
  LoadElements(AIndexStart,LElements);
  n := 0; // n = Elements moved
  // Increase
  i := AIndexStart+AInsertCount;
  // i = first position to move "right"
  repeat
    GetPointerTo(i,True,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    // Move from LIndexInBlock to FElementsOfEachBlock-1 in this block
    j := FElementsOfEachBlock - (LIndexInBlock); // j = Elements to move right on this block
    if ((n+j)*4>Length(LElements)) then j := (Length(LElements) DIV 4)-n;
    FAbstractMem.Write( LBlockPointer + (LIndexInBlock*4), LElements[ n*4 ], j*4 );
    inc(n,j);
    inc(i,j);
  until (i >= FNextElementPosition + AInsertCount) or (j=0);
  Inc(FNextElementPosition,AInsertCount);
  FAbstractMem.Write( FInitialZone.position + 8, FNextElementPosition, 4 );
end;

procedure TAbstractMemTList.CheckInitialized;
begin
  if (FElementsOfEachBlock<=0) then raise EAbstractMemTList.Create(FormaT('%s not initialized',[ClassName]));
end;

procedure TAbstractMemTList.Clear;
var LBlockPointer, LNext : TAbstractMemPosition;
begin
  FAbstractMemTListLock.Acquire;
  Try
  CheckInitialized;
  // Free mem
  LBlockPointer := FFirstBlockPointer;
  FFirstBlockPointer := 0;
  FNextElementPosition := 0;
  FAbstractMem.Write( FInitialZone.position + 12, FFirstBlockPointer, 4 );
  while (LBlockPointer>0) do begin
    // Read next
    FAbstractMem.Read( LBlockPointer + (FElementsOfEachBlock * 4), LNext, 4);
    FAbstractMem.Dispose(LBlockPointer);
    LBlockPointer := LNext;
  end;

  SetLength(FCacheData,0);
  FCacheUpdated := False;
  FCacheDataUsedBytes := 0;
  Finally
    FAbstractMemTListLock.Release;
  End;
end;

function TAbstractMemTList.Count: Integer;
begin
  Result := FNextElementPosition;
end;

constructor TAbstractMemTList.Create(AAbstractMem: TAbstractMem; const AInitialZone: TAMZone; ADefaultElementsPerBlock : Integer; AUseCache : Boolean);
begin
  SetLength(FCacheData,0);
  FUseCache := AUseCache;
  FCacheUpdated := False;
  FCacheDataLoaded := False;
  FCacheDataUsedBytes := 0;

  FAbstractMem := AAbstractMem;
  FInitialZone.Clear;

  FElementsOfEachBlock := 0;
  FFirstBlockPointer := 0;
  FNextElementPosition := 0;

  FAbstractMemTListLock := TCriticalSection.Create;

  Initialize(AInitialZone,ADefaultElementsPerBlock);
end;

procedure TAbstractMemTList.Delete(index: Integer);
begin
  RemoveRange(index,1);
end;

destructor TAbstractMemTList.Destroy;
begin
  if FUseCache then FlushCache;
  FAbstractMemTListLock.Free;
  inherited;
end;

procedure TAbstractMemTList.Dispose;
begin
  FAbstractMemTListLock.Acquire;
  Try
  if FInitialZone.position<=0 then Exit; // Nothing to dispose
  Clear;
  Try
    if FInitialZone.size=0 then FAbstractMem.Dispose(FInitialZone.position)
    else FAbstractMem.Dispose(FInitialZone);
  Finally
    FInitialZone.Clear;
  End;
  Finally
    FAbstractMemTListLock.Release;
  End;
end;

procedure TAbstractMemTList.FlushCache;
var i : Integer;
  LPreviousBlockPointer,LBlockPointer, LNext, LZero : TAbstractMemPosition;
  LIndexInBlock, LElements : Integer;
begin
  FAbstractMemTListLock.Acquire;
  try
  if (Not FUseCache) or (Not FCacheUpdated) then Exit;
  CheckInitialized;
  LPreviousBlockPointer := 0;
  LBlockPointer := 0;
  LIndexInBlock := 0;
  LNext := 0;
  // Save full:
  i := 0;
  while ((i*4) < (FCacheDataUsedBytes)) do begin
    GetPointerTo(i,True,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    if (i+FElementsOfEachBlock-1 >= FNextElementPosition) then begin
      LElements := FNextElementPosition - i;
    end else LElements := FElementsOfEachBlock;
    FAbstractMem.Write(LBlockPointer,FCacheData[i*4],(LElements*4));
    inc(i,LElements);
    FAbstractMem.Read( LBlockPointer + (FElementsOfEachBlock * 4), LNext, 4);
    LPreviousBlockPointer := LBlockPointer;
  end;
  // Save Header:
  FAbstractMem.Write( FInitialZone.position + 8, FNextElementPosition, 4 );
  // Free unused blocks:
  if (FNextElementPosition=0) And (FFirstBlockPointer>0) then begin
    // This is first block pointer
    LNext := FFirstBlockPointer;
    FFirstBlockPointer := 0;
    FAbstractMem.Write( FInitialZone.position + 12, FFirstBlockPointer, 4 );
    LPreviousBlockPointer := 0;
  end;
  while (LNext>0) do begin
    if LPreviousBlockPointer>0 then begin
      LZero := 0;
      FAbstractMem.Write( LPreviousBlockPointer + (FElementsOfEachBlock * 4), LZero, 4);
    end;
    LPreviousBlockPointer := LBlockPointer;
    LBlockPointer := LNext;
    FAbstractMem.Read( LBlockPointer + (FElementsOfEachBlock * 4), LNext, 4);
    FAbstractMem.Dispose(LBlockPointer);
  end;
  //
  FCacheUpdated := False;
  finally
    FAbstractMemTListLock.Release;
  end;
end;

procedure TAbstractMemTList.GetPointerTo(AIndex: Integer; AAllowIncrease : Boolean; out APreviousBlockPointer, ABlockPointer: TAbstractMemPosition; out AIndexInBlock: Integer);
var LBlockIndex : Integer;
  i : Integer;
  LNewBlock : TAMZone;
  LZero : Integer;
begin
  CheckInitialized;
  if (AIndex<0) or ((Not AAllowIncrease) And (AIndex>=FNextElementPosition)) then raise EAbstractMemTList.Create(Format('%s index %d out of range 0..%d',[ClassName,AIndex,FNextElementPosition-1]));

  // Search ABlockPointer
  LBlockIndex := AIndex DIV FElementsOfEachBlock;
  AIndexInBlock := AIndex MOD FElementsOfEachBlock;

  APreviousBlockPointer := 0;
  ABlockPointer := FFirstBlockPointer;
  i := 0;
  repeat
    if (ABlockPointer<=0) then begin
      // Create
      LNewBlock := FAbstractMem.New( 4 + (FElementsOfEachBlock * 4) );
      ABlockPointer := LNewBlock.position;
      // Save this pointer
      if (i=0) then begin
        // This is FFirstBlockPointer
        FFirstBlockPointer := LNewBlock.position;
        // Save header:
        FAbstractMem.Write( FInitialZone.position + 12, FFirstBlockPointer, 4 );
      end else begin
        // This is previous block
        FAbstractMem.Write( APreviousBlockPointer + (FElementsOfEachBlock*4), LNewBlock.position, 4 );
      end;
      // Clear next
      LZero := 0;
      FAbstractMem.Write( ABlockPointer + (FElementsOfEachBlock*4), LZero, 4 );
    end;
    if (i<LBlockIndex) then begin
      APreviousBlockPointer := ABlockPointer;
      // Read
      FAbstractMem.Read( ABlockPointer + (FElementsOfEachBlock*4), ABlockPointer, 4 );
    end;
    inc(i);
  until (i > LBlockIndex);
end;

function TAbstractMemTList.GetPosition(AIndex: Integer): TAbstractMemPosition;
var LBlockPointer,LPreviousBlockPointer : TAbstractMemPosition;
  LIndexInBlock : Integer;
begin
  Result := 0;
  FAbstractMemTListLock.Acquire;
  try
  if (UseCacheData) then begin
    if (AIndex<0) or (AIndex>=FNextElementPosition) then raise EAbstractMemTList.Create(Format('%s index %d out of range 0..%d',[ClassName,AIndex,FNextElementPosition-1]));
    Move( FCacheData[AIndex*4], Result, 4);
  end else begin
    GetPointerTo(AIndex,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    FAbstractMem.Read( LBlockPointer + (LIndexInBlock*4), Result, 4);
  end;
  finally
    FAbstractMemTListLock.Release;
  end;
end;

procedure TAbstractMemTList.Initialize(const AInitialZone: TAMZone; ADefaultElementsPerBlock: Integer);
var LBytes : TBytes;
  i : Integer;
begin
  FInitialZone := AInitialZone;
  // Try to read
  FElementsOfEachBlock := 0;
  FFirstBlockPointer := 0;
  FNextElementPosition := 0;
  SetLength(LBytes,CT_AbstractMemTList_HeaderSize);
  try
    if (FInitialZone.position>0) And ((FInitialZone.size=0) or (FInitialZone.size>=CT_AbstractMemTList_HeaderSize)) then begin
      FAbstractMem.Read(FInitialZone.position,LBytes[0],CT_AbstractMemTList_HeaderSize);
      if Length(CT_AbstractMemTList_Magic)<>4 then raise EAbstractMemTList.Create('Invalid CT_AbstractMemTList_Magic size!');
      // Check magic
      for i := 0 to CT_AbstractMemTList_Magic.Length-1 do begin
        if LBytes[i]<>Ord(CT_AbstractMemTList_Magic.Chars[i]) then Exit;
      end;
      // Capture Size
      Move(LBytes[4],FElementsOfEachBlock,4);
      Move(LBytes[8],FNextElementPosition,4);
      Move(LBytes[12],FFirstBlockPointer,4);
      if (FElementsOfEachBlock<=0) then begin
        // Not valid
        FElementsOfEachBlock := 0;
        FFirstBlockPointer := 0;
        FNextElementPosition := 0;
      end;
    end;
  finally
    if (FInitialZone.position>0) and (FElementsOfEachBlock<=0) and ((FInitialZone.size=0) or (FInitialZone.size>=CT_AbstractMemTList_HeaderSize))  then begin
      // Need to initialize and save
      FElementsOfEachBlock := ADefaultElementsPerBlock;
      if FElementsOfEachBlock<=0 then raise EAbstractMemTList.Create('Invalid Default Elements per block');

      for i := 0 to CT_AbstractMemTList_Magic.Length-1 do begin
        LBytes[i] := Byte(Ord(CT_AbstractMemTList_Magic.Chars[i]));
      end;
      Move(FElementsOfEachBlock,LBytes[4],4);
      Move(FNextElementPosition,LBytes[8],4);
      Move(FFirstBlockPointer,LBytes[12],4);
      // Save header
      FAbstractMem.Write( FInitialZone.position, LBytes[0], Length(LBytes) );
    end;
  end;
end;

procedure TAbstractMemTList.Insert(AIndex: Integer; const APosition: TAbstractMemPosition);
var LBlockPointer,LPreviousBlockPointer : TAbstractMemPosition;
  LIndexInBlock : Integer;
begin
  FAbstractMemTListLock.Acquire;
  try
  AddRange(AIndex,1);
  if (UseCacheData) then begin
    Move(APosition, FCacheData[AIndex*4], 4);
    FCacheUpdated := True;
  end else begin
    GetPointerTo(AIndex,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    FAbstractMem.Write( LBlockPointer + (LIndexInBlock*4), APosition, 4 );
  end;
  finally
    FAbstractMemTListLock.Release;
  end;
end;

procedure TAbstractMemTList.LoadElements(AIndexStart: Integer; var AElements: TBytes);
var LBlockPointer, LPreviousBlockPointer : TAbstractMemPosition;
  LIndexInBlock, i, j : Integer;
begin
  CheckInitialized;
  if (AIndexStart<0) or (AIndexStart>FNextElementPosition) then raise EAbstractMemTList.Create(Format('%s LoadElements out of range %d in 0..%d',[ClassName,AIndexStart,FNextElementPosition-1]));

  SetLength(AElements, (FNextElementPosition - AIndexStart)*4);

  i := AIndexStart;
  while (i<FNextElementPosition) do begin
    GetPointerTo( i ,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    // Load this
    j := FElementsOfEachBlock - LIndexInBlock;
    if (i + j -1) >= FNextElementPosition then j := FNextElementPosition - i;

    FAbstractMem.Read(LBlockPointer + (LindexInBlock * 4), AElements[ (i-AIndexStart)*4 ], (j)*4  );

    inc(i,j);
  end;
end;

procedure TAbstractMemTList.LockList;
begin
  FAbstractMemTListLock.Acquire;
end;

procedure TAbstractMemTList.RemoveRange(AIndexStart, ARemoveCount: Integer);
var LBlockPointer, LPreviousBlockPointer, LNext : TAbstractMemPosition;
  LIndexInBlock, i, j, n : Integer;
  LElements : TBytes;
  LBlocksBefore, LBlocksAfter : Integer;
begin
  FAbstractMemTListLock.Acquire;
  try
  if (ARemoveCount<=0) then raise EAbstractMemTList.Create(Format('%s remove count %d',[ClassName,ARemoveCount]));
  if (AIndexStart+ARemoveCount-1>=FNextElementPosition) then begin
    if (FNextElementPosition>0) then
      raise EAbstractMemTList.Create(Format('%s remove %d..%d out of range 0..%d',[ClassName,AIndexStart,AIndexStart + ARemoveCount -1, FNextElementPosition-1]))
    else raise EAbstractMemTList.Create(Format('%s remove %d..%d out of range (NO ELEMENTS)',[ClassName,AIndexStart,AIndexStart + ARemoveCount -1]))
  end;

  if (UseCacheData) then begin
    if (AIndexStart+ARemoveCount < FNextElementPosition) then begin
      Move(FCacheData[(AIndexStart + ARemoveCount) *4],
           FCacheData[(AIndexStart) *4],
           FCacheDataUsedBytes-((AIndexStart + ARemoveCount)*4));
    end;
    Dec(FCacheDataUsedBytes,(ARemoveCount*4));
    FCacheUpdated := True;
    Dec(FNextElementPosition,ARemoveCount);
    Exit;
  end;

  LoadElements(AIndexStart+ARemoveCount,LElements);
  n := 0; // n = Elements moved
  //
  i := AIndexStart+ARemoveCount-1;
  // i = first position to move "left"

  repeat
    GetPointerTo(i,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    // Move from LIndexInBlock to FElementsOfEachBlock-1 in this block
    j := FElementsOfEachBlock - (LIndexInBlock);
    if ((n+j)*4>Length(LElements)) then j := (Length(LElements) DIV 4)-n;
    FAbstractMem.Write( LBlockPointer + (LIndexInBlock*4), LElements[ n*4 ], j*4 );
    inc(n,j);
    inc(i,j);
  until (i >= FNextElementPosition - ARemoveCount);// or (j=0);

  LBlocksBefore := ((FNextElementPosition DIV FElementsOfEachBlock)+1);
  LBlocksAfter := (((FNextElementPosition-ARemoveCount) DIV FElementsOfEachBlock)+1);

  if (LBlocksBefore<LBlocksAfter) then begin
    GetPointerTo(FNextElementPosition-ARemoveCount,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    while (LBlockPointer>0) do begin
      FAbstractMem.Read( LBlockPointer + (FElementsOfEachBlock * 4), LNext, 4);
      FAbstractMem.Dispose(LBlockPointer);
      LBlockPointer := LNext;
      //
      if LPreviousBlockPointer>0 then begin
        LNext := 0;
        FAbstractMem.Write( LPreviousBlockPointer + (FElementsOfEachBlock * 4), LNext, 4);
      end else begin
        // This is first block pointer
        FFirstBlockPointer := 0;
        FAbstractMem.Write( FInitialZone.position + 12, FFirstBlockPointer, 4 );
      end;
    end;

  end;

  // Save to header
  Dec(FNextElementPosition,ARemoveCount);
  FAbstractMem.Write( FInitialZone.position + 8, FNextElementPosition, 4 );
  finally
    FAbstractMemTListLock.Release;
  end;
end;

procedure TAbstractMemTList.SetPosition(AIndex: Integer; const Value: TAbstractMemPosition);
var LBlockPointer, LPreviousBlockPointer : TAbstractMemPosition;
  LIndexInBlock : Integer;
begin
  FAbstractMemTListLock.Acquire;
  try
  if (UseCacheData) then begin
    Move( Value, FCacheData[AIndex*4], 4);
    FCacheUpdated := True;
  end else begin
    GetPointerTo(AIndex,False,LPreviousBlockPointer,LBlockPointer,LIndexInBlock);
    FAbstractMem.Write( LBlockPointer + (LIndexInBlock*4), Value, 4);
  end;
  finally
    FAbstractMemTListLock.Release;
  end;
end;

procedure TAbstractMemTList.SetUseCache(const Value: Boolean);
begin
  if (Value=FUseCache) then Exit;
  if (FUseCache) then begin
    FlushCache;
    SetLength(FCacheData,0);
    FCacheDataUsedBytes := 0;
  end else begin
    SetLength(FCacheData,0);
    FCacheDataLoaded := False;
    FCacheUpdated := False;
    FCacheDataUsedBytes := 0;
  end;
  FUseCache := Value;
end;

procedure TAbstractMemTList.UnlockList;
begin
  FAbstractMemTListLock.Release;
end;

function TAbstractMemTList.UseCacheData: Boolean;
begin
  if FUseCache then begin
    Result := True;
    if Not FCacheDataLoaded then begin
      FCacheDataLoaded := True;
      LoadElements(0,FCacheData);
      FCacheDataUsedBytes := Length(FCacheData);
    end;
  end else Result := False;
end;

{ TAbstractMemTListBaseAbstract<T> }

function TAbstractMemTListBaseAbstract<T>.Add(const AItem: T): Integer;
var
  LBytes : TBytes;
  LZone : TAMZone;
begin
  FList.LockList;
  try
  SetLength(LBytes,0);
  Self.SaveTo(AItem,True,LBytes);
  if (Length(LBytes)>0) then begin
    LZone := FList.AbstractMem.New( Length(LBytes) );
    FList.AbstractMem.Write( LZone.position, LBytes[0], Length(LBytes) );
  end else LZone.Clear;
  Result := FList.Add( LZone.position );
  finally
    FList.UnlockList;
  end;
end;

procedure TAbstractMemTListBaseAbstract<T>.Clear;
var i : Integer;
  LPosition : TAbstractMemPosition;
begin
  FList.LockList;
  try
  for i := 0 to FList.Count-1 do begin
    LPosition := FList.Position[ i ];
    FList.AbstractMem.Dispose( LPosition );
  end;
  FList.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TAbstractMemTListBaseAbstract<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TAbstractMemTListBaseAbstract<T>.Create(AAbstractMem: TAbstractMem;
  const AInitialZone: TAMZone; ADefaultElementsPerBlock: Integer; AUseCache : Boolean);
begin
  FAbstractMem := AAbstractMem;
  FList := TAbstractMemTList.Create(AAbstractMem,AInitialZone,ADefaultElementsPerBlock,AUseCache);
end;

procedure TAbstractMemTListBaseAbstract<T>.Delete(index: Integer);
var LPosition : TAbstractMemPosition;
begin
  FList.LockList;
  try
  LPosition := FList.Position[ index ];
  FList.AbstractMem.Dispose( LPosition );
  FList.Delete( index );
  finally
    FList.UnlockList;
  end;
end;

destructor TAbstractMemTListBaseAbstract<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TAbstractMemTListBaseAbstract<T>.Dispose;
begin
  Clear;
  FList.Dispose;
end;

procedure TAbstractMemTListBaseAbstract<T>.FlushCache;
begin
  FList.FlushCache;
end;

function TAbstractMemTListBaseAbstract<T>.GetInitialZone: TAMZone;
begin
  Result := FList.InitialiZone;
end;

function TAbstractMemTListBaseAbstract<T>.GetItem(index: Integer): T;
var
  LPosition : TAbstractMemPosition;
  LZone : TAMZone;
  LBytes : TBytes;
begin
  FList.LockList;
  try
  LPosition := FList.Position[ index ];
  if (LPosition>0) then begin
    if Not FList.AbstractMem.GetUsedZoneInfo( LPosition, False, LZone) then
      raise EAbstractMemTList.Create(Format('%s.GetItem Inconsistency error used zone info not found at index %d at pos %d',[Self.ClassName,index,LPosition]));
    SetLength(LBytes,LZone.size);
    if FList.AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
      raise EAbstractMemTList.Create(Format('%s.GetItem Inconsistency error cannot read %d bytes for index %d at pos %d',[Self.ClassName,LZone.size,index,LPosition]));
  end else SetLength(LBytes,0);
  LoadFrom(LBytes, Result );
  finally
    FList.UnlockList;
  end;
end;

function TAbstractMemTListBaseAbstract<T>.GetUseCache: Boolean;
begin
  FList.LockList;
  try
    Result := FList.UseCache;
  finally
    FList.UnlockList;
  end;
end;

procedure TAbstractMemTListBaseAbstract<T>.SetItem(index: Integer;
  const AItem: T);
var
  LBytes : TBytes;
  LZone : TAMZone;
  LPreviousElementPosition : TAbstractMemPosition;
begin
  FList.LockList;
  try
  LPreviousElementPosition := FList.Position[ index ];
  if (LPreviousElementPosition>0) then begin
    // Had value
    if Not FList.AbstractMem.GetUsedZoneInfo( LPreviousElementPosition, False, LZone) then
      raise EAbstractMemTList.Create(Format('%s.SetItem Inconsistency error used zone info not found at index %d at pos %d',[Self.ClassName,index,LPreviousElementPosition]));
    SetLength(LBytes,LZone.size);
    if FList.AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
      raise EAbstractMemTList.Create(Format('%s.SetItem Inconsistency error cannot read %d bytes for index %d at pos %d',[Self.ClassName,LZone.size,index,LPreviousElementPosition]));
  end else begin
    SetLength(LBytes,0);
    LZone.Clear;
  end;

  Self.SaveTo(AItem,False,LBytes);

  if (LPreviousElementPosition>0) and ((Length(LBytes)>LZone.size) or (Length(LBytes)=0)) then begin
    // Dispose previous element
    FList.AbstractMem.Dispose( LPreviousElementPosition );
    LZone.Clear;
  end;
  if (Length(LBytes)>0) then begin
    if (LZone.position=0) then begin
      // Create new zone
      LZone := FList.AbstractMem.New( Length(LBytes) );
    end;
    FList.AbstractMem.Write( LZone.position, LBytes[0], Length(LBytes) );
    FList.Position[ index ] := LZone.position;
  end else begin
    // Save a 0 position
    FList.Position[ index ] := 0;
  end;
  finally
    FList.UnlockList;
  end;
end;

procedure TAbstractMemTListBaseAbstract<T>.SetUseCache(const Value: Boolean);
begin
  FList.LockList;
  try
    FList.UseCache := Value;
  finally
    FList.UnlockList;
  end;
end;

function TAbstractMemTListBaseAbstract<T>.ToString(const AItem: T): String;
begin
  Result := Self.ClassName+'.T '+IntToStr(SizeOf(AItem));
end;

{ TAbstractMemOrderedTList<T> }

function TAbstractMemOrderedTList<T>.Add(const AItem: T): Integer;
var
  LFound : Boolean;
  LBytes : TBytes;
  LZone : TAMZone;
begin
  FList.LockList;
  try
  LFound := Find(AItem,Result);
  if (LFound and FAllowDuplicates) or (Not LFound) then begin
    SetLength(LBytes,0);
    Self.SaveTo(AItem,True,LBytes);
    if (Length(LBytes)>0) then begin
      LZone := FList.AbstractMem.New( Length(LBytes) );
      FList.AbstractMem.Write( LZone.position, LBytes[0], Length(LBytes) );
    end else LZone.Clear;
    FList.Insert( Result , LZone.position );
  end else Result := -1;
  finally
    FList.UnlockList;
  end;
end;

constructor TAbstractMemOrderedTList<T>.Create(AAbstractMem: TAbstractMem;
  const AInitialZone: TAMZone; ADefaultElementsPerBlock: Integer;
  AAllowDuplicates, AUseCache: Boolean);
begin
  inherited Create(AAbstractMem, AInitialZone, ADefaultElementsPerBlock, AUseCache);
  FAllowDuplicates := AAllowDuplicates;
end;

function TAbstractMemOrderedTList<T>.Find(const AItemToFind: T; out AIndex: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
  LLeft : T;
begin
  FList.LockList;
  try
  Result := False;
  L := 0;
  H := FList.Count - 1;
  // Optimization when inserting always a ordered list
  if (H>0) then begin
    LLeft := GetItem( H );
    C := Compare(LLeft, AItemToFind);
    if (C<0) then begin
      AIndex := H+1;
      Exit;
    end else if (C=0) then begin
      AIndex := H; // When equals, insert to the left
      Result := True;
      Exit;
    end;
  end;
  while L <= H do
  begin
    I := (L + H) shr 1;

    LLeft := GetItem( I );

    C := Compare(LLeft, AItemToFind);

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
  AIndex := L;
  finally
    FList.UnlockList;
  end;
end;

function TAbstractMemOrderedTList<T>.Get(index: Integer): T;
begin
  Result := GetItem(index);
end;

function TAbstractMemOrderedTList<T>.IndexOf(const AItem: T): Integer;
begin
  If Not Find(AItem,Result) then Result := -1;
end;

end.
