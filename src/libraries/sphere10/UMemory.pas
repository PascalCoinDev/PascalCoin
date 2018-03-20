{
  Copyright (c) 2017 - 2018 Sphere 10 Software

  Memory management routines.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
    Dmitriy Pomerantsev <pda2@yandex.ru>: scope management inspired by https://github.com/pda0/AutoScope/blob/master/AutoScope.pas
}

unit UMemory;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

type

  { TDisposables }

  TDisposables = record
    private type
       PDisposables = ^TDisposables;

       TDisposablePointer = record
         Ptr: Pointer;
         IsObject: Boolean;
       end;

      TGuard = class(TInterfacedObject)
        private
          FDispoablesRec: PDisposables;
        public
          constructor Create(ADisposablesRec: PDisposables);
          destructor Destroy; override;
      end;
    private
      FGuardian: IInterface;
      FPointers: array of TDisposablePointer;
      FLastIndex: Integer;
      class procedure Initialize(var ADisposables: TDisposables); static;
      class procedure Finalize(var ADisposables: TDisposables); static;
      procedure RegisterPointer(Ptr: Pointer; IsObject: Boolean);
      procedure UnregisterPointer(Ptr: Pointer);
    public
      function AddObject(const AnObject: TObject): TObject;
      procedure RemoveObject(const AnObject: TObject);
      procedure GetMem(out P: Pointer; Size: Integer);
      procedure FreeMem(var P: Pointer);
      procedure AddMem(const P: Pointer);
      procedure ReallocMem(var P: Pointer; Size: Integer);
      procedure RemoveMem(const P: Pointer);
      /// <summary>A syntax sugar for the AddObject method.</summary>
      property Objects[const AnObject: TObject]: TObject read AddObject; default;
  end;

  { TAutoPtr - for disposing single instances }

  TAutoPtr<T> = record
    private type
      __TAutoPtr_T = TAutoPtr<T>;
      __PAutoPtr_T = ^__TAutoPtr_T;
      TGuard = class(TInterfacedObject)
        private
          FOwner: __PAutoPtr_T;
        public
          constructor Create(AAutoPtrRec: __PAutoPtr_T);
          destructor Destroy; override;
      end;
    private
      FGuardian: IInterface;
      FPointer:  Pointer;
      class procedure Initialize(var AAutoPtr: __TAutoPtr_T); static;
      class procedure Finalize(var AAutoPtr: __TAutoPtr_T); static;
      function GetPointer : Pointer; inline;
      procedure SetPointer(Ptr: Pointer); inline;
      function GetObject : TObject; inline;
      procedure SetObject(const AnObject: TObject); inline;
      function GetValue : T; inline;
      procedure SetValue(const AValue : T); inline;
      procedure CheckGuard; inline;
    public
      property Pointer_ : Pointer read GetPointer write SetPointer;
      property Object_ : TObject read GetObject write SetObject;
      property Value : T read GetValue write SetValue;
      procedure Clear;
  end;

implementation

uses sysutils;

{ TDisposables }

constructor TDisposables.TGuard.Create(ADisposablesRec: PDisposables);
begin
  FDispoablesRec := ADisposablesRec;
  TDisposables.Initialize(FDispoablesRec^);
end;

destructor TDisposables.TGuard.Destroy;
begin
  inherited;
  try
    TDisposables.Finalize(FDispoablesRec^);
  except
    FreeInstance;
    raise;
  end;
end;

class procedure TDisposables.Initialize(var ADisposables: TDisposables);
begin
  ADisposables.FLastIndex := -1;
  SetLength(ADisposables.FPointers, 16);
end;

class procedure TDisposables.Finalize(var ADisposables: TDisposables);
var
  FirstException: Pointer;
  i: Integer;
begin
  FirstException := nil;

  for i := ADisposables.FLastIndex downto 0 do
  try
    if ADisposables.FPointers[i].IsObject then
      TObject(ADisposables.FPointers[i].Ptr).Free
    else begin
      if Assigned(ADisposables.FPointers[i].Ptr) then
        System.FreeMem(ADisposables.FPointers[i].Ptr);
    end;
  except
    if not Assigned(FirstException) then
      FirstException := AcquireExceptionObject;
  end;

  if Assigned(FirstException) then
  begin
    SetLength(ADisposables.FPointers, 0);
    raise TObject(FirstException);
  end;
end;

procedure TDisposables.RegisterPointer(Ptr: Pointer; IsObject: Boolean);
begin
  if FLastIndex > High(FPointers) then
    SetLength(FPointers, Length(FPointers) * 2);

  Inc(FLastIndex);
  FPointers[FLastIndex].Ptr := Ptr;
  FPointers[FLastIndex].IsObject := IsObject;
end;

procedure TDisposables.UnregisterPointer(Ptr: Pointer);
var i: Integer;
begin
  for i := 0 to FLastIndex do
    if FPointers[i].Ptr = Ptr then
    begin
      FPointers[i].Ptr := nil;
      Break;
    end;
end;

function TDisposables.AddObject(const AnObject: TObject): TObject;
begin
    if not Assigned(FGuardian) then
      FGuardian := TGuard.Create(@Self);
  RegisterPointer(Pointer(AnObject), True);
  Result := AnObject;
end;

procedure TDisposables.RemoveObject(const AnObject: TObject);
begin
    if not Assigned(FGuardian) then
      FGuardian := TGuard.Create(@Self);
  UnregisterPointer(Pointer(AnObject));
end;

procedure TDisposables.GetMem(out P: Pointer; Size: Integer);
begin
  System.GetMem(P, Size);
  AddMem(P);
end;

procedure TDisposables.FreeMem(var P: Pointer);
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);
  UnregisterPointer(P);
  System.FreeMem(P);
end;

procedure TDisposables.AddMem(const P: Pointer);
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);
  RegisterPointer(P, False);
end;

procedure TDisposables.ReallocMem(var P: Pointer; Size:Integer);
var
  i: Integer;
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);

  for i := FLastIndex downto 0 do
    if not FPointers[i].IsObject and (FPointers[i].Ptr = P) then
    begin
      System.ReallocMem(FPointers[i].Ptr, Size);
      P := FPointers[i].Ptr;
      Break;
    end;
end;

procedure TDisposables.RemoveMem(const P: Pointer);
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);
  UnregisterPointer(P);
end;

{ TAutoPtr }

constructor TAutoPtr<T>.TGuard.Create(AAutoPtrRec: __PAutoPtr_T);
begin
  FOwner := AAutoPtrRec;
  TAutoPtr<T>.Initialize(FOwner^);
end;

destructor TAutoPtr<T>.TGuard.Destroy;
begin
  inherited;
  try
    TAutoPtr<T>.Finalize(FOwner^);
  except
    FreeInstance;
    raise;
  end;
end;

class procedure TAutoPtr<T>.Initialize(var AAutoPtr: __TAutoPtr_T);
begin
  AAutoPtr.FGuardian := nil;
  AAutoPtr.FPointer := nil;
end;

class procedure TAutoPtr<T>.Finalize(var AAutoPtr: __TAutoPtr_T);
begin
  AAutoPtr.Clear;
end;

procedure TAutoPtr<T>.Clear;
begin
  CheckGuard;
  if FPointer <> nil then begin
    TObject(FPointer).Free;
    FPointer := nil;
    // avoid FGuard nullifcation due to recursive calls
  end;
end;

function TAutoPtr<T>.GetPointer : Pointer;
begin
  CheckGuard;
  Result := FPointer;
end;

procedure TAutoPtr<T>.SetPointer(Ptr: Pointer);
begin
  CheckGuard;
  if FPointer <> nil then
    Clear;
  FPointer := Ptr;
end;

function TAutoPtr<T>.GetObject : TObject;
begin
  Result := TObject(GetPointer);
end;

procedure TAutoPtr<T>.SetObject(const AnObject: TObject);
begin
  SetPointer( Pointer(AnObject));
end;

function TAutoPtr<T>.GetValue : TObject;
begin
  Result := T(GetPointer);
end;

procedure TAutoPtr<T>.SetValue(const AValue: T);
begin
  SetObject(TObject(AValue));
end;

procedure TAutoPtr<T>.CheckGuard;
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);
end;

end.
