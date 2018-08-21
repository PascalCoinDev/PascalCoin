unit UMemory;

{ Copyright (c) 2017 - 2018 Sphere 10 Software <https://www.sphere10.com>

  Memory management routines.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author
  - Dmitriy Pomerantsev <pda2@yandex.ru>: scope management inspired by https://github.com/pda0/AutoScope/blob/master/AutoScope.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}


{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

type

  { TDisposePolicy }

  TDisposePolicy = (idpNone, idpNil, idpFree, idpFreeAndNil, idpRelease, idpFreeMem);

  { TDisposables }

  TDisposables = record
    private type
       PDisposables = ^TDisposables;

       TDisposablePointer = record
         Ptr: Pointer;
         DisposePolicy : TDisposePolicy;
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
      procedure RegisterPointer(Ptr: Pointer; ADisposePolicy: TDisposePolicy);
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
    case ADisposables.FPointers[i].DisposePolicy of
      idpNone: ;
      idpNil: ADisposables.FPointers[i].Ptr := nil;
      idpFree, idpFreeAndNil: if Assigned(ADisposables.FPointers[i].Ptr) then begin
        FreeAndNil(ADisposables.FPointers[i].Ptr);
        ADisposables.FPointers[i].Ptr := nil;
      end;
      idpRelease: begin
        raise ENotSupportedException.Create('Dispose policy idoRelease not supported');
      end;
      idpFreeMem: if Assigned(ADisposables.FPointers[i].Ptr) then System.FreeMem(ADisposables.FPointers[i].Ptr);
      else raise ENotSupportedException.Create('Dispose policy not supported');;
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

procedure TDisposables.RegisterPointer(Ptr: Pointer; ADisposePolicy: TDisposePolicy);
begin
  if FLastIndex > High(FPointers) then
    SetLength(FPointers, Length(FPointers) * 2);

  Inc(FLastIndex);
  FPointers[FLastIndex].Ptr := Ptr;
  FPointers[FLastIndex].DisposePolicy := ADisposePolicy;
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
  RegisterPointer(Pointer(AnObject), idpFree);
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
  RegisterPointer(P, idpFreeMem);
end;

procedure TDisposables.ReallocMem(var P: Pointer; Size:Integer);
var
  i: Integer;
begin
  if not Assigned(FGuardian) then
    FGuardian := TGuard.Create(@Self);

  for i := FLastIndex downto 0 do
    if (FPointers[i].DisposePolicy = idpFreeMem) and (FPointers[i].Ptr = P) then
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

end.
