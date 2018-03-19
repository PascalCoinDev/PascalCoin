{**********************************************************************
    ● Copyright(c) 2017 Dmitriy Pomerantsev <pda2@yandex.ru>
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    https://github.com/pda0/AutoScope
    Ver 1.0.2
    + Added some new tests.
    + Added `paranoia' mode (disabled by default). Define WITH_PARANOIA if you
      want. TScoped will place a reference to itself in an external variable to
      prevent too smart compiler from removing the record prematurely.
    Ver 1.0.1
    * Now cleanup process is protected from destructor's exceptions.
      It may not work temporarily in llvm-based compiler because of the bug
      https://quality.embarcadero.com/browse/RSP-18031
    Ver 1.0.0
    * Initial release.
 **********************************************************************}
unit UAutoScope;
{$IFDEF FPC}
  {$CODEPAGE UTF8}
  {$MODE DELPHI}{$H+}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

{$IFDEF FPC}
  { #%^$! delphi compiller!!! }
  {$IFDEF VER1}{$ERROR Too old compiller.}{$ENDIF}
  {$IFDEF VER2}
    {$IFDEF VER2_0}{$ERROR Too old compiller.}{$ENDIF}
    {$IFDEF VER2_2}{$ERROR Too old compiller.}{$ENDIF}
    {$IFDEF VER2_4}{$ERROR Too old compiller.}{$ENDIF}
  {$ENDIF}
  {$DEFINE USE_INTERFACE}
  //{$IFNDEF VER_3_0}
  //  {$IFDEF USE_INTERFACE}{$UNDEF USE_INTERFACE}{$ENDIF}
  //  {$DEFINE USE_OPERATORS}
  //{$ENDIF}
{$ELSE}
  {$DEFINE USE_INTERFACE}
{$ENDIF}

type
  TScopedPtr = record
    Ptr: Pointer;
    {$IFNDEF NEXTGEN}
    IsObject: Boolean;
    {$ENDIF}
  end;
  {$IFDEF USE_INTERFACE}
    {$DEFINE NEED_SCOPED_PTR}
  {$ENDIF}
  {$IFDEF WITH_PARANOIA}
    {$IFNDEF NEED_SCOPED_PTR}{$DEFINE NEED_SCOPED_PTR}{$ENDIF}
  {$ENDIF}

  {$IFDEF NEED_SCOPED_PTR}
  PScoped = ^TScoped;
  {$ENDIF}

  /// <summary>The automatic memory deallocation object.</summary>
  /// <remarks>
  ///   TScoped automatically free memory when it's instance runs out of scope.
  ///   Do not declare it as const or threadvar variable.
  /// </remarks>
  /// <threadsafety static="false" instance="false" />
  TScoped = record
  {$IFDEF USE_INTERFACE}
  private type
    TScopedGuardian = class(TInterfacedObject)
    private
      FScopedRec: PScoped;
    public
      constructor Create(ScopedRec: PScoped; ACapacity:Integer);
      destructor Destroy; override;
    end;
  {$ENDIF}
  private
    {$IFDEF USE_INTERFACE}
    FGuardian: IInterface;
    {$ENDIF}
    FPointers: array of TScopedPtr;
    FLastIndex: {$IFDEF FPC}TDynArrayIndex{$ELSE}Integer{$ENDIF};
    {$IFDEF USE_OPERATORS}
    class operator Initialize(var AScope: TScoped);
    class operator Finalize(var AScope: TScoped; ACapacity:Integer);
    {$ENDIF}
    {$IFDEF USE_INTERFACE}
    class procedure Initialize(var AScope: TScoped; ACapacity:Integer); static;
    class procedure Finalize(var AScope: TScoped); static;
    {$ENDIF}
    procedure RegisterPointer(Ptr: Pointer; IsObject: Boolean);
    procedure UnregisterPointer(Ptr: Pointer);
  public
    /// <summary>Adds the object to the automatic deletion list.</summary>
    /// <param name="AnObject">A class instance for automatic deletion.</param>
    /// <returns><c>AnObject</c> value as is.</returns>
    /// <remarks>
    ///   <para>
    ///     When an instance of <c>TScoped</c> runs out of a scope, all objects
    ///     will be deleted in the reverse order to the addition.
    ///   </para>
    ///   <para>
    ///     Does nothing in NextGen mode due of ARC.
    ///   </para>
    /// </remarks>
    function AddObject(const AnObject: TObject): TObject;
    /// <summary>Removes the object from the automatic deletion list.</summary>
    /// <param name="AnObject">A class instance for removal from list.</param>
    /// <remarks>
    ///   <para>
    ///     After calling this method, you have to remove the class instance
    ///     by yourself.
    ///   </para>
    ///   <para>
    ///     This method have O(n) complexity because it's not a primary case
    ///     scenario of <c>TScoped</c> usage. If you want to create and destroy
    ///     some class often, please use normal methods like
    ///     <c>Create</c>/<c>Free</c>.
    ///   </para>
    ///   <para>
    ///     Does nothing in NextGen mode due of ARC.
    ///   </para>
    /// </remarks>
    procedure RemoveObject(const AnObject: TObject);

    // HS: added
    procedure InitCapacity(ACapacity : Integer);
    function Count : Integer;
    function ItemAt(AIndex : Integer) : TObject;
    function ScopedPtrAt(AIndex : Integer) : TScopedPtr;

    /// <summary>Allocates an automatically releasing memory block.</summary>
    /// <param name="P">Returns a pointer to allocated memory block.</param>
    /// <param name="Size">Is a size in bytes of required memory.</param>
    /// <remarks>
    ///   When an instance of <c>TScoped</c> runs out of a scope, all memory
    ///   block will be released in the reverse order to the allocation.
    /// </remarks>
    procedure GetMem(out P: Pointer; Size: {$IFDEF FPC}PtrUInt{$ELSE}Integer{$ENDIF});
    /// <summary>Releases previously allocated memory block.</summary>
    /// <param name="P">
    ///   Is a pointer to memory block, allocated by
    ///   <see cref="TScoped.GetMem"/> or added by <see cref="TScoped.AddMem"/>.
    /// </param>
    /// <remarks>
    ///   This method have O(n) complexity because it's not a primary case
    ///   scenario of <c>TScoped</c> usage. If you want to allocate and release
    ///   memory often, please use normal functions like
    ///   <c>System.GetMem</c>/<c>System.FreeMem</c>.
    /// </remarks>
    procedure FreeMem(var P: Pointer);
    /// <summary>Adds a memory block to the automatically releasing list.</summary>
    /// <param name="P">
    ///   Is a pointer to memory block, allocated by somewhere else.
    /// </param>
    /// <remarks>
    ///   <para>
    ///     When an instance of <c>TScoped</c> runs out of a scope, all memory
    ///     block will be released in the reverse order to the addition.
    ///   </para>
    ///   <para>
    ///     Do not try to add pointer to memory block, allocated by
    ///     <see cref="TScoped.GetMem"/>, do not add some pointer more than one
    ///     time.
    ///   </para>
    ///   <para>
    ///     Use only pointer which have to be releasev via
    ///     <see cref="System.FreeMem"/>. Do not use typed pointers, allocated
    ///     by <see cref="New"/>. <c>TScoped</c> is incompatible with typed
    ///     pointers.
    ///   </para>
    /// </remarks>
    procedure AddMem(const P: Pointer);
    /// <summary>Reallocates a memory block.</summary>
    /// <param name="P">
    ///   Is a pointer to memory block, allocated by
    ///   <see cref="TScoped.GetMem"/> or added by <see cref="TScoped.AddMem"/>.
    /// </param>
    /// <remarks>
    ///   This method have O(n) complexity because it's not a primary case
    ///   scenario of <c>TScoped</c> usage. If you want to allocate and release
    ///   memory often, please use normal functions like
    ///   <c>System.GetMem</c>/<c>System.FreeMem</c>.
    /// </remarks>
    procedure ReallocMem(var P: Pointer; Size: {$IFDEF FPC}PtrUInt{$ELSE}Integer{$ENDIF});
    /// <summary>Removes a memory block from the automatic deletion list.</summary>
    /// <param name="P">
    ///   Is a pointer to memory block, allocated by
    ///   <see cref="TScoped.GetMem"/> or added by <see cref="TScoped.AddMem"/>.
    /// </param>
    /// <remarks>
    ///   <para>
    ///     After calling this method, you have to release the memory block
    ///     by yourself.
    ///   </para>
    ///   <para>
    ///     This method have O(n) complexity because it's not a primary case
    ///     scenario of <c>TScoped</c> usage. If you want to allocate and
    ///     release memory often, please use normal functions like
    ///     <c>System.GetMem</c>/<c>System.FreeMem</c>.
    ///   </para>
    /// </remarks>
    procedure RemoveMem(const P: Pointer);
    /// <summary>A syntax sugar for the AddObject method.</summary>
    property Objects[const AnObject: TObject]: TObject read AddObject; default;
  end;

{$IFDEF WITH_PARANOIA}
var
  __no_use_ptr: PScoped;
{$ENDIF}

implementation

uses sysutils;

{ TScoped }

{$IFDEF USE_INTERFACE}
constructor TScoped.TScopedGuardian.Create(ScopedRec: PScoped; ACapacity:Integer);
begin
  FScopedRec := ScopedRec;
  TScoped.Initialize(FScopedRec^, ACapacity);
end;

destructor TScoped.TScopedGuardian.Destroy;
begin
  inherited;
  try
    TScoped.Finalize(FScopedRec^);
  except
    FreeInstance;
    raise;
  end;
end;
{$ENDIF}

{$IFDEF USE_OPERATORS}
class operator TScoped.Initialize(var AScope: TScoped);
{$ENDIF}
{$IFDEF USE_INTERFACE}
class procedure TScoped.Initialize(var AScope: TScoped; ACapacity:Integer);
{$ENDIF}
begin
  {$IFDEF WITH_PARANOIA}
   __no_use_ptr := @AScope;
  {$ENDIF}
  AScope.FLastIndex := -1;
  SetLength(AScope.FPointers, ACapacity);
end;

{$IFDEF USE_OPERATORS}
class operator TScoped.Finalize(var AScope: TScoped);
{$ENDIF}
{$IFDEF USE_INTERFACE}
class procedure TScoped.Finalize(var AScope: TScoped);
{$ENDIF}
var
  {$IFNDEF NEXTGEN}
  FirstException: Pointer;
  {$ENDIF}
  i: {$IFDEF FPC}TDynArrayIndex{$ELSE}Integer{$ENDIF};
begin
  FirstException := nil;

  for i := AScope.FLastIndex downto 0 do
  {$IFNDEF NEXTGEN}
  try
    if AScope.FPointers[i].IsObject then
      TObject(AScope.FPointers[i].Ptr).Free
    else begin
  {$ENDIF}
      if Assigned(AScope.FPointers[i].Ptr) then
        System.FreeMem(AScope.FPointers[i].Ptr);
  {$IFNDEF NEXTGEN}
    end;
  except
    if not Assigned(FirstException) then
      FirstException := AcquireExceptionObject;
  end;

  if Assigned(FirstException) then
  begin
    SetLength(AScope.FPointers, 0);
    raise TObject(FirstException);
  end;
  {$ENDIF}
end;

{ TScoped is for small amount of local objects or memory blocks, which will be
  created at start of a routine, deleted at the end, and very rarely at the
  middle of the execution. Therefore there is no need for complex methods of
  low `big O' complexity. The simplicity and fast of primary case scenario speed
  is preferred. }

procedure TScoped.RegisterPointer(Ptr: Pointer; IsObject: Boolean);
begin
  if FLastIndex > High(FPointers) then
    SetLength(FPointers, Length(FPointers) * 2);

  Inc(FLastIndex);
  FPointers[FLastIndex].Ptr := Ptr;
  {$IFNDEF NEXTGEN}
  FPointers[FLastIndex].IsObject := IsObject;
  {$ENDIF}
end;

procedure TScoped.UnregisterPointer(Ptr: Pointer);
var
  i: {$IFDEF FPC}TDynArrayIndex{$ELSE}Integer{$ENDIF};
begin
  for i := 0 to FLastIndex do
    if FPointers[i].Ptr = Ptr then
    begin
      FPointers[i].Ptr := nil;
      Break;
    end;
end;

function TScoped.AddObject(const AnObject: TObject): TObject;
begin
  {$IFNDEF NEXTGEN}
    {$IFDEF USE_INTERFACE}
    if not Assigned(FGuardian) then
      FGuardian := TScopedGuardian.Create(@Self, 16);
    {$ENDIF}

  RegisterPointer(Pointer(AnObject), True);
  {$ENDIF}
  Result := AnObject;
end;

procedure TScoped.RemoveObject(const AnObject: TObject);
begin
  {$IFNDEF NEXTGEN}
    {$IFDEF USE_INTERFACE}
    if not Assigned(FGuardian) then
      FGuardian := TScopedGuardian.Create(@Self, 16);
    {$ENDIF}

  UnregisterPointer(Pointer(AnObject));
  {$ENDIF}
end;

procedure TScoped.InitCapacity(ACapacity : Integer);
begin
  {$IFNDEF NEXTGEN}
    {$IFDEF USE_INTERFACE}
    if Assigned(FGuardian) then
      raise Exception.Create('Already initialized');
    FGuardian := TScopedGuardian.Create(@Self, ACapacity);
    {$ENDIF}
  {$ENDIF}
end;

function TScoped.Count : Integer;
begin
  if not Assigned(FGuardian) then
    Exit(0);
  Result := FLastIndex + 1;
end;

function TScoped.ItemAt(AIndex : Integer) : TObject;
var sp : TScopedPtr;
begin
  sp := FPointers[AIndex];
  Result := TObject(sp.Ptr);
end;

function TScoped.ScopedPtrAt(AIndex : Integer) : TScopedPtr;
var sp : TScopedPtr;
begin
  Result := FPointers[AIndex];
end;

procedure TScoped.GetMem(out P: Pointer; Size:
  {$IFDEF FPC}PtrUInt{$ELSE}Integer{$ENDIF});
begin
  System.GetMem(P, Size);
  AddMem(P);
end;

procedure TScoped.FreeMem(var P: Pointer);
begin
  {$IFDEF USE_INTERFACE}
  if not Assigned(FGuardian) then
    FGuardian := TScopedGuardian.Create(@Self, 16);
  {$ENDIF}

  UnregisterPointer(P);
  System.FreeMem(P);
end;

procedure TScoped.AddMem(const P: Pointer);
begin
  {$IFDEF USE_INTERFACE}
  if not Assigned(FGuardian) then
    FGuardian := TScopedGuardian.Create(@Self, 16);
  {$ENDIF}

  RegisterPointer(P, False);
end;

procedure TScoped.ReallocMem(var P: Pointer; Size:
  {$IFDEF FPC}PtrUInt{$ELSE}Integer{$ENDIF});
var
  i: {$IFDEF FPC}TDynArrayIndex{$ELSE}Integer{$ENDIF};
begin
  {$IFDEF USE_INTERFACE}
  if not Assigned(FGuardian) then
    FGuardian := TScopedGuardian.Create(@Self, 16);
  {$ENDIF}

  for i := FLastIndex downto 0 do
    if not FPointers[i].IsObject and (FPointers[i].Ptr = P) then
    begin
      System.ReallocMem(FPointers[i].Ptr, Size);
      P := FPointers[i].Ptr;
      Break;
    end;
end;

procedure TScoped.RemoveMem(const P: Pointer);
begin
  {$IFDEF USE_INTERFACE}
  if not Assigned(FGuardian) then
    FGuardian := TScopedGuardian.Create(@Self, 16);
  {$ENDIF}

  UnregisterPointer(P);
end;

end.
