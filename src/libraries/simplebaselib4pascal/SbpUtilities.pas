unit SbpUtilities;

{$I SimpleBaseLib.inc}

interface

uses
  SysUtils,
  SbpSimpleBaseLibTypes;

type
  TUtilities = class sealed(TObject)

  strict private

    class function HaveChar(c: Char; const list: TSimpleBaseLibCharArray)
      : Boolean; static; inline;

  public

    class function TrimRight(const S: String;
      const trimchars: TSimpleBaseLibCharArray): String; static; inline;

    class function AreArraysEqual(const A, B: TSimpleBaseLibByteArray)
      : Boolean; static;

    class function LowCase(ch: Char): Char; static; inline;

    class function IsUpper(ch: Char): Boolean; static; inline;

    class function IsDigit(ch: Char): Boolean; static; inline;

    class function IsWhiteSpace(ch: Char): Boolean; static; inline;

    class function StringToCharArray(const S: String): TSimpleBaseLibCharArray;
      static; inline;

  end;

implementation

class function TUtilities.AreArraysEqual(const A,
  B: TSimpleBaseLibByteArray): Boolean;
begin
  if System.Length(A) <> System.Length(B) then
  begin
    Result := false;
    Exit;
  end;

  Result := CompareMem(A, B, System.Length(A) * System.SizeOf(Byte));
end;

class function TUtilities.HaveChar(c: Char;
  const list: TSimpleBaseLibCharArray): Boolean;
var
  I: Int32;
begin
  I := 0;
  Result := false;
  While (not Result) and (I < System.Length(list)) do
  begin
    Result := (list[I] = c);
    System.Inc(I);
  end;
end;

class function TUtilities.IsDigit(ch: Char): Boolean;
begin
  Result := (Ord(ch) >= Ord('0')) and (Ord(ch) <= Ord('9'));
end;

class function TUtilities.IsUpper(ch: Char): Boolean;
begin
  Result := (Ord(ch) >= Ord('A')) and (Ord(ch) <= Ord('Z'));
end;

class function TUtilities.IsWhiteSpace(ch: Char): Boolean;
begin
  Result := (ch = ' ') or (Ord(ch) = $85) or (Ord(ch) = $A0) or
    ((Ord(ch) >= $09) and (Ord(ch) <= $0D));
end;

class function TUtilities.LowCase(ch: Char): Char;
begin
  case ch of
    'A' .. 'Z':
      Result := Char((Int32(Ord(ch)) + Int32(Ord('a'))) - Int32(Ord('A')));
  else
    Result := ch;
  end;
end;

class function TUtilities.StringToCharArray(const S: String)
  : TSimpleBaseLibCharArray;
begin
  Result := Nil;
  if System.Length(S) > 0 then
  begin
    System.SetLength(Result, System.Length(S) + 1);
    StrPLCopy(PChar(Result), S, System.Length(Result));
    System.SetLength(Result, System.Length(S)); // to remove the null terminator
  end;
end;

class function TUtilities.TrimRight(const S: String;
  const trimchars: TSimpleBaseLibCharArray): String;
var
  I, Len, LowPoint: Int32;
begin
  Len := System.Length(S);
  I := Len;
  While (I >= 1) and HaveChar(S[I], trimchars) do
  begin
    System.Dec(I);
  end;
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(S);
{$ELSE}
  LowPoint := 1;
{$ENDIF DELPHIXE3_UP}
  if I < LowPoint then
  begin
    Result := ''
  end
  else if I = Len then
  begin
    Result := S
  end
  else
  begin
    Result := System.Copy(S, LowPoint, I);
  end;
end;

end.
