{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpOSRandom;

{$I CryptoLib.inc}

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Windows,
{$ELSEIF DEFINED(IOSDELPHI)}
  // iOS stuffs for Delphi
  Macapi.Dispatch,
  iOSapi.Foundation,
{$ELSEIF DEFINED(IOSFPC)}
  // iOS stuffs for FreePascal
{$LINKFRAMEWORK Security}
{$ELSE}
  Classes,
  SysUtils,
{$IFEND MSWINDOWS}
  ClpCryptoLibTypes;

resourcestring
{$IF DEFINED(MSWINDOWS)}
  SMSWIndowsCryptoAPIGenerationError =
    'An Error Occured while generating random data using MS WIndows Crypto API.';
{$ELSEIF (DEFINED(IOSDELPHI) OR DEFINED(IOSFPC))}
  SIOSSecRandomCopyBytesGenerationError =
    'An Error Occured while generating random data using SecRandomCopyBytes API.';
{$ELSE}
  SUnixRandomReadError =
    'An Error Occured while reading random data from /dev/urandom or /dev/random.';
{$IFEND MSWINDOWS}

type

  /// <summary>
  /// <para>
  /// TOSRandom Number Class.
  /// </para>
  /// <para>
  /// This class returns random bytes from an OS-specific randomness
  /// source. The returned data should be unpredictable enough for
  /// cryptographic applications, though its exact quality depends on the
  /// OS implementation.
  /// On a UNIX-like system this will read directly from /dev/urandom or /dev/random
  /// (if the former is not available),
  /// on iOS, calls SecRandomCopyBytes as /dev/(u)random is sandboxed,
  /// on MSWINDOWS it will call CryptGenRandom().
  /// </para>
  /// </summary>

  TOSRandom = class sealed(TObject)

  strict private

    class function NoZeroes(const data: TCryptoLibByteArray): Boolean;
      static; inline;

{$IF DEFINED(MSWINDOWS)}
    class function GenRandomBytesWindows(len: Int32; const data: PByte): Int32;
{$ELSEIF DEFINED(IOSDELPHI)}
    class function GenRandomBytesIOSDelphi(len: Int32;
      const data: PByte): Int32;
{$ELSEIF DEFINED(IOSFPC)}
    class function GenRandomBytesIOSFPC(len: Int32; const data: PByte): Int32;
{$ELSE}
    class function GenRandomBytesUnix(len: Int32; const data: PByte): Int32;
{$IFEND $MSWINDOWS}
  public

    class procedure GetBytes(const data: TCryptoLibByteArray); static;
    class procedure GetNonZeroBytes(const data: TCryptoLibByteArray); static;

  end;

{$IFDEF MSWINDOWS}

const
  ADVAPI32 = 'advapi32.dll';

function CryptAcquireContextW(phProv: Pointer; pszContainer: LPCWSTR;
  pszProvider: LPCWSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
  external ADVAPI32 Name 'CryptAcquireContextW';

function CryptGenRandom(hProv: THandle; dwLen: DWORD; pbBuffer: PByte): BOOL;
  stdcall; external ADVAPI32 Name 'CryptGenRandom';

function CryptReleaseContext(hProv: THandle; dwFlags: DWORD): BOOL; stdcall;
  external ADVAPI32 Name 'CryptReleaseContext';
{$ENDIF MSWINDOWS}
{$IFDEF IOSDELPHI}

type
  SecRandomRef = Pointer;

const
  libSecurity = '/System/Library/Frameworks/Security.framework/Security';

function kSecRandomDefault: Pointer;

function SecRandomCopyBytes(rnd: SecRandomRef; count: LongWord; bytes: PByte)
  : Integer; cdecl; external libSecurity Name _PU + 'SecRandomCopyBytes';
{$ENDIF IOSDELPHI}
{$IFDEF IOSFPC}

type
  // similar to a TOpaqueData already defined in newer FPC but not available in 3.0.4
  __SecRandom = record end;
 // similar to an OpaquePointer already defined in newer FPC but not available in 3.0.4
  SecRandomRef = ^__SecRandom;

const
  { * This is a synonym for NULL, if you'd rather use a named constant.   This
    refers to a cryptographically secure random number generator.  * }
  kSecRandomDefault: SecRandomRef = Nil;

function SecRandomCopyBytes(rnd: SecRandomRef; count: LongWord; bytes: PByte)
  : Integer; cdecl; external;

{$ENDIF IOSFPC}

implementation

{$IFDEF IOSDELPHI}

function kSecRandomDefault: Pointer;
begin
  result := CocoaPointerConst(libSecurity, 'kSecRandomDefault');
end;
{$ENDIF IOSDELPHI}

class function TOSRandom.NoZeroes(const data: TCryptoLibByteArray): Boolean;
var
  i: Int32;
begin
  result := True;
  for i := System.Low(data) to System.High(data) do
  begin
    if data[i] = 0 then
    begin
      result := False;
      Exit;
    end;
  end;

end;

{$IF DEFINED(MSWINDOWS)}

class function TOSRandom.GenRandomBytesWindows(len: Int32;
  const data: PByte): Int32;
var
  hProv: THandle;
const
  PROV_RSA_FULL = 1;
  CRYPT_VERIFYCONTEXT = DWORD($F0000000);
  CRYPT_SILENT = $00000040;
begin
  if not CryptAcquireContextW(@hProv, nil, nil, PROV_RSA_FULL,
    CRYPT_VERIFYCONTEXT or CRYPT_SILENT) then
  begin
    result := HResultFromWin32(GetLastError);
    Exit;
  end;

  try
    if not CryptGenRandom(hProv, len, data) then
    begin
      result := HResultFromWin32(GetLastError);
      Exit;
    end;
  finally
    CryptReleaseContext(hProv, 0);
  end;

  result := S_OK;
end;

{$ELSEIF DEFINED(IOSDELPHI)}

class function TOSRandom.GenRandomBytesIOSDelphi(len: Int32;
  const data: PByte): Int32;
begin
  result := SecRandomCopyBytes(kSecRandomDefault, LongWord(len), data);
end;

{$ELSEIF DEFINED(IOSFPC)}

class function TOSRandom.GenRandomBytesIOSFPC(len: Int32;
  const data: PByte): Int32;
begin
  // UNTESTED !!!, Please Take Note.
  result := SecRandomCopyBytes(kSecRandomDefault, LongWord(len), data);
end;
{$ELSE}

class function TOSRandom.GenRandomBytesUnix(len: Int32;
  const data: PByte): Int32;
var
  LStream: TFileStream;
  RandGen: String;
begin
  RandGen := '/dev/urandom';
  if not FileExists(RandGen) then
  begin
    RandGen := '/dev/random';
    if not FileExists(RandGen) then
    begin
      result := -1;
      Exit;
    end;
  end;

  LStream := TFileStream.Create(RandGen, fmOpenRead);

  try
    try
      LStream.ReadBuffer(data[0], len);
      result := 0;
    except
      result := -1;
    end;
  finally
    LStream.Free;
  end;
end;

{$IFEND MSWINDOWS}

class procedure TOSRandom.GetBytes(const data: TCryptoLibByteArray);
var
  count: Int32;
begin
  count := System.Length(data);
{$IF DEFINED(MSWINDOWS)}
  if GenRandomBytesWindows(count, PByte(data)) <> 0 then
  begin
    raise EAccessCryptoLibException.CreateRes
      (@SMSWIndowsCryptoAPIGenerationError);
  end;

{$ELSEIF DEFINED(IOSDELPHI)}
  if GenRandomBytesIOSDelphi(count, PByte(data)) <> 0 then
  begin
    raise EAccessCryptoLibException.CreateRes
      (@SIOSSecRandomCopyBytesGenerationError);
  end;

{$ELSEIF DEFINED(IOSFPC)}
  if GenRandomBytesIOSFPC(count, PByte(data)) <> 0 then
  begin
    raise EAccessCryptoLibException.CreateRes
      (@SIOSSecRandomCopyBytesGenerationError);
  end;

{$ELSE}
  if GenRandomBytesUnix(count, PByte(data)) <> 0 then
  begin
    raise EAccessCryptoLibException.CreateRes(@SUnixRandomReadError);
  end;
{$IFEND MSWINDOWS}
end;

class procedure TOSRandom.GetNonZeroBytes(const data: TCryptoLibByteArray);
begin
  repeat
    TOSRandom.GetBytes(data);
  until (NoZeroes(data));
end;

end.
