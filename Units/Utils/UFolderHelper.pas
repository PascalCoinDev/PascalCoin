unit UFolderHelper;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Type TFileVersionInfo = record
       HasInfo : Boolean;
       CompanyName : String;
       FileDescription : String;
       FileVersion : String;
       InternalName : String;
       LegalCopyRight : String;
       LegalTradeMarks : String;
       OriginalFileName : String;
       ProductName : String;
       ProductVersion : String;
       Comments : String;
       Debug : Boolean;
       Pre_Release : Boolean;
       Patched : Boolean;
       PrivateBuild : Boolean;
       InfoInferred : Boolean;
       SpecialBuild : Boolean;
     End;

  TFolderHelper = record
  strict private
    {$IFnDEF FPC}
    class function GetFolder(const aCSIDL: Integer): string; static;
    {$ENDIF}
    class function GetAppDataFolder : string; static;
  public
    class function GetPascalCoinDataFolder : string; static;
    class Function GetTFileVersionInfo(Const FileName : String) : TFileVersionInfo; static;
  end;

implementation

uses
{$IFnDEF FPC}
  Windows, ShlObj,
  {$DEFINE FILEVERSIONINFO}
{$ELSE}
  {$IFnDEF LINUX}
  Windows,
  {$DEFINE FILEVERSIONINFO}
  {$ENDIF}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  SysUtils;

{$IFnDEF FPC}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle;
  dwFlags: DWord; pszPath: LPWSTR): HRESULT; stdcall;
  forward;
function SHGetFolderPath; external 'SHFolder.dll' name 'SHGetFolderPathW';
{$ENDIF}

class function TFolderHelper.GetAppDataFolder: string;
begin
  {$IFDEF FPC}
  {$IFDEF LINUX}
  Result :=GetEnvironmentVariable('HOME');
  {$ELSE}
  Result :=GetEnvironmentVariable('APPDATA');
  {$ENDIF}
  {$ELSE}
  Result := GetFolder(CSIDL_APPDATA); // c:\Users\(User Name)\AppData\Roaming
  {$ENDIF}
end;

{$IFnDEF FPC}
class function TFolderHelper.GetFolder(const aCSIDL: Integer): string;
var
  FolderPath: array[0 .. MAX_PATH] of Char;
begin
  Result := '';
  if SHGetFolderPath(0, aCSIDL, 0, 0, @FolderPath) = S_OK then
    Result := FolderPath;
end;
{$ENDIF}

class function TFolderHelper.GetPascalCoinDataFolder: string;
begin
  Result := GetAppDataFolder+PathDelim+'PascalCoin';
end;

class function TFolderHelper.GetTFileVersionInfo(Const FileName: String): TFileVersionInfo;
{$IFDEF FILEVERSIONINFO}
Var verInfoSize : DWord;
    GetInfoSizeJunk : DWord;
    VersionInfo,
    Translation,
    InfoPointer : Pointer;
    VersionInfoSize: UINT;
    VersionValue :  string;
{$ENDIF}
Begin
   With result do
   Begin
     HasInfo := False;
     CompanyName := '';
     FileDescription := '';
     FileVersion := '';
     InternalName := '';
     LegalCopyRight := '';
     LegalTradeMarks := '';
     OriginalFileName := '';
     ProductName := '';
     ProductVersion := '';
     Comments := '';
     Debug := False;
     Pre_Release := False;
     Patched := False;
     PrivateBuild := False;
     InfoInferred := False;
     SpecialBuild := False;
   End;
   {$IFDEF FILEVERSIONINFO}
   VerInfoSize := GetFileVersionInfoSize(PChar(FileName),GetInfoSizeJunk);
   If verInfoSize>0 Then
     Begin
       Result.HasInfo := True;
       GetMem(VersionInfo,VerInfoSize);
       GetFileVersionInfo(PChar(FileName),0,VerInfoSize,VersionInfo);
       VerQueryValue(VersionInfo,'\\VarFileInfo\\Translation',Translation,VersionInfoSize);
       VersionValue := '\\StringFileInfo\\'+
         inttohex(LoWord(LongInt(Translation^)),4)+
         inttohex(HiWord(LongInt(Translation^)),4)+ '\\';
       If VerQueryValue(VersionInfo,PChar(VersionValue+'CompanyName'),InfoPointer,VersionInfoSize) Then
         Result.CompanyName := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'FileDescription'),InfoPointer,VersionInfoSize) Then
         Result.FileDescription := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'FileVersion'),InfoPointer,VersionInfoSize) Then
         Result.FileVersion := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'InternalName'),InfoPointer,VersionInfoSize) Then
         Result.InternalName := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'LegalCopyright'),InfoPointer,VersionInfoSize) Then
         Result.LegalCopyRight := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'LegalTrademarks'),InfoPointer,VersionInfoSize) Then
         Result.LegalTradeMarks := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'OriginalFilename'),InfoPointer,VersionInfoSize) Then
         Result.OriginalFileName := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'ProductName'),InfoPointer,VersionInfoSize) Then
         Result.ProductName := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'ProductVersion'),InfoPointer,VersionInfoSize) Then
         Result.ProductVersion := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,PChar(VersionValue+'Comments'),InfoPointer,VersionInfoSize) Then
         Result.Comments := String(PChar(InfoPointer));
       If VerQueryValue(VersionInfo,'\',InfoPointer,VersionInfoSize) Then
         Begin
           Result.Debug := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_DEBUG);
           Result.Pre_Release := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_PRERELEASE);
           Result.Patched := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_PATCHED);
           Result.PrivateBuild := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_PRIVATEBUILD);
           Result.InfoInferred := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_INFOINFERRED);
           Result.SpecialBuild := BOOL(TVSFixedFileInfo(InfoPointer^).dwFileFlags AND VS_FF_SPECIALBUILD);
         End;
       FreeMem(VersionInfo,VerInfoSize);
     End;
   {$ENDIF}
end;

end.
