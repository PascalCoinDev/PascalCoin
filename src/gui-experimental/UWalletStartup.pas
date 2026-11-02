unit UWalletStartup;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Parses startup options for data location, offline mode and startup dialogs
  without opening wallet data or sockets. }

{ Public types in this unit

  Type                  | Purpose
  ----------------------+-------------------------------------------------------------------------
  TWalletStartupOptions | Carries parsed data-directory, offline, dialog and help options.
}

interface

uses
  SysUtils;

type

  { Options are parsed without opening settings, keys, blockchain files or sockets. }

  TWalletStartupOptions = record
    DataDirectory : String;
    Offline : Boolean;
    NoStartupDialogs : Boolean;
    ShowHelp : Boolean;
  end;

function TryParseWalletStartupOptions(const AArguments : array of String;
  out AOptions : TWalletStartupOptions; out AError : String) : Boolean;
function WalletStartupHelp : String;

implementation

function TryParseWalletStartupOptions(const AArguments : array of String;
  out AOptions : TWalletStartupOptions; out AError : String) : Boolean;
const
  CT_DataDirectoryPrefix = '--data-dir=';
var
  i : Integer;
  LArgument : String;
  LHasDataDirectory : Boolean;
begin
  AOptions.DataDirectory := '';
  AOptions.Offline := false;
  AOptions.NoStartupDialogs := false;
  AOptions.ShowHelp := false;
  AError := '';
  LHasDataDirectory := false;
  Result := false;
  for i := Low(AArguments) to High(AArguments) do begin
    LArgument := AArguments[i];
    if LArgument = '--offline' then
      AOptions.Offline := true
    else if LArgument = '--no-startup-dialogs' then
      AOptions.NoStartupDialogs := true
    else if (LArgument = '--help') or (LArgument = '-h') then
      AOptions.ShowHelp := true
    else if Copy(LArgument, 1, Length(CT_DataDirectoryPrefix)) = CT_DataDirectoryPrefix then begin
      if LHasDataDirectory then begin
        AError := 'Specify --data-dir only once.';
        Exit;
      end;
      AOptions.DataDirectory := Copy(LArgument, Length(CT_DataDirectoryPrefix) + 1, MaxInt);
      if Trim(AOptions.DataDirectory) = '' then begin
        AError := '--data-dir requires a non-empty directory.';
        Exit;
      end;
      AOptions.DataDirectory := ExpandFileName(AOptions.DataDirectory);
      LHasDataDirectory := true;
    end else begin
      AError := 'Unknown argument: ' + LArgument;
      Exit;
    end;
  end;
  Result := true;
end;

function WalletStartupHelp : String;
begin
  Result := 'PascalCoin experimental wallet' + sLineBreak +
    '  --data-dir=<directory>  Use this directory for all wallet and node data.' + sLineBreak +
    '  --offline               Disable peer discovery, connections and all servers.' + sLineBreak +
    '  --no-startup-dialogs    Suppress startup error dialogs.' + sLineBreak +
    '  --help, -h              Show this help without opening wallet data.';
end;

end.
