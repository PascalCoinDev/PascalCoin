unit UWalletLanguages;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses SysUtils, UCommon, UAppParams;

type
  TWalletLanguage = record
    Code, DisplayName: String;
  end;

  { Product composition over the installed gettext implementation. Catalogs keep
    the classic locale/<code>/LC_MESSAGES/default.po/.mo layout. Apply at startup;
    preferences changed in the UI take effect on the next start. }

  TWalletLanguages = class
  public
    class function LocaleDirectory: String; static;
    class function Available(const ALocaleDirectory: String = ''): TArray<TWalletLanguage>; static;
    class function ReadPreference(AParams: TAppParams = nil): String; static;
    class procedure WritePreference(const ACode: String; AParams: TAppParams = nil); static;
    class function Apply(const ALanguage: String; const ALocaleDirectory: String = ''): String; static;
  end;

implementation

{$I ..\config.inc}

uses
  Classes, UMemory, USettings,
  {$IFDEF FPC}LCLTranslator, LResources, Forms, Translations, GetText
  {$ELSE}Vcl.Forms, Vcl.AppEvnts, gnugettext{$ENDIF};

{$IFDEF FPC}
type
  TCatalogFile = class(TMOFile)
    function HasTranslations: Boolean;
  end;

function TCatalogFile.HasTranslations: Boolean;
begin
  // msgfmt omits untranslated entries; the header alone is not a translation.
  Result := StringCount > 1;
end;
{$ENDIF}

{$IFNDEF FPC}
type
  { The VCL event dispatcher preserves other application subscribers. GNU
    gettext owns its original-text marker on each translated component, so no
    registry of form pointers can outlive a destroyed form. }
  TWalletFormTranslator = class(TComponent)
  private
    FEvents: TApplicationEvents;
    FApplying: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure OnActivateOrModal(Sender: TObject);
    procedure TranslateComponent(AComponent: TComponent; ARetranslate: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure TranslateForms(ARetranslate: Boolean);
  end;

var
  VclFormTranslator: TWalletFormTranslator;

constructor TWalletFormTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents := TApplicationEvents.Create(Self);
  FEvents.OnIdle := OnIdle;
  FEvents.OnActivate := OnActivateOrModal;
  FEvents.OnModalBegin := OnActivateOrModal;
end;

procedure TWalletFormTranslator.TranslateComponent(AComponent: TComponent;
  ARetranslate: Boolean);
begin
  if (csLoading in AComponent.ComponentState) or
    (csDestroying in AComponent.ComponentState) then Exit;
  if not Assigned(AComponent.FindComponent('GNUgettextMarker')) then
    gnugettext.TranslateComponent(AComponent)
  else if ARetranslate then
    gnugettext.RetranslateComponent(AComponent);
end;

procedure TWalletFormTranslator.TranslateForms(ARetranslate: Boolean);
var
  i: Integer;
begin
  if FApplying then Exit;
  FApplying := True;
  try
    for i := Screen.CustomFormCount - 1 downto 0 do
      if i < Screen.CustomFormCount then TranslateComponent(Screen.CustomForms[i], ARetranslate);
    for i := Screen.DataModuleCount - 1 downto 0 do
      if i < Screen.DataModuleCount then TranslateComponent(Screen.DataModules[i], ARetranslate);
  finally
    FApplying := False;
  end;
end;

procedure TWalletFormTranslator.OnIdle(Sender: TObject; var Done: Boolean);
begin
  // New modeless/embedded forms need the same translation as startup forms.
  // Leave Done unchanged so this subscriber never turns idle into a busy loop.
  TranslateForms(False);
end;

procedure TWalletFormTranslator.OnActivateOrModal(Sender: TObject);
begin
  // Modal forms have finished construction here, before their first showing.
  TranslateForms(False);
end;
{$ENDIF}

function IsLanguageCode(const ACode: String): Boolean;
var
  i: Integer;
begin
  Result := ACode <> '';
  for i := 1 to Length(ACode) do
    if not (ACode[i] in ['a'..'z', 'A'..'Z', '_', '-']) then exit(false);
end;

function LanguageName(const ACode: String): String;
begin
  {$IFNDEF FPC}
  Result := CodeToNativeLanguage(ACode);
  {$ELSE}
  // Native labels for the supplied catalogs; additional languages retain their code.
  if ACode = 'en' then Result := 'English'
  else if ACode = 'nl' then Result := 'Nederlands'
  else if ACode = 'de' then Result := 'Deutsch'
  else if ACode = 'fr' then Result := 'Français'
  else Result := ACode;
  {$ENDIF}
end;

function HasCatalogTranslations(const ADirectory, ACode: String): Boolean;
var
  disposed: TDisposables;
  path: String;
  {$IFDEF FPC}
  po: TPOFile;
  mo: TCatalogFile;
  {$ELSE}
  catalog: TFileStream;
  mo: gnugettext.TMoFile;
  {$ENDIF}
begin
  Result := false;
  path := IncludeTrailingPathDelimiter(ADirectory) + ACode + PathDelim +
    'LC_MESSAGES' + PathDelim + 'default';
  try
    {$IFDEF FPC}
    if FileExists(path + '.po') then begin
      po := disposed.AddObject(TPOFile.Create(path + '.po')) as TPOFile;
      Result := po.Statistics.Translated > 0;
    end else if FileExists(path + '.mo') then begin
      mo := disposed.AddObject(TCatalogFile.Create(path + '.mo')) as TCatalogFile;
      Result := mo.HasTranslations;
    end;
    {$ELSE}
    if FileExists(path + '.mo') then begin
      catalog := disposed.AddObject(TFileStream.Create(path + '.mo',
        fmOpenRead or fmShareDenyNone)) as TFileStream;
      if catalog.Size < 28 then Exit;
      // Use the existing parser with an independent in-memory copy. A listing
      // must inspect this exact directory without changing the active domain,
      // retaining file mappings or advertising header-only catalogs.
      mo := disposed.AddObject(gnugettext.TMoFile.Create(path + '.mo', 0, 0,
        False, '')) as gnugettext.TMoFile;
      Result := mo.StringCount > 1;
    end;
    {$ENDIF}
  except
    // A damaged optional catalog must not prevent opening the wallet or options.
    on E: Exception do Result := false;
  end;
end;

class function TWalletLanguages.LocaleDirectory: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'locale';
end;

class function TWalletLanguages.Available(const ALocaleDirectory: String): TArray<TWalletLanguage>;
var
  directory: String;
  search: TSearchRec;
  language: TWalletLanguage;
begin
  Result := nil;
  language.Code := 'en';
  language.DisplayName := LanguageName(language.Code);
  TArrayTool<TWalletLanguage>.Add(Result, language);
  directory := ALocaleDirectory;
  if directory = '' then directory := LocaleDirectory;
  if FindFirst(IncludeTrailingPathDelimiter(directory) + '*', faDirectory, search) <> 0 then exit;
  try
    repeat
      if (search.Attr and faDirectory <> 0) and IsLanguageCode(search.Name) and
        (not SameText(search.Name, 'en')) and HasCatalogTranslations(directory, search.Name) then begin
        language.Code := search.Name;
        language.DisplayName := LanguageName(language.Code);
        TArrayTool<TWalletLanguage>.Add(Result, language);
      end;
    until FindNext(search) <> 0;
  finally
    FindClose(search);
  end;
end;

class function TWalletLanguages.ReadPreference(AParams: TAppParams): String;
begin
  if not Assigned(AParams) then AParams := TSettings.AppParams;
  Result := AParams.ParamByName[CT_PARAM_UILanguage].GetAsString('en');
end;

class procedure TWalletLanguages.WritePreference(const ACode: String; AParams: TAppParams);
begin
  if not IsLanguageCode(ACode) then
    raise EArgumentException.Create('Invalid interface language code');
  if not Assigned(AParams) then AParams := TSettings.AppParams;
  AParams.ParamByName[CT_PARAM_UILanguage].SetAsString(ACode);
end;

class function TWalletLanguages.Apply(const ALanguage: String; const ALocaleDirectory: String): String;
var
  directory, catalog: String;
  {$IFDEF FPC}
  translator: TUpdateTranslator;
  disposed: TDisposables;
  i: Integer;
  {$ENDIF}
begin
  directory := ALocaleDirectory;
  if directory = '' then directory := LocaleDirectory;
  Result := 'en';
  if IsLanguageCode(ALanguage) and ((ALanguage = 'en') or
    HasCatalogTranslations(directory, ALanguage)) then Result := ALanguage;
  {$IFDEF FPC}
  // Use the LCL's existing translator with the exact GNU catalog path. Its
  // default search does not resolve a custom directory's LC_MESSAGES layout.
  catalog := IncludeTrailingPathDelimiter(directory) + Result + PathDelim +
    'LC_MESSAGES' + PathDelim + 'default';
  translator := nil;
  if FileExists(catalog + '.po') then begin
    translator := disposed.AddObject(TPOTranslator.Create(catalog + '.po')) as TUpdateTranslator;
    Translations.TranslateResourceStrings(catalog + '.po');
  end else if FileExists(catalog + '.mo') then begin
    translator := disposed.AddObject(TDefaultTranslator.Create(catalog + '.mo')) as TUpdateTranslator;
    GetText.TranslateResourceStrings(catalog + '.mo');
  end;
  FreeAndNil(LRSTranslator);
  LRSTranslator := translator;
  disposed.RemoveObject(translator);
  if Assigned(translator) then begin
    for i := 0 to Screen.CustomFormCount - 1 do
      translator.UpdateTranslation(Screen.CustomForms[i]);
    for i := 0 to Screen.DataModuleCount - 1 do
      translator.UpdateTranslation(Screen.DataModules[i]);
  end;
  {$ELSE}
  bindtextdomain('default', directory);
  textdomain('default');
  UseLanguage(Result);
  if not Assigned(VclFormTranslator) then VclFormTranslator := TWalletFormTranslator.Create(nil);
  VclFormTranslator.TranslateForms(True);
  {$ENDIF}
end;

{$IFNDEF FPC}
initialization
  VclFormTranslator := nil;

finalization
  FreeAndNil(VclFormTranslator);
{$ENDIF}

end.
