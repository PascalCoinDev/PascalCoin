unit UWalletLanguageTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

interface

uses Classes, SysUtils, Forms, fpcunit, testregistry, UWalletLanguages;

type
  TLanguageTestForm = class(TForm)
  end;

  TWalletLanguageTests = class(TTestCase)
  private
    FDirectory: String;
    procedure WriteCatalog(const ACode, ATranslation: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CatalogCanBeParsedByNativeTranslator;
    procedure ListsOnlyUsableCatalogsAndEnglish;
    procedure MissingCatalogFallsBackWithoutChangingPreference;
    procedure PreferenceRoundTripsThroughExistingAppParams;
    procedure NativeTranslatorHandlesExistingAndFutureForms;
    procedure RejectsInvalidLanguagePreference;
  end;

implementation

uses UMemory, UAppParams, LResources, Translations;

{$R LanguageTestForm.lfm}

procedure TWalletLanguageTests.SetUp;
var
  id: TGUID;
begin
  CreateGUID(id);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'PascalCoinLanguageTest-' + GUIDToString(id);
  ForceDirectories(FDirectory);
end;

procedure TWalletLanguageTests.TearDown;
const
  codes: array[0..2] of String = ('nl', 'de', 'fr');
var
  code, path: String;
begin
  // The native LCL owns this translator; detach the fixture before deleting files.
  FreeAndNil(LRSTranslator);
  for code in codes do begin
    path := FDirectory + PathDelim + code + PathDelim + 'LC_MESSAGES';
    DeleteFile(path + PathDelim + 'default.po');
    RemoveDir(path);
    RemoveDir(FDirectory + PathDelim + code);
  end;
  DeleteFile(FDirectory + PathDelim + 'AppParams.prm');
  RemoveDir(FDirectory);
end;

procedure TWalletLanguageTests.WriteCatalog(const ACode, ATranslation: String);
var
  disposed: TDisposables;
  text: TStringList;
  path: String;
begin
  path := FDirectory + PathDelim + ACode + PathDelim + 'LC_MESSAGES';
  ForceDirectories(path);
  text := disposed.AddObject(TStringList.Create) as TStringList;
  text.Add('msgid ""');
  text.Add('msgstr ""');
  text.Add('"Content-Type: text/plain; charset=UTF-8\n"');
  text.Add('');
  text.Add('#: LanguageTestForm.Caption');
  text.Add('msgid "Options"');
  text.Add('msgstr "' + ATranslation + '"');
  text.Add('');
  text.SaveToFile(path + PathDelim + 'default.po');
end;

procedure TWalletLanguageTests.ListsOnlyUsableCatalogsAndEnglish;
var
  languages: TArray<TWalletLanguage>;
begin
  WriteCatalog('nl', 'Instellingen');
  WriteCatalog('de', '');
  WriteCatalog('fr', '');
  languages := TWalletLanguages.Available(FDirectory);
  AssertEquals(2, Length(languages));
  AssertEquals('en', languages[0].Code);
  AssertEquals('nl', languages[1].Code);
  AssertEquals('Nederlands', languages[1].DisplayName);
end;

procedure TWalletLanguageTests.CatalogCanBeParsedByNativeTranslator;
var
  po: TPOFile;
  disposed: TDisposables;
begin
  WriteCatalog('nl', 'Instellingen');
  po := disposed.AddObject(TPOFile.Create(FDirectory + PathDelim +
    'nl' + PathDelim + 'LC_MESSAGES' + PathDelim + 'default.po')) as TPOFile;
  AssertEquals('Instellingen', po.Translate('', 'Options'));
  AssertEquals(1, po.Statistics.Translated);
end;

procedure TWalletLanguageTests.MissingCatalogFallsBackWithoutChangingPreference;
var
  params: TAppParams;
  disposed: TDisposables;
begin
  params := disposed.AddObject(TAppParams.Create(nil)) as TAppParams;
  params.FileName := FDirectory + PathDelim + 'AppParams.prm';
  TWalletLanguages.WritePreference('fr', params);
  AssertEquals('en', TWalletLanguages.Apply(TWalletLanguages.ReadPreference(params), FDirectory));
  AssertEquals('fr', TWalletLanguages.ReadPreference(params));
end;

procedure TWalletLanguageTests.PreferenceRoundTripsThroughExistingAppParams;
var
  params: TAppParams;
  disposed: TDisposables;
begin
  params := disposed.AddObject(TAppParams.Create(nil)) as TAppParams;
  params.FileName := FDirectory + PathDelim + 'AppParams.prm';
  TWalletLanguages.WritePreference('nl', params);
  disposed.RemoveObject(params);
  params.Free;
  params := disposed.AddObject(TAppParams.Create(nil)) as TAppParams;
  params.FileName := FDirectory + PathDelim + 'AppParams.prm';
  AssertEquals('nl', TWalletLanguages.ReadPreference(params));
end;

procedure TWalletLanguageTests.NativeTranslatorHandlesExistingAndFutureForms;
var
  before, after: TLanguageTestForm;
  disposed: TDisposables;
begin
  WriteCatalog('nl', 'Instellingen');
  before := disposed.AddObject(TLanguageTestForm.Create(nil)) as TLanguageTestForm;
  AssertEquals('Options', before.Caption);
  AssertEquals('nl', TWalletLanguages.Apply('nl', FDirectory));
  AssertEquals('Instellingen', before.Caption);
  after := disposed.AddObject(TLanguageTestForm.Create(nil)) as TLanguageTestForm;
  AssertEquals('Instellingen', after.Caption);
end;

procedure TWalletLanguageTests.RejectsInvalidLanguagePreference;
var
  params: TAppParams;
  disposed: TDisposables;
  rejected: Boolean;
begin
  params := disposed.AddObject(TAppParams.Create(nil)) as TAppParams;
  params.FileName := FDirectory + PathDelim + 'AppParams.prm';
  TWalletLanguages.WritePreference('en', params);
  rejected := false;
  try
    TWalletLanguages.WritePreference('..\outside', params);
  except
    on E: EArgumentException do rejected := true;
  end;
  AssertTrue(rejected);
  AssertEquals('en', TWalletLanguages.ReadPreference(params));
end;

initialization
  RegisterTest(TWalletLanguageTests);
end.
