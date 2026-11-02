# GNU gettext backend

PascalCoin uses this existing GNU gettext implementation for Delphi translation.
Add `src/libraries/gnugettext` to the Delphi unit search path. The original
authors, upstream history and redistribution terms remain in
[gnugettext.pas](gnugettext.pas); this directory retains those terms.

The experimental wallet composes it through
[UWalletLanguages.pas](../../gui-experimental/UWalletLanguages.pas):

```pascal
// Catalog: MyLocaleDirectory/nl/LC_MESSAGES/default.mo
TWalletLanguages.Apply('nl', MyLocaleDirectory);
```

The wrapper checks the requested directory using GNU's existing MO reader and
its read-only `StringCount` metadata. Header-only catalogs are excluded. GNU
gettext retains each form's original strings and owns its translation marker;
language changes retranslate from those originals. VCL application events apply
translation to existing forms, future modal forms before showing, and new
modeless or embedded forms during UI idle. Existing application subscribers are
preserved.
