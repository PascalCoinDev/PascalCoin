# Log memo batch regression

Checks that the experimental log form shows each batch in one memo update. It
keeps formatting, the 300-line limit, multiline text, debug filtering and
immediate direct callbacks. Empty and fully filtered batches change nothing.
`-Classic` checks the classic form.

```powershell
.\RunLogMemoBatchChecks.ps1 -OutputDirectory <new-folder>
.\RunLogMemoBatchChecks.ps1 -Classic -OutputDirectory <another-new-folder>
```

The output directory must be new. The probe runs offline with new data, and the
script gives it a private desktop.

For Delphi, build `LogMemoBatchProbe.dpr` or `ClassicLogMemoBatchProbe.dpr` with
the matching wallet's search paths. Put the OpenSSL runtime beside it and pass
only a new output-log path.
