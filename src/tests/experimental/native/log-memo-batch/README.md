# Log memo batch regression

Runs the real experimental log form and logger in a fresh offline profile on a
private Windows desktop. Each delivered batch must publish native memo text once,
retain the existing formatting and 300/250 line rules, preserve multiline text and
debug filtering, and leave direct log callbacks immediate. Empty and fully filtered
batches must not change the native text.

```powershell
.\RunLogMemoBatchChecks.ps1 -OutputDirectory <local-folder>
.\RunLogMemoBatchChecks.ps1 -Classic -OutputDirectory <local-folder>
```

The output directory must be new. The broker never switches the input desktop or
force-terminates the probe. `LogMemoBatchProbe.dpr` runs the same checks in Delphi;
pass a new absolute output-log path as its only argument and use the experimental
wallet's normal unit search paths and OpenSSL runtime.

The Classic probe creates its real form without activating its chain loader. It
sets a fresh data directory and disables networking before constructing the form,
then verifies the Classic log formatting and the same native publication rules.
`ClassicLogMemoBatchProbe.dpr` provides its Delphi entry point.
