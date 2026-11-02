# Full-range Cardinal ordering

`OrderedCardinalProbe` runs the same three groups registered as
`TOrderedCardinalTests` in the experimental FPCUnit suite. It exercises the actual
AbstractMem `TComparison_Cardinal` and `TOrderedList<Cardinal>` used by affected
account history: all 25 pairs of `0`, `1`, `MaxInt`, `MaxInt+1`, `High(Cardinal)`;
all 120 insertion/find/removal permutations; and duplicates/missing values.
Assertions require comparison sign and ordering, without prescribing magnitude.

The Delphi project enables range/overflow checking. Build the FPC entry point
with Delphi mode, `-Cr -Co`, a separate unit output directory and the AbstractMem
unit path. Both entry points accept one new absolute result-log filename. The
FPC program also writes a heap trace beside it. No node, wallet, sockets or chain
data are created by this focused probe.

This AbstractMem unit is not a PascalFramework component and has no matching
standalone framework copy. Other integer/pointer comparators are outside this
regression's scope.
