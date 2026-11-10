# Full-range Cardinal ordering

`OrderedCardinalProbe` checks ordering in AbstractMem's `TComparison_Cardinal`
and `TOrderedList<Cardinal>`. It runs the same groups as the FPCUnit
`TOrderedCardinalTests`:

- 25 boundary pairs
- 120 insertion, find and removal permutations
- duplicate and missing values

The [Delphi project](OrderedCardinalProbe.dproj) enables range and overflow
checks. For FPC, build in Delphi mode with `-Cr -Co`, a separate unit output
folder and the AbstractMem unit path.

Both programs take one new absolute log filename. FPC builds write a heap trace
beside it. The probe creates no node, wallet, sockets or chain data.
