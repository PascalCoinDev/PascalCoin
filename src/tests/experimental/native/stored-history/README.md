# Generated stored-history checks

The shared [`UStoredHistoryChecks`](UStoredHistoryChecks.pas) fixture runs through
FPCUnit and the native Delphi/FPC probe. It builds a generated safebox, validates
signed operations and commits the real `TPCSafeBoxTransaction`. Its storage
override records every requested block, so the tests verify the search boundary
and early exit as well as the returned rows.

The fifteen groups cover signed 64-bit balances, reverse traversal, finite and
Maximum history through the actual data source, tip-only queries, an account last
updated outside the range, finite-page skip/take, empty queries, pending rows and
the live pending index. Linked blocks at 160, 161 and 162 distinguish an inclusive
boundary from an unnecessary older-block read. The hash regression compares each
returned operation with the core hash generated from its actual stored block
height, including legacy hashes where applicable.

The layered-storage regressions write signed blocks through the actual legacy
file storage and AbstractMem index. They cover history older than the index's
first block, a missing indexed block still present in the legacy archive, and
a block absent from both stores. Maximum and finite-depth queries must publish
the available operations without substituting or duplicating another block.

[`ULivePendingHistoryChecks`](ULivePendingHistoryChecks.pas) admits signed
transactions, account-information, data and multioperations through
`TNode.AddOperations`. It checks the live pending account index and counts actual
`AffectedAccounts` calls: unrelated selections must avoid traversal; affected
selections retain the generic helper's rows, order, duplicates and paging.
These are history-query tests, not proof-of-work or archive-validation tests.

Run with one new absolute log path and an explicitly owned `TEMP` directory.
The fixtures select fresh node directories, use generated keys, open no peers
or wallet, and free borrowed transactions before their nodes/safeboxes. The
Delphi project and checked FPC runner must use current product/core/library
sources; the native probe uses the same implementation as the public unit tests.
