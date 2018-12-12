About paszlib units

On October 2018 a bug on decompress ZLIB functions was found on PascalCoin core
After some tests, the bug was related to a bug on FreePascal paszlib package (Latest release version was FreePascal 3.0.4)

After notified at FreePascal bugtracker ( https://bugs.freepascal.org/view.php?id=34422 ) the solution was to wait until next FreePascal release

In order to solve, the solution applyed for PascalCoin core was to download fixed FreePascal paszlib package and include in PascalCoin core while using FreePascal 3.0.4 compiler

In future, once FreePascal releases a new FreePascal version with bug fixed, those manually added files will not be needed

Added on PascalCoin core on 2018-12-12  (Available for PascalCoin version 4.0.2)
