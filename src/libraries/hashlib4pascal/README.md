HashLib4Pascal: Hashing for Modern Object Pascal [![License](http://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Xor-el/HashLib4Pascal/blob/master/LICENSE)
========================================

``HashLib4Pascal`` is an Object Pascal hashing library released under the permissive [MIT License](https://github.com/Xor-el/HashLib4Pascal/blob/master/LICENSE) which provides an easy to use interface for computing hashes and checksums of data. It also supports state based (incremental) hashing.

``HashLib4Pascal's`` goal is to be the best option for hashing in Object Pascal by offering various hashing primitives via an easy to use API to Object Pascal developers.

Development is coordinated on [GitHub](https://github.com/Xor-el/HashLib4Pascal) and contributions are welcome. If you need help, please open an issue [here](https://github.com/Xor-el/HashLib4Pascal/issues).


**Build Status**
[![Build Status](https://travis-ci.org/Xor-el/HashLib4Pascal.svg?branch=master)](https://travis-ci.org/Xor-el/HashLib4Pascal)

Available Algorithms
----------------------------------------

 ### Hashes
----------------------------------------
##### Cyclic Redundancy Checks

* `All CRC Variants from CRC3 to CRC64` 

##### Checksums

* `Adler32`

##### Non-Cryptographic Hash Functions 
----------------------------------------

###### 32 bit hashes

* `AP` `BKDR` `Bernstein` `Bernstein1` `DEK` `DJB` `ELF` `FNV` 

* `FNV1a` `JS` `Jenkins3` `Murmur2` `MurmurHash3_x86_32` `OneAtTime`

*  `PJW` `RS` `Rotating` `SDBM` `ShiftAndXor` `SuperFast` `XXHash32`

###### 64 bit hashes

* `FNV64` `FNV1a64` `Murmur2_64` `SipHash2_4` `XXHash64`

###### 128 bit hashes

* `MurmurHash3_x86_128` `MurmurHash3_x64_128` 

##### Cryptographic Hash Functions 
----------------------------------------

 * `MD2`

 * `MD4`

 * `MD5`

 * `SHA-0`

 * `SHA-1`

 * `SHA-2 (224, 256, 384, 512, 512-224, 512-256)`

 * `GOST 34.11-94`

 * `GOST R 34.11-2012 (AKA Streebog) (256, 512)`
 
 * `Grindahl (256, 512)`
 
 * `Has160`

 * `RIPEMD (128, 256, 256, 320)`

 * `Tiger (128, 160, 192 (Rounds 3, 4, 5))` 

 * `Tiger2 (128, 160, 192 (Rounds 3, 4, 5))` 
 
 * `Snefru (128, 256)`
 
 * `Haval (128, 160, 192, 224, 256 (Rounds 3, 4, 5))`
 
 * `Panama`
 
 * `RadioGatun (RadioGatun32, RadioGatun64)`

 * `WhirlPool`

 * `Blake2B (160, 256, 384, 512)`
 
 * `Blake2S (128, 160, 224, 256)`

 * `SHA-3 (224, 256, 384, 512)`
 
 * `Keccak (224, 256, 288, 384, 512)`

### Key Derivation Functions
----------------------------------------

###### Password Hashing Schemes (Password Based Key Derivation Functions)

----------------------------------------

* `PBKDF2`
 
* `Argon2 (2i, 2d and 2id variants)`

* `Scrypt`

### MAC
----------------------------------------

* `HMAC (all supported hashes)`

### XOF (Extendable Output Function)
----------------------------------------

* `Shake (Shake-128, Shake-256)`

### Supported Compilers
----------------------------------------

* `FreePascal 3.0.0+`

* `Delphi 2010+`

### Other Implementations
----------------------------------------

If you want implementations in other languages, you can check out these

* [HashLib4CPP](https://github.com/ron4fun/HashLib4CPP) by Mbadiwe Nnaemeka Ronald

### Tip Jar
----------------------------------------

* :dollar: **Bitcoin**: `1MhFfW7tDuEHQSgie65uJcAfJgCNchGeKf`
* :euro: **Ethereum**: `0x6c1DC21aeC49A822A4f1E3bf07c623C2C1978a98`
* :pound: **Pascalcoin**: `345367-40`
