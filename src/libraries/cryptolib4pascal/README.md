CryptoLib4Pascal: Crypto for Modern Object Pascal [![License](http://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Xor-el/CryptoLib4Pascal/blob/master/LICENSE)
========================================

``CryptoLib4Pascal`` is an Object Pascal cryptographic library released under the permissive [MIT License](https://github.com/Xor-el/CryptoLib4Pascal/blob/master/LICENSE). 

``CryptoLib4Pascal's`` goal is to be the best option for cryptography in Object Pascal by offering cryptographic recipes and primitives to Object Pascal developers.

Development is coordinated on [GitHub](https://github.com/Xor-el/CryptoLib4Pascal) and contributions are welcome. If you need help, please open an issue [here](https://github.com/Xor-el/CryptoLib4Pascal/issues).


<!--**Build Status**
[![Build Status](https://travis-ci.org/Xor-el/CryptoLib4Pascal.svg?branch=master)](https://travis-ci.org/Xor-el/CryptoLib4Pascal)-->

Available Algorithms
----------------------------------------

 ### Symmetric Encryption
----------------------------------------
###### Block Ciphers
* `AES (128, 192, and 256)` 

* `Rijndael` 

* `Blowfish`

* `Speck`

###### Stream Ciphers
* `ChaCha`

* `(X)Salsa20` 

##### Block Cipher Modes Of Operation 
----------------------------------------

* `ECB` 

* `CBC` 

* `CFB` 

* `CTR` 

* `CTS` 

* `OFB` 

* `SIC`

##### Block Cipher Padding Schemes 
----------------------------------------

* `ISO 10126-2` 

* `ISO 7816-4` 

* `Bit (ISO/IEC 9797-1)` 

* `PKCS#5` 

* `PKCS#7`
 
* `TBC (Trailing Bit Complement)` 

* `ANSI X9.23` 

* `Zero`

### Asymmetric Cryptography
----------------------------------------

* `DSA`

* `(DET)ECDSA (supported curves: NIST, X9.62, SEC2, Brainpool)`

* `ECNR`

* `ECSchnorr`
 
* `EdDSA (Ed25519, Ed25519Blake2B)`

### Key Agreement/Exchange
----------------------------------------

* `DH`

* `ECDH`

* `ECDHC`
 
* `X25519` 

### Key Derivation Functions
----------------------------------------

* `HKDF` 
 
* `KDF1`

* `KDF2`

###### Password Hashing Schemes (Password Based Key Derivation Functions)
----------------------------------------

* `PBKDF2`
 
* `Argon2 (2i, 2d and 2id variants)`

* `Scrypt`

### MAC
----------------------------------------

* `HMAC (all supported hashes)`
 
* `X25519` 

### Hashes
----------------------------------------

 * `MD2`

 * `MD4`

 * `MD5`

 * `SHA-1`

 * `SHA-2 (224, 256, 384, 512, 512-224, 512-256)`

 * `Gost3411`

 * `Gost3411-2012 (256, 512)`

 * `RIPEMD (128, 256, 256, 320)`

 * `Tiger`

 * `WhirlPool`

 * `Blake2B (160, 256, 384, 512)`
 
 * `Blake2S (128, 160, 224, 256)`

 * `SHA-3 (224, 256, 384, 512)`
 
 * `Keccak (224, 256, 288, 384, 512)`

### XOF (Extendable Output Function)
----------------------------------------

* `Shake (Shake-128, Shake-256)`

### Other Useful Things
----------------------------------------

* `RNG wrappers for system RNG`

* `ASN1 Parsing Utilities`

* `Base Encoding and Decoding Utilities`

### Compile-Time Dependencies
----------------------------------------

* [HashLib4Pascal](https://github.com/Xor-el/HashLib4Pascal)
* [SimpleBaseLib4Pascal](https://github.com/Xor-el/SimpleBaseLib4Pascal)

### Supported Compilers
----------------------------------------

* `FreePascal 3.2.0+`

* `Delphi Tokyo+`

### Supported OSes
----------------------------------------

* `Windows XP+`

* `Linux (Including Android and Raspberry PI)`

* `macOS 10.7+`

* `iOS 2.0+`

* `FreeBSD, NetBSD, OpenBSD and DragonBSD`

### Acknowledgements
----------------------------------------

* Thanks to [Sphere 10 Software](http://www.sphere10.com/) for sponsoring the development of this library.

### Tip Jar
----------------------------------------

* :dollar: **Bitcoin**: `1MhFfW7tDuEHQSgie65uJcAfJgCNchGeKf`
* :euro: **Ethereum**: `0x6c1DC21aeC49A822A4f1E3bf07c623C2C1978a98`
* :pound: **Pascalcoin**: `345367-40`
