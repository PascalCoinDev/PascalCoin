# HashLib4Pascal [![License](http://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Xor-el/HashLib4Pascal/blob/master/LICENSE)
HashLib4Pascal is a Delphi/FreePascal compatible library that provides an easy to use interface for computing hashes and checksums of strings (with a specified encoding), files, streams, byte arrays and untyped data to mention but a few.
It also supports Incremental Hashing.

**Build Status**
[![Build Status](https://travis-ci.org/Xor-el/HashLib4Pascal.svg?branch=master)](https://travis-ci.org/Xor-el/HashLib4Pascal)

**Supported Algorithms:**

    non-cryptographic 32-bits hash algorithms: AP, BKDR, Bernstein, Bernstein1, DEK, DJB, 
    ELF, FNV, FNV1a, JS, Jenkins3, Murmur2, MurmurHash3_x86_32, OneAtTime, PJW, RS, 
    Rotating, SDBM, ShiftAndXor, SuperFast, XXHash32.

    non-cryptographic 64-bits algorithms: FNV, FNV1a, Murmur2_64, SipHash2_4, XXHash64.

    non-cryptographic 128-bits algorithms: MurmurHash3_x86_128, MurmurHash3_x64_128. 

    checksum algorithms: Adler32, All CRC Variants from CRC3 to CRC64. 

    cryptographic algorithms: GOST, Grindahl, HAS160, Haval, MD2, MD4, MD5, Panama, 
    RadioGatun, RIPEMD, RIPEMD128, RIPEMD160, RIPEMD256, RIPEMD320, SHA0, SHA1, SHA2-224,
    SHA2-256, SHA2-384, SHA2-512, SHA2-512-224, SHA2-512-256, SHA3-224, SHA3-256, SHA3-384, 
    SHA3-512, Keccak-224, Keccak-256, Keccak-384, Keccak-512, Snefru128, Snefru256, Tiger, Tiger2, 
    WhirlPool, Blake2B, Blake2S, Streebog (GOST3411_2012_256, GOST3411_2012_512), Shake_128, Shake_256.

    HMAC for any of the above.
    
    PBKDF2_HMAC for any of the above.
    
    PBKDF_Argon2 (2i, 2d and 2id variants).

**Supported Compilers**
 
    FreePascal 3.0.0 and Above.
    
    Delphi 2010 and Above.

## Installing the Library. ##

**Method One:**

 - Use the Provided Packages in the "Packages" Folder.

**Method Two:**

- Add the Library Path and Sub Path to your Project Search Path.

**Usage Examples.**

- Coming Soon.
- But in the mean time, you can poke around the sources and Unit Tests.

 ## Unit Tests. ##

To Run Unit Tests,

**For FPC 3.0.0 and above**


    Simply compile and run "HashLib.Tests" project in "FreePascal.Tests" Folder.

**For Delphi 2010 and above**

   **Method One (Using DUnit Test Runner)**

     To Build and Run the Unit Tests For Delphi 10 Seattle (should be similar for 
     other versions)
    
    1. Open Project Options of Unit Test (HashLib.Tests) in "Delphi.Tests" Folder.
    
    2. Change Target to All Configurations (Or "Base" In Older Delphi Versions.)
    
    3. In Output directory add ".\$(Platform)\$(Config)" without the quotes.
    
    4. In Search path add "$(BDS)\Source\DUnit\src" without the quotes.
    
    5. In Unit output directory add "." without the quotes.
    
    6. In Unit scope names (If Available), Delete "DUnitX" from the List.
    
    Press Ok and save, then build and run.
    
 **Method Two (Using TestInsight) (Preferred).**

    1. Download and Install TestInsight.
    
    2. Open Project Options of Unit Test (HashLib.Tests.TestInsight) in "Delphi.Tests" 
        Folder. 

    3. Change Target to All Configurations (Or "Base" In Older Delphi Versions.)

    4. In Unit scope names (If Available), Delete "DUnitX" from the List.

    5. To Use TestInsight, right-click on the project, then select 
    
  ###### "Enable for TestInsight" or "TestInsight Project". ######
        Save Project then Build and Run Test Project through TestInsight. 
        
  **Other Implementations**

If you want implementations in other languages, you can check out these

[HashLib4CPP By Mbadiwe Nnaemeka Ronald](https://github.com/ron4fun/HashLib4CPP)

**License**

This "Software" is Licensed Under  **`MIT License (MIT)`** .

#### Tip Jar
* :dollar: **Bitcoin**: `1MhFfW7tDuEHQSgie65uJcAfJgCNchGeKf`
* :euro: **Ethereum**: `0x6c1DC21aeC49A822A4f1E3bf07c623C2C1978a98`
* :pound: **Pascalcoin**: `345367-40`
