# CryptoLib4Pascal [![License](http://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Xor-el/CryptoLib4Pascal/blob/master/LICENSE)
CryptoLib4Pascal is a Cryptographic Package for Delphi/FreePascal Compilers that provides at the moment support for creating, signing and verifying DSA, ECDSA, ECNR and ECSchnorr signatures using various curves and hashes, AES, Blowfish Encryption and Decryption (With various modes and paddings) and ECIES.

**Supported Elliptic Curves:**

Curves are grouped in three different classes.
1. NistNamed Curves
2. SecNamed Curves (these are bare generic implementation of curves (at the moment, not as optimized as their CustomNamed Curves Counterparts))
3. CustomNamed Curves. (these are specially optimized implementations of curves found in the previous two.)

**NistNamed Curves:**
```
K-283, B-283, B-409, B-233, K-163, P-521, K-233, K-409, P-192, P-384, P-224, P-256, B-163, B-571, K-571.
```

**SecNamed Curves:**
```
sect131r1, secp160k1, sect193r1, secp384r1, secp128r1, sect163k1, sect193r2, sect239k1, sect409r1, secp256r1,
secp521r1, sect131r2, secp112r1, sect113r2, sect163r1, secp224k1, secp224r1, sect283k1, sect233k1, sect283r1,
sect409k1, sect571k1, secp192r1, sect233r1, secp256k1, sect571r1, secp192k1, secp160r1, sect113r1, secp160r2,
secp112r2, sect163r2, secp128r2.
```

**CustomNamed Curves:**

| curve name | alias(es) |
|--------|--------|
|  **secp256k1**      |        |
|  **secp256r1**      |   **P-256**    |
|  **secp384r1**      |   **P-384**    |
|  **secp521r1**      |   **P-521**    |
|  **sect283k1**      |   **K-283**    |


**Supported Algorithms:**
    
    Supported signing algorithms 
    
    ECDSA
    NONEwithECDSA, SHA-1withECDSA, SHA-224withECDSA, 
    SHA-256withECDSA, SHA-384withECDSA, SHA-512withECDSA and RIPEMD160withECDSA
    
    ECSchnorr
    SHA-1withECSCHNORRSIPA, SHA-224withECSCHNORRSIPA, SHA-256withECSCHNORRSIPA, SHA-384withECSCHNORRSIPA,
    SHA-512withECSCHNORRSIPA, RIPEMD160withECSCHNORRSIPA
    
    
    
   **Dependencies:**
   
   [HashLib4Pascal](https://github.com/Xor-el/HashLib4Pascal) >= v2.5
   
   [SimpleBaseLib4Pascal](https://github.com/Xor-el/SimpleBaseLib4Pascal) >= v1.4
    
   For FreePascal v3.0.x [Generics.Collections](https://github.com/maciej-izak/generics.collections)

**Supported Compilers**
 
    FreePascal 3.0.4 and Above.
    
    Delphi XE6 and Above. (might work in earlier versions though.)

**Installing the Library.**

**Method One:**

 Use the Provided Packages in the "Packages" Folder.

**Method Two:**

 Add the Library Path and Sub Path to your Project Search Path.

**Usage Examples.**

    Check the "CryptoLib.Samples" folder and the Unit Tests.

 **Unit Tests.**

To Run Unit Tests,

**For FPC 3.0.4 and above**


    Simply compile and run "CryptoLib.Tests" project in "FreePascal.Tests" Folder.

**For Delphi**

   **Method One (Using DUnit Test Runner)**

     To Build and Run the Unit Tests For Delphi 10 Tokyo (should be similar for 
     other versions)
    
    1). Open Project Options of Unit Test (CryptoLib.Tests) in "Delphi.Tests" Folder.
    
    2). Change Target to All Configurations (Or "Base" In Older Delphi Versions.)
    
    3). In Output directory add ".\$(Platform)\$(Config)" without the quotes.
    
    4). In Search path add "$(BDS)\Source\DUnit\src" without the quotes.
    
    5). In Unit output directory add "." without the quotes.
    
    6). In Unit scope names (If Available), Delete "DUnitX" from the List.
    
    Press Ok and save, then build and run.
    
 **Method Two (Using TestInsight) (Preferred).**

    1). Download and Install TestInsight.
    
    2). Open Project Options of Unit Test (CryptoLib.Tests.TestInsight) in "Delphi.Tests" 
        Folder. 

    3). Change Target to All Configurations (Or "Base" In Older Delphi Versions.)

    4). In Unit scope names (If Available), Delete "DUnitX" from the List.

    5). To Use TestInsight, right-click on the project, then select 
		"Enable for TestInsight" or "TestInsight Project".
        Save Project then Build and Run Test Project through TestInsight. 
        
  **Acknowledgements**
 
Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring the development of this library.

**License**

This "Software" is Licensed Under  **`MIT License (MIT)`** .

#### Tip Jar
* :dollar: **Bitcoin**: `1MhFfW7tDuEHQSgie65uJcAfJgCNchGeKf`
* :euro: **Ethereum**: `0x6c1DC21aeC49A822A4f1E3bf07c623C2C1978a98`
* :pound: **Pascalcoin**: `345367-40`

