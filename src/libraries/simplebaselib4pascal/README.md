# SimpleBaseLib4Pascal [![License](http://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Xor-el/SimpleBaseLib4Pascal/blob/master/LICENSE)
SimpleBaseLib4Pascal as the name implies is a simple to use Base Encoding Package for Delphi/FreePascal Compilers that provides at the moment support for encoding and decoding various bases such as Base16, Base32 (various variants), Base58 (various variants) and Base64 (various variants) and Base85 (various variants).

**Build Status**
[![Build Status](https://travis-ci.com/Xor-el/SimpleBaseLib4Pascal.svg?branch=master)](https://travis-ci.com/Xor-el/SimpleBaseLib4Pascal)

**Supported Encodings:**

    Base32: RFC 4648, Crockford and Extended Hex (BASE32-HEX) alphabets with Crockford character substitution 
    and custom flavors.
    
    Base58: Bitcoin, Ripple and Flickr alphabets and custom flavors.
    
    Base64: Default, DefaultNoPadding, UrlEncoding, XmlEncoding, RegExEncoding and FileEncoding alphabets 
    (and any custom alphabet you might have)
    
    Base85: Ascii85 (Original), Z85 and custom flavors.
    
    Base16: An experimental hexadecimal encoder/decoder.

**Supported Compilers**
 
    FreePascal 3.0.0 and Above.
    
    Delphi 2010 and Above.

**Installing the Library.**

**Method One:**

 Use the Provided Packages in the "Packages" Folder.

**Method Two:**

 Add the Library Path and Sub Path to your Project Search Path.

**Usage Examples.**

    Check the "SimpleBaseLib.Benchmark" folder and the Unit Tests.

 **Unit Tests.**

To Run Unit Tests,

**For FPC 3.0.0 and above**


    Simply compile and run "SimpleBaseLib.Tests" project in "FreePascal.Tests" Folder.

**For Delphi 2010 and above**

   **Method One (Using DUnit Test Runner)**

     To Build and Run the Unit Tests For Delphi 10 Tokyo (should be similar for 
     other versions)
    
    1). Open Project Options of Unit Test (SimpleBaseLib.Tests) in "Delphi.Tests" Folder.
    
    2). Change Target to All Configurations (Or "Base" In Older Delphi Versions.)
    
    3). In Output directory add ".\$(Platform)\$(Config)" without the quotes.
    
    4). In Search path add "$(BDS)\Source\DUnit\src" without the quotes.
    
    5). In Unit output directory add "." without the quotes.
    
    6). In Unit scope names (If Available), Delete "DUnitX" from the List.
    
    Press Ok and save, then build and run.
    
 **Method Two (Using TestInsight) (Preferred).**

    1). Download and Install TestInsight.
    
    2). Open Project Options of Unit Test (SimpleBaseLib.Tests.TestInsight) in "Delphi.Tests" 
        Folder. 

    3). Change Target to All Configurations (Or "Base" In Older Delphi Versions.)

    4). In Unit scope names (If Available), Delete "DUnitX" from the List.

    5). To Use TestInsight, right-click on the project, then select 
		"Enable for TestInsight" or "TestInsight Project".
        Save Project then Build and Run Test Project through TestInsight. 

**License**

This "Software" is Licensed Under  **`MIT License (MIT)`** .

#### Tip Jar
* :dollar: **Bitcoin**: `1MhFfW7tDuEHQSgie65uJcAfJgCNchGeKf`
* :euro: **Ethereum**: `0x6c1DC21aeC49A822A4f1E3bf07c623C2C1978a98`
* :pound: **Pascalcoin**: `345367-40`



