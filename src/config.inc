{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  **************************************
  PascalCoin configuration file to build
  **************************************

  Compiler directives definitions:
  
  NOTE: By default, only need to choose "TESTNET" or "PRODUCTION", all other directives are automatically selected

  - TESTNET                 : Will compile a TESTNET version
  - PRODUCTION              : Will compile a PRODUCTION (Main Net) version
  
  ( User does not need to define/undefine next values )
  
  - Use_CryptoLib4Pascal    : Will not use OpenSSL library, will use pascal native CryptoLib4Pascal
  - Use_OpenSSL             : Will use OpenSSL library (Need version 1.1)
  
  - Synapse                 : Will use Synapse sockets (Preferred)
  - DelphiSockets           : Will use Delphi Indy sockets (Use only for special purposes... for example for mobile apps)
  
}

  {$DEFINE PRODUCTION}
  {.$DEFINE TESTNET}

  // Activate to define CryptoLib4Pascal by default on all compilations
  {.$DEFINE Use_CryptoLib4Pascal}


  // Used to activate RandomHash in V4 hard-fork
  {$DEFINE ACTIVATE_RANDOMHASH_V4}

  // Account Key Storage is for memory reduction because uses a unified storage to store all public keys and then use only a pointer to them
  {$DEFINE useAccountKeyStorage}

  // For GUI: Allows to show average time on blockchain explorer
  {$DEFINE SHOW_AVERAGE_TIME_STATS}

  // For special use: Allows to store in a buffer, operations not proceded due to n_operation value invalid
  {.$DEFINE BufferOfFutureOperations}
  
  // HighLog will result in a higher log generation
  {.$DEFINE HIGHLOG}

  // Warning: This define is only for testing purposes! Never enable it!
  {.$DEFINE TESTING_NO_POW_CHECK}

  // This will allow check safebox names list. Warning: This will slow app
  {.$DEFINE Check_Safebox_Names_Consistency}
  
  // This will assume that PoW on old protocols are true and will not check, usefull after enough time to increase validation speed. 
  // Warning: Use only on versions after enough time since last protocol upgrade and non main-node versions
  {$DEFINE ASSUME_VALID_POW_OLD_PROTOCOLS}
  
  // Activate ABSTRACTMEM library. Will use a virtual memory caching mechanism for efficient usage without high RAM requirements
  {$DEFINE USE_ABSTRACTMEM}
  

{ ********************************************************************
  Don't touch more code, it will addapt based on your preferences
  ******************************************************************** }

{$IFDEF TESTNET}{$IFDEF PRODUCTION}
ERROR: You must select ONLY ONE option: PRODUCTION or TESTNET
{$ENDIF}{$ELSE}{$DEFINE PRODUCTION}{$ENDIF}

{$IF (not Defined(Use_OpenSSL)) and (not Defined(Use_CryptoLib4Pascal))}
  {$DEFINE Use_OpenSSL}
  {$UNDEF Use_CryptoLib4Pascal}
{$ENDIF}

{$IF (Defined(Use_OpenSSL)) and (Defined(Use_CryptoLib4Pascal))}
ERROR: You must select ONLY ONE option: Use_OpenSSL or Use_CryptoLib4Pascal
{$ENDIF}
  
{$IFDEF DARWIN}
  {$UNDEF Use_OpenSSL}
  {$DEFINE Use_CryptoLib4Pascal}
{$ENDIF}
  

  {$IFDEF ANDROID} 
    // Android usage (on Delphi) does not use Synapse
    {$UNDEF Synapse}
    {$DEFINE DelphiSockets}
  {$ELSE}
    {$DEFINE Synapse}
    {$UNDEF DelphiSockets}
  {$ENDIF}
  {$UNDEF OpenSSL10}

{$IFNDEF FPC}
  // We are on Delphi compiler
  {$IF Defined(ANDROID) or Defined(MACOS) or Defined(IOS) }
    // On Delphi and Android/IOS/MAC environment, AnsiString is not available and use DelphiSockets instead of Synapse
    {$DEFINE NO_ANSISTRING}
    {$UNDEF Use_OpenSSL}
    {$DEFINE Use_CryptoLib4Pascal}
    {$UNDEF Synapse}
    {$DEFINE DelphiSockets}
  {$ELSE}
    {$UNDEF NO_ANSISTRING}
  {$ENDIF}
  
  {$IF COMPILERVERSION > 33}
    {$DEFINE DELPHI_SYDNEY_PLUS}  
  {$ENDIF}
{$ENDIF}


{$IFDEF OpenSSL10}
  ERROR: OpenSLL v1.0 is not longer valid, use OpenSSL v1.1 instead
{$ENDIF}
{$IFDEF PRODUCTION}
{$IFDEF TESTING_NO_POW_CHECK}
  ERROR: TESTING_NO_POW_CHECK not valid on PRODUCTION MODE! Use it for testing purposes only!
{$ENDIF}
{$ENDIF}

