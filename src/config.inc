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
  - OpenSSL10               : When "Use_OpenSSL" enabled, will use version 1.0 instead of 1.1 -> DO NOT USE!
  
  - Synapse                 : Will use Synapse sockets (Preferred)
  - DelphiSockets           : Will use Delphi Indy sockets (Use only for special purposes... for example for mobile apps)
  
  - OPTIONS_BY_DEFAULT      : (Preferred)
      Will define "Synapse" and will undefine "OpenSSL10"
  - DelphiSockets_OpenSSLv10 :
      Will define "DelphiSockets" and will define "OpenSSL10"
  - Synapse_OpenSSLv10      :
      Will define "Synapse" and will define "OpenSSL10"
  - Synapse_OpenSSLv11      :
      Will define "Synapse" and will undefine "OpenSSL10" 

}

  {.$DEFINE PRODUCTION}
  {$DEFINE TESTNET}

  {$DEFINE OPTIONS_BY_DEFAULT}
  {.$DEFINE DelphiSockets_OpenSSLv10}
  {.$DEFINE Synapse_OpenSSLv10}
  {.$DEFINE Synapse_OpenSSLv11}

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

{ ********************************************************************
  Don't touch more code, it will addapt based on your preferences
  ******************************************************************** }

{$IFDEF TESTNET}{$IFDEF PRODUCTION}
ERROR: You must select ONLY ONE option: PRODUCTION or TESTNET
{$ENDIF}{$ELSE}{$DEFINE PRODUCTION}{$ENDIF}

{$IFNDEF OPTIONS_BY_DEFAULT}{$IFNDEF DelphiSockets_OpenSSLv10}{$IFNDEF Synapse_OpenSSLv10}{$IFNDEF Synapse_OpenSSLv11}
ERROR: You must select ONE option!
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
{$IFDEF OPTIONS_BY_DEFAULT}
  {$DEFINE Use_OpenSSL}
  // By default are: Synapse + OpenSSLv11
  {$IFDEF ANDROID} 
    // Android usage (on Delphi) does not use Synapse
    {$UNDEF Synapse}
    {$DEFINE DelphiSockets}
  {$ELSE}
    {$DEFINE Synapse}
  {$ENDIF}
  {$UNDEF OpenSSL10}
  {$IFDEF DelphiSockets_OpenSSLv10}ERROR: You selected more than 1 option{$ENDIF}
  {$IFDEF Synapse_OpenSSLv10}ERROR: You selected more than 1 option{$ENDIF}
  {$IFDEF Synapse_OpenSSLv11}ERROR: You selected more than 1 option{$ENDIF}
{$ELSE}
  {$IFDEF DelphiSockets_OpenSSLv10}
    {$IFDEF FPC}
      ERROR: With Free Pascal you cannot choose compiler directive DelphiSockets_OpenSSLv10
    {$ELSE}
      {$UNDEF Synapse}
      {$DEFINE OpenSSL10}
    {$ENDIF}
    {$IFDEF Synapse_OpenSSLv10}ERROR: You selected more than 1 option{$ENDIF}
    {$IFDEF Synapse_OpenSSLv11}ERROR: You selected more than 1 option{$ENDIF}
  {$ELSE}
    {$IFDEF Synapse_OpenSSLv10}
      {$DEFINE Synapse}
      {$DEFINE OpenSSL10}
      {$IFDEF Synapse_OpenSSLv11}ERROR: You selected more than 1 option{$ENDIF}
    {$ELSE}
      {$IFDEF FPC}
        {$DEFINE Synapse}
        {$UNDEF OpenSSL10}
      {$ELSE}
        ERROR: With Delphi you cannot choose compiler directive Synapse_OpenSSLv11
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ifNdef FPC}
  // We are on Delphi compiler
  {$IF Defined(ANDROID) or Defined(MACOS) or Defined(IOS) }
    // On Delphi and Android/IOS/MAC environment, AnsiString is not available and use DelphiSockets instead of Synapse
    {$DEFINE NO_ANSISTRING}
    {$UNDEF OpenSSL10}
    {$UNDEF Use_OpenSSL}
    {$DEFINE Use_CryptoLib4Pascal}
    {$UNDEF Synapse}
    {$DEFINE DelphiSockets}
  {$ELSE}
    {$UNDEF NO_ANSISTRING}
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

