{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical miPendingOperations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  **************************************
  PascalCoin configuration file to build
  **************************************

  Choose ONLY ONE of directives:

    OPTIONS_BY_DEFAULT:
      In Delphi = Synapse + OpenSSL v1.0
      In FPC = Synapse + OpenSSL v1.1
    DelphiSockets_OpenSSLv10
    Synapse_OpenSSLv10
    Synapse_OpenSSLv11

}

  {$DEFINE OPTIONS_BY_DEFAULT}
  {.$DEFINE DelphiSockets_OpenSSLv10}
  {.$DEFINE Synapse_OpenSSLv10}
  {.$DEFINE Synapse_OpenSSLv11}

  {.$DEFINE PRODUCTION}
  {$DEFINE TESTNET}

  // For GUI: Allows to show average time on blockchain explorer
  {.$DEFINE SHOW_AVERAGE_TIME_STATS}

  // For special use: Allows to store in a buffer, miPendingOperations not proceded due to n_operation value invalid
  {.$DEFINE BufferOfFutureOperations}
  
  // HighLog will result in a higher log generation
  {.$DEFINE HIGHLOG}

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
  // By default are: Delphi = Synapse + OpenSSLv10  -  FPC = Synapse + OpenSSLv11
  {$IFDEF FPC}
    // With Free Pascal Compiler, only Synapse + OpenSSLv1.0 for Delphi or OpenSSLv1.1 for Linux
    {$DEFINE Synapse}
    {$IFDEF LINUX}
    {$UNDEF OpenSSL10}
    {$ELSE}
    {$DEFINE OpenSSL10}
    {$ENDIF}
  {$ELSE}
    // Delphi, Synapse + OpenSSL v1.0
    {$DEFINE Synapse}
    {$DEFINE OpenSSL10}
  {$ENDIF}
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

