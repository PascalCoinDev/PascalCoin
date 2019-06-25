{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$DEFINE DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)
{$IFDEF FPC}
{$I CryptoLibHelper.inc} // Had to Include this Since Delphi Does not allow "FPC_FULLVERSION" to Compile.
{$UNDEF DELPHI}
{$MODE delphi}

{$IFDEF CPU386}
   {$DEFINE CRYPTOLIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE CRYPTOLIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM}
   {$DEFINE CRYPTOLIB_ARM}
{$ENDIF}

{$IFDEF CPUAARCH64}
   {$DEFINE CRYPTOLIB_AARCH64}
{$ENDIF}

{$IFDEF IPHONESIM}
   {$DEFINE CRYPTOLIB_IOSSIM}
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
   {$DEFINE CRYPTOLIB_MSWINDOWS}
{$ELSEIF DEFINED(UNIX)}
   {$DEFINE CRYPTOLIB_UNIX}
   {$IF DEFINED(BSD)}
      {$IF DEFINED(DARWIN)}
         {$DEFINE CRYPTOLIB_APPLE}
         {$IF DEFINED(CRYPTOLIB_ARM) OR DEFINED(CRYPTOLIB_AARCH64)}
            {$DEFINE CRYPTOLIB_IOS}
         {$ELSE}
            {$DEFINE CRYPTOLIB_MACOS}
         {$IFEND}
      {$ELSEIF DEFINED(FREEBSD) OR DEFINED(NETBSD) OR DEFINED(OPENBSD) OR DEFINED(DRAGONFLY)}
         {$DEFINE CRYPTOLIB_GENERIC_BSD}
      {$IFEND}
  {$ELSEIF DEFINED(LINUX)}
     {$DEFINE CRYPTOLIB_LINUX}
  {$ELSE}
     {$DEFINE CRYPTOLIB_UNDEFINED_UNIX_VARIANTS}
  {$IFEND}
{$ELSE}
   {$MESSAGE ERROR 'UNSUPPORTED TARGET.'}
{$ENDIF}

{$DEFINE USE_UNROLLED_VARIANT}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

// Enable Pointer Math
{$POINTERMATH ON}

// Disable Warnings and Hints.
{$WARNINGS OFF}
{$HINTS OFF}
{$NOTES OFF}

// Optimizations
{$OPTIMIZATION LEVEL3}
{$OPTIMIZATION PEEPHOLE}
{$OPTIMIZATION REGVAR}
{$OPTIMIZATION LOOPUNROLL}
{$OPTIMIZATION STRENGTH}
{$OPTIMIZATION CSE}
{$OPTIMIZATION DFA}

{$IFDEF CPUI386}
   {$OPTIMIZATION USEEBP}
{$ENDIF}

{$IFDEF CPUX86_64}
   {$OPTIMIZATION USERBP}
{$ENDIF}

{$ENDIF FPC}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$IFDEF DELPHI}

{$IFDEF CPU386}
   {$DEFINE CRYPTOLIB_X86}
{$ENDIF}

{$IFDEF CPUX64}
   {$DEFINE CRYPTOLIB_X86_64}
{$ENDIF}

{$IFDEF CPUARM32}
   {$DEFINE CRYPTOLIB_ARM}
{$ENDIF}

{$IFDEF CPUARM64}
   {$DEFINE CRYPTOLIB_AARCH64}
{$ENDIF}

{$IFDEF IOS}
  {$IFNDEF CPUARM}
     {$DEFINE CRYPTOLIB_IOSSIM}
  {$ENDIF}
{$ENDIF}

{$IFDEF IOS}
   {$DEFINE CRYPTOLIB_IOS}
{$IFEND}

{$IFDEF MSWINDOWS}
   {$DEFINE CRYPTOLIB_MSWINDOWS}
{$ENDIF}

{$IFDEF MACOS}
   {$DEFINE CRYPTOLIB_MACOS}
{$ENDIF}

{$IF DEFINED(CRYPTOLIB_IOS) OR DEFINED(CRYPTOLIB_MACOS)}
   {$DEFINE CRYPTOLIB_APPLE}
{$IFEND}

{$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
   {$DEFINE CRYPTOLIB_LINUX}
{$ENDIF}

{$IF DEFINED(CRYPTOLIB_APPLE) OR DEFINED(CRYPTOLIB_LINUX)}
   {$DEFINE CRYPTOLIB_UNIX}
{$ENDIF}

{$DEFINE USE_UNROLLED_VARIANT}

// This option is needed to enable code browsing (aka Ctrl+Click)
// It does not affect the binary size or generated code
{$DEFINITIONINFO ON}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

 // Enable Pointer Math
{$POINTERMATH ON}

// Disable String Checks
{$STRINGCHECKS OFF}

// Disable Duplicate Constructor Warnings
{$WARN DUPLICATE_CTOR_DTOR OFF}

// Disable Deprecated Warnings
{$WARN SYMBOL_DEPRECATED OFF}

 // XE3 and Above
{$IF CompilerVersion >= 24.0}
   {$DEFINE DELPHIXE3_UP}
   {$DEFINE SUPPORT_TSTREAM_READ_BYTEARRAY_OVERLOAD}
   {$DEFINE SUPPORT_TSTREAM_WRITE_BYTEARRAY_OVERLOAD}
   {$LEGACYIFEND ON}
   {$ZEROBASEDSTRINGS OFF}
{$IFEND}

  // XE and Above
{$IF CompilerVersion >= 22.0}
   {$DEFINE DELPHIXE_UP}
{$IFEND}

 // XE4 and Above
{$IF CompilerVersion >= 25.0}
   {$DEFINE DELPHIXE4_UP}
   {$DEFINE SHIFT_OVERFLOW_BUG_FIXED}
{$IFEND}

  // 10.2 Tokyo and Above
{$IF CompilerVersion >= 32.0}
   {$DEFINE DELPHI10.2_TOKYO_UP}
{$IFEND}

  // 10.2 Tokyo and Above
{$IFNDEF DELPHI10.2_TOKYO_UP}
   {$MESSAGE ERROR 'This Library requires Delphi Tokyo or higher.'}
{$ENDIF}


{$ENDIF DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

