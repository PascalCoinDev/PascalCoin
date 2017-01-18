// defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

// known delphi versions
{$IFNDEF FPC}       // if freepascal isnt defined
  {$IFDEF VER140}   // Delphi 6
    unsupported!
  {$ENDIF}

  {$IFDEF VER130}   // Delphi 5
    unsupported!
  {$ENDIF}

  {$IFDEF VER120}   // Delphi 4
    unsupported!
  {$ENDIF}

  {$IFDEF VER110}   // C++ Builder 3
    unsupported!
  {$ENDIF}

  {$IFDEF VER100}   // Delphi 3
    unsupported!
  {$ENDIF}
{$ENDIF}

{$A4}

// 64 BIT architecture
// Free pascal
{$IFDEF CPU64}
  {$DEFINE DGL_64BIT}
{$ENDIF}
// Delphi
{$IFDEF WIN64}
  {$DEFINE DGL_64BIT}
{$ENDIF}


// generell options
{$H+,O+,X+}

// detecting Windows
{$IFDEF Win32}          // Delphi and fpc of 32 Bit Windows
  {$DEFINE DGL_WIN}
{$ENDIF}

{$IFDEF Win64}          // Delphi and fpc of 32 Bit Windows
  {$DEFINE DGL_WIN}
{$ENDIF}

// detecting Linux
{$IFDEF linux}          // Linux
  {$DEFINE DGL_LINUX}
{$ENDIF}

{$IFDEF DARWIN}  // Mac OS X and FPC
   {$DEFINE DGL_MAC}
{$ENDIF}

{$IFDEF DELPHI}  // Mac OS X add Delphi
{$IFDEF MACOS}
   {$DEFINE DGL_MAC}
{$ENDIF}
{$ENDIF}
