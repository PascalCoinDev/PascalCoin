(*******************************************************************************
 * Copyright (c) 2008-2010 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 ******************************************************************************)
(********************************************)
(*                                          *)
(*     OpenCL1.2 and Delphi and Windows     *)
(*                                          *)
(*      created by      : Maksym Tymkovych  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.07              *)
(*      file name       : CL_Platform.pas   *)
(*      last modify     : 10.12.11          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2011*****)

//Fixed By Dmitry Belkevich
//Site www.makhaon.com
//E-mail dmitry@makhaon.com
//(c) 2009
//Beta release 1.0

unit CL_Platform;

interface

(*
Delphi 6 and down don't support UInt64;
*)

{$INCLUDE 'OpenCL.inc'}

type
  PCL_char    = ^TCL_char;
  TCL_char    = Shortint;//-127..+128;

  PCL_uchar   = ^TCL_uchar;
  TCL_uchar   = Byte;//0..255;

  PCL_short   = ^TCL_short;
  TCL_short   = Smallint;//- 32767..+32768;

  PCL_ushort  = ^TCL_ushort;
  TCL_ushort  = Word;//0..+65535;

  PCL_int     = ^TCL_int;
  TCL_int     = Longint;//-2147483647..+2147483648;

  PCL_uint    = ^TCL_uint;
  TCL_uint    = Longword;//0..4294967295;

  PCL_long    = ^TCL_long;
  TCL_long    = Int64;

  PCL_ulong   = ^TCL_ulong;
  //The error is found by Andrew Terekhov
  TCL_ulong   = {$IFDEF DEFINE_UINT64_EQU_INT64} Int64;{$ELSE} UInt64;{$ENDIF}

  PCL_half   = ^TCL_half;
  TCL_half    = TCL_ushort;

  PCL_float   = ^TCL_float;
  TCL_float   = Single;

  PCL_double  = ^TCL_double;
  TCL_double  = Double;

  PCL_half2 = ^TCL_half2;
  TCL_half2 = record
    i16 : Array [0..1]of TCL_half;
  end;

  PCL_half4 = ^TCL_half4;
  TCL_half4 = record
    i16 : Array [0..3]of TCL_half;
  end;

  PCL_half8 = ^TCL_half8;
  TCL_half8 = record
    i16 : Array [0..7]of TCL_half;
  end;

  PCL_half16 = ^TCL_half16;
  TCL_half16 = record
    i16 : Array [0..15]of TCL_half;
  end;

  PCL_char2 = ^TCL_char2;
  TCL_char2 = record
    i8 : Array [0..1]of TCL_char;
  end;

  PCL_char4 = ^TCL_char4;
  TCL_char4 = record
    i8 : Array [0..3]of TCL_char;
  end;

  PCL_char8 = ^TCL_char8;
  TCL_char8 = record
    i8 : Array [0..7]of TCL_char;
  end;

  PCL_char16 = ^TCL_char16;
  TCL_char16 = record
    i8 : Array [0..15]of TCL_char;
  end;

  PCL_uchar2 = ^TCL_uchar2;
  TCL_uchar2 = record
    u8 : Array [0..1]of TCL_uchar;
  end;

  PCL_uchar4 = ^TCL_uchar4;
  TCL_uchar4 = record
    u8 : Array [0..3]of TCL_uchar;
  end;

  PCL_uchar8 = ^TCL_uchar8;
  TCL_uchar8 = record
    u8 : Array [0..7]of TCL_uchar;
  end;

  PCL_uchar16 = ^TCL_uchar16;
  TCL_uchar16 = record
    u8 : Array [0..15]of TCL_uchar;
  end;

  PCL_short2 = ^TCL_short2;
  TCL_short2 = record
    i16 : Array [0..1]of TCL_short;
  end;

  PCL_short4 = ^TCL_short4;
  TCL_short4 = record
    i16 : Array [0..3]of TCL_short;
  end;

  PCL_short8 = ^TCL_short8;
  TCL_short8 = record
    i16 : Array [0..7]of TCL_short;
  end;

  PCL_short16 = ^TCL_short16;
  TCL_short16 = record
    i16 : Array [0..15]of TCL_short;
  end;

  PCL_ushort2 = ^TCL_ushort2;
  TCL_ushort2 = record
    u16 : Array [0..1]of TCL_ushort;
  end;

  PCL_ushort4 = ^TCL_ushort4;
  TCL_ushort4 = record
    u16 : Array [0..3]of TCL_ushort;
  end;

  PCL_ushort8 = ^TCL_ushort8;
  TCL_ushort8 = record
    u16 : Array [0..7]of TCL_ushort;
  end;

  PCL_ushort16 = ^TCL_ushort16;
  TCL_ushort16 = record
    u16 : Array [0..15]of TCL_ushort;
  end;

  PCL_int2 = ^TCL_int2;
  TCL_int2 = record
    i32 : Array [0..1]of TCL_int;
  end;

  PCL_int4 = ^TCL_int4;
  TCL_int4 = record
    i32 : Array [0..3]of TCL_int;
  end;

  PCL_int8 = ^TCL_int8;
  TCL_int8 = record
    i32 : Array [0..7]of TCL_int;
  end;

  PCL_int16 = ^TCL_int16;
  TCL_int16 = record
    i32 : Array [0..15]of TCL_int;
  end;

  PCL_uint2 = ^TCL_uint2;
  TCL_uint2 = record
    u32 : Array [0..1]of TCL_uint;
  end;

  PCL_uint4 = ^TCL_uint4;
  TCL_uint4 = record
    u32 : Array [0..3]of TCL_uint;
  end;

  PCL_uint8 = ^TCL_uint8;
  TCL_uint8 = record
    u32 : Array [0..7]of TCL_uint;
  end;

  PCL_uint16 = ^TCL_uint16;
  TCL_uint16 = record
    u32 : Array [0..15]of TCL_uint;
  end;

  PCL_long2 = ^TCL_long2;
  TCL_long2 = record
    i64 : Array [0..1]of TCL_long;
  end;

  PCL_long4 = ^TCL_long4;
  TCL_long4 = record
    i64 : Array [0..3]of TCL_long;
  end;

  PCL_long8 = ^TCL_long8;
  TCL_long8 = record
    i64 : Array [0..7]of TCL_long;
  end;

  PCL_long16 = ^TCL_long16;
  TCL_long16 = record
    i64 : Array [0..15]of TCL_long;
  end;

  PCL_ulong2 = ^TCL_ulong2;
  TCL_ulong2 = record
    u64 : Array [0..1]of TCL_ulong;
  end;

  PCL_ulong4 = ^TCL_ulong4;
  TCL_ulong4 = record
    u64 : Array [0..3]of TCL_ulong;
  end;

  PCL_ulong8 = ^TCL_ulong8;
  TCL_ulong8 = record
    u64 : Array [0..7]of TCL_ulong;
  end;

  PCL_ulong16 = ^TCL_ulong16;
  TCL_ulong16 = record
    u64 : Array [0..15]of TCL_ulong;
  end;

  PCL_float2 = ^TCL_float2;
  TCL_float2 = record
    f32 : Array [0..1]of TCL_float;
  end;

  PCL_float4 = ^TCL_float4;
  TCL_float4 = record
    f32 : Array [0..3]of TCL_float;
  end;

  PCL_float8 = ^TCL_float8;
  TCL_float8 = record
    f32 : Array [0..7]of TCL_float;
  end;

  PCL_float16 = ^TCL_float16;
  TCL_float16 = record
    f32 : Array [0..15]of TCL_float;
  end;

  PCL_double2 = ^TCL_double2;
  TCL_double2 = record
    f64 : Array [0..1]of TCL_double;
  end;

  PCL_double4 = ^TCL_double4;
  TCL_double4 = record
    f64 : Array [0..3]of TCL_double;
  end;

  PCL_double8 = ^TCL_double8;
  TCL_double8 = record
    f64 : Array [0..7]of TCL_double;
  end;

  PCL_double16 = ^TCL_double16;
  TCL_double16 = record
    f64 : Array [0..15]of TCL_double;
  end;

const
  CL_CHAR_BIT       =  8;
  CL_SCHAR_MAX      =  127;
  CL_SCHAR_MIN      =  (-127-1);
  CL_CHAR_MAX       =  CL_SCHAR_MAX;
  CL_CHAR_MIN       =  CL_SCHAR_MIN;
  CL_UCHAR_MAX      =  255;
  CL_SHRT_MAX       =  32767;
  CL_SHRT_MIN       =  (-32767-1);
  CL_USHRT_MAX      =  65535;
  CL_INT_MAX        =  2147483647;
  CL_INT_MIN        =  (-2147483647-1);
  CL_UINT_MAX       =  $ffffffff;
  CL_LONG_MAX       =  TCL_long ($7FFFFFFFFFFFFFFF);
  CL_LONG_MIN       =  TCL_long (-$7FFFFFFFFFFFFFFF) - 1;
  CL_ULONG_MAX      =  TCL_ulong($FFFFFFFFFFFFFFFF);

  CL_FLT_DIG        =  6;
  CL_FLT_MANT_DIG   =  24;
  CL_FLT_MAX_10_EXP =  +38;
  CL_FLT_MAX_EXP    =  +128;
  CL_FLT_MIN_10_EXP =  -37;
  CL_FLT_MIN_EXP    =  -125;
  CL_FLT_RADIX      =  2;
  CL_FLT_MAX        =  340282346638528859811704183484516925440.0;
  CL_FLT_MIN        =  1.175494350822287507969e-38;
  //CL_FLT_EPSILON    =  0x1.0p-23f;

  CL_DBL_DIG        =  15;
  CL_DBL_MANT_DIG   =  53;
  CL_DBL_MAX_10_EXP =  +308;
  CL_DBL_MAX_EXP    =  +1024;
  CL_DBL_MIN_10_EXP =  -307;
  CL_DBL_MIN_EXP    =  -1021;
  CL_DBL_RADIX      =  2;
  CL_DBL_MAX        =  179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0;
  CL_DBL_MIN        =  2.225073858507201383090e-308;
  CL_DBL_EPSILON    =  2.220446049250313080847e-16;

  CL_M_E            = 2.718281828459045090796;
  CL_M_LOG2E        = 1.442695040888963387005;
  CL_M_LOG10E       = 0.434294481903251816668;
  CL_M_LN2          = 0.693147180559945286227;
  CL_M_LN10         = 2.302585092994045901094;
  CL_M_PI           = 3.141592653589793115998;
  CL_M_PI_2         = 1.570796326794896557999;
  CL_M_PI_4         = 0.785398163397448278999;
  CL_M_1_PI         = 0.318309886183790691216;
  CL_M_2_PI         = 0.636619772367581382433;
  CL_M_2_SQRTPI     = 1.128379167095512558561;
  CL_M_SQRT2        = 1.414213562373095145475;
  CL_M_SQRT1_2      = 0.707106781186547572737;

  CL_M_E_F          = 2.71828174591064;
  CL_M_LOG2E_F      = 1.44269502162933;
  CL_M_LOG10E_F     = 0.43429449200630;
  CL_M_LN2_F        = 0.69314718246460;
  CL_M_LN10_F       = 2.30258512496948;
  CL_M_PI_F         = 3.14159274101257;
  CL_M_PI_2_F       = 1.57079637050629;
  CL_M_PI_4_F       = 0.78539818525314;
  CL_M_1_PI_F       = 0.31830987334251;
  CL_M_2_PI_F       = 0.63661974668503;
  CL_M_2_SQRTPI_F   = 1.12837922573090;
  CL_M_SQRT2_F      = 1.41421353816986;
  CL_M_SQRT1_2_F    = 0.70710676908493;


  CL_HUGE_VALF      : TCL_float  = 1e50;
  CL_HUGE_VAL       : TCL_double = 1e500;
  CL_MAXFLOAT       = CL_FLT_MAX;
  CL_INFINITY       : TCL_float  = 1e50; //CL_HUGE_VALF
  CL_NAN            = 0/0;//(CL_INFINITY - CL_INFINITY);

implementation

end.
