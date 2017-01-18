(**********************************************************************************
 * Copyright (c) 2008-2011 The Khronos Group Inc.
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
 **********************************************************************************)
(************************************************)
(*                                              *)
(*     OpenCL1.2 and Delphi and Windows         *)
(*                                              *)
(*      created by      : Maksym Tymkovych      *)
(*                           (niello)           *)
(*                                              *)
(*      headers versions: 0.07                  *)
(*      file name       : CL_d3d9.pas           *)
(*      last modify     : 10.12.11              *)
(*      license         : BSD                   *)
(*                                              *)
(*      Site            : www.niello.org.ua     *)
(*      e-mail          : muxamed13@ukr.net     *)
(*      ICQ             : 446-769-253           *)
(*                                              *)
(*      updated by      : Alexander Kiselev     *)
(*                          (Igroman)           *)
(*      Site : http://Igroman14.livejournal.com *)
(*      e-mail          : Igroman14@yandex.ru   *)
(*      ICQ             : 207-381-695           *)
(*                    (c) 2010                  *)
(*                                              *)
(***********Copyright (c) niello 2008-2011*******)
unit CL_D3D9;

interface

{$INCLUDE OpenCL.inc}

uses
  CL,
  Direct3D9,
  CL_Platform;

const
  CL_INVALID_D3D_OBJECT = CL_INVALID_GL_OBJECT;

const
  CL_D3D9_DEVICE = $1072;

type
  PIDirect3DResource9 = ^IDirect3DResource9;

{$IFDEF CL_VERSION_1_0}
  TclCreateFromD3D9Buffer = function (
                                        context: Tcl_context;                   (* context *)
                                        flags: Tcl_mem_flags;                   (* flags *)
                                        pD3DResource: PIDirect3DResource9;      (* pD3DResource *)  //IDirect3DResource9 *
                                        shared_handle: Pointer;                 (* shared_handle *)
                                        errcode_ret: PInteger                   (* errcode_ret *)
                                        ):Tcl_mem;
                                        {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};


  TclCreateImageFromD3D9Resource = function (context: Tcl_context;              (* context *)
                                             flags: Tcl_mem_flags;              (* flags *)
                                             pD3DResource: PIDirect3DResource9; (* pD3DResource *)  //IDirect3DResource9 *
                                             shared_handle: Pointer;            (* shared_handle *)
                                             errcode_ret: PInteger              (* errcode_ret *)
                                             ):Tcl_mem;
                                             {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clCreateFromD3D9Buffer: TclCreateFromD3D9Buffer;
  clCreateImageFromD3D9Resource: TclCreateImageFromD3D9Resource;
{$ENDIF}

function InitCL_D3D9: Boolean;

implementation

function InitCL_D3D9: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then
  begin
    {$IFDEF CL_VERSION_1_0}
      clCreateFromD3D9Buffer := TclCreateFromD3D9Buffer(oclGetProcAddress('clCreateFromD3D9Buffer', OCL_LibHandle));
      clCreateImageFromD3D9Resource := TclCreateImageFromD3D9Resource(oclGetProcAddress('clCreateImageFromD3D9Resource', OCL_LibHandle));
    {$ENDIF}
    Result := True;
  end;
end;

end.