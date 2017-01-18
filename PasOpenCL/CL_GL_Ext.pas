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
(************************************************)
(*                                              *)
(*     OpenCL1.2 and Delphi and Windows         *)
(*                                              *)
(*      created by      : Maksym Tymkovych      *)
(*                           (niello)           *)
(*                                              *)
(*      headers versions: 0.07                  *)
(*      file name       : CL_gl_ext.pas         *)
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

(* cl_gl_ext.h contains vendor (non-KHR) OpenCL extensions which have         *)
(* OpenGL dependencies.                                                       *)
unit CL_GL_Ext;

interface

{$INCLUDE OpenCL.inc}

(*
 *  cl_khr_gl_event  extension
 *  See section 9.9 in the OpenCL 1.1 spec for more information
 *)

uses
  CL,
  OpenGL,
  CL_GL,
  CL_Platform;

{$IFDEF CL_VERSION_1_1}
const
  CL_COMMAND_GL_FENCE_SYNC_OBJECT_KHR     = $200D;

type
  TclCreateEventFromGLsyncKHR = function(
                                         context: Pcl_context;                  (* context *)
                                         cl_GLsync: TCL_GLsync;                 (* cl_GLsync *)
                                         errcode_ret: Pcl_int                   (* errcode_ret *)
                                         ): PCL_event;
                                         {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
var
  clCreateEventFromGLsyncKHR: TclCreateEventFromGLsyncKHR;
{$ENDIF}

function InitCL_GL_EXT: Boolean;

implementation

function InitCL_GL_EXT: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then
  begin
    {$IFDEF CL_VERSION_1_1}
      clCreateEventFromGLsyncKHR := TclCreateEventFromGLsyncKHR(oclGetProcAddress('clCreateEventFromGLsyncKHR', OCL_LibHandle));
    {$ENDIF}
    Result := True;
  end;
end;

end.
