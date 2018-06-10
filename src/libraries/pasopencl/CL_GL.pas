(******************************************************************************
 * Copyright (c) 2011 The Khronos Group Inc.
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
(************************************************************************)
(*                                                                      *)
(*                 OpenCL1.2 and Delphi and Windows                     *)
(*                                                                      *)
(*      headers versions: 0.07                                          *)
(*      file name       : CL_GL.pas                                     *)
(*      last modify     : 10.12.11                                      *)
(*      license         : BSD                                           *)
(*                                                                      *)
(*      created by      : Maksym Tymkovych (niello)                     *)
(*      Site            : www.niello.org.ua                             *)
(*      e-mail          : muxamed13@ukr.net                             *)
(*      ICQ             : 446-769-253                                   *)
(*                                                                      *)
(*      updated by      : Alexander Kiselev (Igroman)                   *)
(*      Site            : http://Igroman14.livejournal.com              *)
(*      e-mail          : Igroman14@yandex.ru                           *)
(*      ICQ             : 207-381-695                                   *)
(*                    (c) 2010                                          *)
(*                                                                      *)
(********************** Copyright (c) niello 2008-2011 ******************)

(*
 * cl_gl.h contains Khronos-approved (KHR) OpenCL extensions which have
 * OpenGL dependencies. The application is responsible for #including
 * OpenGL or OpenGL ES headers before #including cl_gl.h.
 *)
unit CL_GL;

interface

{$INCLUDE OpenCL.inc}

uses
  dglOpenGL,
  CL,
  CL_Platform;

type

  PCL_gl_object_type  = ^TCL_gl_object_type;
  TCL_gl_object_type  = TCL_uint;
  PCL_gl_texture_info = ^TCL_gl_texture_info;
  TCL_gl_texture_info = TCL_uint;
  PCL_gl_platform_info= ^TCL_gl_platform_info;
  TCL_gl_platform_info= TCL_uint;

  Tcl_GLsync = ^Integer;

const
  (* cl_gl_object_type *)
  CL_GL_OBJECT_BUFFER           =  $2000;
  CL_GL_OBJECT_TEXTURE2D        =  $2001;
  CL_GL_OBJECT_TEXTURE3D        =  $2002;
  CL_GL_OBJECT_RENDERBUFFER     =  $2003;

  (* cl_gl_texture_info *)
  CL_GL_TEXTURE_TARGET          =  $2004;
  CL_GL_MIPMAP_LEVEL            =  $2005;

  (* Additional Error Codes  *)
  (*
    Returned by clCreateContext, clCreateContextFromType, and
    clGetGLContextInfoKHR when an invalid OpenGL context or share group
    object handle is specified in <properties>:
  *)
  CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR  = -1000;

  (* cl_gl_context_info  *)
  (*
    Accepted as the <param_name> argument of clGetGLContextInfoKHR:
  *)
  CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR    = $2006;
  CL_DEVICES_FOR_GL_CONTEXT_KHR           = $2007;

  (* Additional cl_context_properties  *)
  (*
    Accepted as an attribute name in the 'properties' argument of
    clCreateContext and clCreateContextFromType:
  *)
  CL_GL_CONTEXT_KHR             =  $2008;
  CL_EGL_DISPLAY_KHR            =  $2009;
  CL_GLX_DISPLAY_KHR            =  $200A;
  CL_WGL_HDC_KHR                =  $200B;
  CL_CGL_SHAREGROUP_KHR         =  $200C;


{$IFDEF DYNLINK}
type
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateFromGLBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clCreateFromGLBuffer{$ENDIF}(
                                      context: Pcl_context;                     (* context *)
                                      flags: Tcl_mem_flags;                     (* flags *)
                                      bufobj:  GLuint;                          (* bufobj *)
                                      errcode_ret: PInteger                     (* errcode_ret *)
                                      ): PCL_mem;
                                      extdecl; {$IFNDEF DYNLINK}external name 'clCreateFromGLBuffer';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    {$IFDEF DYNLINK}TclCreateFromGLTexture2D = {$ENDIF}function {$IFNDEF DYNLINK}clCreateFromGLTexture2D{$ENDIF}(
                                         context: Pcl_context;                  (* context *)
                                         flags: Tcl_mem_flags;                  (* flags *)
                                         target: GLenum;                        (* target *)
                                         miplevel: GLint;                       (* miplevel *)
                                         texture: GLuint;                       (* texture *)
                                         errcode_ret: Pcl_int                   (* errcode_ret *)
                                         ): PCL_mem;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clCreateFromGLTexture2D';{$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    {$IFDEF DYNLINK}TclCreateFromGLTexture3D = {$ENDIF}function {$IFNDEF DYNLINK}clCreateFromGLTexture3D{$ENDIF}(
                                         context: Pcl_context;                  (* context *)
                                         flags: Tcl_mem_flags;                  (* flags *)
                                         target: GLenum;                        (* target *)
                                         miplevel: GLint;                       (* miplevel *)
                                         texture: GLuint;                       (* texture *)
                                         errcode_ret: Pcl_int                   (* errcode_ret *)
                                         ): PCL_mem;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clCreateFromGLTexture3D';{$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclCreateFromGLTexture = {$ENDIF}function {$IFNDEF DYNLINK}clCreateFromGLTexture{$ENDIF}(
                                         context: Pcl_context;                  (* context *)
                                         flags: Tcl_mem_flags;                  (* flags *)
                                         target: GLenum;                        (* target *)
                                         miplevel: GLint;                       (* miplevel *)
                                         texture: GLuint;                       (* texture *)
                                         errcode_ret: Pcl_int                   (* errcode_ret *)
                                         ): PCL_mem;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clCreateFromGLTexture';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateFromGLRenderbuffer = {$ENDIF}function {$IFNDEF DYNLINK}clCreateFromGLRenderbuffer{$ENDIF}(
                                            context: Pcl_context;               (* context *)
                                            flags: Tcl_mem_flags;               (* flags *)
                                            renderbuffer: GLuint;               (* renderbuffer *)
                                            errcode_ret: Pcl_int                (* errcode_ret *)
                                            ): Pcl_mem;
                                            extdecl; {$IFNDEF DYNLINK}external name 'clCreateFromGLRenderbuffer';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetGLObjectInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetGLObjectInfo{$ENDIF}(
                                   memobj: Pcl_mem;                             (* memobj *)
                                   gl_object_type: Pcl_gl_object_type;          (* gl_object_type *)
                                   gl_object_name: PGLuint                      (* gl_object_name *)
                                   ): TCL_int;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clGetGLObjectInfo';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetGLTextureInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetGLTextureInfo{$ENDIF}(
                                    memobj: Pcl_mem;                            (* memobj *)
                                    param_name: Tcl_gl_texture_info;            (* param_name *)
                                    param_value_size: Tsize_t;                  (* param_value_size *)
                                    param_value: Pointer;                       (* param_value *)
                                    param_value_size_ret: Psize_t               (* param_value_size_ret *)
                                    ): TCL_int;
                                    extdecl; {$IFNDEF DYNLINK}external name 'clGetGLTextureInfo';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueAcquireGLObjects = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueAcquireGLObjects{$ENDIF}(
                                           command_queue: Pcl_command_queue;    (* command_queue *)
                                           num_objects: Tcl_uint;               (* num_objects *)
                                           const mem_objects: PPcl_mem;         (* mem_objects *)
                                           num_events_in_wait_list: Tcl_uint;   (* num_events_in_wait_list *)
                                           const event_wait_list: PPcl_event;   (* event_wait_list *)
                                           event: PPcl_event                    (* event *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueAcquireGLObjects';{$ENDIF}
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueReleaseGLObjects = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueReleaseGLObjects{$ENDIF}(
                                           command_queue: Pcl_command_queue;    (* command_queue *)
                                           num_objects: Tcl_uint;               (* num_objects *)
                                           const mem_objects: PPcl_mem;         (* mem_objects *)
                                           num_events_in_wait_list: Tcl_uint;   (* num_events_in_wait_list *)
                                           const event_wait_list: PPcl_event;   (* event_wait_list *)
                                           event: PPcl_event                    (* event *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueReleaseGLObjects';{$ENDIF}
{$ENDIF}
type
     Pcl_gl_context_info = ^Tcl_gl_context_info;
     Tcl_gl_context_info = Tcl_uint;


{$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetGLContextInfoKHR = {$ENDIF}function {$IFNDEF DYNLINK}clGetGLContextInfoKHR{$ENDIF}(
                                       const properties: Pcl_context_properties;(* properties *)
                                       param_name: Tcl_gl_context_info;         (* param_name *)
                                       param_value_size: Tsize_t;               (* param_value_size *)
                                       param_value: Pointer;                    (* param_value *)
                                       param_value_size_ret : Psize_t           (* param_value_size_ret *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clGetGLContextInfoKHR';{$ENDIF}
{$ENDIF}
{$IFDEF DYNLINK}
var
{$IFDEF CL_VERSION_1_0}
  clCreateFromGLBuffer:       TclCreateFromGLBuffer;
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    clCreateFromGLTexture2D:    TclCreateFromGLTexture2D;
    clCreateFromGLTexture3D:    TclCreateFromGLTexture3D;
  {$ENDIF}
  clCreateFromGLRenderbuffer: TclCreateFromGLRenderbuffer;
  clGetGLObjectInfo:          TclGetGLObjectInfo;
  clGetGLTextureInfo:         TclGetGLTextureInfo;
  clEnqueueAcquireGLObjects:  TclEnqueueAcquireGLObjects;
  clEnqueueReleaseGLObjects:  TclEnqueueReleaseGLObjects;
  clGetGLContextInfoKHR:      TclGetGLContextInfoKHR;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clCreateFromGLTexture:      TclCreateFromGLTexture;
{$ENDIF}
{$ENDIF}

function InitCL_GL: Boolean;

implementation

function InitCL_GL: Boolean;
begin
  {$IFDEF DYNLINK}
  Result := False;
  if OCL_LibHandle <> nil then
  begin
    {$IFDEF CL_VERSION_1_0}
      clCreateFromGLBuffer := TclCreateFromGLBuffer(oclGetProcAddress('clCreateFromGLBuffer', OCL_LibHandle));
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
        clCreateFromGLTexture2D := TclCreateFromGLTexture2D(oclGetProcAddress('clCreateFromGLTexture2D', OCL_LibHandle));
        clCreateFromGLTexture3D := TclCreateFromGLTexture3D(oclGetProcAddress('clCreateFromGLTexture3D', OCL_LibHandle));
      {$ENDIF}
      clCreateFromGLRenderbuffer := TclCreateFromGLRenderbuffer(oclGetProcAddress('clCreateFromGLRenderbuffer', OCL_LibHandle));
      clGetGLObjectInfo := TclGetGLObjectInfo(oclGetProcAddress('clGetGLObjectInfo', OCL_LibHandle));
      clGetGLTextureInfo := TclGetGLTextureInfo(oclGetProcAddress('clGetGLTextureInfo', OCL_LibHandle));
      clEnqueueAcquireGLObjects := TclEnqueueAcquireGLObjects(oclGetProcAddress('clEnqueueAcquireGLObjects', OCL_LibHandle));
      clEnqueueReleaseGLObjects := TclEnqueueReleaseGLObjects(oclGetProcAddress('clEnqueueReleaseGLObjects', OCL_LibHandle));
      clGetGLContextInfoKHR := TclGetGLContextInfoKHR(clGetExtensionFunctionAddress('clGetGLContextInfoKHR'));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_2}
      clCreateFromGLTexture := TclCreateFromGLTexture(oclGetProcAddress('clCreateFromGLTexture', OCL_LibHandle));
    {$ENDIF}
    Result := True;
  end;
  {$ELSE}
  Result := true;
  {$ENDIF}
end;

end.
