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
(*      file name       : CL_d3d10.pas          *)
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

unit CL_D3D10;

interface

{$INCLUDE OpenCL.inc}

uses
  OpenGL,
  CL,
  D3D10,
  CL_Platform;

{$INCLUDE 'OpenCL.inc'}

type
  UINT = Longword;

(******************************************************************************
 * cl_khr_d3d10_sharing                                                       *)
const
   cl_khr_d3d10_sharing = 1;
type
  Pcl_d3d10_device_source_khr = ^Tcl_d3d10_device_source_khr;
  Tcl_d3d10_device_source_khr = TCL_uint;

  Pcl_d3d10_device_set_khr = ^Tcl_d3d10_device_set_khr;
  Tcl_d3d10_device_set_khr = TCL_uint;

(******************************************************************************)
const
// Error Codes
  CL_INVALID_D3D10_DEVICE_KHR                  = -1002;
  CL_INVALID_D3D10_RESOURCE_KHR                = -1003;
  CL_D3D10_RESOURCE_ALREADY_ACQUIRED_KHR       = -1004;
  CL_D3D10_RESOURCE_NOT_ACQUIRED_KHR           = -1005;

// cl_d3d10_device_source_nv
  CL_D3D10_DEVICE_KHR                          = $4010;
  CL_D3D10_DXGI_ADAPTER_KHR                    = $4011;

// cl_d3d10_device_set_nv
  CL_PREFERRED_DEVICES_FOR_D3D10_KHR           = $4012;
  CL_ALL_DEVICES_FOR_D3D10_KHR                 = $4013;

// cl_context_info
  CL_CONTEXT_D3D10_DEVICE_KHR                  = $4014;
  CL_CONTEXT_D3D10_PREFER_SHARED_RESOURCES_KHR = $402C;

// cl_mem_info
  CL_MEM_D3D10_RESOURCE_KHR                    = $4015;

// cl_image_info
  CL_IMAGE_D3D10_SUBRESOURCE_KHR               = $4016;

// cl_command_type
  CL_COMMAND_ACQUIRE_D3D10_OBJECTS_KHR         = $4017;
  CL_COMMAND_RELEASE_D3D10_OBJECTS_KHR         = $4018;

(******************************************************************************)
{$IFDEF CL_VERSION_1_0}
type

  TclGetDeviceIDsFromD3D10KHR_fn = function(
                                              platform: Tcl_platform_id;
                                              d3d_device_source: Tcl_d3d10_device_source_khr;
                                              d3d_object: Pointer;
                                              d3d_device_set: Tcl_d3d10_device_set_khr;
                                              num_entries: Tcl_uint;
                                              devices: PPcl_device_id;
                                              num_devices: Pcl_uint
                                              ): Tcl_int;
                                              {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
type

  TclCreateFromD3D10BufferKHR_fn = function(
                                              context: Pcl_context;
                                              flags: Tcl_mem_flags;
                                              resource: PID3D10Buffer;
                                              errcode_ret: Pcl_int
                                              ): Tcl_int;
                                              {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
type

  TclCreateFromD3D10Texture2DKHR_fn = function(
                                                 context: Pcl_context;
                                                 flags: Tcl_mem_flags;
                                                 resource: PID3D10Texture2D;
                                                 subresource: UINT;
                                                 errcode_ret: Pcl_int
                                                 ): Tcl_int;
                                                 {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
type

  TclCreateFromD3D10Texture3DKHR_fn = function(
                                                 context: Pcl_context;
                                                 flags: Tcl_mem_flags;
                                                 resource: PID3D10Texture3D;
                                                 subresource: UINT;
                                                 errcode_ret: Pcl_int
                                                 ): Tcl_int;
                                                 {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
type

  TclEnqueueAcquireD3D10ObjectsKHR_fn = function(
                                                   command_queue: Pcl_command_queue;
                                                   num_objects: Tcl_uint;
                                                   const mem_objects: PPcl_mem;
                                                   num_events_in_wait_list: Tcl_uint;
                                                   const event_wait_list: PPcl_event;
                                                   event: PPcl_event
                                                   ): Tcl_int;
                                                   {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
type

  TclEnqueueReleaseD3D10ObjectsKHR_fn = function(
                                                   command_queue: Pcl_command_queue;
                                                   num_objects: Tcl_uint;
                                                   mem_objects: PPcl_mem;
                                                   num_events_in_wait_list: Tcl_uint;
                                                   const event_wait_list: PPcl_event;
                                                   event: PPcl_event
                                                   ): Tcl_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}


const

  CL_D3D10_DEVICE                            =  $1070;

{$IFDEF CL_VERSION_1_0}
type

  TclCreateFromD3D10Buffer = function (
                                         context: Pcl_context;                  (* context *)
                                         flags: Tcl_mem_flags;                  (* flags *)
                                         pD3DResource:  PID3D10Resource;        (* pD3DResource *)               //ID3D10Resource *
                                         errcode_ret: PInteger                  (* errcode_ret *)
                                         ) : Tcl_mem;
                                         {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  TclCreateImageFromD3D10Resource = function (context: Pcl_context;             (* context *)
                                                flags: Tcl_mem_flags;           (* flags *)
                                                pD3DResource: PID3D10Resource;  (* pD3DResource *)              //ID3D10Resource  *
                                                errcode_ret: PInteger           (* errcode_ret *)
                                                ) : Tcl_mem;
                                                {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
var
  clGetDeviceIDsFromD3D10KHR_fn: TclGetDeviceIDsFromD3D10KHR_fn;
  clCreateFromD3D10BufferKHR_fn: TclCreateFromD3D10BufferKHR_fn;
  clCreateFromD3D10Texture2DKHR_fn: TclCreateFromD3D10Texture2DKHR_fn;
  clCreateFromD3D10Texture3DKHR_fn: TclCreateFromD3D10Texture3DKHR_fn;
  clEnqueueAcquireD3D10ObjectsKHR_fn: TclEnqueueAcquireD3D10ObjectsKHR_fn;
  clEnqueueReleaseD3D10ObjectsKHR_fn: TclEnqueueReleaseD3D10ObjectsKHR_fn;

  clCreateFromD3D10Buffer: TclCreateFromD3D10Buffer;
  clCreateImageFromD3D10Resource: TclCreateImageFromD3D10Resource;
{$ENDIF}

function InitCL_D3D10: Boolean;

implementation

function InitCL_D3D10: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then begin
    {$IFDEF CL_VERSION_1_0}
      clGetDeviceIDsFromD3D10KHR_fn := TclGetDeviceIDsFromD3D10KHR_fn(oclGetProcAddress('clGetDeviceIDsFromD3D10KHR_fn', OCL_LibHandle));
      clCreateFromD3D10BufferKHR_fn := TclCreateFromD3D10BufferKHR_fn(oclGetProcAddress('clCreateFromD3D10BufferKHR_fn', OCL_LibHandle));
      clCreateFromD3D10Texture2DKHR_fn := TclCreateFromD3D10Texture2DKHR_fn(oclGetProcAddress('clCreateFromD3D10Texture2DKHR_fn', OCL_LibHandle));
      clCreateFromD3D10Texture3DKHR_fn := TclCreateFromD3D10Texture3DKHR_fn(oclGetProcAddress('clCreateFromD3D10Texture3DKHR_fn', OCL_LibHandle));
      clEnqueueAcquireD3D10ObjectsKHR_fn := TclEnqueueAcquireD3D10ObjectsKHR_fn(oclGetProcAddress('clEnqueueAcquireD3D10ObjectsKHR_fn', OCL_LibHandle));
      clEnqueueReleaseD3D10ObjectsKHR_fn := TclEnqueueReleaseD3D10ObjectsKHR_fn(oclGetProcAddress('clEnqueueReleaseD3D10ObjectsKHR_fn', OCL_LibHandle));

      clCreateFromD3D10Buffer := TclCreateFromD3D10Buffer(oclGetProcAddress('clCreateFromD3D10Buffer', OCL_LibHandle));
      clCreateImageFromD3D10Resource := TclCreateImageFromD3D10Resource(oclGetProcAddress('clCreateImageFromD3D10Resource', OCL_LibHandle));
    {$ENDIF}
    Result := True;
  end;
end;

end.