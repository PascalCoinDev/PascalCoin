(*******************************************************************************
 * Copyright (c) 2008-2012 The Khronos Group Inc.
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
(*      file name     : CL_dx9_media_sharing.pas*)
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
unit CL_DX9_Media_Sharing;

interface

{$INCLUDE OpenCL.inc}

uses
  CL_Platform,
  CL,
  Direct3D9;

(******************************************************************************
(* cl_khr_dx9_media_sharing                                                   *)
const
  cl_khr_dx9_media_sharing = 1;
type

PIDirect3DSurface9 = ^IDirect3DSurface9;

Pcl_dx9_surface_info_khr = ^Tcl_dx9_surface_info_khr;
Tcl_dx9_surface_info_khr = packed record
    resource: PIDirect3DSurface9;
    shared_handle: THandle;
end;

(******************************************************************************)
const
// Error Codes
  CL_INVALID_DX9_MEDIA_ADAPTER_KHR                = -1010;
  CL_INVALID_DX9_MEDIA_SURFACE_KHR                = -1011;
  CL_DX9_MEDIA_SURFACE_ALREADY_ACQUIRED_KHR       = -1012;
  CL_DX9_MEDIA_SURFACE_NOT_ACQUIRED_KHR           = -1013;

// cl_media_adapter_type_khr
  CL_ADAPTER_D3D9_KHR                             = $2020;
  CL_ADAPTER_D3D9EX_KHR                           = $2021;
  CL_ADAPTER_DXVA_KHR                             = $2022;

// cl_media_adapter_set_khr
  CL_PREFERRED_DEVICES_FOR_DX9_MEDIA_ADAPTER_KHR  = $2023;
  CL_ALL_DEVICES_FOR_DX9_MEDIA_ADAPTER_KHR        = $2024;

// cl_context_info
  CL_CONTEXT_D3D9_DEVICE_KHR                      = $2025;
  CL_CONTEXT_D3D9EX_DEVICE_KHR                    = $2026;
  CL_CONTEXT_DXVA_DEVICE_KHR                      = $2027;

// cl_mem_info
  CL_MEM_DX9_MEDIA_ADAPTER_TYPE_KHR               = $2028;
  CL_MEM_DX9_MEDIA_SURFACE_INFO_KHR               = $2029;

// cl_image_info
  CL_IMAGE_DX9_MEDIA_PLANE_KHR                    = $202A;

// cl_command_type
  CL_COMMAND_ACQUIRE_DX9_MEDIA_SURFACES_KHR       = $202B;
  CL_COMMAND_RELEASE_DX9_MEDIA_SURFACES_KHR       = $202C;

(******************************************************************************)

{$IFDEF CL_VERSION_1_2}

type
  TclGetDeviceIDsForDX9MediaAdapterKHR_fn = function
    (
    _platform: Pcl_platform_id;
    //media_adapter_type: Tcl_dx9_media_adapter_type_khr;
    //media_adapter: Pointer; media_adapter_set: Tcl_dx9_media_adapter_set_khr;
    num_entries: Tcl_uint; devices: PPcl_device_id;
    num_devices: Pcl_uint
    ): TCL_int; {$IFDEF CDECL} cdecl {$ELSE} stdcall{$ENDIF};

  TclCreateFromDX9MediaSurfaceKHR_fn = function
    (
    context: Pcl_context;
    flags: Tcl_mem_flags;
    //adapter_type: Tcl_dx9_media_adapter_type_khr;
    surface_info: Pointer;
    plane: Tcl_uint;
    errcode_ret: Pcl_int
    ): PCL_mem; {$IFDEF CDECL} cdecl {$ELSE} stdcall{$ENDIF};

  TclEnqueueAcquireDX9MediaSurfacesKHR_fn = function
    (
    command_queue: Pcl_command_queue;
    num_objects: Tcl_uint;
    const mem_objects: PPcl_mem;
    num_events_in_wait_list: Tcl_uint;
    const event_wait_list: PPcl_event;
    event: PPcl_event
    ): TCL_int; {$IFDEF CDECL} cdecl {$ELSE} stdcall{$ENDIF};

  TclEnqueueReleaseDX9MediaSurfacesKHR_fn = function
    (
    command_queue: Pcl_command_queue;
    num_objects: Tcl_uint;
    const mem_objects: PPcl_mem;
    num_events_in_wait_list: Tcl_uint;
    const event_wait_list: PPcl_event;
    event: PPcl_event
    ): TCL_int; {$IFDEF CDECL} cdecl {$ELSE} stdcall{$ENDIF};

{$ENDIF}

{$IFDEF CL_VERSION_1_2}
var
  clGetDeviceIDsForDX9MediaAdapterKHR_fn: TclGetDeviceIDsForDX9MediaAdapterKHR_fn;
  clCreateFromDX9MediaSurfaceKHR_fn: TclCreateFromDX9MediaSurfaceKHR_fn;
  clEnqueueAcquireDX9MediaSurfacesKHR_fn: TclEnqueueAcquireDX9MediaSurfacesKHR_fn;
  clEnqueueReleaseDX9MediaSurfacesKHR_fn: TclEnqueueReleaseDX9MediaSurfacesKHR_fn;
{$ENDIF}

function InitCL_DX9MediaSharing: Boolean;

implementation

function InitCL_DX9MediaSharing: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then
  begin
    {$IFDEF CL_VERSION_1_2}
      clGetDeviceIDsForDX9MediaAdapterKHR_fn := TclGetDeviceIDsForDX9MediaAdapterKHR_fn(oclGetProcAddress('clGetDeviceIDsForDX9MediaAdapterKHR_fn', OCL_LibHandle));
      clCreateFromDX9MediaSurfaceKHR_fn := TclCreateFromDX9MediaSurfaceKHR_fn(oclGetProcAddress('clCreateFromDX9MediaSurfaceKHR_fn', OCL_LibHandle));
      clEnqueueAcquireDX9MediaSurfacesKHR_fn := TclEnqueueAcquireDX9MediaSurfacesKHR_fn(oclGetProcAddress('clEnqueueAcquireDX9MediaSurfacesKHR_fn', OCL_LibHandle));
      clEnqueueReleaseDX9MediaSurfacesKHR_fn := TclEnqueueReleaseDX9MediaSurfacesKHR_fn(oclGetProcAddress('clEnqueueReleaseDX9MediaSurfacesKHR_fn', OCL_LibHandle));
    {$ENDIF}
    Result := True;
  end;
end;

end.
