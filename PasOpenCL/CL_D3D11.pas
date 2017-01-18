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
(*      file name       : CL_d3d11.pas          *)
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
unit CL_D3D11;

interface

{$INCLUDE OpenCL.inc}

uses
  CL_Platform,
  CL;

(******************************************************************************
 * cl_khr_d3d11_sharing                                                       *)
const
  cl_khr_d3d11_sharing = 1;

type
  TCL_d3d11_device_source_khr = TCL_uint;
  PCL_d3d11_device_source_khr = ^TCL_d3d11_device_source_khr;

  TCL_d3d11_device_set_khr = TCL_uint;
  PCL_d3d11_device_set_khr = ^TCL_d3d11_device_set_khr;

(******************************************************************************)
const
// Error Codes
  CL_INVALID_D3D11_DEVICE_KHR                  = -1006;
  CL_INVALID_D3D11_RESOURCE_KHR                = -1007;
  CL_D3D11_RESOURCE_ALREADY_ACQUIRED_KHR       = -1008;
  CL_D3D11_RESOURCE_NOT_ACQUIRED_KHR           = -1009;

// cl_d3d11_device_source
  CL_D3D11_DEVICE_KHR                          = $4019;
  CL_D3D11_DXGI_ADAPTER_KHR                    = $401A;

// cl_d3d11_device_set
  CL_PREFERRED_DEVICES_FOR_D3D11_KHR           = $401B;
  CL_ALL_DEVICES_FOR_D3D11_KHR                 = $401C;

// cl_context_info
  CL_CONTEXT_D3D11_DEVICE_KHR                  = $401D;
  CL_CONTEXT_D3D11_PREFER_SHARED_RESOURCES_KHR = $402D;

// cl_mem_info
  CL_MEM_D3D11_RESOURCE_KHR                    = $401E;

// cl_image_info
  CL_IMAGE_D3D11_SUBRESOURCE_KHR               = $401F;

// cl_command_type
  CL_COMMAND_ACQUIRE_D3D11_OBJECTS_KHR         = $4020;
  CL_COMMAND_RELEASE_D3D11_OBJECTS_KHR         = $4021;

(******************************************************************************)


{$IFDEF CL_VERSION_1_0}
type
  TclGetDeviceIDsFromD3D11KHR_fn = function (
    cl_platform_id             platform,
    cl_d3d11_device_source_khr d3d_device_source,
    void *                     d3d_object,
    cl_d3d11_device_set_khr    d3d_device_set,
    cl_uint                    num_entries,
    cl_device_id *             devices,
    cl_uint *                  num_devices
    ): TCL_int;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclCreateFromD3D11BufferKHR_fn = function (
    cl_context     context,
    cl_mem_flags   flags,
    ID3D11Buffer * resource,
    cl_int *       errcode_ret
    ): PCL_mem;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclCreateFromD3D11Texture2DKHR_fn = function (
    cl_context        context,
    cl_mem_flags      flags,
    ID3D11Texture2D * resource,
    UINT              subresource,
    cl_int *          errcode_ret
    ): PCL_mem;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclCreateFromD3D11Texture3DKHR_fn = function (
    cl_context        context,
    cl_mem_flags      flags,
    ID3D11Texture3D * resource,
    UINT              subresource,
    cl_int *          errcode_ret
    ): PCL_mem;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclEnqueueAcquireD3D11ObjectsKHR_fn = function (
    cl_command_queue command_queue,
    cl_uint          num_objects,
    const cl_mem *   mem_objects,
    cl_uint          num_events_in_wait_list,
    const cl_event * event_wait_list,
    cl_event *       event
    ): TCL_int;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclEnqueueReleaseD3D11ObjectsKHR_fn = function (
    cl_command_queue command_queue,
    cl_uint          num_objects,
    const cl_mem *   mem_objects,
    cl_uint          num_events_in_wait_list,
    const cl_event * event_wait_list,
    cl_event *       event
    ): TCL_int;
                                       {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
{$ENDIF}

implementation

end.