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
(*      file name       : CL_ext.pas            *)
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

(* cl_ext.h contains OpenCL extensions which don't have external *)
(* (OpenGL, D3D) dependencies.                                   *)

unit CL_Ext;

interface

{$INCLUDE OpenCL.inc}

uses
  CL,
  CL_Platform;

const
(* cl_khr_fp64 extension - no extension #define since it has no functions  *)
  CL_DEVICE_DOUBLE_FP_CONFIG                =  $1032;


(* cl_khr_fp16 extension - no extension #define since it has no functions  *)
  CL_DEVICE_HALF_FP_CONFIG                  =  $1033;


(* Memory object destruction
 *
 * Apple extension for use to manage externally allocated buffers used with cl_mem objects with CL_MEM_USE_HOST_PTR
 *
 * Registers a user callback function that will be called when the memory object is deleted and its resources 
 * freed. Each call to clSetMemObjectCallbackFn registers the specified user callback function on a callback 
 * stack associated with memobj. The registered user callback functions are called in the reverse order in 
 * which they were registered. The user callback functions are called and then the memory object is deleted 
 * and its resources freed. This provides a mechanism for the application (and libraries) using memobj to be 
 * notified when the memory referenced by host_ptr, specified when the memory object is created and used as 
 * the storage bits for the memory object, can be reused or freed.
 *
 * The application may not call CL api's with the cl_mem object passed to the pfn_notify.
 *
 * Please check for the "cl_APPLE_SetMemObjectDestructor" extension using clGetDeviceInfo(CL_DEVICE_EXTENSIONS)
 * before using.
 *)
const
  cl_APPLE_SetMemObjectDestructor = 1;
{$IFDEF CL_VERSION_1_0}
type
  TclSetMemObjectDestructorAPPLE = function(
                                        memobj: Tcl_mem; (* memobj *)
                                        pfn_notify: Pointer;(* /*pfn_notify*/)( cl_mem /* memobj */, void* /*user_data*/*)
                                        user_data: Pointer(*user_data *)
                                        ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clSetMemObjectDestructorAPPLE: TclSetMemObjectDestructorAPPLE;
{$ENDIF}

(* Context Logging Functions
 *
 * The next three convenience functions are intended to be used as the pfn_notify parameter to clCreateContext().
 * Please check for the "cl_APPLE_ContextLoggingFunctions" extension using clGetDeviceInfo(CL_DEVICE_EXTENSIONS)
 * before using.
 *
 * clLogMessagesToSystemLog fowards on all log messages to the Apple System Logger
 *)
const
  cl_APPLE_ContextLoggingFunctions = 1;
{$IFDEF CL_VERSION_1_0}
type
  TclLogMessagesToSystemLogAPPLE = function(
                                            const errstr: PAnsiChar;            (* errstr *)
                                            const private_info: Pointer;        (* private_info *)
                                            cb: TSize_t;                        (* cb *)
                                            user_data: Pointer                  (* user_data *)
                                            ): TCL_int;
                                            {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

(* clLogMessagesToStdout sends all log messages to the file descriptor stdout *)
  TclLogMessagesToStdoutAPPLE = function(
                                            const errstr: PAnsiChar;            (* errstr *)
                                            const private_info: Pointer;        (* private_info *)
                                            cb: TSize_t;                        (* cb *)
                                            user_data: Pointer                  (* user_data *)
                                            ): TCL_int;
                                            {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

(* clLogMessagesToStderr sends all log messages to the file descriptor stderr *)
  TclLogMessagesToStderrAPPLE = function(
                                          const errstr: PAnsiChar;            (* errstr *)
                                          const private_info: Pointer;        (* private_info *)
                                          cb: TSize_t;                        (* cb *)
                                          user_data: Pointer                  (* user_data *)
                                          ): TCL_int;
                                          {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clLogMessagesToSystemLogAPPLE: TclLogMessagesToSystemLogAPPLE;
  clLogMessagesToStdoutAPPLE: TclLogMessagesToStdoutAPPLE;
  clLogMessagesToStderrAPPLE: TclLogMessagesToStderrAPPLE;
{$ENDIF}

const
(* cl_khr_icd extension                                                    *)
  cl_khr_icd                                =  1;

(* cl_platform_info                                                        *)
  (*
    Accepted as <param_name> to the function clGetPlatformInfo
  *)
  CL_PLATFORM_ICD_SUFFIX_KHR                =  $0920;

(* Additional Error Codes                                                  *)
  (*
    Returned by clGetPlatformIDs when no platforms are found
  *)
  CL_PLATFORM_NOT_FOUND_KHR                 =  -1001;

type
  TclIcdGetPlatformIDsKHR = function (
                                num_entries: Tcl_uint;          (* num_entries *)
                                platforms: Pcl_platform_id;     (* platforms *)
                                num_platforms: Pcl_uint         (* num_platforms *)
                                ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


var
  clIcdGetPlatformIDsKHR: TclIcdGetPlatformIDsKHR;

(******************************************
* cl_nv_device_attribute_query extension *
******************************************)
(* cl_nv_device_attribute_query extension - no extension #define since it has no functions *)
const
  CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV      = $4000;
  CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV      = $4001;
  CL_DEVICE_REGISTERS_PER_BLOCK_NV           = $4002;
  CL_DEVICE_WARP_SIZE_NV                     = $4003;
  CL_DEVICE_GPU_OVERLAP_NV                   = $4004;
  CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV           = $4005;
  CL_DEVICE_INTEGRATED_MEMORY_NV             = $4006;


(*********************************
* cl_amd_device_attribute_query *
*********************************)
const
  (*
    Accepted as the <param_name> parameter of clGetDeviceInfo. Return the
    offset in nano-seconds between an event timestamp and Epoch.
  *)
  CL_DEVICE_PROFILING_TIMER_OFFSET_AMD       = $4036;

{$IFDEF CL_VERSION_1_1}
    (***********************************
    * cl_ext_device_fission extension *
    ***********************************)
  const
    cl_ext_device_fission   = 1;
    
  type
    (*
    clReleaseDeviceEXT decrements the <device> reference count. After the 
    reference count reaches zero, the object shall be destroyed and associated 
    resources released for reuse by the system. 
    
    clReleaseDeviceEXT returns CL_SUCCESS if the function is executed 
    successfully or the device is a root level device. It returns 
    CL_INVALID_DEVICE if the <device> is not a valid device.
    *)
    TclReleaseDeviceEXT = function( device: TCL_device_id (*device*) ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  var
    clReleaseDeviceEXT: TclReleaseDeviceEXT;
    (*
    clReleaseDeviceEXT returns CL_SUCCESS if the function is executed 
    successfully or the device is a root level device. It returns 
    CL_INVALID_DEVICE if the <device> is not a valid device.

    CAUTION: Since root level devices are generally returned by a clGet call 
    (clGetDeviceIDs) and not a clCreate call, the user generally does not own a 
    reference count for root level devices. The reference count attached to a 
    device retured from clGetDeviceIDs is owned by the implementation. 
    Developers need to be careful when releasing cl_device_ids to always balance
    clCreateSubDevicesEXT or clRetainDeviceEXT with each call to 
    clReleaseDeviceEXT for the device. By convention, software layers that own 
    a reference count should be themselves responsible for releasing it.
    *)
  type
    TclRetainDeviceEXT = function( device: TCL_device_id (*device*) ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  var
    clRetainDeviceEXT: TclRetainDeviceEXT;
  type
    Pcl_device_partition_property_ext = ^Tcl_device_partition_property_ext;
    Tcl_device_partition_property_ext = Tcl_ulong; //?typedef cl_bitfield cl_device_partition_property_ext;

    (*
    lCreateSubDevicesEXT creates an array of sub-devices that each reference a 
    nonintersecting set of compute units within <in_device>, according to a 
    partition scheme given by the <cl_device_partition_property_ext> list. The 
    output sub-devices may be used in every way that the root device can be 
    used, including building programs, further calls to clCreateSubDevicesEXT 
    and creating command queues. They may also be used within any context 
    created using the in_device or parent/ancestor thereof. When a command 
    queue is created against a sub-device, the commands enqueued on that queue 
    are executed only on the sub-device.

    in_device - The device to be partitioned 

    num_entries - The number of cl_device_ids that will fit in the array pointed
        to by <out_devices>. If <out_devices> is not NULL, <num_entries> must be
        greater than zero.  
    
    out_devices - On output, the array pointed to by <out_devices> will contain 
        up to <num_entries> sub-devices. If the <out_devices> argument is NULL, 
        it is ignored. The number of cl_device_ids returned is the minimum of 
        <num_entries> and the number of devices created by the partition scheme.
    
    num_devices - On output, the number of devices that the <in_device> may be 
        partitioned in to according to the partitioning scheme given by 
        <properties>. If num_devices is NULL, it is ignored.

    properties - A zero terminated list of device fission {property-value, 
        cl_int[]} pairs that describe how to partition the device into 
        sub-devices.  <properties> may not be NULL. Only one of 
        CL_DEVICE_PARTITION_EQUALLY_EXT, CL_DEVICE_PARTITION_BY_COUNTS_EXT, 
        CL_DEVICE_PARTITION_BY_NAMES_EXT or 
        CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT may be used in the same 
        properties list. Available properties are:

        CL_DEVICE_PARTITION_EQUALLY_EXT - Split the aggregate device into as 
            many smaller aggregate devices as can be created, each containing N
            compute units. The value N is passed as the value accompanying this 
            property.  If N does not divide evenly into 
            CL_DEVICE_MAX_COMPUTE_UNITS then the remaining compute units are 
            not used.

            Example: To divide a device containing 16 compute units into two 
            sub-devices, each containing 8 compute units, pass:

                { CL_DEVICE_PARTITION_EQUALLY_EXT, 8, 
                  CL_PROPERTIES_LIST_END_EXT }
    
        CL_DEVICE_PARTITION_BY COUNTS_EXT - This property is followed by a 
            CL_PARTITION_BY_COUNTS_LIST_END_EXT terminated list of compute unit 
            counts. For each non-zero count M in the list, a sub-device is 
            created with M compute units in it.  
            CL_PARTITION_BY_COUNTS_LIST_END_EXT is defined to be 0.

            Example: to split a four compute unit device into two sub-devices, 
            each containing two compute units, pass:

                { CL_DEVICE_PARTITION_BY_COUNTS_EXT, 
                  2, 2, CL_PARTITION_BY_COUNTS_LIST_END_EXT,
                  CL_PROPERTIES_LIST_END_EXT }

            The first 2 means put two compute units in the first sub-device. The
            second 2 means put two compute units in the second sub-device. 
            CL_PARTITION_BY_COUNTS_LIST_END_EXT terminates the list of 
            sub-devices.  CL_PROPERTIES_LIST_END_EXT terminates the list of 
            properties.  The total number of compute units specified may not 
            exceed the number of compute units in the device.

        CL_DEVICE_PARTITION_BY NAMES_EXT - This property is followed by a list 
            of compute unit names. Each list starts with a 
            CL_PARTITION_BY_NAMES_LIST_END_EXT terminated list of compute unit 
            names.  Compute unit names are integers that count up from zero to 
            the number of compute units less one. 
            CL_PARTITION_BY_NAMES_LIST_END_EXT is defined to be -1. Only 
            one sub-device may be created at a time with this selector. An 
            individual compute unit name may not appear more than once in the 
            sub-device description.

            Example: To create a three compute unit sub-device using compute 
            units, { 0, 1, 3 }, pass:
    
                { CL_DEVICE_PARTITION_BY NAMES_EXT,
                  0, 1, 3, CL_PARTITION_BY_NAMES_LIST_END_EXT,
                  CL_PROPERTIES_LIST_END_EXT }

            The meaning of these numbers are, in order:
              0 the name of the first compute unit in the sub-device
              1 the name of the second compute unit in the sub-device
              3 the name of the third compute unit in the sub-device
              CL_PROPERTIES_LIST_END_EXT list terminator for the list of 
                  properties

        CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT - Split the device into 
            smaller aggregate devices containing one or more compute units 
            that all share part of a cache hierarchy. The value accompanying 
            this property may be drawn from the following CL_AFFINITY_DOMAIN 
            list: 
        
            CL_AFFINITY_DOMAIN_NUMA_EXT - Split the device into sub-devices 
                comprised of compute units that share a NUMA band.

            CL_AFFINITY_DOMAIN_L4_CACHE_EXT - Split the device into sub-devices 
                comprised of compute units that share a level 4 data cache.

            CL_AFFINITY_DOMAIN_L3_CACHE_EXT - Split the device into sub-devices 
                comprised of compute units that share a level 3 data cache.

            CL_AFFINITY_DOMAIN_L2_CACHE_EXT - Split the device into sub-devices 
                comprised of compute units that share a level 2 data cache.

            CL_AFFINITY_DOMAIN_L1_CACHE_EXT - Split the device into sub-devices 
                comprised of compute units that share a level 1 data cache.

            CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE_EXT - Split the device along the
                next fissionable CL_AFFINITY_DOMAIN.  The implementation shall 
                find the first level along which the device or sub-device may be
                further subdivided in the order NUMA, L4, L3, L2, L1, and 
                fission the device into sub-devices comprised of compute units 
                that share memory sub-systems at this level. The user may 
                determine what happened by calling 
                clGetDeviceInfo(CL_DEVICE_PARTITION_STYLE_EXT) on the 
                sub-devices.

            Example: To split a non-NUMA device along the outermost cache level 
            (if any), pass:

                { CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT, 
                  CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE_EXT, 
                  CL_PROPERTIES_LIST_END_EXT }

        CL_PROPERTIES_LIST_END_EXT - A list terminator for a properties list.


        The following values may be returned by clCreateSubDevicesEXT:
    
    CL_SUCCESS - The command succeeded.

    CL_INVALID_VALUE - The properties key is unknown, or the indicated partition
        style (CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT, 
        CL_DEVICE_PARTITION_EQUALLY_EXT, CL_DEVICE_PARTITION_BY NAMES_EXT or 
        CL_DEVICE_PARTITION_BY COUNTS_EXT) is not supported for this device by 
        the implementation. On an OpenCL 1.1 implementation, these cases return 
        CL_INVALID_PROPERTY instead, to be consistent with clCreateContext 
        behavior.

    CL_INVALID_VALUE - num_entries is zero and out_devices is not NULL, or both 
        out_devices and num_devices are NULL.

    CL_DEVICE_PARTITION_FAILED_EXT - The indicated partition scheme is supported
        by the implementation, but the implementation can not further partition 
        the device in this way. For example, 
        CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT was requested, but all 
        compute units in in_device share the same cache at the level requested.

    CL_INVALID_PARTITION_COUNT_EXT - The total number of compute units requested
        exceeds CL_DEVICE_MAX_COMPUTE_UNITS, or the number of compute units for 
        any one sub-device is less than 1, or the number of sub-devices 
        requested exceeds CL_DEVICE_MAX_COMPUTE_UNITS.

    CL_INVALID_PARTITION_NAME_EXT - A compute unit name appearing in a name list
        following CL_DEVICE_PARTITION_BY NAMES_EXT is not in the range 
        [-1, number of compute units - 1].

    CL_INVALID_DEVICE - The in_device is not a valid device. The in_device is 
        not a device in context.
    *)
    TclCreateSubDevicesEXT = function(
                            in_device: TCL_device_id; (*in_device*)
                            const properties: Pcl_device_partition_property_ext; (* properties *)
                            num_entries: TCL_uint; (*num_entries*)
                            out_devices: PCL_device_id; (*out_devices*)
                            num_devices:  TCL_uint(*num_devices*)
                            ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};
    var
      clCreateSubDevicesEXT: TclCreateSubDevicesEXT;


  const

    (*
    Accepted as a property name in the <properties> parameter of
    clCreateSubDeviceEXT:
    *)
    (* cl_device_partition_property_ext *)
      CL_DEVICE_PARTITION_EQUALLY_EXT             = $4050;
      CL_DEVICE_PARTITION_BY_COUNTS_EXT           = $4051;
      CL_DEVICE_PARTITION_BY_NAMES_EXT            = $4052;
      CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT  = $4053;
      
    (* clDeviceGetInfo selectors *)
    (*
    Accepted as a property being queried in the <param_name> argument of
    clGetDeviceInfo:

    clGetDeviceInfo - If the device is a sub-device created by 
        clCreateSubDevicesEXT, then the value returned for 
        CL_DEVICE_MAX_COMPUTE_UNITS is the number of compute units in the 
        sub-device. The CL_DEVICE_VENDOR_ID may be different from the parent 
        device CL_DEVICE_VENDOR_ID, but should be the same for all devices and 
        sub-devices that can share a binary executable, such as that returned 
        from clGetProgramInfo(CL_PROGRAM_BINARIES). Other selectors such as 
        CL_DEVICE_GLOBAL_MEM_CACHE_SIZE may optionally change value to better 
        reflect the behavior of the sub-device in an implementation defined 
        manner.
    The following selectors are added for clGetDeviceInfo:
    *)

    (*
      CL_DEVICE_PARENT_DEVICE_EXT - a selector to get the cl_device_id for 
      the parent cl_device_id to which the sub-device belongs.
      (Sub-division can be multi-level.) If the device is a root level
      device, then it will return NULL.
    *)
      CL_DEVICE_PARENT_DEVICE_EXT                 = $4054;
    (*
      CL_DEVICE_PARTITION_TYPES_EXT - a selector to get a list of supported
      partition types for partitioning a device. The return type is an
      array of cl_device partition property ext values drawn from the
      following list:

      CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT
      CL_DEVICE_PARTITION_BY COUNTS_EXT
      CL_DEVICE_PARTITION_BY NAMES_EXT
      CL_DEVICE_PARTITION_EQUALLY_EXT

      The implementation shall return at least one property from the above
      list.  However, when a partition style is found within this list,
      the partition style is not required to work in every case. For
      example, a device might support partitioning by affinity domain, but
      not along NUMA domains.
    *)
      CL_DEVICE_PARTITION_TYPES_EXT               = $4055;

    (*
      CL_DEVICE_AFFINITY_DOMAINS_EXT - a selector to get a list of supported
      affinity domains for partitioning the device using the
      CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN_EXT partition style. The
      return type is an array of cl_device_partition_property_ext values.
      The values shall come from the list:

      CL_AFFINITY_DOMAIN_L1_CACHE_EXT
      CL_AFFINITY_DOMAIN_L2_CACHE_EXT
      CL_AFFINITY_DOMAIN_L3_CACHE_EXT
      CL_AFFINITY_DOMAIN_L4_CACHE_EXT
      CL_AFFINITY_DOMAIN_NUMA_EXT

      If no partition style is supported then the size of the returned
      array is zero.  Even though a device has a NUMA, or particular
      cache level, an implementation may elect not to provide fissioning
      at that level.
    *)
      CL_DEVICE_AFFINITY_DOMAINS_EXT              = $4056;
    (*
      CL_DEVICE_REFERENCE_COUNT_EXT Return the device 
      reference count.  The return type is cl_uint. If the device is a
      root level device, a reference count of 1 is returned.
    *)
      CL_DEVICE_REFERENCE_COUNT_EXT               = $4057;
    (*
      CL_DEVICE_PARTITION_STYLE_EXT - a selector to get the 
      cl_device_partition_property_ext list used to create the sub-device.
      If the device is a root level device then a list consisting of
      { CL_PROPERTIES_LIST_END_EXT} is returned. If the property on device
      creation was (CL_DEVICE_PARTITION BY_AFFINITY_DOMAIN_EXT,
      CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE) then
      CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE will be replaced by the symbol
      representing the actual CL_AFFINITY DOMAIN used
      (e.g. CL_AFFINITY_DOMAIN_NUMA). The returned value is an array of 
      cl_device_partition_property_ext. The length of the array is
      obtained from the size returned by the param size value ret
      parameter to the function.
    *)
      CL_DEVICE_PARTITION_STYLE_EXT               = $4058;
    
    (* error codes *)
    (*
      Returned by clCreateSubDevicesEXT when the indicated partition scheme is
      supported by the implementation, but the implementation can not further
      partition the device in this way.
    *)
      CL_DEVICE_PARTITION_FAILED_EXT              =-1057;
    (*
      Returned by clCreateSubDevicesEXT when the total number of compute units
      requested exceeds CL_DEVICE_MAX_COMPUTE_UNITS, or the number of compute
      units for any one sub-device is less than 1.
    *)
      CL_INVALID_PARTITION_COUNT_EXT              =-1058;
    (*
      Returned by clCreateSubDevicesEXT when a compute unit name appearing in a
      name list following CL_DEVICE_PARTITION_BY_NAMES_EXT is not in range.
    *)
      CL_INVALID_PARTITION_NAME_EXT               =-1059;

    (*
      Accepted as a property name, when accompanying the
      CL_DEVICE_PARITION_BY_AFFINITY_DOMAIN_EXT property, in the <properties>
      parameter of clCreateSubDeviceEXT:
    *)
    (* CL_AFFINITY_DOMAINs *)
      CL_AFFINITY_DOMAIN_L1_CACHE_EXT             = $1;
      CL_AFFINITY_DOMAIN_L2_CACHE_EXT             = $2;
      CL_AFFINITY_DOMAIN_L3_CACHE_EXT             = $3;
      CL_AFFINITY_DOMAIN_L4_CACHE_EXT             = $4;
      CL_AFFINITY_DOMAIN_NUMA_EXT                 = $10;
      CL_AFFINITY_DOMAIN_NEXT_FISSIONABLE_EXT     = $100;


    (* cl_device_partition_property_ext list terminators *)
    
      (*
        Accepted as the property list terminator in the <properties> parameter of
        clCreateSubDeviceEXT:
      *)
      CL_PROPERTIES_LIST_END_EXT                  : Tcl_device_partition_property_ext = (0);

      (*
        Accepted as the partition counts list terminator in the <properties>
        parameter of clCreateSubDeviceEXT:
      *)
      CL_PARTITION_BY_COUNTS_LIST_END_EXT         : Tcl_device_partition_property_ext = (0);

      (*
        Accepted as the partition names list terminator in the <properties>
        parameter of clCreateSubDeviceEXT:
      *)
      CL_PARTITION_BY_NAMES_LIST_END_EXT          : Tcl_device_partition_property_ext = Tcl_device_partition_property_ext(-1);

    (* cl_ext_atomic_counters_32 and cl_ext_atomic_counters_64 extensions
     * no extension #define since they have no functions                                              
     *)
      CL_DEVICE_MAX_ATOMIC_COUNTERS_EXT           = $4032;

{$ENDIF} (*CL_VERSION_1_1*)
{$IFDEF CL_VERSION_1_0}
    (***********************************
     * cl_ext_migrate_memobject extension definitions
     ***********************************)
      cl_ext_migrate_memobject = 1;

  type

    Pcl_mem_migration_flags_ext = ^Tcl_mem_migration_flags_ext;
    Tcl_mem_migration_flags_ext = TCL_bitfield;

  const
    (*
      Besides a value of zero, the following cl_mem_migration_flags_ext values are
      allowed:
    *)
    CL_MIGRATE_MEM_OBJECT_HOST_EXT              = $1;
    (*
      Returned in the <param_value> parameter of the clGetEventInfo when
      <param_name> is CL_EVENT_COMMAND_TYPE:
    *)
    CL_COMMAND_MIGRATE_MEM_OBJECT_EXT           = $4040;
    
  type
    TclEnqueueMigrateMemObjectEXT = function(
                                  command_queue: Tcl_command_queue;   (* command_queue *)
                                  num_mem_objects: Tcl_uint;          (* num_mem_objects *)
                                  const mem_objects: Pcl_mem;         (* mem_objects *)
                                  flags: Tcl_mem_migration_flags_ext; (* flags *)
                                  num_events_in_wait_list: Tcl_uint;  (* num_events_in_wait_list *)
                                  const event_wait_list: Pcl_event;   (* event_wait_list *)
                                  event: Pcl_event                    (* event *)
                                  ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  var
    clEnqueueMigrateMemObjectEXT: TclEnqueueMigrateMemObjectEXT;
{$ENDIF}

{$IFDEF CL_VERSION_1_1}
(*********************************
* cl_qcom_ext_host_ptr extension
*********************************)

const

  CL_MEM_EXT_HOST_PTR_QCOM                  = (1 shl 29);

  CL_DEVICE_EXT_MEM_PADDING_IN_BYTES_QCOM   = $40A0;
  CL_DEVICE_PAGE_SIZE_QCOM                  = $40A1;
  CL_IMAGE_ROW_ALIGNMENT_QCOM               = $40A2;
  CL_IMAGE_SLICE_ALIGNMENT_QCOM             = $40A3;
  CL_MEM_HOST_UNCACHED_QCOM                 = $40A4;
  CL_MEM_HOST_WRITEBACK_QCOM                = $40A5;
  CL_MEM_HOST_WRITETHROUGH_QCOM             = $40A6;
  CL_MEM_HOST_WRITE_COMBINING_QCOM          = $40A7;

type
  TCL_image_pitch_info_qcom = TCL_uint;
  PCL_image_pitch_info_qcom = ^TCL_image_pitch_info_qcom;


TclGetDeviceImageInfoQCOM = function(
                         device: PCL_device_id;
                         image_width: TSize_t;
                         image_height: TSize_t;
                         const image_format: PCL_image_format;
                         param_name: PCL_image_pitch_info_qcom;
                         param_value_size: TSize_t;
                         param_value: Pointer;
                         param_value_size_ret: PSize_t
                         ): TCL_int;
                         {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

var
  clGetDeviceImageInfoQCOM: TclGetDeviceImageInfoQCOM;

type

TCL_mem_ext_host_ptr = packed record
  (* Type of external memory allocation. *)
  (* Legal values will be defined in layered extensions. *)
  allocation_type: TCL_uint;
  (* Host cache policy for this external memory allocation. *)
  host_cache_policy: TCL_uint;
end;
PCL_mem_ext_host_ptr = ^TCL_mem_ext_host_ptr;

(*********************************
* cl_qcom_ion_host_ptr extension
*********************************)
const
  CL_MEM_ION_HOST_PTR_QCOM                  = $40A8;

type
TCL_mem_ion_host_ptr = packed record
  (* Type of external memory allocation. *)
  (* Must be CL_MEM_ION_HOST_PTR_QCOM for ION allocations. *)
  ext_host_ptr: PCL_mem_ext_host_ptr;
  (* ION file descriptor *)
  ion_filedesc: Integer;
  (* Host pointer to the ION allocated memory *)
  ion_hostptr: Pointer;
end;

PCL_mem_ion_host_ptr = ^TCL_mem_ion_host_ptr;

{$ENDIF}



function InitCL_EXT: Boolean;

implementation

function InitCL_EXT: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then
  begin

    //clIcdGetPlatformIDsKHR := oclGetProcAddress('clIcdGetPlatformIDsKHR', OCL_LibHandle));
    clIcdGetPlatformIDsKHR := TclIcdGetPlatformIDsKHR(oclGetProcAddress('clIcdGetPlatformIDsKHR', OCL_LibHandle));

    {$IFDEF CL_VERSION_1_0}
      clSetMemObjectDestructorAPPLE := TclSetMemObjectDestructorAPPLE(oclGetProcAddress('clSetMemObjectDestructorAPPLE', OCL_LibHandle));
      clLogMessagesToSystemLogAPPLE := TclLogMessagesToSystemLogAPPLE(oclGetProcAddress('clLogMessagesToSystemLogAPPLE', OCL_LibHandle));
      clLogMessagesToStdoutAPPLE := TclLogMessagesToStdoutAPPLE(oclGetProcAddress('clLogMessagesToStdoutAPPLE', OCL_LibHandle));
      clLogMessagesToStderrAPPLE := TclLogMessagesToStderrAPPLE(oclGetProcAddress('clLogMessagesToStderrAPPLE', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_1}
      clReleaseDeviceEXT := TclReleaseDeviceEXT(oclGetProcAddress('clReleaseDeviceEXT', OCL_LibHandle));
      clRetainDeviceEXT := TclRetainDeviceEXT(oclGetProcAddress('clRetainDeviceEXT', OCL_LibHandle));
      clCreateSubDevicesEXT := TclCreateSubDevicesEXT(oclGetProcAddress('clCreateSubDevicesEXT', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_0}
      clEnqueueMigrateMemObjectEXT := TclEnqueueMigrateMemObjectEXT(oclGetProcAddress('clEnqueueMigrateMemObjectEXT', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
       clGetDeviceImageInfoQCOM := TclGetDeviceImageInfoQCOM(oclGetProcAddress('clGetDeviceImageInfoQCOM', OCL_LibHandle));
    {$ENDIF}

    Result := True;
  end;
end;

end.
