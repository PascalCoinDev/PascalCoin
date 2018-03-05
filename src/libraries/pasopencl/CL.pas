(*******************************************************************************
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
(*      file name       : CL.pas                                        *)
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

//OpenCL 1.0 for Delphi 7-2010
//Fixed By Dmitry Belkevich
//Site www.makhaon.com
//E-mail dmitry@makhaon.com
//(c) 2009
//Beta release 1.0

unit CL;

interface

{$INCLUDE OpenCL.inc}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  CL_Platform;

{$IFDEF WINDOWS}
const
  OpenCLLibName = 'OpenCL.dll';
{$ENDIF}
{$IFDEF LINUX}
const
  OpenCLLibName = 'libOpenCL.so';
{$ENDIF}
{$IFDEF DARWIN}
{$IFDEF DYNLINK}
const
  OpenCLLibName = '/usr/lib/libOpenCL.dylib';
{$ELSE}
{$linkframework OpenCL}
{$ENDIF}
{$ENDIF}

{$IFDEF DEFINE_8087CW_NOT_IMPLEMENTED}
var
  Default8087CW: Word = $133F;
{$ENDIF}

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'types'}{$ENDIF}
type
  TSize_t = Longword;
  PSize_t = ^TSize_t;

  PIntPtr_t = ^TIntPtr_t;
  TIntPtr_t = ^Integer;

  TSizet = array [0..2] of TSize_t;
  PSizet = ^TSizet;

  TCL_platform_id = record
  end;  
  PCL_platform_id = ^TCL_platform_id;
  PPCL_platform_id = ^PCL_platform_id;

  TCL_device_id = record
  end;
  PCL_device_id = ^TCL_device_id;
  PPCL_device_id = ^PCL_device_id;

  TCL_context = record
  end;
  PCL_context = ^TCL_context;

  TCL_command_queue = record
  end;
  PCL_command_queue = ^TCL_command_queue;

  TCL_mem = record
  end;
  PCL_mem = ^TCL_mem;
  PPCL_mem = ^PCL_mem;

  TCL_program = record
  end;
  PCL_program = ^TCL_program;
  PPCL_program = ^PCL_program;

  TCL_kernel = record
  end;
  PCL_kernel = ^TCL_kernel;
  PPCL_kernel = ^PCL_kernel;

  TCL_event = record
  end;
  PCL_event = ^TCL_event;
  PPCL_event = ^PCL_event;

  TCL_sampler = record
  end;
  PCL_sampler = ^TCL_sampler;

  TCL_bool = TCL_uint; (* WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels. *)
  PCL_bool = ^TCL_bool;

  TCL_bitfield = TCL_ulong;
  PCL_bitfield = ^TCL_bitfield;

  TCL_device_type = TCL_bitfield;
  PCL_device_type = ^TCL_device_type;

  TCL_platform_info = TCL_uint;
  PCL_platform_info = ^TCL_platform_info;

  TCL_device_info = TCL_uint;
  PCL_device_info = ^TCL_device_info;

  TCL_device_address_info = TCL_bitfield;
  PCL_device_address_info = ^TCL_device_address_info;

  TCL_device_fp_config = TCL_bitfield;
  PCL_device_fp_config = ^TCL_device_fp_config;

  TCL_device_mem_cache_type = TCL_uint;
  PCL_device_mem_cache_type = ^TCL_device_mem_cache_type;

  TCL_device_local_mem_type = TCL_uint;
  PCL_device_local_mem_type = ^TCL_device_local_mem_type;

  TCL_device_exec_capabilities = TCL_bitfield;
  PCL_device_exec_capabilities = ^TCL_device_exec_capabilities;

  TCL_command_queue_properties = TCL_bitfield;
  PCL_command_queue_properties = ^TCL_command_queue_properties;

  PCL_device_partition_property = ^TCL_device_partition_property;
  TCL_device_partition_property = TIntPtr_t;  //intptr_t

  PCL_device_affinity_domain = ^TCL_device_affinity_domain;
  TCL_device_affinity_domain = TCL_bitfield;

  PCL_context_properties = PInteger;
  PPCL_context_properties = ^PCL_context_properties;

  TCL_context_info = TCL_uint;
  PCL_context_info = ^TCL_context_info;

  TCL_queue_properties = TCL_uint;
  PCL_queue_properties = ^TCL_queue_properties;

  TCL_command_queue_info = TCL_uint;
  PCL_command_queue_info = ^TCL_command_queue_info;

  TCL_channel_order = TCL_uint;
  PCL_channel_order = ^TCL_channel_order;

  TCL_channel_type = TCL_uint;
  PCL_channel_type = ^TCL_channel_type;

  TCL_mem_flags = TCL_bitfield;
  PCL_mem_flags = ^TCL_mem_flags;

  TCL_svm_mem_flags = TCL_bitfield;
  PCL_svm_mem_flags = ^TCL_svm_mem_flags;

  TCL_mem_object_type = TCL_uint;
  PCL_mem_object_type = ^TCL_mem_object_type;

  TCL_mem_info = TCL_uint;
  PCL_mem_info = ^TCL_mem_info;

  TCL_mem_migration_flags = TCL_bitfield;
  PCL_mem_migration_flags = ^TCL_mem_migration_flags;

  TCL_image_info = TCL_uint;
  PCL_image_info = ^TCL_image_info;

  TCL_buffer_create_type = TCL_uint;
  PCL_buffer_create_type = ^TCL_buffer_create_type;

  TCL_addressing_mode = TCL_uint;
  PCL_addressing_mode = ^TCL_addressing_mode;

  TCL_filter_mode = TCL_uint;
  PCL_filter_mode = ^TCL_filter_mode;

  TCL_sampler_info = TCL_uint;
  PCL_sampler_info = ^TCL_sampler_info;

  TCL_map_flags = TCL_bitfield;
  PCL_map_flags = ^TCL_map_flags;

  TCL_pipe_properties = TIntPtr_t;  //intptr_t
  PCL_pipe_properties = ^TCL_pipe_properties;

  TCL_pipe_info = TCL_uint;
  PCL_pipe_info = TCL_pipe_info;

  TCL_program_info = TCL_uint;
  PCL_program_info = ^TCL_program_info;

  TCL_program_build_info = TCL_uint;
  PCL_program_build_info = ^TCL_program_build_info;

  TCL_build_status = TCL_int;
  PCL_build_status = ^TCL_build_status;

  TCL_kernel_info = TCL_uint;
  PCL_kernel_info = ^TCL_kernel_info;

  TCL_kernel_arg_info = TCL_uint;
  PCL_kernel_arg_info = ^TCL_kernel_arg_info;

  TCL__kernel_arg_address_qualifier = TCL_uint;
  PCL__kernel_arg_address_qualifier = ^TCL__kernel_arg_address_qualifier;

  TCL__kernel_arg_access_qualifier = TCL_uint;
  PCL__kernel_arg_access_qualifier = ^TCL__kernel_arg_access_qualifier;

  TCL_kernel_work_group_info = TCL_uint;
  PCL_kernel_work_group_info = ^TCL_kernel_work_group_info;

  TCL_event_info = TCL_uint;
  PCL_event_info = ^TCL_event_info;

  TCL_command_type = TCL_uint;
  PCL_command_type = ^TCL_command_type;

  TCL_profiling_info = TCL_uint;
  PCL_profiling_info = ^TCL_profiling_info;

  TCL_sampler_properties = TCL_bitfield;
  PCL_sampler_properties = ^TCL_sampler_properties;

  TCL_kernel_exec_info = TCL_uint;
  PCL_kernel_exec_info = ^TCL_kernel_exec_info;

  TCL_image_format = packed record
    Image_channel_order: TCL_channel_order;
    Image_channel_data_type: TCl_channel_type;
  end;
  PCL_image_format = ^TCL_image_format;

  TCL_image_desc = packed record
    image_type: TCL_mem_object_type;
    image_width,
    image_height,
    image_depth,
    image_array_size,
    image_row_pitch,
    image_slice_pitch: TSize_t;
    num_mip_levels: TCL_uint;
    num_samples: TCL_uint;
    buffer: Pcl_mem;
  end;
  PCL_image_desc = ^TCL_image_desc;

  PCL_buffer_region = ^TCL_buffer_region;
  TCL_buffer_region = packed record
    origin: TSize_t;
    size: TSize_t;
  end;
{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}

//******************************************************************************/
{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'const'}{$ENDIF}
const
  (* Error Codes *)
  CL_SUCCESS                                   = 0;
  CL_DEVICE_NOT_FOUND                          = -1;
  CL_DEVICE_NOT_AVAILABLE                      = -2;
  CL_COMPILER_NOT_AVAILABLE                    = -3;
  CL_MEM_OBJECT_ALLOCATION_FAILURE             = -4;
  CL_OUT_OF_RESOURCES                          = -5;
  CL_OUT_OF_HOST_MEMORY                        = -6;
  CL_PROFILING_INFO_NOT_AVAILABLE              = -7;
  CL_MEM_COPY_OVERLAP                          = -8;
  CL_IMAGE_FORMAT_MISMATCH                     = -9;
  CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10;
  CL_BUILD_PROGRAM_FAILURE                     = -11;
  CL_MAP_FAILURE                               = -12;
  CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13;
  CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14;
  CL_COMPILE_PROGRAM_FAILURE                   = -15;
  CL_LINKER_NOT_AVAILABLE                      = -16;
  CL_LINK_PROGRAM_FAILURE                      = -17;
  CL_DEVICE_PARTITION_FAILED                   = -18;
  CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19;

  CL_INVALID_VALUE                             = -30;
  CL_INVALID_DEVICE_TYPE                       = -31;
  CL_INVALID_PLATFORM                          = -32;
  CL_INVALID_DEVICE                            = -33;
  CL_INVALID_CONTEXT                           = -34;
  CL_INVALID_QUEUE_PROPERTIES                  = -35;
  CL_INVALID_COMMAND_QUEUE                     = -36;
  CL_INVALID_HOST_PTR                          = -37;
  CL_INVALID_MEM_OBJECT                        = -38;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           = -39;
  CL_INVALID_IMAGE_SIZE                        = -40;
  CL_INVALID_SAMPLER                           = -41;
  CL_INVALID_BINARY                            = -42;
  CL_INVALID_BUILD_OPTIONS                     = -43;
  CL_INVALID_PROGRAM                           = -44;
  CL_INVALID_PROGRAM_EXECUTABLE                = -45;
  CL_INVALID_KERNEL_NAME                       = -46;
  CL_INVALID_KERNEL_DEFINITION                 = -47;
  CL_INVALID_KERNEL                            = -48;
  CL_INVALID_ARG_INDEX                         = -49;
  CL_INVALID_ARG_VALUE                         = -50;
  CL_INVALID_ARG_SIZE                          = -51;
  CL_INVALID_KERNEL_ARGS                       = -52;
  CL_INVALID_WORK_DIMENSION                    = -53;
  CL_INVALID_WORK_GROUP_SIZE                   = -54;
  CL_INVALID_WORK_ITEM_SIZE                    = -55;
  CL_INVALID_GLOBAL_OFFSET                     = -56;
  CL_INVALID_EVENT_WAIT_LIST                   = -57;
  CL_INVALID_EVENT                             = -58;
  CL_INVALID_OPERATION                         = -59;
  CL_INVALID_GL_OBJECT                         = -60;
  CL_INVALID_BUFFER_SIZE                       = -61;
  CL_INVALID_MIP_LEVEL                         = -62;
  CL_INVALID_GLOBAL_WORK_SIZE                  = -63;
  CL_INVALID_PROPERTY                          = -64;
  CL_INVALID_IMAGE_DESCRIPTOR                  = -65;
  CL_INVALID_COMPILER_OPTIONS                  = -66;
  CL_INVALID_LINKER_OPTIONS                    = -67;
  CL_INVALID_DEVICE_PARTITION_COUNT            = -68;
  CL_INVALID_PIPE_SIZE                         = -69;
  CL_INVALID_DEVICE_QUEUE                      = -70;

  (* OpenCL Version *)
  CL_VERSION_1_0                               = 1;
  CL_VERSION_1_1                               = 1;
  CL_VERSION_1_2                               = 1;
  CL_VERSION_2_0                               = 1;

  (* cl_bool *)
  CL_FALSE                                     = 0;
  CL_TRUE                                      = 1;
  CL_BLOCKING                                  = CL_TRUE;
  CL_NON_BLOCKING                              = CL_FALSE;

  (* cl_platform_info *)
  CL_PLATFORM_PROFILE                          = $0900;
  CL_PLATFORM_VERSION                          = $0901;
  CL_PLATFORM_NAME                             = $0902;
  CL_PLATFORM_VENDOR                           = $0903;
  CL_PLATFORM_EXTENSIONS                       = $0904;

  (* cl_device_type - bitfield *)
  CL_DEVICE_TYPE_DEFAULT                       = (1 shl 0);
  CL_DEVICE_TYPE_CPU                           = (1 shl 1);
  CL_DEVICE_TYPE_GPU                           = (1 shl 2);
  CL_DEVICE_TYPE_ACCELERATOR                   = (1 shl 3);
  CL_DEVICE_TYPE_CUSTOM                        = (1 shl 4);
  CL_DEVICE_TYPE_ALL                           = $FFFFFFFF;

  (* cl_device_info *)
  CL_DEVICE_TYPE                               = $1000;
  CL_DEVICE_VENDOR_ID                          = $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS                  = $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS           = $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE                = $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES                = $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR        = $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT       = $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT         = $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG        = $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT       = $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE      = $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY                = $100C;
  CL_DEVICE_ADDRESS_BITS                       = $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS                = $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS               = $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE                 = $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH                  = $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT                 = $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH                  = $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT                 = $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH                  = $1015;
  CL_DEVICE_IMAGE_SUPPORT                      = $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE                 = $1017;
  CL_DEVICE_MAX_SAMPLERS                       = $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN                = $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE           = $101A;
  CL_DEVICE_SINGLE_FP_CONFIG                   = $101B;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE              = $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE          = $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE              = $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE                    = $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE           = $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS                  = $1021;
  CL_DEVICE_LOCAL_MEM_TYPE                     = $1022;
  CL_DEVICE_LOCAL_MEM_SIZE                     = $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT           = $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION         = $1025;
  CL_DEVICE_ENDIAN_LITTLE                      = $1026;
  CL_DEVICE_AVAILABLE                          = $1027;
  CL_DEVICE_COMPILER_AVAILABLE                 = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES             = $1029;
  CL_DEVICE_QUEUE_PROPERTIES                   = $102A;
  CL_DEVICE_NAME                               = $102B;
  CL_DEVICE_VENDOR                             = $102C;
  CL_DRIVER_VERSION                            = $102D;
  CL_DEVICE_PROFILE                            = $102E;
  CL_DEVICE_VERSION                            = $102F;
  CL_DEVICE_EXTENSIONS                         = $1030;
  CL_DEVICE_PLATFORM                           = $1031;
  CL_DEVICE_DOUBLE_FP_CONFIG                   = $1032;

  (* 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG *)
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF        = $1034;
  CL_DEVICE_HOST_UNIFIED_MEMORY                = $1035;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR           = $1036;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT          = $1037;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT            = $1038;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG           = $1039;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT          = $103A;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE         = $103B;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF           = $103C;
  CL_DEVICE_OPENCL_C_VERSION                   = $103D;
  CL_DEVICE_LINKER_AVAILABLE                   = $103E;
  CL_DEVICE_BUILT_IN_KERNELS                   = $103F;
  CL_DEVICE_IMAGE_MAX_BUFFER_SIZE              = $1040;
  CL_DEVICE_IMAGE_MAX_ARRAY_SIZE               = $1041;
  CL_DEVICE_PARENT_DEVICE                      = $1042;
  CL_DEVICE_PARTITION_MAX_SUB_DEVICES          = $1043;
  CL_DEVICE_PARTITION_PROPERTIES               = $1044;
  CL_DEVICE_PARTITION_AFFINITY_DOMAIN          = $1045;
  CL_DEVICE_PARTITION_TYPE                     = $1046;
  CL_DEVICE_REFERENCE_COUNT                    = $1047;
  CL_DEVICE_PREFERRED_INTEROP_USER_SYNC        = $1048;
  CL_DEVICE_PRINTF_BUFFER_SIZE                 = $1049;
  CL_DEVICE_IMAGE_PITCH_ALIGNMENT              = $104A;
  CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT       = $104B;
  CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS          = $104C;
  CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE           = $104D;
  CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES         = $104E;
  CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE     = $104F;
  CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE           = $1050;
  CL_DEVICE_MAX_ON_DEVICE_QUEUES               = $1051;
  CL_DEVICE_MAX_ON_DEVICE_EVENTS               = $1052;
  CL_DEVICE_SVM_CAPABILITIES                   = $1053;
  CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE = $1054;
  CL_DEVICE_MAX_PIPE_ARGS                      = $1055;
  CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS       = $1056;
  CL_DEVICE_PIPE_MAX_PACKET_SIZE               = $1057;
  CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT= $1058;
  CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT  = $1059;
  CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT   = $105A;

  (* cl_device_fp_config - bitfield *)
  CL_FP_DENORM                                 = (1 shl 0);
  CL_FP_INF_NAN                                = (1 shl 1);
  CL_FP_ROUND_TO_NEAREST                       = (1 shl 2);
  CL_FP_ROUND_TO_ZERO                          = (1 shl 3);
  CL_FP_ROUND_TO_INF                           = (1 shl 4);
  CL_FP_FMA                                    = (1 shl 5);
  CL_FP_SOFT_FLOAT                             = (1 shl 6);
  CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT          = (1 shl 7);

  (* cl_device_mem_cache_type *)
  CL_NONE                                      = $0;
  CL_READ_ONLY_CACHE                           = $1;
  CL_READ_WRITE_CACHE                          = $2;

  (* cl_device_local_mem_type *)
  CL_LOCAL                                     = $1;
  CL_GLOBAL                                    = $2;

  (* cl_device_exec_capabilities - bitfield *)
  CL_EXEC_KERNEL                               = (1 shl 0);
  CL_EXEC_NATIVE_KERNEL                        = (1 shl 1);

  (* cl_command_queue_properties - bitfield *)
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE       = (1 shl 0);
  CL_QUEUE_PROFILING_ENABLE                    = (1 shl 1);
  CL_QUEUE_ON_DEVICE                           = (1 shl 2);
  CL_QUEUE_ON_DEVICE_DEFAULT                   = (1 shl 3);

  (* cl_context_info  *)
  CL_CONTEXT_REFERENCE_COUNT                   = $1080;
  CL_CONTEXT_DEVICES                           = $1081;
  CL_CONTEXT_PROPERTIES                        = $1082;
  CL_CONTEXT_NUM_DEVICES                       = $1083;

  (* cl_context_properties *)
  CL_CONTEXT_PLATFORM                          = $1084;
  CL_CONTEXT_INTEROP_USER_SYNC                 = $1085;

  (* cl_device_partition_property *)
  CL_DEVICE_PARTITION_EQUALLY                  = $1086;
  CL_DEVICE_PARTITION_BY_COUNTS                = $1087;
  CL_DEVICE_PARTITION_BY_COUNTS_LIST_END       = $0;
  CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN       = $1088;

  (* cl_device_affinity_domain *)
  CL_DEVICE_AFFINITY_DOMAIN_NUMA               = (1 shl 0);
  CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE           = (1 shl 1);
  CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE           = (1 shl 2);
  CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE           = (1 shl 3);
  CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE           = (1 shl 4);
  CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE = (1 shl 5);

  (* cl_command_queue_info *)
  CL_QUEUE_CONTEXT                             = $1090;
  CL_QUEUE_DEVICE                              = $1091;
  CL_QUEUE_REFERENCE_COUNT                     = $1092;
  CL_QUEUE_PROPERTIES                          = $1093;
  CL_QUEUE_SIZE                                = $1094;

  (* cl_mem_flags - bitfield *)
  CL_MEM_READ_WRITE                            = (1 shl 0);
  CL_MEM_WRITE_ONLY                            = (1 shl 1);
  CL_MEM_READ_ONLY                             = (1 shl 2);
  CL_MEM_USE_HOST_PTR                          = (1 shl 3);
  CL_MEM_ALLOC_HOST_PTR                        = (1 shl 4);
  CL_MEM_COPY_HOST_PTR                         = (1 shl 5);
  // reserved                                  = (1 shl 6);
  CL_MEM_HOST_WRITE_ONLY                       = (1 shl 7);
  CL_MEM_HOST_READ_ONLY                        = (1 shl 8);
  CL_MEM_HOST_NO_ACCESS                        = (1 shl 9);
  CL_MEM_SVM_FINE_GRAIN_BUFFER                 = (1 shl 10);   //* used by cl_svm_mem_flags only */
  CL_MEM_SVM_ATOMICS                           = (1 shl 11);   //* used by cl_svm_mem_flags only */

  (* cl_mem_migration_flags - bitfield *)
  CL_MIGRATE_MEM_OBJECT_HOST                   = (1 shl 0);
  CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED      = (1 shl 1);

  (* cl_channel_order *)
  CL_R                                         = $10B0;
  CL_A                                         = $10B1;
  CL_RG                                        = $10B2;
  CL_RA                                        = $10B3;
  CL_RGB                                       = $10B4;
  CL_RGBA                                      = $10B5;
  CL_BGRA                                      = $10B6;
  CL_ARGB                                      = $10B7;
  CL_INTENSITY                                 = $10B8;
  CL_LUMINANCE                                 = $10B9;
  CL_Rx                                        = $10BA;
  CL_RGx                                       = $10BB;
  CL_RGBx                                      = $10BC;
  CL_DEPTH                                     = $10BD;
  CL_DEPTH_STENCIL                             = $10BE;
  CL_sRGB                                      = $10BF;
  CL_sRGBx                                     = $10C0;
  CL_sRGBA                                     = $10C1;
  CL_sBGRA                                     = $10C2;
  CL_ABGR                                      = $10C3;

  (* cl_channel_type *)
  CL_SNORM_INT8                                = $10D0;
  CL_SNORM_INT16                               = $10D1;
  CL_UNORM_INT8                                = $10D2;
  CL_UNORM_INT16                               = $10D3;
  CL_UNORM_SHORT_565                           = $10D4;
  CL_UNORM_SHORT_555                           = $10D5;
  CL_UNORM_INT_101010                          = $10D6;
  CL_SIGNED_INT8                               = $10D7;
  CL_SIGNED_INT16                              = $10D8;
  CL_SIGNED_INT32                              = $10D9;
  CL_UNSIGNED_INT8                             = $10DA;
  CL_UNSIGNED_INT16                            = $10DB;
  CL_UNSIGNED_INT32                            = $10DC;
  CL_HALF_FLOAT                                = $10DD;
  CL_FLOAT                                     = $10DE;
  CL_UNORM_INT24                               = $10DF;

  (* cl_mem_object_type *)
  CL_MEM_OBJECT_BUFFER                         = $10F0;
  CL_MEM_OBJECT_IMAGE2D                        = $10F1;
  CL_MEM_OBJECT_IMAGE3D                        = $10F2;
  CL_MEM_OBJECT_IMAGE2D_ARRAY                  = $10F3;
  CL_MEM_OBJECT_IMAGE1D                        = $10F4;
  CL_MEM_OBJECT_IMAGE1D_ARRAY                  = $10F5;
  CL_MEM_OBJECT_IMAGE1D_BUFFER                 = $10F6;
  CL_MEM_OBJECT_PIPE                           = $10F7;

  (* cl_mem_info *)
  CL_MEM_TYPE                                  = $1100;
  CL_MEM_FLAGS                                 = $1101;
  CL_MEM_SIZE                                  = $1102;
  CL_MEM_HOST_PTR                              = $1103;
  CL_MEM_MAP_COUNT                             = $1104;
  CL_MEM_REFERENCE_COUNT                       = $1105;
  CL_MEM_CONTEXT                               = $1106;
  CL_MEM_ASSOCIATED_MEMOBJECT                  = $1107;
  CL_MEM_OFFSET                                = $1108;
  CL_MEM_USES_SVM_POINTER                      = $1109;

  (* cl_image_info *)
  CL_IMAGE_FORMAT                              = $1110;
  CL_IMAGE_ELEMENT_SIZE                        = $1111;
  CL_IMAGE_ROW_PITCH                           = $1112;
  CL_IMAGE_SLICE_PITCH                         = $1113;
  CL_IMAGE_WIDTH                               = $1114;
  CL_IMAGE_HEIGHT                              = $1115;
  CL_IMAGE_DEPTH                               = $1116;
  CL_IMAGE_ARRAY_SIZE                          = $1117;
  CL_IMAGE_BUFFER                              = $1118;
  CL_IMAGE_NUM_MIP_LEVELS                      = $1119;
  CL_IMAGE_NUM_SAMPLES                         = $111A;

  (* cl_pipe_info *)
  CL_PIPE_PACKET_SIZE                          = $1120;
  CL_PIPE_MAX_PACKETS                          = $1121;

  (* cl_addressing_mode *)
  CL_ADDRESS_NONE                              = $1130;
  CL_ADDRESS_CLAMP_TO_EDGE                     = $1131;
  CL_ADDRESS_CLAMP                             = $1132;
  CL_ADDRESS_REPEAT                            = $1133;
  CL_ADDRESS_MIRRORED_REPEAT                   = $1134;

  (* cl_filter_mode *)
  CL_FILTER_NEAREST                            = $1140;
  CL_FILTER_LINEAR                             = $1141;

  (* cl_sampler_info *)
  CL_SAMPLER_REFERENCE_COUNT                   = $1150;
  CL_SAMPLER_CONTEXT                           = $1151;
  CL_SAMPLER_NORMALIZED_COORDS                 = $1152;
  CL_SAMPLER_ADDRESSING_MODE                   = $1153;
  CL_SAMPLER_FILTER_MODE                       = $1154;
  CL_SAMPLER_MIP_FILTER_MODE                   = $1155;
  CL_SAMPLER_LOD_MIN                           = $1156;
  CL_SAMPLER_LOD_MAX                           = $1157;

  (* cl_map_flags - bitfield *)
  CL_MAP_READ                                  = (1 shl 0);
  CL_MAP_WRITE                                 = (1 shl 1);
  CL_MAP_WRITE_INVALIDATE_REGION               = (1 shl 2);

  (* cl_program_info *)
  CL_PROGRAM_REFERENCE_COUNT                   = $1160;
  CL_PROGRAM_CONTEXT                           = $1161;
  CL_PROGRAM_NUM_DEVICES                       = $1162;
  CL_PROGRAM_DEVICES                           = $1163;
  CL_PROGRAM_SOURCE                            = $1164;
  CL_PROGRAM_BINARY_SIZES                      = $1165;
  CL_PROGRAM_BINARIES                          = $1166;
  CL_PROGRAM_NUM_KERNELS                       = $1167;
  CL_PROGRAM_KERNEL_NAMES                      = $1168;

  (* cl_program_build_info *)
  CL_PROGRAM_BUILD_STATUS                      = $1181;
  CL_PROGRAM_BUILD_OPTIONS                     = $1182;
  CL_PROGRAM_BUILD_LOG                         = $1183;
  CL_PROGRAM_BINARY_TYPE                       = $1184;
  CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE  = $1185;

  (* cl_program_binary_type *)
  CL_PROGRAM_BINARY_TYPE_NONE                  = $0;
  CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT       = $1;
  CL_PROGRAM_BINARY_TYPE_LIBRARY               = $2;
  CL_PROGRAM_BINARY_TYPE_EXECUTABLE            = $4;

  (* cl_build_status *)
  CL_BUILD_SUCCESS                             = 0;
  CL_BUILD_NONE                                = -1;
  CL_BUILD_ERROR                               = -2;
  CL_BUILD_IN_PROGRESS                         = -3;

  (* cl_kernel_info *)
  CL_KERNEL_FUNCTION_NAME                      = $1190;
  CL_KERNEL_NUM_ARGS                           = $1191;
  CL_KERNEL_REFERENCE_COUNT                    = $1192;
  CL_KERNEL_CONTEXT                            = $1193;
  CL_KERNEL_PROGRAM                            = $1194;
  CL_KERNEL_ATTRIBUTES                         = $1195;

  (* cl_kernel_arg_info *)
  CL_KERNEL_ARG_ADDRESS_QUALIFIER              = $1196;
  CL_KERNEL_ARG_ACCESS_QUALIFIER               = $1197;
  CL_KERNEL_ARG_TYPE_NAME                      = $1198;
  CL_KERNEL_ARG_TYPE_QUALIFIER                 = $1199;
  CL_KERNEL_ARG_NAME                           = $119A;

  (* cl_kernel_arg_address_qualifier *)
  CL_KERNEL_ARG_ADDRESS_GLOBAL                 = $119B;
  CL_KERNEL_ARG_ADDRESS_LOCAL                  = $119C;
  CL_KERNEL_ARG_ADDRESS_CONSTANT               = $119D;
  CL_KERNEL_ARG_ADDRESS_PRIVATE                = $119E;

  (* cl_kernel_arg_access_qualifier *)
  CL_KERNEL_ARG_ACCESS_READ_ONLY               = $11A0;
  CL_KERNEL_ARG_ACCESS_WRITE_ONLY              = $11A1;
  CL_KERNEL_ARG_ACCESS_READ_WRITE              = $11A2;
  CL_KERNEL_ARG_ACCESS_NONE                    = $11A3;
    
  (* cl_kernel_arg_type_qualifer *)
  CL_KERNEL_ARG_TYPE_NONE                      = 0;
  CL_KERNEL_ARG_TYPE_CONST                     = (1 shl 0);
  CL_KERNEL_ARG_TYPE_RESTRICT                  = (1 shl 1);
  CL_KERNEL_ARG_TYPE_VOLATILE                  = (1 shl 2);
  CL_KERNEL_ARG_TYPE_PIPE                      = (1 shl 3);

  (* cl_kernel_work_group_info *)
  CL_KERNEL_WORK_GROUP_SIZE                    = $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE            = $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE                     = $11B2;
  CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE = $11B3;
  CL_KERNEL_PRIVATE_MEM_SIZE                   = $11B4;
  CL_KERNEL_GLOBAL_WORK_SIZE                   = $11B5;

(* cl_kernel_exec_info *)
  CL_KERNEL_EXEC_INFO_SVM_PTRS                 = $11B6;
  CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM    = $11B7;

  (* cl_event_info  *)
  CL_EVENT_COMMAND_QUEUE                       = $11D0;
  CL_EVENT_COMMAND_TYPE                        = $11D1;
  CL_EVENT_REFERENCE_COUNT                     = $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS            = $11D3;
  CL_EVENT_CONTEXT                             = $11D4;

  (* cl_command_type *)
  CL_COMMAND_NDRANGE_KERNEL                    = $11F0;
  CL_COMMAND_TASK                              = $11F1;
  CL_COMMAND_NATIVE_KERNEL                     = $11F2;
  CL_COMMAND_READ_BUFFER                       = $11F3;
  CL_COMMAND_WRITE_BUFFER                      = $11F4;
  CL_COMMAND_COPY_BUFFER                       = $11F5;
  CL_COMMAND_READ_IMAGE                        = $11F6;
  CL_COMMAND_WRITE_IMAGE                       = $11F7;
  CL_COMMAND_COPY_IMAGE                        = $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER              = $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE              = $11FA;
  CL_COMMAND_MAP_BUFFER                        = $11FB;
  CL_COMMAND_MAP_IMAGE                         = $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT                  = $11FD;
  CL_COMMAND_MARKER                            = $11FE;
  CL_COMMAND_ACQUIRE_GL_OBJECTS                = $11FF;
  CL_COMMAND_RELEASE_GL_OBJECTS                = $1200;
  CL_COMMAND_READ_BUFFER_RECT                  = $1201;
  CL_COMMAND_WRITE_BUFFER_RECT                 = $1202;
  CL_COMMAND_COPY_BUFFER_RECT                  = $1203;
  CL_COMMAND_USER                              = $1204;
  CL_COMMAND_BARRIER                           = $1205;
  CL_COMMAND_MIGRATE_MEM_OBJECTS               = $1206;
  CL_COMMAND_FILL_BUFFER                       = $1207;
  CL_COMMAND_FILL_IMAGE                        = $1208;
  CL_COMMAND_SVM_FREE                          = $1209;
  CL_COMMAND_SVM_MEMCPY                        = $120A;
  CL_COMMAND_SVM_MEMFILL                       = $120B;
  CL_COMMAND_SVM_MAP                           = $120C;
  CL_COMMAND_SVM_UNMAP                         = $120D;

  (* command execution status *)
  CL_COMPLETE                                  = $0;
  CL_RUNNING                                   = $1;
  CL_SUBMITTED                                 = $2;
  CL_QUEUED                                    = $3;

  (* cl_buffer_create_type  *)
  CL_BUFFER_CREATE_TYPE_REGION                 = $1220;

  (* cl_profiling_info  *)
  CL_PROFILING_COMMAND_QUEUED                  = $1280;
  CL_PROFILING_COMMAND_SUBMIT                  = $1281;
  CL_PROFILING_COMMAND_START                   = $1282;
  CL_PROFILING_COMMAND_END                     = $1283;
  CL_PROFILING_COMMAND_COMPLETE                = $1284;
{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}

//********************************************************************************************************/

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'Types proceduress'}{$ENDIF}
{$IFDEF DYNLINK}
type
{$ENDIF}
  (* Platform API *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetPlatformIDs = {$ENDIF}function {$IFNDEF DYNLINK}clGetPlatformIDs{$ENDIF}(
                                 num_entries: TCL_uint;                         (* num_entries *)
                                 platforms: PPCL_platform_id;                   (* platforms *)
                                 num_platforms: PCL_uint                        (* num_platforms *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetPlatformIDs';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetPlatformInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetPlatformInfo{$ENDIF}(
                                   platform: PCL_platform_id;                   (* platform *)
                                   param_name: TCL_platform_info;               (* param_name *)
                                   param_value_size: TSize_t;                   (* param_value_size *)
                                   param_value: Pointer;                        (* param_value *)
                                   param_value_size_ret: PSize_t                (* param_value_size_ret *)
                                   ): TCL_int;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clGetPlatformInfo';{$ENDIF}
  {$ENDIF}

  (* Device APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetDeviceIDs = {$ENDIF}function {$IFNDEF DYNLINK}clGetDeviceIDs{$ENDIF}(
                                 platform: PCL_platform_id                      (* platform *);
                                 device_type: TCL_device_type;                  (* device_type *)
                                 num_entries: TCL_uint;                         (* num_entries *)
                                 devices: PPCL_device_id;                       (* devices *)
                                 num_devices: PCL_uint                          (* num_devices *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetDeviceIDs';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetDeviceInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetDeviceInfo{$ENDIF}(
                                 device: PCL_device_id;                         (* device *)
                                 param_name: TCL_device_info;                   (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetDeviceInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclCreateSubDevices = {$ENDIF}function {$IFNDEF DYNLINK}clCreateSubDevices{$ENDIF}(
                                  in_device: PCL_device_id;                     (* in_device *)
                                  const properties: PCL_device_partition_property;(* properties *)
                                  num_devices: TCL_uint;                        (* num_devices *)
                                  out_devices: PPCL_device_id;                  (* out_devices *)
                                  num_devices_ret: PCL_uint                     (* num_devices_ret *)
                                  ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateSubDevices';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclRetainDevice = {$ENDIF}function {$IFNDEF DYNLINK}clRetainDevice{$ENDIF}(device: PCL_device_id): TCL_int;extdecl; {$IFNDEF DYNLINK}external name 'clRetainDevice';{$ENDIF}
  {$ENDIF}
    
  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclReleaseDevice = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseDevice{$ENDIF}(device: PCL_device_id): TCL_int;extdecl; {$IFNDEF DYNLINK}external name 'clReleaseDevice';{$ENDIF}
  {$ENDIF}

  (* Context APIs  *)
  {$IFDEF CL_VERSION_1_0}
{$IFNDEF DYNLINK}
type
{$ENDIF}
  TContextNotify = procedure(const Name: PAnsiChar; const Data: Pointer; Size: TSize_t; Data2: Pointer); extdecl;
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateContext = {$ENDIF}function {$IFNDEF DYNLINK}clCreateContext{$ENDIF}(
                                 const properties: PPCL_context_properties;     (* properties *)
                                 num_devices: TCL_uint;                         (* num_devices *)
                                 const devices: PPCL_device_id;                 (* devices *)
                                 pfn_notify: TContextNotify;                    (* pfn_notify *)
                                 user_data: Pointer;                            (* user_data *)
                                 errcode_ret: PCL_int                           (* errcode_ret *)
                                 ): PCL_context;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateContext';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateContextFromType = {$ENDIF}function {$IFNDEF DYNLINK}clCreateContextFromType{$ENDIF}(
                                         const properties: PPCL_context_properties; (* properties *)
                                         device_type: TCL_device_type;              (* device_type *)
                                         pfn_notify: TContextNotify;                (*pfn_notify*)
                                         user_data: Pointer;                        (* user_data *)
                                         errcode_ret: PCL_int                       (* errcode_ret *)
                                         ): PCL_context;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clCreateContextFromType';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainContext = {$ENDIF}function {$IFNDEF DYNLINK}clRetainContext{$ENDIF}(context: PCL_context): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainContext';{$ENDIF}
  {$ENDIF}


  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseContext = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseContext{$ENDIF}(context: PCL_context): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseContext';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetContextInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetContextInfo{$ENDIF}(
                                 context: PCL_context;                          (* context *)
                                 param_name: TCL_context_info;                  (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetContextInfo';{$ENDIF}
  {$ENDIF}

  (* Command Queue APIs *)
  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclCreateCommandQueueWithProperties = {$ENDIF}function {$IFNDEF DYNLINK}clCreateCommandQueueWithProperties{$ENDIF}(
                                   context: PCL_context;                        (* context *)
                                   device: PCL_device_id;                       (* device *)
                                   const properties: PCL_queue_properties;      (* properties *)
                                   errcode_ret: PCL_int                         (* errcode_ret *)
                                   ): PCL_command_queue;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clCreateCommandQueueWithProperties';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF DYNLINK}TclCreateCommandQueue = {$ENDIF}function {$IFNDEF DYNLINK}clCreateCommandQueue{$ENDIF}(
                                     context: PCL_context;                      (* context *)
                                     device: PCL_device_id;                     (* device *)
                                     properties: TCL_command_queue_properties;  (* properties *)
                                     errcode_ret: PCL_int                       (* errcode_ret *)
                                     ): PCL_command_queue;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clCreateCommandQueue';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainCommandQueue = {$ENDIF}function {$IFNDEF DYNLINK}clRetainCommandQueue{$ENDIF}(command_queue: PCL_command_queue): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainCommandQueue';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseCommandQueue = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseCommandQueue{$ENDIF}(command_queue: PCL_command_queue): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseCommandQueue';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetCommandQueueInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetCommandQueueInfo{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       param_name: TCL_command_queue_info;      (* param_name *)
                                       param_value_size: TSize_t;               (* param_value_size *)
                                       param_value: Pointer;                    (* param_value *)
                                       param_value_size_ret: PSize_t            (* param_value_size_ret *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clGetCommandQueueInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_0_APIS}
    (*
     *  WARNING:
     *     This API introduces mutable state into the OpenCL implementation. It has been REMOVED
     *  to better facilitate thread safety.  The 1.0 API is not thread safe. It is not tested by the
     *  OpenCL 1.1 conformance test, and consequently may not work or may not work dependably.
     *  It is likely to be non-performant. Use of this API is not advised. Use at your own risk.
     *
     *  Software developers previously relying on this API are instructed to set the command queue
     *  properties when creating the queue, instead.
     *)
     
  {$IFDEF DYNLINK}TclSetCommandQueueProperty = {$ENDIF}function {$IFNDEF DYNLINK}clSetCommandQueueProperty{$ENDIF}(
                                           command_queue: PCL_command_queue;              (* command_queue *)
                                           properties: TCL_command_queue_properties;      (* properties *)
                                           enable: TCL_bool;                              (* enable *)
                                           old_properties: PCL_command_queue_properties   (* old_properties *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clSetCommandQueueProperty';{$ENDIF}
  {$ENDIF}
  {$ENDIF}

  (* Memory Object APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clCreateBuffer{$ENDIF}(
                               context: PCL_context;                            (* context *)
                               flags: TCL_mem_flags;                            (* flags *)
                               size: TSize_t;                                   (* size *)
                               host_ptr: Pointer;                               (* host_ptr *)
                               errcode_ret: PCL_int                             (* errcode_ret *)
                               ): PCL_mem;
                               extdecl; {$IFNDEF DYNLINK}external name 'clCreateBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclCreateSubBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clCreateSubBuffer{$ENDIF}(
                                 buffer: Pcl_mem;                               (* buffer *)
                                 flags: TCL_mem_flags;                          (* flags *)
                                 buffer_create_type: Tcl_buffer_create_type;    (* buffer_create_type *)
                                 const buffer_create_info: Pointer;             (* buffer_create_info *)
                                 errcode_ret: PCL_int                           (* errcode_ret *)
                                 ): PCL_mem;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateSubBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclCreateImage = {$ENDIF}function {$IFNDEF DYNLINK}clCreateImage{$ENDIF}(
                                 context: PCL_context;                          (* context *)
                                 flags: TCL_mem_flags;                          (* flags *)
                                 const image_format: PCL_image_format;          (* image_format *)
                                 const image_desc: PCL_image_desc;              (* image_desc *)
                                 host_ptr: Pointer;                             (* host_ptr *)
                                 errcode_ret: PCL_int                           (* errcode_ret *)
                                 ): PCL_mem;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclCreatePipe = {$ENDIF}function {$IFNDEF DYNLINK}clCreatePipe{$ENDIF}(
                                 context: PCL_context;                          (* context *)
                                 flags: TCL_mem_flags;                          (* flags *)
                                 pipe_packet_size: TCL_uint;                    (* pipe_packet_size *)
                                 pipe_max_packets: TCL_uint;                    (* pipe_max_packets *)
                                 const properties: PCL_pipe_properties;         (* properties *)
                                 errcode_ret: PCL_int                          (* errcode_ret *)
                                 ): PCL_mem;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreatePipe';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclCreateImage2D = {$ENDIF}function {$IFNDEF DYNLINK}clCreateImage2D{$ENDIF}(
                                 context: PCL_context;                          (* context *)
                                 flags: TCL_mem_flags;                          (* flags *)
                                 const image_format: PCL_image_format;          (* image_format *)
                                 image_width: TSize_t;                          (* image_width *)
                                 image_height: TSize_t;                         (* image_height *)
                                 image_row_pitch: TSize_t;                      (* image_row_pitch *)
                                 host_ptr: Pointer;                             (* host_ptr *)
                                 errcode_ret: PCL_int                           (* errcode_ret *)
                                 ): PCL_mem;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateImage2D';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclCreateImage3D = {$ENDIF}function {$IFNDEF DYNLINK}clCreateImage3D{$ENDIF}(
                                 context: PCL_context;                          (* context *)
                                 flags: TCL_mem_flags;                          (* flags *)
                                 const image_format: PCL_image_format;          (* image_format *)
                                 image_width: TSize_t;                          (* image_width *)
                                 image_height: TSize_t;                         (* image_height *)
                                 image_depth: TSize_t;                          (* image_depth *)
                                 image_row_pitch: TSize_t;                      (* image_row_pitch *)
                                 image_slice_pitch: TSize_t;                    (* image_slice_pitch *)
                                 host_ptr: Pointer;                             (* host_ptr *)
                                 errcode_ret: PCL_int                           (* errcode_ret *)
                                 ): PCL_mem;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clCreateImage3D';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainMemObject = {$ENDIF}function {$IFNDEF DYNLINK}clRetainMemObject{$ENDIF}(memobj: PCL_mem): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainMemObject';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseMemObject = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseMemObject{$ENDIF}(memobj: PCL_mem): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseMemObject';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetSupportedImageFormats = {$ENDIF}function {$IFNDEF DYNLINK}clGetSupportedImageFormats{$ENDIF}(
                                           context: PCL_context;                (* context *)
                                           flags: TCL_mem_flags;                (* flags *)
                                           image_type: TCL_mem_object_type;     (* image_type *)
                                           num_entries: TCL_uint;               (* num_entries *)
                                           image_formats: PCL_image_format;     (* image_formats *)
                                           num_image_formats: PCL_uint          (* num_image_formats *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clGetSupportedImageFormats';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetMemObjectInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetMemObjectInfo{$ENDIF}(
                                   memobj: PCL_mem;                             (* memobj *)
                                   param_name: TCL_mem_info;                    (* param_name *)
                                   param_value_size: TSize_t;                   (* param_value_size *)
                                   param_value: Pointer;                        (* param_value *)
                                   param_value_size_ret: PSize_t                (* param_value_size_ret *)
                                   ): TCL_int;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clGetMemObjectInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetImageInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetImageInfo{$ENDIF}(
                                 image: PCL_mem;                                (* image *)
                                 param_name: TCL_image_info;                    (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetImageInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclGetPipeInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetPipeInfo{$ENDIF}(
                                 pipe :PCL_mem;                                 (* pipe *)
                                 param_name: PCL_pipe_info;                      (* param_name *)
                                 param_value_size: TSize_t;                      (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: Psize_t                 (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetPipeInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
{$IFNDEF DYNLINK}
type
{$ENDIF}
  TMemObjectNotify = procedure(memob: PCL_mem; user_data: Pointer); extdecl;
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclSetMemObjectDestructorCallback = {$ENDIF}function {$IFNDEF DYNLINK}clSetMemObjectDestructorCallback{$ENDIF}(
                                                 memobj: Pcl_mem;               (* memobj *)
                                                 pfn_notify: TMemObjectNotify;  (* pfn_notify *)
                                                 user_data: Pointer             (*user_data *)
                                                 ): TCL_int;
                                                 extdecl; {$IFNDEF DYNLINK}external name 'clSetMemObjectDestructorCallback';{$ENDIF}
  {$ENDIF}

  (* SVM Allocation APIs *)
  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclSVMAlloc = {$ENDIF}function {$IFNDEF DYNLINK}clSVMAlloc{$ENDIF}(
                                                 context: PCL_context;          (* context *)
                                                 flags: TCL_svm_mem_flags;      (* flags *)
                                                 size: TSize_t;                 (* size *)
                                                 alignment: TCL_uint            (* alignment *)
                                                 ): Pointer;
                                                 extdecl; {$IFNDEF DYNLINK}external name 'clSVMAlloc';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclSVMFree = {$ENDIF}procedure {$IFNDEF DYNLINK}clSVMFree{$ENDIF}(
                                                 context: PCL_context;           (* context *)
                                                 svm_pointer: Pointer           (* svm_pointer *)
                                                 );
                                                 extdecl; {$IFNDEF DYNLINK}external name 'clSVMFree';{$ENDIF}
  {$ENDIF}

  (* Sampler APIs  *)
  {$IFDEF CL_VERSION_1_0}
    {$IFDEF DYNLINK}TclCreateSampler = {$ENDIF}function {$IFNDEF DYNLINK}clCreateSampler{$ENDIF}(
                                context: PCL_context;                           (* context *)
                                normalized_coords: TCL_bool;                    (* normalized_coords *)
                                addressing_mode: TCL_addressing_mode;           (* addressing_mode *)
                                filter_mode: TCL_filter_mode;                   (* filter_mode *)
                                errcode_ret: PCL_int                            (* errcode_ret *)
                                ): PCL_sampler;
                                extdecl; {$IFNDEF DYNLINK}external name 'clCreateSampler';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclCreateSamplerWithProperties = {$ENDIF}function {$IFNDEF DYNLINK}clCreateSamplerWithProperties{$ENDIF}(
                                context: PCL_context;                           (* context *)
                                const normalized_coords: PCL_sampler_properties;(* normalized_coords *)
                                errcode_ret: PCL_int                      (* errcode_ret *)
                                ): PCL_sampler;
                                extdecl; {$IFNDEF DYNLINK}external name 'clCreateSamplerWithProperties';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainSampler = {$ENDIF}function {$IFNDEF DYNLINK}clRetainSampler{$ENDIF}(sampler: PCL_sampler): TCL_sampler; extdecl; {$IFNDEF DYNLINK}external name 'clRetainSampler';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseSampler = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseSampler{$ENDIF}(sampler: PCL_sampler): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseSampler';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetSamplerInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetSamplerInfo{$ENDIF}(
                                 sampler: PCL_sampler;                          (* sampler *)
                                 param_name: TCL_sampler_info;                  (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetSamplerInfo';{$ENDIF}
  {$ENDIF}

  (* Program Object APIs  *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateProgramWithSource = {$ENDIF}function {$IFNDEF DYNLINK}clCreateProgramWithSource{$ENDIF}(
                                           context: PCL_context;                (* context *)
                                           count: TCL_uint;                     (* count *)
                                           const strings: PPAnsiChar;           (* strings *)
                                           const lengths: PSize_t;              (* lengths *)
                                           errcode_ret: PCL_int                 (* errcode_ret *)
                                           ): PCL_program;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clCreateProgramWithSource';{$ENDIF}
  {$ENDIF}

{$IFNDEF DYNLINK}
type
{$ENDIF}
  PPByte = ^PByte;
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateProgramWithBinary = {$ENDIF}function {$IFNDEF DYNLINK}clCreateProgramWithBinary{$ENDIF}(
                                           context: PCL_context;                (* context *)
                                           num_devices: TCL_uint;               (* num_devices *)
                                           const device_list: PCL_device_id;    (* device_list *)
                                           const lengths: PSize_t;              (* lengths *)
                                           const binaries: PPByte;              (* binaries *)
                                           binary_status: PCL_int;              (* binary_status *)
                                           errcode_ret: PCL_int                 (* errcode_ret *)
                                           ): PCL_program;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clCreateProgramWithBinary';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclCreateProgramWithBuiltInKernels = {$ENDIF}function {$IFNDEF DYNLINK}clCreateProgramWithBuiltInKernels{$ENDIF}(
                                           context: PCL_context;                (* context *)
                                           num_devices: TCL_uint;               (* num_devices *)
                                           const device_list: PCL_device_id;    (* device_list *)
                                           const kernel_names: PAnsiChar;       (* kernel_names *)
                                           errcode_ret: PCL_int                 (* errcode_ret *)
                                           ): PCL_program;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clCreateProgramWithBuiltInKernels';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainProgram = {$ENDIF}function {$IFNDEF DYNLINK}clRetainProgram{$ENDIF}(_program: PCL_program): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainProgram';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseProgram = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseProgram{$ENDIF}(_program: PCL_program): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseProgram';{$ENDIF}
  {$ENDIF}

{$IFNDEF DYNLINK}
type
{$ENDIF}
  {$IFDEF CL_VERSION_1_0}
  TProgramNotify = procedure(_program: PCL_program; user_data: Pointer); extdecl;
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclBuildProgram = {$ENDIF}function {$IFNDEF DYNLINK}clBuildProgram{$ENDIF}(
                               _program: PCL_program;                           (* program *)
                               num_devices: TCL_uint;                           (* num_devices *)
                               const device_list: PPCL_device_id;               (* device_list *)
                               const options: PAnsiChar;                        (* options *)
                               pfn_notify: TProgramNotify;                      (* void (pfn_notify)*)
                               user_data: Pointer                               (* user_data *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clBuildProgram';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclUnloadCompiler = {$ENDIF}function {$IFNDEF DYNLINK}clUnloadCompiler{$ENDIF}: TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clUnloadCompiler';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclCompileProgram = {$ENDIF}function {$IFNDEF DYNLINK}clCompileProgram{$ENDIF}(
                               _program: PCL_program;                           (* program *)
                               num_devices: TCL_uint;                           (* num_devices *)
                               const device_list: PPCL_device_id;               (* device_list *)
                               const options: PAnsiChar;                        (* options *)
                               num_input_headers: Tcl_uint;                     (* num_input_headers *)
                               const input_headers: PPCL_program;               (* input_headers *)
                               const header_include_names: PPAnsiChar;          (* header_include_names *)
                               pfn_notify: TProgramNotify;                      (* void (pfn_notify)*)
                               user_data: Pointer                               (* user_data *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clCompileProgram';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclLinkProgram = {$ENDIF}function {$IFNDEF DYNLINK}clLinkProgram{$ENDIF}(
                               context: PCL_context;                            (* context *)
                               num_devices: TCL_uint;                           (* num_devices *)
                               const device_list: PPCL_device_id;               (* device_list *)
                               const options: PAnsiChar;                        (* options *)
                               num_input_programs: Tcl_uint;                    (* num_input_programs *)
                               const input_programs: PPCL_program;              (* input_programs *)
                               pfn_notify: TProgramNotify;                      (* void (pfn_notify)*)
                               user_data: Pointer;                              (* user_data *)
                               errcode_ret: PCL_int                             (* errcode_ret *)
                               ): PCL_program;
                               extdecl; {$IFNDEF DYNLINK}external name 'clLinkProgram';{$ENDIF}
  {$ENDIF}


  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclUnloadPlatformCompiler = {$ENDIF}function {$IFNDEF DYNLINK}clUnloadPlatformCompiler{$ENDIF}(platform: PCL_platform_id): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clUnloadPlatformCompiler';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetProgramInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetProgramInfo{$ENDIF}(
                                   _program: PCL_program;                       (* program *)
                                   param_name: TCL_program_info;                (* param_name *)
                                   param_value_size: TSize_t;                   (* param_value_size *)
                                   param_value: Pointer;                        (* param_value *)
                                   param_value_size_ret: PSize_t                (* param_value_size_ret *)
                                   ): TCL_int;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clGetProgramInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetProgramBuildInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetProgramBuildInfo{$ENDIF}(
                                       _program: PCL_program;                   (* program *)
                                       device: PCL_device_id;                   (* device *)
                                       param_name: TCL_program_build_info;      (* param_name *)
                                       param_value_size: TSize_t;               (* param_value_size *)
                                       param_value: Pointer;                    (* param_value *)
                                       param_value_size_ret: PSize_t            (* param_value_size_ret *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clGetProgramBuildInfo';{$ENDIF}
  {$ENDIF}

  (* Kernel Object APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateKernel = {$ENDIF}function {$IFNDEF DYNLINK}clCreateKernel{$ENDIF}(
                               _program: PCL_program;                           (* program *)
                               const kernel_name: PAnsiChar;                    (* kernel_name *)
                               errcode_ret: PCL_int                             (* errcode_ret *)
                               ): PCL_kernel;
                               extdecl; {$IFNDEF DYNLINK}external name 'clCreateKernel';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclCreateKernelsInProgram = {$ENDIF}function {$IFNDEF DYNLINK}clCreateKernelsInProgram{$ENDIF}(
                                         _program: PCL_program;                 (* program *)
                                         num_kernels: TCL_uint;                 (* num_kernels *)
                                         kernels: PPCL_kernel;                  (* kernels *)
                                         num_kernels_ret: PCL_uint              (* num_kernels_ret *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clCreateKernelsInProgram';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainKernel = {$ENDIF}function {$IFNDEF DYNLINK}clRetainKernel{$ENDIF}(kernel: PCL_kernel): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainKernel';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseKernel = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseKernel{$ENDIF}(kernel: PCL_kernel): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseKernel';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclSetKernelArg = {$ENDIF}function {$IFNDEF DYNLINK}clSetKernelArg{$ENDIF}(
                               kernel: PCL_kernel;                              (* kernel *)
                               arg_index: TCL_uint;                             (* arg_index *)
                               arg_size: TSize_t;                               (* arg_size *)
                               const arg_value: Pointer                         (* arg_value *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clSetKernelArg';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclSetKernelArgSVMPointer = {$ENDIF}function {$IFNDEF DYNLINK}clSetKernelArgSVMPointer{$ENDIF}(
                                     kernel: PCL_kernel;                        (* kernel *)
                                     arg_index: TCL_uint;                       (* arg_index *)
                                     const arg_value: Pointer                   (* arg_value *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clSetKernelArgSVMPointer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclSetKernelExecInfo = {$ENDIF}function {$IFNDEF DYNLINK}clSetKernelExecInfo{$ENDIF}(
                                     kernel: PCL_kernel;                        (* kernel *)
                                     param_name: PCL_kernel_exec_info;          (* param_name *)
                                     param_value_size: TSize_t;                 (* param_value_size *)
                                     const param_value : Pointer                (* param_value *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clSetKernelExecInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetKernelInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetKernelInfo{$ENDIF}(
                                 kernel: PCL_kernel;                            (* kernel *)
                                 param_name: TCL_kernel_info;                   (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetKernelInfo';{$ENDIF}
  {$ENDIF}
  {$IFDEF CL_VERSION_1_2}

  {$IFDEF DYNLINK}TclGetKernelArgInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetKernelArgInfo{$ENDIF}(
                                 kernel: PCL_kernel;                            (* kernel *)
                                 arg_indx: Tcl_uint;                            (* arg_indx *)
                                 param_name: TCL_kernel_arg_info;               (* param_name *)
                                 param_value_size: TSize_t;                     (* param_value_size *)
                                 param_value: Pointer;                          (* param_value *)
                                 param_value_size_ret: PSize_t                  (* param_value_size_ret *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clGetKernelArgInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetKernelWorkGroupInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetKernelWorkGroupInfo{$ENDIF}(
                                         kernel: PCL_kernel;                    (* kernel *)
                                         device: PCL_device_id;                 (* device *)
                                         param_name: TCL_kernel_work_group_info;(* param_name *)
                                         param_value_size: TSize_t;             (* param_value_size *)
                                         param_value: Pointer;                  (* param_value *)
                                         param_value_size_ret: PSize_t          (* param_value_size_ret *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clGetKernelWorkGroupInfo';{$ENDIF}
  {$ENDIF}

  (* Event Object APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclWaitForEvents = {$ENDIF}function {$IFNDEF DYNLINK}clWaitForEvents{$ENDIF}(
                                 num_events: TCL_uint;                          (* num_events *)
                                 const event_list: PCL_event                    (* event_list *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clWaitForEvents';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetEventInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetEventInfo{$ENDIF}(
                               event: PCL_event;                                (* event *)
                               param_name: PCL_event_info;                      (* param_name *)
                               param_value_size: TSize_t;                       (* param_value_size *)
                               param_value: Pointer;                            (* param_value *)
                               param_value_size_ret: PSize_t                    (* param_value_size_ret *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clGetEventInfo';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclCreateUserEvent = {$ENDIF}function {$IFNDEF DYNLINK}clCreateUserEvent{$ENDIF}(
                                  context: Pcl_context;                         (* context  *)
                                  errcode_ret: Pcl_int                          (* errcode_ret *)
                                  ): PCL_event;
                                  extdecl; {$IFNDEF DYNLINK}external name 'clCreateUserEvent';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclRetainEvent = {$ENDIF}function {$IFNDEF DYNLINK}clRetainEvent{$ENDIF}(event: PCL_event): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clRetainEvent';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclReleaseEvent = {$ENDIF}function {$IFNDEF DYNLINK}clReleaseEvent{$ENDIF}(event: PCL_event): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clReleaseEvent';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclSetUserEventStatus = {$ENDIF}function {$IFNDEF DYNLINK}clSetUserEventStatus{$ENDIF}(
                                     event: Pcl_event;                          (* event *)
                                     execution_status: Tcl_int                  (* execution_status *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clSetUserEventStatus';{$ENDIF}
  {$ENDIF}


  {$IFDEF CL_VERSION_1_1}
{$IFNDEF DYNLINK}
type
{$ENDIF}
  TclEventNotify = procedure(event: PCL_event; cl_int: TCL_int; p: Pointer); extdecl;
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclSetEventCallback = {$ENDIF}function {$IFNDEF DYNLINK}clSetEventCallback{$ENDIF}(
                                    event: Pcl_event;                           (* event *)
                                    command_exec_callback_type: Tcl_int;        (* command_exec_callback_type *)
                                    pfn_notify: TclEventNotify;                 (* pfn_notify *)
                                    user_data: Pointer                          (* user_data *)
                                    ): TCL_int;
                                    extdecl; {$IFNDEF DYNLINK}external name 'clSetEventCallback';{$ENDIF}
  {$ENDIF}

  (* Profiling APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclGetEventProfilingInfo = {$ENDIF}function {$IFNDEF DYNLINK}clGetEventProfilingInfo{$ENDIF}(
                                         event: PCL_event;                      (* event *)
                                         param_name: TCL_profiling_info;        (* param_name *)
                                         param_value_size: TSize_t;             (* param_value_size *)
                                         param_value: Pointer;                  (* param_value *)
                                         param_value_size_ret: PSize_t          (* param_value_size_ret *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clGetEventProfilingInfo';{$ENDIF}
  {$ENDIF}

  (* Flush and Finish APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclFlush = {$ENDIF}function {$IFNDEF DYNLINK}clFlush{$ENDIF}(command_queue: PCL_command_queue): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clFlush';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclFinish = {$ENDIF}function {$IFNDEF DYNLINK}clFinish{$ENDIF}(command_queue: PCL_command_queue): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clFinish';{$ENDIF}
  {$ENDIF}

  (* Enqueued Commands APIs *)
  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueReadBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueReadBuffer{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     buffer: PCL_mem;                           (* buffer *)
                                     blocking_read: TCL_bool;                   (* blocking_read *)
                                     offset: TSize_t;                           (* offset *)
                                     cb: TSize_t;                               (* cb *)
                                     ptr: Pointer;                              (* ptr *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueReadBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclEnqueueReadBufferRect = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueReadBufferRect{$ENDIF}(
                                        command_queue: Pcl_command_queue;       (* command_queue *)
                                        buffer: Pcl_mem;                        (* buffer *)
                                        blocking_read: Tcl_bool;                (* blocking_read *)
                                        const buffer_origin: PSize_t;           (* buffer_origin *)
                                        const host_origin: PSize_t;             (* host_origin *)
                                        const region: PSize_t;                  (* region *)
                                        buffer_row_pitch: TSize_t;              (* buffer_row_pitch *)
                                        buffer_slice_pitch: TSize_t;            (* buffer_slice_pitch *)
                                        host_row_pitch: TSize_t;                (* host_row_pitch *)
                                        host_slice_pitch: TSize_t;              (* host_slice_pitch *)
                                        ptr: Pointer;                           (* ptr *)
                                        num_events_in_wait_list: Tcl_uint;      (* num_events_in_wait_list *)
                                        const event_wait_list: PPcl_event;      (* event_wait_list *)
                                        event: PPcl_event                       (* event *)
                                        ): TCL_int;
                                        extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueReadBufferRect';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueWriteBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueWriteBuffer{$ENDIF}(command_queue: PCL_command_queue;           (* command_queue *)
                                     buffer: PCL_mem;                           (* buffer *)
                                     blocking_write: TCL_bool;                  (* blocking_write *)
                                     offset: TSize_t;                           (* offset *)
                                     cb: TSize_t;                               (* cb *)
                                     const ptr: Pointer;                        (* ptr *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueWriteBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclEnqueueWriteBufferRect = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueWriteBufferRect{$ENDIF}(
                                        command_queue: Pcl_command_queue;       (* command_queue *)
                                        buffer: Pcl_mem;                        (* buffer *)
                                        blocking_write: Tcl_bool;               (* blocking_write *)
                                        const buffer_origin: PSize_t;           (* buffer_origin *)
                                        const host_origin: PSize_t;             (* host_origin *)
                                        const region: PSize_t;                  (* region *)
                                        buffer_row_pitch: TSize_t;              (* buffer_row_pitch *)
                                        buffer_slice_pitch: TSize_t;            (* buffer_slice_pitch *)
                                        host_row_pitch: TSize_t;                (* host_row_pitch *)
                                        host_slice_pitch: TSize_t;              (* host_slice_pitch *)
                                        ptr: Pointer;                           (* ptr *)
                                        num_events_in_wait_list: Tcl_uint;      (* num_events_in_wait_list *)
                                        const event_wait_list: PPcl_event;      (* event_wait_list *)
                                        event: PPcl_event                       (* event *)
                                        ): TCL_int;
                                        extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueWriteBufferRect';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclEnqueueFillBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueFillBuffer{$ENDIF}(
                                        command_queue: Pcl_command_queue;       (* command_queue *)
                                        buffer: Pcl_mem;                        (* buffer *)
                                        const pattern: Pointer;                 (* pattern *) 
                                        pattern_size: TSize_t;                  (* pattern_size *)
                                        offset: TSize_t;                        (* offset *)
                                        size: TSize_t;                          (* size *)
                                        num_events_in_wait_list: Tcl_uint;      (* num_events_in_wait_list *)
                                        const event_wait_list: PPcl_event;      (* event_wait_list *)
                                        event: PPcl_event                       (* event *)
                                        ): TCL_int;
                                        extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueFillBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueCopyBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueCopyBuffer{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     src_buffer: PCL_mem;                       (* src_buffer *)
                                     dst_buffer: PCL_mem;                       (* dst_buffer *)
                                     src_offset: TSize_t;                       (* src_offset *)
                                     dst_offset: TSize_t;                       (* dst_offset *)
                                     cb: TSize_t;                               (* cb *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueCopyBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_1}
  {$IFDEF DYNLINK}TclEnqueueCopyBufferRect = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueCopyBufferRect{$ENDIF}(
                                         command_queue: PCL_command_queue;      (* command_queue *)
                                         src_buffer: PCL_mem;                   (* src_buffer *)
                                         dst_buffer: PCL_mem;                   (* dst_buffer *)
                                         const src_origin: PSize_t;             (* src_origin *)
                                         const dst_origin: PSize_t;             (* dst_origin *)
                                         const region: PSize_t;                 (* region *)
                                         src_row_pitch: TSize_t;                (* src_row_pitch *)
                                         src_slice_pitch: TSize_t;              (* src_slice_pitch *)
                                         dst_row_pitch: TSize_t;                (* dst_row_pitch *)
                                         dst_slice_pitch: TSize_t;              (* dst_slice_pitch *)
                                         num_events_in_wait_list: TCL_uint;     (* num_events_in_wait_list *)
                                         const event_wait_list: PPCL_event;     (* event_wait_list *)
                                         event: PPCL_event                      (* event *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueCopyBufferRect';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueReadImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueReadImage{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     image: PCL_mem;                            (* image *)
                                     blocking_read: TCL_bool;                   (* blocking_read *)
                                     const origin: PSizet;                      (* origin[3] *)
                                     const region: PSizet;                      (* region[3] *)
                                     row_pitch: TSize_t;                        (* row_pitch *)
                                     slice_pitch: TSize_t;                      (* slice_pitch *)
                                     ptr: Pointer;                              (* ptr *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueReadImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueWriteImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueWriteImage{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     image: PCL_mem;                            (* image *)
                                     blocking_write: TCL_bool;                  (* blocking_write *)
                                     const origin: PSizet;                      (* [3] *)
                                     const region: PSizet;                      (* [3] *)
                                     input_row_pitch: TSize_t;                  (* input_row_pitch *)
                                     input_slice_pitch: TSize_t;                (* input_slice_pitch *)
                                     const ptr: Pointer;                        (* ptr *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueWriteImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclEnqueueFillImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueFillImage{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     image: PCL_mem;                            (* image *)
                                     const fill_color: Pointer;                 (* fill_color *)
                  // const size_t *     /* origin[3] */,
                   //const size_t *     /* region[3] */,
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *) 
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueFillImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueCopyImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueCopyImage{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     src_image: PCL_mem;                        (* src_image *)
                                     dst_image: PCL_mem;                        (* dst_image *)
                                     const src_origin: PSizet;                  (* src_origin[3] *)
                                     const dst_origin: PSizet;                  (* dst_origin[3] *)
                                     const region: PSizet;                      (* region[3] *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event                          (* event *)
                                     ): TCL_int;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueCopyImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueCopyImageToBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueCopyImageToBuffer{$ENDIF}(
                                           command_queue: PCL_command_queue;    (* command_queue *)
                                           src_image: PCL_mem;                  (* src_image *)
                                           dst_buffer: PCL_mem;                 (* dst_buffer *)
                                           const src_origin: PSizet;            (* [3] *)
                                           const region: PSizet;                (* [3] *)
                                           dst_offset: TSize_t;                 (* dst_offset *)
                                           num_events_in_wait_list: TCL_uint;   (* num_events_in_wait_list *)
                                           const event_wait_list: PPCL_event;   (* event_wait_list *)
                                           event: PPCL_event                    (* event *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueCopyImageToBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueCopyBufferToImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueCopyBufferToImage{$ENDIF}(
                                           command_queue: PCL_command_queue;    (* command_queue *)
                                           src_buffer: PCL_mem;                 (* src_buffer *)
                                           dst_image: PCL_mem;                  (* dst_image *)
                                           src_offset: TSize_t;                 (* src_offset *)
                                           const dst_origin: PSizet;            (* [3] *)
                                           const region: PSizet;                (* [3] *)
                                           num_events_in_wait_list: TCL_uint;   (* num_events_in_wait_list *)
                                           const event_wait_list: PPCL_event;   (* event_wait_list *)
                                           event: PPCL_event                    (* event *)
                                           ): TCL_int;
                                           extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueCopyBufferToImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueMapBuffer = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueMapBuffer{$ENDIF}(
                                     command_queue: PCL_command_queue;          (* command_queue *)
                                     buffer: PCL_mem;                           (* buffer *)
                                     blocking_map: TCL_bool;                    (* blocking_map *)
                                     map_flags: TCL_map_flags;                  (* map_flags *)
                                     offset: TSize_t;                           (* offset *)
                                     cb: TSize_t;                               (* cb *)
                                     num_events_in_wait_list: TCL_uint;         (* num_events_in_wait_list *)
                                     const event_wait_list: PPCL_event;         (* event_wait_list *)
                                     event: PPCL_event;                         (* event *)
                                     errcode_ret: PCL_int                       (* errcode_ret *)
                                     ): Pointer;
                                     extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueMapBuffer';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueMapImage = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueMapImage{$ENDIF}(
                                   command_queue: PCL_command_queue;            (* command_queue *)
                                   image: PCL_mem;                              (* image *)
                                   blocking_map: TCL_bool;                      (* blocking_map *)
                                   map_flags: TCL_map_flags;                    (* map_flags *)
                                   const origin: PSizet;                        (* [3] *)
                                   const region: PSizet;                        (* region[3] *)
                                   image_row_pitch: PSize_t;                    (* image_row_pitch *)
                                   image_slice_pitch: PSize_t;                  (* image_slice_pitch *)
                                   num_events_in_wait_list: TCL_uint;           (* num_events_in_wait_list *)
                                   const event_wait_list: PPCL_event;           (* event_wait_list *)
                                   event: PPCL_event;                           (* event *)
                                   errcode_ret: PCL_int                         (* errcode_ret *)
                                   ): Pointer;
                                   extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueMapImage';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueUnmapMemObject = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueUnmapMemObject{$ENDIF}(
                                         command_queue: PCL_command_queue;      (* command_queue *)
                                         memobj: PCL_mem;                       (* memobj *)
                                         mapped_ptr: Pointer;                   (* mapped_ptr *)
                                         num_events_in_wait_list: TCL_uint;     (* num_events_in_wait_list *)
                                         const event_wait_list: PPCL_event;     (* event_wait_list *)
                                         event: PPCL_event                      (* event *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueUnmapMemObject';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclEnqueueMigrateMemObjects = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueMigrateMemObjects{$ENDIF}(
                                         command_queue: PCL_command_queue;      (* command_queue *)
                                         num_mem_objects: Tcl_uint;             (* num_mem_objects *)
                                         const mem_objects: PPCL_mem;           (* mem_objects *)
                                         flags: Tcl_mem_migration_flags;        (* flags *)
                                         num_events_in_wait_list: TCL_uint;     (* num_events_in_wait_list *)
                                         const event_wait_list: PPCL_event;     (* event_wait_list *)
                                         event: PPCL_event                      (* event *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueMigrateMemObjects';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueNDRangeKernel = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueNDRangeKernel{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       kernel: PCL_kernel;                      (* kernel *)
                                       work_dim: TCL_uint;                      (* work_dim *)
                                       const global_work_offset: PSize_t;       (* global_work_offset *)
                                       const global_work_size: PSize_t;         (* global_work_size *)
                                       const local_work_size: PSize_t;          (* local_work_size *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueNDRangeKernel';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}

      {$IFDEF DYNLINK}TclEnqueueTask = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueTask{$ENDIF}(
                               command_queue: PCL_command_queue;                (* command_queue *)
                               kernel: PCL_kernel;                              (* kernel *)
                               num_events_in_wait_list: TCL_uint;               (* num_events_in_wait_list *)
                               const event_wait_list: PPCL_event;               (* event_wait_list *)
                               event: PPCL_event                                (* event *)
                               ): TCL_int;
                               extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueTask';{$ENDIF}

  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
{$IFNDEF DYNLINK}
type
{$ENDIF}
  TEnqueueUserProc = procedure (userdata: Pointer); extdecl;
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
  {$IFDEF DYNLINK}TclEnqueueNativeKernel = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueNativeKernel{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       user_func: TEnqueueUserProc;             (*user_func*)
                                       args: Pointer;                           (* args *)
                                       cb_args: TSize_t;                        (* cb_args *)
                                       num_mem_objects: TCL_uint;               (* num_mem_objects *)
                                       const mem_list: PPCL_mem;                (* mem_list *)
                                       const args_mem_loc: PPointer;            (* args_mem_loc *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueNativeKernel';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
    {$IFDEF DYNLINK}TclEnqueueMarkerWithWaitList = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueMarkerWithWaitList{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueMarkerWithWaitList';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}

  {$IFDEF DYNLINK}TclEnqueueBarrierWithWaitList = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueBarrierWithWaitList{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueBarrierWithWaitList';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclEnqueueSVMFree = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueSVMFree{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       num_svm_pointers: TCL_uint;              (* num_svm_pointers *)
                                       svm_pointers: PPointer;                  (* svm_pointers[] *)
                 //void (CL_CALLBACK * /*pfn_free_func*/)(cl_command_queue /* queue */,
                 //                                       cl_uint          /* num_svm_pointers */,
                 //                                       void *[]         /* svm_pointers[] */,
                 //                                       void *           /* user_data */),
                                       user_data: Pointer;                      (* user_data *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueSVMFree';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  TclEnqueueSVMMemcpy  = function(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       blocking_copy: TCL_bool;                 (* blocking_copy *)
                                       dst_ptr: Pointer;                        (* dst_ptr *)
                                       const src_ptr: Pointer;                  (* src_ptr *)
                                       size: TSize_t;                           (* size *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueSVMFree';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclEnqueueSVMMemFill = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueSVMMemFill{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       svm_ptr: Pointer;                        (* svm_ptr *)
                                       const pattern: Pointer;                  (* pattern *)
                                       pattern_size: TSize_t;                   (* pattern_size *)
                                       size: TSize_t;                           (* size *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list */,
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueSVMMemFill';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclEnqueueSVMMap = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueSVMMap{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       blocking_map: TCL_bool;                  (* blocking_map *)
                                       flags: TCL_map_flags;                     (* flags *)
                                       svm_ptr: Pointer;                        (* svm_ptr *)
                                       size: TSize_t;                           (* size *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueSVMMap';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_2_0}
  {$IFDEF DYNLINK}TclEnqueueSVMUnmap = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueSVMUnmap{$ENDIF}(
                                       command_queue: PCL_command_queue;        (* command_queue *)
                                       svm_ptr: Pointer;                        (* svm_ptr *)
                                       num_events_in_wait_list: TCL_uint;       (* num_events_in_wait_list *)
                                       const event_wait_list: PPCL_event;       (* event_wait_list *)
                                       event: PPCL_event                        (* event *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueSVMUnmap';{$ENDIF}
  {$ENDIF}


  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclSetPrintfCallback = {$ENDIF}function {$IFNDEF DYNLINK}clSetPrintfCallback{$ENDIF}(
                                       context: PCL_context;                    (* context *)
                    //void (CL_CALLBACK * /* pfn_notify */)(cl_context /* program */,
                    //                                      cl_uint /*printf_data_len */,
                    //                                      char * /* printf_data_ptr */,
                    //                                      void * /* user_data */),
                                       user_data: Pointer                       (* user_data *)
                                       ): TCL_int;
                                       extdecl; {$IFNDEF DYNLINK}external name 'clSetPrintfCallback';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclEnqueueMarker = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueMarker{$ENDIF}(
                                 command_queue: PCL_command_queue;              (* command_queue *)
                                 event: PPCL_event                              (* event *)
                                 ): TCL_int;
                                 extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueMarker';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclEnqueueWaitForEvents = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueWaitForEvents{$ENDIF}(
                                         command_queue: PCL_command_queue;      (* command_queue *)
                                         num_events: TCL_uint;                  (* num_events *)
                                         const event_list: PPCL_event           (* event_list *)
                                         ): TCL_int;
                                         extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueWaitForEvents';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
      {$IFDEF DYNLINK}TclEnqueueBarrier = {$ENDIF}function {$IFNDEF DYNLINK}clEnqueueBarrier{$ENDIF}(command_queue: PCL_command_queue (* command_queue *) ): TCL_int; extdecl; {$IFNDEF DYNLINK}external name 'clEnqueueBarrier';{$ENDIF}
    {$ENDIF}
  {$ENDIF}

  (* Extension function access
   *
   * Returns the extension function address for the given function name,
   * or NULL if a valid function can not be found.  The client must
   * check to make sure the address is not NULL, before using or
   * calling the returned function address.
   *)

  {$IFDEF CL_VERSION_1_0}
      {$IFDEF DYNLINK}TclGetExtensionFunctionAddress = {$ENDIF}function {$IFNDEF DYNLINK}clGetExtensionFunctionAddress{$ENDIF}(const func_name: PAnsiChar): Pointer; extdecl; {$IFNDEF DYNLINK}external name 'clGetExtensionFunctionAddress';{$ENDIF}
  {$ENDIF}

  {$IFDEF CL_VERSION_1_2}
  {$IFDEF DYNLINK}TclGetExtensionFunctionAddressForPlatform = {$ENDIF}function {$IFNDEF DYNLINK}clGetExtensionFunctionAddressForPlatform{$ENDIF}(
                                                       platform: PCL_platform_id; (* platform *)
                                                       const func_name: PAnsiChar  (* func_name *)
                                                       ): Pointer; extdecl; {$IFNDEF DYNLINK}external name 'clGetExtensionFunctionAddressForPlatform';{$ENDIF}
  {$ENDIF}

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'Var'}{$ENDIF}
{$IFDEF DYNLINK}
var
  (* Platform API *)
{$IFDEF CL_VERSION_1_0}
  clGetPlatformIDs:   TclGetPlatformIDs;
  clGetPlatformInfo:  TclGetPlatformInfo;
{$ENDIF}


  (* Device APIs *)
{$IFDEF CL_VERSION_1_0}
  clGetDeviceIDs:     TclGetDeviceIDs;
  clGetDeviceInfo:    TclGetDeviceInfo;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clCreateSubDevices: TclCreateSubDevices;
  clRetainDevice: TclRetainDevice;
  clReleaseDevice: TclReleaseDevice;
{$ENDIF}

  (* Context APIs *)
{$IFDEF CL_VERSION_1_0}
  clCreateContext:          TclCreateContext;
  clCreateContextFromType:  TclCreateContextFromType;
  clRetainContext:          TclRetainContext;
  clReleaseContext:         TclReleaseContext;
  clGetContextInfo:         TclGetContextInfo;
{$ENDIF}

  (* Command Queue APIs *)
{$IFDEF CL_VERSION_1_0}
  clCreateCommandQueue:       TclCreateCommandQueue;
  clRetainCommandQueue:       TclRetainCommandQueue;
  clReleaseCommandQueue:      TclReleaseCommandQueue;
  clGetCommandQueueInfo:      TclGetCommandQueueInfo;
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_0_APIS}
  clSetCommandQueueProperty:  TclSetCommandQueueProperty;
  {$ENDIF}
{$ENDIF}

  (* Memory Object APIs *)
{$IFDEF CL_VERSION_1_0}
  clCreateBuffer:             TclCreateBuffer;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clCreateSubBuffer:          TclCreateSubBuffer; // OpenCL 1.1
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clCreateImage: TclCreateImage;
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    clCreateImage2D:            TclCreateImage2D;
    clCreateImage3D:            TclCreateImage3D;
  {$ENDIF}
  clRetainMemObject:          TclRetainMemObject;
  clReleaseMemObject:         TclReleaseMemObject;
  clGetSupportedImageFormats: TclGetSupportedImageFormats;
  clGetMemObjectInfo:         TclGetMemObjectInfo;
  clGetImageInfo:             TclGetImageInfo;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clSetMemObjectDestructorCallback: TclSetMemObjectDestructorCallback; // OpenCL 1.1
{$ENDIF}

  (* Sampler APIs *)
{$IFDEF CL_VERSION_1_0}

  clCreateSampler:            TclCreateSampler;

  clRetainSampler:            TclRetainSampler;
  clReleaseSampler:           TclReleaseSampler;
  clGetSamplerInfo:           TclGetSamplerInfo;
{$ENDIF}

  (* Program Object APIs *)
{$IFDEF CL_VERSION_1_0}
  clCreateProgramWithSource:  TclCreateProgramWithSource;
  clCreateProgramWithBinary:  TclCreateProgramWithBinary;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clCreateProgramWithBuiltInKernels: TclCreateProgramWithBuiltInKernels;
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  clRetainProgram:            TclRetainProgram;
  clReleaseProgram:           TclReleaseProgram;
  clBuildProgram:             TclBuildProgram;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clCompileProgram: TclCompileProgram;
  clLinkProgram: TclLinkProgram;
  clUnloadPlatformCompiler: TclUnloadPlatformCompiler;
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    clUnloadCompiler:       TclUnloadCompiler;
  {$ENDIF}
  clGetProgramInfo:       TclGetProgramInfo;
  clGetProgramBuildInfo:  TclGetProgramBuildInfo;
{$ENDIF}

  (* Kernel Object APIs *)
{$IFDEF CL_VERSION_1_0}
  clCreateKernel:             TclCreateKernel;
  clCreateKernelsInProgram:   TclCreateKernelsInProgram;
  clRetainKernel:   TclRetainKernel;
  clReleaseKernel:  TclReleaseKernel;
  clSetKernelArg:   TclSetKernelArg;
  clGetKernelInfo:  TclGetKernelInfo;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clGetKernelArgInfo: TclGetKernelArgInfo;
{$ENDIF}

{$IFDEF CL_VERSION_1_0}
  clGetKernelWorkGroupInfo: TclGetKernelWorkGroupInfo;
{$ENDIF}

  (* Event Object APIs *)
{$IFDEF CL_VERSION_1_0}
  clWaitForEvents:  TclWaitForEvents;
  clGetEventInfo:   TclGetEventInfo;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clCreateUserEvent: TclCreateUserEvent; // OpenCL 1.1
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clRetainEvent:    TclRetainEvent;
  clReleaseEvent:   TclReleaseEvent;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clSetUserEventStatus: TclSetUserEventStatus; // OpenCL 1.1
  clSetEventCallback: TclSetEventCallback; // OpenCL 1.1
{$ENDIF}

  (* Profiling APIs *)
{$IFDEF CL_VERSION_1_0}
  clGetEventProfilingInfo:  TclGetEventProfilingInfo;
{$ENDIF}

  (* Flush and Finish APIs *)
{$IFDEF CL_VERSION_1_0}
  clFlush:  TclFlush;
  clFinish: TclFinish;
{$ENDIF}
  (* Enqueued Commands APIs *)
{$IFDEF CL_VERSION_1_0}
  clEnqueueReadBuffer:  TclEnqueueReadBuffer;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clEnqueueReadBufferRect: TclEnqueueReadBufferRect; // OpenCL 1.1
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clEnqueueWriteBuffer: TclEnqueueWriteBuffer;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clEnqueueWriteBufferRect: TclEnqueueWriteBufferRect; // OpenCL 1.1
{$ENDIF}
{$IFDEF CL_VERSION_1_2}
  clEnqueueFillBuffer: TclEnqueueFillBuffer;
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clEnqueueCopyBuffer:  TclEnqueueCopyBuffer;
{$ENDIF}
{$IFDEF CL_VERSION_1_1}
  clEnqueueCopyBufferRect: TclEnqueueCopyBufferRect; //OpenCL 1.1
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clEnqueueReadImage:   TclEnqueueReadImage;
  clEnqueueWriteImage:  TclEnqueueWriteImage;
{$ENDIF}
{$IFDEF CL_VERSION_1_2}
  clEnqueueFillImage: TclEnqueueFillImage;
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clEnqueueCopyImage:   TclEnqueueCopyImage;
  clEnqueueCopyImageToBuffer: TclEnqueueCopyImageToBuffer;
  clEnqueueCopyBufferToImage: TclEnqueueCopyBufferToImage;
  clEnqueueMapBuffer:   TclEnqueueMapBuffer;
  clEnqueueMapImage:    TclEnqueueMapImage;
  clEnqueueUnmapMemObject:    TclEnqueueUnmapMemObject;
{$ENDIF}
{$IFDEF CL_VERSION_1_2}
  clEnqueueMigrateMemObjects: TclEnqueueMigrateMemObjects;
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  clEnqueueNDRangeKernel:     TclEnqueueNDRangeKernel;

    clEnqueueTask:          TclEnqueueTask;

  clEnqueueNativeKernel:  TclEnqueueNativeKernel;
{$ENDIF}
{$IFDEF CL_VERSION_1_2}
  clEnqueueMarkerWithWaitList: TclEnqueueMarkerWithWaitList;
  clEnqueueBarrierWithWaitList: TclEnqueueBarrierWithWaitList;
  clSetPrintfCallback: TclSetPrintfCallback;
{$ENDIF}
{$IFDEF CL_VERSION_1_0}
  {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
    clEnqueueMarker:        TclEnqueueMarker;
    clEnqueueWaitForEvents: TclEnqueueWaitForEvents;
    clEnqueueBarrier:       TclEnqueueBarrier;
  {$ENDIF}
{$ENDIF}

(* Extension function access
 *
 * Returns the extension function address for the given function name,
 * or NULL if a valid function can not be found.  The client must
 * check to make sure the address is not NULL, before using or
 * calling the returned function address.
 *)
{$IFDEF CL_VERSION_1_0}
    clGetExtensionFunctionAddress:  TclGetExtensionFunctionAddress;
{$ENDIF}

{$IFDEF CL_VERSION_1_2}
  clGetExtensionFunctionAddressForPlatform: TclGetExtensionFunctionAddressForPlatform;
{$ENDIF}

{$IFDEF CL_VERSION_2_0}
  clCreateCommandQueueWithProperties: TclCreateCommandQueueWithProperties;
  clCreatePipe: TclCreatePipe;
  clGetPipeInfo: TclGetPipeInfo;
  clSVMAlloc: TclSVMAlloc;
  clSVMFree: TclSVMFree;
  clCreateSamplerWithProperties: TclCreateSamplerWithProperties;
  clSetKernelArgSVMPointer: TclSetKernelArgSVMPointer;
  clSetKernelExecInfo: TclSetKernelExecInfo;

  clEnqueueSVMFree: TclEnqueueSVMFree;
  clEnqueueSVMMemcpy: TclEnqueueSVMMemcpy;
  clEnqueueSVMMemFill: TclEnqueueSVMMemFill;
  clEnqueueSVMMap: TclEnqueueSVMMap;
  clEnqueueSVMUnmap: TclEnqueueSVMUnmap;
{$ENDIF}

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}

function oclGetProcAddress(ProcName: PAnsiChar; LibHandle: Pointer = nil): Pointer;
function InitOpenCL(LibName: String = OpenCLLibName): Boolean;

var
  OCL_LibHandle: Pointer = nil;
{$ENDIF}

function GetString(const Status: TCL_int): AnsiString;

implementation

{$IFDEF LINUX}
const
  RTLD_LAZY = $001;
  RTLD_NOW = $002;
  RTLD_BINDING_MASK = $003;

  // Seems to work on Debian / Fedora
  LibraryLib = {$IFDEF LINUX} 'libdl.so.2'{$ELSE} 'c'{$ENDIF};

function dlopen(Name: PAnsiChar; Flags: LongInt): Pointer; cdecl; external LibraryLib name 'dlopen';
function dlclose(Lib: Pointer): LongInt; cdecl; external LibraryLib name 'dlclose';
function dlsym(Lib: Pointer; Name: PAnsiChar): Pointer; cdecl; external LibraryLib name 'dlsym';
{$ENDIF}

function oclLoadLibrary(Name: PChar): Pointer;
begin
  {$IFDEF WINDOWS}
    Result := Pointer(LoadLibrary(Name));
  {$ENDIF}

  {$IFDEF LINUX}
    Result := dlopen(Name, RTLD_LAZY);
  {$ENDIF}
end;

function oclFreeLibrary(LibHandle: Pointer): Boolean;
begin
  if LibHandle = nil then
    Result := False
  else
  {$IFDEF WINDOWS}
    Result := FreeLibrary(HMODULE(LibHandle));
  {$ENDIF}

  {$IFDEF LINUX}
    Result := dlclose(LibHandle) = 0;
  {$ENDIF}
end;

{$IFDEF DYNLINK}
function oclGetProcAddress(ProcName: PAnsiChar; LibHandle: Pointer = nil): Pointer;
begin
  if LibHandle = nil then LibHandle := OCL_LibHandle;

  {$IFDEF WINDOWS}
    Result := GetProcAddress(HMODULE(LibHandle), ProcName);
  {$ENDIF}

  {$IFDEF LINUX}
    Result := dlsym(LibHandle, ProcName);
  {$ENDIF}
end;

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'InitOpenCL'}{$ENDIF}
function InitOpenCL(LibName: String): Boolean;
begin
  Result := False;

  // free opened libraries
  if OCL_LibHandle <> nil then
    oclFreeLibrary(OCL_LibHandle);

  // load library
  OCL_LibHandle := oclLoadLibrary(PChar(LibName));

  // load CL functions
  if (OCL_LibHandle <> nil) then
  begin
    (* Platform API *)
    {$IFDEF CL_VERSION_1_0}
      clGetPlatformIDs := TclGetPlatformIDs(oclGetProcAddress('clGetPlatformIDs', OCL_LibHandle));
      clGetPlatformInfo := TclGetPlatformInfo(oclGetProcAddress('clGetPlatformInfo', OCL_LibHandle));
    {$ENDIF}

    (* Device APIs *)
    {$IFDEF CL_VERSION_1_0}
      clGetDeviceIDs := TclGetDeviceIDs(oclGetProcAddress('clGetDeviceIDs', OCL_LibHandle));
      clGetDeviceInfo := TclGetDeviceInfo(oclGetProcAddress('clGetDeviceInfo', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clCreateSubDevices := TclCreateSubDevices(oclGetProcAddress('clCreateSubDevices', OCL_LibHandle));
      clRetainDevice := TclRetainDevice(oclGetProcAddress('clRetainDevice', OCL_LibHandle));
      clReleaseDevice := TclReleaseDevice(oclGetProcAddress('clReleaseDevice', OCL_LibHandle));
    {$ENDIF}

    (* Context APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateContext := TclCreateContext(oclGetProcAddress('clCreateContext', OCL_LibHandle));
      clCreateContextFromType := TclCreateContextFromType(oclGetProcAddress('clCreateContextFromType', OCL_LibHandle));
      clRetainContext := TclRetainContext(oclGetProcAddress('clRetainContext', OCL_LibHandle));
      clReleaseContext := TclReleaseContext(oclGetProcAddress('clReleaseContext', OCL_LibHandle));
      clGetContextInfo := TclGetContextInfo(oclGetProcAddress('clGetContextInfo', OCL_LibHandle));
    {$ENDIF}

    (* Command Queue APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateCommandQueue := TclCreateCommandQueue(oclGetProcAddress('clCreateCommandQueue', OCL_LibHandle));
      clRetainCommandQueue := TclRetainCommandQueue(oclGetProcAddress('clRetainCommandQueue', OCL_LibHandle));
      clReleaseCommandQueue := TclReleaseCommandQueue(oclGetProcAddress('clReleaseCommandQueue', OCL_LibHandle));
      clGetCommandQueueInfo := TclGetCommandQueueInfo(oclGetProcAddress('clGetCommandQueueInfo', OCL_LibHandle));
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_0_APIS}
        clSetCommandQueueProperty := TclSetCommandQueueProperty(oclGetProcAddress('clSetCommandQueueProperty', OCL_LibHandle));
      {$ENDIF}
    {$ENDIF}

    (* Memory Object APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateBuffer := TclCreateBuffer(oclGetProcAddress('clCreateBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
      clCreateSubBuffer := TclCreateSubBuffer(oclGetProcAddress('clCreateSubBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clCreateImage := TclCreateImage(oclGetProcAddress('clCreateImage', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
        clCreateImage2D := TclCreateImage2D(oclGetProcAddress('clCreateImage2D', OCL_LibHandle));
        clCreateImage3D := TclCreateImage3D(oclGetProcAddress('clCreateImage3D', OCL_LibHandle));
      {$ENDIF}
      clRetainMemObject := TclRetainMemObject(oclGetProcAddress('clRetainMemObject', OCL_LibHandle));
      clReleaseMemObject := TclReleaseMemObject(oclGetProcAddress('clReleaseMemObject', OCL_LibHandle));
      clGetSupportedImageFormats := TclGetSupportedImageFormats(oclGetProcAddress('clGetSupportedImageFormats', OCL_LibHandle));
      clGetMemObjectInfo := TclGetMemObjectInfo(oclGetProcAddress('clGetMemObjectInfo', OCL_LibHandle));
      clGetImageInfo := TclGetImageInfo(oclGetProcAddress('clGetImageInfo', OCL_LibHandle));
    {$ENDIF}
    
    {$IFDEF CL_VERSION_1_1}
      clSetMemObjectDestructorCallback := TclSetMemObjectDestructorCallback(oclGetProcAddress('clSetMemObjectDestructorCallback', OCL_LibHandle));
    {$ENDIF}

    (* Sampler APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateSampler := TclCreateSampler(oclGetProcAddress('clCreateSampler', OCL_LibHandle));
      clRetainSampler := TclRetainSampler(oclGetProcAddress('clRetainSampler', OCL_LibHandle));
      clReleaseSampler := TclReleaseSampler(oclGetProcAddress('clReleaseSampler', OCL_LibHandle));
      clGetSamplerInfo := TclGetSamplerInfo(oclGetProcAddress('clGetSamplerInfo', OCL_LibHandle));
    {$ENDIF}

    (* Program Object APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateProgramWithSource := TclCreateProgramWithSource(oclGetProcAddress('clCreateProgramWithSource', OCL_LibHandle));
      clCreateProgramWithBinary := TclCreateProgramWithBinary(oclGetProcAddress('clCreateProgramWithBinary', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clCreateProgramWithBuiltInKernels := TclCreateProgramWithBuiltInKernels(oclGetProcAddress('clCreateProgramWithBuiltInKernels', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      clRetainProgram := TclRetainProgram(oclGetProcAddress('clRetainProgram', OCL_LibHandle));
      clReleaseProgram := TclReleaseProgram(oclGetProcAddress('clReleaseProgram', OCL_LibHandle));
      clBuildProgram := TclBuildProgram(oclGetProcAddress('clBuildProgram', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clCompileProgram := TclCompileProgram(oclGetProcAddress('clCompileProgram', OCL_LibHandle));
      clLinkProgram := TclLinkProgram(oclGetProcAddress('clLinkProgram', OCL_LibHandle));
      clUnloadPlatformCompiler := TclUnloadPlatformCompiler(oclGetProcAddress('clUnloadPlatformCompiler', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
        clUnloadCompiler := TclUnloadCompiler(oclGetProcAddress('clUnloadCompiler', OCL_LibHandle));
      {$ENDIF}
      clGetProgramInfo := TclGetProgramInfo(oclGetProcAddress('clGetProgramInfo', OCL_LibHandle));
      clGetProgramBuildInfo := TclGetProgramBuildInfo(oclGetProcAddress('clGetProgramBuildInfo', OCL_LibHandle));
    {$ENDIF}
    
    (* Kernel Object APIs *)
    {$IFDEF CL_VERSION_1_0}
      clCreateKernel := TclCreateKernel(oclGetProcAddress('clCreateKernel', OCL_LibHandle));
      clCreateKernelsInProgram := TclCreateKernelsInProgram(oclGetProcAddress('clCreateKernelsInProgram', OCL_LibHandle));

      clRetainKernel := TclRetainKernel(oclGetProcAddress('clRetainKernel', OCL_LibHandle));
      clReleaseKernel := TclReleaseKernel(oclGetProcAddress('clReleaseKernel', OCL_LibHandle));
      clSetKernelArg := TclSetKernelArg(oclGetProcAddress('clSetKernelArg', OCL_LibHandle));
      clGetKernelInfo := TclGetKernelInfo(oclGetProcAddress('clGetKernelInfo', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_2}
      clGetKernelArgInfo := TclGetKernelArgInfo(oclGetProcAddress('clGetKernelArgInfo', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_0}
      clGetKernelWorkGroupInfo := TclGetKernelWorkGroupInfo(oclGetProcAddress('clGetKernelWorkGroupInfo', OCL_LibHandle));
    {$ENDIF}

    (* Event Object APIs *)
    {$IFDEF CL_VERSION_1_0}
      clWaitForEvents :=  TclWaitForEvents(oclGetProcAddress('clWaitForEvents', OCL_LibHandle));
      clGetEventInfo := TclGetEventInfo(oclGetProcAddress('clGetEventInfo', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
      clCreateUserEvent := TclCreateUserEvent(oclGetProcAddress('clCreateUserEvent', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_0}
      clRetainEvent := TclRetainEvent(oclGetProcAddress('clRetainEvent', OCL_LibHandle));
      clReleaseEvent := TclReleaseEvent(oclGetProcAddress('clReleaseEvent', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_1}
      clSetUserEventStatus := TclSetUserEventStatus(oclGetProcAddress('clSetUserEventStatus', OCL_LibHandle));
      clSetEventCallback := TclSetEventCallback(oclGetProcAddress('clSetEventCallback', OCL_LibHandle));
    {$ENDIF}

    (* Profiling APIs *)
    {$IFDEF CL_VERSION_1_0}
      clGetEventProfilingInfo := TclGetEventProfilingInfo(oclGetProcAddress('clGetEventProfilingInfo', OCL_LibHandle));
    {$ENDIF}

    (* Flush and Finish APIs *)
    {$IFDEF CL_VERSION_1_0}
      clFlush := TclFlush(oclGetProcAddress('clFlush', OCL_LibHandle));
      clFinish := TclFinish(oclGetProcAddress('clFinish', OCL_LibHandle));
    {$ENDIF}

    (* Enqueued Commands APIs *)
    {$IFDEF CL_VERSION_1_0}
      clEnqueueReadBuffer := TclEnqueueReadBuffer(oclGetProcAddress('clEnqueueReadBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
      clEnqueueReadBufferRect := TclEnqueueReadBufferRect(oclGetProcAddress('clEnqueueReadBufferRect', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      clEnqueueWriteBuffer := TclEnqueueWriteBuffer(oclGetProcAddress('clEnqueueWriteBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clEnqueueFillBuffer := TclEnqueueFillBuffer(oclGetProcAddress('clEnqueueFillBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
      clEnqueueWriteBufferRect := TclEnqueueWriteBufferRect(oclGetProcAddress('clEnqueueWriteBufferRect', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      clEnqueueCopyBuffer := TclEnqueueCopyBuffer(oclGetProcAddress('clEnqueueCopyBuffer', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_1}
      clEnqueueCopyBufferRect := TclEnqueueCopyBufferRect(oclGetProcAddress('clEnqueueCopyBufferRect', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      clEnqueueReadImage := TclEnqueueReadImage(oclGetProcAddress('clEnqueueReadImage', OCL_LibHandle));
      clEnqueueWriteImage := TclEnqueueWriteImage(oclGetProcAddress('clEnqueueWriteImage', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clEnqueueFillImage := TclEnqueueFillImage(oclGetProcAddress('clEnqueueFillImage', OCL_LibHandle));
    {$ENDIF}
    
    {$IFDEF CL_VERSION_1_0}
      clEnqueueCopyImage := TclEnqueueCopyImage(oclGetProcAddress('clEnqueueCopyImage', OCL_LibHandle));
      clEnqueueCopyImageToBuffer := TclEnqueueCopyImageToBuffer(oclGetProcAddress('clEnqueueCopyImageToBuffer', OCL_LibHandle));
      clEnqueueCopyBufferToImage := TclEnqueueCopyBufferToImage(oclGetProcAddress('clEnqueueCopyBufferToImage', OCL_LibHandle));
      clEnqueueMapBuffer := TclEnqueueMapBuffer(oclGetProcAddress('clEnqueueMapBuffer', OCL_LibHandle));
      clEnqueueMapImage := TclEnqueueMapImage(oclGetProcAddress('clEnqueueMapImage', OCL_LibHandle));
      clEnqueueUnmapMemObject := TclEnqueueUnmapMemObject(oclGetProcAddress('clEnqueueUnmapMemObject', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_2}
      clEnqueueMigrateMemObjects := TclEnqueueMigrateMemObjects(oclGetProcAddress('clEnqueueMigrateMemObjects', OCL_LibHandle));
    {$ENDIF}
    {$IFDEF CL_VERSION_1_0}
      clEnqueueNDRangeKernel := TclEnqueueNDRangeKernel(oclGetProcAddress('clEnqueueNDRangeKernel', OCL_LibHandle));
      clEnqueueTask := TclEnqueueTask(oclGetProcAddress('clEnqueueTask', OCL_LibHandle));
      clEnqueueNativeKernel := TclEnqueueNativeKernel(oclGetProcAddress('clEnqueueNativeKernel', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clEnqueueMarkerWithWaitList := TclEnqueueMarkerWithWaitList(oclGetProcAddress('clEnqueueMarkerWithWaitList', OCL_LibHandle));
      clEnqueueBarrierWithWaitList := TclEnqueueBarrierWithWaitList(oclGetProcAddress('clEnqueueBarrierWithWaitList', OCL_LibHandle));
      clSetPrintfCallback := TclSetPrintfCallback(oclGetProcAddress('clSetPrintfCallback', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_1_0}
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
        clEnqueueMarker := TclEnqueueMarker(oclGetProcAddress('clEnqueueMarker', OCL_LibHandle));
        clEnqueueWaitForEvents := TclEnqueueWaitForEvents(oclGetProcAddress('clEnqueueWaitForEvents', OCL_LibHandle));
        clEnqueueBarrier := TclEnqueueBarrier(oclGetProcAddress('clEnqueueBarrier', OCL_LibHandle));
      {$ENDIF}
    {$ENDIF}

    //Extension function access
    {$IFDEF CL_VERSION_1_0}
      {$IFDEF CL_USE_DEPRECATED_OPENCL_1_1_APIS}
        clGetExtensionFunctionAddress := TclGetExtensionFunctionAddress(oclGetProcAddress('clGetExtensionFunctionAddress', OCL_LibHandle));
      {$ENDIF}
    {$ENDIF}

    {$IFDEF CL_VERSION_1_2}
      clGetExtensionFunctionAddressForPlatform := TclGetExtensionFunctionAddressForPlatform(oclGetProcAddress('clGetExtensionFunctionAddressForPlatform', OCL_LibHandle));
    {$ENDIF}

    {$IFDEF CL_VERSION_2_0}
      clCreateCommandQueueWithProperties := TclCreateCommandQueueWithProperties(oclGetProcAddress('clCreateCommandQueueWithProperties', OCL_LibHandle));
      clCreatePipe := TclCreatePipe(oclGetProcAddress('clCreatePipe', OCL_LibHandle));
      clGetPipeInfo := TclGetPipeInfo(oclGetProcAddress('clGetPipeInfo', OCL_LibHandle));
      clSVMAlloc := TclSVMAlloc(oclGetProcAddress('clSVMAlloc', OCL_LibHandle));
      clSVMFree := TclSVMFree(oclGetProcAddress('clSVMFree', OCL_LibHandle));
      clCreateSamplerWithProperties := TclCreateSamplerWithProperties(oclGetProcAddress('clCreateSamplerWithProperties', OCL_LibHandle));
      clSetKernelArgSVMPointer := TclSetKernelArgSVMPointer(oclGetProcAddress('clSetKernelArgSVMPointer', OCL_LibHandle));
      clSetKernelExecInfo := TclSetKernelExecInfo(oclGetProcAddress('clSetKernelExecInfo', OCL_LibHandle));
      clEnqueueSVMFree := TclEnqueueSVMFree(oclGetProcAddress('clEnqueueSVMFree', OCL_LibHandle));
      clEnqueueSVMMemcpy := TclEnqueueSVMMemcpy(oclGetProcAddress('clEnqueueSVMMemcpy', OCL_LibHandle));
      clEnqueueSVMMemFill := TclEnqueueSVMMemFill(oclGetProcAddress('clEnqueueSVMMemFill', OCL_LibHandle));
      clEnqueueSVMMap := TclEnqueueSVMMap(oclGetProcAddress('clEnqueueSVMMap', OCL_LibHandle));
      clEnqueueSVMUnmap := TclEnqueueSVMUnmap(oclGetProcAddress('clEnqueueSVMUnmap', OCL_LibHandle));
    {$ENDIF}

      Result := True;
  end;
end;
{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}
{$ENDIF}

{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$REGION 'GetString'}{$ENDIF}
function GetString(const Status: TCL_int): AnsiString;
begin
  Result := '';
  case Status of
    CL_SUCCESS: Result := 'Success';
    CL_DEVICE_NOT_FOUND: Result := 'device not found';
    CL_DEVICE_NOT_AVAILABLE: Result := 'device not available';
    CL_COMPILER_NOT_AVAILABLE: Result := 'compiler not available';
    CL_MEM_OBJECT_ALLOCATION_FAILURE: Result := 'mem object allocation failure';
    CL_OUT_OF_RESOURCES: Result := 'out of resources';
    CL_OUT_OF_HOST_MEMORY: Result := 'out of host memory';
    CL_PROFILING_INFO_NOT_AVAILABLE: Result := 'profiling info not available';
    CL_MEM_COPY_OVERLAP: Result := 'mem copy overlap';
    CL_IMAGE_FORMAT_MISMATCH: Result := 'image format mismatch';
    CL_IMAGE_FORMAT_NOT_SUPPORTED: Result := 'image format not support';
    CL_BUILD_PROGRAM_FAILURE: Result := 'build program failure';
    CL_MAP_FAILURE: Result := 'map failure';
    CL_MISALIGNED_SUB_BUFFER_OFFSET: Result := 'misaligned sub buffer offset';
    CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST: Result := 'exec status error for events in wait list';
    CL_COMPILE_PROGRAM_FAILURE: Result := 'compile program failure';
    CL_LINKER_NOT_AVAILABLE: Result := 'linker not available';
    CL_LINK_PROGRAM_FAILURE: Result := 'link program failure';
    CL_DEVICE_PARTITION_FAILED: Result := 'device partition failed';
    CL_KERNEL_ARG_INFO_NOT_AVAILABLE: Result := 'kernel arg info not available';

    CL_INVALID_VALUE: Result := 'invalid value';
    CL_INVALID_DEVICE_TYPE: Result := 'invalid device type';
    CL_INVALID_PLATFORM: Result := 'invalid platform';
    CL_INVALID_DEVICE: Result := 'invalid device';
    CL_INVALID_CONTEXT: Result := 'invalid context';
    CL_INVALID_QUEUE_PROPERTIES: Result := 'invalid queue properties';
    CL_INVALID_COMMAND_QUEUE: Result := 'invalid command queue';
    CL_INVALID_HOST_PTR: Result := 'invalid host ptr';
    CL_INVALID_MEM_OBJECT: Result := 'invalid mem object';
    CL_INVALID_IMAGE_FORMAT_DESCRIPTOR: Result := 'invalid image format descriptor';
    CL_INVALID_IMAGE_SIZE: Result := 'invalid image size';
    CL_INVALID_SAMPLER: Result := 'invalid sampler';
    CL_INVALID_BINARY: Result := 'invalid binary';
    CL_INVALID_BUILD_OPTIONS: Result := 'invalid build options';
    CL_INVALID_PROGRAM: Result := 'invalid program';
    CL_INVALID_PROGRAM_EXECUTABLE: Result := 'invalid program executable';
    CL_INVALID_KERNEL_NAME: Result := 'invalid kernel name';
    CL_INVALID_KERNEL_DEFINITION: Result := 'invalid kernel definition';
    CL_INVALID_KERNEL: Result := 'invalid kernel';
    CL_INVALID_ARG_INDEX: Result := 'invalid arg index';
    CL_INVALID_ARG_VALUE: Result := 'invalid arg value';
    CL_INVALID_ARG_SIZE: Result := 'invalid arg size';
    CL_INVALID_KERNEL_ARGS: Result := 'invalid kernel args';
    CL_INVALID_WORK_DIMENSION: Result := 'invalid work dimension';
    CL_INVALID_WORK_GROUP_SIZE: Result := 'invalid work group size';
    CL_INVALID_WORK_ITEM_SIZE: Result := 'invalid work item size';
    CL_INVALID_GLOBAL_OFFSET: Result := 'invalid global offset';
    CL_INVALID_EVENT_WAIT_LIST: Result := 'invalid event wait list';
    CL_INVALID_EVENT: Result := 'invalid event';
    CL_INVALID_OPERATION: Result := 'invalid operation';
    CL_INVALID_GL_OBJECT: Result := 'invalid gl object';
    CL_INVALID_BUFFER_SIZE: Result := 'invalid buffer size';
    CL_INVALID_MIP_LEVEL: Result := 'invalid mip level';
    CL_INVALID_GLOBAL_WORK_SIZE: Result := 'invalid global work size';
    CL_INVALID_PROPERTY: Result := 'invalid property';
    CL_INVALID_IMAGE_DESCRIPTOR: Result := 'invalid image descriptor';
    CL_INVALID_COMPILER_OPTIONS: Result := 'invalid compiler options';
    CL_INVALID_LINKER_OPTIONS: Result := 'invalid linker options';
    CL_INVALID_DEVICE_PARTITION_COUNT: Result := 'invalid device partiotion count';
  end;
end;
{$IFNDEF DEFINE_REGION_NOT_IMPLEMENTED}{$ENDREGION}{$ENDIF}

initialization
{$IFDEF DEFINE_8087CW_NOT_IMPLEMENTED}
asm
  MOV     Default8087CW,AX
  FLDCW   Default8087CW
end;
{$ELSE}
  Set8087CW($133F);
{$ENDIF}
end.
