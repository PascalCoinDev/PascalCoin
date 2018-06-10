(*
** Copyright 1998-2002, NVIDIA Corporation.
** All Rights Reserved.
**
** THE INFORMATION CONTAINED HEREIN IS PROPRIETARY AND CONFIDENTIAL TO
** NVIDIA, CORPORATION.  USE, REPRODUCTION OR DISCLOSURE TO ANY THIRD PARTY
** IS SUBJECT TO WRITTEN PRE-APPROVAL BY NVIDIA, CORPORATION.
**
**
*)
(********************************************)
(*                                          *)
(*     OpenCL1.1 and Delphi and Windows     *)
(*                                          *)
(*      created by      : Maksym Tymkovych  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.03              *)
(*      file name       : clext.pas         *)
(*      last modify     : 13.02.10          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2011*****)
unit CLExt;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

const

 CL_NV_DEVICE_COMPUTE_CAPABILITY_MAJOR     =  $4000;
 CL_NV_DEVICE_COMPUTE_CAPABILITY_MINOR     =  $4001;
 CL_NV_DEVICE_REGISTERS_PER_BLOCK          =  $4002;
 CL_NV_DEVICE_WARP_SIZE                    =  $4003;
 CL_NV_DEVICE_GPU_OVERLAP                  =  $4004;
 CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT          =  $4005;
 CL_NV_DEVICE_INTEGRATED_MEMORY            =  $4006;

implementation

end.