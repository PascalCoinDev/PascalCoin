(********************************************)
(*                                          *)
(*                DelphiCL                  *)
(*                                          *)
(*      created by      : Maksym Tymkovych  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.07              *)
(*      file name       : DelphiCL.pas      *)
(*      last modify     : 10.12.11          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2011*****)

unit DelphiCL;

interface

{$INCLUDE OpenCL.inc}
{$INCLUDE DelphiCL.inc}

uses
  CL,
  CL_GL,
  Windows,
  SysUtils,
  dglOpenGL,
  CL_Platform;

const
  DCL_BUILD_OPTION_SINGLE_PRECISION_CONSTANT = '-cl-single-precision-constant ';
  DCL_BUILD_OPTION_DENORMS_ARE_ZERO          = '-cl-denorms-are-zero ';
  DCL_BUILD_OPTION_OPT_DISABLE               = '-cl-opt-disable ';
  DCL_BUILD_OPTION_STRICT_ALIASING           = '-cl-strict-aliasing ';
  DCL_BUILD_OPTION_MAD_ENABLE                = '-cl-mad-enable ';
  DCL_BUILD_OPTION_NO_SIGNED_ZEROS           = '-cl-no-signed-zeros ';
  DCL_BUILD_OPTION_UNSAFE_MATH_OPTIMIZATIONS = '-cl-unsafe-math-optimizations ';
  DCL_BUILD_OPTION_FINITE_MATH_ONLY          = '-cl-finite-math-only ';
  DCL_BUILD_OPTION_FAST_RELAXED_MATH         = '-cl-fast-relaxed-math ';
  DCL_BUILD_OPTION_W                         = '-w ';
  DCL_BUILD_OPTION_WERROR                    = '-Werror ';
  DCL_BUILD_OPTION_STD                       = '-cl-std=';

{$IFDEF LOGGING}
var
  DCLFileLOG: TextFile;
  procedure WriteLog(const Str: AnsiString);
{$ENDIF}

type

  TDCLMemFlags = (mfReadWrite, mfWriteOnly, mfReadOnly, mfUseHostPtr, mfAllocHostPtr, mfCopyHostPtr);
  TDCLMemFlagsSet = set of TDCLMemFlags;

  PDCLBuffer = ^TDCLBuffer;
  TDCLBuffer = class
  private
    FMem: PCL_mem;
    FStatus: TCL_int;
    FSize: TSize_t;
  protected
    constructor Create(const Context: PCL_context; const Flags: TDCLMemFlagsSet; const Size: TSize_t; const Data: Pointer=nil);
    constructor CreateFromGL(const Context: PCL_context; const Flags: TDCLMemFlagsSet; const Data: Pointer=nil);
  public
    destructor Destroy();override;
    property Size: TSize_t read FSize;
    property Status: TCL_int read FStatus;
  end;

  PDCLImage2D = ^TDCLImage2D;
  TDCLImage2D = class
  private
    FMem: PCL_mem;
    FStatus: TCL_int;
    FFormat: TCL_image_format;
    FRowPitch: TSize_t;
    FWidth,
    FHeight: TSize_t;
  protected
    constructor Create(const Context: PCL_context; const Flags: TDCLMemFlagsSet; const Format: PCL_image_format; const Width, Height: TSize_t; const RowPitch: TSize_t = 0; const Data: Pointer = nil);
    constructor CreateFromGL(const Context: PCL_context; const Flags: TDCLMemFlagsSet; const Texture: TGLuint);
  public
    destructor Destroy(); override;
    property Width: TSize_t read FWidth;
    property Height: TSize_t read FHeight;
    property RowPitch: TSize_t read FRowPitch;
    property Status: TCL_int read FStatus;
  end;

  TDCLCommandQueueProperties = (cqpOutOfOrderExecModeEnable);
  TDCLCommandQueuePropertiesSet = set of TDCLCommandQueueProperties;

  PDCLKernel = ^TDCLKernel;
  TDCLKernel = class
  private
    FKernel: PCL_kernel;
    FStatus: TCL_int;
  protected
    constructor Create(const Program_: PCL_program; const KernelName: PAnsiChar);
    function GetFunctionName(): AnsiString;
    function GetNumArgs(): TCL_uint;
  public
    property Status: TCL_int read FStatus;
    property FunctionName: AnsiString read GetFunctionName;
    property NumArgs: TCL_uint read GetNumArgs;
    procedure SetArg(const Index: TCL_uint; const Size: TSize_t; const Value: Pointer); overload;
    procedure SetArg(const Index: TCL_uint; const Value: TDCLBuffer); overload;
    procedure SetArg(const Index: TCL_uint; const Value: TDCLImage2D); overload;
    destructor Destroy(); override;
  end;

  PDCLCommandQueue = ^TDCLCommandQueue;
  TDCLCommandQueue = class
  private
    FCommandQueue: PCL_command_queue;
    FStatus: TCL_int;
    FProperties: TDCLCommandQueuePropertiesSet;
    {$IFDEF PROFILING}
    FExecuteTime: TCL_ulong;
    {$ENDIF}
    constructor Create(const Device_Id: PCL_device_id; const Context: PCL_context; const Properties: TDCLCommandQueuePropertiesSet = []);
  public
    procedure ReadBuffer(const Buffer: TDCLBuffer; const Size: TSize_t; const Data: Pointer);
    procedure WriteBuffer(const Buffer: TDCLBuffer; const Size: TSize_t; const Data: Pointer);
    procedure ReadImage2D(const Image: TDCLImage2D; const Data: Pointer);
    procedure WriteImage2D(const Image: TDCLImage2D; const Width, Height: TSize_t; const Data: Pointer);
    procedure Execute(const Kernel: TDCLKernel; const Size: TSize_t); overload;
    procedure Execute(const Kernel: TDCLKernel; //const Device: PCL_device_id;
                      const Size: array of TSize_t);overload;


    procedure AcquireGLObject(const Buffer: TDCLBuffer);overload;
    procedure AcquireGLObject(const Image2D: TDCLImage2D);overload;
    procedure ReleaseGLObject(const Buffer: TDCLBuffer);overload;
    procedure ReleaseGLObject(const Image2D: TDCLImage2D);overload;

    property Status: TCL_int read FStatus;
    property Properties: TDCLCommandQueuePropertiesSet read FProperties;
    {$IFDEF PROFILING}
    property ExecuteTime: TCL_ulong read FExecuteTime;
    {$ENDIF}
    destructor Destroy(); override;
  end;

  TArraySize_t = array of TSize_t;

  PDCLProgram = ^TDCLProgram;
  TDCLProgram = class
  private
    FProgram: PCL_program;
    FStatus: TCL_int;
    FSource: PAnsiChar;
    FLog: AnsiString;
    FBinarySizes: TSize_t;
    FBinaries: array of array of AnsiChar;
  protected
    constructor Create(const Device: PCL_device_id; const Context: PCL_context; const Source: PPAnsiChar; const Options: PAnsiChar = nil);
  public
    property BinarySizes: TSize_t read FBinarySizes;
    property Source: PAnsiChar read FSource;
    property Status: TCL_int read FStatus;
    property Log: AnsiString read FLog;
    function CreateKernel(const KernelName: PAnsiChar): TDCLKernel;
    procedure SaveToFile(const FileName: AnsiString);
    destructor Destroy(); override;
  end;

  PDCLContext = ^TDCLContext;
  TDCLContext = class
  private
    FContext: PCL_context;
    FStatus: TCL_int;
    FNumDevices: TCL_uint;
  protected
    //property Context: PCL_context read FContext;
  public
    constructor Create(Device_id: PCL_device_id);
    constructor CreateGL(Device_id: PCL_device_id);
    property Status: TCL_int read FStatus;
    property NumDevices: TCL_uint read FNumDevices;
    destructor Destroy(); override;
  end;

  TDCLDeviceFPConfig = ({$IFDEF CL_VERSION_1_0}
                        dfpcDenorm, dfpcInfNan, dfpcRoundToNearest, dfpcRoundToZero,
                        dfpcRoundToInf, dfpcFMA
                        {$ENDIF}
                        {$IFDEF CL_VERSION_1_1}
                        , dfpcSoftFloat
                        {$ENDIF}
                        {$IFDEF CL_VERSION_1_2}
                        , dfpcCorrectlyRoundedDivideSqrt
                        {$ENDIF}
                        );
  TDCLDeviceFPConfigSet = Set of TDCLDeviceFPConfig;

  TDCLDeviceExecutionCapabilities = ({$IFDEF CL_VERSION_1_0}decExecKernel, decExecNativeKernel{$ENDIF});
  TDCLDeviceExecutionCapabilitiesSet = set of TDCLDeviceExecutionCapabilities;

  TDCLDeviceMemCacheType = ({$IFDEF CL_VERSION_1_0}
                            dmctNone, dmctReadOnlyCache, dmctWriteOnlyCache
                            {$ENDIF});
  TDCLDeviceLocalMemType = ({$IFDEF CL_VERSION_1_0}
                            dlmtLocal, dlmtGlobal
                            {$ENDIF});

  PDCLDevice = ^TDCLDevice;
  TDCLDevice = class
  //private
    FDevice_id: PCL_device_id;
  private
    FStatus: TCL_int;

    FName: AnsiString;
    FVendor: AnsiString;
    FVersion: AnsiString;
    FProfile: AnsiString;

    FIsCPU: Boolean;
    FIsGPU: Boolean;
    FIsAccelerator: Boolean;
    FIsDefault: Boolean;

    FMaxWorkGroupSize: TSize_t;

    FNativeVectorPreferredChar: TCL_uint;
    FNativeVectorPreferredShort: TCL_uint;
    FNativeVectorPreferredInt: TCL_uint;
    FNativeVectorPreferredLong: TCL_uint;
    FNativeVectorPreferredFloat: TCL_uint;
    FNativeVectorPreferredDouble: TCL_uint;
    FNativeVectorPreferredHalf: TCL_uint;
    FNativeVectorWidthChar: TCL_uint;
    FNativeVectorWidthShort: TCL_uint;
    FNativeVectorWidthInt: TCL_uint;
    FNativeVectorWidthLong: TCL_uint;
    FNativeVectorWidthFloat: TCL_uint;
    FNativeVectorWidthDouble: TCL_uint;
    FNativeVectorWidthHalf: TCL_uint;

    FMaxClockFrequency: TCL_uint;
    FAddressBits: TCL_uint;
    FMaxMemAllocSize: TCL_ulong;

    FIsImageSupport: Boolean;

    FMaxReadImageArgs: TCL_uint;
    FMaxWriteImageArgs: TCL_uint;
    FImage2DMaxWidth: TSize_t;
    FImage2DMaxHeight: TSize_t;
    FImage3DMaxWidth: TSize_t;
    FImage3DMaxHeight: TSize_t;
    FImage3DMaxDepth: TSize_t;
    FMaxSamplers: TCL_uint;
    FMaxParameterSize: TSize_t;
    FMemBaseAddrAlign: TCL_uint;
    FMinDataTypeAlignSize: TCL_uint;

    FGlobalMemCacheLineSize: TCL_uint;
    FGlobalMemCacheSize: TCL_ulong;
    FGlobalMemSize: TCL_ulong;
    FMaxConstantBufferSize: TCL_ulong;
    FMaxConstantArgs: TCL_uint;

    FLocalMemSize: TCL_ulong;
    FIsErrorCorrectionSupport: Boolean;
    FIsHostUnifiedMemory: Boolean;
    FProfilingTimerResolution: TSize_t;
    FIsEndianLittle: Boolean;
    FIsAvailable: Boolean;
    FIsCompilerAvailable: Boolean;

    FVendorId: TCL_uint;
    FMaxComputeUnits: TCL_uint;
    FMaxWorkItemDimensions: TCL_uint;
    FExtensionsString: AnsiString;
    FOpenCLCVersion: AnsiString;
    FDriverVersion: AnsiString;

    FExtensionsCount: TSize_t;
    FExtensions: array of AnsiString;

    FContext: TDCLContext;

    FFPConfigSet: TDCLDeviceFPConfigSet;
    FExecutionCapabilities: TDCLDeviceExecutionCapabilitiesSet;
    FGlobalMemCacheType: TDCLDeviceMemCacheType;

    FLocalMemType: TDCLDeviceLocalMemType;

    function GetExtensions(const Index: TSize_t): AnsiString;
    function IsPresentExtension(const ExtensionName: AnsiString): Boolean;
    function IsPresentInFPConfig(const FPConfig: TDCLDeviceFPConfig): Boolean;
  protected
    constructor Create(Device_id: PCL_device_id);
    property Device_id: PCL_device_id read FDevice_id;
  public
    property Status: TCL_int read FStatus;

    property Name: AnsiString read FName;
    property Vendor: AnsiString read FVendor;
    property Version: AnsiString read FVersion;
    property Profile: AnsiString read FProfile;

    property IsCPU: Boolean read FIsCPU;
    property IsGPU: Boolean read FIsGPU;
    property IsAccelerator: Boolean read FIsAccelerator;
    property IsDefault: Boolean read FIsDefault;

    property MaxWorkGroupSize: TSize_t read FMaxWorkGroupSize;

    property NativeVectorPreferredChar: TCL_uint read FNativeVectorPreferredChar;
    property NativeVectorPreferredShort: TCL_uint read FNativeVectorPreferredShort;
    property NativeVectorPreferredInt: TCL_uint read FNativeVectorPreferredInt;
    property NativeVectorPreferredLong: TCL_uint read FNativeVectorPreferredLong;
    property NativeVectorPreferredFloat: TCL_uint read FNativeVectorPreferredFloat;
    property NativeVectorPreferredDouble: TCL_uint read FNativeVectorPreferredDouble;
    property NativeVectorPreferredHalf: TCL_uint read FNativeVectorPreferredHalf;
    property NativeVectorWidthChar: TCL_uint read FNativeVectorWidthChar;
    property NativeVectorWidthShort: TCL_uint read FNativeVectorWidthShort;
    property NativeVectorWidthInt: TCL_uint read FNativeVectorWidthInt;
    property NativeVectorWidthLong: TCL_uint read FNativeVectorWidthLong;
    property NativeVectorWidthFloat: TCL_uint read FNativeVectorWidthFloat;
    property NativeVectorWidthDouble: TCL_uint read FNativeVectorWidthDouble;
    property NativeVectorWidthHalf: TCL_uint read FNativeVectorWidthHalf;

    property MaxClockFrequency: TCL_uint  read FMaxClockFrequency;
    property AddressBits: TCL_uint  read FAddressBits;
    property MaxMemAllocSize: TCL_ulong  read FMaxMemAllocSize;

    property IsImageSupport: Boolean read FIsImageSupport;

    property MaxReadImageArgs: TCL_uint read FMaxReadImageArgs;
    property MaxWriteImageArgs: TCL_uint read FMaxWriteImageArgs;
    property Image2DMaxWidth: TSize_t read FImage2DMaxWidth;
    property Image2DMaxHeight: TSize_t read FImage2DMaxHeight;
    property Image3DMaxWidth: TSize_t read FImage3DMaxWidth;
    property Image3DMaxHeight: TSize_t read FImage3DMaxHeight;
    property Image3DMaxDepth: TSize_t read FImage3DMaxDepth;
    property MaxSamplers: TCL_uint read FMaxSamplers;
    property MaxParameterSize: TSize_t read FMaxParameterSize;
    property MemBaseAddrAlign: TCL_uint read FMemBaseAddrAlign;
    property MinDataTypeAlignSize: TCL_uint read FMinDataTypeAlignSize;

    property GlobalMemCacheLineSize: TCL_uint read FGlobalMemCacheLineSize;
    property GlobalMemCacheSize: TCL_ulong read FGlobalMemCacheSize;
    property GlobalMemSize: TCL_ulong read FGlobalMemSize;
    property MaxConstantBufferSize: TCL_ulong read FMaxConstantBufferSize;
    property MaxConstantArgs: TCL_uint read FMaxConstantArgs;

    property LocalMemSize: TCL_ulong read FLocalMemSize;
    property IsErrorCorrectionSupport: Boolean read FIsErrorCorrectionSupport;
    property IsHostUnifiedMemory: Boolean read FIsHostUnifiedMemory;
    property ProfilingTimerResolution: TSize_t read FProfilingTimerResolution;
    property IsEndianLittle: Boolean read FIsEndianLittle;
    property IsAvailable: Boolean read FIsAvailable;
    property IsCompilerAvailable: Boolean read FIsCompilerAvailable;

    property VendorId: TCL_uint read FVendorId;
    property MaxComputeUnits: TCL_uint read FMaxComputeUnits;
    property MaxWorkItemDimensions: TCL_uint read FMaxWorkItemDimensions;

    property DriverVersion: AnsiString read FDriverVersion;
    property OpenCLCVersion: AnsiString read FOpenCLCVersion;
    property ExtensionsString: AnsiString read FExtensionsString;

    property Context: TDCLContext read FContext;
    function CreateContext(): TDCLContext;
    function CreateContextGL(): TDCLContext;
    function CreateCommandQueue(const properties: TDCLCommandQueuePropertiesSet = []): TDCLCommandQueue;overload;
    function CreateCommandQueue(const context: TDCLContext; const properties: TDCLCommandQueuePropertiesSet = []): TDCLCommandQueue;overload;
    function CreateBuffer(const Size: TSize_t; const Data: Pointer = nil; const flags: TDCLMemFlagsSet = [mfReadWrite]): TDCLBuffer;

    function CreateFromGLBuffer(const Data: Pointer = nil; const flags: TDCLMemFlagsSet = [mfWriteOnly]): TDCLBuffer;

    function CreateImage2D(const Format: PCL_image_format; const Width, Height, RowPitch: TSize_t; const Data: Pointer = nil; const flags: TDCLMemFlagsSet = [mfReadWrite]): TDCLImage2D;
    function CreateFromGLImage2D(const Texture: TGLuint; const Flags: TDCLMemFlagsSet = [mfWriteOnly]): TDCLImage2D;

    function CreateProgram(const Source: PPAnsiChar; const Options: PAnsiChar = nil): TDCLProgram; overload;
    function CreateProgram(const FileName: String; const Options: PAnsiChar = nil): TDCLProgram; overload;

    property ExtensionsCount: TSize_t read FExtensionsCount;
    property Extensions[const Index: TSize_t]: AnsiString read GetExtensions;
    property IsSupportedExtension[const Index: AnsiString]: Boolean read IsPresentExtension;

    property FPConfig[const Index: TDCLDeviceFPConfig]: Boolean read IsPresentInFPConfig;
    property GlobalMemCacheType: TDCLDeviceMemCacheType read FGlobalMemCacheType;
    destructor Destroy(); override;
  end;

  PDCLPlatform = ^TDCLPlatform;
  TDCLPlatform = class
  private
    FPlatform_id: PCL_platform_id;
    FProfile: AnsiString;
    FVersion: AnsiString;
    FName: AnsiString;
    FVendor: AnsiString;
    FExtensionsString: AnsiString;
    FStatus: TCL_int;
    FDevices: array of TDCLDevice;
    FDeviceCount: TCL_uint;

    FCPUs,
    FGPUs,
    FAccelerators: array of TCL_uint;
    FCPUCount,
    FGPUCount,
    FAcceleratorCount: TCL_uint;

    FExtensionsCount: TSize_t;
    FExtensions: array of AnsiString;
    function GetDevice(Index: TCL_uint): PDCLDevice;
    function GetCPU(Index: TCL_uint): PDCLDevice;
    function GetGPU(Index: TCL_uint): PDCLDevice;
    function GetAccelerator(Index: TCL_uint): PDCLDevice;
    function GetExtensions(Index: TSize_t): AnsiString;
    function IsPresentExtension(const ExtensionName: AnsiString): Boolean;

    function GetDeviceWithMaxClockFrequency(): PDCLDevice;
    function GetDeviceWithMaxComputeUnits(): PDCLDevice;

    function GetDeviceWithMaxGlobalMemCacheLineSize(): PDCLDevice;
    function GetDeviceWithMaxGlobalMemCacheSize(): PDCLDevice;
    function GetDeviceWithMaxGlobalMemSize(): PDCLDevice;

    function GetDeviceWithMaxImage2DWidth(): PDCLDevice;
    function GetDeviceWithMaxImage2DHeight(): PDCLDevice;
    function GetDeviceWithMaxImage3DWidth(): PDCLDevice;
    function GetDeviceWithMaxImage3DHeight(): PDCLDevice;
    function GetDeviceWithMaxImage3DDepth(): PDCLDevice;

    function GetDeviceWithMaxLocalMemSize(): PDCLDevice;
    function GetDeviceWithMaxConstantArgs(): PDCLDevice;
    function GetDeviceWithMaxConstantBufferSize(): PDCLDevice;
    function GetDeviceWithMaxMemAllocSize(): PDCLDevice;
    function GetDeviceWithMaxParameterSize(): PDCLDevice;
    function GetDeviceWithMaxReadImageArgs(): PDCLDevice;
    function GetDeviceWithMaxSamplers(): PDCLDevice;
    function GetDeviceWithMaxWorkGroupSize(): PDCLDevice;
    function GetDeviceWithMaxWorkItemDimensions(): PDCLDevice;
    function GetDeviceWithMaxWriteImageArgs(): PDCLDevice;
  public
    constructor Create(Platform_id: PCL_platform_id);
    property Profile: AnsiString read FProfile;
    property Version: AnsiString read FVersion;
    property Name: AnsiString read FName;
    property Vendor: AnsiString read FVendor;
    property ExtensionsString: AnsiString read FExtensionsString;

    property DeviceCount: TCL_uint read FDeviceCount;
    property CPUCount: TCL_uint read FCPUCount;
    property GPUCount: TCL_uint read FGPUCount;
    property AcceleratorCount: TCL_uint read FAcceleratorCount;

    property Status: TCL_int read FStatus;
    property Devices[Index: TCL_uint]: PDCLDevice read GetDevice;
    property CPUs[Index: TCL_uint]: PDCLDevice read GetCPU;
    property GPUs[Index: TCL_uint]: PDCLDevice read GetGPU;
    property Accelerators[Index: TCL_uint]: PDCLDevice read GetAccelerator;
    property ExtensionsCount: TSize_t read FExtensionsCount;
    property Extensions[Index: TSize_t]: AnsiString read GetExtensions;
    property IsSupportedExtension[const Index: AnsiString]: Boolean read IsPresentExtension;

    property DeviceWithMaxClockFrequency: PDCLDevice read GetDeviceWithMaxClockFrequency;
    property DeviceWithMaxComputeUnits: PDCLDevice read GetDeviceWithMaxComputeUnits;
    property DeviceWithMaxGlobalMemCacheLineSize: PDCLDevice read GetDeviceWithMaxGlobalMemCacheLineSize;
    property DeviceWithMaxGlobalMemCacheSize: PDCLDevice read GetDeviceWithMaxGlobalMemCacheSize;
    property DeviceWithMaxGlobalMemSize: PDCLDevice read GetDeviceWithMaxGlobalMemSize;
    property DeviceWithMaxImage2DWidth: PDCLDevice read GetDeviceWithMaxImage2DWidth;
    property DeviceWithMaxImage2DHeight: PDCLDevice read GetDeviceWithMaxImage2DHeight;
    property DeviceWithMaxImage3DWidth: PDCLDevice read GetDeviceWithMaxImage3DWidth;
    property DeviceWithMaxImage3DHeight: PDCLDevice read GetDeviceWithMaxImage3DHeight;
    property DeviceWithMaxImage3DDepth: PDCLDevice read GetDeviceWithMaxImage3DDepth;
    property DeviceWithMaxLocalMemSize: PDCLDevice read GetDeviceWithMaxLocalMemSize;
    property DeviceWithMaxConstantArgs: PDCLDevice read GetDeviceWithMaxConstantArgs;
    property DeviceWithMaxConstantBufferSize: PDCLDevice read GetDeviceWithMaxConstantBufferSize;
    property DeviceWithMaxMemAllocSize: PDCLDevice read GetDeviceWithMaxMemAllocSize;
    property DeviceWithMaxParameterSize: PDCLDevice read GetDeviceWithMaxParameterSize;
    property DeviceWithMaxReadImageArgs: PDCLDevice read GetDeviceWithMaxReadImageArgs;
    property DeviceWithMaxSamplers: PDCLDevice read GetDeviceWithMaxSamplers;
    property DeviceWithMaxWorkGroupSize: PDCLDevice read GetDeviceWithMaxWorkGroupSize;
    property DeviceWithMaxWorkItemDimensions: PDCLDevice read GetDeviceWithMaxWorkItemDimensions;
    property DeviceWithMaxWriteImageArgs: PDCLDevice read GetDeviceWithMaxWriteImageArgs;
    destructor Destroy(); override;
  end;

  PDCLPlatforms = ^TDCLPlatforms;
  TDCLPlatforms = class
  private
    FPlatforms: array of TDCLPlatform;
    FPlatformCount: TCL_uint;
    FStatus: TCL_int;
    function GetPlatform(Index: TCL_uint): PDCLPlatform;
  public
    constructor Create();
    property PlatformCount: TCL_uint read FPlatformCount;
    property Status: TCL_int read FStatus;
    property Platforms[Index: TCL_uint]: PDCLPlatform read GetPlatform;
    destructor Destroy(); override;
  end;

implementation

function UpperCase(const S: AnsiString): AnsiString;
var
  Ch: AnsiChar;
  L: Integer;
  Source, Dest: PAnsiChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

{// Osak Alexey for 64 bit value
function IntToStr( Value : Integer ) : AnsiString;
begin
  Str( Value, Result );
end;
}

{$IFDEF LOGGING}
  procedure WriteLog(const Str: AnsiString);
  begin
    Writeln(DCLFileLOG, Str);
    Flush(DCLFileLOG);
  end;
{$ENDIF}

{ TDCLPlatforms }

constructor TDCLPlatforms.Create;
var
  platforms: array of PCL_platform_id;
  i: integer;
begin
  FStatus := clGetPlatformIDs(0, nil, @FPlatformCount);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformIDs: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('PlatformCount: ' + IntToStr(FPlatformCount) + ';');
  {$ENDIF}
  if FStatus=CL_SUCCESS then
  begin
    if FPlatformCount>0 then
    begin
      SetLength(platforms, FPlatformCount);
      SetLength(FPlatforms, FPlatformCount);
      FStatus := clGetPlatformIDs(FPlatformCount, @platforms[0], nil);
      {$IFDEF LOGGING}
        WriteLog('clGetPlatformIDs: ' + GetString(FStatus) + ';');
      {$ENDIF}
      for i := 0 to FPlatformCount-1 do
      begin
        FPlatforms[i] := TDCLPlatform.Create(platforms[i]);
      end;
      SetLength(platforms, 0);
    end;
  end;
end;

destructor TDCLPlatforms.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPlatformCount-1 do
  begin
    //FPlatforms[i].Free();
    FreeAndNil(FPlatforms[i]);
  end;
  SetLength(FPlatforms, 0);
  inherited;
end;

function TDCLPlatforms.GetPlatform(Index: TCL_uint): PDCLPlatform;
begin
  if (Index<FPlatformCount)then Result := @FPlatforms[Index]
  else Result := nil;
end;

{ TDCLPlatform }

constructor TDCLPlatform.Create(Platform_id: PCL_platform_id);
var
  Size: TSize_t;
  devices: array of PCL_device_id;
  i, current, previous: integer;

begin
  inherited Create();
  FPlatform_id := Platform_id;

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_PROFILE, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FProfile, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_PROFILE, Size, @FProfile[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PLATFORM_PROFILE: ' + FProfile + ';');
  {$ENDIF}

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VERSION, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FVersion, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VERSION, Size, @FVersion[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PLATFORM_VERSION: ' + FVersion + ';');
  {$ENDIF}

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_NAME, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FName, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_NAME, Size, @FName[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PLATFORM_NAME: ' + FName + ';');
  {$ENDIF}

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VENDOR, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FVendor, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VENDOR, Size, @FVendor[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CCL_PLATFORM_VENDOR: ' + FVendor + ';');
  {$ENDIF}

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_EXTENSIONS, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FExtensionsString, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_EXTENSIONS, Size, @FExtensionsString[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetPlatformInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PLATFORM_EXTENSIONS: ' + FExtensionsString + ';');
  {$ENDIF}

  FExtensionsCount := 0;
  i := 1;
  while (i<=Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[i]=' ') or (FExtensionsString[i]=#0)) then Inc(FExtensionsCount);
    inc(i);
  end;
  SetLength(FExtensions, FExtensionsCount);
  previous := 1;
  current := 1;
  i := 0;
  while (current<=Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[current]=' ') or (FExtensionsString[current]=#0)) then
    begin
      FExtensions[i] := UpperCase( Copy(FExtensionsString, previous, current-previous-1));
      previous := current + 1;
      inc(i);
    end;
    inc(current);
  end;

  FStatus := clGetDeviceIDs(FPlatform_id, CL_DEVICE_TYPE_ALL, 0, nil, @FDeviceCount);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceIDs: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('FDeviceCount: ' + IntToStr(FDeviceCount) + ';');
  {$ENDIF}

  FCPUCount := 0;
  FGPUCount := 0;
  FAcceleratorCount := 0;
  if FDeviceCount>0 then
  begin
    SetLength(devices, FDeviceCount);
    FStatus := clGetDeviceIDs(FPlatform_id, CL_DEVICE_TYPE_ALL, FDeviceCount, @devices[0], nil);
    {$IFDEF LOGGING}
      WriteLog('clGetDeviceIDs: ' + GetString(FStatus) + ';');
    {$ENDIF}
    SetLength(FDevices, FDeviceCount);
    for i := 0 to FDeviceCount-1 do
    begin
      {$IFDEF LOGGING}
        WriteLog('FDevice: ' + IntToStr(i) + ';');
      {$ENDIF}
      FDevices[i] := TDCLDevice.Create(devices[i]);
      if FDevices[i].IsCPU then
      begin
        Inc(FCPUCount);
        SetLength(FCPUs, FCPUCount);
        FCPUs[FCPUCount-1] := i;
      end;
      if FDevices[i].IsGPU then
      begin
        Inc(FGPUCount);
        SetLength(FGPUs, FGPUCount);
        FGPUs[FGPUCount-1] := i;
      end;

      if FDevices[i].IsAccelerator then
      begin
        Inc(FAcceleratorCount);
        SetLength(FAccelerators, FAcceleratorCount);
        FAccelerators[FAcceleratorCount-1] := i;
      end;
    end;
    SetLength(devices, 0);
  end;

end;

destructor TDCLPlatform.Destroy;
var
  i: integer;
begin
  SetLength(FExtensions, 0);
  FExtensionsString := '';
  FProfile := '';
  FVersion := '';
  FName := '';
  FVendor := '';

  for i := 0 to FDeviceCount-1 do
  begin
    FreeAndNil(FDevices[i]);
    //FDevices[i].Free();
  end;

  SetLength(FCPUs, 0);
  SetLength(FGPUs, 0);
  SetLength(FAccelerators, 0);

  SetLength(FDevices, 0);
  inherited;
end;

function TDCLPlatform.GetAccelerator(Index: TCL_uint): PDCLDevice;
begin
  if Index<FAcceleratorCount then Result := @FDevices[FAccelerators[Index]]
  else Result := nil;
end;

function TDCLPlatform.GetCPU(Index: TCL_uint): PDCLDevice;
begin
  if Index<FCPUCount then Result := @FDevices[FCPUs[Index]]
  else Result := nil;
end;

function TDCLPlatform.GetDevice(Index: TCL_uint): PDCLDevice;
begin
  if (Index<FDeviceCount)then Result := @FDevices[Index]
  else Result := nil;
end;

function TDCLPlatform.GetDeviceWithMaxClockFrequency: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxClockFrequency;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxComputeUnits: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxComputeUnits;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxConstantArgs: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxConstantArgs;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxConstantBufferSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_ulong;
  begin
    Result := Device.MaxConstantBufferSize;
  end;
var
  i: Integer;
  MaxValue: TCL_ulong;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemCacheLineSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.GlobalMemCacheLineSize;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemCacheSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_ulong;
  begin
    Result := Device.GlobalMemCacheSize;
  end;
var
  i: Integer;
  MaxValue: TCL_ulong;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_ulong;
  begin
    Result := Device.GlobalMemSize;
  end;
var
  i: Integer;
  MaxValue: TCL_ulong;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage2DHeight: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.Image2DMaxHeight;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage2DWidth: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.Image2DMaxWidth;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DDepth: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.Image3DMaxDepth;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DHeight: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.Image3DMaxHeight;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DWidth: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.Image3DMaxWidth;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxLocalMemSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_ulong;
  begin
    Result := Device.LocalMemSize;
  end;
var
  i: Integer;
  MaxValue: TCL_ulong;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxMemAllocSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_ulong;
  begin
    Result := Device.MaxMemAllocSize;
  end;
var
  i: Integer;
  MaxValue: TCL_ulong;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxParameterSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.MaxParameterSize;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxReadImageArgs: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxReadImageArgs;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxSamplers: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxSamplers;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWorkGroupSize: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TSize_t;
  begin
    Result := Device.MaxWorkGroupSize;
  end;
var
  i: Integer;
  MaxValue: TSize_t;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWorkItemDimensions: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxWorkItemDimensions;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWriteImageArgs: PDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): TCL_uint;
  begin
    Result := Device.MaxWriteImageArgs;
  end;
var
  i: Integer;
  MaxValue: TCL_uint;
  MaxValuePos: TCL_uint;
begin
  if FDeviceCount=0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount-1 do
  begin
    if GetParameterDevice(FDevices[i])>MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := @FDevices[MaxValuePos];
end;

function TDCLPlatform.GetExtensions(Index: TSize_t): AnsiString;
begin
  if Index<FExtensionsCount then Result := FExtensions[Index]
  else Result := '';
end;

function TDCLPlatform.GetGPU(Index: TCL_uint): PDCLDevice;
begin
  if Index<FGPUCount then Result := @FDevices[FGPUs[Index]]
  else Result := nil;
end;

function TDCLPlatform.IsPresentExtension(
  const ExtensionName: AnsiString): Boolean;
var
  i: Integer;
  UppName: AnsiString;
begin
  Result := False;
  UppName := UpperCase(ExtensionName);
  for i := 0 to High(FExtensions) do
  begin
    if FExtensions[i]=UppName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TDCLDevice }

constructor TDCLDevice.Create(Device_id: PCL_device_id);
(*
  need to add
  CL_DEVICE_MAX_WORK_ITEM_SIZES
  CL_DEVICE_QUEUE_PROPERTIES
*)

var
  Size: TSize_t;
  device_type: TCL_device_type;
  b_bool: TCL_bool;
  fp_config: TCL_device_fp_config;
  execution_capabilities: TCL_device_exec_capabilities;
  global_mem_cache_type: TCL_device_mem_cache_type;
  local_mem_type: TCL_device_local_mem_type;
  i, current, previous: Integer;
begin
  inherited Create();
  FDevice_id := Device_id;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NAME, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FName, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NAME, Size, @FName[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NAME: ' + FName + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FVendor, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR, Size, @FVendor[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_VENDOR: ' + FVendor + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VERSION, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VERSION, Size, @FVersion[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_VERSION: ' + FVersion + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILE, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FProfile, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILE, Size, @FProfile[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PROFILE: ' + FProfile + ';');
  {$ENDIF}


  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_TYPE, SizeOf(device_type), @device_type, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  if (device_type and CL_DEVICE_TYPE_CPU)<>0 then
  begin
    FIsCPU := True;
    {$IFDEF LOGGING}
      WriteLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_CPU;');
    {$ENDIF}
  end;
  if (device_type and CL_DEVICE_TYPE_GPU)<>0 then
  begin
    FIsGPU := True;
    {$IFDEF LOGGING}
      WriteLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_GPU;');
    {$ENDIF}
  end;
  if (device_type and CL_DEVICE_TYPE_ACCELERATOR)<>0 then
  begin
    FIsAccelerator := True;
    {$IFDEF LOGGING}
      WriteLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_ACCELERATOR;');
    {$ENDIF}
  end;
  if (device_type and CL_DEVICE_TYPE_DEFAULT)<>0 then
  begin
    FIsDefault := True;
    {$IFDEF LOGGING}
      WriteLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_DEFAULT;');
    {$ENDIF}
  end;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR  , SizeOf(FMaxWorkGroupSize), @FMaxWorkGroupSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR: ' + IntToStr(FMaxWorkGroupSize) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR , SizeOf(FNativeVectorPreferredChar), @FNativeVectorPreferredChar, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR: ' + IntToStr(FNativeVectorPreferredChar) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT , SizeOf(FNativeVectorPreferredShort), @FNativeVectorPreferredShort, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT: ' + IntToStr(FNativeVectorPreferredShort) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT , SizeOf(FNativeVectorPreferredInt), @FNativeVectorPreferredInt, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT: ' + IntToStr(FNativeVectorPreferredInt) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG , SizeOf(FNativeVectorPreferredLong), @FNativeVectorPreferredLong, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog(' CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG: ' + IntToStr(FNativeVectorPreferredLong) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT , SizeOf(FNativeVectorPreferredFloat), @FNativeVectorPreferredFloat, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT: ' + IntToStr(FNativeVectorPreferredFloat) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE , SizeOf(FNativeVectorPreferredDouble), @FNativeVectorPreferredDouble, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE: ' + IntToStr(FNativeVectorPreferredDouble) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF , SizeOf(FNativeVectorPreferredHalf), @FNativeVectorPreferredHalf, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF: ' + IntToStr(FNativeVectorPreferredHalf) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR , SizeOf(FNativeVectorWidthChar), @FNativeVectorWidthChar, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR: ' + IntToStr(FNativeVectorWidthChar) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT , SizeOf(FNativeVectorWidthShort), @FNativeVectorWidthShort, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT: ' + IntToStr(FNativeVectorWidthShort) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_INT , SizeOf(FNativeVectorWidthInt), @FNativeVectorWidthInt, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_INT: ' + IntToStr(FNativeVectorWidthInt) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG , SizeOf(FNativeVectorWidthLong), @FNativeVectorWidthLong, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG: ' + IntToStr(FNativeVectorWidthLong) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT , SizeOf(FNativeVectorWidthFloat), @FNativeVectorWidthFloat, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT: ' + IntToStr(FNativeVectorWidthFloat) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE , SizeOf(FNativeVectorWidthDouble), @FNativeVectorWidthDouble, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE: ' + IntToStr(FNativeVectorWidthDouble) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF , SizeOf(FNativeVectorWidthHalf), @FNativeVectorWidthHalf, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF: ' + IntToStr(FNativeVectorWidthHalf) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CLOCK_FREQUENCY , SizeOf(FMaxClockFrequency), @FMaxClockFrequency, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_CLOCK_FREQUENCY: ' + IntToStr(FMaxClockFrequency) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ADDRESS_BITS , SizeOf(FAddressBits), @FAddressBits, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_ADDRESS_BITS: ' + IntToStr(FAddressBits) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_MEM_ALLOC_SIZE , SizeOf(FMaxMemAllocSize), @FMaxMemAllocSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_MEM_ALLOC_SIZE: ' + IntToStr(FMaxMemAllocSize) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE_SUPPORT , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE_SUPPORT: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsImageSupport := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_READ_IMAGE_ARGS , SizeOf(FMaxReadImageArgs), @FMaxReadImageArgs, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_READ_IMAGE_ARGS: ' + IntToStr(FMaxReadImageArgs) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_WRITE_IMAGE_ARGS , SizeOf(FMaxWriteImageArgs), @FMaxWriteImageArgs, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_WRITE_IMAGE_ARGS: ' + IntToStr(FMaxWriteImageArgs) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE2D_MAX_WIDTH , SizeOf(FImage2DMaxWidth), @FImage2DMaxWidth, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE2D_MAX_WIDTH: ' + IntToStr(FImage2DMaxWidth) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE2D_MAX_HEIGHT , SizeOf(FImage2DMaxHeight), @FImage2DMaxHeight, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE2D_MAX_HEIGHT: ' + IntToStr(FImage2DMaxHeight) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_WIDTH , SizeOf(FImage3DMaxWidth), @FImage3DMaxWidth, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE3D_MAX_WIDTH: ' + IntToStr(FImage3DMaxWidth) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_HEIGHT , SizeOf(FImage3DMaxHeight), @FImage3DMaxHeight, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE3D_MAX_HEIGHT: ' + IntToStr(FImage3DMaxHeight) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_DEPTH , SizeOf(FImage3DMaxDepth), @FImage3DMaxDepth, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_IMAGE3D_MAX_DEPTH: ' + IntToStr(FImage3DMaxDepth) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_SAMPLERS , SizeOf(FMaxSamplers), @FMaxSamplers, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_SAMPLERS: ' + IntToStr(FMaxSamplers) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_PARAMETER_SIZE , SizeOf(FMaxParameterSize), @FMaxParameterSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_PARAMETER_SIZE: ' + IntToStr(FMaxParameterSize) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MEM_BASE_ADDR_ALIGN , SizeOf(FMemBaseAddrAlign), @FMemBaseAddrAlign, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MEM_BASE_ADDR_ALIGN: ' + IntToStr(FMemBaseAddrAlign) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE , SizeOf(FMinDataTypeAlignSize), @FMinDataTypeAlignSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE: ' + IntToStr(FMinDataTypeAlignSize) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE , SizeOf(FGlobalMemCacheLineSize), @FGlobalMemCacheLineSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE: ' + IntToStr(FGlobalMemCacheLineSize) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE , SizeOf(FGlobalMemCacheSize), @FGlobalMemCacheSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_GLOBAL_MEM_CACHE_SIZE: ' + IntToStr(FGlobalMemCacheSize) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_SIZE  , SizeOf(FGlobalMemSize), @FGlobalMemSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_GLOBAL_MEM_SIZE: ' + IntToStr(FGlobalMemSize) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE , SizeOf(FMaxConstantBufferSize), @FMaxConstantBufferSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE: ' + IntToStr(FMaxConstantBufferSize) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CONSTANT_ARGS , SizeOf(FMaxConstantArgs), @FMaxConstantArgs, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_CONSTANT_ARGS: ' + IntToStr(FMaxConstantArgs) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_LOCAL_MEM_SIZE , SizeOf(FLocalMemSize), @FLocalMemSize, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_LOCAL_MEM_SIZE: ' + IntToStr(FLocalMemSize) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ERROR_CORRECTION_SUPPORT , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_ERROR_CORRECTION_SUPPORT: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsErrorCorrectionSupport := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_HOST_UNIFIED_MEMORY , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_HOST_UNIFIED_MEMORY: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsHostUnifiedMemory := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILING_TIMER_RESOLUTION , SizeOf(FProfilingTimerResolution), @FProfilingTimerResolution, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_PROFILING_TIMER_RESOLUTION: ' + IntToStr(FProfilingTimerResolution) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ENDIAN_LITTLE , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_ENDIAN_LITTLE: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsEndianLittle := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_AVAILABLE , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_AVAILABLE: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsAvailable := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_COMPILER_AVAILABLE , SizeOf(b_bool), @b_bool, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_COMPILER_AVAILABLE: ' + IntToStr(b_bool) + ';');
  {$ENDIF}
  FIsCompilerAvailable := Boolean(b_bool);

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR_ID , SizeOf(FVendorId), @FVendorId, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_VENDOR_ID: ' + IntToStr(FVendorId) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_COMPUTE_UNITS , SizeOf(FMaxComputeUnits), @FMaxComputeUnits, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_COMPUTE_UNITS: ' + IntToStr(FMaxComputeUnits) + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS , SizeOf(FMaxWorkItemDimensions), @FMaxWorkItemDimensions, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS: ' + IntToStr(FMaxWorkItemDimensions) + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_EXTENSIONS, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_EXTENSIONS: ' + IntToStr(Size) + ';');
  {$ENDIF}
  SetLength(FExtensionsString, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_EXTENSIONS, Size, @FExtensionsString[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_EXTENSIONS: ' + FExtensionsString + ';');
  {$ENDIF}

  FExtensionsCount := 0;
  i := 1;
  while (i<=Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[i]=' ') or (FExtensionsString[i]=#0)) then
    begin
      if (i>1) then
      begin
        if ((FExtensionsString[i-1]<>' ') and (FExtensionsString[i-1]<>#0))then
        begin
          Inc(FExtensionsCount);
        end;
      end
      else Inc(FExtensionsCount);
    end;
    inc(i);
  end;
  SetLength(FExtensions, FExtensionsCount);
  previous := 1;
  current := 1;
  i := 0;
  while (current<=Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[current]=AnsiString(' ')) or (FExtensionsString[current]=#0)) then
    begin
      if (current>previous) then FExtensions[i] := UpperCase( Copy(FExtensionsString, previous, current-previous-1));
      previous := current + 1;
      inc(i);
    end;
    inc(current);
  end;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_OPENCL_C_VERSION, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}

  SetLength(FOpenCLCVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_OPENCL_C_VERSION, Size, @FOpenCLCVersion[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DEVICE_OPENCL_C_VERSION: ' + FOpenCLCVersion + ';');
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DRIVER_VERSION, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FDriverVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DRIVER_VERSION, Size, @FDriverVersion[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_DRIVER_VERSION: ' + FDriverVersion + ';');
  {$ENDIF}
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_SINGLE_FP_CONFIG, SizeOf(fp_config), @fp_config, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: ' + IntToStr(fp_config) + ';');
  {$ENDIF}

  FFPConfigSet := [];
  {$IFDEF CL_VERSION_1_0}
    if (fp_config and CL_FP_DENORM)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcDenorm];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_DENORM;');
      {$ENDIF}
    end;
    if (fp_config and CL_FP_INF_NAN)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcInfNan];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_INF_NAN;');
      {$ENDIF}
    end;
    if (fp_config and CL_FP_ROUND_TO_NEAREST)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcRoundToNearest];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_ROUND_TO_NEAREST;');
      {$ENDIF}
    end;
    if (fp_config and CL_FP_ROUND_TO_ZERO)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcRoundToZero];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_ROUND_TO_ZERO;');
      {$ENDIF}
    end;
    if (fp_config and CL_FP_ROUND_TO_INF)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcRoundToInf];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_ROUND_TO_INF;');
      {$ENDIF}
    end;
    if (fp_config and CL_FP_FMA)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcFMA];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_FMA;');
      {$ENDIF}
    end;
  {$ENDIF}
  {$IFDEF CL_VERSION_1_1}
    if (fp_config and CL_FP_SOFT_FLOAT)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcSoftFloat];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_SOFT_FLOAT;');
      {$ENDIF}
    end;
  {$ENDIF}
  {$IFDEF CL_VERSION_1_2}
    if (fp_config and CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT)<>0 then
    begin
      FFPConfigSet := FFPConfigSet + [dfpcCorrectlyRoundedDivideSqrt];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_SINGLE_FP_CONFIG: CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT;');
      {$ENDIF}
    end;
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_EXECUTION_CAPABILITIES, SizeOf(execution_capabilities), @execution_capabilities, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_DEVICE_EXECUTION_CAPABILITIES: ' + IntToStr(execution_capabilities) + ';');
  {$ENDIF}
  FExecutionCapabilities := [];
  {$IFDEF CL_VERSION_1_0}
    if (execution_capabilities and CL_EXEC_KERNEL)<>0 then
    begin
      FExecutionCapabilities := FExecutionCapabilities + [decExecKernel];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_EXECUTION_CAPABILITIES: CL_EXEC_KERNEL;');
      {$ENDIF}
    end;
    if (execution_capabilities and CL_EXEC_NATIVE_KERNEL)<>0 then
    begin
      FExecutionCapabilities := FExecutionCapabilities + [decExecNativeKernel];
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_EXECUTION_CAPABILITIES: CL_EXEC_NATIVE_KERNEL;');
      {$ENDIF}
    end;
  {$ENDIF}

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_CACHE_TYPE, SizeOf(global_mem_cache_type), @global_mem_cache_type, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_DEVICE_GLOBAL_MEM_CACHE_TYPE: ' + IntToStr(global_mem_cache_type) + ';');
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    if (global_mem_cache_type and CL_NONE)<>0 then
    begin
      FGlobalMemCacheType := dmctNone;
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_GLOBAL_MEM_CACHE_TYPE: CL_NONE;');
      {$ENDIF}
    end;
    if (global_mem_cache_type and CL_READ_ONLY_CACHE)<>0 then
    begin
      FGlobalMemCacheType := dmctReadOnlyCache;
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_GLOBAL_MEM_CACHE_TYPE: CL_READ_ONLY_CACHE;');
      {$ENDIF}
    end;
    if (global_mem_cache_type and CL_READ_WRITE_CACHE)<>0 then
    begin
      FGlobalMemCacheType := dmctWriteOnlyCache;
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_GLOBAL_MEM_CACHE_TYPE: CL_READ_WRITE_CACHE;');
      {$ENDIF}
    end;
  {$ENDIF}


  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_LOCAL_MEM_TYPE, SizeOf(local_mem_type), @local_mem_type, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetDeviceInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_DEVICE_LOCAL_MEM_TYPE: ' + IntToStr(local_mem_type) + ';');
  {$ENDIF}

  {$IFDEF CL_VERSION_1_0}
    if (local_mem_type and CL_LOCAL)<>0 then
    begin
      FLocalMemType := dlmtLocal;
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_LOCAL_MEM_TYPE: CL_LOCAL;');
      {$ENDIF}
    end;
    if (local_mem_type and CL_GLOBAL)<>0 then
    begin
      FLocalMemType := dlmtGlobal;
      {$IFDEF LOGGING}
        WriteLog('CL_DEVICE_LOCAL_MEM_TYPE: CL_GLOBAL;');
      {$ENDIF}
    end;
  {$ENDIF}

  FContext := TDCLContext.Create(FDevice_id);
end;

function TDCLDevice.CreateBuffer(const Size: TSize_t; const Data: Pointer; const flags: TDCLMemFlagsSet): TDCLBuffer;
begin
  Result := TDCLBuffer.Create(Context.FContext, flags, Size, Data);
end;

function TDCLDevice.CreateCommandQueue(
  const properties: TDCLCommandQueuePropertiesSet): TDCLCommandQueue;
begin
  Result := TDCLCommandQueue.Create(Device_id, Context.FContext, properties);
end;

function TDCLDevice.CreateCommandQueue(const Context: TDCLContext;
  const properties: TDCLCommandQueuePropertiesSet): TDCLCommandQueue;
begin
  Result := TDCLCommandQueue.Create(Device_id, Context.FContext, properties);
end;

function TDCLDevice.CreateContext: TDCLContext;
begin
  Result := TDCLContext.Create(FDevice_id);
end;

function TDCLDevice.CreateContextGL: TDCLContext;
begin
  Result := TDCLContext.CreateGL(FDevice_id);
end;

function TDCLDevice.CreateProgram(const Source: PPAnsiChar;
  const Options: PAnsiChar): TDCLProgram;
begin
  Result := TDCLProgram.Create(FDevice_id, FContext.FContext, Source, Options);
end;

function TDCLDevice.CreateImage2D(const Format: PCL_image_format; const Width, Height, RowPitch: TSize_t;
  const Data: Pointer; const flags: TDCLMemFlagsSet): TDCLImage2D;
begin
  Result :=  TDCLImage2D.Create(Context.FContext, flags, Format, Width, Height, RowPitch, Data);
end;

function TDCLDevice.CreateFromGLImage2D(const Texture: TGLuint;
  const Flags: TDCLMemFlagsSet): TDCLImage2D;
begin
  Result := TDCLImage2D.CreateFromGL(Context.FContext, Flags, Texture);
end;

function TDCLDevice.CreateProgram(const FileName: String;
  const Options: PAnsiChar): TDCLProgram;
var
  F: TextFile;
  Source: AnsiString;
  buf: AnsiString;
begin
  AssignFile(F, FileName);
  Reset(F);
  Source := '';
  while not(EOF(F))do
  begin
    Readln(F, buf);
    Source := Source + buf + #10 + #13;
  end;
  CloseFile(F);
  Result := CreateProgram(@PAnsiString(Source), Options);
end;

destructor TDCLDevice.Destroy;
begin
  FreeAndNil(FContext);
  SetLength(FExtensions, 0);
  FExtensionsString  := '';
  FOpenCLCVersion  := '';
  FDriverVersion  := '';
  FName  := '';
  FVendor  := '';
  FVersion  := '';
  FProfile  := '';
  inherited;
end;

function TDCLDevice.GetExtensions(const Index: TSize_t): AnsiString;
begin
  if Index<FExtensionsCount then Result := FExtensions[Index]
  else Result := '';
end;

function TDCLDevice.IsPresentExtension(
  const ExtensionName: AnsiString): Boolean;
var
  i: Integer;
  UppName: AnsiString;
begin
  Result := False;
  UppName := UpperCase(ExtensionName);
  for i := 0 to High(FExtensions) do
  begin
    if FExtensions[i]=UppName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TDCLDevice.IsPresentInFPConfig(
  const FPConfig: TDCLDeviceFPConfig): Boolean;
begin
  Result := FPConfig in FFPConfigSet;
end;

function TDCLDevice.CreateFromGLBuffer(const Data: Pointer;
  const flags: TDCLMemFlagsSet): TDCLBuffer;
begin
  Result := TDCLBuffer.CreateFromGL(Context.FContext, flags, Data);
end;

{ TDCLContext }

constructor TDCLContext.Create(Device_id: PCL_device_id);
(*
  CL_CONTEXT_REFERENCE_COUNT
  CL_CONTEXT_DEVICES
  CL_CONTEXT_PROPERTIES
*)
begin
  inherited Create();
  FContext := clCreateContext(nil, 1, @Device_id, nil, nil, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateContext: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clGetContextInfo(FContext, CL_CONTEXT_NUM_DEVICES , SizeOf(FNumDevices), @FNumDevices, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetContextInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_CONTEXT_NUM_DEVICES: ' + IntToStr(FNumDevices) + ';');
  {$ENDIF}
end;

constructor TDCLContext.CreateGL(Device_id: PCL_device_id);
var
  props: array [0..4] of TCL_uint;
begin
  inherited Create();
  props[0] := CL_GL_CONTEXT_KHR;

  //MacOsX not yet (Andoid hm....)
  //MacOSX, Linux, Windows: http://www.dyn-lab.com/articles/cl-gl.html
  {$IFDEF WINDOWS}
    props[1] := wglGetCurrentContext();//glXGetCurrentContext(),
    props[2] := CL_WGL_HDC_KHR;
    props[3] := wglGetCurrentDC();//glXGetCurrentDisplay(),
  {$ENDIF}
  {$IFDEF LINUX}
    props[1] := glXGetCurrentContext();
    props[2] := CL_GLX_DISPLAY_KHR;
    props[3] := glXGetCurrentDisplay();
  {$ENDIF}
  props[4] := 0;

  FContext := clCreateContext(@props[0], 1, @Device_id, nil, nil, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateContext: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clGetContextInfo(FContext, CL_CONTEXT_NUM_DEVICES , SizeOf(FNumDevices), @FNumDevices, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetContextInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_CONTEXT_NUM_DEVICES: ' + IntToStr(FNumDevices) + ';');
  {$ENDIF}
end;

destructor TDCLContext.Destroy;
begin
  FStatus := clReleaseContext(FContext);
  {$IFDEF LOGGING}
    WriteLog('clReleaseContext: ' + GetString(FStatus) + ';');
  {$ENDIF}
  inherited;
end;

{ TDCLQueue }

constructor TDCLCommandQueue.Create(const Device_Id: PCL_device_id; const Context: PCL_context;
  const properties: TDCLCommandQueuePropertiesSet);
var
  props: TCL_command_queue_properties;
begin
  props := 0;
  if cqpOutOfOrderExecModeEnable in properties then
    props := props or CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE;
  {$IFDEF PROFILING}
    props := props or CL_QUEUE_PROFILING_ENABLE;
  {$ENDIF}
  FCommandQueue := clCreateCommandQueue(Context, Device_Id, props, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateCommandQueue: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FProperties := Properties;
end;

{ TDCLBuffer }

constructor TDCLBuffer.Create(const Context: PCL_context;
  const flags: TDCLMemFlagsSet; const Size: TSize_t; const Data: Pointer=nil);
var
  fgs: TCL_mem_flags;
begin
  inherited Create();
  fgs := 0;
  if mfReadWrite in flags then fgs := fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in flags then fgs := fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in flags then fgs := fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in flags then fgs := fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in flags then fgs := fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in flags then fgs := fgs or CL_MEM_COPY_HOST_PTR;
  FMem := clCreateBuffer(Context, fgs, Size, Data, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateBuffer: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FSize := Size;
end;

constructor TDCLBuffer.CreateFromGL(const Context: PCL_context;
  const Flags: TDCLMemFlagsSet; const Data: Pointer);
var
  fgs: TCL_mem_flags;
begin
  inherited Create();
  fgs := 0;
  if mfReadWrite in flags then fgs := fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in flags then fgs := fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in flags then fgs := fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in flags then fgs := fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in flags then fgs := fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in flags then fgs := fgs or CL_MEM_COPY_HOST_PTR;
  FMem := clCreateFromGLBuffer(Context, fgs, PGLUint(Data)^, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateFromGLBuffer: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FSize := Size;
end;

destructor TDCLBuffer.Destroy;
begin
  FStatus := clReleaseMemObject(FMem);
  inherited;
end;

procedure TDCLCommandQueue.Execute(const Kernel: TDCLKernel;
  const Size: TSize_t);
{$IFDEF PROFILING}
var
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  FStatus := clEnqueueNDRangeKernel(FCommandQueue, Kernel.FKernel, 1, nil, @Size, nil, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueNDRangeKernel: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime-StartTime;
    {$IFDEF LOGGING}
      WriteLog('EnqueueNDRangeKernel time: ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}
end;

procedure TDCLCommandQueue.Execute(const Kernel: TDCLKernel; //const Device: PCL_device_id;
  const Size: array of TSize_t);
{$IFDEF PROFILING}
var
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  {$IFDEF LOGGING}
    WriteLog('clGetKernelWorkGroupInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clEnqueueNDRangeKernel(FCommandQueue, Kernel.FKernel, Length(Size), nil, @Size[0], nil, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueNDRangeKernel: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime-StartTime;
    {$IFDEF LOGGING}
      WriteLog('EnqueueNDRangeKernel time: ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}
end;

destructor TDCLCommandQueue.Destroy;
begin
  FStatus := clReleaseCommandQueue(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clReleaseCommandQueue: ' + GetString(FStatus) + ';');
  {$ENDIF}
  inherited;
end;

{ TDCLProgram }

constructor TDCLProgram.Create(const Device: PCL_device_id;
  const Context: PCL_context;
  const Source: PPAnsiChar; const Options: PAnsiChar);
var
  Size: TSize_t;
begin
  FProgram := clCreateProgramWithSource(Context, 1, Source, nil, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateProgramWithSource: ' + GetString(FStatus) + ';');
  {$ENDIF}
//  exit; //XXXXXXXXXXXXXXX
  FStatus := clBuildProgram(FProgram, 1, @Device, Options, nil, nil);
  //FStatus := clBuildProgram(FProgram, 0, nil, Options, nil, nil);
  {$IFDEF LOGGING}
    WriteLog('clBuildProgram: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clGetProgramBuildInfo(FProgram, Device, CL_PROGRAM_BUILD_LOG, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramBuildInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(FLog, Size);
  FStatus := clGetProgramBuildInfo(FProgram, Device, CL_PROGRAM_BUILD_LOG, Size, @FLog[1], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramBuildInfo: ' + GetString(FStatus) + ';');
    WriteLog('FLog: ' + FLog + ';');
  {$ENDIF}
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_SOURCE, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FSource := GetMemory(Size);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_SOURCE, Size, FSource, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PROGRAM_SOURCE: ' + AnsiString(FSource) + ';');
  {$ENDIF}

  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_BINARY_SIZES, SizeOf(FBinarySizes), @FBinarySizes, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PROGRAM_BINARY_SIZES: ' + IntToStr(FBinarySizes) + ';');
  {$ENDIF}
  SetLength(FBinaries, 1, FBinarySizes);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_BINARIES, SizeOf(FBinaries), @FBinaries[0], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetProgramInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_PROGRAM_BINARIES: ' + AnsiString(FBinaries[0]) + ';');
  {$ENDIF}
end;

function TDCLProgram.CreateKernel(const KernelName: PAnsiChar): TDCLKernel;
begin
  Result := TDCLKernel.Create(FProgram, KernelName);
end;

destructor TDCLProgram.Destroy;
begin
  FStatus := clReleaseProgram(FProgram);
  {$IFDEF LOGGING}
    WriteLog('clReleaseProgram: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FSource := '';
  FBinarySizes := 0;
  SetLength(FBinaries, 0, 0);
  inherited;
end;

procedure TDCLProgram.SaveToFile(const FileName: AnsiString);
var
  F: file;
begin
  try
    AssignFile(F, FileName);
    Rewrite(F, 1);
    BlockWrite(F, FBinaries[0], FBinarySizes);
  finally
    CloseFile(F);
  end;
end;

{ TDCLKernel }

constructor TDCLKernel.Create(const Program_: PCL_program;
  const KernelName: PAnsiChar);
begin
  FKernel := clCreateKernel(Program_, KernelName, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateKernel: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

destructor TDCLKernel.Destroy;
begin
  FStatus := clReleaseKernel(FKernel);
  {$IFDEF LOGGING}
    WriteLog('clReleaseKernel: ' + GetString(FStatus) + ';');
  {$ENDIF}
  inherited;
end;

function TDCLKernel.GetFunctionName: AnsiString;
var
  Size: TSize_t;
  buffer: array of AnsiChar;
begin
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_FUNCTION_NAME, 0, nil, @Size);
  {$IFDEF LOGGING}
    WriteLog('clGetKernelInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  SetLength(buffer, Size);
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_FUNCTION_NAME, Size, @buffer[0], nil);
  {$IFDEF LOGGING}
    WriteLog('clGetKernelInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_KERNEL_FUNCTION_NAME: ' + AnsiString(buffer) + ';');
  {$ENDIF}
  Result := AnsiString(buffer);
  SetLength(buffer, 0);
end;

function TDCLKernel.GetNumArgs: TCL_uint;
begin
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_NUM_ARGS, SizeOf(Result), @Result, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetKernelInfo: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF LOGGING}
    WriteLog('CL_KERNEL_NUM_ARGS: ' + IntToStr(Result) + ';');
  {$ENDIF}
end;

procedure TDCLKernel.SetArg(const Index: TCL_uint; const Size: TSize_t;
  const Value: Pointer);
begin
  FStatus := clSetKernelArg(FKernel, Index, Size, Value);
  {$IFDEF LOGGING}
    WriteLog('clSetKernelArg: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

procedure TDCLKernel.SetArg(const Index: TCL_uint;
  const Value: TDCLBuffer);
begin
  SetArg(Index, SizeOf(@Value.FMem), @Value.FMem);
end;

procedure TDCLKernel.SetArg(const Index: TCL_uint;
  const Value: TDCLImage2D);
begin
  SetArg(Index, SizeOf(@Value.FMem), @Value.FMem);
end;

procedure TDCLCommandQueue.ReadBuffer(const Buffer: TDCLBuffer;
  const Size: TSize_t; const Data: Pointer);
{$IFDEF PROFILING}
var
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  FStatus := clEnqueueReadBuffer(FCommandQueue, Buffer.FMem, CL_TRUE, 0, Size, Data, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueReadBuffer: ' + GetString(FStatus) + ';');
  {$ENDIF}
  clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime-StartTime;
    {$IFDEF LOGGING}
      WriteLog('EnqueueReadBuffer time : ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}
end;

procedure TDCLCommandQueue.ReadImage2D(const Image: TDCLImage2D; const Data: Pointer);
var
  origin, region: array [0..2] of TSize_t;
{$IFDEF PROFILING}
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  ZeroMemory(@origin, SizeOf(origin));
  region[0] := Image.Width;
  region[1] := Image.Height;
  region[2] := 1;// Image 2D
  FStatus := clEnqueueReadImage(FCommandQueue, Image.FMem, CL_TRUE, @origin, @region, 0, 0, Data, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueReadImage: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime-StartTime;
    {$IFDEF LOGGING}
      WriteLog('clEnqueueReadImage time : ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}

end;

procedure TDCLCommandQueue.WriteImage2D(const Image: TDCLImage2D;
  const Width, Height: TSize_t; const Data: Pointer);
var
  origin, region: array [0..2] of TSize_t;
{$IFDEF PROFILING}
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  ZeroMemory(@origin, SizeOf(origin));
  region[0] := Width;
  region[1] := Height;
  region[2] := 1;// Image 2D
  FStatus := clEnqueueWriteImage(FCommandQueue, Image.FMem, CL_TRUE, @origin, @region, 0, 0, Data, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueWriteImage: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime-StartTime;
    {$IFDEF LOGGING}
      WriteLog('clEnqueueWriteImage time : ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}
end;

procedure TDCLCommandQueue.WriteBuffer(const Buffer: TDCLBuffer;
  const Size: TSize_t; const Data: Pointer);
{$IFDEF PROFILING}
var
  TimingEvent: PCL_event;
  StartTime,
  EndTime: TCL_ulong;
{$ENDIF}
begin
  FStatus := clEnqueueWriteBuffer(FCommandQueue, Buffer.FMem, CL_TRUE, 0, Size, Data, 0, nil, {$IFDEF PROFILING}@TimingEvent{$ELSE}nil{$ENDIF});
  {$IFDEF LOGGING}
    WriteLog('clEnqueueWriteBuffer: ' + GetString(FStatus) + ';');
  {$ENDIF}
  FStatus := clFinish(FCommandQueue);
  {$IFDEF LOGGING}
    WriteLog('clFinish: ' + GetString(FStatus) + ';');
  {$ENDIF}
  {$IFDEF PROFILING}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_START, SizeOf(StartTime), @StartTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FStatus := clGetEventProfilingInfo(TimingEvent, CL_PROFILING_COMMAND_END, SizeOf(EndTime), @EndTime, nil);
    {$IFDEF LOGGING}
      WriteLog('clGetEventProfilingInfo: ' + GetString(FStatus) + ';');
    {$ENDIF}
    FExecuteTime := EndTime - StartTime;
    {$IFDEF LOGGING}
      WriteLog('clEnqueueWriteBuffer time : ' + IntToStr(FExecuteTime) + ' ns;');
    {$ENDIF}
  {$ENDIF}
end;

procedure TDCLCommandQueue.AcquireGLObject(const Buffer: TDCLBuffer);
begin
  FStatus := clEnqueueAcquireGLObjects(FCommandQueue, 1, @Buffer.FMem, 0, nil, nil);
  {$IFDEF LOGGING}
    WriteLog('clEnqueueAcquireGLObjects: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

procedure TDCLCommandQueue.AcquireGLObject(const Image2D: TDCLImage2D);
begin
  FStatus := clEnqueueAcquireGLObjects(FCommandQueue, 1, @Image2D.FMem, 0, nil, nil);
  {$IFDEF LOGGING}
    WriteLog('clEnqueueAcquireGLObjects: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

procedure TDCLCommandQueue.ReleaseGLObject(const Buffer: TDCLBuffer);
begin
  FStatus := clEnqueueReleaseGLObjects(FCommandQueue, 1, @Buffer.FMem, 0, nil, nil);
  {$IFDEF LOGGING}
    WriteLog('clEnqueueReleaseGLObjects: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

procedure TDCLCommandQueue.ReleaseGLObject(const Image2D: TDCLImage2D);
begin
  FStatus := clEnqueueReleaseGLObjects(FCommandQueue, 1, @Image2D.FMem, 0, nil, nil);
  {$IFDEF LOGGING}
    WriteLog('clEnqueueReleaseGLObjects: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

{ TDCLImage2D }

constructor TDCLImage2D.Create(const Context: PCL_context;
  const Flags: TDCLMemFlagsSet; const Format: PCL_image_format; const Width,
  Height, RowPitch: TSize_t; const Data: Pointer);
var
  Fgs: TCL_mem_flags;
begin
  inherited Create();
  Fgs := 0;
  if mfReadWrite in Flags then Fgs := Fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in Flags then Fgs := Fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in Flags then Fgs := Fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in Flags then Fgs := Fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in Flags then Fgs := Fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in Flags then Fgs := Fgs or CL_MEM_COPY_HOST_PTR;
  FFormat :=  Format^;
  FWidth := Width;
  FHeight :=  Height;
// XXXXXXXXXXXX1.2  FMem := clCreateImage2D(Context, Fgs, @FFormat, Width, Height, RowPitch, Data, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateImage2D: ' + GetString(FStatus) + ';');
  {$ENDIF}
end;

constructor TDCLImage2D.CreateFromGL(const Context: PCL_context;
  const Flags: TDCLMemFlagsSet; const Texture: TGLuint);
var
  fgs: TCL_mem_flags;
begin
  inherited Create();
  fgs := 0;
  if mfReadWrite in flags then fgs := fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in flags then fgs := fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in flags then fgs := fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in flags then fgs := fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in flags then fgs := fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in flags then fgs := fgs or CL_MEM_COPY_HOST_PTR;

// XXXXXXXXXXXX1.2  FMem := clCreateFromGLTexture2D(Context, fgs, GL_TEXTURE_2D, 0, Texture, @FStatus);
  {$IFDEF LOGGING}
    WriteLog('clCreateFromGLTexture2D: ' + GetString(FStatus) + ';');
  {$ENDIF}

  FStatus := clGetImageInfo(FMem, CL_IMAGE_WIDTH, SizeOf(FWidth), @FWidth, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetImageInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_IMAGE_WIDTH: ' + IntToStr(FWidth) + ';');
  {$ENDIF}

  FStatus := clGetImageInfo(FMem, CL_IMAGE_HEIGHT, SizeOf(FHeight), @FHeight, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetImageInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_IMAGE_HEIGHT: ' + IntToStr(FHeight) + ';');
  {$ENDIF}

  FStatus := clGetImageInfo(FMem, CL_IMAGE_FORMAT, SizeOf(FFormat), @FFormat, nil);
  {$IFDEF LOGGING}
    WriteLog('clGetImageInfo: ' + GetString(FStatus) + ';');
    WriteLog('CL_IMAGE_FORMAT_channel_order: ' + IntToStr(FFormat.Image_channel_order) + ';');
    WriteLog('CL_IMAGE_FORMAT_Image_channel_data_type: ' + IntToStr(FFormat.Image_channel_data_type) + ';');
  {$ENDIF}

end;

destructor TDCLImage2D.Destroy;
begin
  FStatus := clReleaseMemObject(FMem);
  {$IFDEF LOGGING}
    WriteLog('clReleaseMemObject: ' + GetString(FStatus) + ';');
  {$ENDIF}
  inherited;
end;

{$IFDEF LOGGING}

initialization
  AssignFile(DCLFileLOG, ExtractFilePath(ParamStr(0)) + 'DELPHI_LOG.log');
  Rewrite(DCLFileLOG);
finalization
  CloseFile(DCLFileLOG);
{$ENDIF}
end.
