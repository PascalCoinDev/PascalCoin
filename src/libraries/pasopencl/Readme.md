PasOpenCL
=========

PasOpenCL is a fork of 'delphi-opencl' hosted on Google Code (http://code.google.com/p/delphi-opencl/). The code of the fork is kept more or less unchanged, but the structure of the repository has changed a bit. For this reason a different name has been picked.

One prominent change is the absense of the dglOpenGL.pas header, which can be found at the personal fork at:
https://github.com/CWBudde/dglOpenGL
or at the original location at
https://bitbucket.org/saschawillems/dglopengl

Below you can read the information given to the original project.

Original Project
---------------- 

This project is intended to make possible the use of OpenCL in Delphi and Free Pascal (Lazarus). 

Authors: 
niello  Site: http://www.niello.org.ua 

Igroman  Site: http://Igroman14.livejournal.com 

What OpenCL platforms / Operating Systems / Compilers are supported? 

OpenCL Platforms: 
tested on AMD, Nvidia 
if you are running on a different platform, please contact us 
Operating Systems: 
Windows XP, Windows Vista, Windows 7 
Code for Linux added but not tested 
if you are running on a different OS, please contact us 
Compilers: 
Delphi (6, 7, 2010, XE2) 
FPC (2.2.0, 2.4.2) 
if you are running on a different compiler, please contact us 


How to use it? 
* CL, CL_Platforms, CL_GL, CL_D3D9, CL_D3D10, CL_D3D11, CL_DX9_Media_Sharing, CL_Ext, CL_GL_Ext (.pas) are OpenCL headers for Pascal compilers. 
* OpenCL (.inc) - is the include file for them. 
* DelphiCL (.pas) - OOP classes for OpenCL. 
* DelphiCL (.inc) - is the include file for the above. (LOGGING for logging in file "DELPHI_LOG.log", PROFILING for profiling). 


Before using the TDCLClasses, need execute procedure: 
   InitOpenCL();// default library name = OpenCL.dll for Windows, libOpenCL.so for Linux
 
 if you are using CL_GL sharing, need added InitCL_GL() procedure: 
   InitCL_GL();
 


The TDCLPlatforms allow get access to the all platforms on the computer (AMD, NVidia, Intel etc.). 
 You need to create specimen of this class: 
 var
 platforms: TDCLPlatforms;
 ...
 begin
   platforms := TDCLPlatforms.Create();
   //Your code here
   FreeAndNil(platforms);
 end;
 


The TDCLPlatforms have property Platforms (array of PDCLPlatform - Pointer to the TDCLPlatform). 
 More in the source files. 
 Maybe later we'll write a more detailed guide.