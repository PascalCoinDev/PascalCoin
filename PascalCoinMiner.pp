program PascalCoinMiner;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

{ Copyright (c) 2017 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, crt, SyncObjs,
  UBlockChain, UPoolMinerThreads, UGPUMining,
  UPoolMining, ULog, UThread, UAccounts, UCrypto,
  UConst, UTime, UJSONFunctions, UNode, UNetProtocol, USha256,
  UOpenSSL,
  DelphiCL;

type

  { TPascalMinerApp }

  TPascalMinerApp = class(TCustomApplication)
    FLastLogs : TStringList;
    procedure ShowGPUDrivers;
    procedure OnConnectionStateChanged(Sender : TObject);
    procedure OnDeviceStateChanged(Sender : TObject);
    procedure OnMinerValuesChanged(Sender : TObject);
    procedure OnFoundNOnce(Sender : TCustomMinerDeviceThread; Timestamp, nOnce : Cardinal);
    procedure WriteLog(LogTxt : String);
    procedure WriteLine(nline : Integer; txt : String);
    procedure OnInThreadNewLog(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString);
  protected
    FWindow32X1,FWindow32Y1,FWindow32X2,FWindow32Y2: DWord;
    FLock : TCriticalSection;
    FPrivateKey : TECPrivateKey;
    FPoolMinerThread : TPoolMinerThread;
    FDeviceThread : TCustomMinerDeviceThread;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

Const
  CT_MINER_VERSION = 'Beta 0.1';
  CT_Line_DeviceStatus = 3;
  CT_Line_ConnectionStatus = 5;
  CT_Line_MinerValues = 8;
  CT_Line_MiningStatus = 11;
  CT_Line_LastFound = 14;
  CT_Line_Logs = 17;
  CT_MaxLogs = 10;
  CT_Line_End = 28;
  CT_OpenCL_FileName = 'pascalsha.cl';

{ TPascalMinerApp }

procedure TPascalMinerApp.ShowGPUDrivers;
Var i,j,n : Integer;
  dev : TDCLDevice;
begin
  n := 0;
  If Not TGPUDriver.GPUDriver.HasOpenCL then WriteLn('No GPU driver found')
  else begin
    Writeln('');
    WriteLn('** Platforms (Total ',TGPUDriver.GPUDriver.Platforms.PlatformCount,')');
    for i:=0 to TGPUDriver.GPUDriver.Platforms.PlatformCount-1 do begin
      WriteLn('Platform ',i,' Name:',Trim(TGPUDriver.GPUDriver.Platforms.Platforms[i]^.Name),
        ' Version:',Trim(TGPUDriver.GPUDriver.Platforms.Platforms[i]^.Version),
        ' Vendor:',Trim(TGPUDriver.GPUDriver.Platforms.Platforms[i]^.Vendor),
        ' CPU''s:',TGPUDriver.GPUDriver.Platforms.Platforms[i]^.CPUCount,
        ' GPU''s:',TGPUDriver.GPUDriver.Platforms.Platforms[i]^.GPUCount,
        ' Devices: ',TGPUDriver.GPUDriver.Platforms.Platforms[i]^.DeviceCount
        );
      inc(n,TGPUDriver.GPUDriver.Platforms.Platforms[i]^.DeviceCount);
    end;
    Writeln('');
    Writeln('** Platforms and devices available:  (Total ',n,')');
    for i:=0 to TGPUDriver.GPUDriver.Platforms.PlatformCount-1 do begin
      for j:=0 to TGPUDriver.GPUDriver.Platforms.Platforms[i]^.DeviceCount-1 do begin
        dev := TGPUDriver.GPUDriver.Platforms.Platforms[i]^.Devices[j]^;
        Writeln('-p ',i,' -d ',j,' Name:',Trim(dev.Name),' Compute Units:',dev.MaxComputeUnits,' Max Freq.:',dev.MaxClockFrequency);
      end;
    end;
  end;
end;

procedure TPascalMinerApp.OnConnectionStateChanged(Sender: TObject);
Const CT_state : Array[boolean] of String = ('Disconnected','Connected');
begin
  If FPoolMinerThread.PoolMinerClient.Connected then begin
    WriteLine(CT_Line_ConnectionStatus,'Connected to '+FPoolMinerThread.PoolMinerClient.ClientRemoteAddr);
    If Assigned(FDeviceThread) then FDeviceThread.Paused:=false;
  end else begin
    If Assigned(FDeviceThread) then FDeviceThread.Paused:=true;
    WriteLine(CT_Line_ConnectionStatus,'Trying to connect to '+FPoolMinerThread.PoolMinerClient.ClientRemoteAddr);
  end;
end;

procedure TPascalMinerApp.OnDeviceStateChanged(Sender: TObject);
begin
  If Sender is TCustomMinerDeviceThread then WriteLine(CT_Line_DeviceStatus,TCustomMinerDeviceThread(Sender).GetState);
end;

procedure TPascalMinerApp.OnMinerValuesChanged(Sender: TObject);
begin
  If Sender is TCustomMinerDeviceThread then begin
    If TCustomMinerDeviceThread(Sender).MinerValuesForWork.block>0 then begin
      WriteLine(CT_Line_MinerValues,Format('Current block: %d Miner name: %s Target: %s',
        [TCustomMinerDeviceThread(Sender).MinerValuesForWork.block,
         TCustomMinerDeviceThread(Sender).MinerValuesForWork.payload_start,
         IntToHex(TCustomMinerDeviceThread(Sender).MinerValuesForWork.target,8)
//         ,TCrypto.ToHexaString(TCustomMinerDeviceThread(Sender).MinerValuesForWork.target_pow)
         ]));
    end;
  end;
end;

procedure TPascalMinerApp.OnFoundNOnce(Sender: TCustomMinerDeviceThread; Timestamp, nOnce: Cardinal);
begin
  WriteLine(CT_Line_LastFound,FormatDateTime('hh:nn:ss',now)+' Block:'+IntToStr(Sender.MinerValuesForWork.block)+' NOnce:'+Inttostr(nOnce)+
    ' Timestamp:'+inttostr(Timestamp)+' Miner:'+Sender.MinerValuesForWork.payload_start);
end;

procedure TPascalMinerApp.WriteLog(LogTxt: String);
begin
  WriteLine(CT_Line_MiningStatus,FormatDateTime('hh:nn:ss',now)+' '+LogTxt);
end;

procedure TPascalMinerApp.WriteLine(nline: Integer; txt: String);
Var x,y,i : Integer;
begin
  FLock.Acquire;
  try
    x := WhereX32;
    y := WhereY32;
    i := length(txt);
    if i<=(FWindow32X2-FWindow32X1+1) then begin
      setlength(txt,FWindow32X2-FWindow32X1+1);
      fillchar(txt[i+1],FWindow32X2-FWindow32X1+1-i,' ');
    end else begin
      txt := copy(txt,1,FWindow32X2-FWindow32X1+1);
    end;
    GotoXY32(FWindow32X1,nline);
    write(txt);
  finally
    FLock.Release;
  end;
end;

procedure TPascalMinerApp.OnInThreadNewLog(logtype: TLogType; Time: TDateTime;
  ThreadID: Cardinal; const sender, logtext: AnsiString);
var msg : String;
  i,nline : Integer;
begin
  If logtype=ltdebug then exit;
  FLock.Acquire;
  try
    msg := formatdatetime('hh:nn:ss',now)+' '+CT_LogType[logtype]+' '+logtext;
    FLastLogs.AddObject(msg,TObject(PtrInt(logtype)));
    i := FLastLogs.Count-CT_MaxLogs;
    if (i<0) then i:=0;
    nline := CT_Line_Logs;
    while (i<FLastLogs.Count) do begin
      WriteLine(nline,FLastLogs[i]);
      inc(nline); inc(i);
    end;
    if FLastLogs.Count>(CT_MaxLogs*2) then begin
      for i:=1 to CT_MaxLogs do FLastLogs.Delete(0);
    end;
//    If logtype=lterror then WriteLine(CT_Line_LogError,'Error: '+logtext)
//    else if logtype=ltinfo then WriteLine(CT_Line_LogInfo,'Info: '+logtext)
  Finally
    FLock.Release;
  end;
end;

procedure TPascalMinerApp.DoRun;
var
  ErrorMsg: String;
  s : String;
  nsarr : TNodeServerAddressArray;

  Function AddMiner : Boolean;
  var p,d,c : Integer;
  begin
    Result := false;
    if (Not HasOption('p','platform')) And (Not HasOption('d','device')) And (Not HasOption('c','cpu:')) then begin
      Writeln('Need to specify -pX and -dY for GPU mining or -cN for CPU mining. See -h for more info');
      ShowGPUDrivers;
      Terminate;
      Exit;
    end;
    if HasOption('c','cpu') then begin
      c := StrToIntDef(GetOptionValue('c','cpu'),-1);
      if (c<=0) or (c>CPUCount) then begin
        WriteLn('Invalid cpu value ',c,'. Valid values: 1..',CPUCount);
        Terminate;
        exit;
      end;
      FDeviceThread:= TCPUDeviceThread.Create(FPoolMinerThread,CT_TMinerValuesForWork_NULL);
      FDeviceThread.OnStateChanged:=@OnDeviceStateChanged;
      FDeviceThread.OnMinerValuesChanged:=@OnMinerValuesChanged;
      FDeviceThread.OnFoundNOnce:=@OnFoundNOnce;
      TCPUDeviceThread(FDeviceThread).CPUs:=c;
      FDeviceThread.Paused:=true;
    end else begin
      p := StrToIntDef(GetOptionValue('p','platform'),-1);
      d := StrToIntDef(GetOptionValue('d','device'),-1);
      if (p<0) or (p>=TGPUDriver.GPUDriver.Platforms.PlatformCount) then begin
        WriteLn('Invalid Platform ',p,'. Valid values: 0..',TGPUDriver.GPUDriver.Platforms.PlatformCount-1);
        Terminate;
        exit;
      end;
      if (d<0) or (d>=TGPUDriver.GPUDriver.Platforms.Platforms[p]^.DeviceCount) then begin
        WriteLn('Invalid device ',d,'. Valid values: 0..',TGPUDriver.GPUDriver.Platforms.Platforms[p]^.DeviceCount-1);
        Terminate;
        exit;
      end;
      //
      FDeviceThread := TGPUDeviceThread.Create(FPoolMinerThread,CT_TMinerValuesForWork_NULL);
      FDeviceThread.OnStateChanged:=@OnDeviceStateChanged;
      FDeviceThread.OnMinerValuesChanged:=@OnMinerValuesChanged;
      FDeviceThread.OnFoundNOnce:=@OnFoundNOnce;
      TGPUDeviceThread(FDeviceThread).Platform:=p;
      TGPUDeviceThread(FDeviceThread).Device:=d;
      TGPUDeviceThread(FDeviceThread).ProgramFileName:=ExtractFileDir(ExeName)+PathDelim+CT_OpenCL_FileName;
      FDeviceThread.Paused:=true;
    end;
  end;

  Procedure DoWaitAndLog;
  Var tc : Cardinal;
    gs,ms : TMinerStats;
    hr : Int64;
  Begin
    tc := GetTickCount;
    repeat
      If FPoolMinerThread.PoolMinerClient.Connected then FDeviceThread.Paused:=false;
      while (Not Terminated) do begin
        sleep(100);
        If (tc + 1000)<GetTickCount then begin
          tc := GetTickCount;
          ms := FDeviceThread.DeviceStats;
          gs := FPoolMinerThread.GlobalMinerStats;
          if ms.WorkingMilliseconds>0 then hr := (ms.RoundsCount DIV Int64(ms.WorkingMilliseconds)) * 1000
          else hr := 0;
          If gs.RoundsCount>0 then begin
            WriteLog(Format('Mining at %0.2f MHash/s - Total Rounds: %0.2f G ',[hr / (1024*1024),gs.RoundsCount/1073741824]));
          end else begin
            WriteLog('Not mining... check connection or paused state...');
          end;
          WriteLine(CT_Line_LastFound-1,'MY VALID BLOCKS FOUND: '+IntToStr(gs.WinsCount) );
        end;
        If KeyPressed then begin
          If ReadKey in ['c','C','q','Q'] then begin
            WriteLine(CT_Line_End,'Finalizing...');
            terminate;
          end;
        end;
      end;
    until Terminated;
  end;

  Procedure DoVisualprocess(minerName : String);
  Var sc : tcrtcoord;
    Flog : TLog;
  Begin
    cursoroff;
    try
      clrscr;
      FWindow32X1:=WindMinX;
      FWindow32X2:=WindMaxX;
      FWindow32Y1:=WindMinY;
      FWindow32Y2:=WindMaxY;
      WriteLine(1,'** PascalCoin miner ** Version: '+CT_MINER_VERSION);
      WriteLine(CT_Line_MinerValues-1,'MINER VALUES:');
      WriteLine(CT_Line_MiningStatus-1,'MINING STATUS:');
      WriteLine(CT_Line_LastFound-1,'MY VALID BLOCKS FOUND: 0');
      WriteLine(CT_Line_Logs-1,'LOGS:');

      FPoolMinerThread := TPoolMinerThread.Create(nsarr[0].ip,nsarr[0].port,FPrivateKey.PublicKey);
      try
        FPoolMinerThread.MinerAddName:=minerName;
        WriteLine(CT_Line_MinerValues-1,'MINER VALUES: (My miner name="'+minerName+'")');
        FPoolMinerThread.OnConnectionStateChanged:=@OnConnectionStateChanged;
        OnConnectionStateChanged(FPoolMinerThread);
        AddMiner;
        WriteLine(2,FDeviceThread.MinerDeviceName);
        Flog := TLog.Create(Nil);
        try
          Flog.OnInThreadNewLog:=@OnInThreadNewLog;
          DoWaitAndLog;
        finally
          FLog.free;
        end;
        FPoolMinerThread.Terminate;
      Finally
        FPoolMinerThread.Free;
      end;
    finally
      cursoron;
    end;
  end;

begin
  FLastLogs := TStringList.Create;
  FLock := TCriticalSection.Create;
  Try
    // quick check parameters
    ErrorMsg:=CheckOptions('hp:d:s:c:n:', 'help platform device server cpu minername');
    if ErrorMsg<>'' then begin
      //ShowException(Exception.Create(ErrorMsg));
      WriteLn(ErrorMsg);
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Exit;
    end;

    if (Not HasOption('p','platform')) And (Not HasOption('d','device')) And (Not HasOption('c','cpu:')) then begin
      Writeln('Need to specify -p X and -d Y for GPU mining or -c N for CPU mining. See -h for more info');
      ShowGPUDrivers;
      Exit;
    end;

    If Not FileExists(ExtractFileDir(ExeName)+PathDelim+CT_OpenCL_FileName) then begin
      Writeln('**********************');
      Writeln('OpenCL file not found!');
      Writeln('File: ',CT_OpenCL_FileName);
      Exit;
    end;


    If HasOption('s','server') then s := GetOptionValue('s','server')
    else s:='';
    if (s='') then begin
      WriteLn('Input server name (default is localhost:',CT_JSONRPCMinerServer_Port,'):');
      Readln(s);
      if (s='') then s := 'localhost:'+inttostr(CT_JSONRPCMinerServer_Port);
    end;
    TNode.DecodeIpStringToNodeServerAddressArray(s,nsarr);
    if (length(nsarr)<>1) then begin
      Writeln('INVALID SERVER VALUE ',s);
      WriteHelp;
      Exit;
    end;
    If (Not HasOption('n','minername')) then begin
      WriteLn('Input miner name that will be added to server miner name:');
      Readln(s);
    end else s:=GetOptionValue('n','minername');
    Try
      TCrypto.InitCrypto;
    Except
      On E:Exception do begin
        Writeln('**************************');
        Writeln('Error initializing library '+SSL_C_LIB+' (Not found or not valid)');
        Writeln('Error message: '+E.Message);
        Writeln('**************************');
        Exit;
      end;
    end;
    FPrivateKey := TECPrivateKey.Create;
    Try
      FPrivateKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      DoVisualprocess(s);
    finally
      FreeAndNil(FPrivateKey);
    end;
  finally
    FreeAndNil(FLock);
    FreeAndNil(FLastLogs);
    if not terminated then
      Terminate;
  end;
end;

constructor TPascalMinerApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPascalMinerApp.Destroy;
begin
  inherited Destroy;
end;

procedure TPascalMinerApp.WriteHelp;
begin
  { add your help code here }
  writeln('PascalCoin Miner - Version: ',CT_MINER_VERSION);
  writeln('Usage: ', ExeName, ' -h -s S -p X -d Y -c N -n MYNAME');
  writeln('  -h for help');
  writeln('  -s S  (S is PascalCoin server:port where default value is localhost:',CT_JSONRPCMinerServer_Port,')');
  writeln('  -p X  (X is GPU platform)');
  writeln('  -d Y  (Y is GPU device for platform)');
  writeln('  -c N  (For CPU mining, where N is CPU''s to use. Activating this disable GPU mining)');
  writeln('  -n MYNAME  (Will add MYNAME value to miner name assigned by server)');
  writeln('');
  writeln('Basic example CPU mining: ');
  writeln('  ',ExtractFileName(ExeName),' -s 192.168.1.77:4009 -c 2 -n USER_1');
  writeln('  (2 CPU''s to server 192.168.1.77 port 4009 and miner name USER_1)');
  writeln('Basic example GPU mining: ');
  writeln('  ',ExtractFileName(ExeName),' -p 0 -d 0 -n ABC');
  writeln('  (p 0 d 0 at server localhost and port ',CT_JSONRPCMinerServer_Port,' miner name ABC)');
  writeln('');
  ShowGPUDrivers;
end;

var
  Application: TPascalMinerApp;
begin
  Application:=TPascalMinerApp.Create(nil);
  Application.Title:='Pascal Miner';
  Application.Run;
  Application.Free;
end.

