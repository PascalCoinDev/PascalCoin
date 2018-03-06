program PascalCoinMiner;

{$mode delphi}{$H+}
{$DEFINE UseCThreads}
{$I config.inc}

{ Copyright (c) 2017 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

{$I config.inc}

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
    procedure WriteLine(nline : Integer; txt : String);
    procedure OnInThreadNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : AnsiString);
  protected
    FWindow32X1,FWindow32Y1,FWindow32X2,FWindow32Y2: DWord;
    FLock : TCriticalSection;
    FPrivateKey : TECPrivateKey;
    FPoolMinerThread : TPoolMinerThread;
    FDeviceThreads : TList;
    FAppStartTime : TDateTime;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

Const
  CT_MINER_VERSION = {$IFDEF PRODUCTION}'0.5'{$ELSE}{$IFDEF TESTNET}'0.5 TESTNET'{$ELSE}ERROR{$ENDIF}{$ENDIF};
  CT_Line_DeviceStatus = 3;
  CT_Line_ConnectionStatus = 4;
  CT_Line_MinerValues = 7;
  CT_Line_MiningStatus = 10;
  CT_Line_LastFound = 12;
  CT_Line_Logs = 15;
  CT_MaxLogs = 10;
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
var i : Integer;
  s : String;
begin
  If FPoolMinerThread.PoolMinerClient.PoolType=ptNone then s:='MINING'
  else s:='POOL MINING USER "'+FPoolMinerThread.PoolMinerClient.UserName+'"';
  If FPoolMinerThread.PoolMinerClient.Connected then begin
    WriteLine(CT_Line_ConnectionStatus,s + ' server: '+FPoolMinerThread.PoolMinerClient.ClientRemoteAddr);
    For i:=0 to FDeviceThreads.Count-1 do begin
      TCustomMinerDeviceThread(FDeviceThreads[i]).Paused:=false;
    end;
  end else begin
    For i:=0 to FDeviceThreads.Count-1 do begin
      TCustomMinerDeviceThread(FDeviceThreads[i]).Paused:=true;
    end;
    WriteLine(CT_Line_ConnectionStatus,'** NOT CONNECTED '+s + ' Connecting to '+FPoolMinerThread.PoolMinerClient.ClientRemoteAddr);
  end;
end;

procedure TPascalMinerApp.OnDeviceStateChanged(Sender: TObject);
Var i : Integer;
  s : String;
begin
  If Sender is TCustomMinerDeviceThread then begin
    If TCustomMinerDeviceThread(Sender).IsMining then WriteLine(CT_Line_DeviceStatus,'') // clear line
    else WriteLine(CT_Line_DeviceStatus,'*** Not mining ***');
  end;
end;

procedure TPascalMinerApp.OnMinerValuesChanged(Sender: TObject);
begin
  If Sender is TCustomMinerDeviceThread then begin
    If TCustomMinerDeviceThread(Sender).MinerValuesForWork.block>0 then begin
      WriteLine(CT_Line_MinerValues,Format('Current block: %d Wallet Name: "%s" Target: %s',
        [TCustomMinerDeviceThread(Sender).MinerValuesForWork.block,
         FPoolMinerThread.GlobalMinerValuesForWork.payload_start,
         IntToHex(TCustomMinerDeviceThread(Sender).MinerValuesForWork.target,8)
         ]));
    end;
  end;
end;

procedure TPascalMinerApp.OnFoundNOnce(Sender: TCustomMinerDeviceThread; Timestamp, nOnce: Cardinal);
begin
  WriteLine(CT_Line_LastFound + FDeviceThreads.Count,FormatDateTime('hh:nn:ss',now)+' Block:'+IntToStr(Sender.MinerValuesForWork.block)+' NOnce:'+Inttostr(nOnce)+
    ' Timestamp:'+inttostr(Timestamp)+' Miner:'+Sender.MinerValuesForWork.payload_start);
end;

procedure TPascalMinerApp.WriteLine(nline: Integer; txt: String);
Var i : Integer;
begin
  FLock.Acquire;
  try
    i := length(txt);
    if i<=(FWindow32X2-FWindow32X1+1) then begin
      setlength(txt,FWindow32X2-FWindow32X1+1);
      fillchar(txt[i+1],FWindow32X2-FWindow32X1+1-i,' ');
    end else begin
      txt := copy(txt,1,FWindow32X2-FWindow32X1+1);
    end;
    if (nline<=(FWindow32Y2-FWindow32Y1)) then begin
      GotoXY(FWindow32X1,nline);
      write(txt);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TPascalMinerApp.OnInThreadNewLog(logtype: TLogType; Time: TDateTime;
  ThreadID: TThreadID; const sender, logtext: AnsiString);
var msg : String;
  i,nline : Integer;
begin
  If logtype=ltdebug then exit;
  FLock.Acquire;
  try
    msg := formatdatetime('hh:nn:ss',now)+' '+CT_LogType[logtype]+' '+logtext;
    // TODO - test logtype is properly casted/stored/retrieved/accessed.
    // Confirm casting doesn't lose bits in 32/64 bit archs
    // OLD: FLastLogs.AddObject(msg,TObject(PtrInt(logtype)));
    FLastLogs.AddObject(msg,Pointer(logtype));
    i := FLastLogs.Count-CT_MaxLogs;
    if (i<0) then i:=0;
    nline := CT_Line_Logs+FDeviceThreads.Count;
    while (i<FLastLogs.Count) do begin
      WriteLine(nline,FLastLogs[i]);
      inc(nline); inc(i);
    end;
    if FLastLogs.Count>(CT_MaxLogs*2) then begin
      for i:=1 to CT_MaxLogs do FLastLogs.Delete(0);
    end;
  Finally
    FLock.Release;
  end;
end;

procedure TPascalMinerApp.DoRun;
var
  ErrorMsg: String;
  s : String;
  nsarr : TNodeServerAddressArray;

  Function AddMiners : Boolean;
  var p,d,c,i : Integer;
    strl : TStringList;
    devt : TCustomMinerDeviceThread;
  begin
    Result := false;
    if (Not HasOption('p','platform')) And (Not HasOption('d','device')) And (Not HasOption('c','cpu')) then begin
      Writeln('Need to specify -p X and -d Y for GPU mining or -c N for CPU mining. See -h for more info');
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
      devt:= TCPUDeviceThread.Create(FPoolMinerThread,CT_TMinerValuesForWork_NULL);
      devt.OnStateChanged:=OnDeviceStateChanged;
      devt.OnMinerValuesChanged:=OnMinerValuesChanged;
      devt.OnFoundNOnce:=OnFoundNOnce;
      TCPUDeviceThread(devt).CPUs:=c;
      devt.Paused:=true;
      FDeviceThreads.Add(devt);
    end else begin
      p := StrToIntDef(GetOptionValue('p','platform'),-1);
      d := StrToIntDef(GetOptionValue('d','device'),-1);
      if (p<0) or (p>=TGPUDriver.GPUDriver.Platforms.PlatformCount) then begin
        WriteLn('Invalid Platform ',p,'. Valid values: 0..',TGPUDriver.GPUDriver.Platforms.PlatformCount-1);
        Terminate;
        exit;
      end;
      strl := TStringList.Create;
      try
        if (d<0) then begin
          // Is a value separated by commas?
          strl.Delimiter:=',';
          strl.DelimitedText:=GetOptionValue('d','device');
        end else strl.Text:=inttostr(d);
        for i:=0 to strl.Count-1 do begin
          d := StrToIntDef(strl[i],-1);
          if (d<0) or (d>=TGPUDriver.GPUDriver.Platforms.Platforms[p]^.DeviceCount) then begin
            WriteLn('Invalid device ',d,'. Valid values: 0..',TGPUDriver.GPUDriver.Platforms.Platforms[p]^.DeviceCount-1);
            Terminate;
            exit;
          end;
          //
          devt := TGPUDeviceThread.Create(FPoolMinerThread,CT_TMinerValuesForWork_NULL);
          devt.OnStateChanged:=OnDeviceStateChanged;
          devt.OnMinerValuesChanged:=OnMinerValuesChanged;
          devt.OnFoundNOnce:=OnFoundNOnce;
          TGPUDeviceThread(devt).Platform:=p;
          TGPUDeviceThread(devt).Device:=d;
          TGPUDeviceThread(devt).ProgramFileName:=ExtractFileDir(ExeName)+PathDelim+CT_OpenCL_FileName;
          devt.Paused:=true;
          FDeviceThreads.Add(devt);
        end;
      finally
        strl.Free;
      end;
    end;
    Result := true;
  end;

  Procedure DoWaitAndLog;
  Var tc : Cardinal;
    gs,ms : TMinerStats;
    hrReal,hrHashing, glhrHashing, glhrReal : Real;
    i : Integer;
    devt : TCustomMinerDeviceThread;
    s : String;
  Begin
    tc := GetTickCount64;
    repeat
      If FPoolMinerThread.PoolMinerClient.Connected then begin
        for i:=0 to FDeviceThreads.Count-1 do begin
          TCustomMinerDeviceThread(FDeviceThreads[i]).Paused:=false;
        end;
      end;
      while (Not Terminated) do begin
        sleep(100);
        //devt := TCustomMinerDeviceThread(FDeviceThreads[0]);
        If (tc + 1000)<GetTickCount64 then begin
          tc := GetTickCount64;
          //ms := devt.DeviceStats;
          For i:=0 to FDeviceThreads.Count-1 do begin
            devt := TCustomMinerDeviceThread(FDeviceThreads[i]);
            ms := devt.DeviceStats;
            if ms.WorkingMillisecondsHashing>0 then hrHashing := (((ms.RoundsCount DIV Int64(ms.WorkingMillisecondsHashing)))/(1000))
            else hrHashing := 0;
            gs := devt.GlobalDeviceStats;
            If ms.RoundsCount>0 then begin
              s := FormatDateTime('hh:nn:ss',now)+Format(' Miner:"%s" at %0.2f MH/s - Rounds: %0.2f G Found: %d',[devt.MinerValuesForWork.payload_start,hrHashing, gs.RoundsCount/1000000000, gs.WinsCount]);
              If (gs.Invalids>0) then s := s +' '+inttostr(gs.Invalids)+' ERRORS!';
              WriteLine(CT_Line_MiningStatus+i,s);
            end else begin
              If gs.RoundsCount>0 then begin
                s := FormatDateTime('hh:nn:ss',now)+Format(' Miner:"%s" **NOT MINING** - Rounds: %0.2f G Found: %d',[devt.MinerValuesForWork.payload_start,gs.RoundsCount/1000000000, gs.WinsCount]);
                If (gs.Invalids>0) then s := s +' '+inttostr(gs.Invalids)+' ERRORS!';
              end else begin
                s := FormatDateTime('hh:nn:ss',now)+' Not mining...';
              end;
              WriteLine(CT_Line_MiningStatus+i,s);
            end;
          end;
          WriteLine(CT_Line_LastFound+FDeviceThreads.Count-1,'MY VALID BLOCKS FOUND: '+IntToStr(gs.WinsCount) +' Working time: '+IntToStr(Trunc(now - FAppStartTime))+'d '+FormatDateTime('hh:nn:ss',Now-FAppStartTime) );
        end;
        If KeyPressed then begin
          If ReadKey in ['c','C','q','Q'] then begin
            WriteLine(CT_Line_Logs+FDeviceThreads.Count+CT_MaxLogs,'Finalizing...');
            terminate;
          end;
        end;
      end;
    until Terminated;
  end;

  Procedure DoVisualprocess(minerName, UserName, Password : String);
  Var sc : tcrtcoord;
    Flog : TLog;
    devt : TCustomMinerDeviceThread;
    i : Integer;
  Begin
    FPoolMinerThread := TPoolMinerThread.Create(nsarr[0].ip,nsarr[0].port,FPrivateKey.PublicKey);
    try
      If (UserName<>'') then begin
        FPoolMinerThread.PoolMinerClient.PoolType:=ptIdentify;
        FPoolMinerThread.PoolMinerClient.UserName:=UserName;
        FPoolMinerThread.PoolMinerClient.Password:=Password;
      end;
      If Not AddMiners then exit;
      if HasOption('t','testmode') then begin
        i := StrToIntDef(GetOptionValue('t','testmode'),-1);
        if (i>=0) And (i<=32) then begin
          FPoolMinerThread.TestingPoWLeftBits:=i;
        end else begin
          WriteLn('Invalid bits for testing mode. value ',i,'. Valid values: 0..32  (0=No testing mode)');
          Terminate;
          exit;
        end;
      end;
      //
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
        WriteLine(CT_Line_LastFound+FDeviceThreads.Count-1,'MY VALID BLOCKS FOUND: 0');
        WriteLine(CT_Line_Logs+FDeviceThreads.Count-1,'LOGS:');

        FPoolMinerThread.MinerAddName:=minerName;
        WriteLine(CT_Line_MinerValues-1,'MINER VALUES: (My miner name="'+minerName+'")');

        FPoolMinerThread.OnConnectionStateChanged:=OnConnectionStateChanged;
        OnConnectionStateChanged(FPoolMinerThread);

        If (FDeviceThreads.Count)=1 then begin
          devt := TCustomMinerDeviceThread(FDeviceThreads[0]);
          WriteLine(2,devt.MinerDeviceName);
        end else begin
          WriteLine(2,'Mining using '+IntToStr(FDeviceThreads.Count)+' devices');
        end;
        Flog := TLog.Create(Nil);
        try
          Flog.OnInThreadNewLog:=OnInThreadNewLog;
          DoWaitAndLog;
        finally
          FLog.free;
        end;
      finally
        cursoron;
      end;
    Finally
      FPoolMinerThread.Terminate;
      FPoolMinerThread.Free;
    end;
  end;

Var username,password : String;
begin
  FLastLogs := TStringList.Create;
  FLock := TCriticalSection.Create;
  Try
    // quick check parameters
    ErrorMsg:=CheckOptions('hp:d:s::c:n::t:u::x::', 'help platform device server cpu minername testmode user pwd');
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
      Writeln('Need to specify -p X and -d Y for GPU mining or -c N for CPU mining');
      Writeln('Execute ',ExtractFileName(ExeName),' -h for more info');
      ShowGPUDrivers;
      Exit;
    end;

    If Not FileExists(ExtractFileDir(ExeName)+PathDelim+CT_OpenCL_FileName) then begin
      Writeln('**********************');
      Writeln('OpenCL file not found!');
      Writeln('File: ',CT_OpenCL_FileName);
      Exit;
    end;


    If HasOption('s','server') then begin
      s := Trim(GetOptionValue('s','server'));
      if (s='') then s := 'localhost:'+inttostr(CT_JSONRPCMinerServer_Port);
    end else s:='';
    if (s='') then begin
      WriteLn('Input server name (default is localhost:',CT_JSONRPCMinerServer_Port,'):');
      Readln(s);
      trim(s);
      if (s='') then s := 'localhost:'+inttostr(CT_JSONRPCMinerServer_Port);
    end;
    if (pos(':',s)=0) then begin
      s := trim(s) + ':'+inttostr(CT_JSONRPCMinerServer_Port);
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
    username:='';
    password:='';
    If (HasOption('u','user')) Or (HasOption('x','pwd')) then begin
      username:=trim(GetOptionValue('u','user'));
      password:=trim(GetOptionValue('x','pwd'));
      if (username='') then begin
        WriteLn('Input Pool username (or empty for non pool connection):');
        Readln(username);
      end;
      if (password='') And (username<>'') then begin
        WriteLn('Input Pool password for user ',username,':');
        Readln(password);
      end;
    end;

    FPrivateKey := TECPrivateKey.Create;
    Try
      FPrivateKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      DoVisualprocess(s,username,password);
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
Var FLog : TLog;
begin
  inherited Create(TheOwner);
  FDeviceThreads := TList.Create;
  StopOnException:=True;
  FAppStartTime := Now;
  FLog := TLog.Create(self);
  FLog.SaveTypes:=CT_TLogTypes_DEFAULT;
  FLog.FileName:=ExtractFileDir(ExeName)+PathDelim+'PascalCoinMiner.log';
end;

destructor TPascalMinerApp.Destroy;
begin
  FreeAndNil(FDeviceThreads);
  inherited Destroy;
end;

procedure TPascalMinerApp.WriteHelp;
begin
  { add your help code here }
  writeln('PascalCoin Miner - Version: ',CT_MINER_VERSION);
  writeln('Usage: ', ExtractFileName(ExeName), ' -h -s S -p X -d Y -c N -n MYNAME');
  writeln('  -h for help');
  writeln('  -s S  (S is PascalCoin server:port where default value is localhost:',CT_JSONRPCMinerServer_Port,')');
  writeln('  -p X  (X is GPU platform)');
  writeln('  -d Y  (Y is GPU device for platform)');
  writeln('    Y can be multiple devices. Example -d 0,2,3  Will use devices 0, 2 and 3');
  writeln('  -c N  (For CPU mining, where N is CPU''s to use. Activating this disable GPU mining)');
  writeln('  -n MYNAME  (Will add MYNAME value to miner name assigned by server)');
  writeln('  ** POOL IDENTIFICATION PROTOCOL **');
  writeln('  (Not needed for PascalCoin core, only some third party pools)');
  writeln('  -u USERNAME');
  writeln('  -x PASSWORD');
  writeln('');
  writeln('Basic example GPU mining over multiple devices: ');
  writeln('  ',ExtractFileName(ExeName),' -p 0 -d 0,1,2,3 -s -n ABC');
  writeln('  (Devices 0,1,2,3 at server localhost:',CT_JSONRPCMinerServer_Port,' miner name ABC)');
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

