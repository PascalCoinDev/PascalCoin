unit UPCRPCFileUtils;

{ Copyright (c) 2020 by PascalCoin developers, orignal code by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ./../config.inc}

Uses classes, SysUtils,
  UJSONFunctions, URPC, UCrypto, ULog,
  {$IFDEF USE_ABSTRACTMEM}
  UPCAbstractMem, UPCAbstractMemAccountKeys,
  {$ENDIF}
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UBlockChain, UPCOrderedLists;


Type

  { TRPCFileUtils }

  TRPCFileUtils = Class
  private
  public
    class function SaveAsSafeboxStream(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
    class function GenerateNewAbstractMemSafebox(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
    class function AbstractMemStats(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;

  End;

implementation

uses UPCDataTypes, UFileStorage, UNode;

{ TRPCFileUtils }

class function TRPCFileUtils.GenerateNewAbstractMemSafebox(
  const ASender: TRPCProcess; const AMethodName: String; AInputParams,
  AJSONResponse: TPCJSONObject; var AErrorNum: Integer;
  var AErrorDesc: String): Boolean;
{$IFDEF USE_ABSTRACTMEM}
var
  LFileName : String;
{$ENDIF}
begin
  if Not ASender.RPCServer.AllowUsePrivateKeys then begin
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit(False);
  end;
{$IFDEF USE_ABSTRACTMEM}
  LFileName := AInputParams.AsString('filename', '').Trim;
  if (LFileName='') then begin
    LFileName := TFileStorage.GetSafeboxCheckpointingFileName(TFileStorage(TNode.Node.Bank.Storage).DatabaseFolder,TNode.Node.Bank.BlocksCount);
  end;
  TNode.Node.Bank.SafeBox.SaveCheckpointing(LFileName);
  AJSONResponse.GetAsObject('result').GetAsVariant('filename').Value := LFileName;
  AErrorNum := 0;
  AErrorDesc := '';
  Result := True;
{$ELSE}
  AErrorNum := CT_RPC_ErrNum_NotImplemented;
  AErrorDesc := 'AbstractMem library is not available in this build';
  Result := False;
{$ENDIF}
end;

class function TRPCFileUtils.AbstractMemStats(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var LStrings, LReport : TStrings;
   LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : Integer;
   i, nMax : Integer;
   Lobj : TPCJSONObject;
   Larray : TPCJSONArray;
begin
  if Not ASender.RPCServer.AllowUsePrivateKeys then begin
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit(False);
  end;
{$IFDEF USE_ABSTRACTMEM}
  LStrings := TStringList.Create;
  Try
    if AInputParams.GetAsVariant('report').AsBoolean(False) then LReport := LStrings
    else LReport := Nil;
    Lobj := AJSONResponse.GetAsObject('result').GetAsObject('abstractmem');
    if TNode.Node.Bank.SafeBox.PCAbstractMem.AbstractMem.CheckConsistency(LReport, LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then begin
      Lobj.GetAsVariant('checkconsistency').Value := True;
    end else begin
      Lobj.GetAsVariant('checkconsistency').Value := False;
    end;
    Lobj.GetAsVariant('total_used_size').Value := LTotalUsedSize;
    Lobj.GetAsVariant('total_used_blocks_count').Value := LTotalUsedBlocksCount;
    Lobj.GetAsVariant('total_leaks_size').Value := LTotalLeaksSize;
    Lobj.GetAsVariant('total_leaks_blocks_count').Value := LTotalLeaksBlocksCount;

    if Assigned(LReport) then begin
      Larray := Lobj.GetAsArray('report');
      i := AInputParams.GetAsVariant('report_start').AsInteger(0);
      nMax := AInputParams.GetAsVariant('report_max').AsInteger(100);
      while (nMax>0) and (i>=0) and (i<LStrings.Count-1) do begin
        Larray.GetAsVariant(Larray.Count).Value := LStrings[i];
        inc(i);
        dec(nMax);
      end;
    end;
    Result := True;
  Finally
    LStrings.Free;
  end;
{$ELSE}
  AErrorNum := CT_RPC_ErrNum_NotImplemented;
  AErrorDesc := 'AbstractMem library is not available in this build';
  Result := False;
{$ENDIF}
  //
end;

class function TRPCFileUtils.SaveAsSafeboxStream(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;

var LFileName : String;
  LFs : TFileStream;
  LStart,LEnd : Integer;
begin
  if Not ASender.RPCServer.AllowUsePrivateKeys then begin
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit(False);
  end;

  LFileName := AInputParams.AsString('filename', '').Trim;
  if (LFileName='') then begin
    LFileName := TFileStorage.GetSafeboxCheckpointingFileName(TFileStorage(TNode.Node.Bank.Storage).DatabaseFolder,TNode.Node.Bank.BlocksCount);
    LFileName := ChangeFileExt(LFileName,'.safebox');
  end;
  LFs := TFileStream.Create(LFileName,fmCreate);
  try
    LFs.Size := 0;
    LFs.Position := 0;
    LStart := 0;
    LEnd := TNode.Node.Bank.BlocksCount-1;
    TNode.Node.Bank.SafeBox.SaveSafeBoxToAStream(LFs,LStart,LEnd);
  finally
    LFs.Free;
  end;
  AJSONResponse.GetAsObject('result').GetAsVariant('filename').Value := LFileName;
  AJSONResponse.GetAsObject('result').GetAsVariant('start').Value := LStart;
  AJSONResponse.GetAsObject('result').GetAsVariant('end').Value := LEnd;
  AErrorNum := 0;
  AErrorDesc := '';
  Result := True;
end;

initialization
  TRPCProcess.RegisterProcessMethod('save-safebox-stream',TRPCFileUtils.SaveAsSafeboxStream);
  TRPCProcess.RegisterProcessMethod('save-safebox-abstractmem',TRPCFileUtils.GenerateNewAbstractMemSafebox);
  TRPCProcess.RegisterProcessMethod('abstractmem-stats',TRPCFileUtils.AbstractMemStats);
finalization
  TRPCProcess.UnregisterProcessMethod('save-safebox-stream');
  TRPCProcess.UnregisterProcessMethod('save-safebox-abstractmem');
  TRPCProcess.UnregisterProcessMethod('abstractmem-stats');
end.
