unit UPCDownloadSafebox;

{ Copyright (c) 2016-2023 by Albert Molina

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
  {$mode delphi}
{$ENDIF}

interface

{$I ./../config.inc}

uses
  Classes, SysUtils,
  UNetProtocol, UThread,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UBlockChain,
  UNode, UPCTemporalFileStream, UChunk,
  UAccounts, ULog, UConst, UCrypto, UBaseTypes, UPCDataTypes;

type
  TPCDownloadSafebox = class
  private
    FDownloadedBank: TPCBank;
    FOnProgressNotify: TProgressNotify;
    type
    TDownloadSBThread = Class(TPCThread)
    private
      FOwner : TPCDownloadSafebox;
      FBlockStart, FBlocksCount : Cardinal;
      FStream: TPCTemporalFileStream;
    protected
      procedure BCExecute; override;
    public
      constructor Create(AOwner : TPCDownloadSafebox; ABlockStart, ACount : Cardinal);
      destructor Destroy; override;
      Property Stream : TPCTemporalFileStream read FStream write FStream;
    End;
    TPCDownloadSafeboxChunk = record
      BlockStart : Cardinal;
      Count : Cardinal;
      Thread : TDownloadSBThread;
    end;
    var
    FNode : TNode;
    FSavedSafeboxHighOperationBlock : TOperationBlock;
    FChunks : TPCThreadList<TPCDownloadSafeboxChunk>;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function DownloadSafebox(AOwnerThread : TThread; ASavedSafeboxHighOperationBlock : TOperationBlock; AChunks : TPCSafeboxChunks) : Boolean;
    property DownloadedBank : TPCBank read FDownloadedBank;
    property OnProgressNotify : TProgressNotify read FOnProgressNotify write FOnProgressNotify;
  end;

implementation

{ TPCDownloadSafebox.TDownloadSBThread }

procedure TPCDownloadSafebox.TDownloadSBThread.BCExecute;
  function DownloadSafebox(AConnection : TNetConnection; ASafeboxHash : TRawBytes; ASafeboxChunkStream : TStream) : Boolean;
  Var sendData,receiveData : TStream;
    headerdata : TNetHeaderData;
    request_id : Cardinal;
    c : Cardinal;
    LRandomMilis : Integer;
    LsafeBoxHeader : TPCSafeBoxHeader;
    LErrors : String;
    Ltc : TTickCount;
  Begin
    Result := False;
    Ltc := TPlatform.GetTickCount;
    sendData := TMemoryStream.Create;
    receiveData := TMemoryStream.Create;
    try
      c := FOwner.FSavedSafeboxHighOperationBlock.block;
      sendData.Write(c,4); // 4 bytes for blockcount
      TStreamOp.WriteAnsiString(SendData,ASafeboxHash); // SafeboxHash
      sendData.Write(FBlockStart,4);
      c := FBlockStart + FBlocksCount - 1;
      sendData.Write(c,4);
      //
      request_id := TNetData.NetData.NewRequestId;
      if AConnection.DoSendAndWaitForResponse(CT_NetOp_GetSafeBox,request_id,sendData,receiveData,30000,headerdata) then begin
        if HeaderData.is_error then exit;
        ASafeboxChunkStream.Position := 0;
        ASafeboxChunkStream.Size:=0;
        If Not TPCChunk.LoadSafeBoxFromChunk(receiveData,ASafeboxChunkStream,LsafeBoxHeader,LErrors) then begin
          AConnection.DisconnectInvalidClient(false,'Invalid received chunk: '+LErrors);
          exit;
        end;
        If (Not (TBaseType.Equals(LsafeBoxHeader.safeBoxHash,ASafeboxHash))) or (LsafeBoxHeader.startBlock<>FBlockStart) or (LsafeBoxHeader.endBlock<>c) or
          (LsafeBoxHeader.protocol<CT_PROTOCOL_2) or
          (LsafeBoxHeader.protocol>CT_BlockChain_Protocol_Available) then begin
          Lerrors := Format('Invalid received Safebox chunk Blockscount:%d %d - from:%d %d to %d %d - SafeboxHash:%s %s - Protocol %d',
              [LsafeBoxHeader.blocksCount,FBlocksCount,LsafeBoxHeader.startBlock,FBlockStart,LsafeBoxHeader.endBlock,c,
               LsafeBoxHeader.safeBoxHash.ToHexaString,ASafeboxHash.ToHexaString,LsafeBoxHeader.protocol]);
          AConnection.DisconnectInvalidClient(false,'Invalid received chunk: '+Lerrors);
          exit;
        end;
        Result := True;
        TLog.NewLog(ltdebug,Self.ClassName,Format('Received Safebox chunk %d..%d from %s in %.2f secs',[FBlockStart,FBlockStart+FBlocksCount,AConnection.ClientRemoteAddr,TPlatform.GetElapsedMilliseconds(Ltc)/1000]));
      end else begin
        Lerrors := 'No response on DownloadSafeBoxChunk';
        TLog.NewLog(ltdebug,Self.ClassName,Lerrors);
      end;
    finally
      receiveData.Free;
      sendData.Free;
    end;
  end;
var LConnection : TNetConnection;
begin
  repeat
    FStream.Position := 0;
    FStream.Size := 0; // Clear
    // Search for a connection
    FOwner.FChunks.LockList;
    try
      if TNetData.NetData.GetConnection(Random(TNetData.NetData.ConnectionsCountAll),LConnection) then begin
        if Assigned(LConnection) And (Not LConnection.Connected) then LConnection := Nil;
      end else LConnection := Nil;
      if Assigned(LConnection) then begin
        if TNetData.NetData.ConnectionLock(Self,LConnection,100) then begin
          TNetData.NetData.ConnectionUnlock(LConnection);
        end else LConnection := Nil;
      end;
    finally
      FOwner.FChunks.UnlockList;
    end;
    if Assigned(LConnection) then begin
      if DownloadSafebox(LConnection,FOwner.FSavedSafeboxHighOperationBlock.initial_safe_box_hash,Self.FStream) then Break;
    end;
    Sleep(100);
  until Terminated;
end;

constructor TPCDownloadSafebox.TDownloadSBThread.Create(AOwner : TPCDownloadSafebox; ABlockStart, ACount : Cardinal);
begin
  FOwner := AOwner;
  FBlockStart := ABlockStart;
  FBlocksCount := ACount;
  FStream := TPCTemporalFileStream.Create(Format('CHUNK_%.8d_%.8d',[ABlockStart,ABlockStart+ACount-1]));
  inherited Create(True);

  FreeOnTerminate := False;
  Suspended := False;
end;

destructor TPCDownloadSafebox.TDownloadSBThread.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

{ TPCDownloadSafebox }

constructor TPCDownloadSafebox.Create;
begin
  FNode := TNode.Node;
  FDownloadedBank := TPCBank.Create(Nil);
  FChunks := TPCThreadList<TPCDownloadSafeboxChunk>.Create('');
  FSavedSafeboxHighOperationBlock := CT_OperationBlock_NUL;
  FOnProgressNotify := Nil;
end;

destructor TPCDownloadSafebox.Destroy;
var i : Integer;
  Ll : TList<TPCDownloadSafeboxChunk>;
begin
  Ll := FChunks.LockList;
  Try
    for i:=0 to Ll.Count-1 do begin
      if assigned(Ll.Items[i].Thread) then begin
        Ll.Items[i].Thread.Terminate;
        Ll.Items[i].Thread.WaitFor;
        Ll.Items[i].Thread.Free;
      end;
    end;
  Finally
    FChunks.UnlockList;
  End;
  FreeAndNil(FChunks);
  FreeAndNil(FDownloadedBank);
  inherited;
end;

function TPCDownloadSafebox.DownloadSafebox(AOwnerThread : TThread; ASavedSafeboxHighOperationBlock: TOperationBlock; AChunks : TPCSafeboxChunks): Boolean;
var LDownloadedSafeboxBlocksCount, request_id : Cardinal;
  LreceivedChunk : TStream;
  safeBoxHeader : TPCSafeBoxHeader;
  i : Integer;
  LContinue : Boolean;
  Ll : TList<TPCDownloadSafeboxChunk>;
  Ldsbc : TPCDownloadSafeboxChunk;
  LTerminated : Boolean;
  LTerminatedCount, LTotal : Integer;
  LFileName, Lstatus : String;
Begin
  Result := False;
  // Check
  LDownloadedSafeboxBlocksCount := ((ASavedSafeboxHighOperationBlock.block DIV CT_BankToDiskEveryNBlocks)) * CT_BankToDiskEveryNBlocks;
  if LDownloadedSafeboxBlocksCount<>ASavedSafeboxHighOperationBlock.block then Exit(False);
  FSavedSafeboxHighOperationBlock := ASavedSafeboxHighOperationBlock;

  LTotal := 0;
  Ll := FChunks.LockList;
  Try
    for i:=0 to ((LDownloadedSafeboxBlocksCount-1) DIV CT_MAX_SAFEBOXCHUNK_BLOCKS) do begin
      Ldsbc.BlockStart := (i * CT_MAX_SAFEBOXCHUNK_BLOCKS);
      Ldsbc.Count := CT_MAX_SAFEBOXCHUNK_BLOCKS;
      if Ldsbc.BlockStart + Ldsbc.Count > LDownloadedSafeboxBlocksCount then begin
        Ldsbc.Count := LDownloadedSafeboxBlocksCount - Ldsbc.BlockStart;
      end;
      Ldsbc.Thread := Nil;
      Ll.Add(Ldsbc);
    end;
    LTotal := Ll.Count;
  Finally
    FChunks.UnlockList;
  End;

  if Assigned(AOwnerThread) then LContinue := Not AOwnerThread.CheckTerminated
  else LContinue := True;

  LTerminated := False;

  while (FNode.NetServer.Active) And LContinue And (Not LTerminated) do begin
    //
    LTerminatedCount := 0;
    Ll := FChunks.LockList;
    Try
      i := 0;
      for i:=0 to Ll.Count-1 do begin
        Ldsbc := Ll.Items[i];
        if Not Assigned(Ldsbc.Thread) then begin
          Ldsbc.Thread := TPCDownloadSafebox.TDownloadSBThread.Create(Self,Ldsbc.BlockStart,Ldsbc.Count);
          Ll.Items[i] := Ldsbc;
        end else begin
          if Ldsbc.Thread.Terminated then Inc(LTerminatedCount);
        end;
      end;
      LTerminated := LTerminatedCount >= Ll.Count;
      Lstatus := Format('Downloading Safebox chunks %d/%d',[LTerminatedCount,Ll.Count]);
    Finally
      FChunks.UnlockList;
    End;

    Sleep(10);
    //
    if Assigned(AOwnerThread) then LContinue := Not AOwnerThread.CheckTerminated
    else LContinue := True;
    if (LContinue) and (Assigned(FOnProgressNotify)) then begin
      FOnProgressNotify(Self,LStatus,LTerminatedCount,LTotal);
    end;
  end;

  if (LTerminated) And (LContinue) then begin
    AChunks.Clear;
    Ll := FChunks.LockList;
    try
      for i := 0 to Ll.Count-1 do begin
        Ll.Items[i].Thread.Stream.Position := 0;
        AChunks.AddChunk(Ll.Items[i].Thread.Stream,True,True);
        Ll.Items[i].Thread.Stream := Nil;
      end;
      LFileName := TNode.Node.Bank.GetStorageFolder('')+PathDelim+'safebox_'+IntToStr(LDownloadedSafeboxBlocksCount)+'.safebox';

      if (Assigned(FOnProgressNotify)) then begin
        FOnProgressNotify(Self,Format('Saving Safebox %d chunks to %s',[LTotal,ExtractFileName(LFileName)]),0,0);
      end;
      AChunks.SaveSafeboxfile(LFileName);
    finally
      FChunks.UnlockList;
    end;
    //
    Result := True;
  end;


end;

initialization
finalization
end.
