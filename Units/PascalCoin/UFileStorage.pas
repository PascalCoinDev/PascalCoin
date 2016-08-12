unit UFileStorage;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, UBlockChain;

Type
  TFileStorage = Class(TStorage)
  private
    FBaseDataFolder : AnsiString;
    FIsRestoring : Boolean;
    Function GetFolder(Const AOrphan : TOrphan): AnsiString;
  protected
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(Start_Block : Cardinal; Const DestOrphan : TOrphan) : Boolean; override;
    Function DoSaveBank : Boolean; override;
    Function DoRestoreBank(max_block : Int64) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal; Orphan : TOrphan); override;
    Function BlockExists(Block : Cardinal) : Boolean; override;
  public
    Constructor Create(AOwner : TComponent); Override;
    Class Function GetBlockChainFileName(Const BaseDataFolder : AnsiString; block : Cardinal) : AnsiString;
    Class Function GetBankFileName(Const BaseDataFolder : AnsiString; block : Cardinal) : AnsiString;
  End;

implementation

uses
  SysUtils, ULog, Forms, UConst;

{ TFileStorage }

procedure _CopyFile(const FileName, DestName: string);
var CopyBuffer   : Pointer; { buffer for copying }
  BytesCopied  : Longint;
  Source, Dest : Integer; { handles }
  Destination  : TFileName; { holder for expanded destination name }
const ChunkSize  : Longint = 8192; { copy in 8K chunks }
begin
  Destination := DestName;
  GetMem(CopyBuffer, ChunkSize); { allocate the buffer }
  try
   Source := FileOpen(FileName, fmShareDenyWrite); { open source file }
   if (Source<0) then raise EFOpenError.CreateFmt('Error: Can''t open file!', [FileName]);
   try
     Dest := FileCreate(Destination); { create output file; overwrite existing }
     if (Dest<0) then raise EFCreateError.CreateFmt('Error: Can''t create file!', [Destination]);
     try
       repeat
         BytesCopied := FileRead(Source, CopyBuffer^, ChunkSize); { read chunk }
         if BytesCopied > 0  {if we read anything... }
            then FileWrite(Dest, CopyBuffer^, BytesCopied); { ...write chunk }
       until BytesCopied < ChunkSize; { until we run out of chunks }
     finally
       FileClose(Dest); { close the destination file }
     end;
   finally
     FileClose(Source); { close the source file }
   end;
  finally
   FreeMem(CopyBuffer, ChunkSize); { free the buffer }
  end;
end;

function TFileStorage.BlockExists(Block: Cardinal): Boolean;
begin
  Result := FileExists(GetBlockChainFileName(GetFolder(Orphan),Block));
end;

constructor TFileStorage.Create(AOwner: TComponent);
begin
  FIsRestoring := false;
  FBaseDataFolder := '';
  inherited;
end;

procedure TFileStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal; Orphan: TOrphan);
Var bfn : AnsiString;
begin
  while (BlockExists(StartingDeleteBlock)) do begin
    DeleteFile(GetBlockChainFileName(GetFolder(Orphan),StartingDeleteBlock));
    inc(StartingDeleteBlock);
    bfn := GetBankFileName(GetFolder(Orphan),StartingDeleteBlock);
    if FileExists(bfn) then begin
      DeleteFile(bfn);
    end;
  end;
end;

function TFileStorage.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
Var filename : AnsiString;
  fs : TFileStream;
  e : AnsiString;
begin
  Result := false;
  filename :=  GetBlockChainFileName(GetFolder(Orphan),Block);
  if Not FileExists(filename) then begin
    TLog.NewLog(lterror,Classname,'Operations ('+inttostr(Block)+') file not found: '+filename);
    exit;
  end;
  fs := TFileStream.Create(filename, fmOpenRead);
  try
    If Operations.LoadFromStream(false,true,fs, e) then result := true
    else begin
      TLog.NewLog(lterror,Classname,'Error reading file: '+filename+' Errors: '+e);
    end;
  finally
    fs.Free;
  end;
end;

function TFileStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan): Boolean;
Var fn,destfn,bankfilename : AnsiString;
  i : Cardinal;
begin
  if Bank.BlocksCount<Start_Block then exit;
  TLog.NewLog(ltInfo,Classname,'Moving operations ('+inttostr(start_block)+' to '+inttostr(Bank.BlocksCount)+' from '+GetFolder('')+' to '+GetFolder(DestOrphan));
  for i := start_block to Bank.BlocksCount - 1 do begin
    fn := GetBlockChainFileName(GetFolder(Orphan),i);
    destfn := GetBlockChainFileName(GetFolder(DestOrphan),i);
    if FileExists(destfn) then TLog.NewLog(lterror,Classname,'File exists '+fn);
    if Not FileExists(fn) then TLog.NewLog(lterror,Classname,'File not exists '+fn)
    else _CopyFile(fn,destfn);
    // Delete banks:
    bankfilename := GetBankFileName(GetFolder(Orphan),i);
    if (bankfilename<>'') then begin
      if not DeleteFile(bankfilename) then begin
        TLog.NewLog(lterror,Classname,'Cannot delete old bank file: '+bankfilename);
      end;
    end;
  end;
end;

function TFileStorage.DoRestoreBank(max_block: Int64): Boolean;
  function LoadOperationsFromFile(blockcount : Cardinal; Operations : TPCOperationsComp; var errors : AnsiString) : Boolean;
  var filename : AnsiString;
    fs : TFileStream;
  Begin
    Result := false;
    filename := GetBlockChainFileName(GetFolder(Orphan),blockcount);
    if FileExists(filename) then begin
      fs := TFileStream.Create(filename, fmOpenRead);
      try
          Result := Operations.LoadFromStream(false,true,fs, errors);
      finally
        fs.Free;
      end;
    end else errors := 'File operations (Block: '+inttostr(blockcount)+') not exists:'+filename;
  End;
var
    sr: TSearchRec;
    FileAttrs,errcode: Integer;
    folder : AnsiString;
    filename,auxfn : AnsiString;
    fs : TFileStream;
    errors : AnsiString;
    c,lastc : Cardinal;
    operations : TPCOperationsComp;
begin
  FileAttrs := faArchive;
  folder := GetFolder(Orphan);
  filename := '';
  operations := TPCOperationsComp.Create(Nil);
  try
    if SysUtils.FindFirst(folder+'\bank*.bank', FileAttrs, sr) = 0 then begin
      lastc := 0;
      repeat
        if (sr.Attr and FileAttrs) = sr.Attr then begin
          auxfn := ChangeFileExt(sr.Name,'');
          val(copy(auxfn,5,length(auxfn)),c,errcode);
          if (errcode=0) And ((c<=max_block)) then begin
            if LoadOperationsFromFile(c,operations,errors) then begin
              if (filename='') then begin
                filename := sr.Name;
                lastc := c;
              end else if (lastc<c) then begin
                filename := sr.Name;
                lastc := c;
              end;
            end else begin
              TLog.NewLog(lterror,ClassName,'Found a bank in file:'+filename+' but not operations:'+errors);
            end;
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if (filename<>'') then begin
      fs := TFileStream.Create(folder+'\'+filename,fmOpenRead);
      try
        if not Bank.LoadFromStream(fs,errors) then begin
          TLog.NewLog(lterror,ClassName,'Error reading bank from file: '+filename+ ' Error: '+errors);
        end;
      finally
        fs.Free;
      end;
    end;
  finally
    operations.Free;
  end;
end;

function TFileStorage.DoSaveBank: Boolean;
var fs: TFileStream;
    bankfilename: AnsiString;
begin
  Result := true;
  bankfilename := GetBankFileName(GetFolder(Orphan),Bank.BlocksCount);
  if (bankfilename<>'') then begin
    fs := TFileStream.Create(bankfilename,fmCreate);
    try
      fs.Size := 0;
      Bank.SaveToStream(fs);
    finally
      fs.Free;
    end;
  end;
end;

function TFileStorage.DoSaveBlockChain(Operations: TPCOperationsComp): Boolean;
Var
  fs: TFileStream;
  bankfilename,folder: AnsiString;
begin
  folder := GetFolder(Orphan);
  If Not ForceDirectories(folder) then exit;
  if Not FIsRestoring then begin
    fs := TFileStream.Create(GetBlockChainFileName(folder,Operations.OperationBlock.block), fmCreate);
    try
      fs.Size := 0;
      Operations.SaveToStream(true,false,fs);
    finally
      fs.Free;
    end;
  end;
  Result := true;
  SaveBank;
end;

class function TFileStorage.GetBankFileName(const BaseDataFolder: AnsiString; block: Cardinal): AnsiString;
Var c : Cardinal;
  folder : AnsiString;
begin
  Result := '';
  if (Block MOD CT_BankToDiskEveryNBlocks)<>0 then exit; // No bank!
  If not ForceDirectories(BaseDataFolder) then exit;
  Result := BaseDataFolder + '\bank'+Format('%.6d',[Block])+'.bank';
end;

class function TFileStorage.GetBlockChainFileName(const BaseDataFolder: AnsiString; block: Cardinal): AnsiString;
Var c : Cardinal;
  folder : AnsiString;
begin
  Result := '';
  c := block DIV 1000;
  folder := BaseDataFolder+'\block'+Format('%.4d',[c]);
  If not ForceDirectories(folder) then exit;
  Result :=  folder+'\op'+Format('%.6d',[block])+ '.bco';
end;


function TFileStorage.GetFolder(const AOrphan: TOrphan): AnsiString;
begin
  if FBaseDataFolder = '' then begin
    FBaseDataFolder := ExtractFileDir(Application.ExeName)+'\'+ChangeFileExt(ExtractFileName(Application.ExeName),'')+ '\BCOperations';
  end;
  if AOrphan<>'' then Result := FBaseDataFolder + '\'+AOrphan
  else Result := FBaseDataFolder;
end;

end.
