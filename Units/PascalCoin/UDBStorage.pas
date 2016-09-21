unit UDBStorage;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

{ ABOUT THIS UNIT:
  This unit is used only in Windows version to use an Microsoft Access database
  to store data to be more accessible and quickly readable.

  All the blockchain is stored in a table "tblockchain" in stream format, but
  also the operationblock is also saved in individual fields to easily
  search it with SQL select statements.

  This unit will store 2 additional tables:
  First table is "tbank" that will help this application when reading all
  the blockchain and only will need to read from "last stored bank"

  Second table is "toperations" thans stores all the operations made to
  the accounts (transfers, change key...). It's usefull to quickly search
  operations that affects an account.

  FINAL NOTE:
  To compile in other OS you can use "UFileStorage" or another unit to
  store data in other database types or simply as a file }

interface

uses
  UBlockChain, Classes, db, ADODB, SysUtils, UAccounts, ULog, Variants, UCrypto,
  UConst, UOpTransaction, UThread;

Const
  CT_TblName_BlockChain = 'tblockchain';
  CT_TblName_Operations = 'toperations';
  CT_TblName_Bank = 'tbank';

  CT_TblFld_BlockChain_id = 'idblockchain';
  CT_TblFld_BlockChain_block = 'block';
  CT_TblFld_BlockChain_accountkey = 'accountkey';
  CT_TblFld_BlockChain_s_reward = 's_reward';
  CT_TblFld_BlockChain_s_fee = 's_fee';
  CT_TblFld_BlockChain_protocol_version = 'protocol_version';
  CT_TblFld_BlockChain_protocol_available = 'protocol_available';
  CT_TblFld_BlockChain_s_timestamp = 's_timestamp';
  CT_TblFld_BlockChain_s_compact_target = 's_compact_target';
  CT_TblFld_BlockChain_s_nonce = 's_nonce';
  CT_TblFld_BlockChain_rawpayload = 'rawpayload';
  CT_TblFld_BlockChain_safe_box_hash = 'safe_box_hash';
  CT_TblFld_BlockChain_operations_hash = 'operations_hash';
  CT_TblFld_BlockChain_proof_of_work = 'proof_of_work';
  CT_TblFld_BlockChain_operations_stream = 'operations_stream';
  CT_TblFld_BlockChain_orphan = 'orphan';
  CT_TblFld_BlockChain_operations_count = 'operations_count';

  CT_TblFld_Operations_id = 'idoperation';
  CT_TblFld_Operations_optype = 'optype';
  CT_TblFld_Operations_block = 'block';
  CT_TblFld_Operations_s_timestamp = 's_timestamp';
  CT_TblFld_Operations_nopblock = 'nopblock';
  CT_TblFld_Operations_account = 'account';
  CT_TblFld_Operations_other_account = 'other_account';
  CT_TblFld_Operations_n_operation = 'n_operation';
  CT_TblFld_Operations_optype_op = 'optype_op';
  CT_TblFld_Operations_s_amount = 's_amount';
  CT_TblFld_Operations_s_fee = 's_fee';
  CT_TblFld_Operations_s_balance = 's_balance';
  CT_TblFld_Operations_rawpayload = 'payload';
  CT_TblFld_Operations_newaccountkey = 'newaccountkey';
  CT_TblFld_Operations_orphan = 'orphan';

  CT_TblFld_Bank_id = 'idbank';
  CT_TblFld_Bank_block = 'block';
  CT_TblFld_Bank_bank_stream = 'bank_stream';
  CT_TblFld_Bank_orphan = 'orphan';

Type
  TOperationResume = Record
    Block : Cardinal;
    time : Cardinal;
    AffectedAccount : Cardinal;
    OperationTxt : AnsiString;
    Amount : Int64;
    Fee : Int64;
    Balance : Int64;
    OriginalPayload : TRawBytes;
    PrintablePayload : AnsiString;
  end;

Const
  CT_TOperationResume_NUL : TOperationResume = (Block:0;time:0;AffectedAccount:0; OperationTxt:'';Amount:0;Fee:0;Balance:0;OriginalPayload:'';PrintablePayload:'');

Type

  TOperationsResumeList = Class
  private
    FList : TPCThreadList;
    function GetOperationResume(index: Integer): TOperationResume;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Add(Const OperationResume : TOperationResume);
    Function Count : Integer;
    Procedure Delete(index : Integer);
    Procedure Clear;
    Property OperationResume[index : Integer] : TOperationResume read GetOperationResume; default;
  End;

  TDBStorage = Class(TStorage)
  private
    FAdoConnection : TADOConnection;
    FAccessFileName: AnsiString;
    procedure SetAccessFileName(const Value: AnsiString);
  protected
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(Start_Block : Cardinal; Const DestOrphan : TOrphan) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal; Orphan : TOrphan); override;
    Function DoSaveBank : Boolean; override;
    Function DoRestoreBank(max_block : Int64) : Boolean; override;
    Function BlockExists(Block : Cardinal) : Boolean; override;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; override;
    Property AccessFileName : AnsiString read FAccessFileName write SetAccessFileName;
    Function SQL_UPDATE(const TblName : AnsiString; Const SetValue : AnsiString; const Where : String): Boolean;
    Function SQL_DELETE(const TblName : AnsiString; const Where : String): Boolean;
    Class Function ValueToSql(const Value : Variant) : String;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); override;
    Property ADOConnection : TADOConnection read FAdoConnection;
    Class Function DBPayloadToReadableText(Const DBRawPayload : TRawBytes; Var ReadableText : AnsiString) : Boolean;
    //
    Class Function OperationToOperationResume(Operation : TPCOperation; Affected_account_number : Cardinal; var OperationResume : TOperationResume) : Boolean;
    Function GetOperationsFromAccount(Const OperationsResume : TOperationsResumeList; account_number : Cardinal; start,max : Integer) :Integer;
    Class Function GetOperationsFromAccountSQL(account_number, block_start, block_end : Int64; dateStart,dateEnd : TDate; start,max : Integer) :String;
    Class Function GetBlockChainSQL(block_start, block_end : Int64; dateStart,dateEnd : TDate; start,max : Integer) :String;
    Class Function BlobSaveToStream(Blob : TBlobField; Stream : TStream) : Boolean;
    Class Procedure BlobLoadFromStream(Blob : TBlobField; Stream : TStream);
    //
    Class Function DBStringFieldToRaw(Field : TField; var raw : TRawBytes) : Boolean;
    Class Procedure DBRawToStringField(Field : TField; const raw : TRawBytes);
  End;

implementation

uses
  Forms, UTime;

{ TDBStorage }

class procedure TDBStorage.BlobLoadFromStream(Blob: TBlobField; Stream: TStream);
Var ms : TMemoryStream;
  b : Byte;
begin
  { NOTE:
    Access database has a "bug" when storing data as a memo if data size is odd.
    So we need to indicate when data size is odd in order to read correctly
    data when reading
  }
  ms := TMemoryStream.Create;
  try
    if stream.Size MOD 2=0 then b := 0
    else b := 1;
    ms.Write(b,1);
    Stream.Position := 0;
    ms.CopyFrom(Stream,Stream.Size);
    ms.Position := ms.Size;
    ms.Write(b,1);
    ms.Position := 0;
    Blob.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

class function TDBStorage.BlobSaveToStream(Blob: TBlobField; Stream: TStream) : Boolean;
Var ms : TMemoryStream;
  ac : AnsiChar;
  b : Byte;
begin
  { NOTE:
    Access database has a "bug" when storing data as a memo if data size is odd.
    So we need to indicate when data size is odd in order to read correctly
    data when reading
  }
  Result := false;
  ms := TMemoryStream.Create;
  try
    Blob.SaveToStream(ms);
    if ms.size>0 then begin
      ms.Position := 0;
      ms.Read(b,1);
      if (b<>0) and (b<>1) then exit;
      stream.size := 0;
      if b=0 then stream.CopyFrom(ms,ms.Size-2)
      else stream.CopyFrom(ms,ms.Size-1);
      Result := true;
    end else Result := true;
  finally
    ms.Free;
  end;
end;

function TDBStorage.BlockExists(Block: Cardinal): Boolean;
Var ds : TADOQuery;
  whereorphan : AnsiString;
begin
  if Orphan='' then whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)'
  else whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
  ds := TADOQuery.Create(Self);
  Try
    ds.Connection := FAdoConnection;
    ds.SQL.Text := 'SELECT * FROM '+CT_TblName_BlockChain+' WHERE ('+CT_TblFld_BlockChain_block+'='+Inttostr(Block)+') AND '+whereorphan+
      ' ORDER BY '+CT_TblFld_BlockChain_id;
    ds.Open;
    Result := Not ds.IsEmpty;
  Finally
    ds.Free;
  End;
end;

procedure TDBStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  inherited;
  if CopyFrom is TDBStorage then AccessFileName := TDBStorage(CopyFrom).AccessFileName;
end;

constructor TDBStorage.Create(AOwner: TComponent);
begin
  inherited;
  FAdoConnection := TADOConnection.Create(Self);
end;

class function TDBStorage.DBPayloadToReadableText(const DBRawPayload: TRawBytes; var ReadableText: AnsiString): Boolean;
begin
  Result := TCrypto.IsHumanReadable(DBRawPayload);
  if Result then ReadableText := DBRawPayload;
end;

class procedure TDBStorage.DBRawToStringField(Field: TField; const raw: TRawBytes);
  {
  Microsoft Access Database does not store string values as a Raw value... if there
  is a "0" character bad things happen... This function ensures that there will
  not be any "0" in a saved string.
  }
Var dbraw : TRawBytes;
  i : Integer;
begin
  dbraw := '';
  for i := 1 to length(raw) do begin
    if raw[i]=chr(0) then dbraw := dbraw + chr(255)+chr(1)
    else if raw[i]=chr(255) then dbraw := dbraw + chr(255)+chr(2)
    else dbraw := dbraw + raw[i];
  end;
  Field.AsAnsiString := dbRaw;
end;

class function TDBStorage.DBStringFieldToRaw(Field: TField; var raw: TRawBytes): Boolean;
  {
  Microsoft Access Database does not store string values as a Raw value... if there
  is a "0" character bad things happen... This function restores a previously
  raw value saved without "0" character.
  }
Var dbraw : TRawBytes;
  i : Integer;
begin
  dbraw := Field.AsAnsiString;
  Result := false;
  raw := '';
  i := 1;
  while (i<=length(dbraw)) do begin
    if (dbraw[i]=chr(255)) then begin
      if (i<length(dbraw)) then begin
        inc(i);
        if dbraw[i]=chr(1) then raw := raw + chr(0)
        else if dbraw[i]=chr(2) then raw := raw + chr(255)
        else exit;
      end else exit;
    end else raw := raw + dbraw[i];
    inc(i);
  end;
  Result := true;
end;

destructor TDBStorage.Destroy;
begin
  FreeAndNil(FAdoConnection);
  inherited;
end;

procedure TDBStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal; Orphan: TOrphan);
Var ds : TADOQuery;
  whereorphan : AnsiString;
begin
  if Orphan='' then begin
    whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)';
    TLog.NewLog(ltdebug,Classname,Format('DoDeleteBlockChainBlocks Starting Block:%d (Main blockchain)',[StartingDeleteBlock,Orphan]));
  end else begin
    whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
    TLog.NewLog(ltdebug,Classname,Format('DoDeleteBlockChainBlocks Starting Block:%d (Orphan:%s)',[StartingDeleteBlock,Orphan]));
  end;
  SQL_DELETE(CT_TblName_BlockChain,'('+CT_TblFld_BlockChain_block+'>='+inttostr(StartingDeleteBlock)+') AND '+whereorphan);
  if Orphan='' then begin
    whereorphan := '('+CT_TblFld_Bank_orphan+' IS NULL)';
  end else begin
    whereorphan := '('+CT_TblFld_Bank_orphan+' LIKE '''+Orphan+''')';
  end;
  SQL_DELETE(CT_TblName_Bank,'('+CT_TblFld_Bank_block+'>='+inttostr(StartingDeleteBlock)+') AND '+whereorphan);
  if Orphan='' then begin
    whereorphan := '('+CT_TblFld_Operations_orphan+' IS NULL)';
  end else begin
    whereorphan := '('+CT_TblFld_Operations_orphan+' LIKE '''+Orphan+''')';
  end;
  SQL_DELETE(CT_TblName_Operations,'('+CT_TblFld_Operations_block+'>='+inttostr(StartingDeleteBlock)+') AND '+whereorphan);
end;

function TDBStorage.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
Var ds : TADOQuery;
  whereorphan : AnsiString;
  bf : TBlobField;
  ms : TMemoryStream;
  errors : AnsiString;
begin
  if Orphan='' then begin
    whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)';
    TLog.NewLog(ltdebug,Classname,Format('DoLoadBlockChain Block:%d (Main blockchain)',[Block,Orphan]));
  end else begin
    whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
    TLog.NewLog(ltdebug,Classname,Format('DoLoadBlockChain Block:%d (Orphan:%s)',[Block,Orphan]));
  end;
  Result := false;
  Operations.Clear(true);
  ds := TADOQuery.Create(Self);
  Try
    ds.Connection := FAdoConnection;
    ds.SQL.Text := 'SELECT * FROM '+CT_TblName_BlockChain+' WHERE ('+CT_TblFld_BlockChain_block+'='+Inttostr(Block)+') AND '+whereorphan+
      ' ORDER BY '+CT_TblFld_BlockChain_id;
    ds.Open;
    if Not ds.IsEmpty then begin
      bf := ds.FieldByName(CT_TblFld_BlockChain_operations_stream) as TBlobField;
      ms := TMemoryStream.Create;
      try
        BlobSaveToStream(bf,ms);
        ms.Position := 0;
        Result := Operations.LoadBlockFromStream(ms,errors);
        if Not Result then begin
          TLog.NewLog(lterror,Classname,Format('Error reading databse Block %d: %s Stream size:%d',[Block,errors,ms.Size]));
        end;
      finally
        ms.Free;
      end;
    end else begin
      TLog.NewLog(lterror,Classname,Format('Block %d not found in database. SQL:%s',[Block,ds.SQL.Text]));
    end;
  Finally
    ds.Free;
  End;
end;

function TDBStorage.DoMoveBlockChain(Start_Block: Cardinal; const DestOrphan: TOrphan): Boolean;
Var whereorphan : AnsiString;
  setvalue : AnsiString;
begin
  Result := true;
  FAdoConnection.BeginTrans;
  try
    TLog.NewLog(ltdebug,Classname,Format('DoMoveBlockChain start:%d From:%s to Dest:%s',[start_block,Orphan,DestOrphan]));
    // Moving BlockChain table
    if Orphan='' then whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)'
    else whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
    if DestOrphan='' then setvalue := CT_TblFld_BlockChain_orphan+'=NULL'
    else setvalue := CT_TblFld_BlockChain_orphan+ '='''+DestOrphan+'''';
    SQL_UPDATE(CT_TblName_BlockChain,setvalue,whereorphan+' AND ('+CT_TblFld_BlockChain_block+'>='+inttostr(Start_Block)+')');

    // Moving Operations table
    if Orphan='' then whereorphan := '('+CT_TblFld_Operations_orphan+' IS NULL)'
    else whereorphan := '('+CT_TblFld_Operations_orphan+' LIKE '''+Orphan+''')';
    if DestOrphan='' then setvalue := CT_TblFld_Operations_orphan+'=NULL'
    else setvalue := CT_TblFld_Operations_orphan+ '='''+DestOrphan+'''';
    SQL_UPDATE(CT_TblName_Operations,setvalue,whereorphan+' AND ('+CT_TblFld_Operations_block+'>='+inttostr(Start_Block)+')');

    // Moving Bank table
    if Orphan='' then whereorphan := '('+CT_TblFld_Bank_orphan+' IS NULL)'
    else whereorphan := '('+CT_TblFld_Bank_orphan+' LIKE '''+Orphan+''')';
    if DestOrphan='' then setvalue := CT_TblFld_Bank_orphan+'=NULL'
    else setvalue := CT_TblFld_Bank_orphan+ '='''+DestOrphan+'''';
    SQL_UPDATE(CT_TblName_Bank,setvalue,whereorphan+' AND ('+CT_TblFld_Bank_block+'>='+inttostr(Start_Block)+')');
    FAdoConnection.CommitTrans;
  Except
    // Rollback changes
    On E:Exception do begin
      FAdoConnection.RollbackTrans;
      Result :=False;
      Raise;
    end;
  end;
end;

function TDBStorage.DoRestoreBank(max_block: Int64): Boolean;
var ds : TAdoQuery;
  whereorphan : AnsiString;
  sql,errors : AnsiString;
  bf : TBlobField;
  ms : TMemoryStream;
begin
  Result := false;
  TLog.NewLog(ltdebug,Classname,'DoRestoreBank max:'+inttostr(max_block));
  if Orphan='' then whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)'
  else whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
  sql := 'SELECT * FROM '+CT_TblName_Bank+' WHERE ('+whereorphan+')';
  if max_block<MaxInt then sql := sql+ ' AND ('+CT_TblFld_Bank_block+'<='+Inttostr(max_block)+') ';
  sql := sql +' ORDER BY '+CT_TblFld_Bank_block+' DESC';
  ds := TADOQuery.Create(Self);
  Try
    ds.Connection := FAdoConnection;
    ds.CursorLocation := clUseServer; // Important: This improves speed!
    ds.SQL.Text := sql;
    ds.Open;
    while (Not ds.Eof) do begin
      bf := ds.FieldByName(CT_TblFld_Bank_bank_stream) as TBlobField;
      ms := TMemoryStream.Create;
      try
        BlobSaveToStream(bf,ms);
        ms.Position := 0;
        Result := Bank.LoadBankFromStream(ms,errors);
        if Not Result then begin
          TLog.NewLog(lterror,Classname,Format('Error reading databse Bank block %d: %s',[ds.FieldByName(CT_TblFld_Bank_block).AsInteger,errors]));
        end else begin
          // Bye bye
          Result := true;
          break;
        end;
      finally
        ms.Free;
      end;
      ds.Next;
    end;
  Finally
    ds.Free;
  End;
end;

function TDBStorage.DoSaveBank: Boolean;
var ds : TAdoQuery;
  whereorphan : AnsiString;
  sql,errors : AnsiString;
  bf : TBlobField;
  ms : TMemoryStream;
begin
  Result := false;
  FAdoConnection.BeginTrans;
  try
    TLog.NewLog(ltdebug,Classname,'DoSaveBank blockscount:'+inttostr(Bank.BlocksCount));
    if Orphan='' then whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)'
    else whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
    SQL_DELETE(CT_TblName_Bank,whereorphan+' AND ('+CT_TblFld_Bank_block+'='+inttostr(Bank.BlocksCount)+')');

    ds := TADOQuery.Create(Self);
    try
      ds.Connection := FAdoConnection;
      SQL := 'SELECT * FROM '+CT_TblName_Bank+' WHERE (1=0)';
      ds.SQL.Text := sql;
      ds.Open;
      ds.Insert;
      ds.FieldByName(CT_TblFld_Bank_block).AsInteger := Bank.BlocksCount;
      if Orphan<>'' then ds.FieldByName(CT_TblFld_Bank_orphan).AsAnsiString := Orphan
      else ds.FieldByName(CT_TblFld_Bank_orphan).Value := Null;
      bf := ds.FieldByName(CT_TblFld_Bank_bank_stream) as TBlobField;
      ms := TMemoryStream.Create;
      Try
        Bank.SaveBankToStream(ms);
        ms.Position := 0;
        BlobLoadFromStream(bf,ms);
        TLog.NewLog(ltdebug,Classname,Format('Saving bank of block %d with stream size %d bytes',[Bank.BlocksCount,ms.Size]));
      Finally
        ms.Free;
      End;
      ds.Post;
      Result := true;
    finally
      ds.Free;
    end;
    FAdoConnection.CommitTrans;
  Except
    FAdoConnection.RollbackTrans;
    raise;
  end;
end;

function TDBStorage.DoSaveBlockChain(Operations: TPCOperationsComp): Boolean;
  Function GetOperationBalance(iOperation : Integer; nAccount : Cardinal) : Int64;
  Var i : Integer;
    op : TPCOperation;
  Begin
    Result := Operations.bank.SafeBox.Account(nAccount).balance;
    for i := (Operations.OperationsHashTree.OperationsCount-1) downto (iOperation+1) do begin
      op := Operations.OperationsHashTree.GetOperation(i);
      Case op.OpType of
        CT_Op_Transaction : Begin
          if TOpTransaction(op).Data.sender=nAccount then Result := Result + op.OperationAmount + op.OperationFee
          else if TOpTransaction(op).Data.target=nAccount then Result := Result - op.OperationAmount;
        End;
      End;
    end;
  End;
var ds : TADOQuery;
  whereorphan : AnsiString;
  sql,errors,aux : AnsiString;
  bf : TBlobField;
  ms : TMemoryStream;
  i : Integer;
  op : TPCOperation;
  vOrphan : Variant;
  trans : Integer;
begin
  Result := false;
  trans := FAdoConnection.BeginTrans;
  try
    TLog.NewLog(ltdebug,Classname,'DoSaveBlockChain:'+TPCOperationsComp.OperationBlockToText(Operations.OperationBlock));
    if Orphan='' then begin
      whereorphan := '('+CT_TblFld_BlockChain_orphan+' IS NULL)';
      vOrphan := Null;
    end else begin
      whereorphan := '('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''')';
      vOrphan := Orphan;
    end;
    SQL_DELETE(CT_TblName_BlockChain,whereorphan+' AND ('+CT_TblFld_BlockChain_block+'='+Inttostr(Operations.OperationBlock.block)+')');

    if Orphan='' then whereorphan := '('+CT_TblFld_Operations_orphan+' IS NULL)'
    else whereorphan := '('+CT_TblFld_Operations_orphan+' LIKE '''+Orphan+''')';
    SQL_DELETE(CT_TblName_Operations,whereorphan+' AND ('+CT_TblFld_Operations_block+'='+Inttostr(Operations.OperationBlock.block)+')');

    ds := TADOQuery.Create(Self);
    ms := TMemoryStream.Create;
    try
      ds.Connection := FAdoConnection;
      //
      SQL := 'SELECT * FROM '+CT_TblName_BlockChain+' WHERE (1=0)';
      ds.SQL.Text := SQL;
      ds.Open;
      ds.Insert;
      ds.FieldByName(CT_TblFld_BlockChain_block).Value := Operations.OperationBlock.block;
      aux := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(Operations.OperationBlock.account_key) );
      if (length(aux)>255) then aux := copy(aux,1,252)+'...';
      ds.FieldByName(CT_TblFld_BlockChain_accountkey).Value := Copy(aux,1,255);
      ds.FieldByName(CT_TblFld_BlockChain_s_reward).Value := Format('%.18d', [Operations.OperationBlock.reward] );
      ds.FieldByName(CT_TblFld_BlockChain_s_fee).Value := Format('%.18d', [Operations.OperationBlock.fee] );
      ds.FieldByName(CT_TblFld_BlockChain_protocol_version).Value := Operations.OperationBlock.protocol_version;
      ds.FieldByName(CT_TblFld_BlockChain_protocol_available).Value := Operations.OperationBlock.protocol_available;
      ds.FieldByName(CT_TblFld_BlockChain_s_timestamp).Value := Format('%.10d', [Operations.OperationBlock.timestamp] );
      ds.FieldByName(CT_TblFld_BlockChain_s_compact_target).Value := Format('%.10d', [Operations.OperationBlock.compact_target] );
      ds.FieldByName(CT_TblFld_BlockChain_s_nonce).Value := Format('%.10d', [Operations.OperationBlock.nonce] );
      DBRawToStringField(ds.FieldByName(CT_TblFld_BlockChain_rawpayload),Operations.OperationBlock.block_payload);
      ds.FieldByName(CT_TblFld_BlockChain_safe_box_hash).Value := TCrypto.ToHexaString( Operations.OperationBlock.initial_safe_box_hash );
      ds.FieldByName(CT_TblFld_BlockChain_operations_hash).Value := TCrypto.ToHexaString( Operations.OperationBlock.operations_hash );
      ds.FieldByName(CT_TblFld_BlockChain_proof_of_work).Value :=  TCrypto.ToHexaString( Operations.OperationBlock.proof_of_work );
      ds.FieldByName(CT_TblFld_BlockChain_orphan).Value := vOrphan;
      ds.FieldByName(CT_TblFld_BlockChain_operations_count).Value := Operations.Count;
      bf := ds.FieldByName(CT_TblFld_BlockChain_operations_stream) as TBlobField;
      Operations.SaveBlockToStream(False,ms);
      ms.Position := 0;
      BlobLoadFromStream(bf,ms);
      ds.Post;
      // Save operations:
      ds.Close;
      ds.SQL.Text := 'SELECT * FROM '+CT_TblName_Operations+' WHERE (1=0)';
      ds.Open;
      // First insert reward
      ds.Insert;
      ds.FieldByName(CT_TblFld_Operations_optype).Value := 0;
      ds.FieldByName(CT_TblFld_Operations_block).Value := Operations.OperationBlock.block;
      ds.FieldByName(CT_TblFld_Operations_s_timestamp).Value := Format('%.10d', [Operations.OperationBlock.timestamp] );
      ds.FieldByName(CT_TblFld_Operations_nopblock).Value := -1;
      ds.FieldByName(CT_TblFld_Operations_optype_op).Value := 0;
      ds.FieldByName(CT_TblFld_Operations_s_fee).Value := '0';
      ds.FieldByName(CT_TblFld_Operations_orphan).Value := vOrphan;
      ds.FieldByName(CT_TblFld_Operations_n_operation).Value := 0;
      ds.FieldByName(CT_TblFld_Operations_account).Value := Operations.OperationBlock.block * CT_AccountsPerBlock;
      ds.FieldByName(CT_TblFld_Operations_other_account).Value := Operations.OperationBlock.block * CT_AccountsPerBlock;
      ds.FieldByName(CT_TblFld_Operations_s_amount).Value := Format('%.18d', [Operations.OperationBlock.reward+Operations.OperationBlock.fee] );
      ds.FieldByName(CT_TblFld_Operations_s_balance).Value := Format('%.18d', [Operations.OperationBlock.reward+Operations.OperationBlock.fee] );
      ds.Post;
      // Insert operations
      for i := 0 to Operations.Count-1 do begin
        ds.Insert;
        op := Operations.Operation[i];
        ds.FieldByName(CT_TblFld_Operations_optype).Value := op.OpType;
        ds.FieldByName(CT_TblFld_Operations_block).Value := Operations.OperationBlock.block;
        ds.FieldByName(CT_TblFld_Operations_s_timestamp).Value := Format('%.10d', [Operations.OperationBlock.timestamp] );
        ds.FieldByName(CT_TblFld_Operations_nopblock).Value := i;
        ds.FieldByName(CT_TblFld_Operations_optype_op).Value := 0;
        ds.FieldByName(CT_TblFld_Operations_s_fee).Value := Format('%.18d', [(-1)*op.OperationFee] ); // Fee is a Negative number
        ds.FieldByName(CT_TblFld_Operations_orphan).Value := vOrphan;
        case op.OpType of
          CT_Op_Transaction : Begin
            ds.FieldByName(CT_TblFld_Operations_n_operation).Value := TOpTransaction(op).Data.n_operation;
            ds.FieldByName(CT_TblFld_Operations_account).Value :=  TOpTransaction(op).Data.sender;
            ds.FieldByName(CT_TblFld_Operations_other_account).Value :=  TOpTransaction(op).Data.target;
            ds.FieldByName(CT_TblFld_Operations_s_amount).Value := Format('%.18d', [(-1)*TOpTransaction(op).Data.amount] );
            DBRawToStringField(ds.FieldByName(CT_TblFld_Operations_rawpayload),TOpTransaction(op).Data.payload);
            ds.FieldByName(CT_TblFld_Operations_s_balance).Value := Format('%.18d', [GetOperationBalance(i,TOpTransaction(op).Data.sender)] );
            ds.Post;
            ds.Insert;
            ds.FieldByName(CT_TblFld_Operations_optype).Value := op.OpType;
            ds.FieldByName(CT_TblFld_Operations_block).Value := Operations.OperationBlock.block;
            ds.FieldByName(CT_TblFld_Operations_s_timestamp).Value := Format('%.10d', [Operations.OperationBlock.timestamp] );
            ds.FieldByName(CT_TblFld_Operations_nopblock).Value := i;
            ds.FieldByName(CT_TblFld_Operations_optype_op).Value := 1; // Receive transaction
            ds.FieldByName(CT_TblFld_Operations_s_fee).Value := '0';  // No fee for receiver
            ds.FieldByName(CT_TblFld_Operations_s_balance).Value := Format('%.18d', [GetOperationBalance(i,TOpTransaction(op).Data.target)] );
            ds.FieldByName(CT_TblFld_Operations_orphan).Value := vOrphan;
            ds.FieldByName(CT_TblFld_Operations_n_operation).Value := 0; // No n_operation for receiver
            ds.FieldByName(CT_TblFld_Operations_account).Value :=  TOpTransaction(op).Data.target;
            ds.FieldByName(CT_TblFld_Operations_other_account).Value :=  TOpTransaction(op).Data.sender;
            ds.FieldByName(CT_TblFld_Operations_s_amount).Value := Format('%.18d', [TOpTransaction(op).Data.amount] );
            DBRawToStringField(ds.FieldByName(CT_TblFld_Operations_rawpayload),TOpTransaction(op).Data.payload);
            ds.Post;
          End;
          CT_Op_Changekey : Begin
            ds.FieldByName(CT_TblFld_Operations_n_operation).Value := TOpChangeKey(op).Data.n_operation;
            ds.FieldByName(CT_TblFld_Operations_account).Value :=  TOpChangeKey(op).Data.account;
            ds.FieldByName(CT_TblFld_Operations_s_amount).Value := '0';
            DBRawToStringField(ds.FieldByName(CT_TblFld_Operations_rawpayload),TOpChangeKey(op).Data.payload);
            ds.FieldByName(CT_TblFld_Operations_s_balance).Value := Format('%.18d', [GetOperationBalance(i,TOpChangeKey(op).Data.account)] );
            aux := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(TOpChangeKey(op).Data.new_accountkey) );
            if (length(aux)>255) then aux := copy(aux,1,252)+'...';
            ds.FieldByName(CT_TblFld_Operations_newaccountkey).Value := Copy(aux,1,255);
            ds.Post;
          End;
          CT_Op_Recover : Begin
            ds.FieldByName(CT_TblFld_Operations_n_operation).Value := TOpRecoverFounds(op).Data.n_operation;
            ds.FieldByName(CT_TblFld_Operations_account).Value :=  TOpRecoverFounds(op).Data.account;
            ds.FieldByName(CT_TblFld_Operations_s_amount).Value := '0';
            ds.FieldByName(CT_TblFld_Operations_s_balance).Value := Format('%.18d', [GetOperationBalance(i,TOpRecoverFounds(op).Data.account)] );
            ds.Post;
          End;
        else raise Exception.Create('Development error: OpType not available to save to the Database '+Inttostr(Op.OpType));
        end;
      end;
    finally
      ms.Free;
      ds.Free;
    end;
    SaveBank;
    FAdoConnection.CommitTrans;
    Result := true;
  Except
    FAdoConnection.RollbackTrans;
    raise;
  end;
end;

class function TDBStorage.GetBlockChainSQL(block_start, block_end: Int64;
  dateStart, dateEnd: TDate; start, max: Integer): String;
var sqltop : String;
  whereorphan, where : String;
begin
  if (start=0) AND (max>0) then sqltop := ' TOP '+inttostr(max)+' '
  else sqltop := '';
  where := 'WHERE ('+CT_TblFld_Operations_orphan+' IS NULL)';
  if (block_start>=0) then begin
    where := where + ' AND ('+CT_TblFld_BlockChain_block+'>='+IntToStr(block_start)+')';
  end;
  if (block_end>=0) then begin
    where := where + ' AND ('+CT_TblFld_BlockChain_block+'<='+IntToStr(block_end)+')';
  end;
  if (dateStart>1000) then begin
    where := where + ' AND ('+CT_TblFld_BlockChain_s_timestamp+'>='+TDBStorage.ValueToSql(Format('%.10d', [UnivDateTimeToUnix(DateTime2UnivDateTime(dateStart))]))+')';
  end;
  if (dateEnd>1000) then begin
    where := where + ' AND ('+CT_TblFld_BlockChain_s_timestamp+'<'+ValueToSql(Format('%.10d', [UnivDateTimeToUnix(DateTime2UnivDateTime(dateEnd+1))]))+')';
  end;

  Result := 'SELECT '+sqltop+' * '+
    ' FROM '+CT_TblName_BlockChain+
    ' '+where+
    ' ORDER BY '+CT_TblFld_BlockChain_block+' DESC';
end;

function TDBStorage.GetOperationsFromAccount(Const OperationsResume : TOperationsResumeList; account_number : Cardinal; start,max : Integer) :Integer;
var sqltop,whereorphan,sql,spayload : AnsiString;
  ds : TADOQuery;
  OPR : TOperationResume;
  nrow : Integer;
  ms : TMemoryStream;
begin
  if (start=0) AND (max>0) then sqltop := ' TOP '+inttostr(max)+' '
  else sqltop := '';
  if Orphan='' then whereorphan := '(('+CT_TblFld_BlockChain_orphan+' IS NULL) AND ('+CT_TblFld_Operations_orphan+' IS NULL))'
  else whereorphan := '(('+CT_TblFld_BlockChain_orphan+' LIKE '''+Orphan+''') AND ('+CT_TblFld_Operations_orphan+' LIKE '''+Orphan+'''))';
  sql := 'SELECT '+sqltop+' * '+
    ' FROM '+CT_TblName_Operations+
    ' WHERE ('+CT_TblFld_Operations_account+'='+inttostr(account_number)+') AND '+whereorphan+
    ' ORDER BY '+CT_TblFld_Operations_block+' DESC, '+CT_TblFld_Operations_nopblock+' DESC, '+CT_TblFld_Operations_optype_op+' DESC';
  ms := TMemoryStream.Create;
    ds := TADOQuery.Create(Self);
    try
      ds.Connection := FAdoConnection;
      ds.SQL.Text := sql;
      ds.Open;
      nrow := 0;
      while not ds.Eof do begin
        if (nrow>=start) then begin
          OPR := CT_TOperationResume_NUL;
          OPR.Block := ds.FieldByName(CT_TblFld_Operations_block).AsInteger;
          OPR.AffectedAccount := account_number;
          OPR.time := StrToIntDef( ds.FieldByName(CT_TblFld_BlockChain_s_timestamp).AsString,0);
          case ds.FieldByName(CT_TblFld_Operations_optype).AsInteger of
            0 : OPR.OperationTxt := 'Blockchain reward';
            CT_Op_Transaction : begin
              if ds.FieldByName(CT_TblFld_Operations_optype_op).AsInteger=0 then begin
                OPR.OperationTxt := 'Transaction Sent to '+TAccountComp.AccountNumberToAccountTxtNumber(ds.FieldByName(CT_TblFld_Operations_other_account).AsLargeInt);
              end else begin
                OPR.OperationTxt := 'Transaction Received from '+TAccountComp.AccountNumberToAccountTxtNumber(ds.FieldByName(CT_TblFld_Operations_other_account).AsLargeInt);
              end;
            end;
            CT_Op_Changekey : Begin
              OPR.OperationTxt := 'Change Key';
            End;
            CT_Op_Recover : begin
              OPR.OperationTxt := 'Recover founds';
            end;
          else
            OPR.OperationTxt := 'Unknown OpType:'+Inttostr(ds.FieldByName(CT_TblFld_Operations_optype).AsInteger);
          end;
          OPR.Amount := StrToInt64Def( ds.FieldByName(CT_TblFld_Operations_s_amount).AsString ,0 );
          OPR.Fee := StrToInt64Def( ds.FieldByName(CT_TblFld_Operations_s_fee).AsString ,0);
          OPR.Balance := StrToInt64Def( ds.FieldByName(CT_TblFld_Operations_s_balance).AsString ,0);
          DBStringFieldToRaw(ds.FieldByName(CT_TblFld_Operations_rawpayload),OPR.OriginalPayload);
          If DBPayloadToReadableText(OPR.OriginalPayload,spayload) then OPR.PrintablePayload := spayload
          else OPR.PrintablePayload := TCrypto.ToHexaString(OPR.OriginalPayload);
          OperationsResume.Add(OPR);
          //
          if (max>0) And (OperationsResume.Count>=max) then exit;
        end;
        ds.Next;
        inc(nrow);
      end;
    finally
      ds.Free;
      ms.Free;
    end;
end;

class function TDBStorage.GetOperationsFromAccountSQL(account_number, block_start, block_end : Int64; dateStart,dateEnd : TDate; start,max : Integer) :String;
var sqltop : String;
  whereorphan, where : String;
begin
  if (start=0) AND (max>0) then sqltop := ' TOP '+inttostr(max)+' '
  else sqltop := '';
  where := 'WHERE ('+CT_TblFld_Operations_orphan+' IS NULL)';
  if account_number>=0 then begin
    where := where + ' AND ('+CT_TblFld_Operations_account+'='+inttostr(account_number)+')';
  end;
  if (block_start>=0) then begin
    where := where + ' AND ('+CT_TblFld_Operations_block+'>='+IntToStr(block_start)+')';
  end;
  if (block_end>=0) then begin
    where := where + ' AND ('+CT_TblFld_Operations_block+'<='+IntToStr(block_end)+')';
  end;
  if (dateStart>1000) then begin
    where := where + ' AND ('+CT_TblFld_Operations_s_timestamp+'>='+ValueToSql( Format('%.10d', [UnivDateTimeToUnix(DateTime2UnivDateTime(dateStart))]))+')';
  end;
  if (dateEnd>1000) then begin
    where := where + ' AND ('+CT_TblFld_Operations_s_timestamp+'<'+ValueToSql( Format('%.10d', [UnivDateTimeToUnix(DateTime2UnivDateTime(dateEnd+1))]))+')';
  end;

  Result := 'SELECT '+sqltop+' * '+
    ' FROM '+CT_TblName_Operations+
    ' '+where+
    ' ORDER BY '+CT_TblFld_Operations_block+' DESC, '+CT_TblFld_Operations_nopblock+' DESC, '+CT_TblFld_Operations_optype_op+' DESC';
end;

class function TDBStorage.OperationToOperationResume(Operation : TPCOperation; Affected_account_number : Cardinal; var OperationResume : TOperationResume) : Boolean;
Var spayload : AnsiString;
begin
  OperationResume := CT_TOperationResume_NUL;
  OperationResume.Fee := (-1)*Operation.OperationFee;
  OperationResume.AffectedAccount := Affected_account_number;
  Result := false;
  case Operation.OpType of
    CT_Op_Transaction : Begin
      if TOpTransaction(Operation).Data.sender=Affected_account_number then begin
        OperationResume.OperationTxt := 'Transaction Sent to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target);
        OperationResume.Amount := TOpTransaction(Operation).Data.amount * (-1);
        Result := true;
      end else if TOpTransaction(Operation).Data.target=Affected_account_number then begin
        OperationResume.OperationTxt := 'Transaction Received from '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.sender);
        OperationResume.Amount := TOpTransaction(Operation).Data.amount;
        OperationResume.Fee := 0;
        Result := true;
      end else exit;
    End;
    CT_Op_Changekey : Begin
      OperationResume.OperationTxt := 'Change Key';
      OperationResume.Fee := TOpChangeKey(Operation).Data.fee;
      Result := true;
    End;
    CT_Op_Recover : Begin
      OperationResume.OperationTxt := 'Recover founds';
      OperationResume.Fee := TOpRecoverFounds(Operation).Data.fee;
    End;
  else Exit;
  end;
  OperationResume.OriginalPayload := Operation.OperationPayload;
  If DBPayloadToReadableText(OperationResume.OriginalPayload,spayload) then OperationResume.PrintablePayload := spayload
  else OperationResume.PrintablePayload := TCrypto.ToHexaString(OperationResume.OriginalPayload);
end;

procedure TDBStorage.SetAccessFileName(const Value: AnsiString);
begin
  if FAccessFileName=Value then exit;
  FAccessFileName := Value;
  FAdoConnection.LoginPrompt := false;
  FAdoConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+Value+';Persist Security Info=False';
  FAdoConnection.Connected := true;
end;

function TDBStorage.SQL_DELETE(const TblName: AnsiString; const Where: String): Boolean;
Var sql : AnsiString;
  ds : TADOQuery;
begin
  sql := 'DELETE * FROM '+TblName+' WHERE '+Where;
  Try
    ds := TADOQuery.Create(Self);
    try
      ds.Connection := FAdoConnection;
      ds.SQL.Text := sql;
      ds.ExecSQL;
    finally
      ds.Free;
    end;
  Except
    On E:Exception do begin
      E.Message := 'Error deleting'+#10+E.Message+#10+'SQL: '+sql;
      raise;
    end;
  End;
end;

function TDBStorage.SQL_UPDATE(const TblName : AnsiString; Const SetValue : AnsiString; const Where : String): Boolean;
Var sql : AnsiString;
  ds : TADOQuery;
begin
  sql := 'UPDATE '+TblName+' SET '+SetValue+' WHERE '+Where;
  try
    ds := TADOQuery.Create(Self);
    try
      ds.Connection := FAdoConnection;
      ds.SQL.Text := sql;
      ds.ExecSQL;
    finally
      ds.Free;
    end;
  Except
    On E:Exception do begin
      E.Message := 'Error updating'+#10+E.Message+#10+'SQL: '+sql;
      raise;
    end;
  End;
end;

class function TDBStorage.ValueToSql(const Value: Variant): String;
Var decs,ths : Char;
  dates,times : char;
begin
   decs := DecimalSeparator;
   ths := ThousandSeparator;
   times := TimeSeparator;
   dates := DateSeparator;
   try
     DecimalSeparator := '.';
     ThousandSeparator := ',';
     TimeSeparator := ':';
     DateSeparator := '-';

     Case VarType(Value) of
         varNull, varEmpty : Begin
            Result := 'NULL';
                   End;
         varSmallint,
         varInteger, varByte, varWord,
         varLongWord,varInt64,varUInt64 : Result := IntToStr(Value);
         varSingle,varDouble : begin
           Result := FormatFloat('0.0#################',Value);
         end;
         varCurrency : Result := FormatCurr('0.0#######',Value);
         varDate : Begin
           Result := FormatFloat('0.0#################',Value);
                   End;
         varBoolean : Begin
           If Value then Result := 'TRUE'
           Else Result := 'FALSE'
                      End;
         varString,varOleStr,varUString  : Begin
             Result := ''''+VarToStr(Value)+'''';
           End;
      Else // Case
        Raise Exception.Create('Invalid variant Type: '+InttoHex(VarType(Value),8));
      End;
   finally
     DecimalSeparator := decs;
     ThousandSeparator := ths;
     TimeSeparator := times;
     DateSeparator := dates;
   end;
end;

{ TOperationsResumeList }

Type POperationResume = ^TOperationResume;

procedure TOperationsResumeList.Add(const OperationResume: TOperationResume);
Var P : POperationResume;
begin
  New(P);
  P^ := OperationResume;
  FList.Add(P);
end;

procedure TOperationsResumeList.Clear;
Var P : POperationResume;
  i : Integer;
  l : TList;
begin
  l := FList.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      P := l[i];
      Dispose(P);
    end;
    l.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TOperationsResumeList.Count: Integer;
Var l : TList;
begin
  l := FList.LockList;
  Try
    Result := l.Count;
  Finally
    FList.UnlockList;
  End;
end;

constructor TOperationsResumeList.Create;
begin
  FList := TPCThreadList.Create;
end;

procedure TOperationsResumeList.Delete(index: Integer);
Var P : POperationResume;
  l : TList;
begin
  l := FList.LockList;
  Try
    P := l[index];
    l.Delete(index);
    Dispose(P);
  Finally
    FList.UnlockList;
  End;
end;

destructor TOperationsResumeList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TOperationsResumeList.GetOperationResume(index: Integer): TOperationResume;
Var l : TList;
begin
  l := FList.LockList;
  try
    if index<l.Count then Result := POperationResume(l[index])^
    else Result := CT_TOperationResume_NUL;
  finally
    FList.UnlockList;
  end;
end;

end.
