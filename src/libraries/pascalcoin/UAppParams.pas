unit UAppParams;

{ Copyright (c) 2016 by Albert Molina

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

uses
  Classes, SysUtils;

Type
  TAppParamType = (ptString, ptInteger, ptLongWord, ptInt64, ptBoolean, ptBytes);

  TAppParams = Class;

  { TAppParam }

  TAppParam = Class
    FAppParams : TAppParams;
    Function LoadFromStream(Stream : TStream) : Boolean;
    Procedure SaveToStream(Stream : TStream);
  private
    FParamName: String;
    FValue: Variant;
    FBytesValue : TBytes;
    FParamType: TAppParamType;
    procedure SetParamName(const Value: String);
    procedure SetValue(const Value: Variant);
    procedure SetParamType(const Value: TAppParamType);
    function GetIsNull: Boolean;
  protected
  published
  public
    Constructor Create(AParamName : String);
    Destructor Destroy; override;
    Property ParamName : String read FParamName write SetParamName;
    Property Value : Variant read FValue write SetValue;
    Property ParamType : TAppParamType read FParamType write SetParamType;
    Procedure SetAsInteger(IntValue : Integer);
    Procedure SetAsCardinal(CardValue : Cardinal);
    Procedure SetAsString(StringValue : String);
    Procedure SetAsInt64(Int64Value : Int64);
    Procedure SetAsBoolean(BoolValue : Boolean);
    Procedure SetAsStream(Stream : TStream);
    Procedure SetAsTBytes(Bytes : TBytes);
    Property IsNull : Boolean read GetIsNull;
    function GetAsString(Const DefValue : String): String;
    function GetAsBoolean(Const DefValue : Boolean): Boolean;
    function GetAsInteger(Const DefValue : Integer): Integer;
    function GetAsInt64(Const DefValue : Int64): Int64;
    function GetAsStream(Stream : TStream) : Integer;
    function GetAsTBytes(Const DefValue : TBytes) : TBytes;
  End;

  TAppParams = Class(TComponent)
  private
    FParamsStream : TFileStream;
    FParams : TList;
    FFileName: String;
    Function LoadFromStream(Stream : TStream) : Boolean;
    Procedure SaveToStream(Stream : TStream);
    function GetParam(ParamName: String): TAppParam;
    Procedure InternalClear;
    Function IndexOfParam(Const ParamName : String) : Integer;
    procedure SetFileName(const Value: String);
    Procedure Save;
  protected
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class function AppParams : TAppParams;
    Property FileName : String read FFileName write SetFileName;
    Property ParamByName[ParamName : String] : TAppParam read GetParam;
    Procedure Clear;
    Procedure Delete(Const ParamName : String);
    Function Count : Integer;
    Function Param(index : Integer) : TAppParam;
    Function FindParam(Const ParamName : String) : TAppParam;
  End;

implementation

uses
  Variants;

Const
  CT_AppParams_File_Magic = 'TAppParams';

Var _appParams : TAppParams;

function Internal_ReadBytes(Stream: TStream; var value: TBytes): Integer;
Var
  w: Word;
begin
  if Stream.Size - Stream.Position < 2 then begin
    SetLength(value,0);
    Result := -1;
    Exit;
  end;
  Stream.Read(w, 2);
  if Stream.Size - Stream.Position < w then begin
    Stream.Position := Stream.Position - 2; // Go back!
    SetLength(value,0);
    Result := -1;
    Exit;
  end;
  SetLength(value, w);
  if (w>0) then begin
    Stream.ReadBuffer(value[Low(value)], w);
  end;
  Result := w+2;
end;


function Internal_WriteBytes(Stream: TStream; const value: TBytes): Integer; overload;
Var
  w: Word;
begin
  if (Length(value)>(256*256)) then begin
    raise Exception.Create('Invalid stream size! '+Inttostr(Length(value)));
  end;

  w := Length(value);
  Stream.Write(w, 2);
  if (w > 0) then
    Stream.WriteBuffer(value[Low(value)], Length(value));
  Result := w+2;
end;


{ TAppParam }

constructor TAppParam.Create(AParamName: String);
begin
  FAppParams := Nil;
  FParamName := AParamName;
  FValue := Null;
  FBytesValue := Nil;
end;

destructor TAppParam.Destroy;
begin
  FValue := Null;
  FBytesValue := Nil;
  inherited Destroy;
end;

function TAppParam.GetAsBoolean(const DefValue: Boolean): Boolean;
begin
  if IsNull then Result := DefValue
  else begin
    Try
      Result := FValue;
    Except
      Result := DefValue;
    End;
  end;
end;

function TAppParam.GetAsInt64(const DefValue: Int64): Int64;
begin
  if IsNull then Result := DefValue
  else begin
    Try
      Result := FValue;
    Except
      Result := DefValue;
    End;
  end;
end;

function TAppParam.GetAsInteger(const DefValue: Integer): Integer;
begin
  if IsNull then Result := DefValue
  else begin
    Try
      Result := FValue;
    Except
      Result := DefValue;
    End;
  end;
end;

function TAppParam.GetAsStream(Stream: TStream): Integer;
var bytes : TBytes;
begin
  Stream.Size := 0;
  if IsNull then Result := 0
  else begin
    bytes := GetAsTBytes(Nil);
    if Length(bytes)>0 then begin
      Stream.WriteBuffer(bytes[0],Length(bytes));
      Stream.Position := 0;
    end;
    Result := Length(bytes);
  end;
end;

function TAppParam.GetAsString(const DefValue: String): String;
begin
  if IsNull then Result := DefValue
  else Result := VarToStrDef(FValue,DefValue);
end;

function TAppParam.GetAsTBytes(const DefValue: TBytes): TBytes;
begin
  if IsNull then SetLength(Result,0)
  else begin
    Try
    if (ParamType=(ptString)) then begin
      Result := TEncoding.ANSI.GetBytes(VarToStr(FValue));
    end else Result := Copy(FBytesValue);
    Except
      Result := DefValue;
    End;
  end;
end;

function TAppParam.GetIsNull: Boolean;
begin
  Result := VarIsNull( FValue );
end;

function TAppParam.LoadFromStream(Stream: TStream): Boolean;
Var bpt : Byte;
  pt : TAppParamType;
  i : Integer;
  c : Cardinal;
  i64 : Int64;
  bytes : TBytes;
begin
  Result := false;
  // 2 bytes for length
  if Internal_ReadBytes(Stream,bytes)<0 then exit;
  FParamName := TEncoding.ANSI.GetString(bytes);
  Stream.Read(bpt,1);
  if (bpt>=Integer(low(pt))) And (bpt<=Integer(high(pt))) then pt := TAppParamType(bpt)
  else pt := ptString;
  FParamType := pt;
  Stream.Read(bpt,1);
  if bpt=0 then FValue := Null
  else begin
    case pt of
      ptString : begin
        If Internal_ReadBytes(Stream,bytes)<0 then exit;
        FValue := TEncoding.ANSI.GetString(bytes);
      end;
      ptInteger : begin
        if Stream.Read(i,sizeof(i))<sizeof(i) then exit;
        FValue := i;
      end;
      ptLongWord : Begin
        if Stream.Read(c,sizeof(c))<sizeof(c) then exit;
        FValue := c;
      End;
      ptInt64 : Begin
        if Stream.Read(i64,sizeof(i64))<sizeof(i64) then exit;
        FValue := i64;
      End;
      ptBoolean : Begin
        if Stream.Read(bpt,sizeof(bpt))<sizeof(bpt) then exit;
        if bpt=0 then FValue := false
        else FValue := true;
      End;
      ptBytes : Begin
        if Internal_ReadBytes(Stream,bytes)<0 then exit;
        FValue := 0; // Init as a Zero to set to "not null"
        FBytesValue := Copy(bytes);
      End
    else
      raise Exception.Create('Development error 20160613-1');
    end;
  end;
  Result := true;
end;

procedure TAppParam.SaveToStream(Stream: TStream);
var b : Byte;
  i : Integer;
  c : Cardinal;
  i64 : Int64;
begin
  Internal_WriteBytes(Stream,TEncoding.ANSI.GetBytes(FParamName));
  b := Byte(FParamType);
  Stream.Write(b,1);
  if IsNull then begin
    b := 0;
    Stream.Write(b,1);
  end else begin
    b := 1;
    Stream.Write(b,1);
    case FParamType of
      ptString : begin
        Internal_WriteBytes(Stream,TEncoding.ANSI.GetBytes(VarToStr(FValue)));
      end;
      ptInteger : begin
        i := FValue;
        Stream.Write(i,sizeof(i));
      end;
      ptLongWord : begin
        c := FValue;
        Stream.Write(c,sizeof(c));
      end;
      ptInt64 : begin
        i64 := FValue;
        Stream.Write(i64,sizeof(i64));
      end;
      ptBoolean : Begin
        if FValue then b := 1
        else b := 0;
        Stream.Write(b,sizeof(b));
      End;
      ptBytes : Begin
        Internal_WriteBytes(Stream,FBytesValue);
      End
    else
      raise Exception.Create('Development error 20160613-2');
    end;
  end;
end;

procedure TAppParam.SetAsBoolean(BoolValue: Boolean);
begin
  FParamType := ptBoolean;
  FValue := BoolValue;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetAsCardinal(CardValue: Cardinal);
begin
  FParamType := ptLongWord;
  FValue := CardValue;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetAsInt64(Int64Value: Int64);
begin
  FParamType := ptInt64;
  FValue := Int64Value;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetAsInteger(IntValue: Integer);
begin
  FParamType := ptInteger;
  FValue := IntValue;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetAsStream(Stream: TStream);
var bytes : TBytes;
begin
  Stream.Position := 0;
  setlength(bytes,Stream.Size);
  if (Stream.Size>0) then begin
    Stream.ReadBuffer(bytes[0],Stream.Size);
  end;
  SetAsTBytes(bytes);
end;

procedure TAppParam.SetAsString(StringValue: String);
begin
  if (FParamType=ptString) And (GetAsString('')=StringValue) then exit;

  FParamType := ptString;
  FValue := StringValue;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetAsTBytes(Bytes: TBytes);
begin
  FParamType := ptBytes;
  FBytesValue := Copy(bytes);
  FValue := 0; // Init as a Zero to set to "not null"
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetParamName(const Value: String);
begin
  FParamName := Value;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetParamType(const Value: TAppParamType);
begin
  FParamType := Value;
  If Assigned(FAppParams) then FAppParams.Save;
end;

procedure TAppParam.SetValue(const Value: Variant);
begin
  if FParamType=ptBytes then begin
    FValue := Null;
    FBytesValue := Nil;
  end else FValue := Value;
  If Assigned(FAppParams) then FAppParams.Save;
end;

{ TAppParams }

class function TAppParams.AppParams: TAppParams;
begin
  if Not Assigned(_appParams) then begin
    _appParams := TAppParams.Create(Nil);
  end;
  Result := _appParams;
end;

procedure TAppParams.Clear;
begin
  InternalClear;
  Save;
end;

function TAppParams.Count: Integer;
begin
  Result := FParams.Count;
end;

constructor TAppParams.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TList.Create;
  FFileName := '';
  FParamsStream := Nil;
  if _appParams=Nil then _appParams := Self;

end;

procedure TAppParams.Delete(const ParamName: String);
Var P : TAppParam;
  i : Integer;
begin
  i := IndexOfParam(ParamName);
  if i<0 then exit;
  P := FParams[i];
  FParams.Delete(i);
  P.Free;
  Save;
end;

destructor TAppParams.Destroy;
begin
  FreeAndNil(FParamsStream);
  InternalClear;
  FParams.Free;
  inherited;
  if _appParams=Self then _appParams := Nil;

end;

function TAppParams.FindParam(const ParamName: String): TAppParam;
Var i : Integer;
begin
  i := IndexOfParam(ParamName);
  if i>=0 then Result := FParams[i]
  else Result := Nil;
end;

function TAppParams.GetParam(ParamName: String): TAppParam;
Var i : Integer;
  P : TAppParam;
begin
  i := IndexOfParam(ParamName);
  if i<0 then begin
    P := TAppParam.Create(ParamName);
    P.FAppParams := Self;
    FParams.Add(P);
  end else P := FParams[i];
  Result := P;
end;

function TAppParams.IndexOfParam(const ParamName: String): Integer;
begin
  for Result := 0 to FParams.Count - 1 do begin
    if AnsiSameText(ParamName,TAppParam(FParams[Result]).ParamName) then exit;
  end;
  Result := -1;
end;

procedure TAppParams.InternalClear;
Var P : TAppParam;
  i : Integer;
begin
  for i := 0 to FParams.Count - 1 do begin
    P := FParams[i];
    P.Free;
  end;
  FParams.Clear;
end;

function TAppParams.LoadFromStream(Stream: TStream): Boolean;
Var bytes : TBytes;
  i : Integer;
  c: Int32;
  P : TAppParam;
begin
  Result := false;
  InternalClear;
  If Internal_ReadBytes(Stream,bytes)<0 then exit;
  If TEncoding.ANSI.GetString(bytes)<>CT_AppParams_File_Magic then raise Exception.Create('Invalid file type');
  Stream.Read(c,sizeof(c));
  for i := 0 to c-1 do begin
    P := TAppParam(TAppParam.NewInstance);
    P.FAppParams := Self;
    FParams.Add(P);
    If Not P.LoadFromStream(Stream) then exit;
  end;
  Result := true;
end;

function TAppParams.Param(index: Integer): TAppParam;
begin
  Result := TAppParam(FParams[index]);
end;

procedure TAppParams.Save;
begin
  if Assigned(FParamsStream) then begin
    FParamsStream.Position := 0;
    FParamsStream.Size := 0;
    SaveToStream(FParamsStream);
  end;
end;

procedure TAppParams.SaveToStream(Stream: TStream);
Var i : Int32;
begin
  Internal_WriteBytes(Stream,TEncoding.ANSI.GetBytes(CT_AppParams_File_Magic));
  i := FParams.Count;
  Stream.Write(i,sizeof(i));
  for i := 0 to FParams.Count - 1 do begin
    Try
      TAppParam(FParams[i]).SaveToStream(Stream);
    Except
      On E:Exception do begin
        E.Message := 'Error saving param '+TAppParam(FParams[i]).ParamName+': '+E.Message;
        Raise;
      end;
    end;
  end;
end;

procedure TAppParams.SetFileName(const Value: String);
Var fm : Word;
begin
  if FFileName=Value then exit;
  if Assigned(FParamsStream) then FParamsStream.Free;
  FParamsStream := Nil;
  FFileName := Value;
  if Value<>'' then begin
    if FileExists(Value) then fm := fmOpenReadWrite
    else fm := fmCreate;

    FParamsStream := TFileStream.Create(Value,fm+fmShareExclusive);
    Try
      LoadFromStream(FParamsStream);
    Except
      // Do nothing
    End;
  end;
end;

initialization
  _appParams := Nil;
end.
