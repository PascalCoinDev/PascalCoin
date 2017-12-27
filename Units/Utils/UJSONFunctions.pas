unit UJSONFunctions;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Uses
  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  DBXJSON,
  JSON,
  {$ENDIF}
  SysUtils, DateUtils, Variants, Classes, ULog;

Type
  {$IFDEF FPC}
  TJSONValue = TJSONData;
  {$ENDIF}

  TPCJSONData = Class
  private
    FParent : TPCJSONData;
  protected
    Function ToJSONFormatted(pretty:Boolean;Const prefix : AnsiString) : AnsiString; virtual; abstract;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Class Function ParseJSONValue(Const JSONObject : String) : TPCJSONData; overload;
    Class Function ParseJSONValue(Const JSONObject : TBytes) : TPCJSONData; overload;
    Class Function _GetCount : Integer;
    Function ToJSON(pretty : Boolean) : AnsiString;
    Procedure SaveToStream(Stream : TStream);
    Procedure Assign(PCJSONData : TPCJSONData);
  End;

  TPCJSONDataClass = Class of TPCJSONData;

  { TPCJSONVariantValue }

  TPCJSONVariantValue = Class(TPCJSONData)
  private
    FOldValue : Variant;
    FWritable : Boolean;
    FValue: Variant;
    procedure SetValue(const Value: Variant);
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : AnsiString) : AnsiString; override;
  public
    Constructor Create; override;
    Constructor CreateFromJSONValue(JSONValue : TJSONValue);
    Property Value : Variant read FValue write SetValue;
    Function AsString(DefValue : String) : String;
    Function AsInteger(DefValue : Integer) : Integer;
    Function AsInt64(DefValue : Int64) : Int64;
    Function AsDouble(DefValue : Double) : Double;
    Function AsBoolean(DefValue : Boolean) : Boolean;
    Function AsDateTime(DefValue : TDateTime) : TDateTime;
    Function AsCurrency(DefValue : Currency) : Currency;
    Function AsCardinal(DefValue : Cardinal) : Cardinal;
    Function IsNull : Boolean;
  End;

  TPCJSONNameValue = Class(TPCJSONData)
  private
    FName: String;
    FValue: TPCJSONData;
    FFreeValue : Boolean;
    procedure SetValue(const Value: TPCJSONData);
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : AnsiString) : AnsiString; override;
  public
    Constructor Create(AName : String);
    Destructor Destroy; override;
    Property Name : String read FName;
    Property Value : TPCJSONData read FValue write SetValue;
  End;

  TPCJSONArray = class;
  TPCJSONObject = Class;

  TPCJSONList = Class(TPCJSONData)
  private
    FList : TList;
    function GetItems(Index: Integer): TPCJSONData;
    procedure SetItems(Index: Integer; const Value: TPCJSONData);
  protected
    Function GetIndexAsVariant(Index : Integer) : TPCJSONVariantValue;
    Function GetIndexAsArray(Index : Integer) : TPCJSONArray;
    Function GetIndexAsObject(Index : Integer) : TPCJSONObject;
    Procedure CheckCanInsert(Index:Integer; PCJSONData:TPCJSONData); virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Property Items[Index:Integer] : TPCJSONData read GetItems write SetItems;
    Procedure Insert(Index:Integer; PCJSONData:TPCJSONData);
    Procedure Delete(index : Integer);
    function Count : Integer;
    Procedure Clear;
  End;

  TPCJSONArray = class(TPCJSONList)
  private
    Procedure GrowToIndex(index : Integer);
    function GetItemOfType(Index: Integer; DataClass:TPCJSONDataClass): TPCJSONData;
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : AnsiString) : AnsiString; override;
  public
    Constructor Create; override;
    Constructor CreateFromJSONArray(JSONArray : TJSONArray);
    Destructor Destroy; override;
    Function GetAsVariant(index : Integer) : TPCJSONVariantValue;
    Function GetAsObject(index : Integer) : TPCJSONObject;
    Function GetAsArray(index : Integer) : TPCJSONArray;
  end;

  { TPCJSONObject }

  TPCJSONObject = Class(TPCJSONList)
  private
    Function GetIndexOrCreateName(Name : String) : Integer;
    Function GetByName(Name : String) : TPCJSONNameValue;
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : AnsiString) : AnsiString; override;
    Procedure CheckCanInsert(Index:Integer; PCJSONData:TPCJSONData); override;
    Procedure CheckValidName(Name : String);
  public
    Constructor Create; override;
    Constructor CreateFromJSONObject(JSONObject : TJSONObject);
    Destructor Destroy; override;
    Function FindName(Name : String) : TPCJSONNameValue;
    Function IndexOfName(Name : String) : Integer;
    Procedure DeleteName(Name : String);
    Function GetAsVariant(Name : String) : TPCJSONVariantValue;
    Function GetAsObject(Name : String) : TPCJSONObject;
    Function GetAsArray(Name : String) : TPCJSONArray;
    Function AsString(ParamName : String; DefValue : String) : String;
    Function AsInteger(ParamName : String; DefValue : Integer) : Integer;
    Function AsCardinal(ParamName : String; DefValue : Cardinal) : Cardinal;
    Function AsInt64(ParamName : String; DefValue : Int64) : Int64;
    Function AsDouble(ParamName : String; DefValue : Double) : Double;
    Function AsBoolean(ParamName : String; DefValue : Boolean) : Boolean;
    Function AsDateTime(ParamName : String; DefValue : TDateTime) : TDateTime;
    Function AsCurrency(ParamName : String; DefValue : Currency) : Currency;
    Function SaveAsStream(ParamName : String; Stream : TStream) : Integer;
    Function LoadAsStream(ParamName : String; Stream : TStream) : Integer;
    Function GetNameValue(index : Integer) : TPCJSONNameValue;
    Function IsNull(ParamName : String) : Boolean;
    Procedure SetAs(Name : String; Value : TPCJSONData);
  End;

  EPCParametresError = Class(Exception);

implementation

Function UTF8JSONEncode(plainTxt : String; includeSeparator : Boolean) : String;
Var ws : WideString;
  i : Integer;
Begin
   ws := UTF8Encode(plainTxt);
   {ALERT:
    UTF8Encode function deletes last char if equal to #0, so we put it manually
    }
   if copy(plainTxt,length(plainTxt),1)=#0 then ws := ws + #0;
        i := 1;
        result := '"';
        while i <= length(ws) do
          begin
            case ws[i] of
              '/', '\', '"': result := result + '\' + ws[i];
              #8: result := result + '\b';
              #9: result := result + '\t';
              #10: result := result + '\n';
              #13: result := result + '\r';
              #12: result := result + '\f';
            else
              if (ord(ws[i]) < 32) Or (ord(ws[i])>122) then
                result := result + '\u' + inttohex(ord(ws[i]), 4)
              else
                result := result + ws[i];
            end;
            inc(i);
          end;
        result := result + '"';
End;

{ TPCJSONArray }

constructor TPCJSONArray.Create;
begin
  inherited;

end;

constructor TPCJSONArray.CreateFromJSONArray(JSONArray: TJSONArray);
Var i : Integer;
begin
  Create;
{$IFDEF FPC}
  for i := 0 to JSONArray.Count - 1 do begin
    if (JSONArray.Items[i] is TJSONArray) then begin
      Insert(i,TPCJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Items[i])));
    end else if (JSONArray.Items[i] is TJSONObject) then begin
      Insert(i,TPCJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Items[i])));
    end else if (JSONArray.Items[i] is TJSONValue) then begin
      Insert(i,TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Items[i])));
    end else raise EPCParametresError.Create('Invalid TJSON Data: '+JSONArray.Items[i].ClassName);
  end;
{$ELSE}
  for i := 0 to JSONArray.Size - 1 do begin
    if (JSONArray.Get(i) is TJSONArray) then begin
      Insert(i,TPCJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Get(i))));
    end else if (JSONArray.Get(i) is TJSONObject) then begin
      Insert(i,TPCJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Get(i))));
    end else if (JSONArray.Get(i) is TJSONValue) then begin
      Insert(i,TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Get(i))));
    end else raise EPCParametresError.Create('Invalid TJSON Data: '+JSONArray.Get(i).ClassName);
  end;
{$ENDIF}
end;


destructor TPCJSONArray.Destroy;
begin
  inherited;
end;

function TPCJSONArray.GetAsArray(index: Integer): TPCJSONArray;
begin
  Result := GetItemOfType(index,TPCJSONArray) as TPCJSONArray;
end;

function TPCJSONArray.GetAsObject(index: Integer): TPCJSONObject;
begin
  Result := GetItemOfType(index,TPCJSONObject) as TPCJSONObject;
end;

function TPCJSONArray.GetAsVariant(index: Integer): TPCJSONVariantValue;
begin
  Result := GetItemOfType(index,TPCJSONVariantValue) as TPCJSONVariantValue;
end;

function TPCJSONArray.GetItemOfType(Index: Integer;
  DataClass: TPCJSONDataClass): TPCJSONData;
Var V,New : TPCJSONData;
begin
  GrowToIndex(Index);
  V := GetItems(index);
  if Not (V is DataClass) then begin
    New := DataClass.Create;
    Items[index] := New;
    V := New;
  end;
  Result := V as DataClass;
end;

procedure TPCJSONArray.GrowToIndex(index: Integer);
begin
  While (index>=Count) do Insert(Count,TPCJSONVariantValue.Create);
end;

function TPCJSONArray.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
Var i : Integer;
begin
  If pretty then Result := prefix+'['
  else Result := '[';
  for i := 0 to Count - 1 do begin
    if (i>0) then begin
      Result := Result+',';
      If pretty then Result :=Result +#10+prefix;
    end;
    Result := Result + Items[i].ToJSONFormatted(pretty,prefix+'   ');
  end;
  Result := Result+']';
end;

{ TPCJSONList }

procedure TPCJSONList.CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData);
begin
  if (Index<0) Or (Index>Count) then raise Exception.Create('Invalid insert at index '+Inttostr(Index)+' (Count:'+Inttostr(Count)+')');
end;

procedure TPCJSONList.Clear;
begin
  while (FList.Count>0) do Delete(FList.Count-1);
end;

function TPCJSONList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TPCJSONList.Create;
begin
  inherited;
  FParent := Nil;
  FList := TList.Create;
end;

procedure TPCJSONList.Delete(index: Integer);
Var M : TPCJSONData;
begin
  M := GetItems(index);
  FList.Delete(index);
  M.Free;
end;

destructor TPCJSONList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPCJSONList.GetIndexAsArray(Index: Integer): TPCJSONArray;
Var D : TPCJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TPCJSONArray)) then begin
    Result := TPCJSONArray.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TPCJSONArray(D);
end;

function TPCJSONList.GetIndexAsObject(Index: Integer): TPCJSONObject;
Var D : TPCJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TPCJSONObject)) then begin
    Result := TPCJSONObject.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TPCJSONObject(D);
end;

function TPCJSONList.GetIndexAsVariant(Index: Integer): TPCJSONVariantValue;
Var D : TPCJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TPCJSONVariantValue)) then begin
    Result := TPCJSONVariantValue.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TPCJSONVariantValue(D);
end;

function TPCJSONList.GetItems(Index: Integer): TPCJSONData;
begin
  Result := FList.Items[Index];
end;

procedure TPCJSONList.Insert(Index: Integer; PCJSONData: TPCJSONData);
begin
  CheckCanInsert(Index,PCJSONData);
  FList.Insert(Index,PCJSONData);
end;

procedure TPCJSONList.SetItems(Index: Integer; const Value: TPCJSONData);
Var OldP : TPCJSONData;
begin
  OldP := FList.Items[Index];
  Try
    FList.Items[Index] := Value;
  Finally
    OldP.Free;
  End;
end;

{ TPCJSONVariantValue }

Function VariantToDouble(Value : Variant) : Double;
Var s : String;
Begin
  Result := 0;
  Case varType(Value) of
    varSmallint, varInteger, varSingle, varDouble,
      varCurrency : Result := Value;
  Else
    Begin
      s := VarToStr(Value);
      If s='' Then Abort
      Else Result := StrToFloat(s);
    End;
  End;
End;

function TPCJSONVariantValue.AsBoolean(DefValue: Boolean): Boolean;
begin
  try
    Result := VarAsType(Value,varBoolean);
  except
    Result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsCurrency(DefValue: Currency): Currency;
begin
  try
    Result := VariantToDouble(Value);
  except
    Result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsCardinal(DefValue: Cardinal): Cardinal;
begin
  Result := Cardinal( StrToIntDef(VarToStrDef(Value,''),DefValue) );
end;

function TPCJSONVariantValue.AsDateTime(DefValue: TDateTime): TDateTime;
begin
  try
    Result := VarAsType(Value,varDate);
  except
    Result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsDouble(DefValue: Double): Double;
begin
  try
    Result := VariantToDouble(Value);
  except
    Result := DefValue;
  end;
end;

function TPCJSONVariantValue.AsInt64(DefValue: Int64): Int64;
begin
  Result := StrToInt64Def(VarToStrDef(Value,''),DefValue);
end;

function TPCJSONVariantValue.AsInteger(DefValue: Integer): Integer;
begin
  Result := StrToIntDef(VarToStrDef(Value,''),DefValue);
end;

function TPCJSONVariantValue.AsString(DefValue: String): String;
begin
  try
    Case VarType(Value) of
      varNull : Result := '';
      varSmallint, varInteger :
        Begin
          Result := inttostr(Value);
        End;
      varSingle, varDouble,varCurrency :
        Begin
          Result := FloatToStr(VariantToDouble(Value));
        End;
      varDate : Result := DateTimeToStr(Value);
    Else Result := VarToStr(Value);
    End;
  except
    Result := DefValue;
  end;
end;

constructor TPCJSONVariantValue.Create;
begin
  inherited;
  FValue := Null;
  FOldValue := Unassigned;
  FWritable := False;
end;

constructor TPCJSONVariantValue.CreateFromJSONValue(JSONValue: TJSONValue);
{$IFnDEF FPC}
Var d : Double;
    i64 : Integer;
  ds,ts : Char;
{$ENDIF}
begin
  Create;
  {$IFDEF FPC}
  Value := JSONValue.Value;
  {$ELSE}
  if JSONValue is TJSONNumber then begin
    d := TJSONNumber(JSONValue).AsDouble;
    if Pos('.',JSONValue.ToString)>0 then i64 := 0
    else i64 := TJSONNumber(JSONValue).AsInt;

    // Delphi 6 introduced "conditional compilation" and Delphi XE 6 (27) introduced FormatSettings variable.
    {$IF Defined(DCC) and Declared(CompilerVersion) and (CompilerVersion >= 27.0)}
    ds := FormatSettings.DecimalSeparator;
    ts := FormatSettings.ThousandSeparator;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    {$ELSE}
    ds := DecimalSeparator;
    ts := ThousandSeparator;
    DecimalSeparator := '.';
    ThousandSeparator := ',';
    {$IFEND}

    Try
      if FormatFloat('0.###########',d)=inttostr(i64) then
        Value := i64
      else Value := d;
    Finally
      // Delphi 6 introduced "conditional compilation" and Delphi XE 6 (27) introduced FormatSettings variable.
      {$IF Defined(DCC) and Declared(CompilerVersion) and (CompilerVersion >= 27.0)}
      FormatSettings.DecimalSeparator := ds;
      FormatSettings.ThousandSeparator := ts;
      {$ELSE}
      DecimalSeparator := ds;
      ThousandSeparator := ts;
      {$IFEND}
    End;
  end else if JSONValue is TJSONTrue then Value := true
  else if JSONValue is TJSONFalse then Value := false
  else if JSONValue is TJSONNull then Value := Null
  else Value := JSONValue.Value;
  {$ENDIF}
end;

function TPCJSONVariantValue.IsNull: Boolean;
begin
  Result := VarIsNull(FValue) or VarIsEmpty(FValue);
end;

procedure TPCJSONVariantValue.SetValue(const Value: Variant);
begin
  FOldValue := FValue;
  FValue := Value;
end;

function TPCJSONVariantValue.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
Var   ds,ts : Char;
begin
  Case VarType(Value) of
    varSmallint,varInteger,varByte,varWord,
    varLongWord,varInt64 : Result := VarToStr(Value);
    varBoolean : if (Value) then Result := 'true' else Result:='false';
    varNull : Result := 'null';
    varDate,varDouble : begin
      {$IF Defined(DCC) and Declared(CompilerVersion) and (CompilerVersion >= 27.0)}
      ds := FormatSettings.DecimalSeparator;
      ts := FormatSettings.ThousandSeparator;
      FormatSettings.DecimalSeparator := '.';
      FormatSettings.ThousandSeparator := ',';
      {$ELSE}
      ds := DecimalSeparator;
      ts := ThousandSeparator;
      DecimalSeparator := '.';
      ThousandSeparator := ',';
      {$IFEND}

      try
        Result := FormatFloat('0.###########',Value);
      finally
        {$IF Defined(DCC) and Declared(CompilerVersion) and (CompilerVersion >= 27.0)}
        FormatSettings.DecimalSeparator := ds;
        FormatSettings.ThousandSeparator := ts;
        {$ELSE}
        DecimalSeparator := ds;
        ThousandSeparator := ts;
        {$IFEND}
      end;
    end
  else
    Result := UTF8JSONEncode(VarToStr(Value),true);
  end;
end;

{ TPCJSONObject }

function TPCJSONObject.AsBoolean(ParamName: String; DefValue: Boolean): Boolean;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VarAsType(v,varBoolean);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsCardinal(ParamName: String; DefValue: Cardinal): Cardinal;
begin
  Result := Cardinal(AsInt64(ParamName,DefValue));
end;

function TPCJSONObject.AsCurrency(ParamName: String; DefValue: Currency): Currency;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VariantToDouble(v);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsDateTime(ParamName: String;
  DefValue: TDateTime): TDateTime;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VarAsType(v,varDate);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsDouble(ParamName: String; DefValue: Double): Double;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VariantToDouble(v);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsInt64(ParamName: String; DefValue: Int64): Int64;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := StrToInt64Def(VarToStrDef(v,''),DefValue);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsInteger(ParamName: String; DefValue: Integer): Integer;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := StrToIntDef(VarToStrDef(v,''),DefValue);
  except
    Result := DefValue;
  end;
end;

function TPCJSONObject.AsString(ParamName: String; DefValue: String): String;
Var v : Variant;
  VV : TPCJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    Case VarType(V) of
      varNull : Result := '';
      varSmallint, varInteger :
        Begin
          Result := inttostr(v);
        End;
      varSingle, varDouble,varCurrency :
        Begin
          Result := FloatToStr(VariantToDouble(v));
        End;
      varDate : Result := DateTimeToStr(v);
    Else Result := VarToStr(v);
    End;
  except
    Result := DefValue;
  end;
end;


procedure TPCJSONObject.CheckCanInsert(Index: Integer; PCJSONData: TPCJSONData);
begin
  inherited;
  if Not Assigned(PCJSONData) then raise Exception.Create('Object is nil');
  if Not (PCJSONData is TPCJSONNameValue) then raise Exception.Create('Object inside a '+TPCJSONData.ClassName+' must be a '+TPCJSONNameValue.ClassName+' (currently '+PCJSONData.ClassName+')');
end;

procedure TPCJSONObject.CheckValidName(Name: String);
Var i : Integer;
begin
  for i := 1 to Length(Name) do begin
    if i=1 then begin
      if Not (Name[i] in ['a'..'z','A'..'Z','0'..'9','_','.']) then raise Exception.Create(Format('Invalid char %s at pos %d/%d',[Name[i],i,length(Name)]));
    end else begin
      if Not (Name[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then raise Exception.Create(Format('Invalid char %s at pos %d/%d',[Name[i],i,length(Name)]));
    end;
  end;
end;

constructor TPCJSONObject.Create;
begin
  inherited;
end;

constructor TPCJSONObject.CreateFromJSONObject(JSONObject: TJSONObject);
var i,i2 : Integer;
  {$IFDEF FPC}
  aname : TJSONStringType;
  {$ENDIF}
begin
  Create;
  {$IFDEF FPC}
  for i := 0 to JSONObject.Count - 1 do begin
    aname := JSONObject.Names[i];
    i2 := GetIndexOrCreateName(JSONObject.Names[i]);
    if (JSONObject.Types[ aname ] = jtArray) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONArray.CreateFromJSONArray(JSONObject.Arrays[aname]);
    end else if (JSONObject.Types[ aname ] = jtObject) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONObject.CreateFromJSONObject(JSONObject.Objects[aname]);
    end else if (JSONObject.Types[ aname ] in [jtBoolean,jtNull,jtNumber,jtString]) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONVariantValue.CreateFromJSONValue(JSONObject.Items[i]);
    end else raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.'+aname+': '+JSONObject.Items[i].ClassName);
  end;
  {$ELSE}
  for i := 0 to JSONObject.Size - 1 do begin
    i2 := GetIndexOrCreateName(JSONObject.Get(i).JsonString.Value);
    if (JSONObject.Get(i).JsonValue is TJSONArray) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONArray.CreateFromJSONArray(TJSONArray(JSONObject.Get(i).JsonValue));
    end else if (JSONObject.Get(i).JsonValue is TJSONObject) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONObject.CreateFromJSONObject(TJSONObject(JSONObject.Get(i).JsonValue));
    end else if (JSONObject.Get(i).JsonValue is TJSONValue) then begin
      (Items[i2] as TPCJSONNameValue).Value := TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONObject.Get(i).JsonValue));
    end else raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.'+JSONObject.Get(i).JsonString.Value+': '+JSONObject.Get(i).ClassName);
  end;
  {$ENDIF}
end;


procedure TPCJSONObject.DeleteName(Name: String);
Var i : Integer;
begin
  i := IndexOfName(Name);
  if (i>=0) then begin
    Delete(i);
  end;
end;

destructor TPCJSONObject.Destroy;
begin

  inherited;
end;

function TPCJSONObject.FindName(Name: String): TPCJSONNameValue;
Var i : Integer;
begin
  i := IndexOfName(Name);
  Result := Nil;
  if (i>=0) then Result := Items[i] as TPCJSONNameValue;
end;

function TPCJSONObject.GetAsArray(Name: String): TPCJSONArray;
Var NV : TPCJSONNameValue;
  V : TPCJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TPCJSONArray) then begin
    NV.Value := TPCJSONArray.Create;
  end;
  Result := NV.Value as TPCJSONArray;
end;

function TPCJSONObject.GetAsObject(Name: String): TPCJSONObject;
Var NV : TPCJSONNameValue;
  V : TPCJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TPCJSONObject) then begin
    NV.Value := TPCJSONObject.Create;
  end;
  Result := NV.Value as TPCJSONObject;
end;

function TPCJSONObject.GetAsVariant(Name: String): TPCJSONVariantValue;
Var NV : TPCJSONNameValue;
  V : TPCJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TPCJSONVariantValue) then begin
    NV.Value := TPCJSONVariantValue.Create;
  end;
  Result := NV.Value as TPCJSONVariantValue;
end;

function TPCJSONObject.GetByName(Name: String): TPCJSONNameValue;
Var i : Integer;
begin
  i := GetIndexOrCreateName(Name);
  Result := Items[i] as TPCJSONNameValue;
end;

function TPCJSONObject.GetIndexOrCreateName(Name: String): Integer;
Var
  NV : TPCJSONNameValue;
Begin
  Result := IndexOfName(Name);
  if (Result<0) then begin
    CheckValidName(Name);
    NV := TPCJSONNameValue.Create(Name);
    Result := FList.Add(NV);
  end;
end;

function TPCJSONObject.GetNameValue(index: Integer): TPCJSONNameValue;
begin
  Result := Items[index] as TPCJSONNameValue;
end;

function TPCJSONObject.IsNull(ParamName: String): Boolean;
Var i : Integer;
  NV : TPCJSONNameValue;
begin
  i := IndexOfName(ParamName);
  if i<0 then result := true
  else begin
    Result := false;
    NV := TPCJSONNameValue( FList.Items[i] );
    If (Assigned(NV.Value)) AND (NV.Value is TPCJSONVariantValue) then begin
      Result := TPCJSONVariantValue(NV.Value).IsNull;
    end;
  end;
end;

function TPCJSONObject.IndexOfName(Name: String): Integer;
begin
  for Result := 0 to FList.Count - 1 do begin
    if (Assigned(FList.Items[Result])) And (TObject(FList.Items[Result]) is TPCJSONNameValue) then begin
      If TPCJSONNameValue( FList.Items[Result] ).Name = Name then begin
        exit;
      end;
    end;
  end;
  Result := -1;
end;

function TPCJSONObject.LoadAsStream(ParamName: String; Stream: TStream): Integer;
Var s : AnsiString;
begin
  s := AsString(ParamName,'');
  if (s<>'') then begin
    Stream.Write(s[1],length(s));
  end;
  Result := Length(s);
end;

function TPCJSONObject.SaveAsStream(ParamName: String; Stream: TStream): Integer;
Var s : AnsiString;
begin
  Stream.Position := 0;
  SetLength(s,Stream.Size);
  Stream.Read(s[1],Stream.Size);
  GetAsVariant(ParamName).Value := s;
end;

procedure TPCJSONObject.SetAs(Name: String; Value: TPCJSONData);
 // When assigning a object with SetAs this will not be freed automatically
Var NV : TPCJSONNameValue;
  V : TPCJSONData;
  i : Integer;
begin
  i := GetIndexOrCreateName(Name);
  NV := Items[i] as TPCJSONNameValue;
  NV.Value := Value;
  NV.FFreeValue := false;
end;

function TPCJSONObject.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
Var i : Integer;
begin
  if pretty then Result := prefix+'{'
  else Result := '{';
  for i := 0 to Count - 1 do begin
    if (i>0) then Begin
      Result := Result+',';
      If pretty then Result :=Result +#10+prefix;
    End;
    Result := Result + Items[i].ToJSONFormatted(pretty,prefix+'   ');
  end;
  Result := Result+'}';
end;

{ TPCJSONNameValue }

constructor TPCJSONNameValue.Create(AName: String);
begin
  inherited Create;
  FName := AName;
  FValue := TPCJSONData.Create;
  FFreeValue := True;
end;

destructor TPCJSONNameValue.Destroy;
begin
  if FFreeValue then FValue.Free;
  inherited;
end;

procedure TPCJSONNameValue.SetValue(const Value: TPCJSONData);
Var old : TPCJSONData;
begin
  if FValue=Value then exit;
  old := FValue;
  FValue := Value;
  if FFreeValue then old.Free;
  FFreeValue := true;
end;

function TPCJSONNameValue.ToJSONFormatted(pretty: Boolean; const prefix: AnsiString): AnsiString;
begin
  if pretty then Result := prefix else Result := '';
  Result := Result + UTF8JSONEncode(name,true)+':'+Value.ToJSONFormatted(pretty,prefix+'   ');
end;

{ TPCJSONData }

Var _objectsCount : Integer;

procedure TPCJSONData.Assign(PCJSONData: TPCJSONData);
Var i : Integer;
  NV : TPCJSONNameValue;
  JSOND : TPCJSONData;
  s : AnsiString;
begin
  if Not Assigned(PCJSONData) then Abort;
  if (PCJSONData is TPCJSONObject) AND (Self is TPCJSONObject) then begin
    for i := 0 to TPCJSONObject(PCJSONData).Count - 1 do begin
      NV := TPCJSONObject(PCJSONData).Items[i] as TPCJSONNameValue;
      if NV.Value is TPCJSONObject then begin
        TPCJSONObject(Self).GetAsObject(NV.Name).Assign(NV.Value);
      end else if NV.Value is TPCJSONArray then begin
        TPCJSONObject(Self).GetAsArray(NV.Name).Assign(NV.Value);
      end else if NV.Value is TPCJSONVariantValue then begin
        TPCJSONObject(Self).GetAsVariant(NV.Name).Assign(NV.Value);
      end else raise Exception.Create('Error in TPCJSONData.Assign decoding '+NV.Name+' ('+NV.Value.ClassName+')');
    end;
  end else if (PCJSONData is TPCJSONArray) AND (Self is TPCJSONArray) then begin
    for i := 0 to TPCJSONArray(PCJSONData).Count - 1 do begin
      JSOND := TPCJSONArray(PCJSONData).Items[i];
      s := JSOND.ToJSON(false);
      TPCJSONArray(Self).Insert(TPCJSONArray(Self).Count,TPCJSONData.ParseJSONValue(s));
    end;
  end else if (PCJSONData is TPCJSONVariantValue) AND (Self is TPCJSONVariantValue) then begin
    TPCJSONVariantValue(Self).Value := TPCJSONVariantValue(PCJSONData).Value;
  end else begin
    raise Exception.Create('Error in TPCJSONData.Assign assigning a '+PCJSONData.ClassName+' to a '+ClassName);
  end;

end;

constructor TPCJSONData.Create;
begin
   inc(_objectsCount);
end;

destructor TPCJSONData.Destroy;
begin
  dec(_objectsCount);
  inherited;
end;

class function TPCJSONData.ParseJSONValue(Const JSONObject: TBytes): TPCJSONData;
Var JS : TJSONValue;
  {$IFDEF FPC}
  jss : TJSONStringType;
  i : Integer;
  {$ENDIF}
begin
  Result := Nil;
  JS := Nil;
  {$IFDEF FPC}
  SetLength(jss,length(JSONObject));
  for i:=0 to High(JSONObject) do jss[i+1] := AnsiChar( JSONObject[i] );
  Try
    JS := GetJSON(jss);
  Except
    On E:Exception do begin
      TLog.NewLog(ltDebug,ClassName,'Error processing JSON: '+E.Message);
    end;
  end;
  {$ELSE}
  Try
    JS := TJSONObject.ParseJSONValue(JSONObject,0);
  Except
    On E:Exception do begin
      TLog.NewLog(ltDebug,ClassName,'Error processing JSON: '+E.Message);
    end;
  End;
  {$ENDIF}
  if Not Assigned(JS) then exit;
  Try
    if JS is TJSONObject then begin
      Result := TPCJSONObject.CreateFromJSONObject(TJSONObject(JS));
    end else if JS is TJSONArray then begin
      Result := TPCJSONArray.CreateFromJSONArray(TJSONArray(JS));
    end else if JS is TJSONValue then begin
      Result := TPCJSONVariantValue.CreateFromJSONValue(TJSONValue(JS));
    end else raise EPCParametresError.Create('Invalid TJSON Data type '+JS.ClassName);
  Finally
    JS.Free;
  End;
end;

procedure TPCJSONData.SaveToStream(Stream: TStream);
Var s : AnsiString;
begin
  s := ToJSON(false);
  Stream.Write(s[1],length(s));
end;

class function TPCJSONData.ParseJSONValue(Const JSONObject: String): TPCJSONData;
begin
  Result := ParseJSONValue( TEncoding.ASCII.GetBytes(JSONObject) );
end;

function TPCJSONData.ToJSON(pretty: Boolean): AnsiString;
begin
  Result := ToJSONFormatted(pretty,'');
end;

class function TPCJSONData._GetCount: Integer;
begin
  Result := _objectsCount;
end;

initialization
  _objectsCount := 0;
end.
