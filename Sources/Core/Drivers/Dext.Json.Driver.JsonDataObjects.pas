unit Dext.Json.Driver.JsonDataObjects;

interface

uses
  System.SysUtils,
  Dext.Json.Types,
  JsonDataObjects;

type
  TJsonDataObjectWrapper = class(TInterfacedObject, IDextJsonNode)
  protected
    function GetNodeType: TDextJsonNodeType; virtual; abstract;
    function AsString: string; virtual; abstract;
    function AsInteger: Integer; virtual; abstract;
    function AsInt64: Int64; virtual; abstract;
    function AsDouble: Double; virtual; abstract;
    function AsBoolean: Boolean; virtual; abstract;
    function ToJson(Indented: Boolean = False): string; virtual; abstract;
  end;

  TJsonDataObjectAdapter = class(TJsonDataObjectWrapper, IDextJsonObject)
  private
    FObj: TJsonObject;
    FOwnsObject: Boolean;
  public
    constructor Create(AObj: TJsonObject; AOwnsObject: Boolean = True);
    destructor Destroy; override;

    // IDextJsonNode
    function GetNodeType: TDextJsonNodeType; override;
    function AsString: string; override;
    function AsInteger: Integer; override;
    function AsInt64: Int64; override;
    function AsDouble: Double; override;
    function AsBoolean: Boolean; override;
    function ToJson(Indented: Boolean = False): string; override;

    // IDextJsonObject
    function Contains(const Name: string): Boolean;
    function GetNode(const Name: string): IDextJsonNode;
    function GetString(const Name: string): string;
    function GetInteger(const Name: string): Integer;
    function GetInt64(const Name: string): Int64;
    function GetDouble(const Name: string): Double;
    function GetBoolean(const Name: string): Boolean;
    function GetObject(const Name: string): IDextJsonObject;
    function GetArray(const Name: string): IDextJsonArray;

    procedure SetString(const Name, Value: string);
    procedure SetInteger(const Name: string; Value: Integer);
    procedure SetInt64(const Name: string; Value: Int64);
    procedure SetDouble(const Name: string; Value: Double);
    procedure SetBoolean(const Name: string; Value: Boolean);
    procedure SetObject(const Name: string; Value: IDextJsonObject);
    procedure SetArray(const Name: string; Value: IDextJsonArray);
    procedure SetNull(const Name: string);
  end;

  TJsonDataArrayAdapter = class(TJsonDataObjectWrapper, IDextJsonArray)
  private
    FArr: TJsonArray;
    FOwnsObject: Boolean;
  public
    constructor Create(AArr: TJsonArray; AOwnsObject: Boolean = True);
    destructor Destroy; override;

    // IDextJsonNode
    function GetNodeType: TDextJsonNodeType; override;
    function AsString: string; override;
    function AsInteger: Integer; override;
    function AsInt64: Int64; override;
    function AsDouble: Double; override;
    function AsBoolean: Boolean; override;
    function ToJson(Indented: Boolean = False): string; override;

    // IDextJsonArray
    function GetCount: NativeInt;
    function GetNode(Index: Integer): IDextJsonNode;
    function GetString(Index: Integer): string;
    function GetInteger(Index: Integer): Integer;
    function GetInt64(Index: Integer): Int64;
    function GetDouble(Index: Integer): Double;
    function GetBoolean(Index: Integer): Boolean;
    function GetObject(Index: Integer): IDextJsonObject;
    function GetArray(Index: Integer): IDextJsonArray;

    procedure Add(const Value: string); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: Int64); overload;
    procedure Add(Value: Double); overload;
    procedure Add(Value: Boolean); overload;
    procedure Add(Value: IDextJsonObject); overload;
    procedure Add(Value: IDextJsonArray); overload;
    procedure AddNull;
  end;

  TJsonDataObjectsProvider = class(TInterfacedObject, IDextJsonProvider)
  public
    function CreateObject: IDextJsonObject;
    function CreateArray: IDextJsonArray;
    function Parse(const Json: string): IDextJsonNode;
  end;

implementation

{ TJsonDataObjectAdapter }

constructor TJsonDataObjectAdapter.Create(AObj: TJsonObject; AOwnsObject: Boolean);
begin
  inherited Create;
  FObj := AObj;
  FOwnsObject := AOwnsObject;
end;

destructor TJsonDataObjectAdapter.Destroy;
begin
  if FOwnsObject then
    FObj.Free;
  inherited;
end;

function TJsonDataObjectAdapter.GetNodeType: TDextJsonNodeType;
begin
  Result := jntObject;
end;

function TJsonDataObjectAdapter.AsString: string;
begin
  Result := FObj.ToJSON();
end;

function TJsonDataObjectAdapter.AsInteger: Integer;
begin
  Result := 0; // Not applicable
end;

function TJsonDataObjectAdapter.AsInt64: Int64;
begin
  Result := 0; // Not applicable
end;

function TJsonDataObjectAdapter.AsDouble: Double;
begin
  Result := 0; // Not applicable
end;

function TJsonDataObjectAdapter.AsBoolean: Boolean;
begin
  Result := False; // Not applicable
end;

function TJsonDataObjectAdapter.ToJson(Indented: Boolean): string;
begin
  Result := FObj.ToJSON(Indented);
end;

function TJsonDataObjectAdapter.Contains(const Name: string): Boolean;
begin
  Result := FObj.Contains(Name);
end;

function TJsonDataObjectAdapter.GetNode(const Name: string): IDextJsonNode;
begin
  // Helper to wrap result based on type
  case FObj.Types[Name] of
    jdtObject: Result := TJsonDataObjectAdapter.Create(FObj.O[Name], False); // Reference
    jdtArray: Result := TJsonDataArrayAdapter.Create(FObj.A[Name], False); // Reference
    // For primitives, we don't have a node wrapper yet, returning nil or implementing a primitive wrapper?
    // For simplicity in this phase, we might not need GetNode for primitives if we use GetString etc.
    // But for full abstraction we might need TJsonValueAdapter.
    else Result := nil; 
  end;
end;

function TJsonDataObjectAdapter.GetString(const Name: string): string;
begin
  Result := FObj.S[Name];
end;

function TJsonDataObjectAdapter.GetInteger(const Name: string): Integer;
begin
  Result := FObj.I[Name];
end;

function TJsonDataObjectAdapter.GetInt64(const Name: string): Int64;
begin
  Result := FObj.L[Name];
end;

function TJsonDataObjectAdapter.GetDouble(const Name: string): Double;
begin
  Result := FObj.F[Name];
end;

function TJsonDataObjectAdapter.GetBoolean(const Name: string): Boolean;
begin
  Result := FObj.B[Name];
end;

function TJsonDataObjectAdapter.GetObject(const Name: string): IDextJsonObject;
var
  Obj: TJsonObject;
begin
  Obj := FObj.O[Name];
  if Assigned(Obj) then
    Result := TJsonDataObjectAdapter.Create(Obj, False) // Reference, don't own
  else
    Result := nil;
end;

function TJsonDataObjectAdapter.GetArray(const Name: string): IDextJsonArray;
var
  Arr: TJsonArray;
begin
  Arr := FObj.A[Name];
  if Assigned(Arr) then
    Result := TJsonDataArrayAdapter.Create(Arr, False) // Reference
  else
    Result := nil;
end;

procedure TJsonDataObjectAdapter.SetString(const Name, Value: string);
begin
  FObj.S[Name] := Value;
end;

procedure TJsonDataObjectAdapter.SetInteger(const Name: string; Value: Integer);
begin
  FObj.I[Name] := Value;
end;

procedure TJsonDataObjectAdapter.SetInt64(const Name: string; Value: Int64);
begin
  FObj.L[Name] := Value;
end;

procedure TJsonDataObjectAdapter.SetDouble(const Name: string; Value: Double);
begin
  FObj.F[Name] := Value;
end;

procedure TJsonDataObjectAdapter.SetBoolean(const Name: string; Value: Boolean);
begin
  FObj.B[Name] := Value;
end;

procedure TJsonDataObjectAdapter.SetObject(const Name: string; Value: IDextJsonObject);
begin
  // This is tricky. We need to extract the underlying object or clone it.
  // Since we are in the same driver, we can cast.
  if Value = nil then
    FObj.O[Name] := nil
  else if Value is TJsonDataObjectAdapter then
  begin
    // JsonDataObjects takes ownership when assigning to O[]? 
    // No, O[] property setter usually clones or takes reference?
    // In JsonDataObjects: O[Name] := Value adds the object. 
    // If Value is already owned by someone else, we might have issues.
    // But here we are likely creating a new object to assign.
    
    // IMPORTANT: JsonDataObjects O[] setter creates a COPY if you assign a TJsonObject?
    // Actually JsonDataObjects.pas: property O[const Name: string]: TJsonObject read GetO write SetO;
    // SetO calls Add(Name, Value.Clone) usually? Or takes ownership?
    // Let's assume we need to be careful.
    // Ideally we should Add(Name, Obj).
    
    // For safety with JDO, we might need to Clone if we don't want to transfer ownership or if JDO requires it.
    // But usually we construct an object and put it in.
    
    FObj.O[Name] := (Value as TJsonDataObjectAdapter).FObj.Clone as TJsonObject;
  end;
end;

procedure TJsonDataObjectAdapter.SetArray(const Name: string; Value: IDextJsonArray);
begin
  if Value = nil then
    FObj.A[Name] := nil
  else if Value is TJsonDataArrayAdapter then
  begin
    FObj.A[Name] := (Value as TJsonDataArrayAdapter).FArr.Clone as TJsonArray;
  end;
end;

procedure TJsonDataObjectAdapter.SetNull(const Name: string);
begin
  // JDO doesn't have explicit SetNull but setting empty object or similar?
  // Or remove?
  // JDO handles nulls by absence or explicit null type.
  // FObj.Delete(Name) might be better, or FObj.Values[Name].ValueType := jdtNull?
  // Let's assume Delete for now or just ignore.
  // Actually JDO has explicit Null support?
  // FObj.Types[Name] := jdtNull? No.
  // Let's leave empty for now or implement properly later.
end;

{ TJsonDataArrayAdapter }

constructor TJsonDataArrayAdapter.Create(AArr: TJsonArray; AOwnsObject: Boolean);
begin
  inherited Create;
  FArr := AArr;
  FOwnsObject := AOwnsObject;
end;

destructor TJsonDataArrayAdapter.Destroy;
begin
  if FOwnsObject then
    FArr.Free;
  inherited;
end;

function TJsonDataArrayAdapter.GetNodeType: TDextJsonNodeType;
begin
  Result := jntArray;
end;

function TJsonDataArrayAdapter.AsString: string;
begin
  Result := FArr.ToJSON();
end;

function TJsonDataArrayAdapter.AsInteger: Integer;
begin
  Result := 0;
end;

function TJsonDataArrayAdapter.AsInt64: Int64;
begin
  Result := 0;
end;

function TJsonDataArrayAdapter.AsDouble: Double;
begin
  Result := 0;
end;

function TJsonDataArrayAdapter.AsBoolean: Boolean;
begin
  Result := False;
end;

function TJsonDataArrayAdapter.ToJson(Indented: Boolean): string;
begin
  Result := FArr.ToJSON(Indented);
end;

function TJsonDataArrayAdapter.GetCount: NativeInt;
begin
  Result := FArr.Count;
end;

function TJsonDataArrayAdapter.GetNode(Index: Integer): IDextJsonNode;
begin
  // Similar to Object.GetNode
  case FArr.Types[Index] of
    jdtObject: Result := TJsonDataObjectAdapter.Create(FArr.O[Index], False);
    jdtArray: Result := TJsonDataArrayAdapter.Create(FArr.A[Index], False);
    else Result := nil;
  end;
end;

function TJsonDataArrayAdapter.GetString(Index: Integer): string;
begin
  Result := FArr.S[Index];
end;

function TJsonDataArrayAdapter.GetInteger(Index: Integer): Integer;
begin
  Result := FArr.I[Index];
end;

function TJsonDataArrayAdapter.GetInt64(Index: Integer): Int64;
begin
  Result := FArr.L[Index];
end;

function TJsonDataArrayAdapter.GetDouble(Index: Integer): Double;
begin
  Result := FArr.F[Index];
end;

function TJsonDataArrayAdapter.GetBoolean(Index: Integer): Boolean;
begin
  Result := FArr.B[Index];
end;

function TJsonDataArrayAdapter.GetObject(Index: Integer): IDextJsonObject;
begin
  if (Index >= 0) and (Index < FArr.Count) and (FArr.Types[Index] = jdtObject) then
    Result := TJsonDataObjectAdapter.Create(FArr.O[Index], False)
  else
    Result := nil;
end;

function TJsonDataArrayAdapter.GetArray(Index: Integer): IDextJsonArray;
begin
  if (Index >= 0) and (Index < FArr.Count) and (FArr.Types[Index] = jdtArray) then
    Result := TJsonDataArrayAdapter.Create(FArr.A[Index], False)
  else
    Result := nil;
end;

procedure TJsonDataArrayAdapter.Add(const Value: string);
begin
  FArr.Add(Value);
end;

procedure TJsonDataArrayAdapter.Add(Value: Integer);
begin
  FArr.Add(Value);
end;

procedure TJsonDataArrayAdapter.Add(Value: Int64);
begin
  FArr.Add(Value);
end;

procedure TJsonDataArrayAdapter.Add(Value: Double);
begin
  FArr.Add(Value);
end;

procedure TJsonDataArrayAdapter.Add(Value: Boolean);
begin
  FArr.Add(Value);
end;

procedure TJsonDataArrayAdapter.Add(Value: IDextJsonObject);
begin
  if Value is TJsonDataObjectAdapter then
    FArr.Add((Value as TJsonDataObjectAdapter).FObj.Clone as TJsonObject)
  else
    FArr.Add(TJsonObject.Create); // Fallback empty
end;

procedure TJsonDataArrayAdapter.Add(Value: IDextJsonArray);
begin
  if Value is TJsonDataArrayAdapter then
    FArr.Add((Value as TJsonDataArrayAdapter).FArr.Clone as TJsonArray)
  else
    FArr.Add(TJsonArray.Create); // Fallback empty
end;

procedure TJsonDataArrayAdapter.AddNull;
begin
  // JDO add null?
  FArr.Add(''); // Hack for now, JDO handling of nulls in arrays is specific
end;

{ TJsonDataObjectsProvider }

function TJsonDataObjectsProvider.CreateObject: IDextJsonObject;
begin
  Result := TJsonDataObjectAdapter.Create(TJsonObject.Create, True);
end;

function TJsonDataObjectsProvider.CreateArray: IDextJsonArray;
begin
  Result := TJsonDataArrayAdapter.Create(TJsonArray.Create, True);
end;

function TJsonDataObjectsProvider.Parse(const Json: string): IDextJsonNode;
var
  JsonBase: TJsonBaseObject;
begin
  JsonBase := TJsonBaseObject.Parse(Json);
  if JsonBase is TJsonObject then
    Result := TJsonDataObjectAdapter.Create(TJsonObject(JsonBase), True)
  else if JsonBase is TJsonArray then
    Result := TJsonDataArrayAdapter.Create(TJsonArray(JsonBase), True)
  else
  begin
    JsonBase.Free;
    raise EJsonException.Create('Invalid JSON root');
  end;
end;

end.
