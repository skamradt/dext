unit Dext.OpenAPI.Types;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  /// <summary>
  ///   Represents the data type of a schema property in OpenAPI.
  /// </summary>
  TOpenAPIDataType = (
    odtString,
    odtNumber,
    odtInteger,
    odtBoolean,
    odtArray,
    odtObject
  );

  /// <summary>
  ///   Represents a schema definition in OpenAPI.
  /// </summary>
  TOpenAPISchema = class
  private
    FType: TOpenAPIDataType;
    FFormat: string;
    FDescription: string;
    FRequired: TArray<string>;
    FProperties: TDictionary<string, TOpenAPISchema>;
    FItems: TOpenAPISchema;
    FRef: string; // For $ref references
    FEnum: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    
    property DataType: TOpenAPIDataType read FType write FType;
    property Format: string read FFormat write FFormat;
    property Description: string read FDescription write FDescription;
    property Required: TArray<string> read FRequired write FRequired;
    property Properties: TDictionary<string, TOpenAPISchema> read FProperties;
    property Items: TOpenAPISchema read FItems write FItems;
    property Ref: string read FRef write FRef;
    property Enum: TArray<string> read FEnum write FEnum;
  end;

  /// <summary>
  ///   Represents a parameter in an OpenAPI operation.
  /// </summary>
  TOpenAPIParameterLocation = (oplQuery, oplPath, oplHeader, oplCookie);

  TOpenAPIParameter = class
  private
    FName: string;
    FIn: TOpenAPIParameterLocation;
    FDescription: string;
    FRequired: Boolean;
    FSchema: TOpenAPISchema;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Name: string read FName write FName;
    property Location: TOpenAPIParameterLocation read FIn write FIn;
    property Description: string read FDescription write FDescription;
    property Required: Boolean read FRequired write FRequired;
    property Schema: TOpenAPISchema read FSchema write FSchema;
  end;

  /// <summary>
  ///   Represents a request body in an OpenAPI operation.
  /// </summary>
  TOpenAPIRequestBody = class
  private
    FDescription: string;
    FRequired: Boolean;
    FContent: TDictionary<string, TOpenAPISchema>; // MediaType -> Schema
  public
    constructor Create;
    destructor Destroy; override;
    
    property Description: string read FDescription write FDescription;
    property Required: Boolean read FRequired write FRequired;
    property Content: TDictionary<string, TOpenAPISchema> read FContent;
  end;

  /// <summary>
  ///   Represents a response in an OpenAPI operation.
  /// </summary>
  TOpenAPIResponse = class
  private
    FDescription: string;
    FContent: TDictionary<string, TOpenAPISchema>; // MediaType -> Schema
  public
    constructor Create;
    destructor Destroy; override;
    
    property Description: string read FDescription write FDescription;
    property Content: TDictionary<string, TOpenAPISchema> read FContent;
  end;

  /// <summary>
  ///   Represents an operation (GET, POST, etc.) in OpenAPI.
  /// </summary>
  TOpenAPIOperation = class
  private
    FSummary: string;
    FDescription: string;
    FOperationId: string;
    FTags: TArray<string>;
    FParameters: TObjectList<TOpenAPIParameter>;
    FRequestBody: TOpenAPIRequestBody;
    FResponses: TDictionary<string, TOpenAPIResponse>; // Status Code -> Response
  public
    constructor Create;
    destructor Destroy; override;
    
    property Summary: string read FSummary write FSummary;
    property Description: string read FDescription write FDescription;
    property OperationId: string read FOperationId write FOperationId;
    property Tags: TArray<string> read FTags write FTags;
    property Parameters: TObjectList<TOpenAPIParameter> read FParameters;
    property RequestBody: TOpenAPIRequestBody read FRequestBody write FRequestBody;
    property Responses: TDictionary<string, TOpenAPIResponse> read FResponses;
  end;

  /// <summary>
  ///   Represents a path item (endpoint) in OpenAPI.
  /// </summary>
  TOpenAPIPathItem = class
  private
    FGet: TOpenAPIOperation;
    FPost: TOpenAPIOperation;
    FPut: TOpenAPIOperation;
    FDelete: TOpenAPIOperation;
    FPatch: TOpenAPIOperation;
  public
    destructor Destroy; override;
    
    property Get: TOpenAPIOperation read FGet write FGet;
    property Post: TOpenAPIOperation read FPost write FPost;
    property Put: TOpenAPIOperation read FPut write FPut;
    property Delete: TOpenAPIOperation read FDelete write FDelete;
    property Patch: TOpenAPIOperation read FPatch write FPatch;
  end;

  /// <summary>
  ///   Represents server information in OpenAPI.
  /// </summary>
  TOpenAPIServer = class
  private
    FUrl: string;
    FDescription: string;
  public
    property Url: string read FUrl write FUrl;
    property Description: string read FDescription write FDescription;
  end;

  /// <summary>
  ///   Represents contact information in OpenAPI.
  /// </summary>
  TOpenAPIContact = class
  private
    FName: string;
    FEmail: string;
    FUrl: string;
  public
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Url: string read FUrl write FUrl;
  end;

  /// <summary>
  ///   Represents license information in OpenAPI.
  /// </summary>
  TOpenAPILicense = class
  private
    FName: string;
    FUrl: string;
  public
    property Name: string read FName write FName;
    property Url: string read FUrl write FUrl;
  end;

  /// <summary>
  ///   Represents API information in OpenAPI.
  /// </summary>
  TOpenAPIInfo = class
  private
    FTitle: string;
    FDescription: string;
    FVersion: string;
    FContact: TOpenAPIContact;
    FLicense: TOpenAPILicense;
  public
    destructor Destroy; override;
    
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Version: string read FVersion write FVersion;
    property Contact: TOpenAPIContact read FContact write FContact;
    property License: TOpenAPILicense read FLicense write FLicense;
  end;

  /// <summary>
  ///   Represents a complete OpenAPI 3.0 document.
  /// </summary>
  TOpenAPIDocument = class
  private
    FOpenAPI: string;
    FInfo: TOpenAPIInfo;
    FServers: TObjectList<TOpenAPIServer>;
    FPaths: TDictionary<string, TOpenAPIPathItem>;
    FSchemas: TDictionary<string, TOpenAPISchema>;
  public
    constructor Create;
    destructor Destroy; override;
    
    property OpenAPI: string read FOpenAPI write FOpenAPI;
    property Info: TOpenAPIInfo read FInfo write FInfo;
    property Servers: TObjectList<TOpenAPIServer> read FServers;
    property Paths: TDictionary<string, TOpenAPIPathItem> read FPaths;
    property Schemas: TDictionary<string, TOpenAPISchema> read FSchemas;
  end;

implementation

{ TOpenAPISchema }

constructor TOpenAPISchema.Create;
begin
  inherited;
  FProperties := TDictionary<string, TOpenAPISchema>.Create;
end;

destructor TOpenAPISchema.Destroy;
var
  Schema: TOpenAPISchema;
begin
  for Schema in FProperties.Values do
    Schema.Free;
  FProperties.Free;
  
  if Assigned(FItems) then
    FItems.Free;
    
  inherited;
end;

{ TOpenAPIParameter }

constructor TOpenAPIParameter.Create;
begin
  inherited;
  FSchema := TOpenAPISchema.Create;
end;

destructor TOpenAPIParameter.Destroy;
begin
  FSchema.Free;
  inherited;
end;

{ TOpenAPIRequestBody }

constructor TOpenAPIRequestBody.Create;
begin
  inherited;
  FContent := TDictionary<string, TOpenAPISchema>.Create;
end;

destructor TOpenAPIRequestBody.Destroy;
var
  Schema: TOpenAPISchema;
begin
  for Schema in FContent.Values do
    Schema.Free;
  FContent.Free;
  inherited;
end;

{ TOpenAPIResponse }

constructor TOpenAPIResponse.Create;
begin
  inherited;
  FContent := TDictionary<string, TOpenAPISchema>.Create;
end;

destructor TOpenAPIResponse.Destroy;
var
  Schema: TOpenAPISchema;
begin
  for Schema in FContent.Values do
    Schema.Free;
  FContent.Free;
  inherited;
end;

{ TOpenAPIOperation }

constructor TOpenAPIOperation.Create;
begin
  inherited;
  FParameters := TObjectList<TOpenAPIParameter>.Create(True);
  FResponses := TDictionary<string, TOpenAPIResponse>.Create;
end;

destructor TOpenAPIOperation.Destroy;
var
  Response: TOpenAPIResponse;
begin
  FParameters.Free;
  
  for Response in FResponses.Values do
    Response.Free;
  FResponses.Free;
  
  if Assigned(FRequestBody) then
    FRequestBody.Free;
    
  inherited;
end;

{ TOpenAPIPathItem }

destructor TOpenAPIPathItem.Destroy;
begin
  if Assigned(FGet) then FGet.Free;
  if Assigned(FPost) then FPost.Free;
  if Assigned(FPut) then FPut.Free;
  if Assigned(FDelete) then FDelete.Free;
  if Assigned(FPatch) then FPatch.Free;
  inherited;
end;

{ TOpenAPIInfo }

destructor TOpenAPIInfo.Destroy;
begin
  if Assigned(FContact) then
    FContact.Free;
  if Assigned(FLicense) then
    FLicense.Free;
  inherited;
end;

{ TOpenAPIDocument }

constructor TOpenAPIDocument.Create;
begin
  inherited;
  FOpenAPI := '3.0.0';
  FInfo := TOpenAPIInfo.Create;
  FServers := TObjectList<TOpenAPIServer>.Create(True);
  FPaths := TDictionary<string, TOpenAPIPathItem>.Create;
  FSchemas := TDictionary<string, TOpenAPISchema>.Create;
end;

destructor TOpenAPIDocument.Destroy;
var
  PathItem: TOpenAPIPathItem;
  Schema: TOpenAPISchema;
begin
  FInfo.Free;
  FServers.Free;
  
  for PathItem in FPaths.Values do
    PathItem.Free;
  FPaths.Free;
  
  for Schema in FSchemas.Values do
    Schema.Free;
  FSchemas.Free;
  
  inherited;
end;

end.
