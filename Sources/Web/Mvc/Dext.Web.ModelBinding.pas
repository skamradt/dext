{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Web.ModelBinding;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  Dext.Types.UUID,
  Dext.DI.Interfaces,
  Dext.Web.Interfaces,
  Dext.Json,
  Dext.Json.Types;

type
  EBindingException = class(Exception);

  /// <summary>
  ///   Defines the source from where a model or parameter should be bound.
  /// </summary>
  TBindingSource = (
    bsBody,     // JSON body
    bsQuery,    // Query string
    bsRoute,    // Route parameters
    bsHeader,   // HTTP headers
    bsServices, // DI Container
    bsForm      // Form data (future)
  );

  /// <summary>
  ///   Base class for binding attributes.
  /// </summary>
  BindingAttribute = class abstract(TCustomAttribute)
  private
    FSource: TBindingSource;
  public
    constructor Create(ASource: TBindingSource);
    property Source: TBindingSource read FSource;
  end;

  /// <summary>
  ///   Specifies that a parameter or property should be bound using the request body.
  /// </summary>
  FromBodyAttribute = class(BindingAttribute)
  public
    constructor Create; overload;
  end;

  /// <summary>
  ///   Specifies that a parameter or property should be bound using the request query string.
  /// </summary>
  FromQueryAttribute = class(BindingAttribute)
  private
    FName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies that a parameter or property should be bound using route data.
  /// </summary>
  FromRouteAttribute = class(BindingAttribute)
  private
    FName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies that a parameter or property should be bound using the request headers.
  /// </summary>
  FromHeaderAttribute = class(BindingAttribute)
  private
    FName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies that a parameter should be bound using the dependency injection container.
  /// </summary>
  FromServicesAttribute = class(BindingAttribute)
  public
    constructor Create; overload;
  end;

  /// <summary>
  ///   Defines the contract for model binding.
  /// </summary>
  IModelBinder = interface
    ['{6CDDAA4C-EB6B-42F0-A138-614FFBA931A5}']
    /// <summary>
    ///   Binds a model from the request body.
    /// </summary>
    function BindBody(AType: PTypeInfo; Context: IHttpContext): TValue;
    
    /// <summary>
    ///   Binds a model from the query string.
    /// </summary>
    function BindQuery(AType: PTypeInfo; Context: IHttpContext): TValue;
    
    /// <summary>
    ///   Binds a model from route data.
    /// </summary>
    function BindRoute(AType: PTypeInfo; Context: IHttpContext): TValue;
    
    /// <summary>
    ///   Binds a model from request headers.
    /// </summary>
    function BindHeader(AType: PTypeInfo; Context: IHttpContext): TValue;
    
    /// <summary>
    ///   Binds a model from the service provider.
    /// </summary>
    function BindServices(AType: PTypeInfo; Context: IHttpContext): TValue;

    /// <summary>
    ///   Binds all parameters of a method.
    /// </summary>
    function BindMethodParameters(AMethod: TRttiMethod; AContext: IHttpContext): TArray<TValue>;
    
    /// <summary>
    ///   Binds a single parameter.
    /// </summary>
    function BindParameter(AParam: TRttiParameter; AContext: IHttpContext): TValue;

    /// <summary>
    ///   Binds a record from multiple sources (header, query, route, body, services)
    ///   based on field attributes. This is the preferred method for Minimal API
    ///   where records may have fields from different sources.
    /// </summary>
    function BindRecordHybrid(AType: PTypeInfo; Context: IHttpContext): TValue;
  end;

  TModelBinder = class(TInterfacedObject, IModelBinder)
  private
    function ConvertStringToType(const AValue: string; AType: PTypeInfo): TValue;
  public
    constructor Create;
    destructor Destroy; override;

    // Interface methods
    function BindBody(AType: PTypeInfo; Context: IHttpContext): TValue; overload;
    function BindQuery(AType: PTypeInfo; Context: IHttpContext): TValue; overload;
    function BindRoute(AType: PTypeInfo; Context: IHttpContext): TValue; overload;
    function BindHeader(AType: PTypeInfo; Context: IHttpContext): TValue;
    function BindServices(AType: PTypeInfo; Context: IHttpContext): TValue;

    // Helper methods com genéricos
    function BindBody<T>(Context: IHttpContext): T; overload;
    function BindQuery<T>(Context: IHttpContext): T; overload;
    function BindRoute<T>(Context: IHttpContext): T; overload;

    function BindMethodParameters(AMethod: TRttiMethod; AContext: IHttpContext): TArray<TValue>;
    function BindParameter(AParam: TRttiParameter; AContext: IHttpContext): TValue;

    /// <summary>
    ///   Binds a record from multiple sources based on field attributes.
    ///   Supports [FromHeader], [FromQuery], [FromRoute], [FromServices] and implicit [FromBody].
    /// </summary>
    function BindRecordHybrid(AType: PTypeInfo; Context: IHttpContext): TValue;
  end;

  TModelBinderHelper = class
  public
    class function BindQuery<T>(ABinder: IModelBinder; Context: IHttpContext): T; static;
    class function BindBody<T>(ABinder: IModelBinder; Context: IHttpContext): T; static;
    class function BindRoute<T>(ABinder: IModelBinder; Context: IHttpContext): T; static;
  end;

  // ✅ BINDING PROVIDER
  IBindingSourceProvider = interface
    ['{8D4F3A7C-1E4A-4B8D-B0E7-9F3A8C5D2B1E}']
    function GetBindingSource(Param: TRttiParameter): TBindingSource;
    function GetBindingName(Param: TRttiParameter): string;
  end;

  TBindingSourceProvider = class(TInterfacedObject, IBindingSourceProvider)
  public
    function GetBindingSource(Field: TRttiField): TBindingSource; overload;
    function GetBindingName(Field: TRttiField): string; overload;

    function GetBindingSource(Param: TRttiParameter): TBindingSource; overload;
    function GetBindingName(Param: TRttiParameter): string; overload;
  end;

implementation

uses
  System.NetEncoding,
  Dext.Core.DateUtils,
  Dext.Core.Span,
  Dext.Json.Utf8.Serializer;

{ BindingAttribute }

constructor BindingAttribute.Create(ASource: TBindingSource);
begin
  inherited Create;
  FSource := ASource;
end;

{ FromBodyAttribute }

constructor FromBodyAttribute.Create;
begin
  inherited Create(bsBody);
end;

{ FromQueryAttribute }

constructor FromQueryAttribute.Create;
begin
  inherited Create(bsQuery);
end;

constructor FromQueryAttribute.Create(const AName: string);
begin
  inherited Create(bsQuery);
  FName := AName;
end;

{ FromRouteAttribute }

constructor FromRouteAttribute.Create;
begin
  inherited Create(bsRoute);
end;

constructor FromRouteAttribute.Create(const AName: string);
begin
  inherited Create(bsRoute);
  FName := AName;
end;

{ FromHeaderAttribute }

constructor FromHeaderAttribute.Create;
begin
  inherited Create(bsHeader);
end;

constructor FromHeaderAttribute.Create(const AName: string);
begin
  inherited Create(bsHeader);
  FName := AName;
end;

{ FromServicesAttribute }

constructor FromServicesAttribute.Create;
begin
  inherited Create(bsServices);
end;

{ TModelBinder }

constructor TModelBinder.Create;
begin
  inherited Create;
end;

destructor TModelBinder.Destroy;
begin

  inherited;
end;

function TModelBinder.BindBody(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  Stream: TStream;
  JsonString: string;
  Settings: TDextSettings;
  Bytes: TBytes;
  Span: TByteSpan;
begin
  if (AType.Kind <> tkRecord) and (AType.Kind <> tkClass) then
    raise EBindingException.Create('BindBody currently only supports records and classes');

  Stream := Context.Request.Body;
  if (Stream = nil) or (Stream.Size = 0) then
  begin
     // For Classes, might be valid to return nil, but generally body is expected.
     raise EBindingException.Create('Request body is empty');
  end;

  // OPTIMIZATION: Check if we can use Zero-Allocation UTF8 Serializer
  // Currently we just read bytes to avoid String conversion. 
  // In future we can get Span directly from Request if driver supports it.
  
  try
    // Read Body as Bytes (One copy stream -> bytes, but avoids bytes -> string -> TJsonDOM)
    if Stream is TBytesStream then
      Bytes := TBytesStream(Stream).Bytes
    else
    begin
      Stream.Position := 0;
      SetLength(Bytes, Stream.Size);
      if Stream.Size > 0 then
        Stream.ReadBuffer(Bytes[0], Stream.Size);
    end;
    
    if Length(Bytes) = 0 then
         raise EBindingException.Create('Request body is empty');

    Span := TByteSpan.FromBytes(Bytes);

    // Call Generic Deserialize<T> dynamically? 
    // TUtf8JsonSerializer.Deserialize<T> is static generic.
    // We have AType (PTypeInfo).
    // The current signature of BindBody returns TValue.
    // We need to invoke Deserialize via RTTI or use the non-generic version if we expose one.
    // However, TUtf8JsonSerializer currently exposes `Deserialize<T>`.
    // Let's use RTTI to invoke it for now, or just fallback to String for this specific non-generic call 
    // IF we are not called via generic BindBody<T>. 
    // BUT BindBody<T> calls this BindBody(PTypeInfo).
    
    // To truly use the new serializer efficiently without heavy generic invoking:
    // We should refactor BindBody<T> to call the serializer directly.
    // But for now, let's keep the architecture:
    
    // We'll read as string for THIS method (legacy path) unless we refactor BindBody<T>.
    // Wait, the user wants "Zero-Allocation". 
    // Reading stream to Bytes is fine.
    
    // Let's modify BindBody<T> below to use the new serializer, and keep this for compat/dynamic calls.
    // OR we change this method to try to dispatch to Utf8Serializer if possible.
    
    // Fallback to legacy string based for now in the non-generic untyped method
    JsonString := TEncoding.UTF8.GetString(Bytes);
    Settings := TDextSettings.Default.WithCaseInsensitive;
    Result := TDextJson.Deserialize(AType, JsonString, Settings);
    
  except
    on E: Exception do
      raise EBindingException.Create('Error binding body: ' + E.Message);
  end;
end;

// Optimized Generic Version
function TModelBinder.BindBody<T>(Context: IHttpContext): T;
var
  Stream: TStream;
  Bytes: TBytes;
  Span: TByteSpan;
begin
  // Only for Records for now
  if PTypeInfo(System.TypeInfo(T)).Kind = tkRecord then
  begin
      Stream := Context.Request.Body;
      if (Stream = nil) or (Stream.Size = 0) then
        raise EBindingException.Create('Request body is empty');

      // Read RAW BYTES (Zero String Allocation)
      // Ideally Context.Request would expose a Span/Bytes directly.
      if Stream is TBytesStream then
      begin
         Bytes := TBytesStream(Stream).Bytes;
         // Note: TBytesStream.Bytes returns the internal array usually - Zero copy? 
         // System.Classes TBytesStream properties expose raw TBytes. Good.
      end
      else
      begin
        Stream.Position := 0;
        SetLength(Bytes, Stream.Size);
        if Stream.Size > 0 then
          Stream.ReadBuffer(Bytes[0], Stream.Size);
      end;
      
      try
        Span := TByteSpan.FromBytes(Bytes);
        // FAST PATH: UTF8 Zero-Allocation Deserialization
        Result := TUtf8JsonSerializer.Deserialize<T>(Span);
        Exit;
      except
        // Fallback or re-raise
        on E: EUtf8SerializationException do
           raise EBindingException.Create('Error parsing JSON body: ' + E.Message);
      end;
  end;

  // Fallback for Classes/Other types (Legacy)
  var Value := BindBody(TypeInfo(T), Context);
  Result := Value.AsType<T>;
end;

function TModelBinder.BindQuery(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  ContextRtti: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  QueryParams: TStrings;
  FieldName: string;
  FieldValue: string;
begin
  if (AType.Kind <> tkRecord) and (AType.Kind <> tkClass) then
    raise EBindingException.Create('BindQuery currently only supports records and classes');

  ContextRtti := TRttiContext.Create;
  try
    RttiType := ContextRtti.GetType(AType);
    QueryParams := Context.Request.Query;

    // 1. Initialize Result
    if AType.Kind = tkClass then
    begin
       var MetaClass := (RttiType as TRttiInstanceType).MetaclassType;
       var FoundCreate := False;
       
       // Robustly searching for parameterless constructor
       for var M in RttiType.GetMethods do
       begin
         if (SameText(M.Name, 'Create')) and (M.MethodKind = mkConstructor) and (Length(M.GetParameters) = 0) then
         begin
           Result := M.Invoke(MetaClass, []);
           FoundCreate := True;
           Break;
         end;
       end;
       
       if not FoundCreate then
         raise EBindingException.CreateFmt('Cannot find parameterless constructor "Create" for %s in BindQuery.', [AType.Name]);
    end
    else
    begin
       TValue.Make(nil, AType, Result);
    end;

    // 2. Bind Fields (Records)
    if AType.Kind = tkRecord then
    begin
      for Field in RttiType.GetFields do
      begin
        var SourceProvider := TBindingSourceProvider.Create;
        try
          FieldName := SourceProvider.GetBindingName(Field);
        finally
          SourceProvider.Free;
        end;

        if QueryParams.IndexOfName(FieldName) >= 0 then
        begin
          FieldValue := TNetEncoding.URL.Decode(QueryParams.Values[FieldName]);
          try
            var Val := ConvertStringToType(FieldValue, Field.FieldType.Handle);
            Field.SetValue(Result.GetReferenceToRawData, Val);
          except
             on E: Exception do
               Writeln(Format('BindQuery warning: Error converting field "%s": %s', [FieldName, E.Message]));
          end;
        end;
      end;
    end
    // 3. Bind Properties (Classes)
    else if AType.Kind = tkClass then
    begin
       for var Prop in RttiType.GetProperties do
       begin
          if not Prop.IsWritable then Continue;
          
          FieldName := Prop.Name;
          for var Attr in Prop.GetAttributes do
             if Attr is FromQueryAttribute then
             begin
                var AttrName := FromQueryAttribute(Attr).Name;
                if AttrName <> '' then FieldName := AttrName;
             end;

           if QueryParams.IndexOfName(FieldName) >= 0 then
           begin
              FieldValue := TNetEncoding.URL.Decode(QueryParams.Values[FieldName]);
              try
                var Val := ConvertStringToType(FieldValue, Prop.PropertyType.Handle);
                Prop.SetValue(Result.AsObject, Val);
              except
                 on E: Exception do
                   Writeln(Format('BindQuery warning: Error converting property "%s": %s', [FieldName, E.Message]));
              end;
           end;
       end;
    end;

  finally
    ContextRtti.Free;
  end;
end;

function TryGetCaseInsensitive(const ADict: TDictionary<string, string>; const AKey: string; out AValue: string): Boolean;
begin
  Result := False;
  if ADict = nil then Exit;

  if ADict.TryGetValue(AKey, AValue) then
    Exit(True);

  for var Key in ADict.Keys do
  begin
    if SameText(Key, AKey) then
    begin
      AValue := ADict[Key];
      Exit(True);
    end;
  end;
end;

function TModelBinder.BindQuery<T>(Context: IHttpContext): T;
begin
  var Value := BindQuery(TypeInfo(T), Context);
  Result := Value.AsType<T>;
end;

function TModelBinder.BindRoute(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  ContextRtti: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  RouteParams: TDictionary<string, string>;
  FieldName: string;
  FieldValue: string;
  SingleParamValue: string;
begin
  // ✅ SUPPORT FOR PRIMITIVES (Single Route Param Inference)
  if (AType.Kind in [tkInteger, tkInt64, tkFloat, tkString, tkLString, tkWString, tkUString, tkEnumeration]) or
     ((AType.Kind = tkRecord) and ((AType = TypeInfo(TGUID)) or (AType = TypeInfo(TUUID)))) then
  begin
    RouteParams := Context.Request.RouteParams;
    
    if RouteParams.Count = 1 then
    begin
      for var Pair in RouteParams do
      begin
        SingleParamValue := Pair.Value;
        Break; 
      end;
      
      try
        Result := ConvertStringToType(SingleParamValue, AType);
        Exit;
      except
        on E: Exception do
          raise EBindingException.CreateFmt('Error converting route param "%s" to %s: %s', [SingleParamValue, AType.Name, E.Message]);
      end;
    end
    else if RouteParams.Count > 1 then
      raise EBindingException.CreateFmt('Ambiguous binding for type %s. Found %d route parameters. Use a Record.', [AType.Name, RouteParams.Count]);
  end;

  if AType.Kind <> tkRecord then
    raise EBindingException.Create('BindRoute currently only supports records or single primitive inference');

  TValue.Make(nil, AType, Result);

  ContextRtti := TRttiContext.Create;
  try
    RttiType := ContextRtti.GetType(AType);
    RouteParams := Context.Request.RouteParams;

    for Field in RttiType.GetFields do
    begin
      // Obter nome do campo (com suporte a atributos [FromRoute])
      var SourceProvider := TBindingSourceProvider.Create;
      try
        FieldName := SourceProvider.GetBindingName(Field);
      finally
        SourceProvider.Free;
      end;

      // Buscar valor do route parameter
      if TryGetCaseInsensitive(RouteParams, FieldName, FieldValue) then
      begin
        // FieldValue já foi preenchido pelo TryGetCaseInsensitive

        // ✅ MESMA CONVERSÃO ROBUSTA DO BINDQUERY
        try
          var Val := ConvertStringToType(FieldValue, Field.FieldType.Handle);
          Field.SetValue(Result.GetReferenceToRawData, Val);
        except
          on E: Exception do
          begin
            Writeln(Format('⚠️ BindRoute warning: Error converting field "%s" value "%s": %s',
              [FieldName, FieldValue, E.Message]));
          end;
        end; // try
      end; // if parameter exists
    end; // for each field

  finally
    ContextRtti.Free;
  end;
end;

//function TModelBinder.BindRoute(AType: PTypeInfo; Context: IHttpContext): TValue;
//var
//  ContextRtti: TRttiContext;
//  RttiType: TRttiType;
//  Field: TRttiField;
//  RouteParams: TDictionary<string, string>;
//  FieldName: string;
//  FieldValue: string;
//begin
//  if AType.Kind <> tkRecord then
//    raise EBindingException.Create('BindRoute currently only supports records');
//
//  TValue.Make(nil, AType, Result);
//
//  ContextRtti := TRttiContext.Create;
//  try
//    RttiType := ContextRtti.GetType(AType);
//    RouteParams := Context.Request.RouteParams;
//
//    // ✅ DEBUG: Antes do loop
//    Writeln('🔍 BindRoute - Processing record type: ', RttiType.Name);
//    Writeln('  RouteParams available: ', RouteParams.Count);
//
//    for Field in RttiType.GetFields do
//    begin
//      // Obter nome do campo (com suporte a atributos [FromRoute])
//      var SourceProvider := TBindingSourceProvider.Create;
//      try
//        FieldName := SourceProvider.GetBindingName(Field);
//      finally
//        SourceProvider.Free;
//      end;
//
//      // ✅ DEBUG: Para cada campo
//      Writeln('🔍 Processing field: ', Field.Name);
//      Writeln('  Looking for RouteParam: ', FieldName);
//      Writeln('  Field type: ', Field.FieldType.Name);
//
//      // Buscar valor do route parameter
//      if RouteParams.ContainsKey(FieldName) then
//      begin
//        FieldValue := RouteParams[FieldName];
//        Writeln('  ✅ Found value: ', FieldValue);
//
//        // ✅ CONVERSÃO (código existente)
//        try
//          case Field.FieldType.TypeKind of
//            tkInteger:
//              begin
//                var IntValue := StrToIntDef(FieldValue, 0);
//                Writeln('  Converting to Integer: ', FieldValue, ' -> ', IntValue);
//                Field.SetValue(Result.GetReferenceToRawData, TValue.From<Integer>(IntValue));
//              end;
//
//            tkInt64:
//              Field.SetValue(Result.GetReferenceToRawData,
//                TValue.From<Int64>(StrToInt64Def(FieldValue, 0)));
//
//            tkFloat:
//              begin
//                if Field.FieldType.Handle = TypeInfo(TDateTime) then
//                  Field.SetValue(Result.GetReferenceToRawData,
//                    TValue.From<TDateTime>(StrToDateTimeDef(FieldValue, 0)))
//                else
//                begin
//                  var FloatValue: Double;
//                  if TryStrToFloat(FieldValue, FloatValue, TFormatSettings.Invariant) then
//                    Field.SetValue(Result.GetReferenceToRawData, TValue.From<Double>(FloatValue))
//                  else
//                    Field.SetValue(Result.GetReferenceToRawData, TValue.From<Double>(0));
//                end;
//              end;
//
//            tkString, tkLString, tkWString, tkUString:
//              Field.SetValue(Result.GetReferenceToRawData,
//                TValue.From<string>(FieldValue));
//
//            tkEnumeration:
//              begin
//                if Field.FieldType.Handle = TypeInfo(Boolean) then
//                begin
//                  var BoolValue := SameText(FieldValue, 'true') or
//                                   SameText(FieldValue, '1') or
//                                   SameText(FieldValue, 'yes') or
//                                   SameText(FieldValue, 'on');
//                  Field.SetValue(Result.GetReferenceToRawData,
//                    TValue.From<Boolean>(BoolValue));
//                end
//                else
//                begin
//                  Field.SetValue(Result.GetReferenceToRawData,
//                    TValue.FromOrdinal(Field.FieldType.Handle,
//                      StrToIntDef(FieldValue, 0)));
//                end;
//              end;
//
//            tkRecord:
//              begin
//                if Field.FieldType.Handle = TypeInfo(TGUID) then
//                begin
//                  var GuidValue: TGUID;
//                  var GuidStr := FieldValue.Trim;
//
//                  try
//                    if GuidStr.StartsWith('{') and GuidStr.EndsWith('}') then
//                      GuidValue := StringToGUID(GuidStr)
//                    else if GuidStr.Length = 36 then
//                      GuidValue := StringToGUID('{' + GuidStr + '}')
//                    else
//                      GuidValue := StringToGUID(GuidStr);
//
//                    Field.SetValue(Result.GetReferenceToRawData,
//                      TValue.From<TGUID>(GuidValue));
//                  except
//                    on E: EConvertError do
//                      Field.SetValue(Result.GetReferenceToRawData,
//                        TValue.From<TGUID>(TGUID.Empty));
//                  end;
//                end;
//              end;
//          end; // case
//        except
//          on E: Exception do
//            Writeln('  ❌ Conversion error: ', E.Message);
//        end;
//      end
//      else
//      begin
//        Writeln('  ❌ RouteParam not found: ', FieldName);
//      end;
//    end;
//
//  finally
//    ContextRtti.Free;
//  end;
//end;

function TModelBinder.BindRoute<T>(Context: IHttpContext): T;
begin
  var Value := BindRoute(TypeInfo(T), Context);
  Result := Value.AsType<T>;
end;

function TModelBinder.BindHeader(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  ContextRtti: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Headers: TDictionary<string, string>;
  FieldName: string;
  FieldValue: string;
begin
  if AType.Kind <> tkRecord then
    raise EBindingException.Create('BindHeader currently only supports records');

  TValue.Make(nil, AType, Result);

  ContextRtti := TRttiContext.Create;
  try
    RttiType := ContextRtti.GetType(AType);
    Headers := Context.Request.Headers;

    for Field in RttiType.GetFields do
    begin
      // Obter nome do campo (com suporte a atributos [FromHeader])
      var SourceProvider := TBindingSourceProvider.Create;
      try
        FieldName := SourceProvider.GetBindingName(Field);
      finally
        SourceProvider.Free;
      end;

      // Buscar valor do header (case-insensitive)
      var HeaderKey := FieldName.ToLower; // Headers são case-insensitive
      if Headers.ContainsKey(HeaderKey) then
      begin
        FieldValue := Headers[HeaderKey];

        // ✅ USAR CONVERSÃO ROBUSTA
        try
          var Val := ConvertStringToType(FieldValue, Field.FieldType.Handle);
          Field.SetValue(Result.GetReferenceToRawData, Val);
        except
          on E: Exception do
          begin
            Writeln(Format('⚠️ BindHeader warning: Error converting field "%s" value "%s": %s',
              [FieldName, FieldValue, E.Message]));
          end;
        end; // try
      end; // if header exists
    end; // for each field

  finally
    ContextRtti.Free;
  end;
end;

function TModelBinder.BindMethodParameters(AMethod: TRttiMethod;
  AContext: IHttpContext): TArray<TValue>;
var
  I: Integer;
  Params: TArray<TRttiParameter>;
begin
  Params := AMethod.GetParameters;
  SetLength(Result, Length(Params));

  for I := 0 to High(Params) do
    Result[I] := BindParameter(Params[I], AContext);
end;

function TModelBinder.BindParameter(AParam: TRttiParameter;
  AContext: IHttpContext): TValue;
var
  Attr: TCustomAttribute;
  ParamName: string;
begin
  // 1. IHttpContext
  if AParam.ParamType.Handle = TypeInfo(IHttpContext) then
  begin
    Result := TValue.From<IHttpContext>(AContext);
    Exit;
  end;

  // 2. Explicit Attributes
  for Attr in AParam.GetAttributes do
  begin
    if Attr is FromQueryAttribute then
    begin
      ParamName := FromQueryAttribute(Attr).Name;
      if ParamName = '' then ParamName := AParam.Name;

      var QueryParams := AContext.Request.Query;
      if QueryParams.IndexOfName(ParamName) >= 0 then
        Result := ConvertStringToType(TNetEncoding.URL.Decode(QueryParams.Values[ParamName]), AParam.ParamType.Handle)
      else
        Result := ConvertStringToType('', AParam.ParamType.Handle); // Default
      Exit;
    end
    else if Attr is FromRouteAttribute then
    begin
      ParamName := FromRouteAttribute(Attr).Name;
      if ParamName = '' then ParamName := AParam.Name;

      var RouteParams := AContext.Request.RouteParams;
      var RouteValue: string;
      if TryGetCaseInsensitive(RouteParams, ParamName, RouteValue) then
        Result := ConvertStringToType(RouteValue, AParam.ParamType.Handle)
      else
        raise EBindingException.CreateFmt('Route parameter not found: %s', [ParamName]);
      Exit;
    end
    else if Attr is FromBodyAttribute then
    begin
      Result := BindBody(AParam.ParamType.Handle, AContext);
      Exit;
    end
    else if Attr is FromServicesAttribute then
    begin
      Result := BindServices(AParam.ParamType.Handle, AContext);
      Exit;
    end
    else if Attr is FromHeaderAttribute then
    begin
      ParamName := FromHeaderAttribute(Attr).Name;
      if ParamName = '' then ParamName := AParam.Name;

      var Headers := AContext.Request.Headers;
      if Headers.ContainsKey(LowerCase(ParamName)) then
        Result := ConvertStringToType(Headers[LowerCase(ParamName)], AParam.ParamType.Handle)
      else
        Result := ConvertStringToType('', AParam.ParamType.Handle); // Default
      Exit;
    end;
  end;

  // 3. Inference
  if (AParam.ParamType.TypeKind = tkRecord) then
  begin
    // Use hybrid binding for records - supports Header, Query, Route, Body
    Result := BindRecordHybrid(AParam.ParamType.Handle, AContext);
  end
  else if (AParam.ParamType.TypeKind = tkClass) then
  begin
     // 1. Try Service Injection First
     var IsService := False;
     try
       Result := BindServices(AParam.ParamType.Handle, AContext);
       if not Result.IsEmpty and not Result.AsObject.Equals(nil) then
         IsService := True;
     except
       // Service resolution failed, continue to Model Binding
     end;

     if IsService then Exit;

     // 2. Model Binding (Body vs Query)
     if (AContext.Request.Method = 'POST') or (AContext.Request.Method = 'PUT') or (AContext.Request.Method = 'PATCH') then
         Result := BindBody(AParam.ParamType.Handle, AContext)
     else
         Result := BindQuery(AParam.ParamType.Handle, AContext);
  end
  else if (AParam.ParamType.TypeKind = tkInterface) then
  begin
    Result := BindServices(AParam.ParamType.Handle, AContext);
  end
  else
  begin
    // Primitives: Route -> Query
    ParamName := AParam.Name;
    var RouteParams := AContext.Request.RouteParams;
    var RouteValue: string;
    
    if TryGetCaseInsensitive(RouteParams, ParamName, RouteValue) then
    begin
      Result := ConvertStringToType(RouteValue, AParam.ParamType.Handle);
    end
    else
    begin
      var QueryParams := AContext.Request.Query;
      if QueryParams.IndexOfName(ParamName) >= 0 then
        Result := ConvertStringToType(TNetEncoding.URL.Decode(QueryParams.Values[ParamName]), AParam.ParamType.Handle)
      else
        Result := ConvertStringToType('', AParam.ParamType.Handle); // Default
    end;
  end;
end;

function TModelBinder.BindServices(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  ContextRtti: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Services: IServiceProvider;
  ServiceInstance: TValue;
  ServiceType: TServiceType;
begin
  if (AType.Kind <> tkRecord) and (AType.Kind <> tkInterface) and (AType.Kind <> tkClass) then
    raise EBindingException.Create('BindServices currently only supports records, classes or interfaces');

  ContextRtti := TRttiContext.Create;
  try
    Services := Context.GetServices;
    
    // Class Support (Root Level)
    if AType.Kind = tkClass then
    begin
       var ClassType := (ContextRtti.GetType(AType) as TRttiInstanceType).MetaclassType;
       ServiceType := TServiceType.FromClass(ClassType);
       var ClassInstance := Services.GetService(ServiceType);
       
       if Assigned(ClassInstance) then
       begin
         Result := TValue.From<TObject>(ClassInstance);
         Exit;
       end
       else
          raise EBindingException.CreateFmt('Service not found for class type: %s. Ensure it is registered in DI.', [String(AType.Name)]);
    end;

    // ✅ NOVO: Suporte direto a interfaces
    if AType.Kind = tkInterface then
    begin
      var InterfaceType := ContextRtti.GetType(AType) as TRttiInterfaceType;
      ServiceType := TServiceType.FromInterface(InterfaceType.GUID);
      var InterfaceInstance := Services.GetServiceAsInterface(ServiceType);
      
      if Assigned(InterfaceInstance) then
      begin
        TValue.Make(@InterfaceInstance, AType, Result);
        Exit;
      end
      else
        raise EBindingException.CreateFmt('Service not found for interface: %s', [InterfaceType.Name]);
    end;

    TValue.Make(nil, AType, Result);
    RttiType := ContextRtti.GetType(AType);
    // Services já inicializado acima

    for Field in RttiType.GetFields do
    begin
      // Verificar se o campo tem atributo [FromServices]
      var HasServicesAttr := False;
      for var Attr in Field.GetAttributes do
      begin
        if Attr is FromServicesAttribute then
        begin
          HasServicesAttr := True;
          Break;
        end;
      end;

      if HasServicesAttr then
      begin
        try
          case Field.FieldType.TypeKind of
            tkInterface:
              begin
                // Para interfaces, usar o GUID
                var InterfaceType := Field.FieldType as TRttiInterfaceType;
                ServiceType := TServiceType.FromInterface(InterfaceType.GUID);

                // Obter serviço do container DI como interface
                var InterfaceInstance := Services.GetServiceAsInterface(ServiceType);
                if Assigned(InterfaceInstance) then
                begin
                  // ✅ CORREÇÃO: Criar TValue do tipo específico da interface
                  TValue.Make(@InterfaceInstance, Field.FieldType.Handle, ServiceInstance);
                  Field.SetValue(Result.GetReferenceToRawData, ServiceInstance);
                end
                else
                begin
                  // Serviço não encontrado - pode ser opcional ou requerido?
                  // Por enquanto, deixamos o campo como nil
                end;
              end;

            tkClass:
              begin
                // ✅ CORREÇÃO: Usar o RTTI para obter a classe corretamente
                var ClassType := (Field.FieldType as TRttiInstanceType).MetaclassType;
                ServiceType := TServiceType.FromClass(ClassType);

                var ClassInstance := Services.GetService(ServiceType);
                if Assigned(ClassInstance) then
                begin
                  ServiceInstance := TValue.From<TObject>(ClassInstance);
                  Field.SetValue(Result.GetReferenceToRawData, ServiceInstance);
                end;
              end;
          else
            raise EBindingException.CreateFmt(
              'FromServices attribute not supported for field type: %s',
              [Field.FieldType.Name]);
          end;
        except
          on E: Exception do
            raise EBindingException.CreateFmt(
              'Error binding service for field %s: %s',
              [Field.Name, E.Message]);
        end;
      end;
    end;
  finally
    ContextRtti.Free;
  end;
end;

function TModelBinder.BindRecordHybrid(AType: PTypeInfo; Context: IHttpContext): TValue;
var
  ContextRtti: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  SourceProvider: TBindingSourceProvider;
  BindingSource: TBindingSource;
  FieldName: string;
  FieldValue: TValue;
  BodyBytes: TBytes;
  Stream: TStream;
  Headers: TDictionary<string, string>;
  RouteParams: TDictionary<string, string>;
  QueryParams: TStrings;
  HeaderVal, RouteVal, QueryVal: string;
  BodyJsonObj: IDextJsonObject;
begin
  if AType.Kind <> tkRecord then
    raise EBindingException.Create('BindRecordHybrid only supports records');

  TValue.Make(nil, AType, Result);
  BodyJsonObj := nil;

  ContextRtti := TRttiContext.Create;
  try
    RttiType := ContextRtti.GetType(AType);
    SourceProvider := TBindingSourceProvider.Create;
    try
      // Get request data sources
      Headers := Context.Request.Headers;
      RouteParams := Context.Request.RouteParams;
      QueryParams := Context.Request.Query;

      // Pre-load body JSON once (if there's a body)
      Stream := Context.Request.Body;
      if (Stream <> nil) and (Stream.Size > 0) then
      begin
        if Stream is TBytesStream then
          BodyBytes := TBytesStream(Stream).Bytes
        else
        begin
          Stream.Position := 0;
          SetLength(BodyBytes, Stream.Size);
          if Stream.Size > 0 then
            Stream.ReadBuffer(BodyBytes[0], Stream.Size);
        end;
        
        var BodyJsonStr := TEncoding.UTF8.GetString(BodyBytes);
        if BodyJsonStr <> '' then
        begin
          try
            var JsonNode := TDextJson.Provider.Parse(BodyJsonStr);
            if (JsonNode <> nil) and (JsonNode.GetNodeType = jntObject) then
              BodyJsonObj := JsonNode as IDextJsonObject;
          except
            // Body is not valid JSON - ignore and continue with other binding sources
            BodyJsonObj := nil;
          end;
        end;
      end;

      for Field in RttiType.GetFields do
      begin
        try
          // 1. Determine binding source from field attributes
          BindingSource := SourceProvider.GetBindingSource(Field);
          FieldName := SourceProvider.GetBindingName(Field);

          // 2. Bind based on source
          case BindingSource of
            bsHeader:
              begin
                // Headers are case-insensitive
                if TryGetCaseInsensitive(Headers, FieldName, HeaderVal) then
                  FieldValue := ConvertStringToType(HeaderVal, Field.FieldType.Handle)
                else
                  FieldValue := ConvertStringToType('', Field.FieldType.Handle);
              end;

            bsQuery:
              begin
                if QueryParams.IndexOfName(FieldName) >= 0 then
                  QueryVal := TNetEncoding.URL.Decode(QueryParams.Values[FieldName])
                else
                  QueryVal := '';
                FieldValue := ConvertStringToType(QueryVal, Field.FieldType.Handle);
              end;

            bsRoute:
              begin
                if TryGetCaseInsensitive(RouteParams, FieldName, RouteVal) then
                  FieldValue := ConvertStringToType(RouteVal, Field.FieldType.Handle)
                else
                  FieldValue := ConvertStringToType('', Field.FieldType.Handle);
              end;

            bsServices:
              begin
                // For services, delegate to BindServices for the specific field
                FieldValue := BindServices(Field.FieldType.Handle, Context);
              end;

            bsBody:
              begin
                var FoundInBody := False;
                
                // First, try to find in JSON body
                if (BodyJsonObj <> nil) then
                begin
                  // Try to find the field (case-insensitive)
                  var JsonFieldName := FieldName;
                  var FieldFound := BodyJsonObj.Contains(JsonFieldName);
                  
                  // Try lowercase if not found
                  if not FieldFound then
                  begin
                    JsonFieldName := LowerCase(FieldName);
                    FieldFound := BodyJsonObj.Contains(JsonFieldName);
                  end;
                  
                  // Try camelCase if not found
                  if not FieldFound then
                  begin
                    if Length(FieldName) > 0 then
                    begin
                      JsonFieldName := LowerCase(FieldName[1]) + Copy(FieldName, 2, MaxInt);
                      FieldFound := BodyJsonObj.Contains(JsonFieldName);
                    end;
                  end;
                  
                  if FieldFound then
                  begin
                    FoundInBody := True;
                    // For complex types, we need to check the field type
                    case Field.FieldType.TypeKind of
                      tkInteger:
                        begin
                          var IntVal := BodyJsonObj.GetInteger(JsonFieldName);
                          FieldValue := TValue.From<Integer>(IntVal);
                        end;
                      tkInt64:
                        begin
                          var Int64Val := BodyJsonObj.GetInt64(JsonFieldName);
                          FieldValue := TValue.From<Int64>(Int64Val);
                        end;
                      tkFloat:
                        begin
                          if (Field.FieldType.Handle = TypeInfo(Currency)) then
                          begin
                            var CurrVal := Currency(BodyJsonObj.GetDouble(JsonFieldName));
                            FieldValue := TValue.From<Currency>(CurrVal);
                          end
                          else if (Field.FieldType.Handle = TypeInfo(TDateTime)) or 
                                  (Field.FieldType.Handle = TypeInfo(TDate)) or 
                                  (Field.FieldType.Handle = TypeInfo(TTime)) then
                          begin                            
                            var DateStr := BodyJsonObj.GetString(JsonFieldName);
                            FieldValue := ConvertStringToType(DateStr, Field.FieldType.Handle);
                          end
                          else
                          begin
                            var FloatVal := BodyJsonObj.GetDouble(JsonFieldName);
                            FieldValue := TValue.From<Double>(FloatVal);
                          end;
                        end;
                      tkEnumeration:
                        begin
                          if Field.FieldType.Handle = TypeInfo(Boolean) then
                          begin
                            var BoolVal := BodyJsonObj.GetBoolean(JsonFieldName);
                            FieldValue := TValue.From<Boolean>(BoolVal);
                          end
                          else
                          begin
                            var EnumStr := BodyJsonObj.GetString(JsonFieldName);
                            FieldValue := ConvertStringToType(EnumStr, Field.FieldType.Handle);
                          end;
                        end;
                    else
                      // String and other types
                      var StrVal := BodyJsonObj.GetString(JsonFieldName);
                      FieldValue := ConvertStringToType(StrVal, Field.FieldType.Handle);
                    end;
                  end;
                end;
                
                // Fallback 1: Try RouteParams (for IDs in URL like /api/products/{id})
                if not FoundInBody then
                begin
                  if TryGetCaseInsensitive(RouteParams, FieldName, RouteVal) then
                  begin
                    FieldValue := ConvertStringToType(RouteVal, Field.FieldType.Handle);
                    FoundInBody := True; // Mark as found
                  end;
                end;
                
                // Fallback 2: Try Query params (for ?param=value)
                if not FoundInBody then
                begin
                  if QueryParams.IndexOfName(FieldName) >= 0 then
                  begin
                    QueryVal := TNetEncoding.URL.Decode(QueryParams.Values[FieldName]);
                    FieldValue := ConvertStringToType(QueryVal, Field.FieldType.Handle);
                  end
                  else
                    FieldValue := ConvertStringToType('', Field.FieldType.Handle);
                end;
              end;

            bsForm:
              begin
                // Form data not implemented yet
                FieldValue := ConvertStringToType('', Field.FieldType.Handle);
              end;
          end;

          // 3. Set field value
          if not FieldValue.IsEmpty then
            Field.SetValue(Result.GetReferenceToRawData, FieldValue);

        except
          on E: Exception do
          begin
            // Silently continue with other fields on binding error
            // Uncomment for debugging: WriteLn(Format('Warning: Error binding field "%s": %s', [Field.Name, E.Message]));
          end;
        end;
      end;
    finally
      SourceProvider.Free;
    end;
  finally
    ContextRtti.Free;
  end;
end;

function TModelBinder.ConvertStringToType(const AValue: string; AType: PTypeInfo): TValue;
begin
  try
    case AType.Kind of
      tkInteger: Result := TValue.From<Integer>(StrToIntDef(AValue, 0));
      tkInt64: Result := TValue.From<Int64>(StrToInt64Def(AValue, 0));

      tkFloat:
        begin
          var Dt: TDateTime;
          if (AType = TypeInfo(TDateTime)) or (AType = TypeInfo(TDate)) or (AType = TypeInfo(TTime)) then
          begin
            if TryParseCommonDate(AValue, Dt) then
              Result := TValue.From<TDateTime>(Dt)
            else
              Result := TValue.From<TDateTime>(0);
          end
          else
          begin
            var F: Double;
            if TryStrToFloat(AValue, F, TFormatSettings.Invariant) then
              Result := TValue.From<Double>(F)
            else if TryParseCommonDate(AValue, Dt) then
              Result := TValue.From<TDateTime>(Dt)
            else
              Result := TValue.From<Double>(0);
          end;
        end;
      tkString, tkLString, tkWString, tkUString: Result := TValue.From<string>(AValue);
      tkEnumeration:
        begin
          if AType = TypeInfo(Boolean) then
          begin
            var B := SameText(AValue, 'true') or SameText(AValue, '1') or SameText(AValue, 'on');
            Result := TValue.From<Boolean>(B);
          end
          else
            Result := TValue.FromOrdinal(AType, StrToIntDef(AValue, 0));
        end;
      tkRecord:
        begin
          // TGUID support
          if AType = TypeInfo(TGUID) then
          begin
            var GuidStr := AValue.Trim;
            if GuidStr = '' then
            begin
              Result := TValue.From<TGUID>(TGUID.Empty);
              Exit;
            end;

            if GuidStr.StartsWith('{') and GuidStr.EndsWith('}') then
              Result := TValue.From<TGUID>(StringToGUID(GuidStr))
            else if GuidStr.Length = 36 then
              Result := TValue.From<TGUID>(StringToGUID('{' + GuidStr + '}'))
            else
              Result := TValue.From<TGUID>(StringToGUID(GuidStr));
          end
          // TUUID support (RFC 9562 compliant)
          else if AType = TypeInfo(TUUID) then
          begin
            var GuidStr := AValue.Trim;
            if GuidStr = '' then
            begin
              Result := TValue.From<TUUID>(TUUID.Null);
              Exit;
            end;

            // TUUID.FromString handles all formats (with/without braces, hyphens)
            var U := TUUID.FromString(GuidStr);
            TValue.Make(@U, TypeInfo(TUUID), Result);
          end
          else
            raise EBindingException.Create('Cannot convert string to Record (except TGUID and TUUID)');
        end;
      else
        // Default for unknown types
        TValue.Make(nil, AType, Result);
    end;
  except
    on E: Exception do
      // Return default on error
      TValue.Make(nil, AType, Result);
  end;
end;

{ TModelBinderHelper }

class function TModelBinderHelper.BindQuery<T>(ABinder: IModelBinder; Context: IHttpContext): T;
begin
  var Value := ABinder.BindQuery(TypeInfo(T), Context);
  Result := Value.AsType<T>;
end;

class function TModelBinderHelper.BindBody<T>(ABinder: IModelBinder; Context: IHttpContext): T;
begin
  var Value := ABinder.BindBody(TypeInfo(T), Context);
  Result := Value.AsType<T>;
end;

class function TModelBinderHelper.BindRoute<T>(ABinder: IModelBinder; Context: IHttpContext): T;
var
  Value: TValue;
  P: Pointer;
begin
  Value := ABinder.BindRoute(TypeInfo(T), Context);
  
  // For records like TUUID/TGUID, AsType<T> fails with "Insufficient RTTI"
  // Always use direct memory copy for records
  if PTypeInfo(TypeInfo(T)).Kind = tkRecord then
  begin
    P := Value.GetReferenceToRawData;
    if P <> nil then
      Move(P^, Result, SizeOf(T))
    else
      Result := Default(T);
  end
  else
    Result := Value.AsType<T>;
end;

{ TBindingSourceProvider }

function TBindingSourceProvider.GetBindingSource(Field: TRttiField): TBindingSource;
var
  Attr: TCustomAttribute;
begin
  for Attr in Field.GetAttributes do
  begin
    if Attr is BindingAttribute then
      Exit(BindingAttribute(Attr).Source);
  end;

  // Default: FromBody for all field types without explicit binding attribute
  // This is the expected behavior for records used in POST/PUT requests
  // Fields that need to come from Query/Route/Header should use explicit attributes
  Result := bsBody;
end;

function TBindingSourceProvider.GetBindingName(Field: TRttiField): string;
var
  Attr: TCustomAttribute;
begin
  for Attr in Field.GetAttributes do
  begin
    if (Attr is FromQueryAttribute) and (FromQueryAttribute(Attr).Name <> '') then
      Exit(FromQueryAttribute(Attr).Name)
    else if (Attr is FromRouteAttribute) and (FromRouteAttribute(Attr).Name <> '') then
      Exit(FromRouteAttribute(Attr).Name)
    else if (Attr is FromHeaderAttribute) and (FromHeaderAttribute(Attr).Name <> '') then
      Exit(FromHeaderAttribute(Attr).Name);
  end;

  Result := Field.Name;
end;

function TBindingSourceProvider.GetBindingSource(Param: TRttiParameter): TBindingSource;
var
  Attr: TCustomAttribute;
begin
  for Attr in Param.GetAttributes do
  begin
    if Attr is BindingAttribute then
      Exit(BindingAttribute(Attr).Source);
  end;

  // Default: FromBody para tipos complexos, FromQuery para simples
  if Param.ParamType.TypeKind in [tkRecord, tkClass] then
    Result := bsBody
  else
    Result := bsQuery;
end;

function TBindingSourceProvider.GetBindingName(Param: TRttiParameter): string;
var
  Attr: TCustomAttribute;
begin
  for Attr in Param.GetAttributes do
  begin
    if (Attr is FromQueryAttribute) and (FromQueryAttribute(Attr).Name <> '') then
      Exit(FromQueryAttribute(Attr).Name)
    else if (Attr is FromRouteAttribute) and (FromRouteAttribute(Attr).Name <> '') then
      Exit(FromRouteAttribute(Attr).Name)
    else if (Attr is FromHeaderAttribute) and (FromHeaderAttribute(Attr).Name <> '') then
      Exit(FromHeaderAttribute(Attr).Name);
  end;

  Result := Param.Name;
end;

end.


