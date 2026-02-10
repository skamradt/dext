unit Dext.Entity.Validator;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Classes,
  Dext.Entity.Attributes,
  Dext.Entity.Mapping;

type
  EValidationException = class(Exception);

  TEntityValidator = class
  public
    class procedure Validate(const AEntity: TObject; const AMap: TEntityMap = nil);
  end;

implementation

uses
  System.StrUtils;

{ TEntityValidator }

class procedure TEntityValidator.Validate(const AEntity: TObject; const AMap: TEntityMap);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Val: TValue;
  SVal: string;
  Len: Integer;
  PropMap: TPropertyMap;
  MaxLen, MinLen: Integer;
  IsRequired: Boolean;
  Attr: TCustomAttribute;
begin
  if AEntity = nil then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AEntity.ClassType);
    if Typ = nil then Exit;

    for Prop in Typ.GetProperties do
    begin
      // Skip non-readable properties
      if not Prop.IsReadable then Continue;

      // Get configuration from Map first
      PropMap := nil;
      if (AMap <> nil) then
        AMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      MaxLen := 0;
      MinLen := 0;
      IsRequired := False;
      
      if PropMap <> nil then
      begin
        MaxLen := PropMap.MaxLength;
        MinLen := PropMap.MinLength;
        IsRequired := PropMap.IsRequired;
        
        // If property is ignored or not mapped, skip validation
        if PropMap.IsIgnored then Continue;
      end;
      
      // Fallback/Override from Attributes
      for Attr in Prop.GetAttributes do
      begin
        if Attr is MaxLengthAttribute then MaxLen := MaxLengthAttribute(Attr).Length;
        if Attr is MinLengthAttribute then MinLen := MinLengthAttribute(Attr).Length;
        if Attr is RequiredAttribute then IsRequired := True;
        if Attr is NotMappedAttribute then Continue;
      end;
      
      // Perform Validation
      try
        Val := Prop.GetValue(Pointer(AEntity));
      except
        // Ignore properties that cannot be read (e.g. exceptions in getters)
        Continue;
      end;
      
      if IsRequired then
      begin
        if Val.IsEmpty then
          raise EValidationException.CreateFmt('Property "%s" is required.', [Prop.Name]);
          
        if Val.Kind in [tkString, tkUString, tkWString, tkLString] then
        begin
          if Trim(Val.AsString) = '' then
             raise EValidationException.CreateFmt('Property "%s" is required.', [Prop.Name]);
        end;
        
        // Check for nil objects
        if (Val.Kind = tkClass) and (Val.AsObject = nil) then
           raise EValidationException.CreateFmt('Property "%s" is required.', [Prop.Name]);
      end;
      
      if (Val.Kind in [tkString, tkUString, tkWString, tkLString]) then
      begin
        SVal := Val.AsString;
        Len := SVal.Length;
        
        // Only validate length if value is present (empty strings might be allowed if not Required)
        if Len > 0 then
        begin
          if (MaxLen > 0) and (Len > MaxLen) then
            raise EValidationException.CreateFmt('Property "%s" (Length: %d) exceeds maximum length of %d.', [Prop.Name, Len, MaxLen]);
            
          if (MinLen > 0) and (Len < MinLen) then
             raise EValidationException.CreateFmt('Property "%s" (Length: %d) is shorter than minimum length of %d.', [Prop.Name, Len, MinLen]);
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

end.
