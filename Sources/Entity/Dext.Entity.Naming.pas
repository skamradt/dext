unit Dext.Entity.Naming;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Character;

type
  /// <summary>
  ///   Defines a strategy for resolving database identifiers from code identifiers.
  /// </summary>
  INamingStrategy = interface
    ['{40000000-0000-0000-0000-000000000001}']
    function GetTableName(const AClass: TClass): string;
    function GetColumnName(const AProperty: TRttiProperty): string;
  end;

  /// <summary>
  ///   Default strategy: Uses class/property names as-is (PascalCase usually).
  ///   TUser -> TUser
  ///   UserName -> UserName
  /// </summary>
  TDefaultNamingStrategy = class(TInterfacedObject, INamingStrategy)
  public
    function GetTableName(const AClass: TClass): string; virtual;
    function GetColumnName(const AProperty: TRttiProperty): string; virtual;
  end;

  /// <summary>
  ///   SnakeCase strategy: Converts PascalCase to snake_case.
  ///   TUser -> user
  ///   UserName -> user_name
  /// </summary>
  TSnakeCaseNamingStrategy = class(TInterfacedObject, INamingStrategy)
  public
    function GetTableName(const AClass: TClass): string; virtual;
    function GetColumnName(const AProperty: TRttiProperty): string; virtual;
  protected
    function ToSnakeCase(const AName: string): string;
  end;

  /// <summary>
  ///   LowerCase strategy: Converts everything to lowercase.
  ///   TUser -> tuser (or user if T prefix removed)
  ///   UserName -> username
  /// </summary>
  TLowerCaseNamingStrategy = class(TInterfacedObject, INamingStrategy)
  public
    function GetTableName(const AClass: TClass): string; virtual;
    function GetColumnName(const AProperty: TRttiProperty): string; virtual;
  end;

  /// <summary>
  ///   UpperCase strategy: Converts everything to uppercase.
  ///   TUser -> TUSER
  ///   UserName -> USERNAME
  /// </summary>
  TUppercaseNamingStrategy = class(TInterfacedObject, INamingStrategy)
  public
    function GetTableName(const AClass: TClass): string; virtual;
    function GetColumnName(const AProperty: TRttiProperty): string; virtual;
  end;

implementation

{ TDefaultNamingStrategy }

function TDefaultNamingStrategy.GetTableName(const AClass: TClass): string;
begin
  Result := AClass.ClassName;
  // Remove 'T' prefix if present and length > 1
  if (Result.Length > 1) and (Result.Chars[0] = 'T') and IsUpper(Result.Chars[1]) then
    Result := Result.Substring(1);
end;

function TDefaultNamingStrategy.GetColumnName(const AProperty: TRttiProperty): string;
begin
  Result := AProperty.Name;
end;

{ TSnakeCaseNamingStrategy }

function TSnakeCaseNamingStrategy.GetTableName(const AClass: TClass): string;
var
  Name: string;
begin
  Name := AClass.ClassName;
  // Remove 'T' prefix
  if (Name.Length > 1) and (Name.Chars[0] = 'T') and IsUpper(Name.Chars[1]) then
    Name := Name.Substring(1);
    
  Result := ToSnakeCase(Name);
end;

function TSnakeCaseNamingStrategy.GetColumnName(const AProperty: TRttiProperty): string;
begin
  Result := ToSnakeCase(AProperty.Name);
end;

function TSnakeCaseNamingStrategy.ToSnakeCase(const AName: string): string;
var
  SB: TStringBuilder;
  C: Char;
  i: Integer;
begin
  if AName.IsEmpty then Exit('');
  
  SB := TStringBuilder.Create;
  try
    for i := 0 to AName.Length - 1 do
    begin
      C := AName.Chars[i];
      if IsUpper(C) then
      begin
        if i > 0 then
          SB.Append('_');
        SB.Append(ToLower(C));
      end
      else
        SB.Append(C);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TLowerCaseNamingStrategy }

function TLowerCaseNamingStrategy.GetTableName(const AClass: TClass): string;
var
  Name: string;
begin
  Name := AClass.ClassName;
  if (Name.Length > 1) and (Name.Chars[0] = 'T') and IsUpper(Name.Chars[1]) then
    Name := Name.Substring(1);
  Result := Name.ToLower;
end;

function TLowerCaseNamingStrategy.GetColumnName(const AProperty: TRttiProperty): string;
begin
  Result := AProperty.Name.ToLower;
end;

{ TUppercaseNamingStrategy }

function TUppercaseNamingStrategy.GetTableName(const AClass: TClass): string;
var
  Name: string;
begin
  Name := AClass.ClassName;
  if (Name.Length > 1) and (Name.Chars[0] = 'T') and IsUpper(Name.Chars[1]) then
    Name := Name.Substring(1);
  Result := Name.ToUpper;
end;

function TUppercaseNamingStrategy.GetColumnName(const AProperty: TRttiProperty): string;
begin
  Result := AProperty.Name.ToUpper;
end;

end.
