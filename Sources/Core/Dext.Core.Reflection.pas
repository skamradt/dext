unit Dext.Core.Reflection;

interface

uses
  System.Rtti;

type
  TCustomAttributeClass = class of TCustomAttribute;

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttribute<T: TCustomAttribute>: T; overload;
    function GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute; overload;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute(AClass: TCustomAttributeClass): Boolean; overload;
  end;

  TRttiMemberHelper = class helper for TRttiMember
  public
    function GetAttribute<T: TCustomAttribute>: T; overload;
    function GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute; overload;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute(AClass: TCustomAttributeClass): Boolean; overload;
  end;

  TRttiTypeHelper = class helper for TRttiType
  public
    function GetAttribute<T: TCustomAttribute>: T; overload;
    function GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute; overload;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute(AClass: TCustomAttributeClass): Boolean; overload;
  end;

implementation

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttribute<T>: T;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr is T then
      Exit(T(Attr));
  end;
end;

function TRttiObjectHelper.GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr.InheritsFrom(AClass) then
      Exit(Attr);
  end;
end;

function TRttiObjectHelper.HasAttribute<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil;
end;

function TRttiObjectHelper.HasAttribute(AClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AClass) <> nil;
end;

{ TRttiMemberHelper }

function TRttiMemberHelper.GetAttribute<T>: T;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr is T then
      Exit(T(Attr));
  end;
end;

function TRttiMemberHelper.GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr.InheritsFrom(AClass) then
      Exit(Attr);
  end;
end;

function TRttiMemberHelper.HasAttribute<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil;
end;

function TRttiMemberHelper.HasAttribute(AClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AClass) <> nil;
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.GetAttribute<T>: T;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr is T then
      Exit(T(Attr));
  end;
end;

function TRttiTypeHelper.GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr.InheritsFrom(AClass) then
      Exit(Attr);
  end;
end;

function TRttiTypeHelper.HasAttribute<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil;
end;

function TRttiTypeHelper.HasAttribute(AClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AClass) <> nil;
end;

end.
