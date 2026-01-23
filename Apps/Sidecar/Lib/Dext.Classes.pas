unit Dext.Classes;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TObjectHelper = class helper for TObject
  public
    class function ObjectName: string;
  end;

  TComponentHelper = class helper for TComponent
  public
    function SetUniqueName: string; overload;
    function SetUniqueName(const Prefix: string; const Sufix: string = ''): string; overload;
    function TrySetName(const Name: string): Boolean;
  end;

implementation

const
  CLASS_PREFIX = 'T';

{ TObjectHelper }

class function TObjectHelper.ObjectName: string;
begin
  Result := ClassName;
  if Result.StartsWith(CLASS_PREFIX, True) then
    Delete(Result, 1, 1);
end;

{ TComponentHelper }

function TComponentHelper.TrySetName(const Name: string): Boolean;
begin
  Result := (Owner = nil) or (Owner.FindComponent(Name) = nil);

  if Result then
  begin
    Self.Name := Name;
  end;
end;

function TComponentHelper.SetUniqueName(const Prefix: string; const Sufix: string = ''): string;
var
  ComponentNamePrefix: string;
  I: Integer;
begin
  if Owner = nil then Exit(Name);

  ComponentNamePrefix := Prefix + Self.ClassType.ObjectName + Sufix;
  for I := 1 to MaxInt do
  begin
    if TrySetName(ComponentNamePrefix + IntToStr(I)) then
      Break;
  end;

  Result := Self.Name;
end;

function TComponentHelper.SetUniqueName: string;
begin
  SetUniqueName('', '');
end;

end.
