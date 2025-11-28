unit Dext.Specifications.Criteria;

interface

uses
  System.SysUtils,
  System.Rtti,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types;

/// <summary>
///   Global helper to create a property expression.
/// </summary>
function Prop(const AName: string): TProp;

implementation

function Prop(const AName: string): TProp;
begin
  Result := TProp.Create(AName);
end;

end.
