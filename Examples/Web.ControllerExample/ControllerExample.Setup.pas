unit ControllerExample.Setup;

interface

uses
  Dext.Web.Interfaces;

/// <summary>
///   Ensures appsettings.json exists, creating a default one if needed.
/// </summary>
procedure EnsureAppSettingsExists;

/// <summary>
///   Registers versioned API endpoints (/api/versioned).
/// </summary>
procedure RegisterVersionedRoutes(Builder: IApplicationBuilder);

/// <summary>
///   Prints feature test instructions to console.
/// </summary>
procedure PrintFeatureInstructions;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Dext.Web,
  Dext.Web.Extensions;

procedure EnsureAppSettingsExists;
begin
  // Always overwrite to ensure correct configuration for this example
  var JsonContent := 
    '{' + sLineBreak +
    '  "AppSettings": {' + sLineBreak +
    '    "Message": "Hello from Dext Configuration!",' + sLineBreak +
    '    "SecretKey": "my-super-secret-key-12345",' + sLineBreak +
    '    "MaxRetries": 3' + sLineBreak +
    '  },' + sLineBreak +
    '  "Server": {' + sLineBreak +
    '    "UseHttps": false' + sLineBreak +
    '  }' + sLineBreak +
    '}';
  TFile.WriteAllText('appsettings.json', JsonContent);
  WriteLn('📝 AppSettings configuration updated.');
end;

procedure RegisterVersionedRoutes(Builder: IApplicationBuilder);
begin
  // V1
  Builder.MapGet('/api/versioned', 
    procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Json('{"version": "1.0", "message": "This is API v1"}');
    end);
  TWebRouteHelpers.HasApiVersion(Builder, '1.0');
    
  // V2
  Builder.MapGet('/api/versioned', 
    procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Json('{"version": "2.0", "message": "This is API v2 - Newer and Better!"}');
    end);
  TWebRouteHelpers.HasApiVersion(Builder, '2.0');
end;

procedure PrintFeatureInstructions;
begin
  WriteLn('');
  WriteLn('📋 Feature Test Instructions:');
  WriteLn('---------------------------------------------------------');
  WriteLn('1. Content Negotiation (defaults to JSON):');
  WriteLn('   curl -H "Accept: application/json" http://localhost:8080/api/greet/negotiated');
  WriteLn('');
  WriteLn('2. API Versioning (Query String):');
  WriteLn('   v1: curl "http://localhost:8080/api/versioned?api-version=1.0"');
  WriteLn('   v2: curl "http://localhost:8080/api/versioned?api-version=2.0"');
  WriteLn('');
  WriteLn('3. API Versioning (Header):');
  WriteLn('   v1: curl -H "X-Version: 1.0" http://localhost:8080/api/versioned');
  WriteLn('   v2: curl -H "X-Version: 2.0" http://localhost:8080/api/versioned');
  WriteLn('---------------------------------------------------------');
end;

end.
