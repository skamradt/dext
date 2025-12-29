unit Auth.Endpoints;

interface

uses
  Dext,
  Dext.Web,
  Auth.Service,
  User, 
  Admin.Utils,
  System.Classes, 
  System.IOUtils, 
  System.SysUtils;

type
  TAuthEndpoints = class
  public
    class procedure Map(App: TDextAppBuilder);
  end;

implementation

uses
  Dext.Json,
  Dext.Web.Interfaces, 
  Dext.Web.HandlerInvoker,
  Dext.Web.Results,
  Auth.Dto;

{ TAuthEndpoints }

class procedure TAuthEndpoints.Map(App: TDextAppBuilder);
begin
  App.MapGet('/auth/login',
    procedure(Context: IHttpContext)
    begin
      Results.HtmlFromFile('login.html').Execute(Context);
    end);

  // POST /auth/login - Handle login
  // Fully injected services + DTO + Standardized IResult return
  App.MapPost<IAuthService, IJwtTokenHandler, TLoginDto, IResult>('/auth/login',
    function(AuthService: IAuthService; JwtHandler: IJwtTokenHandler; Dto: TLoginDto): IResult
    var
      Claims: TArray<TClaim>;
      Token: string;
      User: TUser;
    begin
      User := AuthService.ValidateUser(Dto.Username, Dto.Password);
      
      if User <> nil then
      begin
        // Create claims
        Claims := TClaimsBuilder.Create
          .WithNameIdentifier(IntToStr(User.Id))
          .WithName(User.Username)
          .WithRole(User.Role)
          .Build;
        
        // Generate JWT token
        Token := JwtHandler.GenerateToken(Claims);
        
        // ✅ Return JSON with automatic serialization
        Result := Results.Json(
          TDextJson.Serialize<TLoginResponse>(
            TLoginResponse.Create(Token, User.Username, User.Role, 3600)
          )
        );
      end
      else
      begin
        // ✅ Return error with automatic serialization
        Result := Results.Json(
          TDextJson.Serialize<TErrorResponse>(
            TErrorResponse.Create('Invalid credentials')
          ), 
          401
        );
      end;
    end);
    
  App.MapPost('/auth/logout',
    procedure(Context: IHttpContext)
    begin
      // Token removal happens on frontend (localStorage.clear())
      // ✅ Return JSON with automatic serialization
      Results.Json(
        TDextJson.Serialize<TLogoutResponse>(
          TLogoutResponse.Create('Logged out successfully')
        )
      ).Execute(Context);
    end);
end;

end.
