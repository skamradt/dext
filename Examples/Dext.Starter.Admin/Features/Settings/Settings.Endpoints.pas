unit Settings.Endpoints;

interface

uses
  Dext.Web,
  System.Classes,
  System.NetEncoding,
  System.IOUtils,
  System.SysUtils;

type
  TSettingsEndpoints = class
  public
    class procedure Map(App: TDextAppBuilder);
  private
    class function GenerateSuccessNotification(const Message: string): string;
    class function GenerateErrorNotification(const Message: string): string;
  private
    class function CheckAuth(Context: IHttpContext): Boolean;
  end;

implementation

uses
  AppResponseConsts;

function GetFilePath(const RelativePath: string): string;
begin
  // Executable runs from Output\ directory, files are in Dext.Starter.Admin\
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + '..\Dext.Starter.Admin\' + RelativePath;
end;

{ TSettingsEndpoints }

class function TSettingsEndpoints.CheckAuth(Context: IHttpContext): Boolean;
begin
  Result := (Context.User <> nil) and 
            (Context.User.Identity <> nil) and 
            (Context.User.Identity.IsAuthenticated);
            
  if not Result then
    Context.Response.StatusCode := 401;
end;

class function TSettingsEndpoints.GenerateSuccessNotification(const Message: string): string;
begin
  Result := Format(HTML_NOTIFICATION_SUCCESS, [Message]);
end;

class function TSettingsEndpoints.GenerateErrorNotification(const Message: string): string;
begin
  Result := Format(HTML_NOTIFICATION_ERROR, [Message]);
end;

class procedure TSettingsEndpoints.Map(App: TDextAppBuilder);
begin
  // GET /settings - Return settings page
  App.MapGet('/settings',
    procedure(Context: IHttpContext)
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      var Html := TFile.ReadAllText(GetFilePath('wwwroot\views\settings.html'));
      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
    end);

  // POST /settings/profile - Update profile
  App.MapPost('/settings/profile',
    procedure(Context: IHttpContext)
    var
      Notification: string;
      SettingsHtml: string;
      Body, Name, Email: string;
      Reader: TStreamReader;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      // Read body stream logic... (Should rely on Form binding ideally, but keeping manual for now or checking if Dext does it)
      // Dext 2.0 should handle form binding, but to be safe and consistent with previous code structure:
      
      if Context.Request.Body <> nil then
      begin
        Context.Request.Body.Position := 0;
        Reader := TStreamReader.Create(Context.Request.Body, TEncoding.UTF8, False);
        try
          Body := Reader.ReadToEnd;
        finally
          Reader.Free;
        end;
      end
      else
        Body := '';
      
      // Parse URL-encoded form data
      Name := '';
      Email := '';
      
      var Params := Body.Split(['&']);
      for var Param in Params do
      begin
        var Parts := Param.Split(['=']);
        if Length(Parts) = 2 then
        begin
          var Key := TNetEncoding.URL.Decode(Parts[0]);
          var Value := TNetEncoding.URL.Decode(Parts[1]);
          
          if SameText(Key, 'name') then Name := Value
          else if SameText(Key, 'email') then Email := Value;
        end;
      end;
      
      // Logic...
      Notification := GenerateSuccessNotification(Format('Profile updated for %s (%s)!', [Name, Email]));
      SettingsHtml := TFile.ReadAllText(GetFilePath('wwwroot\views\settings.html'));
      
      SettingsHtml := SettingsHtml.Replace('value="Admin User"', Format('value="%s"', [Name]));
      SettingsHtml := SettingsHtml.Replace('value="admin@dext.com"', Format('value="%s"', [Email]));
      
      var Html := Notification + SettingsHtml;
      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
    end);

  // POST /settings/password - Change password
  App.MapPost('/settings/password',
    procedure(Context: IHttpContext)
    var
      Notification: string;
      SettingsHtml: string;
      Body, CurrentPassword, NewPassword, ConfirmPassword: string;
      Reader: TStreamReader;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      if Context.Request.Body <> nil then
      begin
        Context.Request.Body.Position := 0;
        Reader := TStreamReader.Create(Context.Request.Body, TEncoding.UTF8, False);
        try
          Body := Reader.ReadToEnd;
        finally
          Reader.Free;
        end;
      end
      else
        Body := '';
      
      // Parse URL-encoded form data
      CurrentPassword := '';
      NewPassword := '';
      ConfirmPassword := '';
      
      var Params := Body.Split(['&']);
      for var Param in Params do
      begin
        var Parts := Param.Split(['=']);
        if Length(Parts) = 2 then
        begin
          var Key := TNetEncoding.URL.Decode(Parts[0]);
          var Value := TNetEncoding.URL.Decode(Parts[1]);
          
          if SameText(Key, 'current_password') then CurrentPassword := Value
          else if SameText(Key, 'new_password') then NewPassword := Value
          else if SameText(Key, 'confirm_password') then ConfirmPassword := Value;
        end;
      end;
      
      if NewPassword <> ConfirmPassword then
        Notification := GenerateErrorNotification('New passwords do not match')
      else if Length(NewPassword) < 6 then
        Notification := GenerateErrorNotification('Password must be at least 6 characters')
      else
        Notification := GenerateSuccessNotification('Password updated successfully!');
      
      SettingsHtml := TFile.ReadAllText(GetFilePath('wwwroot\views\settings.html'));
      
      var Html := Notification + SettingsHtml;
      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
    end);
end;

end.
