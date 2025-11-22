program SwaggerExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.DI.Interfaces,
  Dext.DI.Extensions,
  Dext.Http.Interfaces,
  Dext.WebHost,
  Dext.Core.ApplicationBuilder.Extensions,
  Dext.Swagger.Middleware,
  Dext.OpenAPI.Extensions,
  Dext.OpenAPI.Generator,
  Dext.Http.Results,
  Dext.Json;

type
  TUser = record
    Id: Integer;
    Name: string;
    Email: string;
  end;

  TCreateUserRequest = record
    Name: string;
    Email: string;
    Password: string;
  end;

  TProduct = record
    Id: Integer;
    Name: string;
    Price: Double;
    InStock: Boolean;
  end;

var
  Users: TArray<TUser>;

procedure InitializeSampleData;
begin
  SetLength(Users, 2);
  Users[0].Id := 1;
  Users[0].Name := 'John Doe';
  Users[0].Email := 'john@example.com';
  
  Users[1].Id := 2;
  Users[1].Name := 'Jane Smith';
  Users[1].Email := 'jane@example.com';
end;

begin
  try
    Writeln('🚀 Starting Dext Swagger Example...');
    Writeln('');
    
    InitializeSampleData;
    
    var Options := TOpenAPIOptions.Default;
    Options.Title := 'Dext Example API';
    Options.Description := 'A sample API demonstrating Dext Framework with Swagger/OpenAPI integration';
    Options.Version := '1.0.0';
    Options.ServerUrl := 'http://localhost:8080';
    Options.ServerDescription := 'Development server';
    Options.ContactName := 'Dext Team';
    Options.ContactEmail := 'contact@dext.dev';
    Options.LicenseName := 'MIT';
    Options.LicenseUrl := 'https://opensource.org/licenses/MIT';
    
    var Host := TDextWebHost.CreateDefaultBuilder
      .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        Writeln('📚 Configuring services...');
        // Register IApplicationBuilder in services for Swagger middleware
        // This will be done automatically by the framework
      end)
      .Configure(procedure(App: IApplicationBuilder)
      begin
        Writeln('📚 Configuring Swagger...');
        
        // Add Swagger middleware
        TSwaggerExtensions.UseSwagger(App, Options);
        
        Writeln('📚 Configuring routes...');
        Writeln('');
        
        // ========================================
        // User Endpoints
        // ========================================
        
        // GET /api/users - Get all users
        Writeln('1. GET /api/users');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapGet<IHttpContext>(
            App,
            '/api/users',
            procedure(Ctx: IHttpContext)
            var
              UsersJson: string;
            begin
              UsersJson := TDextJson.Serialize<TArray<TUser>>(Users);
              Ctx.Response.Json(UsersJson);
            end
          ),
          'Get all users',
          'Retrieves a list of all registered users in the system',
          ['Users']
        );
        
        // GET /api/users/{id} - Get user by ID
        Writeln('2. GET /api/users/{id}');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapGet<Integer, IHttpContext>(
            App,
            '/api/users/{id}',
            procedure(UserId: Integer; Ctx: IHttpContext)
            var
              User: TUser;
              Found: Boolean;
            begin
              Found := False;
              for User in Users do
              begin
                if User.Id = UserId then
                begin
                  Ctx.Response.Json(TDextJson.Serialize<TUser>(User));
                  Found := True;
                  Break;
                end;
              end;
              
              if not Found then
              begin
                Ctx.Response.StatusCode := 404;
                Ctx.Response.Json('{"error": "User not found"}');
              end;
            end
          ),
          'Get user by ID',
          'Retrieves detailed information about a specific user by their unique identifier. Returns 404 if the user is not found.',
          ['Users']
        );
        
        // POST /api/users - Create new user
        Writeln('3. POST /api/users');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapPost<TCreateUserRequest, IHttpContext>(
            App,
            '/api/users',
            procedure(Req: TCreateUserRequest; Ctx: IHttpContext)
            var
              NewUser: TUser;
            begin
              // Validation
              if (Req.Name = '') or (Req.Email = '') or (Req.Password = '') then
              begin
                Ctx.Response.StatusCode := 400;
                Ctx.Response.Json('{"error": "Name, email, and password are required"}');
                Exit;
              end;
              
              // Create new user
              NewUser.Id := Length(Users) + 1;
              NewUser.Name := Req.Name;
              NewUser.Email := Req.Email;
              
              SetLength(Users, Length(Users) + 1);
              Users[High(Users)] := NewUser;
              
              Ctx.Response.StatusCode := 201;
              Ctx.Response.Json(TDextJson.Serialize<TUser>(NewUser));
            end
          ),
          'Create a new user',
          'Creates a new user account with the provided information. Returns the created user with assigned ID.',
          ['Users']
        );

        // DELETE /api/users/{id} - Delete user
        Writeln('4. DELETE /api/users/{id}');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapDelete<Integer, IHttpContext>(
            App,
            '/api/users/{id}',
            procedure(UserId: Integer; Ctx: IHttpContext)
            var
              I: Integer;
              Found: Boolean;
            begin
              Found := False;
              for I := 0 to High(Users) do
              begin
                if Users[I].Id = UserId then
                begin
                  // Remove user (simple implementation)
                  if I < High(Users) then
                    Users[I] := Users[High(Users)];
                  SetLength(Users, Length(Users) - 1);
                  Found := True;
                  Break;
                end;
              end;

              if Found then
              begin
                Ctx.Response.StatusCode := 204;
                Ctx.Response.Write('');
              end
              else
              begin
                Ctx.Response.StatusCode := 404;
                Ctx.Response.Json('{"error": "User not found"}');
              end;
            end
          ),
          'Delete user',
          'Deletes a user from the system. Returns 204 on success, 404 if user not found.',
          ['Users']
        );

        // ========================================
        // Product Endpoints
        // ========================================

        // GET /api/products - Get all products
        Writeln('5. GET /api/products');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapGet<IHttpContext>(
            App,
            '/api/products',
            procedure(Ctx: IHttpContext)
            var
              Products: TArray<TProduct>;
            begin
              SetLength(Products, 2);
              Products[0].Id := 1;
              Products[0].Name := 'Laptop';
              Products[0].Price := 999.99;
              Products[0].InStock := True;

              Products[1].Id := 2;
              Products[1].Name := 'Mouse';
              Products[1].Price := 29.99;
              Products[1].InStock := False;

              Ctx.Response.Json(TDextJson.Serialize<TArray<TProduct>>(Products));
            end
          ),
          'Get all products',
          'Retrieves a list of all available products in the catalog',
          ['Products']
        );

        // ========================================
        // Health Check
        // ========================================

        Writeln('6. GET /health');
        TEndpointMetadataExtensions.WithMetadata(
          TApplicationBuilderExtensions.MapGet<IHttpContext>(
            App,
            '/health',
            procedure(Ctx: IHttpContext)
            begin
              Ctx.Response.Json('{"status": "healthy", "timestamp": "' + DateTimeToStr(Now) + '"}');
            end
          ),
          'Health check',
          'Returns the health status of the API',
          ['System']
        );
      end)
      .Build;

    Writeln('');
    Writeln('✅ Server configured successfully!');
    Writeln('');
    Writeln('📖 Swagger UI available at: http://localhost:8080/swagger');
    Writeln('📄 OpenAPI JSON available at: http://localhost:8080/swagger.json');
    Writeln('');
    Writeln('🔗 Available endpoints:');
    Writeln('   GET    /api/users');
    Writeln('   GET    /api/users/{id}');
    Writeln('   POST   /api/users');
    Writeln('   DELETE /api/users/{id}');
    Writeln('   GET    /api/products');
    Writeln('   GET    /health');
    Writeln('');
    Writeln('Press Enter to stop the server...');
    Writeln('');

    Host.Run;
    Readln;
    Host.Stop;

    Writeln('');
    Writeln('Server stopped successfully');

  except
    on E: Exception do
    begin
      Writeln('❌ Error: ', E.Message);
      Writeln('Press Enter to exit...');
      Readln;
    end;
  end;
end.

