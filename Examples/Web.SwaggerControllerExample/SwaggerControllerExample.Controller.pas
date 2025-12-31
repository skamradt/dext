unit SwaggerControllerExample.Controller;

{
  Controller demonstrating Swagger/OpenAPI documentation with MVC Controllers.

  Shows how to use:
  - [SwaggerOperation] for endpoint summary and description
  - [SwaggerResponse] for documented response types
  - [SwaggerTag] for grouping endpoints
  - [Authorize] for security documentation
}

interface

uses
  System.SysUtils,
  Dext,
  Dext.Web,
  Dext.Web.Results,
  Dext.Json,
  Dext.OpenAPI.Attributes,
  SwaggerControllerExample.Models;

type
  /// <summary>
  ///   Controller for managing books in the library.
  ///   Demonstrates attribute-based Swagger documentation.
  /// </summary>
  [DextController('/api/books')]
  [SwaggerTag('Books')]
  TBooksController = class
  public
    /// <summary>
    ///   Lists all books in the catalog.
    /// </summary>
    [DextGet('')]
    [AllowAnonymous]
    [SwaggerOperation('List all books', 'Returns a list of all books in the library catalog')]
    [SwaggerResponse(200, 'List of books', 'application/json')]
    procedure GetAll(Ctx: IHttpContext); virtual;

    /// <summary>
    ///   Gets a specific book by ID.
    /// </summary>
    [DextGet('/{id}')]
    [AllowAnonymous]
    [SwaggerOperation('Get book by ID', 'Returns detailed information about a specific book')]
    [SwaggerResponse(200, 'Book found', 'application/json')]
    [SwaggerResponse(404, 'Book not found')]
    procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Integer); virtual;

    /// <summary>
    ///   Creates a new book.
    ///   Note: In production, add [Authorize('bearerAuth')] to require authentication.
    /// </summary>
    [DextPost('')]
    [SwaggerOperation('Create a new book', 'Creates a new book entry')]
    [SwaggerResponse(201, 'Book created', 'application/json')]
    [SwaggerResponse(400, 'Invalid request')]
    procedure Create(Ctx: IHttpContext; const Request: TCreateBookRequest); virtual;

    /// <summary>
    ///   Updates book availability.
    ///   Note: In production, add [Authorize('bearerAuth')] to require authentication.
    /// </summary>
    [DextPatch('/{id}/availability')]
    [SwaggerOperation('Update book availability', 'Updates the availability status of a book')]
    [SwaggerResponse(200, 'Availability updated')]
    [SwaggerResponse(404, 'Book not found')]
    procedure UpdateAvailability(Ctx: IHttpContext; [FromRoute] Id: Integer; const Request: TUpdateAvailabilityRequest); virtual;

    /// <summary>
    ///   Deletes a book.
    ///   Note: In production, add [Authorize('bearerAuth')] to require authentication.
    /// </summary>
    [DextDelete('/{id}')]
    [SwaggerOperation('Delete a book', 'Removes a book from the catalog')]
    [SwaggerResponse(204, 'Book deleted')]
    [SwaggerResponse(404, 'Book not found')]
    procedure Delete(Ctx: IHttpContext; [FromRoute] Id: Integer); virtual;
  end;

  /// <summary>
  ///   System endpoints (health, version).
  /// </summary>
  [DextController('/api')]
  [SwaggerTag('System')]
  TSystemController = class
  public
    [DextGet('/health')]
    [AllowAnonymous]
    [SwaggerOperation('Health check', 'Returns the health status of the API')]
    [SwaggerResponse(200, 'Service is healthy')]
    procedure HealthCheck(Ctx: IHttpContext); virtual;
  end;

  /// <summary>
  ///   Authentication controller for demo purposes.
  /// </summary>
  [DextController('/api/auth')]
  [SwaggerTag('Auth')]
  TAuthController = class
  public
    /// <summary>
    ///   Login endpoint - returns a demo JWT token.
    ///   Use any username/password for demo purposes.
    /// </summary>
    [DextPost('/login')]
    [AllowAnonymous]
    [SwaggerOperation('Login', 'Returns a demo JWT token for testing authenticated endpoints')]
    [SwaggerResponse(200, 'Login successful')]
    [SwaggerResponse(400, 'Invalid credentials')]
    procedure Login(Ctx: IHttpContext); virtual;
  end;

const
  /// <summary>
  ///   Demo JWT token for testing. Valid format, self-signed.
  ///   Warning: This is NOT a real token - for demo only!
  /// </summary>
  DEMO_JWT_TOKEN = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkRlbW8gVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

var
  // In-memory book storage for demonstration
  Books: TArray<TBook>;

implementation

{ TBooksController }

procedure TBooksController.GetAll(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize<TArray<TBook>>(Books));
end;

procedure TBooksController.GetById(Ctx: IHttpContext; Id: Integer);
var
  Book: TBook;
begin
  for Book in Books do
  begin
    if Book.Id = Id then
    begin
      Ctx.Response.Json(TDextJson.Serialize<TBook>(Book));
      Exit;
    end;
  end;

  Ctx.Response.StatusCode := 404;
  Ctx.Response.Json('{"error": "Book not found"}');
end;

procedure TBooksController.Create(Ctx: IHttpContext; const Request: TCreateBookRequest);
var
  NewBook: TBook;
begin
  if (Request.Title = '') or (Request.Author = '') then
  begin
    Ctx.Response.StatusCode := 400;
    Ctx.Response.Json('{"error": "Title and author are required"}');
    Exit;
  end;

  NewBook.Id := Length(Books) + 1;
  NewBook.Title := Request.Title;
  NewBook.Author := Request.Author;
  NewBook.Year := Request.Year;
  NewBook.ISBN := Request.ISBN;
  NewBook.Available := True;

  SetLength(Books, Length(Books) + 1);
  Books[High(Books)] := NewBook;

  Ctx.Response.StatusCode := 201;
  Ctx.Response.Json(TDextJson.Serialize<TBook>(NewBook));
end;

procedure TBooksController.UpdateAvailability(Ctx: IHttpContext; Id: Integer; const Request: TUpdateAvailabilityRequest);
var
  I: Integer;
begin
  for I := 0 to High(Books) do
  begin
    if Books[I].Id = Id then
    begin
      Books[I].Available := Request.Available;
      Ctx.Response.Json(TDextJson.Serialize<TBook>(Books[I]));
      Exit;
    end;
  end;

  Ctx.Response.StatusCode := 404;
  Ctx.Response.Json('{"error": "Book not found"}');
end;

procedure TBooksController.Delete(Ctx: IHttpContext; Id: Integer);
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to High(Books) do
  begin
    if Books[I].Id = Id then
    begin
      if I < High(Books) then
        Books[I] := Books[High(Books)];
      SetLength(Books, Length(Books) - 1);
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
    Ctx.Response.Json('{"error": "Book not found"}');
  end;
end;

{ TSystemController }

procedure TSystemController.HealthCheck(Ctx: IHttpContext);
begin
  Ctx.Response.Json('{"status": "healthy", "version": "1.0.0"}');
end;

{ TAuthController }

procedure TAuthController.Login(Ctx: IHttpContext);
begin
  // Demo login - returns a static token for testing
  // In a real application, you would validate credentials and generate a real JWT
  Ctx.Response.Json(
    '{"token": "' + DEMO_JWT_TOKEN + '", ' +
    '"type": "Bearer", ' +
    '"expiresIn": 3600, ' +
    '"message": "Demo token - use in Authorization header as: Bearer <token>"}'
  );
end;

procedure InitializeSampleData;
begin
  SetLength(Books, 3);

  Books[0].Id := 1;
  Books[0].Title := 'Clean Code';
  Books[0].Author := 'Robert C. Martin';
  Books[0].Year := 2008;
  Books[0].ISBN := '978-0132350884';
  Books[0].Available := True;

  Books[1].Id := 2;
  Books[1].Title := 'The Pragmatic Programmer';
  Books[1].Author := 'David Thomas, Andrew Hunt';
  Books[1].Year := 2019;
  Books[1].ISBN := '978-0135957059';
  Books[1].Available := True;

  Books[2].Id := 3;
  Books[2].Title := 'Design Patterns';
  Books[2].Author := 'Gang of Four';
  Books[2].Year := 1994;
  Books[2].ISBN := '978-0201633610';
  Books[2].Available := False;
end;

initialization
  // Force linker to include controller classes
  TBooksController.ClassName;
  TSystemController.ClassName;
  TAuthController.ClassName;
  
  // Initialize sample data
  InitializeSampleData;

end.
