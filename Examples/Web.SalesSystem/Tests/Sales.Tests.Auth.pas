unit Sales.Tests.Auth;

{***************************************************************************}
{                                                                           }
{           Sales System - Authentication Unit Tests                        }
{                                                                           }
{           Tests TAuthService logic without HTTP dependencies              }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing;

type
  /// <summary>
  ///   Unit tests for TAuthService.
  ///   Tests authentication logic in isolation, without HTTP layer.
  /// </summary>
  [TestFixture]
  TAuthServiceTests = class
  private
    FAuthService: IInterface;  // Will hold TAuthService via IAuthService
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Login_Should_Return_Token_For_Valid_Credentials;

    [Test]
    procedure Login_Should_Return_Empty_For_Invalid_Username;

    [Test]
    procedure Login_Should_Return_Empty_For_Invalid_Password;

    [Test]
    procedure Login_Should_Return_Empty_For_Empty_Credentials;

    [Test]
    procedure Token_Should_Be_Valid_JWT_Format;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  Sales.Auth;

{ TAuthServiceTests }

procedure TAuthServiceTests.Setup;
begin
  // Create auth service with test secret
  FAuthService := TAuthService.Create(TAuthConfig.JWT_SECRET);
end;

procedure TAuthServiceTests.TearDown;
begin
  FAuthService := nil;
end;

function GetAuthService(const AService: IInterface): IAuthService;
begin
  if not Supports(AService, IAuthService, Result) then
    raise Exception.Create('Failed to get IAuthService');
end;

procedure TAuthServiceTests.Login_Should_Return_Token_For_Valid_Credentials;
var
  Token: string;
begin
  // Act
  Token := GetAuthService(FAuthService).Login('admin', 'admin');

  // Assert
  Should(Token).NotBe('')
    .Because('Valid credentials should return a token');
end;

procedure TAuthServiceTests.Login_Should_Return_Empty_For_Invalid_Username;
var
  Token: string;
begin
  // Act
  Token := GetAuthService(FAuthService).Login('wronguser', 'admin');

  // Assert
  Should(Token).Be('')
    .Because('Invalid username should not return a token');
end;

procedure TAuthServiceTests.Login_Should_Return_Empty_For_Invalid_Password;
var
  Token: string;
begin
  // Act
  Token := GetAuthService(FAuthService).Login('admin', 'wrongpass');

  // Assert
  Should(Token).Be('')
    .Because('Invalid password should not return a token');
end;

procedure TAuthServiceTests.Login_Should_Return_Empty_For_Empty_Credentials;
var
  Token: string;
begin
  // Act
  Token := GetAuthService(FAuthService).Login('', '');

  // Assert
  Should(Token).Be('')
    .Because('Empty credentials should not return a token');
end;

procedure TAuthServiceTests.Token_Should_Be_Valid_JWT_Format;
var
  Token: string;
  Parts: TArray<string>;
begin
  // Act
  Token := GetAuthService(FAuthService).Login('admin', 'admin');

  // Assert - JWT has 3 parts separated by dots
  Parts := Token.Split(['.']);
  Should(Length(Parts)).Be(3)
    .Because('JWT token should have 3 parts: header.payload.signature');

  // Each part should not be empty
  Should(Parts[0]).NotBe('').Because('Header should not be empty');
  Should(Parts[1]).NotBe('').Because('Payload should not be empty');
  Should(Parts[2]).NotBe('').Because('Signature should not be empty');
end;

end.
