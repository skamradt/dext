unit Auth.Dto;

interface

type
  TLoginDto = record
    Username: string;
    Password: string;
  end;

  TLoginResponse = record
    Token: string;
    Username: string;
    Role: string;
    ExpiresIn: Integer;
    class function Create(const AToken, AUsername, ARole: string; AExpiresIn: Integer): TLoginResponse; static;
  end;

  TLogoutResponse = record
    Message: string;
    class function Create(const AMessage: string): TLogoutResponse; static;
  end;

  TErrorResponse = record
    Error: string;
    class function Create(const AError: string): TErrorResponse; static;
  end;

implementation

{ TLoginResponse }

class function TLoginResponse.Create(const AToken, AUsername, ARole: string; AExpiresIn: Integer): TLoginResponse;
begin
  Result.Token := AToken;
  Result.Username := AUsername;
  Result.Role := ARole;
  Result.ExpiresIn := AExpiresIn;
end;

{ TLogoutResponse }

class function TLogoutResponse.Create(const AMessage: string): TLogoutResponse;
begin
  Result.Message := AMessage;
end;

{ TErrorResponse }

class function TErrorResponse.Create(const AError: string): TErrorResponse;
begin
  Result.Error := AError;
end;

end.
