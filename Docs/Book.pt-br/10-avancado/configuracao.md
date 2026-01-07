# Configuração

Carregue e use configurações da aplicação usando `IConfiguration` e `IOptions<T>`.

## Carregando Configurações

### A partir de JSON

```pascal
var
  Config: IConfiguration;
begin
  Config := TConfigurationBuilder.Create
    .AddJsonFile('appsettings.json')
    .AddEnvironmentVariables
    .Build;
end;
```

### appsettings.json

```json
{
  "Database": {
    "Provider": "PostgreSQL",
    "ConnectionString": "Server=localhost;Database=myapp"
  },
  "Jwt": {
    "SecretKey": "chave-secreta-muito-longa-e-segura",
    "ExpirationMinutes": 60
  }
}
```

## Lendo Valores

```pascal
var ConnStr := Config.GetValue('Database:ConnectionString');
var Expira := Config.GetValue<Integer>('Jwt:ExpirationMinutes', 60);
```

## Padrão Options (Options Pattern)

Mapeie seções de configuração para classes fortemente tipadas:

### 1. Definir Classe de Opções

```pascal
type
  TJwtOptions = class
  public
    SecretKey: string;
    ExpirationMinutes: Integer;
  end;
```

### 2. Registrar

```pascal
Services.Configure<TJwtOptions>(Config.GetSection('Jwt'));
```

### 3. Injetar e Usar

```pascal
constructor TAuthService.Create(Options: IOptions<TJwtOptions>);
begin
  FMinutos := Options.Value.ExpirationMinutes;
end;
```

---

[← Background Services](background-services.md) | [Próximo: API Assíncrona →](async-api.md)
