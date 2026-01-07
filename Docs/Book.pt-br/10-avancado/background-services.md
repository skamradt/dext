# Background Services

Execute tarefas de longa duração em segundo plano com `IHostedService`.

## Criando um Serviço de Background

```pascal
type
  TCleanupService = class(TInterfacedObject, IHostedService)
  private
    FLogger: ILogger;
    FCancelled: Boolean;
  public
    constructor Create(Logger: ILogger);
    procedure StartAsync(CancellationToken: ICancellationToken);
    procedure StopAsync(CancellationToken: ICancellationToken);
  end;

procedure TCleanupService.StartAsync(CancellationToken: ICancellationToken);
begin
  FLogger.Info('Serviço de limpeza iniciando...');
  
  TAsyncTask.Run(procedure
    begin
      while not FCancelled do
      begin
        LimparArquivosTemporarios;
        Sleep(60 * 60 * 1000); // 1 hora
      end;
    end);
end;

procedure TCleanupService.StopAsync(CancellationToken: ICancellationToken);
begin
  FLogger.Info('Serviço de limpeza parando...');
  FCancelled := True;
end;
```

## Registro

```pascal
Services.AddHostedService<TCleanupService>;
```

## Usando Serviços Scoped em Background

Para usar serviços scoped (como DbContext) dentro de um serviço de background, você deve criar um escopo manualmente:

```pascal
procedure TCleanupService.ExecutarLimpeza;
var
  ScopeFactory: IServiceScopeFactory;
  Scope: IServiceScope;
  DbContext: TAppDbContext;
begin
  ScopeFactory := FServiceProvider.GetRequiredService<IServiceScopeFactory>;
  Scope := ScopeFactory.CreateScope;
  try
    DbContext := Scope.ServiceProvider.GetRequiredService<TAppDbContext>;
    // Use o DbContext...
  finally
    Scope.Free; // Isso libera o DbContext e outros serviços scoped
  end;
end;
```

---

[← Injeção de Dependência](injecao-dependencia.md) | [Próximo: Configuração →](configuracao.md)
