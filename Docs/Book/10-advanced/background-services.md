# Background Services

Run long-running tasks in the background with `IHostedService`.

## Creating a Background Service

```pascal
type
  TCleanupService = class(TInterfacedObject, IHostedService)
  private
    FTimer: TTimer;
    FLogger: ILogger;
    FCancelled: Boolean;
  public
    constructor Create(Logger: ILogger);
    destructor Destroy; override;
    procedure StartAsync(CancellationToken: ICancellationToken);
    procedure StopAsync(CancellationToken: ICancellationToken);
  end;

constructor TCleanupService.Create(Logger: ILogger);
begin
  FLogger := Logger;
  FCancelled := False;
end;

procedure TCleanupService.StartAsync(CancellationToken: ICancellationToken);
begin
  FLogger.Info('Cleanup service starting...');
  
  // Run cleanup every hour
  TAsyncTask.Run(procedure
    begin
      while not FCancelled do
      begin
        try
          DoCleanup;
          FLogger.Info('Cleanup completed');
        except
          on E: Exception do
            FLogger.Error('Cleanup failed: ' + E.Message);
        end;
        
        Sleep(60 * 60 * 1000);  // 1 hour
      end;
    end);
end;

procedure TCleanupService.StopAsync(CancellationToken: ICancellationToken);
begin
  FLogger.Info('Cleanup service stopping...');
  FCancelled := True;
end;
```

## Registration

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureServices(procedure(Services: IServiceCollection)
    begin
      Services.AddHostedService<TCleanupService>;
    end)
  .Build
  .Run;
```

## Scoped Services in Background

To use scoped services (like DbContext), create a scope:

```pascal
procedure TCleanupService.DoCleanup;
var
  ScopeFactory: IServiceScopeFactory;
  Scope: IServiceScope;
  DbContext: TAppDbContext;
begin
  ScopeFactory := FServiceProvider.GetRequiredService<IServiceScopeFactory>;
  Scope := ScopeFactory.CreateScope;
  try
    DbContext := Scope.ServiceProvider.GetRequiredService<TAppDbContext>;
    
    // Delete old records
    DbContext.Logs
      .Where(TLog.Props.CreatedAt < (Now - 30))
      .Delete;
      
    DbContext.SaveChanges;
  finally
    // Scope disposes DbContext automatically
  end;
end;
```

## Timed Background Service

```pascal
type
  TTimedService = class(TInterfacedObject, IHostedService)
  private
    FInterval: Integer;  // milliseconds
    procedure Execute;
  public
    constructor Create(IntervalMs: Integer);
    procedure StartAsync(CancellationToken: ICancellationToken);
    procedure StopAsync(CancellationToken: ICancellationToken);
  end;

procedure TTimedService.StartAsync(CancellationToken: ICancellationToken);
begin
  TAsyncTask.Run(procedure
    begin
      while not CancellationToken.IsCancellationRequested do
      begin
        Execute;
        Sleep(FInterval);
      end;
    end);
end;
```

## Common Use Cases

1. **Cache refresh** - Periodically update cached data
2. **Email queue** - Process outbound emails
3. **Cleanup** - Delete old logs, temp files
4. **Health monitoring** - Check external services
5. **Report generation** - Scheduled reports

---

[← Dependency Injection](dependency-injection.md) | [Next: Configuration →](configuration.md)
