# Async API

Async/Await and real asynchronous programming for Delphi.

## What is TAsyncTask?

`TAsyncTask` is a high-level primitive for managing asynchronous operations without the complexity of manual thread management. It allows task chaining and automatic return to the main thread (UI thread).

## Running a Task

```pascal
uses
  Dext.Core.Async;

TAsyncTask.Run(procedure
  begin
    // This runs in a separate thread (background)
    DoHeavyWork;
  end);
```

## Fluent Chaining

You can chain operations that depend on previous results:

```pascal
TAsyncTask.Run<TUserProfile>(
  function: TUserProfile
  begin
    Result := ApiService.GetProfile(UserId);
  end)
  .ThenBy<Boolean>(
    function(Profile: TUserProfile): Boolean
    begin
      Result := Profile.IsActive;
    end)
  .OnComplete(
    procedure(IsActive: Boolean)
    begin
      // Executed on UI Thread
      if IsActive then
        UpdateUI;
    end)
  .Start;
```

## Exception Handling

```pascal
TAsyncTask.Run(procedure
  begin
    raise EInvalidOperation.Create('Error in background');
  end)
  .OnException(
    procedure(Ex: Exception)
    begin
      // Catch error on UI Thread
      ShowMessage('Error: ' + Ex.Message);
    end)
  .Start;
```

## Cancellation

Use `TCancellationToken` to abort running tasks:

```pascal
var CTS := TCancellationTokenSource.Create;

TAsyncTask.Run(procedure
  begin
    while not CTS.Token.IsCancellationRequested do
    begin
      // Process...
    end;
  end)
  .Start;

// Later...
CTS.Cancel;
```

---

[← Configuration](configuration.md) | [Next: Appendix →](../appendix/type-system.md)
