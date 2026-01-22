program RestClientDemo;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Dext.Net.RestClient,
  Dext.Net.RestRequest,
  Dext.Threading.Async,
  Dext.Threading.CancellationToken,
  User.Entity in 'User.Entity.pas';

type
  TPost = class
  private
    FId: Integer;
    FTitle: string;
    FBody: string;
    FUserId: Integer;
  public
    property id: Integer read FId write FId;
    property title: string read FTitle write FTitle;
    property body: string read FBody write FBody;
    property userId: Integer read FUserId write FUserId;
  end;

var
  Countdown: TCountdownEvent;

procedure DemoFluentGet;
begin
  Writeln('--- Demo: Fluent GET (Typed) ---');
  Countdown.AddCount;
  
  RestClient('https://jsonplaceholder.typicode.com')
    .Get('/users/1')
    .OnCompleteAsync(
      procedure(Response: IRestResponse)
      begin
        Writeln('Status: ', Response.StatusCode);
        Writeln('Content: ', Response.ContentString.Substring(0, 100) + '...');
        Countdown.Signal;
      end)
    .OnExceptionAsync(
      procedure(E: Exception)
      begin
        Writeln('Error: ', E.Message);
        Countdown.Signal;
      end)
    .Start;
end;

procedure DemoRequestRequest;
begin
  Writeln('--- Demo: TRestRequest Builder ---');
  Countdown.AddCount;
  
  var RestClient := TRestClient.Create('https://jsonplaceholder.typicode.com');
  
  TRestRequest
    .Create(RestClient, hmPOST, '/posts')
    .Header('X-Custom-Header', 'DextValue')
    .JsonBody('{"title": "foo", "body": "bar", "userId": 1}')
    .Execute
    .OnCompleteAsync(
      procedure(Response: IRestResponse)
      begin
        Writeln('POST Status: ', Response.StatusCode);
        Writeln('POST Response: ', Response.ContentString);
        Countdown.Signal;
      end)
    .OnExceptionAsync(
      procedure(E: Exception)
      begin
        Writeln('Error: ', E.Message);
        Countdown.Signal;
      end)
    .Start;
end;

procedure DemoWithCancellation;
begin
  Writeln('--- Demo: Cancellation ---');
  Countdown.AddCount;
  
  var CancelationTokenSource := TCancellationTokenSource.Create;
  try
    RestClient('https://jsonplaceholder.typicode.com')
      .Get('/posts')
      .WithCancellation(CancelationTokenSource.Token)
      .OnCompleteAsync(
        procedure(Response: IRestResponse)
        begin
          Writeln('Success (should not happen if canceled)');
          Countdown.Signal;
        end)
      .OnExceptionAsync(
        procedure(E: Exception)
        begin
            Writeln('Expected Error: ', E.Message);
            Countdown.Signal;
        end)
      .Start;
      
    CancelationTokenSource.Cancel;
    Writeln('Requested Cancellation.');
  finally
    CancelationTokenSource.Free;
  end;
end;

procedure DemoSynchronous;
begin
  Writeln('--- Demo: Synchronous Request ---');
  
  try
    var Response := RestClient('https://jsonplaceholder.typicode.com')
      .Get<TPost>('/posts/1')
      .Await; // Blocks and runs on current thread
      
    try
      Writeln('Synchronous success!');
      Writeln('ID: ', Response.id);
      Writeln('Title: ', Response.title);
    finally
      Response.Free;
    end;
  except
    on E: Exception do
      Writeln('Synchronous Error: ', E.Message);
  end;
  Writeln;
end;

begin
  try
    Countdown := TCountdownEvent.Create(1);
    try
      DemoFluentGet;
      DemoRequestRequest;
      DemoWithCancellation;
      DemoSynchronous;
      
      Writeln('Waiting for tasks...');
      Countdown.Signal; // Finish setup
      Countdown.WaitFor;
    finally
      Countdown.Free;
    end;
    
    ConsolePause;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ConsolePause;
    end;
  end;
end.

