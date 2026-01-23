unit Dext.Vcl.FormDecorator;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,
  Winapi.Messages,
  Dext.Classes; // Replaces Foundation.Classes

type
  IFormDecorator = interface(IInterface)
    ['{A4EC403B-5204-4D81-8CC2-229EA613C1FE}']
    function GetForm: TCustomForm;
    property Form: TCustomForm read GetForm;
  end;

  TFormDecoratorClass = class of TFormDecorator;
  TFormDecorator = class(TComponent, IFormDecorator)
  private
    FForm: TCustomForm;
    function GetForm: TCustomForm;
    procedure SetDecoratorName;
  public
    constructor Create(Form: TCustomForm); reintroduce; virtual;
    property Form: TCustomForm read GetForm;
  end;

  TWndMethodProxy = reference to procedure(var Message: TMessage; var Handled: Boolean);

  IWindowProcProxy = interface(IFormDecorator)
    ['{791245C4-9002-4085-8397-C0347B7FB2A6}']
    procedure SetWindowProcProxy(Form: TCustomForm);
    procedure RestoreSavedWndProc;

    procedure RegisterWindowProc(WindowProc: TWndMethodProxy);
    procedure UnregisterWindowProc(WindowProc: TWndMethodProxy);
  end;

  TWindowProcProxy = class(TFormDecorator, IWindowProcProxy)
  protected
    FProcList: TList<TWndMethodProxy>;
    FSavedWndProc: TWndMethod;
    procedure WindowProcProxy(var Message: TMessage);
  protected
    procedure SetWindowProcProxy(Form: TCustomForm);
    procedure RestoreSavedWndProc;

    procedure RegisterWindowProc(WindowProc: TWndMethodProxy);
    procedure UnregisterWindowProc(WindowProc: TWndMethodProxy);
  public
    constructor Create(Form: TCustomForm); overload; override;
    constructor Create(Form: TCustomForm; WindowProc: TWndMethodProxy); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

{ TFormDecorator }

constructor TFormDecorator.Create(Form: TCustomForm);
begin
  inherited Create(Form);
  FForm := Form;
  SetDecoratorName;
end;

function TFormDecorator.GetForm: TCustomForm;
begin
  Result := FForm;
end;

procedure TFormDecorator.SetDecoratorName;
var
  DecoratorName: string;
begin
  // Uses Dext.Classes.TObjectHelper
  DecoratorName := Self.ObjectName;
  if Name = '' then
  begin
    Name := DecoratorName;
  end;
end;

{ TWindowProcProxy }

procedure TWindowProcProxy.WindowProcProxy(var Message: TMessage);
var
  Handled: Boolean;
  Proc: TWndMethodProxy;
begin
  Handled := False;
  for Proc in FProcList do
  begin
    Proc(Message, Handled);

    if Handled then
    begin
      Break;
    end;
  end;

  if (not Handled) and Assigned(FSavedWndProc) then
  begin
    FSavedWndProc(Message);
  end;
end;

procedure TWindowProcProxy.RestoreSavedWndProc;
begin
  if Assigned(FSavedWndProc) then
  begin
    Form.WindowProc := FSavedWndProc;
    FSavedWndProc := nil;
  end;
end;

constructor TWindowProcProxy.Create(Form: TCustomForm);
begin
  inherited;
  FProcList := TList<TWndMethodProxy>.Create;
  SetWindowProcProxy(Form);
end;

constructor TWindowProcProxy.Create(Form: TCustomForm; WindowProc: TWndMethodProxy);
begin
  Create(Form);
  RegisterWindowProc(WindowProc);
end;

destructor TWindowProcProxy.Destroy;
begin
  FProcList.Free;
  RestoreSavedWndProc;
  inherited;
end;

procedure TWindowProcProxy.SetWindowProcProxy(Form: TCustomForm);
begin
  FSavedWndProc   := Form.WindowProc;
  Form.WindowProc := WindowProcProxy;
end;

procedure TWindowProcProxy.RegisterWindowProc(WindowProc: TWndMethodProxy);
begin
  if FProcList.IndexOf(WindowProc) = -1 then
  begin
    FProcList.Add(WindowProc);
  end;
end;

procedure TWindowProcProxy.UnregisterWindowProc(WindowProc: TWndMethodProxy);
var
  Index: Integer;
begin
  Index := FProcList.IndexOf(WindowProc);
  if Index > -1 then
  begin
    FProcList.Delete(Index);
  end;
end;

end.
