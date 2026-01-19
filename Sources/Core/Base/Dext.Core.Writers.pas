unit Dext.Core.Writers;

interface

uses
  System.Classes;

type
  IDextWriter = interface
    procedure SafeWriteLn(const aMessage:String);
    procedure SafeWrite(const aMessage:String);
  end;

type
  /// <Summary>
  ///  Use to ignore all SafeWrites
  /// </Summary>
  TNullWriter = class(TInterfacedObject,IDextWriter)
  private
    procedure SafeWriteLn(const aMessage:String);
    procedure SafeWrite(const aMessage:String);
  end;

  /// <Summary>
  ///  Use to route SafeWrites to the Console. Assumes the console is available
  ///  and will ignore any I/O errors that are generated.
  /// </Summary>
  TConsoleWriter = class(TInterfacedObject,IDextWriter)
  private
    procedure SafeWriteLn(const aMessage:String);
    procedure SafeWrite(const aMessage:String);
  end;

  {$IFDEF MSWINDOWS}
  /// <Summary>
  ///  Use to route SafeWrites to the windows debugger via OutputDebugString
  ///  There will generally be a delay, so partial writes are held in memory
  ///  until the next SafeWriteln.
  /// </Summary>
  TWindowsDebugWriter = class(TInterfacedObject,IDextWriter)
  private
    fPartial : string;
    procedure SafeWriteLn(const aMessage:String);
    procedure SafeWrite(const aMessage:String);
  public
    constructor Create;
  end;
  {$ENDIF}

  /// <Summary>
  ///  Use to route all SafeWrites to a Strings/TStringList.  A common use of
  ///  this would be to display the messages in a vcl TMemo control by calling
  ///  TStringsWriter.Create(Memo1.Lines); Just be sure to call
  ///  InitializeDextWriter(nil) or InitializeDextWriter(TStringsWriter.Create(Nil))
  ///  when destroying the form to avoid an Access Violation if SafeWrite is called
  ///  after the destruction of the fStrings
  /// </Summary>
  TStringsWriter = class(TInterfacedObject,IDextWriter)
  private
    fStrings : TStrings;
    fPartial : String;
    procedure _UpdateStrings(const aMessage:String;Newline:Boolean);
    procedure SafeWriteLn(const aMessage:String);
    procedure SafeWrite(const aMessage:String);
  public
    constructor Create(aStrings:TStrings);
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.SyncObjs;

{ TNullWriter }

procedure TNullWriter.SafeWrite(const aMessage: String);
begin
  // do nothing but eat the message
end;

procedure TNullWriter.SafeWriteLn(const aMessage: String);
begin
  // do nothing but eat the message
end;

{ TConsoleWriter }

procedure TConsoleWriter.SafeWrite(const aMessage: String);
begin
  try
    Write(aMessage);
  except
    // Silently ignore I/O errors
  end;
end;

procedure TConsoleWriter.SafeWriteLn(const aMessage: String);
begin
  try
    WriteLn(aMessage);
  except
    // Silently ignore I/O errors
  end;
end;

{ TWindowsDebugWriter }

{$IFDEF MSWINDOWS}
constructor TWindowsDebugWriter.Create;
begin
  Inherited create;
  fPartial := '';
end;

procedure TWindowsDebugWriter.SafeWrite(const aMessage: String);
begin
  TMonitor.enter(Self);
  try
    fPartial := fPartial + aMessage;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWindowsDebugWriter.SafeWriteLn(const aMessage: String);
begin
  TMonitor.Enter(Self);
  try
    OutputDebugString(PWideChar(fPartial + aMessage));
    fPartial := '';
  finally
    TMonitor.Exit(Self);
  end;
end;
{$ENDIF}

{ TStringsWriter }

constructor TStringsWriter.Create(aStrings: TStrings);
begin
  Inherited Create;
  fStrings := aStrings;
  fStrings.Options := fStrings.Options - [ soTrailingLineBreak ];
  fPartial := '';
end;

procedure TStringsWriter.SafeWrite(const aMessage: String);
begin
  if assigned(fStrings) then
    _UpdateStrings(aMessage,False);
end;

procedure TStringsWriter.SafeWriteLn(const aMessage: String);
begin
  if assigned(fStrings) then
    _UpdateStrings(aMessage,True);
  fPartial := '';
end;

procedure TStringsWriter._UpdateStrings(const aMessage: String;
  Newline: Boolean);
begin
  TMonitor.Enter(fStrings);
  try
    fStrings.BeginUpdate;
    fPartial := fPartial + aMessage;
    if fStrings.Count = 0 then
      fStrings.Text := fPartial
    else
      fStrings.Strings[fStrings.Count -1] := fPartial;
    if NewLine then
      fStrings.add('');
  finally
    fStrings.EndUpdate;
    TMonitor.Exit(fStrings);
  end;
end;

end.
