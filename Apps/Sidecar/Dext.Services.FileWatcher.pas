unit Dext.Services.FileWatcher;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, System.SyncObjs;

type
  TFileChangeEvent = procedure(Sender: TObject; const Action: string; const FileName: string) of object;

  PFILE_NOTIFY_INFORMATION = ^FILE_NOTIFY_INFORMATION;
  FILE_NOTIFY_INFORMATION = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WCHAR;
  end;

  TDextFileWatcher = class(TThread)
  private
    FDirectory: string;
    FOnChange: TFileChangeEvent;
    FDirectoryHandle: THandle;
    FOverlapped: TOverlapped;
    FBuffer: array[0..4096] of Byte;
    FBytesReturned: DWORD;
    FStopEvent: TEvent;
    
    procedure HandleChange;
  protected
    procedure Execute; override;
  public
    constructor Create(const Directory: string);
    destructor Destroy; override;
    
    procedure Stop;
    
    property OnChange: TFileChangeEvent read FOnChange write FOnChange;
    property Directory: string read FDirectory;
  end;

implementation

{ TDextFileWatcher }

constructor TDextFileWatcher.Create(const Directory: string);
begin
  inherited Create(True); // Create suspended
  FDirectory := Directory;
  FreeOnTerminate := False;
  FStopEvent := TEvent.Create(nil, True, False, '');
end;

destructor TDextFileWatcher.Destroy;
begin
  Stop; // Ensure stopped
  FStopEvent.Free;
  inherited;
end;

procedure TDextFileWatcher.Execute;
var
  WaitResult: DWORD;
begin
  FDirectoryHandle := CreateFile(
    PChar(FDirectory),
    FILE_LIST_DIRECTORY,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
    0
  );

  if FDirectoryHandle = INVALID_HANDLE_VALUE then
    Exit;

  // Create event for overlapped I/O
  FOverlapped.hEvent := CreateEvent(nil, True, False, nil);

  try
    while not Terminated do
    begin
      if ReadDirectoryChangesW(
        FDirectoryHandle,
        @FBuffer[0],
        SizeOf(FBuffer),
        True, // Watch subtree
        FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_CREATION,
        @FBytesReturned,
        @FOverlapped,
        nil
      ) then
      begin
        // Wait for change or stop signal
        // We use WaitForMultipleObjects to check for both IO Completion (via Overlapped hEvent) and our Stop Event
        var Handles: array[0..1] of THandle;
        Handles[0] := FOverlapped.hEvent;
        Handles[1] := FStopEvent.Handle;
        
        WaitResult := WaitForMultipleObjects(2, @Handles, False, INFINITE);

        if WaitResult = WAIT_OBJECT_0 then
        begin
          // Change detected
          Synchronize(HandleChange);
          ResetEvent(FOverlapped.hEvent);
        end
        else if WaitResult = WAIT_OBJECT_0 + 1 then
        begin
          // Stop requested
          Terminate;
        end;
      end
      else
      begin
        // Error or handle closed
        Terminate;
      end;
    end;
  finally
    CloseHandle(FOverlapped.hEvent);
    CloseHandle(FDirectoryHandle);
  end;
end;

procedure TDextFileWatcher.HandleChange;
var
  NotifyInfo: PFILE_NOTIFY_INFORMATION;
  FileName: string;
  Action: string;
begin
  if not Assigned(FOnChange) then Exit;

  NotifyInfo := PFILE_NOTIFY_INFORMATION(@FBuffer[0]);
  repeat
    SetString(FileName, PWideChar(@NotifyInfo.FileName[0]), NotifyInfo.FileNameLength div 2);
    
    case NotifyInfo.Action of
      FILE_ACTION_ADDED:            Action := 'ADDED';
      FILE_ACTION_REMOVED:          Action := 'REMOVED';
      FILE_ACTION_MODIFIED:         Action := 'MODIFIED';
      FILE_ACTION_RENAMED_OLD_NAME: Action := 'RENAMED_OLD';
      FILE_ACTION_RENAMED_NEW_NAME: Action := 'RENAMED_NEW';
    else
      Action := 'UNKNOWN';
    end;

    // Filter interesting files (e.g. .pas, .dfm)
    if (ExtractFileExt(FileName) = '.pas') or 
       (ExtractFileExt(FileName) = '.dfm') or 
       (ExtractFileExt(FileName) = '.dproj') then
    begin
        FOnChange(Self, Action, FileName);
    end;

    if NotifyInfo.NextEntryOffset = 0 then Break;
    NotifyInfo := PFILE_NOTIFY_INFORMATION(PByte(NotifyInfo) + NotifyInfo.NextEntryOffset);
  until False;
end;

procedure TDextFileWatcher.Stop;
begin
  Terminate;
  FStopEvent.SetEvent;
  WaitFor;
end;

end.
