program TestRingBuffer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Logging,
  Dext.Logging.RingBuffer;

procedure Assert(Condition: Boolean; const Msg: string);
begin
  if not Condition then
    raise Exception.Create('Assertion Failed: ' + Msg);
  WriteLn('PASS: ' + Msg);
end;

procedure TestBasicCycle;
var
  RB: TRingBuffer;
  Entry: PLogEntry;
  ReadEntry: TLogEntry;
begin
  Writeln('--- TestBasicCycle ---');
  RB := TRingBuffer.Create(4); // Size 16
  try
    // Fill 16 items
    for var i := 0 to 15 do
    begin
      Assert(RB.TryWrite(Entry), Format('Write %d', [i]));
      Entry.SequenceId := i;
      Entry.IsReady := True;
    end;
    
    // 17th should fail (Full)
    Assert(not RB.TryWrite(Entry), 'Buffer Full Check');
    
    // Read 8
    for var i := 0 to 7 do
    begin
      Assert(RB.TryRead(ReadEntry), Format('Read %d', [i]));
      Assert(ReadEntry.SequenceId = i, Format('Seq match %d', [i]));
    end;
    
    // Write 8 more (Wrap around)
    for var i := 16 to 23 do
    begin
      Assert(RB.TryWrite(Entry), Format('Write %d (Wrapped)', [i]));
      Entry.SequenceId := i;
      Entry.IsReady := True;
    end;
    
    // Read Remaining 16 (8 old + 8 new)
    for var i := 8 to 23 do
    begin
      Assert(RB.TryRead(ReadEntry), Format('Read %d (Mixed)', [i]));
      Assert(ReadEntry.SequenceId = i, Format('Seq match %d', [i]));
    end;
    
    // Empty check
    Assert(not RB.TryRead(ReadEntry), 'Buffer Empty Check');
    
  finally
    RB.Free;
  end;
end;

begin
  try
    TestBasicCycle;
    Writeln('All RingBuffer tests passed.');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
