{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2026-01-25                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Logging.RingBuffer;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  Dext.Types.UUID,
  Dext.Logging;

const
  CACHE_LINE_SIZE = 64;

type
  PLogEntry = ^TLogEntry;
  TLogEntry = record
    SequenceId: Int64;
    TimeStamp: TDateTime;
    Level: TLogLevel;
    ThreadID: TThreadID;
    
    // Context
    TraceId: TUUID;
    SpanId: TUUID;
    
    ScopeSnapshot: IInterface; // Keep scope alive
    MessageTemplate: string;   // Reference counted string
    // Simplified args storage for now. To be zero-alloc, we'd need a byte buffer and manual serialization
    // For Phase 1, we store the array as a dynamic structure or pointer?
    // Delphi's "array of const" is tricky to store without copying.
    // For V1, let's copy formatted string or args if possible.
    // Ideally, we store the arguments to format later.
    // But TVarRec contains pointers. Pointers to stack variables are dangerous in async!
    // So we MUST copy the data.
    // "Zero-alloc" usually implies a pre-allocated buffer where we copy bytes.
    // For MVP, we will format the string inside the Producer (Log method) to be safe, 
    // OR act as a synchronous sink if args are present, UNLESS we implement a deep copier.
    // Architecture decision: For now, format message immediately if args are present, 
    // unless we implement the complex Arg serializer.
    FormattedMessage: string; 
    
    IsReady: Boolean; // Commit flag
  end;

  // Alignment and padding to ensure cache line exclusivity
  TRingBufferPointers = record
    Head: Int64; // Producer index
    padding1: array[0..55] of Byte; 
    Tail: Int64; // Consumer index
    padding2: array[0..55] of Byte;
  end;

  /// <summary>
  ///   High-performance Single-Consumer Multi-Producer Ring Buffer.
  /// </summary>
  TRingBuffer = class
  private
    FBuffer: TArray<TLogEntry>;
    FMsk: Int64;
    FPointers: TRingBufferPointers;

  public
    constructor Create(SizePowerOfTwo: Integer);
    
    /// <summary>
    ///   Tries to reserve a slot for writing. Returns True if successful and provides a pointer to the slot.
    ///   If buffer is full, returns False (Drop strategy).
    /// </summary>
    function TryWrite(out Entry: PLogEntry): Boolean;
    
    /// <summary>
    ///   Tries to read the next entry. Returns True if data is available.
    /// </summary>
    function TryRead(out Entry: TLogEntry): Boolean;
    
    property Capacity: Int64 read FMsk;
  end;

implementation

constructor TRingBuffer.Create(SizePowerOfTwo: Integer);
begin
  inherited Create;
  if (SizePowerOfTwo < 0) or (SizePowerOfTwo > 30) then
    raise EArgumentException.Create('Invalid size power');
    
  SetLength(FBuffer, 1 shl SizePowerOfTwo);
  FMsk := Length(FBuffer) - 1;
  FPointers.Head := 0;
  FPointers.Tail := 0;
end;



function TRingBuffer.TryWrite(out Entry: PLogEntry): Boolean;
var
  CurrentHead, NextHead, Cap: Int64;
begin
  Cap := Length(FBuffer);
  
  // Optimistic CAS loop
  repeat
    CurrentHead := TInterlocked.Read(FPointers.Head);
    
    // Check if full (Head wraps around and "catches" Tail)
    var Tail := TInterlocked.Read(FPointers.Tail);
    if (CurrentHead - Tail) >= Cap then
    begin
      Result := False; // Buffer Full
      Exit;
    end;
    
    NextHead := CurrentHead + 1;
  until TInterlocked.CompareExchange(FPointers.Head, NextHead, CurrentHead) = CurrentHead;

  // Slot reserved
  Entry := @FBuffer[CurrentHead and FMsk];
  
  // Important: The consumer must wait until we say "IsReady".
  // Consumer logic will spin or check IsReady on the Tail index.
  // Actually, standard RingBuffers often rely on Head/Tail only, but that only works for Single-Producer.
  // For Multi-Producer, we have reserved the slot (index), but we haven't written data yet.
  // If Consumer reads now, it sees garbage.
  // Solution: "IsReady" flag or "Sequence" check.
  // We use IsReady := False before returning pointer.
  // CALLER is responsible for setting Entry.IsReady := True after writing.
  // Since we are recycling slots, we need to be careful.
  // Ideally, the slot should have a Sequence number.
  
  Entry.SequenceId := CurrentHead;
  Entry.IsReady := False; 
  
  Result := True;
end;

function TRingBuffer.TryRead(out Entry: TLogEntry): Boolean;
var
  CurrentTail, Head: Int64;
  Slot: PLogEntry;
begin
  CurrentTail := TInterlocked.Read(FPointers.Tail);
  Head := TInterlocked.Read(FPointers.Head);

  if CurrentTail >= Head then
    Exit(False); // Empty

  // Peek at the slot
  Slot := @FBuffer[CurrentTail and FMsk];

  // For MPSC, we must check if the producer finished writing to this slot.
  // The producer reserves the index, then writes. If we are faster than the writer, we might see IsReady=False.
  if not Slot.IsReady then
    Exit(False); // Slot reserved but data not yet committed.

  // Copy data out
  Entry := Slot^;
  
  // Advance Tail
  // Only one consumer, so we can just increment.
  // But wait! IsReady needs to be reset? 
  // No, next producer will overwrite/reset.
  
  // Important: Standard memory barrier needed? TInterlocked handles it.
  TInterlocked.Increment(FPointers.Tail);
  
  Result := True;
end;

end.
