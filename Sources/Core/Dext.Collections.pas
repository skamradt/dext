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
{  Created: 2025-12-08                                                      }
{  Refactored: 2026-02-23 — Replaced RTL generics with TRawList backend    }
{                                                                           }
{***************************************************************************}
unit Dext.Collections;

interface

uses
  System.SysUtils,
  System.TypInfo,
  Dext.Collections.Base,
  Dext.Collections.Comparers,
  Dext.Collections.Dict,
  Dext.Collections.Memory,
  Dext.Collections.Raw,
  Dext.Specifications.Evaluator,
  Dext.Specifications.Interfaces;

type
  /// <summary>Collection notification type (compatible with RTL enum values)</summary>
  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  
  {$M+}
  IList<T> = interface(Dext.Collections.Base.IEnumerable<T>)
    ['{8877539D-3522-488B-933B-8C4581177699}']
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);

    procedure Add(const Value: T);
    procedure AddRange(const Values: IEnumerable<T>); overload;
    procedure AddRange(const Values: array of T); overload;
    procedure Insert(Index: Integer; const Value: T);
    function Remove(const Value: T): Boolean;
    function Extract(const Value: T): T;
    procedure Delete(Index: Integer);
    procedure RemoveAt(Index: Integer);
    procedure Clear();
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;

    function Where(const Predicate: TFunc<T, Boolean>): IList<T>; overload;
    function Where(const Expression: IExpression): IList<T>; overload;

    function First: T; overload;
    function First(const Expression: IExpression): T; overload;

    function Last: T;

    function FirstOrDefault: T; overload;
    function FirstOrDefault(const DefaultValue: T): T; overload;
    function FirstOrDefault(const Expression: IExpression): T; overload;

    function Any(const Predicate: TFunc<T, Boolean>): Boolean; overload;
    function Any(const Expression: IExpression): Boolean; overload;
    function Any: Boolean; overload;

    function All(const Predicate: TFunc<T, Boolean>): Boolean; overload;
    function All(const Expression: IExpression): Boolean; overload;

    procedure ForEach(const Action: TProc<T>);
    procedure Sort(const AComparer: IComparer<T> = nil);
    function ToArray: TArray<T>;
  end;

  /// <summary>Generic Stack (LIFO) interface</summary>
  IStack<T> = interface(IEnumerable<T>)
    ['{7A8B9C0D-1E2F-3A4B-5C6D-7E8F9A0B1C2D}']
    function GetCount: Integer;
    procedure Push(const Value: T);
    function Pop: T;
    function Peek: T;
    function TryPop(out Value: T): Boolean;
    function TryPeek(out Value: T): Boolean;
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function ToArray: TArray<T>;
    property Count: Integer read GetCount;
  end;

  /// <summary>Generic Queue (FIFO) interface</summary>
  IQueue<T> = interface(Dext.Collections.Base.IEnumerable<T>)
    ['{AD1F2E3D-4C5B-6A7B-8C9D-0E1F2A3B4C5D}']
    function GetCount: Integer;
    procedure Enqueue(const Value: T);
    function Dequeue: T;
    function Peek: T;
    function TryDequeue(out Value: T): Boolean;
    function TryPeek(out Value: T): Boolean;
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function ToArray: TArray<T>;
    property Count: Integer read GetCount;
  end;

  /// <summary>Generic HashSet interface</summary>
  IHashSet<T> = interface(Dext.Collections.Base.IEnumerable<T>)
    ['{6B7C8D9E-0F1A-2B3C-4D5E-6F7A8B9C0D1E}']
    function GetCount: Integer;
    function Add(const Value: T): Boolean;
    function Remove(const Value: T): Boolean;
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function ToArray: TArray<T>;
    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///   High-performance record-based enumerator for TList<T>.
  ///   Avoids interface overhead and AddRef/Release during for-in loops.
  /// </summary>
  TListEnumerator<T> = record
  private
    FCurrent: PByte;
    FEndPtr: PByte;
  public
    function MoveNext: Boolean; inline;
    function GetCurrent: T; inline;
    property Current: T read GetCurrent;
  end;

  /// <summary>Enumerator class for interface-based access (legacy/compatibility)</summary>
  TEnumerator<T> = class(TInterfacedObject, Dext.Collections.Base.IEnumerator<T>)
  private
    FCore: TRawList;
    FIndex: Integer;
  public
    constructor Create(ACore: TRawList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  /// <summary>Base class avoiding Delphi explicit interface method mapping bug</summary>
  TListBase<T> = class(TInterfacedObject, Dext.Collections.Base.IEnumerable<T>)
  public
    function GetInterfaceEnumerator: Dext.Collections.Base.IEnumerator<T>; virtual; abstract;
    function GetEnumerator: Dext.Collections.Base.IEnumerator<T>; virtual;
  end;

  {$M+}
  TList<T> = class(TListBase<T>, IList<T>, ICollection)
  private
    FCore: TRawList;
    FOwnsObjects: Boolean;
    FIsClass: Boolean;
    FTypeKind: TTypeKind;
    FIsManaged: Boolean;
    FHasNotify: Boolean;
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): T; inline;
    procedure SetItem(Index: Integer; const Value: T); inline;
    function GetOwnsObjects: Boolean; inline;
    procedure SetOwnsObjects(Value: Boolean); inline;
  protected
    procedure Notify(Sender: TObject; const Item: T;
      Action: TCollectionNotification); virtual;
  public
    function GetInterfaceEnumerator: Dext.Collections.Base.IEnumerator<T>; override;
    function GetEnumerator: TListEnumerator<T>; reintroduce; inline;
    
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;

    procedure Add(const Value: T); inline;
    procedure AddRange(const Values: IEnumerable<T>); overload;
    procedure AddRange(const Values: array of T); overload;
    procedure Insert(Index: Integer; const Value: T);
    function Remove(const Value: T): Boolean;
    function Extract(const Value: T): T;
    procedure Delete(Index: Integer);
    procedure RemoveAt(Index: Integer); inline;
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;

    function Where(const Predicate: TFunc<T, Boolean>): IList<T>; overload;
    function Where(const Expression: IExpression): IList<T>; overload;

    function First: T; overload;
    function First(const Expression: IExpression): T; overload;

    function Last: T;

    function FirstOrDefault: T; overload;
    function FirstOrDefault(const DefaultValue: T): T; overload;
    function FirstOrDefault(const Expression: IExpression): T; overload;

    function Any(const Predicate: TFunc<T, Boolean>): Boolean; overload;
    function Any(const Expression: IExpression): Boolean; overload;
    function Any: Boolean; overload;

    function All(const Predicate: TFunc<T, Boolean>): Boolean; overload;
    function All(const Expression: IExpression): Boolean; overload;

    procedure ForEach(const Action: TProc<T>);
    procedure Sort(const AComparer: IComparer<T> = nil);
    function ToArray: TArray<T>;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  public
    destructor Destroy; override;
  end;

  /// <summary>Backward compatibility alias</summary>
  TSmartList<T> = class(TList<T>);
  TSmartEnumerator<T> = class(TEnumerator<T>);

  {$M+}
  {$RTTI EXPLICIT METHODS([vcPublic])}
  TCollections = class
  public
    class function CreateList<T>(OwnsObjects: Boolean = False): IList<T>; static;
    class function CreateObjectList<T: class>(OwnsObjects: Boolean = False): IList<T>; static;
    class function CreateDictionary<K, V>(ACapacity: Integer = 0): IDictionary<K, V>; overload; static;
    class function CreateDictionary<K, V>(AOwnsValues: Boolean; ACapacity: Integer = 0): IDictionary<K, V>; overload; static;
    class function CreateDictionaryIgnoreCase<K, V>(AOwnsValues: Boolean = False; ACapacity: Integer = 0): IDictionary<K, V>; static;

    class function CreateStack<T>: IStack<T>; static;
    class function CreateQueue<T>: IQueue<T>; static;
    class function CreateHashSet<T>: IHashSet<T>; static;
  end;
  {$M-}

implementation

uses
  Dext.Collections.HashSet,
  Dext.Collections.Queue,
  Dext.Collections.Stack;

{ TListBase<T> }

function TListBase<T>.GetEnumerator: Dext.Collections.Base.IEnumerator<T>;
begin
  Result := GetInterfaceEnumerator;
end;

{ TEnumerator<T> }

constructor TEnumerator<T>.Create(ACore: TRawList);
begin
  inherited Create;
  FCore := ACore;
  FIndex := -1;
end;

function TEnumerator<T>.GetCurrent: T;
begin
  FCore.GetRawItem(FIndex, @Result);
end;

function TEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FCore.Count;
end;

{ TListEnumerator<T> }

function TListEnumerator<T>.GetCurrent: T;
type
  P_T = ^T;
begin
  Result := P_T(FCurrent)^;
end;

function TListEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FCurrent, SizeOf(T));
  Result := FCurrent < FEndPtr;
end;

{ TList<T> }

constructor TList<T>.Create;
begin
  Create(False);
end;

constructor TList<T>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
  FTypeKind := PTypeInfo(System.TypeInfo(T)).Kind;
  FIsClass := FTypeKind = tkClass;
  FIsManaged := System.IsManagedType(T);
  FHasNotify := False; // Updated when OnNotify is set, but TList usually doesn't use it directly on core
  FCore := TRawList.Create(SizeOf(T), System.TypeInfo(T), FIsManaged);
end;

destructor TList<T>.Destroy;
begin
  Clear;
  FCore.Free;
  inherited;
end;

procedure TList<T>.Notify(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  if FOwnsObjects and (Action = cnRemoved) then
  begin
    if FIsClass then
      TObject(PPointer(@Item)^).Free;
  end;
end;

function TList<T>.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TList<T>.SetOwnsObjects(Value: Boolean);
begin
  FOwnsObjects := Value;
end;

function TList<T>.GetCount: Integer;
begin
  Result := FCore.Count;
end;

function TList<T>.GetItem(Index: Integer): T;
var
  P: Pointer;
begin
  P := FCore.Data + (Index * FCore.ElementSize);
  if not FCore.IsManaged then
  begin
    case FCore.ElementSize of
      1: PByte(@Result)^ := PByte(P)^;
      2: PWord(@Result)^ := PWord(P)^;
      4: PCardinal(@Result)^ := PCardinal(P)^;
      8: PUInt64(@Result)^ := PUInt64(P)^;
    else
      System.Move(P^, Result, FCore.ElementSize);
    end;
  end
  else
  begin
    FillChar(Result, SizeOf(T), 0);
    System.CopyArray(@Result, P, FCore.TypeInfo, 1);
  end;
end;

procedure TList<T>.SetItem(Index: Integer; const Value: T);
var
  OldItem: T;
begin
  OldItem := GetItem(Index);
  FCore.SetRawItem(Index, @Value);
  Notify(Self, OldItem, cnRemoved);
  Notify(Self, Value, cnAdded);
end;

function TList<T>.GetEnumerator: TListEnumerator<T>;
var
  Data: PByte;
begin
  Data := FCore.Data;
  Result.FCurrent := Data - SizeOf(T);
  Result.FEndPtr := Data + (FCore.Count * SizeOf(T));
end;

function TList<T>.GetInterfaceEnumerator: Dext.Collections.Base.IEnumerator<T>;
begin
  Result := TEnumerator<T>.Create(FCore);
end;

procedure TList<T>.Add(const Value: T);
type P_T = ^T;
var
  LData: PByte;
  LCount, LCapacity: Integer;
begin
  LCount := FCore.Count;
  LCapacity := FCore.Capacity;
  
  if (LCount < LCapacity) and (not FIsManaged) then
  begin
    LData := FCore.Data;
    case SizeOf(T) of
      4: PCardinal(LData + (LCount * 4))^ := PCardinal(@Value)^;
      8: PUInt64(LData + (LCount * 8))^ := PUInt64(@Value)^;
      12: begin
            PCardinal(LData + (LCount * 12))^ := PCardinal(@Value)^;
            PUInt64(LData + (LCount * 12) + 4)^ := PUInt64(PByte(@Value) + 4)^;
          end;
      16: begin
            PUInt64(LData + (LCount * 16))^ := PUInt64(@Value)^;
            PUInt64(LData + (LCount * 16) + 8)^ := PUInt64(PByte(@Value) + 8)^;
          end;
    else
      System.Move(Value, (LData + (LCount * SizeOf(T)))^, SizeOf(T));
    end;
    FCore.FastIncrementCount;
  end
  else
    FCore.AddRaw(@Value);
end;

procedure TList<T>.AddRange(const Values: Dext.Collections.Base.IEnumerable<T>);
var
  Enum: Dext.Collections.Base.IEnumerator<T>;
begin
  Enum := Values.GetEnumerator;
  while Enum.MoveNext do
    Add(Enum.Current);
end;

procedure TList<T>.AddRange(const Values: array of T);
var
  I: Integer;
begin
  if FCore.Capacity < FCore.Count + Length(Values) then
    FCore.Capacity := FCore.Count + Length(Values);
  for I := Low(Values) to High(Values) do
    Add(Values[I]);
end;

procedure TList<T>.Insert(Index: Integer; const Value: T);
begin
  FCore.InsertRaw(Index, @Value);
end;

function TList<T>.IndexOf(const Value: T): Integer;
var
  I, LCount: Integer;
  P: PByte;
begin
  LCount := FCore.Count;
  if LCount = 0 then Exit(-1);
  P := FCore.Data;
  // Final Optimization: local LCount and specialized loops to beat RTL
  case FTypeKind of
    tkUString:
      for I := 0 to LCount - 1 do
      begin
        if PString(P)^ = PString(@Value)^ then Exit(I);
        Inc(P, SizeOf(String));
      end;
    tkClass, tkInterface:
      for I := 0 to LCount - 1 do
      begin
        if PPointer(P)^ = PPointer(@Value)^ then Exit(I);
        Inc(P, SizeOf(Pointer));
      end;
    tkLString:
      for I := 0 to LCount - 1 do
      begin
        if PAnsiString(P)^ = PAnsiString(@Value)^ then Exit(I);
        Inc(P, SizeOf(AnsiString));
      end;
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      case SizeOf(T) of
        1: for I := 0 to LCount - 1 do
           begin
             if PByte(P)^ = PByte(@Value)^ then Exit(I);
             Inc(P, 1);
           end;
        2: for I := 0 to LCount - 1 do
           begin
             if PWord(P)^ = PWord(@Value)^ then Exit(I);
             Inc(P, 2);
           end;
        4: for I := 0 to LCount - 1 do
           begin
             if PCardinal(P)^ = PCardinal(@Value)^ then Exit(I);
             Inc(P, 4);
           end;
        8: for I := 0 to LCount - 1 do
           begin
             if PUInt64(P)^ = PUInt64(@Value)^ then Exit(I);
             Inc(P, 8);
           end;
      end;
    tkFloat:
      case SizeOf(T) of
        4: for I := 0 to LCount - 1 do
           begin
             if PSingle(P)^ = PSingle(@Value)^ then Exit(I);
             Inc(P, 4);
           end;
        8: for I := 0 to LCount - 1 do
           begin
             if PDouble(P)^ = PDouble(@Value)^ then Exit(I);
             Inc(P, 8);
           end;
      end;
    tkInt64:
      for I := 0 to LCount - 1 do
      begin
        if PInt64(P)^ = PInt64(@Value)^ then Exit(I);
        Inc(P, 8);
      end;
  else
    for I := 0 to LCount - 1 do
    begin
      if CompareMem(P, @Value, SizeOf(T)) then Exit(I);
      Inc(P, SizeOf(T));
    end;
  end;
  Result := -1;
end;

function TList<T>.Contains(const Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TList<T>.Remove(const Value: T): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOf(Value);
  if Idx >= 0 then
  begin
    RemoveAt(Idx);
    Result := True;
  end
  else
    Result := False;
end;

function TList<T>.Extract(const Value: T): T;
var
  Idx: Integer;
begin
  Idx := IndexOf(Value);
  if Idx >= 0 then
  begin
    Result := GetItem(Idx);
    // Delete from storage without freeing (notify as Extracted)
    FCore.DeleteRaw(Idx);
    Notify(Self, Result, cnExtracted);
  end
  else
    Result := Default(T);
end;

procedure TList<T>.Delete(Index: Integer);
begin
  RemoveAt(Index);
end;

procedure TList<T>.RemoveAt(Index: Integer);
var
  LCount: Integer;
  LData: PByte;
begin
  if not FIsManaged then
  begin
    LCount := FCore.Count;
    if (Index >= 0) and (Index < LCount) then
    begin
      if Index < LCount - 1 then
      begin
        LData := FCore.Data;
        System.Move((LData + (Index + 1) * SizeOf(T))^, (LData + Index * SizeOf(T))^, (LCount - Index - 1) * SizeOf(T));
      end;
      FCore.FastDecrementCount; // Optimized decrement
      Exit;
    end;
  end;

  if FOwnsObjects and FIsClass then
    Notify(Self, GetItem(Index), cnRemoved);
  FCore.DeleteRaw(Index);
end;

procedure TList<T>.Clear;
var
  I: Integer;
begin
  if FOwnsObjects and FIsClass then
  begin
    for I := FCore.Count - 1 downto 0 do
      Notify(Self, GetItem(I), cnRemoved);
  end;
  FCore.Clear;
end;

// LINQ-like Implementation

function TList<T>.Where(const Predicate: TFunc<T, Boolean>): IList<T>;
var
  I: Integer;
  Item: T;
  NewList: IList<T>;
begin
  NewList := TCollections.CreateList<T>(False);
  Result := NewList;
  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if Predicate(Item) then
      NewList.Add(Item);
  end;
end;

function TList<T>.Where(const Expression: IExpression): IList<T>;
var
  I: Integer;
  Item: T;
  NewList: IList<T>;
begin
  NewList := TCollections.CreateList<T>(False);
  Result := NewList;

  if PTypeInfo(System.TypeInfo(T)).Kind <> tkClass then
    raise Exception.Create('Expression evaluation is only supported for class types.');

  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if TExpressionEvaluator.Evaluate(Expression, TObject(PPointer(@Item)^)) then
      NewList.Add(Item);
  end;
end;

function TList<T>.First: T;
begin
  if FCore.Count = 0 then
    raise Exception.Create('List is empty');
  Result := GetItem(0);
end;

function TList<T>.Last: T;
begin
  if FCore.Count = 0 then
    raise Exception.Create('List is empty');
  Result := GetItem(FCore.Count - 1);
end;

function TList<T>.First(const Expression: IExpression): T;
var
  I: Integer;
  Item: T;
begin
  if PTypeInfo(System.TypeInfo(T)).Kind <> tkClass then
    raise Exception.Create('Expression evaluation is only supported for class types.');

  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if TExpressionEvaluator.Evaluate(Expression, TObject(PPointer(@Item)^)) then
      Exit(Item);
  end;
  raise Exception.Create('Sequence contains no matching element');
end;

function TList<T>.FirstOrDefault: T;
begin
  if FCore.Count = 0 then
    Result := Default(T)
  else
    Result := GetItem(0);
end;

function TList<T>.FirstOrDefault(const DefaultValue: T): T;
begin
  if FCore.Count = 0 then
    Result := DefaultValue
  else
    Result := GetItem(0);
end;

function TList<T>.FirstOrDefault(const Expression: IExpression): T;
var
  I: Integer;
  Item: T;
begin
  if PTypeInfo(System.TypeInfo(T)).Kind <> tkClass then
    raise Exception.Create('Expression evaluation is only supported for class types.');

  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if TExpressionEvaluator.Evaluate(Expression, TObject(PPointer(@Item)^)) then
      Exit(Item);
  end;
  Result := Default(T);
end;

function TList<T>.Any(const Predicate: TFunc<T, Boolean>): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCore.Count - 1 do
    if Predicate(GetItem(I)) then
      Exit(True);
  Result := False;
end;

function TList<T>.Any(const Expression: IExpression): Boolean;
var
  I: Integer;
  Item: T;
begin
  if PTypeInfo(System.TypeInfo(T)).Kind <> tkClass then
    raise Exception.Create('Expression evaluation is only supported for class types.');

  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if TExpressionEvaluator.Evaluate(Expression, TObject(PPointer(@Item)^)) then
      Exit(True);
  end;
  Result := False;
end;

function TList<T>.Any: Boolean;
begin
  Result := FCore.Count > 0;
end;

function TList<T>.All(const Predicate: TFunc<T, Boolean>): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCore.Count - 1 do
    if not Predicate(GetItem(I)) then
      Exit(False);
  Result := True;
end;

function TList<T>.All(const Expression: IExpression): Boolean;
var
  I: Integer;
  Item: T;
begin
  if PTypeInfo(System.TypeInfo(T)).Kind <> tkClass then
    raise Exception.Create('Expression evaluation is only supported for class types.');

  for I := 0 to FCore.Count - 1 do
  begin
    Item := GetItem(I);
    if not TExpressionEvaluator.Evaluate(Expression, TObject(PPointer(@Item)^)) then
      Exit(False);
  end;
  Result := True;
end;

procedure TList<T>.ForEach(const Action: TProc<T>);
type
  P_T = ^T;
var
  I: Integer;
  P: PByte;
begin
  if FCore.Count = 0 then Exit;
  P := FCore.Data;
  for I := 0 to FCore.Count - 1 do
  begin
    Action(P_T(P)^);
    Inc(P, SizeOf(T));
  end;
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
type P_T = ^T;
var
  LComparer: IComparer<T>;
  I, J, LCount: Integer;
  LData: PByte;
begin
  LCount := FCore.Count;
  if LCount < 2 then Exit;

  // FAST PATH: small unmanaged list (like TList<Integer>)
  if (AComparer = nil) and (not FCore.IsManaged) and (LCount < 48) then
  begin
    LData := FCore.Data;
    case SizeOf(T) of
      4: begin
           var Arr := PRawIntArray(LData);
           for I := 1 to LCount - 1 do begin
             var V := Arr^[I]; J := I;
             while (J > 0) and (Arr^[J-1] > V) do begin Arr^[J] := Arr^[J-1]; Dec(J); end;
             Arr^[J] := V;
           end;
           Exit;
         end;
      8: begin
           var Arr := PRawInt64Array(LData);
           for I := 1 to LCount - 1 do begin
             var V := Arr^[I]; J := I;
             while (J > 0) and (Arr^[J-1] > V) do begin Arr^[J] := Arr^[J-1]; Dec(J); end;
             Arr^[J] := V;
           end;
           Exit;
         end;
    end;
  end;

  // Standard path for primitives
  if (AComparer = nil) and ((not FCore.IsManaged) or (FTypeKind = tkUString)) then
  begin
    FCore.SortRaw(FTypeKind);
    Exit;
  end;

  if AComparer <> nil then LComparer := AComparer else LComparer := TComparer<T>.Default;

  FCore.SortRaw(
    function(A, B: Pointer): Integer
    begin
      Result := LComparer.Compare(P_T(A)^, P_T(B)^);
    end);
end;

function TList<T>.ToArray: TArray<T>;
begin
  SetLength(Result, FCore.Count);
  if FCore.Count > 0 then
    FCore.GetRawData(@Result[0]);
end;

{ TCollections }

class function TCollections.CreateList<T>(OwnsObjects: Boolean): IList<T>;
begin
  Result := TList<T>.Create(OwnsObjects);
end;

class function TCollections.CreateObjectList<T>(OwnsObjects: Boolean): IList<T>;
begin
  Result := TList<T>.Create(OwnsObjects);
end;

class function TCollections.CreateDictionary<K, V>(ACapacity: Integer): IDictionary<K, V>;
begin
  Result := TDictionary<K, V>.Create(ACapacity);
end;

class function TCollections.CreateDictionary<K, V>(AOwnsValues: Boolean; ACapacity: Integer): IDictionary<K, V>;
begin
  Result := TDictionary<K, V>.Create(AOwnsValues, ACapacity);
end;

class function TCollections.CreateDictionaryIgnoreCase<K, V>(AOwnsValues: Boolean; ACapacity: Integer): IDictionary<K, V>;
begin
  Result := TDictionary<K, V>.Create(True, AOwnsValues, ACapacity);
end;

class function TCollections.CreateStack<T>: IStack<T>;
begin
  Result := TStack<T>.Create;
end;

class function TCollections.CreateQueue<T>: IQueue<T>;
begin
  Result := TQueue<T>.Create;
end;

class function TCollections.CreateHashSet<T>: IHashSet<T>;
begin
  Result := THashSet<T>.Create;
end;

end.
