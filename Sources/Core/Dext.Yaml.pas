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
{  Created: 2026-01-05                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Yaml;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Character;

type
  EYamlException = class(Exception);

  TYamlNodeType = (yntScalar, yntMapping, yntSequence);

  TYamlNode = class
  private
    FParent: TYamlNode;
  public
    property Parent: TYamlNode read FParent write FParent;
    function GetNodeType: TYamlNodeType; virtual; abstract;
    function ToYaml(Indent: Integer = 0): string; virtual; abstract;
  end;

  TYamlScalar = class(TYamlNode)
  private
    FValue: string;
  public
    constructor Create(const AValue: string = '');
    function GetNodeType: TYamlNodeType; override;
    function ToYaml(Indent: Integer = 0): string; override;
    property Value: string read FValue write FValue;
  end;

  TYamlMapping = class(TYamlNode)
  private
    FChildren: TObjectDictionary<string, TYamlNode>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetNodeType: TYamlNodeType; override;
    function ToYaml(Indent: Integer = 0): string; override;
    procedure Add(const Key: string; Node: TYamlNode);
    function TryGet(const Key: string; out Node: TYamlNode): Boolean;
    property Children: TObjectDictionary<string, TYamlNode> read FChildren;
  end;

  TYamlSequence = class(TYamlNode)
  private
    FItems: TObjectList<TYamlNode>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetNodeType: TYamlNodeType; override;
    function ToYaml(Indent: Integer = 0): string; override;
    procedure Add(Node: TYamlNode);
    property Items: TObjectList<TYamlNode> read FItems;
  end;

  TYamlDocument = class
  private
    FRoot: TYamlNode;
  public
    constructor Create(Root: TYamlNode);
    destructor Destroy; override;
    property Root: TYamlNode read FRoot;
    procedure SaveToFile(const FileName: string);
  end;

  TYamlParser = class
  private
    FLines: TStringList;
    FCurrentLine: Integer;
    
    function ParseIndent(const Line: string): Integer;
    function ParseNode(ParentIndent: Integer): TYamlNode;
    function PeekLine: string;
    function ConsumeLine: string;
    function IsEOF: Boolean;
    function ParseMapping(Indent: Integer; const FirstLine: string): TYamlMapping;
    function ParseSequence(Indent: Integer; const FirstLine: string): TYamlSequence;
    
    class function TrimComment(const Line: string): string; static;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const YamlContent: string): TYamlDocument;
  end;

implementation

{ TYamlScalar }

constructor TYamlScalar.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TYamlScalar.GetNodeType: TYamlNodeType;
begin
  Result := yntScalar;
end;

function TYamlScalar.ToYaml(Indent: Integer = 0): string;
begin
  // Basic scalar handling. Does not handle complex multiline or needed quotes logic yet.
  Result := FValue;
end;

{ TYamlMapping }

constructor TYamlMapping.Create;
begin
  inherited Create;
  FChildren := TObjectDictionary<string, TYamlNode>.Create([doOwnsValues]);
end;

destructor TYamlMapping.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TYamlMapping.GetNodeType: TYamlNodeType;
begin
  Result := yntMapping;
end;

function TYamlMapping.ToYaml(Indent: Integer = 0): string;
var
  SB: TStringBuilder;
  Spaces: string;
begin
  SB := TStringBuilder.Create;
  try
    Spaces := StringOfChar(' ', Indent);
    
    // Sort keys for deterministic output? Not required by YAML but nice.
    // TDictionary is unordered.
    var Keys := TList<string>.Create;
    try
      for var Key in FChildren.Keys do
        Keys.Add(Key);
      Keys.Sort;
      
      for var I := 0 to Keys.Count - 1 do
      begin
        var Key := Keys[I];
        var Node := FChildren[Key];
        
        if I > 0 then
          SB.AppendLine;
          
        SB.Append(Spaces).Append(Key).Append(':');
        
        if Node is TYamlScalar then
        begin
          SB.Append(' ').Append(Node.ToYaml(0));
        end
        else if Node is TYamlSequence then
        begin
          SB.AppendLine;
          SB.Append(Node.ToYaml(Indent)); // Sequence items carry their own bullets
        end
        else if Node is TYamlMapping then
        begin
          SB.AppendLine;
          SB.Append(Node.ToYaml(Indent + 2));
        end;
      end;
    finally
      Keys.Free;
    end;
    
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TYamlMapping.Add(const Key: string; Node: TYamlNode);
begin
  Node.Parent := Self;
  FChildren.AddOrSetValue(Key, Node);
end;

function TYamlMapping.TryGet(const Key: string; out Node: TYamlNode): Boolean;
begin
  Result := FChildren.TryGetValue(Key, Node);
end;

{ TYamlSequence }

constructor TYamlSequence.Create;
begin
  inherited Create;
  FItems := TObjectList<TYamlNode>.Create(True);
end;

destructor TYamlSequence.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TYamlSequence.GetNodeType: TYamlNodeType;
begin
  Result := yntSequence;
end;

function TYamlSequence.ToYaml(Indent: Integer = 0): string;
var
  SB: TStringBuilder;
  Spaces: string;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    Spaces := StringOfChar(' ', Indent);
    for I := 0 to FItems.Count - 1 do
    begin
      if I > 0 then
        SB.AppendLine;
        
      var Item := FItems[I];
      SB.Append(Spaces).Append('- ');
      
      if Item is TYamlScalar then
      begin
        SB.Append(Item.ToYaml(0));
      end
      else if Item is TYamlMapping then
      begin
        // Complex mapping inside sequence
        // The first line of mapping can start on the same line as "- " if it's compact?
        // For simplicity, let's treat it as block.
        // But standard YAML allows:
        // - key: value
        //   key2: value
        
        // We need to render the mapping but adjust indentation of first line?
        // Or just render it indent+2.
        
        // Let's implement a simple block strategy:
        // - 
        //   key: value
        // OR
        // - key: value
        
        // Our Mapping.ToYaml(Indent) returns full block.
        // We can trim start? 
        
        // Let's use a specialized Indent strategy.
        // We render the Mapping with (Indent + 2).
        // Then we check if the first line can be hoisted.
        
        // Hacky for now: just new line + indent
         // SB.AppendLine;
         // SB.Append(Item.ToYaml(Indent + 2));
         
         // Better:
         // - key1: val1
         //   key2: val2
         
         var MapYaml := Item.ToYaml(0); // Generate with 0 indent to inspect
         var Lines := MapYaml.Split([sLineBreak], TStringSplitOptions.None);
         
         for var L := 0 to Length(Lines) - 1 do
         begin
           if L = 0 then
              SB.Append(Lines[L])
           else
           begin
              SB.AppendLine;
              SB.Append(Spaces).Append('  ').Append(Lines[L]);
           end;
         end;
      end
      else // Sequence in Sequence
      begin
         SB.AppendLine;
         SB.Append(Item.ToYaml(Indent + 2));
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TYamlSequence.Add(Node: TYamlNode);
begin
  Node.Parent := Self;
  FItems.Add(Node);
end;

{ TYamlDocument }

constructor TYamlDocument.Create(Root: TYamlNode);
begin
  inherited Create;
  FRoot := Root;
end;

destructor TYamlDocument.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TYamlDocument.SaveToFile(const FileName: string);
begin
  if FRoot = nil then
    TFile.WriteAllText(FileName, '')
  else
    TFile.WriteAllText(FileName, FRoot.ToYaml(0));
end;

{ TYamlParser }

constructor TYamlParser.Create;
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor TYamlParser.Destroy;
begin
  FLines.Free;
  inherited;
end;

class function TYamlParser.TrimComment(const Line: string): string;
var
  Idx: Integer;
begin
  // Simple comment handling: # to end of line
  // Note: Doesn't handle # inside quotes strings correctly yet
  Idx := Line.IndexOf('#');
  if Idx < 0 then
    Result := Line
  else
    Result := Line.Substring(0, Idx);
  Result := Result.TrimRight;
end;

function TYamlParser.ParseIndent(const Line: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Line.Length do
  begin
    if Line[I] = ' ' then
      Inc(Result)
    else
      Break;
  end;
end;

function TYamlParser.PeekLine: string;
begin
  if IsEOF then
    Exit('');
  Result := FLines[FCurrentLine];
end;

function TYamlParser.ConsumeLine: string;
begin
  Result := PeekLine;
  if not IsEOF then
    Inc(FCurrentLine);
end;

function TYamlParser.IsEOF: Boolean;
begin
  Result := FCurrentLine >= FLines.Count;
end;

function TYamlParser.Parse(const YamlContent: string): TYamlDocument;
var
  Root: TYamlNode;
begin
  FLines.Text := YamlContent;
  FCurrentLine := 0;
  
  // Skip empty lines at start
  while not IsEOF and (TrimComment(PeekLine).Trim.IsEmpty) do
    ConsumeLine;

  if IsEOF then
    Exit(TYamlDocument.Create(nil));

  // Determine root type.
  // We assume standard block scalars or mappings.
  // Since multiple documents in one file are not supported yet, we parse the first block.
  
  // A root level items usually start with 0 indent.
  // If line starts with "- ", it's a sequence.
  // If line matches "key: ...", it's a mapping.
  // Else scalar.
  
  var Line := TrimComment(PeekLine);
  var Trimmed := Line.Trim;
  
  if (Trimmed.StartsWith('- ') or (Trimmed = '-')) then
    Root := ParseSequence(0, Line)
  else if Trimmed.Contains(':') then // Very basic heuristic
    Root := ParseMapping(0, Line)
  else
    Root := TYamlScalar.Create(Trimmed);

  Result := TYamlDocument.Create(Root);
end;

function TYamlParser.ParseNode(ParentIndent: Integer): TYamlNode;
var
  Line, Content: string;
  Indent: Integer;
begin
  if IsEOF then Exit(nil);

  Line := TrimComment(PeekLine);
  Content := Line.Trim;
  
  // Skip empty
  if Content.IsEmpty then
  begin
    ConsumeLine;
    Exit(ParseNode(ParentIndent));
  end;

  Indent := ParseIndent(Line);
  if Indent < ParentIndent then
    Exit(nil); // End of current block

  if (Content.StartsWith('- ') or (Content = '-')) then
    Result := ParseSequence(Indent, Line)
  else if Content.Contains(':') then
    Result := ParseMapping(Indent, Line)
  else
  begin
    ConsumeLine;
    Result := TYamlScalar.Create(Content);
  end;
end;

function TYamlParser.ParseMapping(Indent: Integer; const FirstLine: string): TYamlMapping;
var
  Mapping: TYamlMapping;
  Line, Trimmed: string;
  CurrentIndent: Integer;
  ColonPos: Integer;
  Key, ValueStr: string;
begin
  Mapping := TYamlMapping.Create;
  try
    while not IsEOF do
    begin
      Line := TrimComment(PeekLine);
      Trimmed := Line.Trim;
      
      if Trimmed.IsEmpty then
      begin
        ConsumeLine;
        Continue;
      end;
      
      CurrentIndent := ParseIndent(Line);
      if CurrentIndent < Indent then
        Break; // End of block
        
      if CurrentIndent > Indent then
      begin
        // Unexpected indentation increase without key context?
        // Should handle nested blocks logic in value parsing.
        // For now, simple mapping expects keys at 'Indent' level.
        ConsumeLine; 
        Continue;
      end;

      // Expect key: value
      ColonPos := Trimmed.IndexOf(':');
      if ColonPos <= 0 then
      begin
        // Malformed or scalar inside mapping? 
        Break;
        // raise EYamlException.Create('Expected "key: value" in mapping');
      end;
      
      Key := Trimmed.Substring(0, ColonPos).Trim;
      ValueStr := Trimmed.Substring(ColonPos + 1).Trim;
      
      ConsumeLine; // Consumed the key line
      
      if ValueStr.IsEmpty then
      begin
        // Nested object or empty null?
        // Check next line indent
        if not IsEOF then
        begin
          var NextLine := TrimComment(PeekLine);
          var NextIndent := ParseIndent(NextLine);
          
          if NextIndent > Indent then
          begin
            // It is a nested object/sequence
             // Recursive call based on lookahead
             var NextContent := NextLine.Trim;
             if (NextContent.StartsWith('- ') or (NextContent = '-')) then
               Mapping.Add(Key, ParseSequence(NextIndent, NextLine))
             else
               Mapping.Add(Key, ParseMapping(NextIndent, NextLine));
          end
          else
            Mapping.Add(Key, TYamlScalar.Create('')); // Empty value
        end
        else
          Mapping.Add(Key, TYamlScalar.Create('')); // EOF
      end
      else
      begin
        // Inline value
        // Handle basic quotes removal?
        if (ValueStr.StartsWith('"') and ValueStr.EndsWith('"')) or 
           (ValueStr.StartsWith('''') and ValueStr.EndsWith('''')) then
             ValueStr := ValueStr.Substring(1, ValueStr.Length - 2);
             
        Mapping.Add(Key, TYamlScalar.Create(ValueStr));
      end;
    end;
    Result := Mapping;
  except
    Mapping.Free;
    raise;
  end;
end;

function TYamlParser.ParseSequence(Indent: Integer; const FirstLine: string): TYamlSequence;
var
  Seq: TYamlSequence;
  Line, Trimmed, Content: string;
  CurrentIndent: Integer;
begin
  Seq := TYamlSequence.Create;
  try
    while not IsEOF do
    begin
      Line := TrimComment(PeekLine);
      Trimmed := Line.Trim;
      
      if Trimmed.IsEmpty then
      begin
        ConsumeLine;
        Continue;
      end;

      CurrentIndent := ParseIndent(Line);
      if CurrentIndent < Indent then
        Break;

      if not Trimmed.StartsWith('-') then
        Break; // End of sequence items

      // Found item "- something" or "-"
      // Ensure it's a list item (followed by space or is just '-')
      if (Trimmed.Length > 1) and (Trimmed.Chars[1] <> ' ') then
         Break; // e.g. "-123" number, not list item

      if Trimmed.Length > 1 then
        Content := Trimmed.Substring(2).Trim
      else
        Content := ''; // Just "-"

      ConsumeLine; // Consumed item line
      
      if Content.IsEmpty then
      begin
         // Nested object in list item?
         if not IsEOF then
         begin
           var NextLine := TrimComment(PeekLine);
           var NextIndent := ParseIndent(NextLine);
           if NextIndent > Indent then
             Seq.Add(ParseMapping(NextIndent, NextLine))
           else
             Seq.Add(TYamlScalar.Create(''));
         end;
      end
      else
      begin
         // Inline scalar or flow mapping?
         // For now treat as scalar
         Seq.Add(TYamlScalar.Create(Content));
      end;
    end;
    Result := Seq;
  except
    Seq.Free;
    raise;
  end;
end;

end.
