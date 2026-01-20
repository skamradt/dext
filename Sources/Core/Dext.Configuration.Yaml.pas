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
unit Dext.Configuration.Yaml;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  Dext.Configuration.Interfaces,
  Dext.Configuration.Core,
  Dext.Yaml;

type
  /// <summary>
  ///   Configuration provider that reads from a YAML file.
  /// </summary>
  TYamlConfigurationProvider = class(TConfigurationProvider)
  private
    FPath: string;
    FOptional: Boolean;
    function ResolveFilePath: string;
    procedure FlattenNode(Node: TYamlNode; const Prefix: string);
  public
    constructor Create(const Path: string; Optional: Boolean);
    procedure Load; override;
  end;

  /// <summary>
  ///   Configuration source for YAML files.
  /// </summary>
  TYamlConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FPath: string;
    FOptional: Boolean;
  public
    constructor Create(const Path: string; Optional: Boolean = False);
    function Build(Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  /// <summary>
  ///   Fluent builder for YAML configuration.
  /// </summary>
  TYamlConfigurationBuilder = record
  public
    class function Create: TDextConfiguration; static;
  end;

  /// <summary>
  ///   Extensions for TDextConfiguration to support YAML.
  /// </summary>
  TDextConfigurationYamlExtensions = record helper for TDextConfiguration
  public
    function AddYamlFile(const Path: string; Optional: Boolean = False): TDextConfiguration;
  end;

implementation

{ TYamlConfigurationSource }

constructor TYamlConfigurationSource.Create(const Path: string; Optional: Boolean);
begin
  inherited Create;
  FPath := Path;
  FOptional := Optional;
end;

function TYamlConfigurationSource.Build(Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TYamlConfigurationProvider.Create(FPath, FOptional);
end;

{ TYamlConfigurationProvider }

constructor TYamlConfigurationProvider.Create(const Path: string; Optional: Boolean);
begin
  inherited Create;
  FPath := Path;
  FOptional := Optional;
end;

procedure TYamlConfigurationProvider.Load;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Content: string;
  ResolvedPath: string;
begin
  ResolvedPath := ResolveFilePath;

  if ResolvedPath = '' then
  begin
    if FOptional then
      Exit
    else
      raise EConfigurationException.CreateFmt('Configuration file not found: %s', [FPath]);
  end;

  try
    Content := TFile.ReadAllText(ResolvedPath, TEncoding.UTF8);
  except
    on E: Exception do
      raise EConfigurationException.CreateFmt('Failed to read configuration file %s: %s', [FPath, E.Message]);
  end;

  if Content.Trim.IsEmpty then Exit;

  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse(Content);
    try
      FData.Clear;
      if Doc.Root <> nil then
        FlattenNode(Doc.Root, '');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TYamlConfigurationProvider.ResolveFilePath: string;
begin
  if FileExists(FPath) then
    Exit(FPath);

  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), FPath);
  if FileExists(Result) then
    Exit;

  Result := '';
end;

procedure TYamlConfigurationProvider.FlattenNode(Node: TYamlNode; const Prefix: string);
var
  Key: string;
  I: Integer;
begin
  if Node = nil then Exit;

  case Node.GetNodeType of
    yntScalar:
      begin
        // If it's a scalar at root, it doesn't make much sense for config key-value pairs
        // unless the prefix is empty. But typically config is a map.
        // If we are recursing, Prefix is the key.
        if Prefix <> '' then
          FData.AddOrSetValue(Prefix, (Node as TYamlScalar).Value);
      end;
      
    yntMapping:
      begin
        var Mapping := Node as TYamlMapping;
        for var Pair in Mapping.Children do
        begin
          Key := Pair.Key;
          if Prefix <> '' then
            Key := TConfigurationPath.Combine(Prefix, Key);
            
          FlattenNode(Pair.Value, Key);
        end;
      end;
      
    yntSequence:
      begin
        var Seq := Node as TYamlSequence;
        for I := 0 to Seq.Items.Count - 1 do
        begin
          Key := IntToStr(I);
          if Prefix <> '' then
            Key := TConfigurationPath.Combine(Prefix, Key);
            
          FlattenNode(Seq.Items[I], Key);
        end;
      end;
  end;
end;

{ TYamlConfigurationBuilder }

class function TYamlConfigurationBuilder.Create: TDextConfiguration;
begin
  Result := TDextConfiguration.New;
end;

{ TDextConfigurationYamlExtensions }

function TDextConfigurationYamlExtensions.AddYamlFile(const Path: string; Optional: Boolean): TDextConfiguration;
begin
  Result := Add(TYamlConfigurationSource.Create(Path, Optional));
end;

end.
