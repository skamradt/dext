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
{                                                                           }
{***************************************************************************}
unit Dext.Configuration.Core;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext.Collections,
  Dext.Collections.Dict,
  Dext.Collections.Comparers,
  Dext.Collections.Algorithms,
  Dext.Configuration.Interfaces;

type
  /// <summary>
  ///   Base helper class for implementing IConfigurationProvider
  /// </summary>
  TConfigurationProvider = class(TInterfacedObject, IConfigurationProvider)
  protected
    FData: IDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    
    function TryGet(const Key: string; out Value: string): Boolean; virtual;
    procedure Set_(const Key, Value: string); virtual;
    procedure Load; virtual;
    function GetChildKeys(const EarlierKeys: TArray<string>; const ParentPath: string): TArray<string>; virtual;
  end;

  TConfigurationSection = class(TInterfacedObject, IConfigurationSection, IConfiguration)
  private
    FRoot: IConfigurationRoot;
    FPath: string;
    FKey: string;
  public
    constructor Create(const Root: IConfigurationRoot; const Path: string);
    
    // IConfigurationSection
    function GetKey: string;
    function GetPath: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
    
    // IConfiguration
    function GetItem(const Key: string): string;
    procedure SetItem(const Key, Value: string);
    function GetSection(const Key: string): IConfigurationSection;
    function GetChildren: TArray<IConfigurationSection>;
  end;

  TConfigurationRoot = class(TInterfacedObject, IConfigurationRoot, IConfiguration)
  private
    FProviders: IList<IConfigurationProvider>;
    
    function GetConfiguration(const Key: string): string;
    procedure SetConfiguration(const Key, Value: string);

  public
    constructor Create(const Providers: IList<IConfigurationProvider>);
    destructor Destroy; override;
    
    procedure Reload;
    function GetSectionChildren(const Path: string): TArray<IConfigurationSection>;
    
    // IConfiguration
    function GetItem(const Key: string): string;
    procedure SetItem(const Key, Value: string);
    function GetSection(const Key: string): IConfigurationSection;
    function GetChildren: TArray<IConfigurationSection>;
  end;

  TConfigurationBuilder = class(TInterfacedObject, IConfigurationBuilder)
  private
    FSources: IList<IConfigurationSource>;
    FProperties: IDictionary<string, TObject>;
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetSources: IList<IConfigurationSource>;
    function GetProperties: IDictionary<string, TObject>;
    
    function Add(Source: IConfigurationSource): IConfigurationBuilder;
    function Build: IConfigurationRoot;
  end;

  TMemoryConfigurationProvider = class(TConfigurationProvider)
  public
    constructor Create(Data: IDictionary<string, string>);
    procedure Load; override;
  end;

  TMemoryConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FData: IDictionary<string, string>;
  public
    constructor Create(Data: IEnumerable<TPair<string, string>>); overload;
    constructor Create(const Data: array of TPair<string, string>); overload;
    destructor Destroy; override;
    function Build(Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  /// <summary>
  ///   Fluent wrapper for IConfigurationBuilder.
  /// </summary>
  TDextConfiguration = record
  private
    FBuilder: IConfigurationBuilder;
  public
    constructor Create(const ABuilder: IConfigurationBuilder);
    class function New: TDextConfiguration; static;

    function Add(const ASource: IConfigurationSource): TDextConfiguration;
    function AddValues(const AValues: array of TPair<string, string>): TDextConfiguration;
    function Build: IConfigurationRoot;
    function Unwrap: IConfigurationBuilder;
  end;

  /// <summary>
  ///   Static helper for configuration paths
  /// </summary>
  TConfigurationPath = class
  public
    const KeyDelimiter = ':';
    class function Combine(const Path, Key: string): string;
    class function GetSectionKey(const Path: string): string;
    class function GetParentPath(const Path: string): string;
  end;

implementation

{ TConfigurationProvider }

constructor TConfigurationProvider.Create;
begin
  inherited;
  FData := TCollections.CreateDictionaryIgnoreCase<string, string>;
end;

destructor TConfigurationProvider.Destroy;
begin
  FData := nil;
  inherited;
end;

function TConfigurationProvider.TryGet(const Key: string; out Value: string): Boolean;
begin
  Result := FData.TryGetValue(Key, Value);
end;

procedure TConfigurationProvider.Set_(const Key, Value: string);
begin
  FData.AddOrSetValue(Key, Value);
end;

procedure TConfigurationProvider.Load;
begin
  // Base implementation does nothing
end;

function TConfigurationProvider.GetChildKeys(const EarlierKeys: TArray<string>; const ParentPath: string): TArray<string>;
var
  Results: IList<string>;
  Key: string;
  Segment: string;
  Prefix: string;
  Len: Integer;
begin
  Results := TCollections.CreateList<string>;
  try
    Results.AddRange(EarlierKeys);
    
    if ParentPath = '' then
      Prefix := ''
    else
      Prefix := ParentPath + TConfigurationPath.KeyDelimiter;
      
    Len := Length(Prefix);
    
    for var Pair in FData do
    begin
      Key := Pair.Key;
      if (Len = 0) or (Key.StartsWith(Prefix, True)) then
      begin
        Segment := Key.Substring(Len);
        var DelimiterIndex := Segment.IndexOf(TConfigurationPath.KeyDelimiter);
        if DelimiterIndex >= 0 then
          Segment := Segment.Substring(0, DelimiterIndex);
          
        if not Results.Contains(Segment) then
          Results.Add(Segment);
      end;
    end;
    
    Result := Results.ToArray;
    TDextSort.Sort<string>(Result, TComparer<string>.Default);
  finally
    Results := nil;
  end;
end;

{ TConfigurationSection }

constructor TConfigurationSection.Create(const Root: IConfigurationRoot; const Path: string);
begin
  inherited Create;
  FRoot := Root;
  FPath := Path;
  FKey := TConfigurationPath.GetSectionKey(Path);
end;

function TConfigurationSection.GetKey: string;
begin
  Result := FKey;
end;

function TConfigurationSection.GetPath: string;
begin
  Result := FPath;
end;

function TConfigurationSection.GetValue: string;
begin
  Result := FRoot[FPath];
end;

procedure TConfigurationSection.SetValue(const Value: string);
begin
  FRoot[FPath] := Value;
end;

function TConfigurationSection.GetItem(const Key: string): string;
begin
  Result := FRoot[TConfigurationPath.Combine(FPath, Key)];
end;

procedure TConfigurationSection.SetItem(const Key, Value: string);
begin
  FRoot[TConfigurationPath.Combine(FPath, Key)] := Value;
end;

function TConfigurationSection.GetSection(const Key: string): IConfigurationSection;
begin
  Result := FRoot.GetSection(TConfigurationPath.Combine(FPath, Key));
end;

function TConfigurationSection.GetChildren: TArray<IConfigurationSection>;
begin
  Result := FRoot.GetSectionChildren(FPath);
end;

{ TConfigurationRoot }

constructor TConfigurationRoot.Create(const Providers: IList<IConfigurationProvider>);
begin
  inherited Create;
  FProviders := TCollections.CreateList<IConfigurationProvider>;
  for var Provider in Providers do
    FProviders.Add(Provider);
  
  for var Provider in FProviders do
    Provider.Load;
end;

destructor TConfigurationRoot.Destroy;
begin
  FProviders := nil;
  inherited;
end;

procedure TConfigurationRoot.Reload;
begin
  for var Provider in FProviders do
    Provider.Load;
end;

function TConfigurationRoot.GetConfiguration(const Key: string): string;
var
  Value: string;
begin
  Result := '';
  // Reverse order: last provider wins
  for var I := FProviders.Count - 1 downto 0 do
  begin
    if FProviders[I].TryGet(Key, Value) then
      Exit(Value);
  end;
end;

procedure TConfigurationRoot.SetConfiguration(const Key, Value: string);
begin
  // Set in all providers? Or just the first one that supports it?
  // Usually configuration is read-only from file sources, but memory source is writable.
  // .NET sets it in all providers.
  for var Provider in FProviders do
    Provider.Set_(Key, Value);
end;

function TConfigurationRoot.GetItem(const Key: string): string;
begin
  Result := GetConfiguration(Key);
end;

procedure TConfigurationRoot.SetItem(const Key, Value: string);
begin
  SetConfiguration(Key, Value);
end;

function TConfigurationRoot.GetSection(const Key: string): IConfigurationSection;
begin
  Result := TConfigurationSection.Create(Self, Key);
end;

// Helper for internal use
function TConfigurationRoot.GetSectionChildren(const Path: string): TArray<IConfigurationSection>;
var
  Keys: TArray<string>;
  Provider: IConfigurationProvider;
  ChildPath: string;
begin
  Keys := [];
  for Provider in FProviders do
  begin
    Keys := Provider.GetChildKeys(Keys, Path);
  end;
  
    // Keys are already distinct per provider logic usually, but we merge them.
    // Provider.GetChildKeys usually adds to existing.
    
    SetLength(Result, Length(Keys));
    for var I := 0 to High(Keys) do
    begin
      ChildPath := TConfigurationPath.Combine(Path, Keys[I]);
      Result[I] := TConfigurationSection.Create(Self, ChildPath);
    end;
end;

function TConfigurationRoot.GetChildren: TArray<IConfigurationSection>;
begin
  Result := GetSectionChildren('');
end;

{ TConfigurationBuilder }

constructor TConfigurationBuilder.Create;
begin
  inherited;
  FSources := TCollections.CreateList<IConfigurationSource>;
  FProperties := TCollections.CreateDictionary<string, TObject>;
end;

destructor TConfigurationBuilder.Destroy;
begin
  FSources := nil;
  FProperties := nil;
  inherited;
end;

function TConfigurationBuilder.GetSources: IList<IConfigurationSource>;
begin
  Result := FSources;
end;

function TConfigurationBuilder.GetProperties: IDictionary<string, TObject>;
begin
  Result := FProperties;
end;

function TConfigurationBuilder.Add(Source: IConfigurationSource): IConfigurationBuilder;
begin
  FSources.Add(Source);
  Result := Self;
end;

function TConfigurationBuilder.Build: IConfigurationRoot;
var
  Providers: IList<IConfigurationProvider>;
begin
  Providers := TCollections.CreateList<IConfigurationProvider>;
  try
    for var Source in FSources do
    begin
      var Provider := Source.Build(Self);
      if Assigned(Provider) then
        Providers.Add(Provider);
    end;
    
    Result := TConfigurationRoot.Create(Providers);
  finally
    Providers := nil;
  end;
end;

{ TDextConfiguration }

constructor TDextConfiguration.Create(const ABuilder: IConfigurationBuilder);
begin
  FBuilder := ABuilder;
end;

class function TDextConfiguration.New: TDextConfiguration;
begin
  Result := TDextConfiguration.Create(TConfigurationBuilder.Create);
end;

function TDextConfiguration.Add(const ASource: IConfigurationSource): TDextConfiguration;
begin
  FBuilder.Add(ASource);
  Result := Self;
end;

function TDextConfiguration.AddValues(const AValues: array of TPair<string, string>): TDextConfiguration;
begin
  Result := Add(TMemoryConfigurationSource.Create(AValues));
end;

function TDextConfiguration.Build: IConfigurationRoot;
begin
  Result := FBuilder.Build;
end;

function TDextConfiguration.Unwrap: IConfigurationBuilder;
begin
  Result := FBuilder;
end;

{ TMemoryConfigurationProvider }

constructor TMemoryConfigurationProvider.Create(Data: IDictionary<string, string>);
begin
  inherited Create;
  if Data <> nil then
  begin
    for var Pair in Data do
      FData.Add(Pair.Key, Pair.Value);
  end;
end;

procedure TMemoryConfigurationProvider.Load;
begin
  // Already loaded in constructor
end;

{ TMemoryConfigurationSource }

constructor TMemoryConfigurationSource.Create(Data: IEnumerable<TPair<string, string>>);
begin
  inherited Create;
  FData := TCollections.CreateDictionary<string, string>;
  if Data <> nil then
  begin
    for var Pair in Data do
      FData.Add(Pair.Key, Pair.Value);
  end;
end;

constructor TMemoryConfigurationSource.Create(const Data: array of TPair<string, string>);
begin
  inherited Create;
  FData := TCollections.CreateDictionary<string, string>;
  for var Pair in Data do
    FData.Add(Pair.Key, Pair.Value);
end;

destructor TMemoryConfigurationSource.Destroy;
begin
  FData := nil;
  inherited;
end;

function TMemoryConfigurationSource.Build(Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TMemoryConfigurationProvider.Create(FData);
end;

{ TConfigurationPath }

class function TConfigurationPath.Combine(const Path, Key: string): string;
begin
  if Path = '' then
    Result := Key
  else
    Result := Path + KeyDelimiter + Key;
end;

class function TConfigurationPath.GetSectionKey(const Path: string): string;
var
  LastDelimiter: Integer;
begin
  if Path = '' then
    Exit('');
    
  LastDelimiter := Path.LastIndexOf(KeyDelimiter);
  if LastDelimiter < 0 then
    Result := Path
  else
    Result := Path.Substring(LastDelimiter + 1);
end;

class function TConfigurationPath.GetParentPath(const Path: string): string;
var
  LastDelimiter: Integer;
begin
  if Path = '' then
    Exit('');
    
  LastDelimiter := Path.LastIndexOf(KeyDelimiter);
  if LastDelimiter < 0 then
    Result := ''
  else
    Result := Path.Substring(0, LastDelimiter);
end;

end.

