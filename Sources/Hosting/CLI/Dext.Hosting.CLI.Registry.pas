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
unit Dext.Hosting.CLI.Registry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  System.Generics.Collections,
  Dext.Yaml;

type
  TProjectInfo = record
    Path: string;
    Name: string;
    LastAccess: TDateTime;
  end;

  TProjectRegistry = class
  private
    FRegistryPath: string;
    function GetRegistryPath: string;
    function LoadRegistry: TYamlDocument;
    procedure EnsureRegistryExists;
  public
    constructor Create;
    procedure RegisterProject(const Path: string; const Name: string = '');
    function GetAllProjects: TArray<TProjectInfo>;
  end;

implementation

{ TProjectRegistry }

constructor TProjectRegistry.Create;
begin
  FRegistryPath := GetRegistryPath;
  EnsureRegistryExists;
end;

function TProjectRegistry.GetRegistryPath: string;
var
  HomeDir: string;
begin
  HomeDir := TPath.GetHomePath;
  Result := TPath.Combine(TPath.Combine(HomeDir, '.dext'), 'projects.yaml');
end;

procedure TProjectRegistry.EnsureRegistryExists;
var
  Dir: string;
begin
  Dir := TPath.GetDirectoryName(FRegistryPath);
  if not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);
    
  if not FileExists(FRegistryPath) then
    TFile.WriteAllText(FRegistryPath, 'projects: []');
end;

function TProjectRegistry.LoadRegistry: TYamlDocument;
var
  Parser: TYamlParser;
  Content: string;
begin
  Parser := TYamlParser.Create;
  try
    if FileExists(FRegistryPath) then
      Content := TFile.ReadAllText(FRegistryPath)
    else
      Content := 'projects: []';
      
    Result := Parser.Parse(Content);
  finally
    Parser.Free;
  end;
end;

procedure TProjectRegistry.RegisterProject(const Path: string; const Name: string);
var
  Doc: TYamlDocument;
  Root: TYamlMapping;
  ProjectsSeq: TYamlSequence;
  ProjectNode: TYamlNode;
  ProjectMap: TYamlMapping;
  Found: Boolean;
  AbsPath: string;
  Node: TYamlNode;
  I: Integer;
begin
  AbsPath := ExpandFileName(Path);
  // Normalize path separators
  AbsPath := AbsPath.Replace('/', PathDelim).Replace('\', PathDelim);

  Doc := LoadRegistry;
  try
    if (Doc.Root = nil) or (Doc.Root.GetNodeType <> yntMapping) then
    begin
        // Reset if corrupted or scalar
        Doc.Free;
        Doc := TYamlDocument.Create(TYamlMapping.Create);
    end;
    
    Root := Doc.Root as TYamlMapping;
    if not Root.TryGet('projects', Node) then
    begin
       ProjectsSeq := TYamlSequence.Create;
       Root.Add('projects', ProjectsSeq);
    end
    else
    begin
       if Node.GetNodeType = yntSequence then
         ProjectsSeq := Node as TYamlSequence
       else
       begin
         // Corrupted 'projects' key
         ProjectsSeq := TYamlSequence.Create;
         // Overwrite/fix it: This is tricky with current implementation structure
         // because 'Node' is owned by Root.
         // Simple way: Clear children and restart? Or just accept risk.
         // Let's assume valid structure for now.
       end;
    end;
    
    // Check if exists
    Found := False;
    for I := 0 to ProjectsSeq.Items.Count - 1 do
    begin
      ProjectNode := ProjectsSeq.Items[I];
      if ProjectNode.GetNodeType = yntMapping then
      begin
        ProjectMap := ProjectNode as TYamlMapping;
        if ProjectMap.TryGet('path', Node) and (Node is TYamlScalar) then
        begin
          if SameText((Node as TYamlScalar).Value, AbsPath) then
          begin
            // Update timestamp
            if ProjectMap.TryGet('last_access', Node) and (Node is TYamlScalar) then
              (Node as TYamlScalar).Value := DateToISO8601(Now);
              
            // Update name if provided
            if (Name <> '') and ProjectMap.TryGet('name', Node) and (Node is TYamlScalar) then
               (Node as TYamlScalar).Value := Name;
               
            Found := True;
            Break;
          end;
        end;
      end;
    end;
    
    if not Found then
    begin
      ProjectMap := TYamlMapping.Create;
      ProjectMap.Add('path', TYamlScalar.Create(AbsPath));
      ProjectMap.Add('name', TYamlScalar.Create(IfThen(Name = '', TPath.GetFileName(AbsPath), Name)));
      ProjectMap.Add('last_access', TYamlScalar.Create(DateToISO8601(Now)));
      ProjectsSeq.Add(ProjectMap);
    end;
    
    Doc.SaveToFile(FRegistryPath);
  finally
    Doc.Free;
  end;
end;

function TProjectRegistry.GetAllProjects: TArray<TProjectInfo>;
var
  Doc: TYamlDocument;
  Root: TYamlMapping;
  ProjectsSeq: TYamlSequence;
  ProjectNode: TYamlNode;
  ProjectMap: TYamlMapping;
  Node: TYamlNode;
  I: Integer;
  List: TList<TProjectInfo>;
  Info: TProjectInfo;
begin
  List := TList<TProjectInfo>.Create;
  try
    Doc := LoadRegistry;
    try
      if (Doc.Root <> nil) and (Doc.Root.GetNodeType = yntMapping) then
      begin
        Root := Doc.Root as TYamlMapping;
        if Root.TryGet('projects', Node) and (Node.GetNodeType = yntSequence) then
        begin
          ProjectsSeq := Node as TYamlSequence;
          for I := 0 to ProjectsSeq.Items.Count - 1 do
          begin
            ProjectNode := ProjectsSeq.Items[I];
            if ProjectNode.GetNodeType = yntMapping then
            begin
              ProjectMap := ProjectNode as TYamlMapping;
              Info.Path := '';
              Info.Name := '';
              Info.LastAccess := 0;
              
              if ProjectMap.TryGet('path', Node) and (Node is TYamlScalar) then
                Info.Path := (Node as TYamlScalar).Value;
                
              if ProjectMap.TryGet('name', Node) and (Node is TYamlScalar) then
                Info.Name := (Node as TYamlScalar).Value;
                
              if ProjectMap.TryGet('last_access', Node) and (Node is TYamlScalar) then
                Info.LastAccess := ISO8601ToDate((Node as TYamlScalar).Value);
                
              if Info.Path <> '' then
                List.Add(Info);
            end;
          end;
        end;
      end;
    finally
      Doc.Free;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

end.
