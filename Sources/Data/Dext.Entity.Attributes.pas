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
unit Dext.Entity.Attributes;

interface

uses
  System.Rtti,
  System.Variants,
  Data.DB;

type
  TInheritanceStrategy = (None, TablePerHierarchy, TablePerType);

type
  TableAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    property Name: string read FName;
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  /// <summary>
  ///   Marks a property as a Primary Key.
  /// </summary>
  PrimaryKeyAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Alias for PrimaryKeyAttribute
  /// </summary>
  PKAttribute = PrimaryKeyAttribute;

  AutoIncAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as NOT NULL.
  /// </summary>
  RequiredAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Specifies the maximum length of array/string data allowed in a property.
  /// </summary>
  MaxLengthAttribute = class(TCustomAttribute)
  private
    FLength: Integer;
  public
    constructor Create(ALength: Integer);
    property Length: Integer read FLength;
  end;

  /// <summary>
  ///   Specifies the minimum length of array/string data allowed in a property.
  /// </summary>
  MinLengthAttribute = class(TCustomAttribute)
  private
    FLength: Integer;
  public
    constructor Create(ALength: Integer);
    property Length: Integer read FLength;
  end;

  /// <summary>
  ///   Specifies the precision and scale for numeric columns.
  /// </summary>
  PrecisionAttribute = class(TCustomAttribute)
  private
    FPrecision: Integer;
    FScale: Integer;
  public
    constructor Create(APrecision, AScale: Integer);
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
  end;

  /// <summary>
  ///   Marks a property as not mapped to the database.
  /// </summary>
  NotMappedAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Specifies the name of the stored procedure associated with a class.
  /// </summary>
  StoredProcedureAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies parameter metadata for stored procedure calls.
  /// </summary>
  DbParamAttribute = class(TCustomAttribute)
  private
    FParamType: TParamType;
    FName: string;
  public
    constructor Create(AParamType: TParamType = ptInput; const AName: string = '');
    property ParamType: TParamType read FParamType;
    property Name: string read FName;
  end;

  /// <summary>
  ///   Marks a property to be used as an Offline Locking Token (user/session id).
  /// </summary>
  LockTokenAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property to be used as an Offline Locking Expiration date/time.
  /// </summary>
  LockExpirationAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as a nested object for multi-mapping hydration.
  /// </summary>
  NestedAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Specifies the backing field to be used for a property during hydration.
  ///   This avoids triggering property setters during object loading.
  /// </summary>
  FieldAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    property Name: string read FName;
  end;

  /// <summary>
  ///   Marks a property as JSON/JSONB column (PostgreSQL).
  /// </summary>
  JsonColumnAttribute = class(TCustomAttribute)
  private
    FUseJsonB: Boolean;
  public
    constructor Create(AUseJsonB: Boolean = True);
    property UseJsonB: Boolean read FUseJsonB;
  end;

  /// <summary>
  ///   Marks a property as a version column for Optimistic Concurrency Control.
  ///   The property must be of an integer type.
  /// </summary>
  VersionAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as an automatic Creation Timestamp.
  ///   The ORM will set this field to the current date/time on Insert.
  /// </summary>
  CreatedAtAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as an automatic Update Timestamp.
  ///   The ORM will set this field to the current date/time on Insert and Update.
  /// </summary>
  UpdatedAtAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Enables soft delete for an entity.
  ///   Instead of physically deleting records, marks them as deleted.
  ///   Usage: [SoftDelete('IsDeleted')] or [SoftDelete('DeletedAt')]
  /// </summary>
  SoftDeleteAttribute = class(TCustomAttribute)
  private
    FColumnName: string;
    FDeletedValue: Variant;
    FNotDeletedValue: Variant;
  public
    /// <summary>
    ///   Creates a soft delete attribute with a boolean column (default: 'IsDeleted')
    /// </summary>
    constructor Create(const AColumnName: string = 'IsDeleted'); overload;
    
    /// <summary>
    ///   Creates a soft delete attribute with custom deleted/not-deleted values
    ///   Useful for timestamp-based soft delete (DeletedAt column)
    /// </summary>
    constructor Create(const AColumnName: string; const ADeletedValue, ANotDeletedValue: Variant); overload;
    
    property ColumnName: string read FColumnName;
    property DeletedValue: Variant read FDeletedValue;
    property NotDeletedValue: Variant read FNotDeletedValue;
  end;

  /// <summary>
  ///   Defines the cascade action for foreign key constraints.
  /// </summary>
  TCascadeAction = (
    caNoAction,    // NO ACTION - Default behavior (may fail if references exist)
    caCascade,     // CASCADE - Delete/Update related rows automatically
    caSetNull,     // SET NULL - Set foreign key to NULL when parent is deleted/updated
    caRestrict     // RESTRICT - Prevent delete/update if references exist
  );

  /// <summary>
  ///   Marks a property as a Foreign Key relationship.
  ///   Example: [ForeignKey('UserId', caCascade, caNoAction)]
  /// </summary>
  ForeignKeyAttribute = class(TCustomAttribute)
  private
    FColumnName: string;
    FOnDelete: TCascadeAction;
    FOnUpdate: TCascadeAction;
  public
    constructor Create(const AColumnName: string); overload;
    constructor Create(const AColumnName: string; AOnDelete: TCascadeAction); overload;
    constructor Create(const AColumnName: string; AOnDelete, AOnUpdate: TCascadeAction); overload;
    property ColumnName: string read FColumnName;
    property OnDelete: TCascadeAction read FOnDelete;
    property OnUpdate: TCascadeAction read FOnUpdate;
  end;

  /// <summary>
  ///   Alias for ForeignKeyAttribute
  /// </summary>
  FKAttribute = ForeignKeyAttribute;

  /// <summary>
  ///   Marks a collection property as a One-to-Many relationship.
  /// </summary>
  HasManyAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a reference property as a Many-to-One relationship.
  /// </summary>
  BelongsToAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a reference property as a One-to-One relationship.
  /// </summary>
  HasOneAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a collection property as a Many-to-Many relationship.
  ///   Requires a join table to link the two entities.
  /// </summary>
  ManyToManyAttribute = class(TCustomAttribute)
  private
    FJoinTableName: string;
    FLeftKeyColumn: string;
    FRightKeyColumn: string;
  public
    /// <summary>
    ///   Creates Many-to-Many with default join table naming convention.
    /// </summary>
    constructor Create; overload;
    
    /// <summary>
    ///   Creates Many-to-Many with explicit join table name.
    /// </summary>
    constructor Create(const AJoinTableName: string); overload;
    
    /// <summary>
    ///   Creates Many-to-Many with explicit join table and key columns.
    /// </summary>
    constructor Create(const AJoinTableName, ALeftKeyColumn, ARightKeyColumn: string); overload;
    
    property JoinTableName: string read FJoinTableName;
    property LeftKeyColumn: string read FLeftKeyColumn;
    property RightKeyColumn: string read FRightKeyColumn;
  end;

  /// <summary>
  ///   Specifies the inverse navigation property on the other end of the relationship.
  /// </summary>
  InversePropertyAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies the delete behavior for the relationship.
  /// </summary>
  DeleteBehaviorAttribute = class(TCustomAttribute)
  private
    FBehavior: TCascadeAction;
  public
    constructor Create(ABehavior: TCascadeAction);
    property Behavior: TCascadeAction read FBehavior;
  end;

  /// <summary>
  ///   Defines the inheritance strategy for the entity hierarchy.
  /// </summary>
  InheritanceAttribute = class(TCustomAttribute)
  private
    FStrategy: TInheritanceStrategy;
  public
    constructor Create(AStrategy: TInheritanceStrategy);
    property Strategy: TInheritanceStrategy read FStrategy;
  end;

  /// <summary>
  ///   Specifies the column used as a discriminator in TPH inheritance.
  /// </summary>
  DiscriminatorColumnAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  /// <summary>
  ///   Specifies the discriminator value for this specific class in the hierarchy.
  /// </summary>
  DiscriminatorValueAttribute = class(TCustomAttribute)
  private
    FValue: Variant;
  public
    constructor Create(const AValue: Variant);
    property Value: Variant read FValue;
  end;

  /// <summary>
  ///   Specifies the database field type for the property.
  ///   Overrides the default type mapping.
  /// </summary>
  DbTypeAttribute = class(TCustomAttribute)
  private
    FDataType: TFieldType;
  public
    constructor Create(ADataType: TFieldType);
    property DataType: TFieldType read FDataType;
  end;

  /// <summary>
  ///   Specifies a custom type converter for the property.
  ///   The converter class must implement ITypeConverter.
  /// </summary>
  TypeConverterAttribute = class(TCustomAttribute)
  private
    FConverterClass: TClass;
  public
    constructor Create(AConverterClass: TClass);
    property ConverterClass: TClass read FConverterClass;
  end;

implementation

{ TableAttribute }

constructor TableAttribute.Create;
begin
  FName := '';
end;

constructor TableAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ColumnAttribute }

constructor ColumnAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ FieldAttribute }

constructor FieldAttribute.Create;
begin
  FName := '';
end;

constructor FieldAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ JsonColumnAttribute }

constructor JsonColumnAttribute.Create(AUseJsonB: Boolean);
begin
  inherited Create;
  FUseJsonB := AUseJsonB;
end;

{ ForeignKeyAttribute }

constructor ForeignKeyAttribute.Create(const AColumnName: string);
begin
  FColumnName := AColumnName;
  FOnDelete := caNoAction;
  FOnUpdate := caNoAction;
end;

constructor ForeignKeyAttribute.Create(const AColumnName: string; AOnDelete: TCascadeAction);
begin
  FColumnName := AColumnName;
  FOnDelete := AOnDelete;
  FOnUpdate := caNoAction;
end;

constructor ForeignKeyAttribute.Create(const AColumnName: string; AOnDelete, AOnUpdate: TCascadeAction);
begin
  FColumnName := AColumnName;
  FOnDelete := AOnDelete;
  FOnUpdate := AOnUpdate;
end;

{ InversePropertyAttribute }

constructor InversePropertyAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ DeleteBehaviorAttribute }

constructor DeleteBehaviorAttribute.Create(ABehavior: TCascadeAction);
begin
  FBehavior := ABehavior;
end;

{ ManyToManyAttribute }

constructor ManyToManyAttribute.Create;
begin
  FJoinTableName := '';
  FLeftKeyColumn := '';
  FRightKeyColumn := '';
end;

constructor ManyToManyAttribute.Create(const AJoinTableName: string);
begin
  FJoinTableName := AJoinTableName;
  FLeftKeyColumn := '';
  FRightKeyColumn := '';
end;

constructor ManyToManyAttribute.Create(const AJoinTableName, ALeftKeyColumn, ARightKeyColumn: string);
begin
  FJoinTableName := AJoinTableName;
  FLeftKeyColumn := ALeftKeyColumn;
  FRightKeyColumn := ARightKeyColumn;
end;

{ InheritanceAttribute }

constructor InheritanceAttribute.Create(AStrategy: TInheritanceStrategy);
begin
  FStrategy := AStrategy;
end;

{ DiscriminatorColumnAttribute }

constructor DiscriminatorColumnAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ DiscriminatorValueAttribute }

constructor DiscriminatorValueAttribute.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

{ SoftDeleteAttribute }

constructor SoftDeleteAttribute.Create(const AColumnName: string);
begin
  FColumnName := AColumnName;
  // Default: Boolean soft delete (IsDeleted = 1 means deleted, 0 means not deleted)
  FDeletedValue := 1;
  FNotDeletedValue := 0;
end;

constructor SoftDeleteAttribute.Create(const AColumnName: string; const ADeletedValue, ANotDeletedValue: Variant);
begin
  FColumnName := AColumnName;
  FDeletedValue := ADeletedValue;
  FNotDeletedValue := ANotDeletedValue;
end;

{ DbTypeAttribute }

constructor DbTypeAttribute.Create(ADataType: TFieldType);
begin
  FDataType := ADataType;
end;

{ TypeConverterAttribute }

constructor TypeConverterAttribute.Create(AConverterClass: TClass);
begin
  FConverterClass := AConverterClass;
end;

{ MaxLengthAttribute }

constructor MaxLengthAttribute.Create(ALength: Integer);
begin
  FLength := ALength;
end;

{ MinLengthAttribute }

constructor MinLengthAttribute.Create(ALength: Integer);
begin
  FLength := ALength;
end;

{ PrecisionAttribute }

constructor PrecisionAttribute.Create(APrecision, AScale: Integer);
begin
  FPrecision := APrecision;
  FScale := AScale;
end;

{ StoredProcedureAttribute }

constructor StoredProcedureAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ DbParamAttribute }

constructor DbParamAttribute.Create(AParamType: TParamType; const AName: string);
begin
  FParamType := AParamType;
  FName := AName;
end;

end.

