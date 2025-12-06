unit Dext.Entity.Attributes;

interface

uses
  System.Rtti,
  System.Variants;

type
  TableAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  PKAttribute = class(TCustomAttribute)
  end;

  AutoIncAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as not mapped to the database.
  /// </summary>
  NotMappedAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   Marks a property as a version column for Optimistic Concurrency Control.
  ///   The property must be of an integer type.
  /// </summary>
  VersionAttribute = class(TCustomAttribute)
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

implementation

{ TableAttribute }

constructor TableAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ ColumnAttribute }

constructor ColumnAttribute.Create(const AName: string);
begin
  FName := AName;
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

end.
