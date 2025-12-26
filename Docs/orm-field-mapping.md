# Dext ORM - Field Mapping Optimization

The **Field Mapping** feature allows Dext ORM to populate entities directly via their **backing fields**, bypassing property setters. This is critical for performance and for ensuring that logic placed in property setters (such as change tracking, validation, or lazy loading triggers) is not executed during the object hydration phase (loading from the database).

## 🎯 Purpose

- **Avoid Side Effects**: Prevents setters from triggering logic designed only for runtime modifications (e.g., `INotifyPropertyChanged`).
- **Performance**: Direct field access via RTTI can be faster or safer than invoking methods.
- **Encapsulation**: Allows properties to be read-only (using a private field) while still allowing the ORM to populate the data.

## 🚀 How to Use

There are three main ways to use Field Mapping:
1.  **Convention-based** (Auto)
2.  **Attribute-based** (`[Field]`)
3.  **Fluent API**

### 1. Convention-based
If you use the `[Field]` attribute without arguments or the `.UseField` fluent method, Dext follows the standard Delphi convention:
- Property: `MyProp`
- Field: `FMyProp`

### 2. Attribute-based Mapping

Use the `[Field]` attribute (from `Dext.Entity.Attributes`) to explicitly map a backing field.

#### Standard Use (Convention)
```pascal
type
  [Table('Users')]
  TUser = class
  private
    FName: string;
    procedure SetName(const Value: string);
  public
    // Maps to 'FName' field automatically
    [Field] 
    property Name: string read FName write SetName; 
  end;
```

#### Explicit Field Name
If your field doesn't follow the `F` prefix convention:
```pascal
type
  TUser = class
  private
    FInternalName: string;
  public
    // Maps explicitly to 'FInternalName'
    [Field('FInternalName')] 
    property Name: string read GetName write SetName;
  end;
```

> **Note**: The `[Column('name')]` attribute controls the **Database Column Name**. The `[Field]` attribute only controls **how the value is set in the Delphi Object**. If you use `[Field]`, the column name remains the property name (default) or whatever is defined in `[Column]`.

### 3. Fluent API configuration

You can configure field mapping in your `DbContext` or `OnModelCreating` configuration methods using the Fluent API.

```pascal
procedure MyConfiguration.Configure(Builder: IEntityTypeBuilder<TUser>);
begin
  // Option A: Use standard convention (F + PropertyName)
  Builder.Prop('Name').UseField;

  // Option B: Explicit field name
  Builder.Prop('Email').HasFieldName('FInternalEmail');
end;
```

## ⚠️ Important Considerations

- **Private Fields**: The target field **must** be accessible via RTTI using `TRttiType.GetField`. Private fields in the same unit are always accessible. For strict private fields, ensure extended RTTI is enabled (default in Delphi).
- **Correctness**: Ensure the field type matches or is convertible to the property type.
