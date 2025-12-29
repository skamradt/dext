# 🎨 Orm.EntityStyles - Two Entity Definition Approaches

A side-by-side comparison of **two ways to define entities** in the Dext ORM, demonstrating that both approaches can coexist in the same project.

---

## ✨ What This Demo Shows

### Style 1: Classic Entities (Native Types)
```pascal
[Table('ClassicPeople')]
TClassicPerson = class
  property Id: Integer read FId write FId;
  property Name: string read FName write FName;
  property Age: Integer read FAge write FAge;
end;
```

**Best for:**
- Migrating existing Delphi code
- Teams familiar with traditional Delphi types
- Projects using TypeSystem for typed queries

### Style 2: Smart Entities (Smart Properties)
```pascal
[Table('SmartPeople')]
TSmartPerson = class
  property Id: IntType read FId write FId;
  property Name: StringType read FName write FName;
  property Age: IntType read FAge write FAge;
end;
```

**Best for:**
- New projects starting fresh
- Developers who want typed queries without metadata classes
- Less boilerplate code

---

## 🚀 Getting Started

### Prerequisites
- Delphi 11+ (Alexandria or later)
- Dext Framework in Library Path

### Running the Demo

1. Open `Orm.EntityStyles.dproj` in Delphi
2. Build the project (F9)
3. Run the executable

No database setup required - uses SQLite in-memory!

---

## 📖 Key Differences

| Feature | Classic | Smart Properties |
|---------|---------|------------------|
| Property Types | `Integer`, `string` | `IntType`, `StringType` |
| Typed Queries | Requires `TEntityType<T>` | Built-in with `Prototype.Entity<T>` |
| Learning Curve | Familiar | New concept |
| Boilerplate | More (metadata class) | Less |
| Migration | Easy from existing code | New projects |

---

## 💡 Query Examples

### Classic Style (with TypeSystem)
```pascal
// Requires separate metadata class
type
  TPersonType = class(TEntityType<TPerson>)
    class var Age: TProp<Integer>;
  end;

// Usage
var Adults := Context.Entities<TPerson>.QueryAll
  .Where(TPersonType.Age >= 18)
  .ToList;
```

### Smart Style (with Prototype)
```pascal
// No separate class needed!
var p := Prototype.Entity<TSmartPerson>;

var Adults := Context.Entities<TSmartPerson>
  .Where(p.Age >= 18)
  .ToList;

// Chained queries
var Result := Context.Entities<TSmartPerson>
  .Where(p.Age > 20)
  .Where(p.Age < 40)
  .ToList;
```

---

## 🔧 When to Use Each

### Choose **Classic** when:
- ✅ Migrating existing codebase
- ✅ Team is comfortable with TypeSystem patterns
- ✅ Need maximum compatibility with existing tooling
- ✅ Prefer explicit metadata definitions

### Choose **Smart Properties** when:
- ✅ Starting a new project
- ✅ Want minimal boilerplate
- ✅ Prefer inline typed queries
- ✅ Coming from other ORMs (Entity Framework, etc.)

### Mix Both!
Both styles can coexist in the same project. Use Classic for legacy entities and Smart for new development.

---

## 📁 Project Structure

```
Orm.EntityStyles/
├── Orm.EntityStyles.dpr       # Main program
├── EntityStyles.Demo.pas      # Demo with both styles
└── README.md                  # This file
```

---

## 📚 Related Examples

- **[Orm.EntityDemo](../Orm.EntityDemo)** - Comprehensive ORM test suite
- **[Orm.SmartProperties](../Orm.SmartProperties)** - Full Smart Properties showcase with Web API

---

## 📄 License

This example is part of the Dext Framework and is licensed under the Apache License 2.0.

---

*Choose your style and start coding! 🚀*
