unit EntityStyles.Demo;

// ============================================================================
//  Orm.EntityStyles - Comparing Two Entity Definition Approaches
// ============================================================================
//  This demo shows TWO ways to define entities in Dext ORM:
//
//  Style 1: CLASSIC - Native Delphi types with TypeSystem
//           Best for: Teams familiar with traditional Delphi, existing projects
//           Pros: Familiar syntax, easy to understand
//           Cons: Requires separate TEntityType<T> for typed queries
//
//  Style 2: SMART PROPERTIES - Using SmartTypes (IntType, StringType)
//           Best for: New projects, developers who prefer less boilerplate
//           Pros: Typed queries without separate metadata class
//           Cons: Less familiar syntax, learning curve
//
//  Both styles can coexist in the same project!
// ============================================================================

interface

procedure RunDemo;

implementation

uses
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Phys.SQLite,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.UI.Intf,
  FireDAC.ConsoleUI.Wait,
  Dext,
  Dext.Collections,
  Dext.Core.SmartTypes,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Core,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.DbSet,
  Dext.Entity.Dialects,
  Dext.Entity.Attributes,
  Dext.Entity.Prototype,
  Dext.Entity;

type
  // ==========================================================================
  //  STYLE 1: CLASSIC ENTITY - Native Delphi Types
  // ==========================================================================
  //  Uses standard Integer, string, etc.
  //  For typed queries, you would create a TPersonType class using TypeSystem.
  // ==========================================================================

  [Table('ClassicPeople')]
  TClassicPerson = class
  private
    FId: Integer;
    FName: string;
    FAge: Integer;
    FEmail: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

  // ==========================================================================
  //  STYLE 2: SMART ENTITY - Smart Properties
  // ==========================================================================
  //  Uses IntType, StringType from Dext.Core.SmartTypes.
  //  Enables typed queries directly on properties without separate metadata.
  //  Use with Prototype.Entity<T> for query expressions.
  // ==========================================================================

  [Table('SmartPeople')]
  TSmartPerson = class
  private
    FId: IntType;
    FName: StringType;
    FAge: IntType;
    FEmail: StringType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    property Name: StringType read FName write FName;
    property Age: IntType read FAge write FAge;
    property Email: StringType read FEmail write FEmail;
  end;

// ============================================================================
//  DEMO: Side-by-side comparison
// ============================================================================

procedure DemoClassicStyle(Ctx: TDbContext);
var
  Set_: IDbSet<TClassicPerson>;
  Person: TClassicPerson;
  All: IList<TClassicPerson>;
begin
  WriteLn('');
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║  STYLE 1: CLASSIC ENTITY (Native Types)                    ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn('');

  Set_ := Ctx.Entities<TClassicPerson>;

  // Create
  WriteLn('📝 Creating classic entity...');
  Person := TClassicPerson.Create;
  Person.Name := 'John Classic';
  Person.Age := 30;
  Person.Email := 'john@classic.com';

  Set_.Add(Person);
  Ctx.SaveChanges;
  WriteLn('   ✅ Created with ID: ', Person.Id);

  // Query (without TypeSystem, use string-based or Find)
  WriteLn('');
  WriteLn('🔍 Querying all classic entities...');
  All := Set_.ToList;
  WriteLn('   Found: ', All.Count, ' record(s)');

  for var P in All do
    WriteLn('   - ', P.Name, ' (Age: ', P.Age, ')');

  WriteLn('');
  WriteLn('💡 Note: For typed queries like .Where(p.Age > 25), you would');
  WriteLn('   need to create a TClassicPersonType using TypeSystem.');
  WriteLn('');
end;

procedure DemoSmartStyle(Ctx: TDbContext);
var
  DbSet: IDbSet<TSmartPerson>;
  p: TSmartPerson;
  Adults, Johns: IList<TSmartPerson>;
begin
  WriteLn('');
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║  STYLE 2: SMART ENTITY (Smart Properties)                  ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn('');

  DbSet := Ctx.Entities<TSmartPerson>;

  // Get prototype for typed queries
  p := Prototype.Entity<TSmartPerson>;

  // Create multiple persons
  WriteLn('📝 Creating smart entities...');

  var Alice := TSmartPerson.Create;
  Alice.Name := 'Alice Smart';
  Alice.Age := 25;
  Alice.Email := 'alice@smart.com';
  DbSet.Add(Alice);

  var Bob := TSmartPerson.Create;
  Bob.Name := 'Bob Smart';
  Bob.Age := 35;
  Bob.Email := 'bob@smart.com';
  DbSet.Add(Bob);

  var John := TSmartPerson.Create;
  John.Name := 'John Smart';
  John.Age := 17;
  John.Email := 'john@smart.com';
  DbSet.Add(John);

  Ctx.SaveChanges;
  WriteLn('   ✅ Created 3 smart entities');

  // Typed Query 1: Age filter
  WriteLn('');
  WriteLn('🔍 Query: Adults (Age >= 18)');
  Adults := DbSet.Where(p.Age >= 18).ToList;
  WriteLn('   Found: ', Adults.Count, ' adult(s)');
  for var A in Adults do
    WriteLn('   - ', string(A.Name), ' (Age: ', Integer(A.Age), ')');

  // Typed Query 2: Name filter
  WriteLn('');
  WriteLn('🔍 Query: Name starts with "Alice"');
  // Note: Using equality for demo, StartsWith would need expression support
  Johns := DbSet.Where(p.Name = 'Alice Smart').ToList;
  WriteLn('   Found: ', Johns.Count, ' match(es)');

  // Chained Query
  WriteLn('');
  WriteLn('🔍 Chained Query: Age > 20 AND Age < 40');
  var MidAge := DbSet.Where(p.Age > 20).Where(p.Age < 40).ToList;
  WriteLn('   Found: ', MidAge.Count, ' in range');
  for var M in MidAge do
    WriteLn('   - ', string(M.Name), ' (Age: ', Integer(M.Age), ')');
  DbSet := nil;
  WriteLn('');
  WriteLn('💡 Smart Properties enable typed queries directly on properties!');
  WriteLn('   No need for separate TypeSystem metadata class.');
  WriteLn('');
end;

procedure RunDemo;
var
  FDConn: TFDConnection;
  Conn: IDbConnection;
  Ctx: TDbContext;
begin
  WriteLn('');
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║        ORM.ENTITYSTYLES - Two Entity Definition Styles     ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn('');
  WriteLn('This demo compares two approaches to defining entities:');
  WriteLn('  1. Classic: Native Delphi types (Integer, string)');
  WriteLn('  2. Smart:   Smart Properties (IntType, StringType)');
  WriteLn('');

  FDConn := TFDConnection.Create(nil);
  try
    // Configure SQLite in-memory
    FDConn.DriverName := 'SQLite';
    FDConn.Params.Values['Database'] := ':memory:';
    FDConn.LoginPrompt := False;

    Conn := TFireDACConnection.Create(FDConn, True);
    Ctx := TDbContext.Create(Conn, TSQLiteDialect.Create);
    try
      Conn.Connect;

      // Register both entity types
      Ctx.Entities<TClassicPerson>;
      Ctx.Entities<TSmartPerson>;
      Ctx.EnsureCreated;

      WriteLn('📊 Database: SQLite (In-Memory)');
      WriteLn('📦 Tables created: ClassicPeople, SmartPeople');

      // Run demos
      DemoClassicStyle(Ctx);
      DemoSmartStyle(Ctx);

      // Summary
      WriteLn('');
      WriteLn('╔════════════════════════════════════════════════════════════╗');
      WriteLn('║                       SUMMARY                              ║');
      WriteLn('╠════════════════════════════════════════════════════════════╣');
      WriteLn('║  Both styles work in the same project!                     ║');
      WriteLn('║                                                            ║');
      WriteLn('║  Choose CLASSIC when:                                      ║');
      WriteLn('║    • Migrating existing code                               ║');
      WriteLn('║    • Team prefers traditional Delphi types                 ║');
      WriteLn('║    • Using TypeSystem for typed queries                    ║');
      WriteLn('║                                                            ║');
      WriteLn('║  Choose SMART when:                                        ║');
      WriteLn('║    • Starting fresh projects                               ║');
      WriteLn('║    • Want typed queries without metadata classes           ║');
      WriteLn('║    • Prefer less boilerplate                               ║');
      WriteLn('╚════════════════════════════════════════════════════════════╝');
      WriteLn('');

    finally
      Ctx.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('❌ ERROR: ', E.ClassName, ': ', E.Message);
      WriteLn('');
    end;
  end;
end;

end.
