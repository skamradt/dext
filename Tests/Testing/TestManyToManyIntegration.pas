unit TestManyToManyIntegration;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Collections,
  Dext.Entity, 
  Dext.Entity.Core, 
  Dext.Entity.Attributes,
  Dext.Types.Lazy,
  Dext.Entity.Setup, 
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Drivers.Interfaces;

type
  TCourseInt = class;

  // Test Entities
  [Table('Students')]
  TStudentInt = class
  private
    FId: Integer;
    FName: string;
    FCourses: Lazy<IList<TCourseInt>>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    [ManyToMany('StudentCourses', 'student_id', 'course_id')]
    property Courses: Lazy<IList<TCourseInt>> read FCourses write FCourses;
  end;

  [Table('Courses')]
  TCourseInt = class
  private
    FId: Integer;
    FTitle: string;
    FStudents: Lazy<IList<TStudentInt>>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;

    [ManyToMany('StudentCourses', 'course_id', 'student_id')]
    property Students: Lazy<IList<TStudentInt>> read FStudents write FStudents;
  end;

  // DbContext
  TIntegrationContext = class(TDbContext)
  protected
    procedure OnModelCreating(Builder: TModelBuilder); override;
  public
    function Students: IDbSet<TStudentInt>;
    function Courses: IDbSet<TCourseInt>;
  end;

  [TestFixture]
  TManyToManyIntegrationTests = class
  private
    FConn: TFDConnection;
    FContext: TIntegrationContext;
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure TestLazyLoading_ManyToMany;
    
    [Test]
    procedure TestLink_ManyToMany;
    
    [Test]
    procedure TestSync_ManyToMany;
    
    [Test]
    procedure TestManualInsertSQL;
  end;

implementation

{ TIntegrationContext }

procedure TIntegrationContext.OnModelCreating(Builder: TModelBuilder);
begin
  inherited;
  Builder.Entity<TStudentInt>;
  Builder.Entity<TCourseInt>;
end;

function TIntegrationContext.Courses: IDbSet<TCourseInt>;
begin
  Result := Entities<TCourseInt>;
end;

function TIntegrationContext.Students: IDbSet<TStudentInt>;
begin
  Result := Entities<TStudentInt>;
end;

{ TManyToManyIntegrationTests }

procedure TManyToManyIntegrationTests.Setup;
var
  DbConn: IDbConnection;
begin
  FEntities := TObjectList<TObject>.Create(True);
  // Create In-Memory SQLite Connection
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  FConn.LoginPrompt := False;
  FConn.Open;

  // Wrap connection
  // Using explicit class from unit to avoid ambiguity if any
  DbConn := TFireDACConnection.Create(FConn, False); 

  // Create Context
  FContext := TIntegrationContext.Create(DbConn);
  
  // Force RTTI for generic lists
  var Dummy1: IList<TCourseInt> := TSmartList<TCourseInt>.Create;
  var Dummy2: IList<TStudentInt> := TSmartList<TStudentInt>.Create;
  
  SetupSchema;
end;

procedure TManyToManyIntegrationTests.SetupSchema;
begin
  // Use Quoted identifiers to match Dext behavior
  FContext.Connection.CreateCommand('CREATE TABLE "Students" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Name" TEXT)').Execute;
  FContext.Connection.CreateCommand('CREATE TABLE "Courses" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Title" TEXT)').Execute;
  FContext.Connection.CreateCommand('CREATE TABLE "StudentCourses" ("student_id" INTEGER, "course_id" INTEGER, PRIMARY KEY("student_id", "course_id"))').Execute;
end;

procedure TManyToManyIntegrationTests.Track(Obj: TObject);
begin
  if FEntities <> nil then
    FEntities.Add(Obj);
end;

procedure TManyToManyIntegrationTests.Teardown;
begin
  if FContext <> nil then FContext.Free;
  if FEntities <> nil then FEntities.Free;
  if FConn <> nil then FConn.Free;
end;

procedure TManyToManyIntegrationTests.TestLazyLoading_ManyToMany;
var
  S1: TStudentInt;
  C1, C2: TCourseInt;
  CheckStudent: TStudentInt;
  CoursesList: IList<TCourseInt>;
begin
  // 1. Insert Data
  S1 := TStudentInt.Create; Track(S1);
  S1.Name := 'John Doe';
  FContext.Students.Add(S1);
  FContext.SaveChanges;
  
  C1 := TCourseInt.Create; Track(C1);
  C1.Title := 'Math';
  FContext.Courses.Add(C1);
  
  C2 := TCourseInt.Create; Track(C2);
  C2.Title := 'Physics';
  FContext.Courses.Add(C2);
  FContext.SaveChanges;
  
  // 2. Link them
  FContext.Students.LinkManyToMany(S1, 'Courses', C1);
  FContext.Students.LinkManyToMany(S1, 'Courses', C2);
  
  // Detach everything to ensure fresh load
  FContext.DetachAll;
  
  // 3. Load Student (Lazy)
  CheckStudent := FContext.Students.Find(S1.Id);
  Should(CheckStudent).NotBeNil;
  Should(CheckStudent.Name).Be('John Doe');
  
  // 4. Trigger Lazy Loading
  Should(CheckStudent.Courses.IsValueCreated).BeFalse; // Not loaded yet
  
  CoursesList := CheckStudent.Courses.Value; // Access property trigger
  
  Should(CheckStudent.Courses.IsValueCreated).BeTrue;
  
  Should(CoursesList).NotBeNil;
  Should(CoursesList.Count).Be(2); 
end;

procedure TManyToManyIntegrationTests.TestLink_ManyToMany;
var
  S1: TStudentInt;
  C1: TCourseInt;
begin
  S1 := TStudentInt.Create; 
  S1.Name := 'Alice';
  FContext.Students.Add(S1);
  
  C1 := TCourseInt.Create; 
  C1.Title := 'Chemistry';
  FContext.Courses.Add(C1);
  
  FContext.SaveChanges;
  
  // Link
  FContext.Students.LinkManyToMany(S1, 'Courses', C1);
  
  // Verify in Join Table
  var Cmd := FContext.Connection.CreateCommand('SELECT COUNT(*) FROM "StudentCourses" WHERE "student_id" = :p1 AND "course_id" = :p2');
  Cmd.AddParam('p1', S1.Id);
  Cmd.AddParam('p2', C1.Id);
  
  var Val := Cmd.ExecuteScalar;
  Should(Val.AsInteger).Be(1);
  
  // Unlink
  FContext.Students.UnlinkManyToMany(S1, 'Courses', C1);
  
  var Val2 := Cmd.ExecuteScalar;
  Should(Val2.AsInteger).Be(0);
end;

procedure TManyToManyIntegrationTests.TestSync_ManyToMany;
var
  S1: TStudentInt;
  C1, C2, C3: TCourseInt;
  Related: TArray<TObject>;
begin
  S1 := TStudentInt.Create; 
  FContext.Students.Add(S1);
  
  C1 := TCourseInt.Create; 
  C2 := TCourseInt.Create; 
  C3 := TCourseInt.Create; 
  FContext.Courses.Add(C1);
  FContext.Courses.Add(C2);
  FContext.Courses.Add(C3);
  FContext.SaveChanges;
  
  // Initial Link: C1, C2
  SetLength(Related, 2);
  Related[0] := C1;
  Related[1] := C2;
  FContext.Students.SyncManyToMany(S1, 'Courses', Related);
  
  var Cmd := FContext.Connection.CreateCommand('SELECT COUNT(*) FROM "StudentCourses" WHERE "student_id" = :p1');
  Cmd.AddParam('p1', S1.Id);
  Should(Cmd.ExecuteScalar.AsInteger).Be(2);
  
  // Sync: C2, C3 (Should remove C1, keep C2, add C3)
  SetLength(Related, 2);
  Related[0] := C2;
  Related[1] := C3;
  FContext.Students.SyncManyToMany(S1, 'Courses', Related);
  
  Should(Cmd.ExecuteScalar.AsInteger).Be(2);
  
  // Verify C1 is gone
  var CmdCheck := FContext.Connection.CreateCommand('SELECT COUNT(*) FROM "StudentCourses" WHERE "student_id" = :p1 AND "course_id" = :p2');
  CmdCheck.AddParam('p1', S1.Id);
  CmdCheck.AddParam('p2', C1.Id);
  Should(CmdCheck.ExecuteScalar.AsInteger).Be(0);
  
  // Verify C3 is present
  // Verify C3 is present
  CmdCheck := FContext.Connection.CreateCommand('SELECT COUNT(*) FROM "StudentCourses" WHERE "student_id" = :p1 AND "course_id" = :p2');
  CmdCheck.AddParam('p1', S1.Id);
  CmdCheck.AddParam('p2', C3.Id);
  Should(CmdCheck.ExecuteScalar.AsInteger).Be(1);
end;

procedure TManyToManyIntegrationTests.TestManualInsertSQL;
begin
  // Verifying writes work
  FContext.Connection.CreateCommand('INSERT INTO "StudentCourses" ("student_id", "course_id") VALUES (99, 88)').Execute;
  
  var Cmd := FContext.Connection.CreateCommand('SELECT COUNT(*) FROM "StudentCourses" WHERE "student_id" = 99');
  var Val := Cmd.ExecuteScalar.AsInteger;
  Should(Val).Be(1);
end;

end.
