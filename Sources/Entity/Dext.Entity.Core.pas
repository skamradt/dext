unit Dext.Entity.Core;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects,
  Dext.Specifications.Interfaces;

type
  /// <summary>
  ///   Non-generic base interface for DbSets.
  ///   Allows access to DbSet operations without knowing the generic type at compile time.
  /// </summary>
  IDbSet = interface
    ['{30000000-0000-0000-0000-000000000000}']
    function FindObject(const AId: Variant): TObject;
    function GenerateCreateTableScript: string;
  end;

  /// <summary>
  ///   Represents a collection of entities mapped to a database table.
  /// </summary>
  IDbSet<T: class> = interface(IDbSet)
    ['{30000000-0000-0000-0000-000000000001}']
    
    // CRUD
    procedure Add(const AEntity: T);
    procedure Update(const AEntity: T);
    procedure Remove(const AEntity: T);
    function Find(const AId: Variant): T; overload;
    function Find(const AId: array of Integer): T; overload;

    // Queries via Specifications
    function List(const ASpec: ISpecification<T>): TList<T>; overload;
    function List: TList<T>; overload; // All
    function FirstOrDefault(const ASpec: ISpecification<T>): T;
    
    function Any(const ASpec: ISpecification<T>): Boolean;
    function Count(const ASpec: ISpecification<T>): Integer;
  end;

  /// <summary>
  ///   Represents a session with the database.
  /// </summary>
  IDbContext = interface
    ['{30000000-0000-0000-0000-000000000002}']
    function Connection: IDbConnection;
    function Dialect: ISQLDialect;
    
    // Transaction Management
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    
    /// <summary>
    ///   Get a non-generic DbSet for the specified entity type.
    /// </summary>
    function DataSet(AEntityType: PTypeInfo): IDbSet;
    
    /// <summary>
    ///   Ensures that the database schema exists.
    ///   Creates tables for all registered entities if they don't exist.
    /// </summary>
    procedure EnsureCreated;
  end;

implementation

end.
