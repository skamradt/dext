# Raw SQL with FromSql

While the Dext ORM provides a powerful fluent API for building queries, sometimes you need to execute raw SQL for performance optimization, complex database-specific features, or legacy support. The `FromSql` method allows you to do this while still benefiting from automatic object mapping (hydration).

## Basic Usage

You can call `FromSql` on any `IDbSet<T>`. The results will be automatically mapped to instances of the entity class.

```pascal
var Users := Db.Users.FromSql('SELECT * FROM Users WHERE Active = 1').ToList;
```

## Parameterized Queries

To prevent SQL injection, always use parameterized queries. Dext supports standard parameter syntax.

```pascal
var MinAge := 18;
var Adults := Db.Users
  .FromSql('SELECT * FROM Users WHERE Age >= :Age', [MinAge])
  .ToList;
```

## Mixing with Fluent API

One of the most powerful features of `FromSql` is that you can continue to chain fluent methods after it. The raw SQL acts as the data source for subsequent operations like filtering, sorting, or paging.

```pascal
var List := Db.Users
  .FromSql('SELECT * FROM Users WHERE Role = :Role', ['Admin'])
  .Where(Prop('Active') = True)
  .OrderBy(Prop('Name'))
  .Skip(10)
  .Take(5)
  .ToList;
```

> **Note**: When chaining after `FromSql`, Dext wraps your raw SQL in a subquery to ensure the filters and paging work correctly regardless of the complexity of your manual SQL.

## TSqlQueryIterator

For very large result sets, you can use `TSqlQueryIterator<T>` to process records one by one without loading the entire list into memory.

```pascal
var Query := Db.Users.FromSql('SELECT * FROM Users');
var Iterator := TSqlQueryIterator<TUser>.Create(Query);
try
  while Iterator.Next do
  begin
    ProcessUser(Iterator.Current);
  end;
finally
  Iterator.Free;
end;
```

## Limitations

*   **Projection**: `FromSql` expects the SQL result to contain at least the columns needed to hydrate the entity `T`. If columns are missing, properties will remain at their default values.
*   **Tracking**: By default, entities returned from `FromSql` are tracked by the `DbContext`. You can use `.AsNoTracking` if you only need the data for read-only purposes.
