# Dext.Entity Metadata Cache

The **Metadata Cache** is a core performance optimization in Dext ORM. It eliminates the overhead of repetitive RTTI (Reflection) lookups during SQL generation, entity hydration, and mapping.

## Architecture

The cache is implemented via the `TModelBuilder` class, which acts as a Singleton Registry.

### Key Components

1.  **`TModelBuilder.Instance` (Singleton)**
    -   Holds a dictionary of `TEntityMap` instances, keyed by `PTypeInfo`.
    -   Ensures only one `TEntityMap` exists per entity type for the entire application lifetime.

2.  **`TEntityMap` (The Cached Metadata)**
    -   Stores pre-calculated metadata:
        -   `TableName`: Resolved from `[Table]` attribute.
        -   `Properties`: Dictionary of mapped properties (Column names, Types, Flags).
        -   `Keys`: Primary Key definitions.
    -   Populated **once** during the first access (Lazy Discovery).

### Auto-Discovery Flow

When `TSQLGenerator<T>` or `TDbSet<T>` requires metadata:
1.  It calls `TModelBuilder.Instance.GetMap(TypeInfo(T))`.
2.  Check if `T` is already in `FMaps`.
3.  **Cache Miss**:
    -   Create new `TEntityMap`.
    -   Scan attributes (`TTable`, `TColumn`, etc.) using `TRttiContext`.
    -   Store in `FMaps`.
4.  **Cache Hit**:
    -   Return existing `TEntityMap`.

## Performance Impact

By using the cache, we avoid:
-   `TRttiContext.Create` (expensive initialization).
-   `GetType`, `GetProperties`, `GetAttributes` loops (O(N) operations on RTTI).

This results in significantly faster query generation and object mapping, especially for complex entities.
