# Fluent Advanced Querying API

The **Advanced Querying API** allows you to go beyond standard CRUD operations and build complex SQL queries using strict typing and fluent syntax.

This includes support for `JOIN`, `GROUP BY`, and `HAVING` clauses, enabling you to generate optimized SQL for reporting and data analysis scenarios directly from your Delphi code.

## 🔗 Joins

Use the `.Join()` method to link related tables. The generated SQL will verify that the syntax behaves correctly according to the configured dialect.

### Syntax
```pascal
Spec.Join(const ATableName, AAlias, ACondition: string; AType: TJoinType = jtInner);
```

### Example
Retrieving Orders with Customer data:
```pascal
var Spec := TSpecification<TOrder>.Create;

Spec.Select('Total');
Spec.Join('Customers', 'C', 'Orders.CustomerId = C.Id', jtInner);
Spec.Where(Prop('Total') > 100);

// SQL Generated:
// SELECT "Total" FROM "Orders" 
// INNER JOIN "Customers" "C" ON "Orders"."CustomerId" = "C"."Id" 
// WHERE "Total" > 100
```

## 📊 Group By

Use `.GroupBy()` to aggregate data. This is often combined with aggregation functions in your `.Select()` projection.

### Syntax
```pascal
Spec.GroupBy(const AColumn: string);
```

### Example
Summing totals by Customer:
```pascal
Spec.Select('CustomerId'); // Column 1
// Note: Currently Select() takes property/column names. 
// For aggregates like SUM(Total), you might need to use a raw select mechanism 
// or custom expression if supported by your version.
Spec.Join('Customers', 'C', 'Orders.CustomerId = C.Id'); 
Spec.GroupBy('CustomerId');

// SQL Generated:
// SELECT "CustomerId" ... FROM "Orders" ... GROUP BY "CustomerId"
```

## 🛡️ Having

The `.Having()` clause filters the results *after* grouping. It works exactly like `.Where()` but operates on the grouped data.

### Syntax
```pascal
Spec.Having(const AExpression: IExpression);
```

### Example
Filtering groups with high totals:
```pascal
// Having: Total > 500
Spec.GroupBy('CustomerId');
Spec.Having(Prop('Total') > 500); // Or strict aggregate expression

// SQL Generated:
// ... GROUP BY "CustomerId" HAVING ("Total" > :pX)
```

## 🧪 Testing & Verification

Functionality is verified by the integration test suite: `Tests/Entity/TestAdvancedQuery.dpr`.
This test suite compiles a specification and asserts that the generated SQL contains the correct clause structure (`INNER JOIN`, `GROUP BY`, `HAVING`) and that parameters are correctly isolated between the `WHERE` and `HAVING` clauses.
