# Troubleshooting

Find solutions to common errors when working with Dext.

## Installation Errors

### Unit 'Dext.Web' not found
- **Solution**: Check if the `Sources` directory (and its subdirectories) are in the IDE's **Library Path**. We recommend using the `$(DEXT)` environment variable.

### Error loading packages (BPL)
- **Solution**: Ensure the `Output` folder where BPLs are generated is in your **System PATH**.

## ORM Errors

### "Entity TUser not found in context"
- **Solution**: Check if you called `Entities<TUser>` in your `DbContext` or registered the mapping if using POCO classes.

### Fields returning Null unexpectedly
- **Solution**: Check if property names or `[Column('name')]` attributes exactly match what's in the database (watch out for Case-Sensitivity in DBs like PostgreSQL).

## HTTP / Web Errors

### 404 Error when calling endpoint
- **Solution**: Check the order of route registration. Routes with parameters (e.g., `/:id`) can "swallow" static routes if registered improperly.

### CORS Error in Browser
- **Solution**: Add `App.UseCors` to your pipeline **before** any endpoint. Make sure to configure allowed origins in production.

## Debugging

Dext has internal logs that can help:

```pascal
// Enable detailed HTTP logs
App.UseHttpLogging;

// See generated SQL queries from ORM
Ctx.Logger := TConsoleLogger.Create;
```

---

[← Dialects](dialects.md) | [Book Index →](../README.md)
