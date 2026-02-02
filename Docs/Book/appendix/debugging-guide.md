# Advanced Debugging Guide

Debugging modern, high-abstraction web frameworks like Dext can be challenging because much of the "magic" (Model Binding, Lazy Loading, Validation) happens in the background before your code is even executed.

Use this guide to master the art of finding bugs in complex Dext pipelines.

## 1. The Request Lifecycle (Where the "Magic" Happens)

When a request hits a Dext application, it follows this simplified path:
1. **Server Listener** (Indy / HTTP.sys)
2. **Middleware Pipeline** (Auth, CORS, Logging, etc.)
3. **Routing Engine** (Finds the right endpoint)
4. **Handler Invoker** (Injects parameters via Model Binding)
5. **Validation** (Runs TValidator on bound data)
6. **Your Handler** (The code you wrote)

**CRITICAL TIP**: If you have a breakpoint in your handler (Step 6) and it's **not being hit** but you get an HTTP 500/400, the error is occurring in Steps 1 through 5.

---

## 2. Debugging Model Binding & Type Converters

One of the most common sources of errors is when the JSON sent by the client doesn't "fit" into your Delphi types.

### Symptoms
- 500 Internal Server Error without hitting your handler breakpoint.
- 400 Bad Request with "Binding Error" messages.
- Properties in your object are `0`, `nil`, or empty.

### How to Debug
If you suspect Model Binding is failing:
1. **Enable Delphi Exceptions**: In the IDE, ensure `Language Exceptions` are enabled (Tools -> Options -> Debugger -> Delphi Exceptions).
2. **Set "Generic" Breakpoints**: Put a breakpoint in the framework's core conversion logic:
   - File: `Dext.Core.ValueConverters.pas`
   - Method: `TValueConverter.Convert`
3. **Inspect the Flow**:
   - Check if `AValue` (from JSON) correctly matches the `ATargetType`.
   - Look for the `try..except` block at the end of `Convert`. If it fails, that's your culprit.

---

## 3. Debugging ORM & Lazy Loading

Lazy Loading is powerful but can hide complex type mismatches.

### Symptoms
- "Invalid class typecast" when accessing a property.
- "Access Violation" when reading an entity member.

### How to Debug
1. **Break on the Injector**:
   - File: `Dext.Entity.LazyLoading.pas`
   - Method: `TLazyInjector.InjectField`
2. **Verify Types**:
   - Check if the `Lazy<T>` field in your entity matches the interface/class being loaded by the `TLazyLoader`.
   - Ensure the property is physically present in the class (RTTI needs it).

---

## 4. Pipeline Debugging (Middleware)

Middlewares can modify requests or abort them early.

### Identifying the Middleware
If you suspect a middleware is interfering:
1. **HTTP Logging**: Use `App.UseHttpLogging`. It will show exactly which step the request reached.
2. **Step-by-Step Traversal**:
   - Open `Dext.Web.Core.pas`.
   - Look for `TMiddlewarePipeline.Process`.
   - Step through (`F7`) each middleware delegate to see which one terminates the request.

---

## 5. Pro-Tips for Rapid Debugging

### Use the Smart Type Metadata Cache
Dext caches metadata in `TReflection.GetMetadata`. If a Smart Type (`Prop<T>`) is acting weird:
- Break in `Dext.Core.Reflection.pas`.
- Inspect the `TTypeMetadata` object. Check if `ValueField` is correctly identified.

### Terminal testing
Always use scripts (like `.ps1`) or tools like Postman/Insomnia to reproduce errors. This allows you to verify if the error is related to **headers**, **JSON format**, or **decimal separators**.

### Decimal Separators
Dext uses `TFormatSettings.Invariant` for JSON parsing. If your app crashes with numeric values, check if you are manually parsing strings elsewhere using local (regional) settings.

---

[← Troubleshooting](troubleshooting.md) | [Book Index →](../README.md)
