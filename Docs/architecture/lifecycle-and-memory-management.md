# Lifecycle and Memory Management in Dext Framework

This document outlines the architectural decisions regarding object lifecycle, thread safety, and memory management within the Dext Framework.

## 1. TDextServices: From Class to Record

Historically, `TDextServices` was a class used as a wrapper for `IServiceCollection`. However, this approach introduced several challenges:

*   **Memory Leaks**: When used within anonymous methods (closures) in `TStartup`, the class instance could be captured, creating circular references with `TWebApplication` or other services.
*   **Helper Inheritance Issues**: Delphi's `class helper` supports inheritance, which was used to chain methods from `Core -> Entity -> Web`. However, this created a fragile dependency on the order of `uses` and made it difficult for developers to use only parts of the framework.

### The Solution: Record-based Facades

`TDextServices` has been refactored into a **Record**.

**Benefits:**
*   **Zero Allocation**: Records are allocated on the stack. No `Create` (on heap) or `Free` is required.
*   **No Capture Leaks**: Since it's a value type, it doesn't create reference cycles when used in closures.
*   **Performance**: Removing heap allocations for core infrastructure speeds up application startup.

**The Facade Pattern:**
Because Delphi `record helper` does NOT support inheritance, we adopted the **Facade Replication Pattern**. Each facade unit (`Dext.Web`, `Dext.Entity`) declares its own record helper for `TDextServices` and replicates the necessary methods (e.g., `AddDbContext`). 

## 2. Builder Patterns

Builders like `THealthCheckBuilder` and `TBackgroundServiceBuilder` were also converted from classes to records.

*   **Automatic Cleanup**: These builders often manage temporary lists during the registration phase. By being records, they ensure that if a developer forgets to call `.Build`, the builder itself doesn't leak.
*   **Closure Safety**: Builders are frequently used with anonymous methods. Records prevent the `ActRec` (activation record) of the anonymous method from holding a reference to a parent builder object that should have been freed.

## 3. Thread Safety

Dependency Injection registration should be thread-safe to support dynamic service registration and parallel application startup.

*   **TDextServiceCollection**: Now uses a `TCriticalSection` to protect `Add...` and `AddRange` methods.
*   **Lazy Initialization**: In `TWebApplication`, the initialization of core services is protected using `TMonitor` or atomic checks to ensure singleton instances (like the ServiceProvider) are created only once, even under heavy concurrency.

## 4. Circular Reference Breaking

In `TWebApplication.Teardown`, we explicitly nil out interface fields (`FServices`, `FConfiguration`). This is a critical step in Delphi to ensure that any captured references in anonymous methods registered in the DI container are released, allowing the application objects to be destroyed properly.

---
*Last Updated: 2026-02-01*
