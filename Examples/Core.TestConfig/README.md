# 🔧 Core.TestConfig - Configuration System Demo

A simple console application demonstrating the **Dext Configuration System** - a flexible, layered configuration management inspired by ASP.NET Core.

---

## ✨ Features

- **Multiple Configuration Sources**: Combine JSON files, environment variables, and more
- **Hierarchical Keys**: Access nested values using `:` delimiter (e.g., `Logging:LogLevel:Default`)
- **Environment Variable Override**: Override any config value via environment variables
- **Cross-Platform**: Works on Windows and Linux/macOS
- **Prefix Filtering**: Filter environment variables by prefix (e.g., `DEXT_`)

---

## 🚀 Getting Started

### Prerequisites
- Delphi 11+ (Alexandria or later)
- Dext Framework in Library Path

### Running the Example

1. Open `Core.TestConfig.dproj` in Delphi
2. Build the project (Console Application)
3. Run the executable

> **Note**: The example will **automatically create** a default `appsettings.json` in the output directory if it doesn't exist. This is for demo purposes only - in production applications, this file should be deployed with your application (see comments in source code).

### Expected Output

```
Starting Configuration Test...
Adding JSON source...
Adding Environment Variables source...
Building configuration...
Reading values...
AppSettings:Message = Hello from appsettings.json!
Logging:LogLevel:Default = Information
PATH length = 1234
Test Finished.
```

---

## 📖 How It Works

### 1. Create Configuration Builder

```delphi
var
  Builder: IConfigurationBuilder;
  Config: IConfigurationRoot;
begin
  Builder := TConfigurationBuilder.Create;
```

### 2. Add Configuration Sources

```delphi
// Add JSON file source (optional = true means file is optional)
Builder.Add(TJsonConfigurationSource.Create('appsettings.json', True));

// Add environment variables source
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

// Add environment variables with prefix filter
Builder.Add(TEnvironmentVariablesConfigurationSource.Create('DEXT_'));
```

### 3. Build and Read Configuration

```delphi
Config := Builder.Build;

// Read values using hierarchical keys
var Message := Config['AppSettings:Message'];
var LogLevel := Config['Logging:LogLevel:Default'];

// Environment variables are also accessible
var Path := Config['PATH'];
```

---

## 🔑 Key Concepts

### Hierarchical Keys

Configuration values are accessed using `:` as a delimiter:

| Key | JSON Path |
|-----|-----------|
| `AppSettings:Message` | `{ "AppSettings": { "Message": "..." } }` |
| `Logging:LogLevel:Default` | `{ "Logging": { "LogLevel": { "Default": "..." } } }` |

### Environment Variable Mapping

Environment variables use `__` (double underscore) to represent `:`:

| Environment Variable | Configuration Key |
|---------------------|-------------------|
| `DEXT__Database__Host` | `Database:Host` |
| `AppSettings__Message` | `AppSettings:Message` |

### Source Priority

Later sources override earlier ones:

```delphi
Builder.Add(TJsonConfigurationSource.Create('appsettings.json', True));  // Base
Builder.Add(TJsonConfigurationSource.Create('appsettings.local.json', True));  // Override
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);  // Final override
```

---

## 🛠️ Configuration Sources

| Source | Unit | Description |
|--------|------|-------------|
| JSON File | `Dext.Configuration.Json` | Load from `.json` files |
| Environment Variables | `Dext.Configuration.EnvironmentVariables` | Load from OS environment |
| Memory | `Dext.Configuration.Core` | In-memory dictionary |

---

## 🐧 Cross-Platform Support

The environment variables source works on:
- **Windows**: Uses `GetEnvironmentStrings` API
- **Linux/macOS**: Uses POSIX `environ` global

No code changes needed - automatically uses correct implementation.

---

## 📚 Related Documentation

- [Dext Configuration Guide](../../Docs/configuration-guide.md)
- [Dext Framework Documentation](../../README.md)
- [Portuguese Version](README.pt-br.md)

---

## 📄 License

This example is part of the Dext Framework and is licensed under the Apache License 2.0.
