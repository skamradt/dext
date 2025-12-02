# 🎉 Dext ORM - Nullable Support & PostgreSQL Validation - Release Notes

**Data**: 02 de Dezembro de 2025  
**Versão**: Alpha 0.6.1  
**Status**: ✅ Concluído e Validado

---

## 📋 Resumo

Esta release adiciona **suporte completo a `Nullable<T>`** no Dext ORM e valida o suporte a **PostgreSQL** com todos os testes passando.

---

## ✨ Novas Funcionalidades

### 1. 🔧 Suporte Completo a `Nullable<T>`

O Dext ORM agora suporta completamente tipos `Nullable<T>` para campos opcionais e Foreign Keys.

#### **Funcionalidades**:
- ✅ **Persist (Salvar)**: Salva valores nullable corretamente no banco de dados
- ✅ **Hydrate (Carregar)**: Carrega valores do banco para `Nullable<T>`
- ✅ **Foreign Key Loading**: Suporta FKs nullable em relacionamentos opcionais
- ✅ **Lazy Loading**: Funciona com `Nullable<T>` em propriedades de navegação
- ✅ **Explicit Loading**: `Entry().Reference().Load()` com FKs nullable

#### **Tipos Suportados**:
- `Nullable<Integer>`
- `Nullable<Int64>`
- `Nullable<String>`
- `Nullable<TGUID>`
- `Nullable<TDateTime>`
- `Nullable<Double>`
- `Nullable<Boolean>`
- Qualquer `Nullable<T>` compatível

#### **Compatibilidade**:
- ✅ **Spring4D**: `fHasValue: string` (vazio = null)
- ✅ **Delphi Nativo**: `fHasValue: Boolean`

#### **Exemplo de Uso**:
```pascal
type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FAge: Nullable<Integer>;
    FAddressId: Nullable<Integer>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    property Name: string read FName write FName;
    
    // Optional age - can be NULL
    property Age: Nullable<Integer> read FAge write FAge;
    
    // Optional Foreign Key - can be NULL
    [Column('address_id')]
    property AddressId: Nullable<Integer> read FAddressId write FAddressId;
  end;
```

### 2. 🗄️ PostgreSQL - Suporte Completo e Validado

PostgreSQL agora está **totalmente validado** com todos os testes passando.

#### **Funcionalidades Validadas**:
- ✅ CRUD completo
- ✅ Composite Keys
- ✅ Relacionamentos (1:1, 1:N, N:1)
- ✅ Lazy Loading
- ✅ Eager Loading (`.Include()`)
- ✅ Explicit Loading
- ✅ Nullable Support
- ✅ Optimistic Concurrency (`[Version]`)
- ✅ Fluent Query API
- ✅ Bulk Operations
- ✅ RETURNING clause para AutoInc

#### **Desafios Resolvidos**:
- ✅ Case sensitivity em nomes de colunas
- ✅ RETURNING clause para obter IDs gerados
- ✅ Nullable types em Foreign Keys

### 3. ⚙️ Database Configuration System

Novo sistema para **alternar facilmente entre bancos de dados** nos testes.

#### **Classe Helper**: `TDbConfig`

```pascal
// Alternar entre bancos
TDbConfig.SetProvider(dpSQLite);
TDbConfig.SetProvider(dpPostgreSQL);
TDbConfig.SetProvider(dpFirebird);

// Criar conexão e dialeto
var Conn := TDbConfig.CreateConnection;
var Dialect := TDbConfig.CreateDialect;

// Configurar PostgreSQL
TDbConfig.ConfigurePostgreSQL('localhost', 5432, 'dext_test', 'postgres', 'postgres');

// Resetar banco para testes
TDbConfig.ResetDatabase;
```

#### **Benefícios**:
- ✅ Fácil alternância entre bancos
- ✅ Configuração centralizada
- ✅ Suporte a environment variables
- ✅ Preparado para CI/CD

---

## 🔧 Implementação Técnica

### Arquivos Modificados

| Arquivo | Modificações |
|---------|-------------|
| `Dext.Entity.pas` | ✅ Função helper `TryUnwrapAndValidateFK` + Refatorado `TReferenceEntry.Load` |
| `Dext.Entity.LazyLoading.pas` | ✅ Função helper + Refatorado `TLazyInvokeHandler.Invoke` |
| `Dext.Core.ValueConverters.pas` | ✅ Suporte a conversão para `Nullable<T>` no `TValueConverter.Convert` |
| `EntityDemo.DbConfig.pas` | ✨ **NOVO** - Sistema de configuração de banco de dados |

### Arquivos de Documentação

| Arquivo | Descrição |
|---------|-----------|
| `Docs/NULLABLE_SUPPORT.md` | ✨ **NOVO** - Documentação completa de Nullable |
| `Docs/DATABASE_CONFIG.md` | ✨ **NOVO** - Guia de configuração de banco de dados |
| `Docs/ORM_ROADMAP.md` | ✅ Atualizado com Nullable e PostgreSQL |
| `ROADMAP.md` | ✅ Atualizado com status do ORM |

---

## 🎯 Fluxo de Funcionamento

### Salvando (Persist)

```
TUser.AddressId (Nullable<Integer>) 
  → TReferenceEntry.Load 
  → TryUnwrapAndValidateFK 
  → Detecta Nullable<Integer>
  → Acessa fHasValue e fValue via RTTI
  → Se HasValue = false → Sai (não carrega)
  → Se HasValue = true → Unwrap: 123 (Integer)
  → Valida: 123 ≠ 0 ✅ 
  → FindObject(123)
```

### Carregando (Hydrate)

```
DB Column: address_id = 123 (Integer)
  → TValueConverter.Convert
  → Detecta target: Nullable<Integer>
  → Encontra campos fHasValue e fValue
  → Converte 123 → Integer
  → Cria Nullable<Integer> via TValue.Make
  → Define fValue = 123
  → Define fHasValue = true (string ou Boolean)
  → TUser.AddressId ✅
```

---

## 📊 Status dos Bancos de Dados

| Banco de Dados | Status | Testes |
|----------------|--------|--------|
| **SQLite** | ✅ Completo | Todos passando |
| **PostgreSQL** | ✅ Completo | Todos passando |
| **Firebird** | ⚠️ Próximo | Dialeto validado |
| **MySQL** | ❌ Planejado | - |
| **SQL Server** | ❌ Planejado | - |
| **Oracle** | ❌ Planejado | - |

---

## 🚀 Próximos Passos

### Firebird (Prioridade 1)

1. Validar integração completa
2. Testar Nullable support
3. Validar Generators e Sequences
4. Executar todos os testes

### MySQL (Prioridade 2)

1. Implementar dialeto
2. Validar AUTO_INCREMENT
3. Testar transações

### SQL Server (Prioridade 3)

1. Implementar dialeto
2. Validar IDENTITY
3. Testar schemas

---

## 📚 Documentação

- **Nullable Support**: [NULLABLE_SUPPORT.md](../Docs/NULLABLE_SUPPORT.md)
- **Database Config**: [DATABASE_CONFIG.md](../Docs/DATABASE_CONFIG.md)
- **ORM Roadmap**: [ORM_ROADMAP.md](../Docs/ORM_ROADMAP.md)
- **Main Roadmap**: [ROADMAP.md](../ROADMAP.md)

---

## 🎉 Conclusão

Esta release marca um **marco importante** no desenvolvimento do Dext ORM:

- ✅ **Nullable Support**: Funcionalidade crítica para aplicações reais
- ✅ **PostgreSQL**: Segundo banco de dados totalmente validado
- ✅ **Database Config**: Infraestrutura para testar múltiplos bancos facilmente

O Dext ORM está cada vez mais **robusto, flexível e pronto para produção**! 🚀

---

*Última atualização: 02 de Dezembro de 2025*
