# 🗄️ Orm.EntityDemo - Showcase do Dext ORM

Uma demonstração abrangente das capacidades do **Dext Entity ORM**. Este projeto mostra funcionalidades modernas de ORM incluindo queries fortemente tipadas, lazy loading, soft delete e suporte multi-banco.

---

## ✨ Funcionalidades Demonstradas

### Funcionalidades Core do ORM
- **Operações CRUD** - Create, Read, Update, Delete com change tracking automático
- **Relacionamentos** - Foreign keys, propriedades de navegação, ações em cascata
- **Lazy Loading** - Carregamento automático de entidades relacionadas ao acessar
- **Explicit Loading** - Carregamento manual com `.Include()`
- **No Tracking** - Queries read-only para performance

### Queries Avançadas
- **Expressões Fortemente Tipadas** - `Where(TUserType.Age > 18)`
- **Fluent Query Builder** - Métodos encadeáveis: `.Where().OrderBy().Take()`
- **Padrão Specifications** - Critérios de query reutilizáveis
- **Filtros Complexos** - Condições AND/OR, LIKE, StartsWith, IsNull

### Integridade de Dados
- **Composite Keys** - Chaves primárias multi-coluna (Integer + String)
- **Soft Delete** - Exclusão lógica com atributo `[SoftDelete]`
- **Controle de Concorrência** - Lock otimístico com `[Version]`
- **Tipos Nullable** - Suporte completo a `Nullable<T>`

### Gerenciamento de Schema
- **Migrations** - Versionamento de schema do banco
- **Scaffolding** - Engenharia reversa de entidades de DB existente
- **Fluent Mapping** - Alternativa aos atributos

### Performance
- **Operações em Lote** - Insert/update/delete em batch
- **Lazy Query Execution** - Geração de SQL adiada
- **Suporte Async** - Operações de banco não-bloqueantes

---

## 🚀 Começando

### Pré-requisitos
- Delphi 11+ (Alexandria ou posterior)
- Dext Framework no Library Path

### Executando os Testes

1. Abra `Orm.EntityDemo.dproj` no Delphi
2. Compile o projeto (F9)
3. Execute o binário

Os testes usam **SQLite Em Memória** por padrão - nenhuma configuração de banco necessária!

### Trocar Provedor de Banco

Edite `Orm.EntityDemo.dpr` e altere o provedor:

```pascal
// Padrão: SQLite Memory (não requer configuração)
ConfigureDatabase(dpSQLiteMemory);

// SQLite Arquivo (persistido)
ConfigureDatabase(dpSQLite);

// PostgreSQL
ConfigureDatabase(dpPostgreSQL);

// Firebird
ConfigureDatabase(dpFirebird);

// SQL Server
ConfigureDatabase(dpSQLServer);
```

---

## 📋 Suíte de Testes

A demo inclui **18 suítes de teste abrangentes**:

| # | Teste | Descrição |
|---|-------|-----------|
| 1 | **TCRUDTest** | Create, Read, Update, Delete básico |
| 2 | **TRelationshipTest** | Foreign Keys e propriedades de navegação |
| 3 | **TAdvancedQueryTest** | Queries complexas com filtros e projeções |
| 4 | **TCompositeKeyTest** | Chaves primárias multi-coluna |
| 5 | **TExplicitLoadingTest** | Carregamento manual de entidades relacionadas |
| 6 | **TLazyLoadingTest** | Carregamento automático ao acessar propriedade |
| 7 | **TFluentAPITest** | Query builder e operações estilo LINQ |
| 8 | **TLazyExecutionTest** | Execução de query adiada |
| 9 | **TBulkTest** | Operações de insert/update/delete em lote |
| 10 | **TConcurrencyTest** | Controle de concorrência otimístico |
| 11 | **TScaffoldingTest** | Engenharia reversa do banco de dados |
| 12 | **TMigrationsTest** | Versionamento de schema e migrations |
| 13 | **TCollectionsTest** | Integração com IList<T> |
| 14 | **TNoTrackingTest** | Queries read-only para performance |
| 15 | **TMixedCompositeKeyTest** | Composite keys Integer + String |
| 16 | **TSoftDeleteTest** | Exclusão lógica com filtros |
| 17 | **TAsyncTest** | Operações de banco assíncronas |
| 18 | **TTypeSystemTest** | Expressões de propriedade fortemente tipadas |

---

## 📖 Exemplos de Código

### CRUD Básico

```pascal
// Create
var User := TUser.Create;
User.Name := 'Alice';
User.Age := 25;
Context.Entities<TUser>.Add(User);
Context.SaveChanges;

// Read
var Found := Context.Entities<TUser>.Find(1);

// Update
Found.Age := 26;
Context.Entities<TUser>.Update(Found);
Context.SaveChanges;

// Delete
Context.Entities<TUser>.Remove(Found);
Context.SaveChanges;
```

### Queries Fortemente Tipadas

```pascal
// Usando TypeSystem para segurança em tempo de compilação
var Adults := Context.Entities<TUser>.QueryAll
  .Where(TUserType.Age >= 18)
  .OrderBy(TUserType.Name.Asc)
  .ToList;

// Filtros complexos
var NYAdults := Context.Entities<TUser>.QueryAll
  .Where((TUserType.Age > 21) and (TUserType.City = 'NY'))
  .ToList;

// Operações com strings
var AliceUsers := Context.Entities<TUser>.QueryAll
  .Where(TUserType.Name.StartsWith('Ali'))
  .ToList;
```

### Definição de Entidade

```pascal
[Table('users')]
TUser = class
private
  FId: Integer;
  FName: string;
  FAddressId: Nullable<Integer>;
  FAddress: Lazy<TAddress>;
public
  [PK, AutoInc]
  property Id: Integer read FId write FId;

  [Column('full_name')]
  property Name: string read FName write FName;

  [ForeignKey('AddressId'), NotMapped]
  property Address: TAddress read GetAddress write SetAddress;
end;
```

### Soft Delete

```pascal
// Entidade com soft delete
[Table('tasks'), SoftDelete('IsDeleted')]
TTask = class
  // ...
  property IsDeleted: Boolean read FIsDeleted write FIsDeleted;
end;

// Query normal exclui deletados
var ActiveTasks := Context.Entities<TTask>.QueryAll.ToList;

// Incluir deletados
var AllTasks := Context.Entities<TTask>.QueryAll
  .IgnoreQueryFilters
  .ToList;

// Apenas deletados
var Trash := Context.Entities<TTask>.QueryAll
  .OnlyDeleted
  .ToList;
```

---

## ⚙️ Configuração de Banco de Dados

### SQLite (Padrão)
Não requer configuração! Usa banco em memória.

### PostgreSQL
```pascal
TDbConfig.ConfigurePostgreSQL('localhost', 5432, 'postgres', 'postgres', 'senha');
```

### Firebird
```pascal
TDbConfig.ConfigureFirebird('C:\temp\test.fdb', 'SYSDBA', 'masterkey');
```

### SQL Server
```pascal
// Autenticação Windows
TDbConfig.ConfigureSQLServerWindowsAuth('localhost', 'dext_test');

// Autenticação SQL
TDbConfig.ConfigureSQLServer('localhost', 'dext_test', 'sa', 'senha');
```

---

## 🔧 Estrutura do Projeto

```
Orm.EntityDemo/
├── Orm.EntityDemo.dpr              # Programa principal
├── EntityDemo.DbConfig.pas         # Configuração do banco
├── EntityDemo.Entities.pas         # Definições de entidades
├── EntityDemo.Entities.Info.pas    # Metadados do TypeSystem
├── EntityDemo.Tests.Base.pas       # Classe base de testes
├── EntityDemo.Tests.*.pas          # Suítes de teste individuais
└── README.md                       # Este arquivo
```

---

## 🐛 Solução de Problemas

### "Driver not found"
Certifique-se de que os drivers FireDAC estão na cláusula uses:
- SQLite: `FireDAC.Phys.SQLite`
- PostgreSQL: `FireDAC.Phys.PG`
- Firebird: `FireDAC.Phys.FB`
- SQL Server: `FireDAC.Phys.MSSQL`

### "Table already exists"
Os testes automaticamente removem tabelas antes de executar. Se ver este erro:
1. Delete o arquivo `test.db` (se usando SQLite arquivo)
2. Ou remova manualmente as tabelas no seu banco

### Memory Leaks Reportados
Alguns leaks são esperados com singletons do FDConnection. O framework usa FastMM4 para detecção de leaks.

---

## 📚 Documentação Relacionada

- [Guia do Dext ORM](../../Docs/orm-guide.md)
- [Suporte a Tipos Nullable](../../Docs/NULLABLE_SUPPORT.md)
- [Configuração de Banco de Dados](../../Docs/DATABASE_CONFIG.md)
- [English Version](README.md)

---

## 📄 Licença

Este exemplo faz parte do Dext Framework e está licenciado sob a Apache License 2.0.

---

*Happy Coding! 🚀*
