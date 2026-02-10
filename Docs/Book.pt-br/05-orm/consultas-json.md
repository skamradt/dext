# Consultas JSON

Consulte dados dentro de colunas JSON/JSONB usando a API de expressões fluentes.

## Visão Geral

O Dext ORM suporta consultas em dados JSON armazenados em colunas do banco de dados. Isso é útil quando você tem dados semi-estruturados que não se encaixam em um schema fixo.

## Configuração

### 1. Marcar Coluna como JSON

Use o atributo `[JsonColumn]` em propriedades string que armazenam JSON:

```pascal
type
  [Table('UserMetadata')]
  TUserMetadata = class
  private
    FId: Integer;
    FName: string;
    FSettings: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    [JsonColumn]  // ou [JsonColumn(True)] para JSONB no PostgreSQL
    property Settings: string read FSettings write FSettings;
  end;
```

### 2. Tipos de Coluna no Banco

| Banco de Dados | Tipo de Coluna Recomendado |
|----------------|---------------------------|
| PostgreSQL | `JSONB` (indexado) ou `JSON` |
| SQLite | `TEXT` (requer extensão JSON1) |
| MySQL | `JSON` |
| SQL Server | `NVARCHAR(MAX)` |

## Consultando Propriedades JSON

Use o método `.Json('caminho')` em expressões de propriedade:

### Acesso Simples a Propriedades

```pascal
// Dados: {"role": "admin", "theme": "dark"}

var Admins := Context.UserMetadata
  .Where(Prop('Settings').Json('role') = 'admin')
  .ToList;
```

**SQL Gerado (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE "Settings" #>> '{role}' = :p1
```

### Acesso a Propriedades Aninhadas

Acesse estruturas JSON aninhadas usando notação de ponto:

```pascal
// Dados: {"profile": {"details": {"level": 5}}}

var Result := Context.UserMetadata
  .Where(Prop('Settings').Json('profile.details.level') = 5)
  .ToList;
```

**SQL Gerado (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE "Settings" #>> '{profile,details,level}' = :p1::text
```

> 💡 Valores numéricos são automaticamente convertidos para TEXT ao consultar JSON.

### Verificar NULL/Chaves Inexistentes

Consulte registros onde uma chave JSON não existe ou é nula:

```pascal
// Encontrar registros sem a chave "nonexistent"
var Result := Context.UserMetadata
  .Where(Prop('Settings').Json('nonexistent').IsNull)
  .ToList;
```

**SQL Gerado (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE ("Settings" #>> '{nonexistent}' IS NULL)
```

## Comportamento Específico por Banco

### PostgreSQL

- Usa operador `#>>` para extração de texto
- Suporta tipo `JSONB` com indexação e otimização
- Cast automático `::text` ao comparar com valores não-string
- Cast automático `::jsonb` no INSERT para propriedades `[JsonColumn]`

### SQLite

- Usa função `json_extract()`
- **Requer** SQLite compilado com `SQLITE_ENABLE_JSON1`
- Habilite em `Dext.inc`: `{$DEFINE DEXT_ENABLE_SQLITE_JSON}`

### MySQL

- Usa funções `JSON_EXTRACT()` e `JSON_UNQUOTE()`
- Tipo de coluna `JSON` nativo

### SQL Server

- Usa função `JSON_VALUE()`
- Armazene em colunas `NVARCHAR(MAX)`

## INSERT com JSONB (PostgreSQL)

Ao usar `[JsonColumn(True)]` (UseJsonB = True), o ORM automaticamente converte valores string para `jsonb` durante o INSERT:

```pascal
var Meta := TUserMetadata.Create;
Meta.Name := 'Admin';
Meta.Settings := '{"role": "admin"}';  // String com conteúdo JSON

Context.UserMetadata.Add(Meta);
Context.SaveChanges;
```

**SQL Gerado:**
```sql
INSERT INTO "UserMetadata" ("Name", "Settings") 
VALUES (:p1, :p2::jsonb)
```

## Exemplo Completo

```pascal
procedure TestJsonQueries(Context: TMyDbContext);
var
  User: TUserMetadata;
  Results: IList<TUserMetadata>;
begin
  // Inserir dados de teste
  User := TUserMetadata.Create;
  User.Name := 'Admin';
  User.Settings := '{"role": "admin", "permissions": ["read", "write"]}';
  Context.UserMetadata.Add(User);
  
  User := TUserMetadata.Create;
  User.Name := 'Guest';
  User.Settings := '{"role": "guest", "permissions": ["read"]}';
  Context.UserMetadata.Add(User);
  
  Context.SaveChanges;
  Context.DetachAll;
  
  // Consultar por propriedade JSON
  Results := Context.UserMetadata
    .Where(Prop('Settings').Json('role') = 'admin')
    .ToList;
    
  Assert(Results.Count = 1);
  Assert(Results[0].Name = 'Admin');
end;
```

## Limitações

1. **Coerção de Tipos**: Extração JSON retorna TEXT; comparações numéricas requerem cast
2. **Indexação**: Apenas PostgreSQL JSONB suporta indexação nativa
3. **Consultas Complexas**: Indexação de arrays e operadores avançados ainda não suportados
4. **SQLite**: Requer sqlite3.dll compilada com suporte a JSON

---

[← Consultas](consultas.md) | [Próximo: Relacionamentos →](relacionamentos.md)
