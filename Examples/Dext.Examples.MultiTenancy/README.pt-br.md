# 🏢 Exemplo de Multi-Tenancy

Demonstra como construir uma **aplicação multi-tenant estilo SaaS** com Dext, com isolamento de tenant e acesso a dados por tenant.

> 📦 Exemplo: `Dext.Examples.MultiTenancy`

## Funcionalidades

- **Middleware de Resolução de Tenant** - Extrai ID do tenant do header `X-Tenant-Id`
- **Gerenciamento de Tenants** - Criar e listar tenants
- **Dados por Tenant** - Produtos são isolados por tenant
- **Padrão Schema-per-Tenant** - Padrão conceitual para isolamento de banco de dados

## Executando o Exemplo

```bash
# Compilar e executar
msbuild Dext.Examples.MultiTenancy.dproj
.\..\..\Output\Dext.Examples.MultiTenancy.exe
```

## Endpoints da API

### Gerenciamento de Tenants (Público)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/tenants` | Criar um novo tenant |
| GET | `/api/tenants` | Listar todos os tenants |
| GET | `/api/tenants/{id}` | Obter tenant por ID |

### Gerenciamento de Produtos (Por Tenant)

> **Requer Header:** `X-Tenant-Id: <tenant-id>`

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/products` | Listar produtos do tenant |
| POST | `/api/products` | Criar produto para o tenant |
| GET | `/api/products/{id}` | Obter produto por ID |

## Exemplos de Uso

### 1. Criar um Tenant

```bash
curl -X POST http://localhost:8080/api/tenants \
  -H "Content-Type: application/json" \
  -d '{"name": "Acme Corp", "subdomain": "acme"}'
```

### 2. Criar um Produto para o Tenant

```bash
curl -X POST http://localhost:8080/api/products \
  -H "Content-Type: application/json" \
  -H "X-Tenant-Id: abc123" \
  -d '{"name": "Widget", "description": "Um ótimo widget", "price": 29.99, "stock": 100}'
```

### 3. Listar Produtos do Tenant

```bash
curl http://localhost:8080/api/products \
  -H "X-Tenant-Id: abc123"
```

## Estratégias de Multi-Tenancy

Este exemplo demonstra a abordagem **Coluna Tenant ID**, onde todos os tenants compartilham um banco de dados mas os dados são filtrados por `tenant_id`.

Outras estratégias (não implementadas neste demo):
- **Banco de Dados Separado por Tenant** - Cada tenant tem seu próprio arquivo de banco de dados
- **Schema por Tenant** - Cada tenant tem seu próprio schema em um banco de dados compartilhado (PostgreSQL)

## Arquitetura

```
Dext.Examples.MultiTenancy/
├── Dext.Examples.MultiTenancy.dpr    # Programa principal
├── Domain/
│   ├── MultiTenancy.Entities.pas     # Entidades TTenant, TProduct
│   └── MultiTenancy.DbContext.pas    # Contextos de banco de dados
├── Middleware/
│   └── MultiTenancy.Middleware.pas   # Resolução de tenant
└── Features/
    ├── MultiTenancy.Service.pas      # Serviços de Tenant e Product
    └── MultiTenancy.Endpoints.pas    # Endpoints da API
```

## Veja Também

- [Configuração](../../Docs/Book.pt-br/10-avancado/configuracao.md) - Config por ambiente
- [Middleware](../../Docs/Book.pt-br/02-framework-web/middleware.md) - Middleware customizado
