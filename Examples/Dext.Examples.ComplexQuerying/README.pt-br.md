# 📊 Exemplo de Queries Complexas

Demonstra **queries avançadas de ORM**, **campos JSON**, **agregações** e **relatórios** com Dext.Entity.

> 📦 Exemplo: `Dext.Examples.ComplexQuerying`

## Funcionalidades

- **Campos JSON** - Entidades com colunas JSON (arrays e objetos)
- **Queries Fluentes** - Encadear filtros com `.Where()`, `.OrderBy()`, etc.
- **Critérios de Busca** - Filtragem dinâmica baseada em input do usuário
- **Agregações** - Relatórios de vendas e top clientes
- **Queries por Data** - Filtrar por intervalos de data

## Executando o Exemplo

```bash
# Compilar e executar
msbuild Dext.Examples.ComplexQuerying.dproj
.\..\..\Output\Dext.Examples.ComplexQuerying.exe
```

## Endpoints da API

### Pedidos

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/orders` | Listar todos os pedidos |
| GET | `/api/orders/{id}` | Obter pedido por ID |
| GET | `/api/orders/status/{status}` | Filtrar por status |
| GET | `/api/orders/customer/{id}` | Filtrar por cliente |
| POST | `/api/orders/search` | Busca avançada |

### Relatórios

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/reports/sales` | Vendas por status |
| GET | `/api/reports/top-customers?top=5` | Top clientes |

### Utilitários

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/seed` | Inserir dados de exemplo |

## Exemplos de Uso

### 1. Inserir Dados de Exemplo

```bash
curl -X POST http://localhost:8080/api/seed
```

### 2. Listar Pedidos

```bash
curl http://localhost:8080/api/orders
```

### 3. Filtrar por Status

```bash
curl http://localhost:8080/api/orders/status/pending
```

### 4. Relatório de Vendas

```bash
curl http://localhost:8080/api/reports/sales
```

## Arquitetura

```
Dext.Examples.ComplexQuerying/
├── Dext.Examples.ComplexQuerying.dpr
├── Domain/
│   ├── ComplexQuerying.Entities.pas
│   └── ComplexQuerying.DbContext.pas
└── Features/
    ├── ComplexQuerying.Service.pas
    └── ComplexQuerying.Endpoints.pas
```

## Veja Também

- [Entidades ORM](../../Docs/Book.pt-br/05-orm/entidades.md)
- [Querying](../../Docs/Book.pt-br/05-orm/querying.md)
