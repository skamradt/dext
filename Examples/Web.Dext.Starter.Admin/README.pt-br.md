# 🚀 Dext Admin Starter Kit

Um template profissional "SaaS Admin" full-stack construído com **Delphi (Dext)** e **Tecnologias Web Modernas**.

> **Caso de Uso:** Perfeito para desenvolvedores migrando de VCL/IntraWeb que desejam uma aparência moderna sem a complexidade de ferramentas de build Node.js/Webpack.

---

## ✨ Funcionalidades

*   **Lógica em Delphi**: API Backend, Banco de Dados e Roteamento totalmente gerenciados pelo Dext.
*   **Frontend Moderno**: Usa **Tailwind CSS** para estilização e **HTMX** para interações dinâmicas.
*   **Zero Ferramentas de Build**: Sem `npm install`, sem `webpack`. Apenas arquivos HTML simples servidos pelo Delphi.
*   **Autenticação**: Fluxo completo de Login com autenticação baseada em JWT.
*   **Dashboard**: Gráficos interativos (Chart.js) e estatísticas em tempo real.
*   **CRUD**: Exemplo completo de gerenciamento de Clientes.
*   **Documentação da API**: Documentação Swagger/OpenAPI gerada automaticamente em `/swagger`.
*   **Tratamento de Erros**: Manipulador global de exceções com respostas de erro estruturadas.

---

## 🛠️ Stack Tecnológica

*   **Backend**: Dext Framework (Minimal APIs + Service Layer + Dext.Entity ORM)
*   **Banco de Dados**: SQLite (arquivo de configuração zero `dext_admin.db`) - Facilmente alternável para PostgreSQL
*   **Frontend**: 
    *   **HTMX**: Para navegação tipo SPA sem escrever JavaScript.
    *   **Tailwind CSS**: Para estilização utility-first (via CDN).
    *   **Alpine.js**: Para interatividade mínima no lado do cliente (toggle da Sidebar).
    *   **Chart.js**: Para visualização de dados.

---

## 🏗️ Visão Geral da Arquitetura

A aplicação segue uma arquitetura modular baseada em features (Vertical Slice Architecture), mantendo a lógica relacionada junta.

```
Web.Dext.Starter.Admin/
├── AppStartup.pas          # Configuração de DI & Middleware
├── AppResponseConsts.pas   # Templates HTML/JSON Centralizados
├── Domain/                 # Entidades & Regras de Negócio Core
│   ├── Entities/           # Entidades do Banco de Dados (ORM)
│   ├── DbContext.pas       # Contexto do Banco de Dados
│   └── DbSeeder.pas        # Lógica de Seed do Banco de Dados
├── Features/               # Vertical Slices (Endpoints + Services + DTOs)
│   ├── Auth/               # Lógica de Login & JWT
│   ├── Customers/          # CRUD de Clientes
│   ├── Dashboard/          # Estatísticas & Gráficos
│   ├── Settings/           # Perfil do Usuário
│   └── Shared/             # Utilitários e middleware compartilhados
└── wwwroot/                # Arquivos estáticos (CSS, JS, views HTML)
```

---

## 🚀 Começando

### Pré-requisitos
- Delphi 11+ (Alexandria ou posterior recomendado)
- Dext Framework instalado e configurado no Library Path

### Executando a Aplicação

1.  **Abrir Projeto**: Abra `Web.Dext.Starter.Admin.dpr` no Delphi.
2.  **Compilar**: Compile o projeto (Console Application).
3.  **Executar**: Execute o binário. Ele iniciará um servidor web em `http://localhost:8080`.
    *   *Nota*: A primeira execução criará automaticamente o banco de dados SQLite e o populará com dados de demonstração.
4.  **Login**:
    *   **Usuário**: `admin`
    *   **Senha**: `admin`
5.  **Explorar**:
    *   **Dashboard**: `http://localhost:8080/dashboard`
    *   **Swagger UI**: `http://localhost:8080/swagger`

---

## 📡 Endpoints da API

### Autenticação
| Método | Endpoint | Descrição | Autenticação Necessária |
|--------|----------|-----------|------------------------|
| GET | `/auth/login` | Página de login (HTML) | Não |
| POST | `/auth/login` | Autenticar usuário | Não |
| POST | `/auth/logout` | Deslogar usuário | Sim |

### Dashboard
| Método | Endpoint | Descrição | Autenticação Necessária |
|--------|----------|-----------|------------------------|
| GET | `/dashboard` | Página do dashboard (HTML) | Sim |
| GET | `/dashboard/stats` | Obter estatísticas do dashboard | Sim |
| GET | `/dashboard/chart` | Obter dados do gráfico | Sim |

### Clientes
| Método | Endpoint | Descrição | Autenticação Necessária |
|--------|----------|-----------|------------------------|
| GET | `/customers` | Página de lista de clientes (HTML) | Sim |
| GET | `/customers/list` | Obter todos os clientes (JSON) | Sim |
| POST | `/customers` | Criar novo cliente | Sim |
| PUT | `/customers/{id}` | Atualizar cliente | Sim |
| DELETE | `/customers/{id}` | Deletar cliente | Sim |

### Configurações
| Método | Endpoint | Descrição | Autenticação Necessária |
|--------|----------|-----------|------------------------|
| GET | `/settings` | Página de configurações (HTML) | Sim |
| GET | `/settings/profile` | Obter perfil do usuário | Sim |
| PUT | `/settings/profile` | Atualizar perfil do usuário | Sim |

---

## 🔑 Conceitos Principais

### 1. Minimal API (Endpoints)
Em vez de Controllers, usamos **Minimal APIs** (`MapGet`, `MapPost`) definidas em métodos estáticos `Map` dentro de cada Feature.

**Exemplo (`Customer.Endpoints.pas`):**
```delphi
class procedure TCustomerEndpoints.Map(App: TDextAppBuilder);
begin
  App.MapGet<ICustomerService, IHttpContext>('/customers/',
    procedure(Service: ICustomerService; Context: IHttpContext)
    begin
       // Use Injeção de Parâmetros Genéricos!
       var Data := Service.GetAll;
       // Retornar Resposta...
    end);
end;
```

### 2. Padrão Service Layer
A lógica de negócio é desacoplada dos Endpoints usando Services (`ICustomerService`, `IDashboardService`).
- **Endpoint**: Analisa Request -> Chama Service -> Formata Response (HTML/JSON).
- **Service**: Lógica de Negócio -> Acesso ao Banco de Dados (`TAppDbContext`).

**Exemplo (`Customer.Service.pas`):**
```delphi
function TCustomerService.GetAll: IList<TCustomer>;
begin
  Result := FDb.Entities<TCustomer>.ToList;
end;
```

### 3. HTMX & Server-Side Rendering
A UI é dinâmica mas renderizada no servidor.
- **Atributos HTMX** (`hx-get`, `hx-target`) no HTML disparam atualizações parciais.
- **Endpoints** retornam snippets HTML (definidos em `AppResponseConsts.pas`) em vez de páginas completas ou JSON.

### 4. Tratamento de Exceções
O middleware global de tratamento de exceções (`UseExceptionHandler`) captura todas as exceções não tratadas e retorna:
- Resposta de erro **JSON** para endpoints de API
- Página de erro **HTML** para requisições do navegador

### 5. Autenticação JWT
- Tokens são gerados no login e validados em cada requisição
- Endpoints protegidos verificam automaticamente a validade do JWT
- Middleware customizado (`TAdminAuthMiddleware`) gerencia a autorização

---

## 🔧 Configuração

### Alternando Provedor de Banco de Dados

Edite `AppStartup.pas` linha 83:

```delphi
const
  DB_PROVIDER = 'SQLITE'; // Mude para 'POSTGRES' para PostgreSQL
```

Para PostgreSQL, atualize a string de conexão (linhas 89-94):
```delphi
Options.ConnectionString := 
  'Server=localhost;' +
  'Port=5432;' +
  'Database=dext_admin;' +
  'User_Name=postgres;' +
  'Password=postgres;';
```

### Alterando o Secret do JWT

Atualize a chave secreta em `AppStartup.pas` (linhas 73 e 166):
```delphi
'dext-admin-secret-key-change-in-production-2024'
```

⚠️ **Importante**: Sempre use um secret forte e único em produção!

---

## 🛠️ Adicionando Novas Features

Para adicionar uma nova feature (ex: "Pedidos"):

1.  **Definir Entidades**: Crie `TOrder` em `Domain\Entities`.
2.  **Criar Service**: Defina `IOrderService` e implemente usando `TAppDbContext`.
3.  **Criar DTOs**: Defina DTOs de request/response em `Features\Orders\Order.Dto.pas`.
4.  **Criar Endpoints**: Crie `TOrderEndpoints` injetando `IOrderService`.
5.  **Registrar DI**: Adicione o service em `AppStartup.ConfigureServices`.
6.  **Conectar**: Chame `TOrderEndpoints.Map(WebApp)` em `AppStartup.Configure`.

---

## 🧪 Testes

### Testes Manuais
1. Use a interface web em `http://localhost:8080`
2. Teste os endpoints da API usando Swagger UI em `http://localhost:8080/swagger`

### Testes de Carga
Use os scripts PowerShell incluídos:
```powershell
# Executar teste de carga
.\load_test.ps1

# Executar teste de carga apenas (sem compilação)
.\load_test_only.ps1
```

---

## 🐛 Solução de Problemas

### Problemas com Banco de Dados
**Problema**: Erro "Database is locked"
**Solução**: Certifique-se de que o modo WAL está habilitado (padrão neste projeto). Verifique `AppStartup.pas` linha 106.

**Problema**: Arquivo de banco de dados não encontrado
**Solução**: O banco de dados é criado automaticamente na primeira execução. Certifique-se de ter permissões de escrita no diretório da aplicação.

### Problemas de Autenticação
**Problema**: Erro "Invalid token"
**Solução**: Verifique se o secret JWT corresponde entre a geração e validação do token.

**Problema**: Não consigo acessar endpoints protegidos
**Solução**: Certifique-se de estar enviando o token JWT no header `Authorization`: `Bearer <token>`

### Porta Já em Uso
**Problema**: Erro "Address already in use"
**Solução**: Altere a porta em `Web.Dext.Starter.Admin.dpr` linha 54:
```delphi
App.Run(8080); // Mude para outra porta, ex: 8081
```

---

## 📚 Leitura Adicional

- [O Livro do Dext](../../Docs/Book.pt-br/README.md) - Documentação Completa do Framework
- [Padrão Application Startup](../../Docs/Book.pt-br/01-primeiros-passos/inicializacao-aplicacao.md) - Melhores práticas com TStartup
- [Autenticação JWT](../../Docs/Book.pt-br/03-autenticacao/jwt-auth.md) - Guia de auth baseada em tokens
- [ORM Primeiros Passos](../../Docs/Book.pt-br/05-orm/primeiros-passos.md) - Integração com banco de dados
- [English Version](README.md)

---

## 📄 Licença

Este exemplo faz parte do Dext Framework e está licenciado sob a Apache License 2.0.
