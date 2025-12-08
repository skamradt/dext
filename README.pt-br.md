[ 🇺🇸 English ](README.md)

# Dext Framework - Modern Full-Stack Development for Delphi

> ⚠️ **Status: Desenvolvimento Ativo**
> O projeto está em fase de implementação da versão 1.0. A API pública, sintaxe fluente e nomes de métodos estão sujeitos a alterações (breaking changes) sem aviso prévio até o lançamento da primeira versão estável.

**Dext** é um ecossistema completo para desenvolvimento moderno em Delphi, combinando um framework web de alta performance (inspirado em ASP.NET Core) com um ORM robusto e ferramentas de infraestrutura avançadas.

## 🎯 Filosofia e Objetivos

*   **Paridade com .NET Core**: O objetivo primário é alcançar a paridade de funcionalidades com os frameworks equivalentes do ecossistema .NET (ASP.NET Core, EF Core), mantendo-se atualizado com as novidades da plataforma.
*   **Performance Nativa**: Após a estabilização funcional da v1, o foco total será em **otimização de performance**, visando competir com frameworks de alta velocidade.
*   **Inovação**: Embora inspirado no .NET, o Dext não se limita a ele, buscando implementar soluções que façam sentido especificamente para a linguagem Delphi.

---

## 📄 Licença

Este projeto é licenciado sob a **Apache License 2.0** (a mesma utilizada pelo .NET Core). Isso permite o uso livre em projetos comerciais e open-source, com a segurança de uma licença permissiva e moderna.

---

## 🚀 Módulos Principais

### 🌐 Dext.Web (Web Framework)
Um framework HTTP leve e poderoso para construir REST APIs e microserviços.
- **Minimal APIs**: Sintaxe fluente e concisa para definição de rotas.
- **Controllers**: Suporte tradicional baseado em classes para APIs complexas.
- **Smart Binding**: Serialização e validação automática de JSON para Records/Classes.
- **Middlewares**: Pipeline de requisição modular e extensível.

### 🗄️ Dext.Entity (ORM)
Um ORM moderno focado em produtividade e performance.
- **Code-First**: Defina seu banco de dados usando classes Delphi.
- **Migrations**: Controle de versão do esquema do banco de dados via CLI.
- **Fluent Query API**: Consultas fortemente tipadas e expressivas.
- **Change Tracking**: Controle automático de mudanças e persistência otimizada.
- **Multi-Database**: Suporte para SQL Server, PostgreSQL, Firebird, MySQL, Oracle e SQLite.

### ⚙️ Dext.Core (Infraestrutura)
A fundação do framework, utilizável em qualquer tipo de aplicação.
- **Dependency Injection**: Container IOC completo e rápido.
- **Configuration**: Sistema de configuração flexível (JSON, Variáveis de Ambiente).
- **Logging**: Abstração de log estruturado.
- **Async/Await**: Primitivas para programação assíncrona real.

---

## 📚 Índice de Documentação

### 🚀 Começando
- [Visão Geral do Framework](Docs/Dext%20Web%20Framework.md)
- [Estrutura do Projeto](Docs/Project%20Dext.md)
- [Minimal API - Guia Rápido](Docs/Dext%20Minimal%20API.md)

### 🌐 Web API
- **Roteamento & Endpoints**
  - [Minimal API](Docs/MinimalAPI.md)
  - [Controllers](Docs/CONTROLLERS_IMPLEMENTATION.md)
  - [Model Binding](Docs/Dext%20Model%20Binding.md)
  - [Validação](Docs/ModelBinding.md) # (Inclui validação)
- **Segurança & Middleware**
  - [Autenticação JWT](Docs/JWT-Authentication.md)
  - [CORS](Docs/CORS.md)
  - [Rate Limiting](Docs/Rate-Limiting.md)
  - [Middlewares](Docs/Dext%20-%20Middlewares.md)
- **Avançado**
  - [Background Services](Docs/BackgroundServices.md)
  - [Action Filters](Docs/ActionFilters.md)
  - [Swagger / OpenAPI](Docs/SWAGGER.md)

### 🗄️ Acesso a Dados (ORM)
- [Comparativo & Recursos](Docs/ORM_COMPARISON_2024.md)
- [Configuração de Banco de Dados](Docs/DATABASE_CONFIG.md)
- [Fluent Query API](Docs/FLUENT_QUERY_API.md)
- [Migrations](Docs/MIGRATIONS_GUIDE.md)
- [Lazy Loading](Docs/LAZY_LOADING_ADVANCED.md)
- [Bulk Operations](Docs/BULK_OPERATIONS.md)
- [Soft Delete](Docs/SOFT_DELETE.md)

### ⚙️ Core & Infraestrutura
- [Dependency Injection & Scopes](Docs/ScopedServices.md)
- [Configuration & Options Pattern](Docs/OptionsPattern.md)
- [Async Programming](Docs/ASYNC_API.md)
- [Caching](Docs/Caching.md)

---

## 💻 Requisitos

- **Delphi**: Recomendado Delphi 10.4 Sydney ou superior (devido ao uso extensivo de features modernas da linguagem).
- **Indy**: Utiliza componentes Indy (já inclusos no Delphi) para a camada de transporte HTTP (sujeito a substituição/otimização futura).

## 📦 Instalação e Configuração

1. **Clone o repositório:**
   ```bash
   git clone https://github.com/dext-framework/dext.git
   ```

2. **Configure o Library Path no Delphi:**
   Adicione os seguintes caminhos ao seu projeto ou IDE:
   - `\Sources\Core`
   - `\Sources\Core\Drivers`
   - `\Sources\Entity` (se usar o ORM)

3. **Dependências:**
   - O framework utiliza `FastMM5` (recomendado para debug de memória).
   - Drivers de banco de dados nativos (FireDAC, etc) são suportados.

---

## ⚡ Exemplo Rápido (Minimal API)

```pascal
program MyAPI;

uses
  Dext.Core.WebApplication,
  Dext.Http.Results;

begin
  var App := TDextApplication.Create;
  var Builder := App.GetApplicationBuilder;

  // Rota simples
  Builder.MapGetR<IResult>('/hello', 
    function: IResult
    begin
      Result := Results.Ok('{"message": "Hello Dext!"}');
    end);

  // Rota com parâmetro e binding
  Builder.MapGetR<Integer, IResult>('/users/{id}',
    function(Id: Integer): IResult
    begin
      Result := Results.Json(Format('{"userId": %d}', [Id]));
    end);

  App.Run(8080);
end.
```

## 💎 Exemplo ORM (Fluent Query)

O Dext ORM permite consultas expressivas e fortemente tipadas, eliminando SQL strings mágicas:

```pascal
// Consulta complexa com Joins e Filtros
// O: TOrder (Alias/Proxy)
var Orders := DbContext.Orders
  .Where((O.Status = TOrderStatus.Paid) and (O.Total > 1000))
  .Include('Customer')       // Eager Loading
  .Include('Items')
  .OrderByDescending('Date')
  .Take(50)
  .ToList;

// Bulk Update de alta performance
DbContext.Products
  .Where(P.Category = 'Outdated') // P: TProduct
  .Update                         // Inicia update em massa
  .Set('Active', False)           // Define campos
  .Execute;
```

## ⚡ Exemplo Async (Fluent Tasks)

Esqueça a complexidade de `TThread`. Use uma API moderna baseada em Promises/Tasks:

```pascal
// Encadeamento de tarefas assíncronas
var Task := TAsyncTask.Run<TUserProfile>(
  function: TUserProfile
  begin
    // Executa em background thread
    Result := ExternalApi.GetUserProfile(UserId);
  end)
  .ThenBy<Boolean>( // Transforma o resultado (Map)
    function(Profile: TUserProfile): Boolean
    begin
      Result := Profile.IsVerified and Profile.HasCredit;
    end)
  .OnComplete( // Volta para a UI Thread automaticamente
    procedure(IsVerified: Boolean)
    begin
      if IsVerified then
        ShowSuccess('User Verified!')
      else
        ShowError('Verification Failed');
    end)
  .Start; // Inicia a execução
```

## 🧪 Exemplos e Testes

O repositório contém projetos de exemplo práticos:

- **`Examples/TaskFlowAPI`**: Uma API REST completa demonstrando arquitetura em camadas, ORM, Auth e DI.
- **`Examples/EntityDemo`**: Demonstração focada nos recursos do ORM (CRUD, Migrations).
- **`Examples/WebFrameworkTests`**: Suite de testes de integração e estabilidade.

---

## 🗺️ Roadmaps

Acompanhe o desenvolvimento do projeto:
- [ORM Roadmap](Docs/ORM_ROADMAP.md)
- [Web Framework Roadmap](Docs/WEB_ROADMAP.md)
- [Infra & IDE Roadmap](Docs/INFRA_ROADMAP.md)

---

**Dext Framework** - *Performance nativa, produtividade moderna.*
Desenvolvido com ❤️ pela comunidade Delphi.
