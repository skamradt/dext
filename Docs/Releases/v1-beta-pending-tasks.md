# 📋 Dext V1 Beta - Tarefas Pendentes

*Última atualização: 2026-01-11*

Este documento centraliza as tarefas pendentes para o lançamento do V1 Beta. Marque com `[x]` conforme forem concluídas.

---

## 📚 Exemplos a Criar

### Alta Prioridade
- [x] **Dext.Examples.Streaming** - Upload/download de arquivos grandes (Stream Writing + Multipart)
- [x] **Dext.Examples.MultiTenancy** - Implementação SaaS completa (Schema per Tenant)
- [x] **Dext.Examples.ComplexQuerying** - Queries avançadas ORM com JSON, Arrays e relatórios

### Atualização de Exemplos Existentes
- [x] **Web.TaskFlowAPI** - Atualizar para usar Cookies e Compression
- [x] **Web.Dext.Starter.Admin** - Revisar e alinhar com best practices atuais

---

## 🧪 Testes Pendentes de Exemplos

> Projetos criados/atualizados que precisam de testes de funcionamento

- [ ] **Web.Dext.Starter.Admin** - Testar compression middleware e JWT config
- [ ] **Web.TaskFlowAPI** - Executar `Test.Web.TaskFlowAPI.ps1`
- [ ] **Dext.Examples.MultiTenancy** - Executar `Test.MultiTenancy.ps1`
- [ ] **Dext.Examples.ComplexQuerying** - Executar `Test.ComplexQuerying.ps1`

---

## 📖 Documentação

- [x] **The Dext Book** - Completo (55 arquivos EN + 55 arquivos PT)
- [x] **Installation Guides** - Guias de instalação atualizados com Environment Variables
- [x] **CLI Documentation** - Comando `facade` documentado
- [x] **API Reference** - Gerada automaticamente com DextDoc (Custom Node.js Generator)
- [ ] **Book Review & Verification** - Revisar conteúdo técnico e executar códigos/exemplos de todos os capítulos
- [ ] **Video Series (Screencasts)** - Gravar vídeos curtos cobrindo os capítulos principais do livro

---

## 🛠️ Qualidade de Código

- [ ] **Automação de Instalação** 
- [ ] **Estratégia de Versioning** - `LIBSUFFIX AUTO` ou sufixos fixos por versão IDE
- [ ] **Otimização de Generics** - Reduzir code bloat e melhorar tempo de compilação
- [ ] **Code Review Geral** - Consistência, memory leaks, exceções não tratadas
- [ ] **Padronização de Formatação** - Object Pascal Style Guide
- [ ] **Agent Guidelines** - `.agent/rules.md` ou `CONTRIBUTING_AI.md`

---

## 🧪 Testes

### Infraestrutura
- [ ] **Docker-Compose** - Environment para todos os bancos de dados
- [ ] **Run-DBTests.ps1** - Script unificado de testes (já iniciado em `infra/tests/`)

### Bancos de Dados Pendentes
- [x] MySQL / MariaDB - Testes automatizados
- [ ] InterBase - Testes automatizados
- [ ] Oracle - Testes automatizados

### Testes Web
- [ ] HTTP Integration Tests (Cookies, Upload binário, Compression)
- [ ] Testes de Concorrência (k6 / Apache Bench)

---

## 🚀 Benchmarks

- [ ] **Web Framework** - Hello World, JSON Serialization, DB Read (wrk/k6)
- [ ] **ORM** - Bulk Insert 10k, Select com Hydration 10k

---

## 🔧 Features Parciais / Validação

- [ ] **HTTPS/SSL** - Validar OpenSSL 1.0.2, 1.1, 3.0 e Taurus TLS
- [ ] **Advanced Querying** - `Join` e `GroupBy` gerando SQL (atualmente In-Memory)

---

## ✅ Concluído Recentemente

- [x] Real-Time Hubs (SignalR) - `Dext.Web.Hubs`
- [x] WebSockets nativos - Implementado em `Dext.Web.Hubs`
- [x] Server-Sent Events (SSE) - Implementado em `Dext.Web.Hubs`
- [x] Basic Authentication Middleware
- [x] CLI Dashboard (Material 3 embedded)
- [x] Scaffolding Command (`dext scaffold`)
- [x] Facade Command (`dext facade`) - Fix `TCascadeAction` enum constants
- [x] The Dext Book (EN + PT)

---

## 🔮 Pós-V1 (Backlog Futuro)

Estes itens foram movidos para versões futuras:

- MediatR Pattern (CQRS)
- Background Jobs/Queues (Redis/RabbitMQ)
- Scheduled Jobs (CRON)
- Docker Tooling (`dext docker init`)
- Feature Toggle
- Telemetry & OpenTelemetry
- View Engine (WebStencils/Razor-like)
- Debug Visualizers para IDE
- Fluent REST Client
- CLI REST Runner (`.http` files)
