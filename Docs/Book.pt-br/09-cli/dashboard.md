# CLI: Dashboard UI

Acompanhe o status do seu projeto através de uma interface web moderna.

## Iniciando a UI

```bash
dext ui
```
O servidor iniciará (geralmente na porta 3000) e você poderá acessar em: `http://localhost:3000`.

## O que tem no Dashboard?

1. **Visão Geral**: Estatísticas de saúde do projeto, última cobertura de código e atividade recente.
2. **Ambiente**: Lista das versões do Delphi detectadas no seu sistema (Environment Manager).
3. **Projetos**: Gerenciamento de múltiplos projetos Dext no seu workspace.
4. **Relatórios de Testes**: Navegue graficamente pelos resultados dos últimos testes e cobertura.
5. **Logs em Tempo Real**: Veja o que está acontecendo no framework (migrations, logs HTTP) via WebSockets (SignalR).

## Configuração de Porta

```bash
dext ui --port 8080
```

---

[← Testes](testes.md) | [Próximo: Tópicos Avançados →](../10-avancado/README.md)
