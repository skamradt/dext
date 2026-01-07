# 9. Ferramenta CLI

O CLI `dext` fornece comandos para migrations, testes, scaffolding e mais.

## Capítulos

1. [Comandos](comandos.md) - Visão geral de todos os comandos
2. [Migrations](migrations.md) - Gerenciamento de schema de banco
3. [Scaffolding](scaffolding.md) - Gerar entidades a partir do BD
4. [Testes](testes.md) - Executar testes com cobertura
5. [Dashboard](dashboard.md) - Interface web para monitoramento

## Referência Rápida

```bash
# Ajuda
dext help

# Migrations
dext migrate:up
dext migrate:down
dext migrate:list
dext migrate:generate

# Testes
dext test
dext test --coverage
dext test --html

# Scaffolding
dext scaffold -c "meubanco.db" -d sqlite -o Entities.pas

# Dashboard
dext ui
dext ui --port 8080
```

## Instalação

O CLI está embutido na sua aplicação. Adicione Dext.Hosting.CLI aos uses:

```pascal
uses
  Dext.Hosting.CLI;

begin
  var CLI := TDextCLI.Create(
    function: IDbContext
    begin
      Result := TAppDbContext.Create(Connection, Dialect);
    end
  );
  
  if CLI.Run then
    Exit; // Comando CLI executado
    
  // Início normal da aplicação...
end.
```

---

[← Testes](../08-testes/README.md) | [Próximo: Comandos →](comandos.md)
