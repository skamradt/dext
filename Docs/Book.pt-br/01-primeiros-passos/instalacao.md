# Instalação

## Requisitos

- **Delphi**: 11.x Alexandria ou 12.x Athens
- **Target**: Win32 ou Win64
- **FireDAC**: Necessário para ORM (incluso no Delphi)

## Métodos de Instalação

### Opção 1: Clonar do GitHub (Recomendado)

```bash
git clone https://github.com/ArmyOfPirates/Dext.git
cd Dext
```

### Opção 2: Download ZIP

Baixe a versão mais recente em [GitHub Releases](https://github.com/ArmyOfPirates/Dext/releases).

## Configuração da IDE

### 1. Adicionar Caminhos de Biblioteca

No Delphi, vá em **Tools → Options → Language → Delphi → Library**:

Adicione esses caminhos ao **Library Path**:

```
<DextPath>\Sources
<DextPath>\Sources\Core
<DextPath>\Sources\Data
<DextPath>\Sources\Testing
<DextPath>\Sources\Web
<DextPath>\Sources\Hosting
```

### 2. Instalar Pacotes Design-Time (Opcional)

Para integração com a IDE, instale os pacotes em:
```
<DextPath>\Packages\
```

## Verificar Instalação

Crie uma nova Console Application e adicione este código:

```pascal
program VerificarDext;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Web;

begin
  WriteLn('Dext está instalado corretamente!');
  WriteLn('Pressione Enter para sair...');
  ReadLn;
end.
```

Se compilar, você está pronto! 🎉

---

[← Voltar para Primeiros Passos](README.md) | [Próximo: Hello World →](hello-world.md)
