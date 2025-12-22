# Instalação e Configuração do Dext Framework

Este guia descreve os passos necessários para compilar o framework e configurar o Delphi para utilização do Dext.

## 1. Compilação dos Fontes

O Dext Framework é projetado para que seus binários compilados (`.dcu`, `.bpl`, `.dcp`) sejam gerados em uma pasta de saída centralizada, facilitando a configuração.

1.  Abra o grupo de projetos principal:
    *   `Sources\DextFramework.groupproj`
2.  No Project Manager, clique com o botão direito no nó raiz (**ProjectGroup**) e selecione **Build All**.
3.  Aguarde a compilação de todos os pacotes.

Os arquivos compilados serão gerados automaticamente na pasta:
*   `Output\$(Platform)\$(Config)`
*   *Exemplo:* `Output\Win32\Debug`

## 2. Configuração do Library Path (DCUs)

Para que a IDE encontre os arquivos compilados do framework:

1.  No Delphi, vá em **Tools** > **Options** > **Language** > **Delphi** > **Library**.
2.  Selecione a **Platform** desejada (ex: Windows 32-bit).
3.  No campo **Library Path**, adicione o caminho absoluto para a pasta de saída gerada no passo anterior.
    *   Exemplo: `C:\dev\Dext\DextRepository\Output\Win32\Debug`

> **Nota:** Se você alternar entre as configurações de Debug e Release ou Plataformas (Win32/Win64), lembre-se de ajustar este caminho ou adicionar ambos.

## 3. Configuração dos Fontes (Browsing/Library Path)

Para permitir a navegação no código fonte (Ctrl+Click) e debugging detalhado, adicione os seguintes diretórios ao **Library Path** (ou Browsing Path) da sua IDE.

Substitua `[Raiz]` pelo caminho onde você clonou o repositório (ex: `C:\dev\Dext\DextRepository\`).

```text
[Raiz]\Sources\Core
[Raiz]\Sources\Core\Base
[Raiz]\Sources\Core\Json
[Raiz]\Sources\Data
[Raiz]\Sources\Hosting\CLI
[Raiz]\Sources\Hosting\CLI\Commands
[Raiz]\Sources\Web
[Raiz]\Sources\Web\Caching
[Raiz]\Sources\Web\Hosting
[Raiz]\Sources\Web\Indy
[Raiz]\Sources\Web\Middleware
[Raiz]\Sources\Web\Mvc
```

### Lista Pronta para Copiar (Exemplo Baseado em `C:\dev\Dext\DextRepository`)

```text
C:\dev\Dext\DextRepository\Sources\Core
C:\dev\Dext\DextRepository\Sources\Core\Base
C:\dev\Dext\DextRepository\Sources\Core\Json
C:\dev\Dext\DextRepository\Sources\Data
C:\dev\Dext\DextRepository\Sources\Hosting\CLI
C:\dev\Dext\DextRepository\Sources\Hosting\CLI\Commands
C:\dev\Dext\DextRepository\Sources\Web
C:\dev\Dext\DextRepository\Sources\Web\Caching
C:\dev\Dext\DextRepository\Sources\Web\Hosting
C:\dev\Dext\DextRepository\Sources\Web\Indy
C:\dev\Dext\DextRepository\Sources\Web\Middleware
C:\dev\Dext\DextRepository\Sources\Web\Mvc
```

*Observação: As pastas `Http` e `Expressions` mencionadas em versões anteriores foram renomeadas ou reorganizadas para `Web` e outros módulos.*

## 4. Verificação

Para confirmar que a instalação está correta:

1.  Feche o grupo de projetos do framework.
2.  Abra o grupo de exemplos:
    *   `Examples\DextExamples.groupproj`
3.  Execute **Build All**.
4.  Se todos os projetos compilarem com sucesso, o ambiente está configurado corretamente.
