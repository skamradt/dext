# Gerador de Documentação (`dext doc`)

O Dext CLI inclui um poderoso gerador de sites estáticos (SSG) projetado especificamente para projetos Delphi. Ele analisa automaticamente seu código-fonte (Pascal) e gera um site de documentação HTML moderno e interativo.

## Recursos

*   **Configuração Zero**: Funciona imediatamente ao escanear o diretório do seu projeto.
*   **Diagramas Mermaid**: Gera automaticamente diagramas de classe interativos (UML) para cada unidade, permitindo visualizar herança e relacionamentos.
    *   **Interativo**: Clique para colapsar/expandir diagramas.
    *   **Escalável**: Diagramas grandes ativam automaticamente a rolagem horizontal sem perder resolução.
*   **Pesquisa**: Busca textual completa no cliente para classes e unidades.
*   **Modo Claro/Escuro**: Alternador de tema integrado que persiste sua preferência.
*   **Interface Moderna**: Layout limpo e responsivo baseado em `theme.css`.

## Uso

Para gerar a documentação do seu projeto, basta executar:

```bash
dext doc
```

Isso irá:
1.  Escanear o diretório atual (recursivamente) por arquivos `.pas`.
2.  Analisar a Árvore de Sintaxe Abstrata (AST) do seu código.
3.  Gerar um site estático em `Docs/Output`.

### Opções

*   `--input <path>`: Especifica o diretório raiz do código-fonte (padrão é o diretório atual).
*   `--output <path>`: Especifica o diretório de saída (padrão é `Docs/Output`).

```bash
dext doc --input ./Sources --output ./MyDocs
```

## Saída Gerada

O comando gera um site estático independente. Você pode implantar esta pasta no GitHub Pages, Netlify ou qualquer serviço de hospedagem estática.

*   `index.html`: O ponto de entrada principal.
*   `theme.css` / `layout.css`: Estilos personalizáveis.
*   `viewer.js`: Lógica do cliente para pesquisa, temas e diagramas.
*   `API/*.html`: Páginas individuais para cada unidade analisada.

## Recursos de Diagrama

O gerador utiliza **Mermaid.js** para renderizar diagramas de classe.
- **Auto-Dimensionamento**: Os diagramas são renderizados em escala 1:1. Diagramas pequenos permanecem nítidos, enquanto os grandes ativam um container de rolagem.
- **Colapsável**: Você pode colapsar a seção do diagrama para focar nos detalhes da API.
