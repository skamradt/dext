# Documentation Generator (`dext doc`)

The Dext CLI includes a powerful static site generator (SSG) specifically designed for Delphi projects. It automatically parses your source code (Pascal) and generates a modern, interactive HTML documentation website.

## Features

*   **Zero Configuration**: Works out of the box by scanning your project directory.
*   **Mermaid Diagrams**: Automatically generates interactive class diagrams (UML) for each unit, enabling you to visualize inheritance and relationships.
    *   **Interactive**: Click to collapse/expand diagrams.
    *   **Scalable**: Large diagrams automatically enable horizontal scrolling without losing resolution.
*   **Search**: Client-side full-text search for classes and units.
*   **Dark/Light Mode**: Built-in theme switcher that persists your preference.
*   **Modern UI**: Clean, responsive layout based on `theme.css`.

## Usage

To generate documentation for your project, simply run:

```bash
dext doc
```

This will:
1.  Scan the current directory (recursively) for `.pas` files.
2.  Parse the Abstract Syntax Tree (AST) of your code.
3.  Generate a static website in `Docs/Output`.

### Options

*   `--input <path>`: Specify the root source directory (defaults to current dir).
*   `--output <path>`: Specify the output directory (defaults to `Docs/Output`).

```bash
dext doc --input ./Sources --output ./MyDocs
```

## Generated Output

The command generates a standalone static site. You can deploy this folder to GitHub Pages, Netlify, or any static hosting service.

*   `index.html`: The main entry point.
*   `theme.css` / `layout.css`: Customizable styles.
*   `viewer.js`: Client-side logic for search, theming, and diagrams.
*   `API/*.html`: Individual pages for each parsed unit.

## Diagram Features

The generator uses **Mermaid.js** to render class diagrams.
- **Auto-Sizing**: Diagrams render at 1:1 scale. Small diagrams remain sharp, while large diagrams trigger a scroll container.
- **Collapsible**: You can collapse the diagram section to focus on the API details.
