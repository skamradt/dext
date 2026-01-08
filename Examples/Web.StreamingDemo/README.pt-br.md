# 📦 Web.StreamingDemo - Upload e Download de Arquivos

Este exemplo demonstra como manipular streams de arquivos no Dext, cobrindo tanto **Uploads Multipart** quanto **Downloads via Stream**.

---

## ✨ Recursos

- **Upload de Arquivo Único**: Usando `IFormFile`.
- **Upload Múltiplo**: Processando coleções de arquivos em uma única requisição.
- **File Streaming**: Servindo arquivos diretamente do disco para o cliente.
- **MIME Type Handling**: Detecção automática de tipos de conteúdo.
- **Download Headers**: Usando `Content-Disposition` para disparar o download no navegador.

---

## 🚀 Como Executar

1. Abra `Web.StreamingDemo.dproj` no Delphi.
2. Compile e Execute (F9).
3. Acesse o demo no navegador: [http://localhost:8080/upload/form](http://localhost:8080/upload/form)

---

## 📡 Endpoints da API

### Upload
- `GET  /upload/form`       - Formulário HTML simples para teste.
- `POST /upload`            - Upload de um único arquivo (Campo: `myfile`).
- `POST /upload/multiple`   - Upload de múltiplos arquivos.

### Download
- `GET  /download/list`     - Retorna uma lista JSON de todos os arquivos.
- `GET  /download/:name`    - Baixa o arquivo como anexo.
- `GET  /stream/:name`      - Serve o arquivo inline (útil para imagens/vídeos).

---

## 🧪 Testando com PowerShell

Um script de teste está incluído para automação:
```powershell
.\Test.Web.StreamingDemo.ps1
```

---

[← Voltar para Exemplos](../README.md)
