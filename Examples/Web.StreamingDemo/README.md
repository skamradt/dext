# 📦 Web.StreamingDemo - File Upload & Download

This example demonstrates how to handle file streams in Dext, covering both **Multipart Uploads** and **Streamed Downloads**.

---

## ✨ Features

- **Single File Upload**: Using `IFormFile`.
- **Multiple File Upload**: Processing collections of files in a single request.
- **File Streaming**: Serving files directly from disk to the client.
- **MIME Type Handling**: Automatic detection of content types.
- **Download Headers**: Using `Content-Disposition` to trigger browser downloads.

---

## 🚀 How to Run

1. Open `Web.StreamingDemo.dproj` in Delphi.
2. Build and Run (F9).
3. Access the demo in your browser: [http://localhost:8080/upload/form](http://localhost:8080/upload/form)

---

## 📡 API Endpoints

### Upload
- `GET  /upload/form`       - Simple HTML form for testing.
- `POST /upload`            - Upload a single file (Form field: `myfile`).
- `POST /upload/multiple`   - Upload multiple files.

### Download
- `GET  /download/list`     - Returns a JSON list of all uploaded files.
- `GET  /download/:name`    - Downloads the file as an attachment.
- `GET  /stream/:name`      - Streams the file inline (useful for images/videos).

---

## 🛠️ Key Implementation Details

### Handling Uploads in Endpoints
In Dext, you can access files directly from the `IHttpContext`:

```pascal
var File := Ctx.Request.Files.GetFile('myfile');
if Assigned(File) then
  File.CopyTo(MyStream);
```

### Serving Streams
To serve a file or any `TStream`, use the `Ctx.Response.Stream` method:

```pascal
Ctx.Response.ContentType := 'application/pdf';
Ctx.Response.Stream(MyFileStream);
```

---

## 🧪 Testing with PowerShell

A test script is included to automate the verification:
```powershell
.\Test.Web.StreamingDemo.ps1
```

---

[← Back to Examples](../README.md)
