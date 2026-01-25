# 11. Desktop UI (Dext.UI)

> Construa aplicações desktop VCL profissionais com padrões modernos e recursos de produtividade.

---

## Visão Geral

`Dext.UI` é um framework para construir aplicações desktop modernas em Delphi. Ele traz padrões inspirados na web como navegação, pipelines de middleware e binding declarativo para o mundo VCL.

## Recursos Principais

| Recurso | Descrição |
|---------|-----------|
| **Navigator** | Navegação inspirada no Flutter com suporte a middleware |
| **Magic Binding** | Binding bidirecional automático via atributos |
| **Padrões MVVM** | Arquitetura limpa com ViewModel e Controller |
| **Testabilidade** | Suporte completo a testes unitários com DI |

### Hospedando um Servidor Web (Sidecar)

Se você precisa hospedar um Servidor Web Dext dentro de sua aplicação VCL (padrão Sidecar), use sempre o método **`Start`** (não-bloqueante) ao invés de `Run` (bloqueante).

```pascal
// No FormCreate:
FHost := WebBuilder.Build;
FHost.Start; // Não congela a UI!
```

---

## Capítulos

- [Navigator Framework](navigator.md) - Navegação Push/Pop com middlewares
- [Magic Binding](magic-binding.md) - Binding declarativo de UI
- [Padrões MVVM](mvvm-patterns.md) - Guia de arquitetura

---

## Exemplo Rápido

```pascal
// App Startup - Configurar Navigator
procedure TAppStartup.ConfigureServices(Services: IDIContainer);
begin
  // Registrar Navigator
  Services.AddSingleton<ISimpleNavigator>(
    function: ISimpleNavigator
    begin
      Result := TSimpleNavigator.Create;
      Result
        .UseAdapter(TCustomContainerAdapter.Create(MainForm.ContentPanel))
        .UseMiddleware(TLoggingMiddleware.Create(Logger));
    end);
end;

// Uso - Navegar para views
procedure TMainForm.ShowCustomerList;
begin
  Navigator.Push(TCustomerListFrame);
end;

procedure TMainForm.ShowCustomerEdit(Customer: TCustomer);
begin
  Navigator.Push(TCustomerEditFrame, TValue.From(Customer));
end;
```

---

## Projeto de Exemplo

| Exemplo | Descrição |
|---------|-----------|
| [Desktop.MVVM.CustomerCRUD](../../../Examples/Desktop.MVVM.CustomerCRUD/) | CRUD completo com Navigator, DI e testes unitários |

---

*Veja o [Desktop UI Roadmap](../../Roadmap/desktop-ui-roadmap.md) para recursos futuros.*
