# Guia de Depuração Avançada

Depurar frameworks web modernos e de alta abstração como o Dext pode ser desafiador, pois muita "mágica" (Model Binding, Lazy Loading, Validação) acontece em segundo plano antes mesmo do seu código ser executado.

Use este guia para dominar a arte de encontrar bugs em pipelines complexos do Dext.

## 1. O Ciclo de Vida da Requisição (Onde a "Mágica" acontece)

Quando uma requisição chega a uma aplicação Dext, ela segue este caminho simplificado:
1. **Server Listener** (Indy / HTTP.sys)
2. **Middleware Pipeline** (Auth, CORS, Logging, etc.)
3. **Engines de Roteamento** (Encontra o endpoint correto)
4. **Invoker de Handler** (Injeta parâmetros via Model Binding)
5. **Validação** (Executa o TValidator nos dados vinculados)
6. **Seu Handler** (O código que você escreveu)

**DICA CRÍTICA**: Se você tem um breakpoint no seu handler (Passo 6) e ele **não está sendo atingido**, mas você recebe um erro HTTP 500 ou 400, o problema está ocorrendo nos Passos 1 ao 5.

---

## 2. Depurando Model Binding & Conversores de Tipo

Uma das fontes mais comuns de erro é quando o JSON enviado pelo cliente não "encaixa" nos seus tipos Delphi.

### Sintomas
- Erro 500 (Internal Server Error) sem atingir o breakpoint no handler.
- Erro 400 (Bad Request) com mensagens de "Binding Error".
- Propriedades no seu objeto ficam `0`, `nil` ou vazias.

### Como Depurar
Se você suspeita que o Model Binding está falhando:
1. **Habilite Exceções do Delphi**: No IDE, garanta que `Language Exceptions` estejam habilitadas (Tools -> Options -> Debugger -> Delphi Exceptions).
2. **Coloque Breakpoints Estratégicos**: Entre na lógica central de conversão do framework:
   - Arquivo: `Dext.Core.ValueConverters.pas`
   - Método: `TValueConverter.Convert`
3. **Inspecione o Fluxo**:
   - Verifique se o `AValue` (vindo do JSON) corresponde ao `ATargetType`.
   - Procure pelo bloco `try..except` ao final de `Convert`. Se ele falhar, ali está o seu culpado.

---

## 3. Depurando ORM & Lazy Loading

O Lazy Loading é poderoso, mas pode esconder incompatibilidades de tipo complexas.

### Sintomas
- Erro "Invalid class typecast" ao acessar uma propriedade.
- "Access Violation" ao ler um membro de uma entidade.

### Como Depurar
1. **Pare no Injetor**:
   - Arquivo: `Dext.Entity.LazyLoading.pas`
   - Método: `TLazyInjector.InjectField`
2. **Verifique os Tipos**:
   - Confira se o campo `Lazy<T>` na sua entidade coincide com a interface/classe sendo carregada pelo `TLazyLoader`.
   - Garanta que a propriedade está fisicamente presente na classe (o RTTI precisa dela).

---

## 4. Depurando o Pipeline (Middlewares)

Middlewares podem modificar requisições ou abortá-las precocemente.

### Identificando o Middleware
Se você suspeita que um middleware está interferindo:
1. **Logging HTTP**: Use `App.UseHttpLogging`. Ele mostrará exatamente até qual passo a requisição chegou.
2. **Trajetória Passo a Passo**:
   - Abra o arquivo `Dext.Web.Core.pas`.
   - Procure por `TMiddlewarePipeline.Process`.
   - Siga com `F7` cada delegado de middleware para ver qual deles encerra a requisição.

---

## 5. Dicas Pro para Depuração Rápida

### Use o Cache de Metadados de Tipos Smart
O Dext faz cache de metadados em `TReflection.GetMetadata`. Se um Smart Type (`Prop<T>`) estiver se comportando de forma estranha:
- Coloque um breakpoint em `Dext.Core.Reflection.pas`.
- Inspecione o objeto `TTypeMetadata`. Verifique se o `ValueField` foi identificado corretamente.

### Testes via Terminal
Sempre use scripts (como `.ps1`) ou ferramentas como Postman/Insomnia para reproduzir erros. Isso permite verificar se o erro está relacionado a **headers**, **formato do JSON** ou **separadores decimais**.

### Separadores Decimais
O Dext utiliza `TFormatSettings.Invariant` para o parsing de JSON. Se sua aplicação "morre" com valores numéricos, verifique se você está fazendo parsing manual de strings em outro lugar usando configurações regionais.

---

## 6. Leis de Memória Interna (RTTI & Managed Records)

Se você estiver desenvolvendo ou estendendo o framework, ou usando tipos complexos como `Prop<T>` (Smart Types), deve conhecer as leis de memória do Delphi aplicadas ao Dext:

### A Lei da Inicialização de Records
Records que contêm campos gerenciados (interfaces, strings, arrays dinâmicos) **devem** ser explicitamente inicializados se forem criados via manipulação direta de memória ou `TValue.Make`.
- **Sintoma**: `Invalid pointer operation` ou `Access Violation` aleatório durante o `TReflection.SetValue`.
- **Causa**: O RTTI do Delphi tenta liberar o "ponteiro lixo" que estava na memória antes de atribuir o novo valor da interface.
- **Solução**: O framework agora faz isso automaticamente, mas se você criar records manualmente, use `FillChar(Ptr^, Size, 0)` em records recém-criados para garantir que tudo comece como `nil`.

### A Lei do Ciclo de Vida do RTTI (Dangling Pointers)
Objetos de RTTI (`TRttiType`, `TRttiField`, `TRttiProperty`) são válidos apenas enquanto o seu `TRttiContext` existir.
- **Sintoma**: Access Violation na segunda ou terceira execução de um endpoint, mas funciona na primeira.
- **Causa**: O metadado do tipo foi cacheado, mas o `TRttiContext` que o gerou foi destruído (ex: era uma variável local de um construtor), deixando ponteiros apontando para memória liberada.
- **Solução**: Use sempre a infraestrutura de reflexão do Dext (`TReflection`), que mantém um contexto global estável durante toda a vida da aplicação.

---

## 7. Model Binding com Injeção de Dependências

O Dext agora suporta injeção de dependências diretamente no processo de binding de classes.

### Como funciona
Se você passar uma classe para um endpoint (POST/PUT), o Dext usará o `TActivator` para instanciá-la. Isso significa que se sua classe tiver dependências no construtor (como um Logger ou uma Configuração), elas serão resolvidas automaticamente.

- **Configuração**: O binding injeta automaticamente o `ServiceProvider` da requisição atual no serializador.
- **Vantagem**: Suas entidades e modelos de entrada podem ser "inteligentes" e ter acesso a serviços do sistema desde o nascimento.

---

[← Solução de Problemas](troubleshooting.md) | [Índice do Livro →](../README.md)
