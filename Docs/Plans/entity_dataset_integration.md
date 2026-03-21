# Plano Técnico: TEntityDataSet & Integração IDE (Design-Time)

## Visão Geral

Implementar o **`TEntityDataSet`**, um `TDataSet` virtual de alta performance focado em listas de objetos (`TObjectList`, `TArray<T>`) que se conecta diretamente à infraestrutura de metadados do Dext ORM (`TEntityMap`). O componente oferecerá leitura extrema de memória, DML atômico e **integração nativa com o pipeline de streaming do Dext (`TUtf8JsonReader`)** para carregar dados de rede/banco sem alocação ou wrappers pesados (`TFDQuery`, `OleVariant`).

---

## 1. Runtime Core: `TEntityDataSet` (Dext.EF.dpk)

Um dataset customizado e virtualizado que lê listas de objetos da memória sem duplicação de dados.

- **Direct Memory Offset (High Speed):** Em vez de usar RTTI (`GetValue`) a cada leitura, o dataset consumirá o `FieldValueOffset` já mapeado e cacheado no `TEntityMap` para leitura direta de ponteiros (`PByte(Obj) + Offset`).
- **Safe RefCount (Strings/Interfaces):** Para tipos com contagem de referência, o offset usará atribuição nativa (casting) em vez de `Move` de bytes brutos, evitando corromper a memória.
- **Virtual Buffer (`TVector`):** Operações de filtragem e índices virtuais em buffers vetoriais contínuos.
- **CloneCursor e Notificações:** Clones compartilharão os dados, com buffers de edição locais e sincronização via **`DataEvent`** pós-Post.

---

## 2. Zero-Alloc Data Pipeline: `.Data` & Streams

Para trafegar dados entre camadas (API -> Client) com máxima eficiência.

- **⚡ Transiente (`.SaveToSpan` / `.LoadFromSpan`):**
  - Exporta/Importa dados binários de offsets contínuos (Memória).
  - Inclui um **Class Hash Header** curto para garantir que a classe na memória atual é 100% idêntica ao buffer. Ideal para clones e passagem de dados em runtime.
- **💾 Persistente / Rede (`.SaveToUtf8Json` / `.LoadFromUtf8Json`):**
  - **Motor:** Reutilização do **`TUtf8JsonSerializer`** e **`TUtf8JsonReader`** (em `Dext.Json.Utf8.Serializer.pas`).
  - **Vantagem:** O próprio FireDAC Phys do Dext cospe JSON Utf8 em bytes. O DataSet consumirá esse Reader em `TByteSpan` para carregar dados de rede em milissegundos.
  - **Retrocompatibilidade:** O JSON carrega o nome dos campos, permitindo que o DataSet seja tolerante se colunas novas forem adicionadas à classe do objeto.

---

## 3. DML e Estados de Edição (`Insert`, `Edit`, `Post`)

- **Uso de `RecordBuffer` nativo:** Snapshot completo da linha com rastreio de alterações.
- **Header de Alteração (Bitmask):** Bitwise flags para saber exatamente qual coluna foi modificada pela Grid ao dar `.Post`.
- **Propriedade `ReadOnly`:** Desativa alocações de buffer de escrita para dashboards ou relatórios.

---

## 4. Filtros Dinâmicos e `IExpression`

- **String Filter Parser:** Converte filtros `.Filter` em objetos **`IExpression`**.
- **`TInMemoryExpressionVisitor`:** Roda filtros na memória, com suporte futuro a delegates anônimos ou JIT compilation para aceleração dinâmica.

---

## 5. Gestão de Propriedades Dinâmicas e Calculados

- **Shadow Properties:** Suporte à propriedade **`IncludeShadowProperties: Boolean`** (Default `False`). Se `True`, o dataset lerá os dados mapeados buscando-os no Context do Dext da entidade (Fallback).
- **Calculated Fields Clássicos:** Suporte ao evento `OnCalcFields` do `TDataSet` nativo.

---

## 6. Ordenação e Navegação

- **Ordenação Dinâmica (`IndexFieldNames`):** Ordenação binária instantânea sobre o array de índices virtuais sem tocar na lista original.
- **Buscas em $O(1)$ (`Locate` / `Lookup`):** Acoplamento com indices Hash do Dext (`TFastMap`) de **Lazy Building** (Construção sob demanda no Locate).

---

## 7. Design-Time: Suporte IDE (Dext.EF.Design.dpk)

- **Custom Property Editor (`EntityName`):** Object Inspector apontando classes lidas dinamicamente.
- **Leitura via DelphiAST:** Varrer os arquivos `.pas` do projeto do usuário na IDE na velocidade da luz.
- **TFields Persistentes:** Sincronização estática direto no `.dfm` (Fields Editor).

---

## 8. Roteiro de Implementação (Checklist)

### 🚀 Fase 1: Runtime Core & Streams (O Essencial)

- [x] Criar `Dext.Entity.DataSet.pas` em `Sources/Data`
- [x] Implementar overrides fundamentais do `TDataSet` (`InternalOpen`, `InternalInitFieldDefs`)
- [x] Implementar **Bookmarks** nativos baseados em índice do Buffer (Ponteiro PEntityRecordHeader).
- [x] Implementar manipulação de dados (`GetRecord`, `GetFieldData`) utilizando offsets do `TEntityMap`.
- [x] Implementar ciclo de **DML** (`Append`, `Edit`, `Post`) via buffers nativos em memória.
- [x] Criar carga de dados via **`TUtf8JsonReader`**: `.LoadFromUtf8Json(const ASpan: TByteSpan)`.
- [x] Desenvolver suporte para carga de objetos: `.Load(const AItems: TArray<TObject>)`.
- [x] Criar Testes Unitários (`Sources/Tests/Dext.Entity.DataSet.Tests.pas`)
- [x] Criar Showcase de visualização em Grid (`Examples/Desktop.EntityDataSet.Demo`)

### 🧠 Fase 2: Filtros, Ordenações e Expressions

- [x] Criar o `Dext.StringExpressionParser` (Tradução de `.Filter` para `IExpression`).
- [x] Implementar `TInMemoryExpressionVisitor` para filtragem (.Filter).
- [x] Implementar ordenação instantânea por **`IndexFieldNames`**.
- [x] Acoplar **iteração em TVector** para buscas no `.Locate`.

### 🎨 Fase 3: Design-Time IDE Component

- [x] Criar arquivo de registro `Dext.Entity.DataSet.Reg.pas`
- [x] Criar pacote `Dext.EF.Design.dpk` Design Only para a IDE

### 📖 Documentation (Dext Book)

- [x] Criar documentação em Inglês (`Docs/Book/11-desktop-ui/entity-dataset.md`)
- [x] Criar documentação em Português (`Docs/Book.pt-br/11-desktop-ui/entity-dataset.md`)
- [x] Atualizar os menus `README.md` das seções de Desktop UI

### 🛠️ Fase 4: Tipos de Dados Avançados & Validação TDD

*Objetivo: Garantir a completude da engine de dados e cobertura de tipos complexos.*

- [x] **Nullable Support:** Testar e validar compatibilidade total com `Nullable<Prop<T>>` e `Nullable<T>`.
- [x] **SmartTypes (Prop/Lazy):** Implementar suporte nativo a `Prop<T>` e `Lazy<T>` com unwrapping automático.
- [x] **Blob Support:** Suporte e testes exaustivos para campos "Blob" (Texto longo - CLOB e Imagens - BLOB).
- [x] **Master-Detail:** Implementar e testar cenários de Mestre-Detalhe vinculados.
- [x] **Performance & RTTI Fix:** Otimização do pipeline de RTTI (Context global) e correção de memory leaks.
- [ ] **Shadow Properties:** Validar leitura/escrita de propriedades persistentes que não possuem campo físico na classe (`TEntityMap`).
- [ ] **Calculated Fields:** Suporte nativo a campos calculados via evento `OnCalcFields` do `TDataSet`.

### 🔄 Fase 5: Integração & Conversão de Dados

*Objetivo: Facilitar a interoperabilidade entre listas, JSON e o engine do DataSet.*

- [ ] **IList<T> Integration:** Melhorar integração nativa com `IList<T>` e coleções fluentes.
- [ ] **Fluent Load:** Refinar método `.Load` para suportar diferentes origens de dados de forma transparente.
- [ ] **Entity to Json:** Bridge para exportar dados do DataSet/Entidade para JSON.
- [ ] **Json to Entity:** Bridge para importar dados de JSON diretamente para entidades via DataSet.

### 🎨 Fase 6: Experiência Design-Time & IDE

*Objetivo: Produtividade máxima do desenvolvedor Delphi no ecossistema Dext.*

- [ ] **Delphi AST Discovery:** Usar o parser AST para autocompletar e descobrir classes de entidades no projeto dentro da IDE. (Atualizar Delphi AST antes de executar esta tarefa)
- [ ] **Auto Persistence:** Auto-criação de `TFields` (Persistent Fields) no Fields Editor em tempo de design.
- [ ] **Design-time Data Viewer:** Implementar janela para execução e visualização de dados (estilo DataApi) em Design-time.
- [ ] **Attribute Driven UI:**
  - Configurar `Caption` / `DisplayLabel` do campo usando atributos da classe.
  - Sincronizar `Constraints` (`Required`, `MaxLength`) via atributos de validação da entidade.

### 🚀 Fase 7: Performance & Quality Review

*Objetivo: Garantir o selo de qualidade Dext de performance e estabilida de.*

- [ ] **Performance Review:** Profiling completo para validação do pipeline Zero-Alloc.
- [ ] **DataSet Design Review:** Revisão final da API pública e eventos para consistência com o Framework.

---
*Plano consolidado e expandido em: 21/03/2026.*
