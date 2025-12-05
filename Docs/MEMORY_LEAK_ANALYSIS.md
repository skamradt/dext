# Análise de Memory Leaks - Dext ORM

## Status Atual (2025-12-05)

### Leaks Resolvidos ✅
1. **TModelBuilder** - Corrigido adicionando `FModelBuilder.Free` no destrutor de `TDbContext`
2. **TObjectDictionary<PTypeInfo, TEntityMap>** - Corrigido com `[doOwnsValues]` no `TModelBuilder`
3. **ChangeTracker dangling pointers** - Corrigido com:
   - Custom `TEqualityComparer<TObject>` baseado em ponteiros
   - `Remove()` method para remover entidades antes de deletar
   - `Clear()` no destrutor de `TDbContext`
4. **Lazy Loading / TLazyLoader** - Corrigido:
   - Substituição de `TVirtualInterface` por `Lazy<T>` e `TLazyLoader`
   - Gerenciamento correto de ciclo de vida de `IList<T>` dentro do Loader
   - Tratamento de `OwnsObjects` para listas criadas internamente
5. **Projections (Select)** - Corrigido:
   - Implementado "Implicit No-Tracking" para projeções
   - Listas de projeção agora usam `OwnsObjects=True` para liberar entidades parciais
   - `TDbSet.Hydrate` suporta instanciação sem tracking

### Leaks Pendentes 🔍

#### 1. RTTI System Leaks (Baixa Prioridade)
**Tipo:** `System.Rtti.TFinalizer`, `TRttiInstancePropertyEx`, `TRttiInstanceMethodEx`
**Causa:** Leaks internos do sistema RTTI do Delphi ao fazer reflexão de atributos
**Impacto:** Pequeno (28-36 bytes cada)
**Ação:** Não há muito o que fazer - são limitações do RTTI do Delphi

**Exemplos:**
```
- 28 bytes: System.Rtti.TFinalizer (allocation #49580)
- 32 bytes: System.Rtti.LazyLoadAttributes.MakeClosure$ActRec
- 36 bytes: System.Rtti.TRttiInstanceMethodEx
```

#### 2. FluentQuery Closure Leaks (Média Prioridade)
**Tipo:** `Dext.Entity.Query.@TFluentQuery`1.Skip$ActRec`
**Causa:** Closures (funções anônimas) capturando variáveis
**Status:** Parcialmente mitigado com a migração para `IList<T>` e iteradores, mas requer monitoramento contínuo.

#### 3. Unknown Leaks (Investigação Necessária)
**Tipo:** Unknown (Eventuais leaks de 200 bytes)
**Causa:** Arrays dinâmicos ou buffers internos
**Impacto:** Baixo após correções principais

3. **Prioridade Baixa:**
   - Documentar limitações conhecidas do RTTI
   - Considerar alternativas ao uso intensivo de RTTI

## Ferramentas Utilizadas

- **FastMM5** com FullDebugMode
- Stack traces detalhados com símbolos de debug
- Testes isolados para identificar fontes de leaks

## Notas

- Muitos dos leaks são inerentes ao uso de RTTI no Delphi
- O impacto total dos leaks é relativamente pequeno (< 10KB por execução completa dos testes)
- A estratégia de testar incrementalmente (um teste por vez) está funcionando bem
