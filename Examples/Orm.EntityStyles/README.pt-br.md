# 🎨 Orm.EntityStyles - Duas Abordagens de Definição de Entidades

Uma comparação lado a lado de **duas formas de definir entidades** no Dext ORM, demonstrando que ambas as abordagens podem coexistir no mesmo projeto.

---

## ✨ O Que Esta Demo Mostra

### Estilo 1: Entidades Clássicas (Tipos Nativos)
```pascal
[Table('ClassicPeople')]
TClassicPerson = class
  property Id: Integer read FId write FId;
  property Name: string read FName write FName;
  property Age: Integer read FAge write FAge;
end;
```

**Melhor para:**
- Migração de código Delphi existente
- Times familiarizados com tipos tradicionais do Delphi
- Projetos usando TypeSystem para queries tipadas

### Estilo 2: Entidades Smart (Smart Properties)
```pascal
[Table('SmartPeople')]
TSmartPerson = class
  property Id: IntType read FId write FId;
  property Name: StringType read FName write FName;
  property Age: IntType read FAge write FAge;
end;
```

**Melhor para:**
- Novos projetos começando do zero
- Desenvolvedores que querem queries tipadas sem classes de metadados
- Menos código boilerplate

---

## 🚀 Começando

### Pré-requisitos
- Delphi 11+ (Alexandria ou posterior)
- Dext Framework no Library Path

### Executando a Demo

1. Abra `Orm.EntityStyles.dproj` no Delphi
2. Compile o projeto (F9)
3. Execute o binário

Não precisa configurar banco de dados - usa SQLite em memória!

---

## 📖 Principais Diferenças

| Funcionalidade | Clássico | Smart Properties |
|----------------|----------|------------------|
| Tipos de Propriedade | `Integer`, `string` | `IntType`, `StringType` |
| Queries Tipadas | Requer `TEntityType<T>` | Built-in com `Prototype.Entity<T>` |
| Curva de Aprendizado | Familiar | Conceito novo |
| Boilerplate | Mais (classe de metadados) | Menos |
| Migração | Fácil de código existente | Projetos novos |

---

## 💡 Exemplos de Query

### Estilo Clássico (com TypeSystem)
```pascal
// Requer classe de metadados separada
type
  TPersonType = class(TEntityType<TPerson>)
    class var Age: TProp<Integer>;
  end;

// Uso
var Adults := Context.Entities<TPerson>.QueryAll
  .Where(TPersonType.Age >= 18)
  .ToList;
```

### Estilo Smart (com Prototype)
```pascal
// Sem classe separada necessária!
var p := Prototype.Entity<TSmartPerson>;

var Adults := Context.Entities<TSmartPerson>
  .Where(p.Age >= 18)
  .ToList;

// Queries encadeadas
var Result := Context.Entities<TSmartPerson>
  .Where(p.Age > 20)
  .Where(p.Age < 40)
  .ToList;
```

---

## 🔧 Quando Usar Cada Um

### Escolha **Clássico** quando:
- ✅ Migrando codebase existente
- ✅ Time está confortável com padrões TypeSystem
- ✅ Precisa máxima compatibilidade com ferramentas existentes
- ✅ Prefere definições de metadados explícitas

### Escolha **Smart Properties** quando:
- ✅ Iniciando projeto novo
- ✅ Quer mínimo boilerplate
- ✅ Prefere queries tipadas inline
- ✅ Vindo de outros ORMs (Entity Framework, etc.)

### Misture Ambos!
Ambos os estilos podem coexistir no mesmo projeto. Use Clássico para entidades legado e Smart para desenvolvimento novo.

---

## 📁 Estrutura do Projeto

```
Orm.EntityStyles/
├── Orm.EntityStyles.dpr       # Programa principal
├── EntityStyles.Demo.pas      # Demo com ambos os estilos
└── README.md                  # Este arquivo
```

---

## 📚 Exemplos Relacionados

- **[Orm.EntityDemo](../Orm.EntityDemo)** - Suíte completa de testes do ORM
- **[Orm.SmartProperties](../Orm.SmartProperties)** - Showcase completo de Smart Properties com Web API

---

## 📄 Licença

Este exemplo faz parte do Dext Framework e está licenciado sob a Apache License 2.0.

---

*Escolha seu estilo e comece a programar! 🚀*
