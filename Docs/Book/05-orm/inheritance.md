# Inheritance (Herança)

O Dext ORM permite mapear hierarquias de classes para o banco de dados de forma transparente, suportando polimorfismo em consultas.

## Estratégias de Herança

Dext suporta as duas principais estratégias de mapeamento de herança:

### 1. Table Per Hierarchy (TPH)

Toda a hierarquia de classes é mapeada para uma única tabela. Uma coluna **discriminadora** é usada para identificar o tipo de cada linha.

Esta é a estratégia **padrão** no Dext quando uma hierarquia é detectada.

```pascal
type
  [Table('people')]
  [Inheritance(TablePerHierarchy)]
  [DiscriminatorColumn('person_type')]
  TPerson = class
  public
    [PK, AutoInc]
    Id: Integer;
    Name: string;
  end;

  [DiscriminatorValue('student')]
  TStudent = class(TPerson)
  public
    EnrollmentNumber: string;
  end;

  [DiscriminatorValue('teacher')]
  TTeacher = class(TPerson)
  public
    Subject: string;
  end;
```

**Vantagem**: Consultas polimórficas (ex: `Context.People.ToList`) são extremamente rápidas, pois não exigem JOINS.

### 2. Table Per Type (TPT)

Cada classe na hierarquia tem sua própria tabela. A tabela da classe filha contém apenas as propriedades específicas dela e compartilha a Chave Primária com a tabela base.

```pascal
type
  [Table('vehicles')]
  [Inheritance(TablePerType)]
  TVehicle = class
  public
    [PK]
    Id: Integer;
    Brand: string;
  end;

  [Table('cars')]
  TCar = class(TVehicle)
  public
    NumberOfDoors: Integer;
  end;
```

**Vantagem**: O esquema do banco de dados é normalizado e segue fielmente o modelo de classes.

---

## Consultas Polimórficas

Uma das maiores vantagens da herança no ORM é a capacidade de consultar a classe base e obter instâncias das classes filhas corretas:

```pascal
var List: IList<TPerson> := Context.People.ToList;

for var Person in List do
begin
  if Person is TStudent then
    WriteLn(Person.Name + ' is a Student (#' + TStudent(Person).EnrollmentNumber + ')')
  else if Person is TTeacher then
    WriteLn(Person.Name + ' is a Teacher (' + TTeacher(Person).Subject + ')');
end;
```

O Dext gerencia automaticamente a instanciação da classe correta baseada no discriminador (em TPH) ou na presença de registros nas tabelas vinculadas (em TPT).

## Configuração via Fluent API

Você também pode configurar a herança sem o uso de atributos:

```pascal
Builder.Entity<TPerson>
  .MapInheritance(TablePerHierarchy)
  .HasDiscriminator('person_type', 'base')
  .Prop('Name').IsRequired;

Builder.Entity<TStudent>()
  .HasDiscriminator('person_type', 'student');
```
