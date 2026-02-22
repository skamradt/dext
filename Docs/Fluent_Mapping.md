# Fluent Mapping no Dext ORM

O Dext ORM suporta mapeamento fluente (Fluent Mapping), permitindo que você configure suas entidades sem depender exclusivamente de atributos. Isso é útil para manter suas classes de domínio limpas ou para configurar entidades de terceiros.

## Configuração no DbContext

Para usar o mapeamento fluente, sobrescreva o método `OnModelCreating` no seu `TDbContext`:

```pascal
type
  TMyContext = class(TDbContext)
  protected
    procedure OnModelCreating(Builder: TModelBuilder); override;
  end;

procedure TMyContext.OnModelCreating(Builder: TModelBuilder);
begin
  Builder.Entity<TUser>(
    procedure(Builder: IEntityTypeBuilder<TUser>)
    var
      u: TUser;
    begin
      u := Prototype.Entity<TUser>; // Helper para seletores tipados

      Builder.ToTable('users');
      
      // Seletores Tipados (Overload de Prop)
      Builder.Prop(u.Id).IsPK;
      Builder.Prop(u.Name).HasColumnName('full_name').HasMaxLength(100);
      Builder.Prop(u.Email).IsRequired;
      
      // Auditoria Automática
      Builder.Prop(u.CreatedAt).IsCreatedAt;
      Builder.Prop(u.UpdatedAt).IsUpdatedAt;
      
      // Concorrência Otimista (Row Version)
      Builder.Prop(u.Version).IsVersion;
      
      // Colunas JSON
      Builder.Prop(u.Settings).IsJson;
    end
  );
end;
```

## Seletores Tipados vs Strings

Para evitar erros de digitação, o Dext permite usar seletores tipados baseados em `Prop<T>`.

*   **Preferred Way (Typed):** `Builder.Prop(u.Name).HasColumnName('name');`
*   **Alternative (String):** `Builder.Prop('Name').HasColumnName('name');`

Para usar seletores tipados, suas entidades devem declarar campos como `Prop<T>` em vez de tipos primitivos (embora as propriedades continuem sendo tipos primitivos para o resto do app).

```pascal
TUser = class
private
  FName: Prop<string>;
public
  property Name: string read FName write FName;
end;
```

## Shadow Properties

Shadow Properties são propriedades que existem no banco de dados e são gerenciadas pelo ORM, mas não possuem um campo correspondente na sua classe Delphi.

```pascal
Builder.Entity<TUser>(
  procedure(Builder: IEntityTypeBuilder<TUser>)
  begin
    // Define uma Shadow Property
    Builder.ShadowProperty('LastSyncDate')
      .HasColumnName('last_sync')
      .IsRequired(False);
  end
);
```

Para acessar ou modificar Shadow Properties:

```pascal
var Entry := Context.Entry(User);
Entry.Member('LastSyncDate').CurrentValue := Now;
```

## Resumo de Funcionalidades do Builder

| Método | Descrição |
| :--- | :--- |
| `.IsPK` | Marca a propriedade como chave primária. |
| `.IsAutoInc` | Marca como autoincremento. |
| `.IsRequired` | Define como NOT NULL. |
| `.HasMaxLength(N)` | Define o tamanho máximo (ex: VARCHAR(100)). |
| `.HasColumnName(N)` | Define o nome exato da coluna no banco. |
| `.IsJson` | Mapeia o objeto/lista para uma coluna JSON (TEXT/JSONB). |
| `.IsLazy` | Habilita Lazy Loading automático via Proxy. |
| `.IsCreatedAt` | Popula automaticamente com a data de criação. |
| `.IsUpdatedAt` | Atualiza automaticamente a cada modificação. |
| `.IsVersion` | Habilita controle de concorrência (incremento numérico). |
| `.IsShadow` | Define como Shadow Property. |
