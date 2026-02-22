# Auto-Proxies e Lazy Loading Transparente

O Dext ORM introduz o **Auto-Proxy Lazy Loading**, permitindo que propriedades de navegação sejam carregadas sob demanda sem o uso do tipo wrapper `Lazy<T>`. Isso permite que suas entidades permaneçam como classes POCO (Plain Old CLR Objects) puras, facilitando a integração com frameworks de UI e serializadores.

## Como Funciona

Ao marcar uma propriedade como `.IsLazy` no mapeamento fluente, o Dext não instancia a classe original diretamente. Em vez disso, ele cria um **Proxy Dinâmico** (uma subclasse gerada em tempo de execução) que intercepta as chamadas aos métodos `getter`.

### Requisito Fundamental: Métodos Virtuais

Para que a intercepção funcione, o Delphi exige que o método que acessa o campo seja **virtual**. Propriedades simples com acesso direto a campos (`read FField`) **não podem** ser interceptadas.

#### Forma Correta (POCO compatível com Proxy):

```pascal
type
  TOrder = class
  private
    FCustomer: TCustomer;
    // ... outros campos
  protected
    function GetCustomer: TCustomer; virtual; // DEVE ser virtual
    procedure SetCustomer(const Value: TCustomer); virtual;
  public
    property Customer: TCustomer read GetCustomer write SetCustomer;
  end;
```

## Configuração via Fluent Mapping

Você ativa o Auto-Proxy no seu mapeamento:

```pascal
procedure TOrderConfig.Configure(Builder: IEntityTypeBuilder<TOrder>);
var
  o: TOrder;
begin
  o := Prototype.Entity<TOrder>;
  
  // Ativa o Lazy Loading via Proxy para a propriedade Customer
  Builder.Property(o.Customer).IsLazy;
end;
```

## Exemplo de Uso

O uso é totalmente transparente para o desenvolvedor:

```pascal
var
  Order: TOrder;
begin
  // O Dext retorna um Proxy de TOrder aqui
  Order := Context.Orders.Find(1);
  
  // No momento que você acessa .Customer, o Interceptor dispara
  // e carrega o Customer do banco de dados automaticamente.
  WriteLn('Cliente: ' + Order.Customer.Name); 
end;
```

## Vantagens
1. **Domínio Limpo**: Suas classes não dependem de `Dext.Types.Lazy`.
2. **Serialização**: Serializadores JSON padrão (como o do Dext ou o do Delphi) conseguem ler a propriedade normalmente.
3. **Compatibilidade**: Funciona perfeitamente com Data Binding e frameworks de UI.

## Limitações e Observações
- **Getters Virtuais**: Como mencionado, propriedades sem getters virtuais serão ignoradas pelo interceptor de proxy.
- **Performance**: Há um custo mínimo de memória e CPU para a criação do Proxy VTable, mas é desprezível para a maioria das aplicações de negócio.
- **Classes Seladas**: Classes marcadas como `final` (se o Delphi as suportasse totalmente) ou com métodos não virtuais não podem ser interceptadas.
