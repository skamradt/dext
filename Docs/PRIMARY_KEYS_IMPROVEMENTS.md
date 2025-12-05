# 🔑 Primary Keys & Composite Keys - Análise Técnica

## 📋 Problemas Identificados

### 1. **Coluna PK Hardcoded como "Id"**

#### Localização do Problema
```pascal
// Dext.Entity.DbSet.pas - linha ~865
function TDbSet<T>.Find(const AId: Variant): T;
begin
  // TODO : Get real mapping id column name
  var Expr: IExpression := TPropExpression.Create('Id') = TValue.FromVariant(AId);
  ...
end;
```

#### Impacto
- ❌ Falha ao buscar entidades com PK diferente de "Id"
- ❌ Não respeita mapeamento via `[PK]` attribute
- ❌ Não respeita mapeamento via Fluent Mapping
- ❌ Quebra em sistemas legados com convenções diferentes

#### Exemplo de Falha
```pascal
[Table('order_items')]
TOrderItem = class
  [PK, Column('order_id')]
  property OrderId: Integer read FOrderId write FOrderId;
  
  [PK, Column('product_id')]
  property ProductId: Integer read FProductId write FProductId;
end;

// ❌ FALHA: Tenta buscar por coluna "Id" que não existe
var Item := Context.Entities<TOrderItem>.Find(100);
```

### 2. **Tipos de PK Limitados a Integer**

#### Localização do Problema
```pascal
// Dext.Entity.DbSet.pas - linha ~878
function TDbSet<T>.Find(const AId: array of Integer): T;
```

#### Impacto
- ❌ Não suporta PKs compostas com tipos mistos
- ❌ Não suporta PKs do tipo String, GUID, DateTime, etc.
- ❌ Limita uso em sistemas legados

#### Casos Reais Não Suportados

**Caso 1: Integer + DateTime**
```pascal
[Table('daily_reports')]
TDailyReport = class
  [PK]
  property CompanyId: Integer;
  
  [PK]
  property ReportDate: TDateTime;
end;

// ❌ NÃO FUNCIONA: Tipos mistos
var Report := Context.Entities<TDailyReport>.Find([100, EncodeDate(2025, 12, 5)]);
```

**Caso 2: String + String**
```pascal
[Table('regions')]
TRegion = class
  [PK, Column('country_code')]
  property CountryCode: string;  // 'BR'
  
  [PK, Column('state_code')]
  property StateCode: string;    // 'SP'
end;

// ❌ NÃO FUNCIONA: Tipo String
var Region := Context.Entities<TRegion>.Find(['BR', 'SP']);
```

**Caso 3: GUID + Integer (Multi-Tenancy)**
```pascal
[Table('tenant_records')]
TTenantRecord = class
  [PK]
  property TenantId: TGUID;
  
  [PK]
  property RecordId: Integer;
end;

// ❌ NÃO FUNCIONA: GUID não suportado
var Record := Context.Entities<TTenantRecord>.Find([TenantGuid, 1234]);
```

## 🎯 Soluções Propostas

### Solução 1: Dynamic PK Column Mapping

#### Implementação
```pascal
function TDbSet<T>.Find(const AId: Variant): T;
var
  L: IList<T>;
  PKProp: string;
begin
  // Check if AId is a VarArray (composite key)
  if VarIsArray(AId) then
  begin
    // ... código existente para VarArray
  end;

  // Single key lookup - USE REAL PK COLUMN
  if FPKColumns.Count = 0 then
    raise Exception.Create('No Primary Key defined for entity ' + FTableName);
    
  if FPKColumns.Count > 1 then
    raise Exception.Create('Entity has composite key. Use Find(array) overload.');
  
  // Get the actual PK property name
  PKProp := '';
  for var Pair in FColumns do
  begin
    if SameText(Pair.Value, FPKColumns[0]) then
    begin
      PKProp := Pair.Key;
      Break;
    end;
  end;
  
  if PKProp = '' then
    PKProp := FPKColumns[0]; // Fallback to column name
  
  // Use the real PK property
  var Expr: IExpression := TPropExpression.Create(PKProp) = TValue.FromVariant(AId);
  var Spec := TSpecification<T>.Create(Expr);
  L := List(Spec);
  
  if L.Count > 0 then
    Result := L[0]
  else
    Result := nil;
end;
```

#### Benefícios
- ✅ Respeita mapeamento real da entidade
- ✅ Funciona com qualquer nome de coluna PK
- ✅ Compatível com Attributes e Fluent Mapping
- ✅ Validação de PK composta

### Solução 2: Mixed Type Composite Keys

#### Implementação - Fase 1: array of Variant
```pascal
function TDbSet<T>.Find(const AId: array of Variant): T;
var
  L: IList<T>;
  Expr: IExpression;
  i: Integer;
  PropName: string;
  Prop: TRttiProperty;
  ConvertedValue: TValue;
begin
  // Validate
  if Length(AId) <> FPKColumns.Count then
    raise Exception.CreateFmt('Expected %d key values but got %d', 
      [FPKColumns.Count, Length(AId)]);

  // Build composite key expression with type conversion
  Expr := nil;
  for i := 0 to FPKColumns.Count - 1 do
  begin
    // Find the property name for this PK column
    PropName := '';
    for var Pair in FColumns do
    begin
      if SameText(Pair.Value, FPKColumns[i]) then
      begin
        PropName := Pair.Key;
        Break;
      end;
    end;

    if PropName = '' then
      PropName := FPKColumns[i];

    // Get property type via RTTI
    if FProps.TryGetValue(FPKColumns[i].ToLower, Prop) then
    begin
      // Convert value to property type
      ConvertedValue := TValue.FromVariant(AId[i]);
      
      // Type-safe conversion based on property type
      case Prop.PropertyType.TypeKind of
        tkInteger, tkInt64:
          ConvertedValue := TValue.From<Integer>(AId[i]);
        tkString, tkUString, tkWString, tkLString:
          ConvertedValue := TValue.From<string>(VarToStr(AId[i]));
        tkFloat:
          if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
            ConvertedValue := TValue.From<TDateTime>(VarToDateTime(AId[i]))
          else
            ConvertedValue := TValue.From<Double>(AId[i]);
        // Add more types as needed
      end;
    end
    else
      ConvertedValue := TValue.FromVariant(AId[i]);

    // Create the equality expression
    var KeyExpr: IExpression := TBinaryExpression.Create(PropName, boEqual, ConvertedValue);

    // Combine with AND
    if Expr = nil then
      Expr := KeyExpr
    else
      Expr := TLogicalExpression.Create(Expr, KeyExpr, loAnd);
  end;

  // Execute the query
  var Spec := TSpecification<T>.Create(Expr);
  L := List(Spec);
  
  if L.Count > 0 then
    Result := L[0]
  else
    Result := nil;
end;
```

#### Uso
```pascal
// Integer + DateTime
var Report := Context.Entities<TDailyReport>.Find([100, EncodeDate(2025, 12, 5)]);

// String + String
var Region := Context.Entities<TRegion>.Find(['BR', 'SP']);

// GUID + Integer
var Record := Context.Entities<TTenantRecord>.Find([TenantGuid, 1234]);

// Backward compatible - Integer + Integer
var Item := Context.Entities<TOrderItem>.Find([100, 50]);
```

#### Benefícios
- ✅ Suporta qualquer combinação de tipos
- ✅ Type-safe via RTTI
- ✅ Backward compatible com código existente
- ✅ Suporte a sistemas legados

## 📊 Priorização

### Alta Prioridade 🔥
1. **Dynamic PK Column Mapping**
   - Impacto: Alto (quebra funcionalidade básica)
   - Esforço: Baixo (refactoring simples)
   - Risco: Baixo (não quebra API existente)

### Média Prioridade ⚡
2. **Mixed Type Composite Keys**
   - Impacto: Médio (habilita novos cenários)
   - Esforço: Médio (requer conversão de tipos)
   - Risco: Médio (precisa de testes extensivos)

## 🧪 Casos de Teste Necessários

### Dynamic PK Mapping
```pascal
procedure TestFindWithCustomPKName;
begin
  // Entity with PK named "OrderId" instead of "Id"
  var Order := Context.Entities<TOrder>.Find(100);
  Assert(Order <> nil);
  Assert(Order.OrderId = 100);
end;
```

### Mixed Type Composite Keys
```pascal
procedure TestFindWithStringStringPK;
begin
  var Region := Context.Entities<TRegion>.Find(['BR', 'SP']);
  Assert(Region <> nil);
  Assert(Region.CountryCode = 'BR');
  Assert(Region.StateCode = 'SP');
end;

procedure TestFindWithIntegerDateTimePK;
begin
  var Report := Context.Entities<TDailyReport>.Find([100, EncodeDate(2025, 12, 5)]);
  Assert(Report <> nil);
  Assert(Report.CompanyId = 100);
  Assert(DateOf(Report.ReportDate) = EncodeDate(2025, 12, 5));
end;
```

## 📝 Notas de Implementação

### Considerações de Performance
- Cache de mapeamento PK → Property já existe em `FPKColumns` e `FColumns`
- RTTI lookup é feito apenas uma vez por tipo (já cached)
- Conversão de tipos tem overhead mínimo

### Compatibilidade
- Todas as mudanças são backward compatible
- Código existente continua funcionando
- Novos overloads adicionam funcionalidade sem quebrar API

### Testes
- Adicionar testes para cada combinação de tipos
- Validar edge cases (null, tipos incompatíveis)
- Testar com diferentes bancos de dados
