# SQL Puro com FromSql

Embora o Dext ORM forneça uma API fluente poderosa para construir consultas, às vezes você precisa executar SQL puro para otimização de performance, recursos complexos específicos do banco de dados ou suporte a sistemas legados. O método `FromSql` permite que você faça isso enquanto ainda se beneficia do mapeamento automático de objetos (hidratação).

## Uso Básico

Você pode chamar `FromSql` em qualquer `IDbSet<T>`. Os resultados serão mapeados automaticamente para instâncias da classe da entidade.

```pascal
var Users := Db.Users.FromSql('SELECT * FROM Users WHERE Active = 1').ToList;
```

## Consultas Parametrizadas

Para evitar injeção de SQL, sempre use consultas parametrizadas. O Dext suporta a sintaxe padrão de parâmetros.

```pascal
var MinAge := 18;
var Adults := Db.Users
  .FromSql('SELECT * FROM Users WHERE Age >= :Age', [MinAge])
  .ToList;
```

## Misturando com API Fluente

Um dos recursos mais poderosos do `FromSql` é que você pode continuar encadeando métodos fluentes após ele. O SQL puro atua como a fonte de dados para operações subsequentes como filtragem, ordenação ou paginação.

```pascal
var List := Db.Users
  .FromSql('SELECT * FROM Users WHERE Role = :Role', ['Admin'])
  .Where(Prop('Active') = True)
  .OrderBy(Prop('Name'))
  .Skip(10)
  .Take(5)
  .ToList;
```

> **Nota**: Quando você encadeia após o `FromSql`, o Dext envolve seu SQL puro em uma subquery para garantir que os filtros e a paginação funcionem corretamente, independentemente da complexidade do seu SQL manual.

## TSqlQueryIterator

Para conjuntos de resultados muito grandes, você pode usar `TSqlQueryIterator<T>` para processar registros um por um sem carregar a lista inteira na memória.

```pascal
var Query := Db.Users.FromSql('SELECT * FROM Users');
var Iterator := TSqlQueryIterator<TUser>.Create(Query);
try
  while Iterator.Next do
  begin
    ProcessUser(Iterator.Current);
  end;
finally
  Iterator.Free;
end;
```

## Limitações

*   **Projeção**: O `FromSql` espera que o resultado SQL contenha pelo menos as colunas necessárias para hidratar a entidade `T`. Se colunas estiverem faltando, as propriedades permanecerão com seus valores padrão.
*   **Rastreamento (Tracking)**: Por padrão, as entidades retornadas pelo `FromSql` são rastreadas pelo `DbContext`. Você pode usar `.AsNoTracking` se precisar dos dados apenas para fins de leitura.
