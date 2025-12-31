unit SwaggerControllerExample.Models;

{
  Data Transfer Objects (DTOs) for the Swagger Controller Example.
  Demonstrates how to use Swagger schema attributes to document
  request and response types in OpenAPI specification.
}

interface

uses
  Dext.OpenAPI.Attributes;

type
  /// <summary>
  ///   Represents a book in the library catalog.
  /// </summary>
  [SwaggerSchema('Book', 'Represents a book in the library catalog')]
  TBook = record
    [SwaggerProperty('Unique identifier for the book')]
    [SwaggerExample('1')]
    Id: Integer;

    [SwaggerProperty('Book title')]
    [SwaggerExample('Clean Code')]
    Title: string;

    [SwaggerProperty('Author name')]
    [SwaggerExample('Robert C. Martin')]
    Author: string;

    [SwaggerProperty('Year of publication')]
    [SwaggerExample('2008')]
    Year: Integer;

    [SwaggerProperty('Book ISBN')]
    [SwaggerExample('978-0132350884')]
    ISBN: string;

    [SwaggerProperty('Availability status')]
    Available: Boolean;
  end;

  TBookArray = TArray<TBook>;

  /// <summary>
  ///   Request body for creating a new book.
  /// </summary>
  [SwaggerSchema('CreateBookRequest', 'Request body for creating a new book')]
  TCreateBookRequest = record
    [SwaggerProperty('Book title')]
    [SwaggerRequired]
    Title: string;

    [SwaggerProperty('Author name')]
    [SwaggerRequired]
    Author: string;

    [SwaggerProperty('Year of publication')]
    Year: Integer;

    [SwaggerProperty('Book ISBN')]
    [SwaggerFormat('isbn')]
    ISBN: string;
  end;

  /// <summary>
  ///   Request body for updating book availability.
  /// </summary>
  [SwaggerSchema('UpdateBookAvailabilityRequest', 'Request to update book availability')]
  TUpdateAvailabilityRequest = record
    [SwaggerProperty('Availability status')]
    [SwaggerRequired]
    Available: Boolean;
  end;

  /// <summary>
  ///   Standard error response.
  /// </summary>
  [SwaggerSchema('ErrorResponse', 'Standard error response')]
  TErrorResponse = record
    [SwaggerProperty('Error message')]
    [SwaggerExample('Book not found')]
    error: string;
  end;

  /// <summary>
  ///   Health check response.
  /// </summary>
  [SwaggerSchema('HealthResponse', 'API health status')]
  THealthResponse = record
    [SwaggerProperty('Service status')]
    [SwaggerExample('healthy')]
    status: string;

    [SwaggerProperty('API version')]
    [SwaggerExample('1.0.0')]
    version: string;
  end;

implementation

end.
