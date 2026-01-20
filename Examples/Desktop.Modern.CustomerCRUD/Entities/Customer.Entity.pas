{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Customer Entity - ORM mapped entity                             }
{                                                                           }
{***************************************************************************}
unit Customer.Entity;

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Core.SmartTypes; // Use Smart Types for type-safe queries

type
  /// <summary>
  /// Customer entity with ORM mapping and validation attributes
  /// </summary>
  [Table]
  TCustomer = class
  private
    FId: IntType;
    FName: StringType;
    FEmail: StringType;
    FPhone: StringType;
    FDocument: StringType;
    FActive: BoolType;
    FNotes: StringType;
    FCreatedAt: DateTimeType;
    FUpdatedAt: DateTimeType;
  public
    constructor Create;
    
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [MaxLength(100), Required]
    property Name: StringType read FName write FName;
    
    [MaxLength(150), Required, EmailAddress]
    property Email: StringType read FEmail write FEmail;
    
    [MaxLength(20)]
    property Phone: StringType read FPhone write FPhone;
    
    [MaxLength(20)]
    property Document: StringType read FDocument write FDocument;
    
    property Active: BoolType read FActive write FActive;
    
    property Notes: StringType read FNotes write FNotes;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    [UpdatedAt]
    property UpdatedAt: DateTimeType read FUpdatedAt write FUpdatedAt;
  end;

implementation

constructor TCustomer.Create;
begin
  inherited;
  FActive := True;
end;

end.
