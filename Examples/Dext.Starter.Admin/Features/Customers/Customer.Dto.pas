unit Customer.Dto;

interface

uses
  Dext.Web;

type
  TCustomerDto = record
    Id: Integer;
    Name: string;
    [Required]
    Email: string;
    TotalSpent: Double;
  end;

implementation

end.
