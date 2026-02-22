unit Dext.Entity.FluentAPI.Test;

interface

uses
  System.SysUtils,
  System.Rtti,
  Dext.Core.SmartTypes,
  Dext.Entity.Core,
  Dext.Entity.Mapping,
  Dext.Entity, 
  Dext.Entity.Prototype,
  Dext.Entity.Attributes;

type
  TBlog = class
  private
    FId: Prop<Integer>;
    FTitle: Prop<string>;
    FCreatedAt: Prop<TDateTime>;
    FVersion: Prop<Integer>;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Version: Integer read FVersion write FVersion;
  end;

  TBlogMap = class(TEntityTypeConfiguration<TBlog>)
  public
    procedure Configure(Builder: IEntityTypeBuilder<TBlog>); override;
  end;

implementation

{ TBlogMap }

procedure TBlogMap.Configure(Builder: IEntityTypeBuilder<TBlog>);
var
  b: TBlog;
begin
  b := Prototype.Entity<TBlog>;
  
  Builder.ToTable('blogs');
  
  Builder.Prop(b.Id).IsPK.IsAutoInc;
  Builder.Prop(b.Title).HasMaxLength(200).IsRequired;
  
  // Auditoria e Concorrência
  Builder.Prop(b.CreatedAt).IsCreatedAt;
  Builder.Prop(b.Version).IsVersion;
  
  // Shadow Property
  Builder.ShadowProperty('InternalRating').HasColumnName('internal_rating');
end;

end.
