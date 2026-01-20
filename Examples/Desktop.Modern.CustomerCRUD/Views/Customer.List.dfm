object CustomerListFrame: TCustomerListFrame
  Left = 0
  Top = 0
  Width = 700
  Height = 500
  TabOrder = 0
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = 12092416
    ParentBackground = False
    TabOrder = 0
    object TitleLabel: TLabel
      Left = 16
      Top = 15
      Width = 80
      Height = 21
      Caption = 'Customers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object ToolbarPanel: TPanel
    Left = 0
    Top = 50
    Width = 700
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    object LblSearch: TLabel
      Left = 470
      Top = 14
      Width = 38
      Height = 15
      Caption = 'Search:'
    end
    object NewButton: TButton
      Left = 10
      Top = 8
      Width = 80
      Height = 28
      Caption = 'New'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = NewButtonClick
    end
    object EditButton: TButton
      Left = 96
      Top = 8
      Width = 80
      Height = 28
      Caption = 'Edit'
      Enabled = False
      TabOrder = 1
      OnClick = EditButtonClick
    end
    object DeleteButton: TButton
      Left = 182
      Top = 8
      Width = 80
      Height = 28
      Caption = 'Delete'
      Enabled = False
      TabOrder = 2
      OnClick = DeleteButtonClick
    end
    object RefreshButton: TButton
      Left = 280
      Top = 8
      Width = 80
      Height = 28
      Caption = 'Refresh'
      TabOrder = 3
      OnClick = RefreshButtonClick
    end
    object SearchEdit: TEdit
      Left = 515
      Top = 11
      Width = 175
      Height = 23
      TabOrder = 4
      OnChange = SearchEditChange
    end
  end
  object GridPanel: TPanel
    Left = 0
    Top = 95
    Width = 700
    Height = 375
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 2
    object CustomerGrid: TStringGrid
      Left = 10
      Top = 10
      Width = 680
      Height = 355
      Align = alClient
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      TabOrder = 0
      OnSelectCell = CustomerGridSelectCell
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 470
    Width = 700
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 3
    object StatusLabel: TLabel
      Left = 10
      Top = 8
      Width = 48
      Height = 15
      Caption = '0 records'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
end
