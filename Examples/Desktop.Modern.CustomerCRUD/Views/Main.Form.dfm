object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Desktop Modern - Customer Management'
  ClientHeight = 600
  ClientWidth = 900
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SidePanel: TPanel
      Left = 0
      Top = 0
      Width = 180
      Height = 600
      Align = alLeft
      BevelOuter = bvNone
      Color = 3355443
      ParentBackground = False
      TabOrder = 0
      object LogoLabel: TLabel
        Left = 0
        Top = 0
        Width = 180
        Height = 60
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'DEXT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object BtnCustomers: TButton
        Left = 10
        Top = 80
        Width = 160
        Height = 35
        Caption = 'Customers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = BtnCustomersClick
      end
      object BtnAbout: TButton
        Left = 10
        Top = 555
        Width = 160
        Height = 35
        Caption = 'About'
        TabOrder = 1
        OnClick = BtnAboutClick
      end
    end
    object ContentPanel: TPanel
      Left = 180
      Top = 0
      Width = 720
      Height = 600
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
    end
  end
end
