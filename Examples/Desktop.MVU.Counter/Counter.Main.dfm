object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MVU Counter - Dext Framework Demo'
  ClientHeight = 520
  ClientWidth = 400
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
    Width = 400
    Height = 480
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 480
    Width = 400
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    object InfoLabel: TLabel
      Left = 0
      Top = 0
      Width = 400
      Height = 40
      Align = alClient
      Alignment = taCenter
      Caption = 'MVU Pattern Demo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 98
      ExplicitHeight = 13
    end
  end
end
