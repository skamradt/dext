object CustomerEditFrame: TCustomerEditFrame
  Left = 0
  Top = 0
  Width = 450
  Height = 500
  TabOrder = 0
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = 12092416
    ParentBackground = False
    TabOrder = 0
    object TitleLabel: TLabel
      Left = 16
      Top = 15
      Width = 112
      Height = 21
      Caption = 'New Customer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object ContentPanel: TPanel
    Left = 0
    Top = 50
    Width = 450
    Height = 400
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 16
    Padding.Right = 16
    Padding.Bottom = 16
    TabOrder = 1
    object NameLabel: TLabel
      Left = 16
      Top = 16
      Width = 35
      Height = 15
      Caption = 'Name:'
    end
    object EmailLabel: TLabel
      Left = 16
      Top = 70
      Width = 32
      Height = 15
      Caption = 'Email:'
    end
    object PhoneLabel: TLabel
      Left = 16
      Top = 124
      Width = 37
      Height = 15
      Caption = 'Phone:'
    end
    object DocumentLabel: TLabel
      Left = 234
      Top = 124
      Width = 59
      Height = 15
      Caption = 'Document:'
    end
    object NotesLabel: TLabel
      Left = 16
      Top = 210
      Width = 34
      Height = 15
      Caption = 'Notes:'
    end
    object NameEdit: TEdit
      Left = 16
      Top = 35
      Width = 418
      Height = 23
      TabOrder = 0
      OnChange = NameEditChange
    end
    object EmailEdit: TEdit
      Left = 16
      Top = 89
      Width = 418
      Height = 23
      TabOrder = 1
      OnChange = EmailEditChange
    end
    object PhoneEdit: TEdit
      Left = 16
      Top = 143
      Width = 200
      Height = 23
      TabOrder = 2
      OnChange = PhoneEditChange
    end
    object DocumentEdit: TEdit
      Left = 234
      Top = 143
      Width = 200
      Height = 23
      TabOrder = 3
      OnChange = DocumentEditChange
    end
    object ActiveCheckBox: TCheckBox
      Left = 16
      Top = 180
      Width = 97
      Height = 17
      Caption = 'Active'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = ActiveCheckBoxClick
    end
    object NotesMemo: TMemo
      Left = 16
      Top = 229
      Width = 418
      Height = 100
      TabOrder = 5
      OnChange = NotesMemoChange
    end
    object ErrorPanel: TPanel
      Left = 16
      Top = 340
      Width = 418
      Height = 44
      BevelOuter = bvNone
      Color = 13421823
      ParentBackground = False
      TabOrder = 6
      Visible = False
      object ErrorsLabel: TLabel
        Left = 8
        Top = 8
        Width = 402
        Height = 28
        AutoSize = False
        Caption = 'Errors will appear here'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 450
    Width = 450
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 2
    object SaveButton: TButton
      Left = 240
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Save'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = SaveButtonClick
    end
    object CancelButton: TButton
      Left = 350
      Top = 10
      Width = 80
      Height = 30
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelButtonClick
    end
  end
end
