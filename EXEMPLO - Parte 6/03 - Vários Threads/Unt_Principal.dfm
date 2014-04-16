object fImportacao: TfImportacao
  Left = 0
  Top = 0
  Caption = 'Exemplo utilizando v'#225'rios threads'
  ClientHeight = 137
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lTempo: TLabel
    Left = 8
    Top = 106
    Width = 344
    Height = 23
    Alignment = taCenter
    AutoSize = False
    Caption = 'lTempo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 173
    Top = 15
    Width = 61
    Height = 13
    Caption = 'Prioridade:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Processar!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 240
    Top = 12
    Width = 112
    Height = 21
    Style = csDropDownList
    ItemIndex = 3
    TabOrder = 1
    Text = 'tpNormal'
    Items.Strings = (
      'tpIdle'
      'tpLowest'
      'tpLower'
      'tpNormal'
      'tpHigher'
      'tpHighest'
      'tpTimeCritical')
  end
  object Button2: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'V'#225'rios!'
    TabOrder = 2
    OnClick = Button2Click
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 256
    Top = 48
  end
end
