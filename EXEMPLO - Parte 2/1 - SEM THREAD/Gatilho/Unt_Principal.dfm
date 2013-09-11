object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gatilho'
  ClientHeight = 113
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 136
    Height = 13
    Caption = 'Handle do controle alvo:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 150
    Top = 16
    Width = 107
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    AlignWithMargins = True
    Left = 8
    Top = 46
    Width = 249
    Height = 25
    Caption = 'PostMessage'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 77
    Width = 249
    Height = 25
    Caption = 'SendMessage'
    TabOrder = 2
    OnClick = Button2Click
  end
end
