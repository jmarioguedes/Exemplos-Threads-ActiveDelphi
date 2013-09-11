object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sistema de gera'#231#227'o de LOG'
  ClientHeight = 383
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 105
    Caption = 'Acionar Thread #1'
    TabOrder = 0
    object Button1: TButton
      Left = 55
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 199
    Top = 8
    Width = 185
    Height = 105
    Caption = 'Acionar Thread #2'
    TabOrder = 1
    object Button2: TButton
      Left = 55
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 119
    Width = 376
    Height = 66
    Caption = 'Linha eventual pelo thread principal'
    TabOrder = 2
    object Label1: TLabel
      Left = 97
      Top = 29
      Width = 187
      Height = 13
      AutoSize = False
      Caption = 'Label1'
    end
    object Button3: TButton
      Left = 14
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 290
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Button4'
      TabOrder = 1
      OnClick = Button4Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 364
    Width = 392
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Memo1: TMemo
    Left = 8
    Top = 191
    Width = 376
    Height = 167
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    WordWrap = False
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 272
  end
end
