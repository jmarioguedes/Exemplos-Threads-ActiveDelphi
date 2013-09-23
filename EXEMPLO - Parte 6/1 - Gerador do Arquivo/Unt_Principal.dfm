object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gera'#231#227'o do arquivo'
  ClientHeight = 102
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 64
    Width = 396
    Height = 17
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Gerar!'
    TabOrder = 1
    OnClick = Button1Click
  end
  object SpinEdit1: TSpinEdit
    Left = 328
    Top = 26
    Width = 76
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 100000
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Texto|*.txt'
    Left = 184
    Top = 16
  end
end
