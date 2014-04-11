object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gera'#231#227'o de arquivo CSV'
  ClientHeight = 89
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 282
    Top = 46
    Width = 75
    Height = 25
    Caption = 'Gerar!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 23
    Width = 345
    Height = 17
    Step = 1
    TabOrder = 1
  end
end
