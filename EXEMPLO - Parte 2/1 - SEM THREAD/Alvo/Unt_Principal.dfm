object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Alvo'
  ClientHeight = 203
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 89
    Top = 8
    Width = 208
    Height = 145
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnLoop: TButton
    Left = 8
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Loop'
    TabOrder = 2
    OnClick = btnLoopClick
  end
  object ProgressBar1: TProgressBar
    Left = 89
    Top = 168
    Width = 208
    Height = 17
    TabOrder = 3
  end
  object btnSleep: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Sleep(5000)'
    TabOrder = 4
    OnClick = btnSleepClick
  end
end
