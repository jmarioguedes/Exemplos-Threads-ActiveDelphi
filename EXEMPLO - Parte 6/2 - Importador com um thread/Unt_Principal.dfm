object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Importador do arquivo - Um thread'
  ClientHeight = 93
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 35
    Width = 447
    Height = 17
    TabOrder = 0
  end
  object bImportar: TButton
    Left = 8
    Top = 58
    Width = 75
    Height = 25
    Caption = '&Importar'
    TabOrder = 1
    OnClick = bImportarClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 448
    Height = 21
    TabOrder = 2
    Text = 
      'D:\Projetos\Exemplos-Threads-ActiveDelphi\EXEMPLO - Parte 6\arqu' +
      'ivo\exemplo.txt'
  end
end
