object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TMonitor - Exemplo de utiliza'#231#227'o do Pulse & PulseAll'
  ClientHeight = 427
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 8
    Width = 567
    Height = 65
    Caption = 'Produtor'
    TabOrder = 0
    object Edit1: TEdit
      Left = 118
      Top = 22
      Width = 169
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button1: TButton
      Left = 293
      Top = 20
      Width = 75
      Height = 25
      Caption = 'Pulse'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 374
      Top = 20
      Width = 75
      Height = 25
      Caption = 'PulseAll'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 79
    Width = 185
    Height = 338
    Caption = 'Consumidor'
    TabOrder = 1
    object Memo1: TMemo
      Left = 2
      Top = 56
      Width = 181
      Height = 280
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 2
      Top = 15
      Width = 181
      Height = 41
      Align = alTop
      BevelInner = bvLowered
      Caption = 'Panel1'
      ShowCaption = False
      TabOrder = 1
      object Label1: TLabel
        Left = 89
        Top = 14
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Button3: TButton
        Left = 8
        Top = 10
        Width = 75
        Height = 25
        Caption = '#1'
        TabOrder = 0
        OnClick = Button3Click
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 199
    Top = 79
    Width = 185
    Height = 338
    Caption = 'Consumidor'
    TabOrder = 2
    object Memo2: TMemo
      Left = 2
      Top = 56
      Width = 181
      Height = 280
      Align = alClient
      Lines.Strings = (
        'Memo2')
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 2
      Top = 15
      Width = 181
      Height = 41
      Align = alTop
      BevelInner = bvLowered
      Caption = 'Panel2'
      ShowCaption = False
      TabOrder = 1
      object Label2: TLabel
        Left = 89
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Label2'
      end
      object Button4: TButton
        Left = 8
        Top = 10
        Width = 75
        Height = 25
        Caption = '#2'
        TabOrder = 0
        OnClick = Button4Click
      end
    end
  end
  object GroupBox4: TGroupBox
    Left = 391
    Top = 79
    Width = 185
    Height = 338
    Caption = 'Consumidor'
    TabOrder = 3
    object Memo3: TMemo
      Left = 2
      Top = 56
      Width = 181
      Height = 280
      Align = alClient
      Lines.Strings = (
        'Memo3')
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 2
      Top = 15
      Width = 181
      Height = 41
      Align = alTop
      BevelInner = bvLowered
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 1
      object Label3: TLabel
        Left = 89
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Label3'
      end
      object Button5: TButton
        Left = 8
        Top = 10
        Width = 75
        Height = 25
        Caption = '#3'
        TabOrder = 0
        OnClick = Button5Click
      end
    end
  end
end
