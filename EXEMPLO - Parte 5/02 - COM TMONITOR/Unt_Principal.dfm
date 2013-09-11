object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Exemplo de utiliza'#231#227'o do TMonitor'
  ClientHeight = 297
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 464
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object Label1: TLabel
      Left = 89
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 306
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = '#1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 225
      Top = 9
      Width = 75
      Height = 25
      Caption = '#2'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 50
    Width = 464
    Height = 244
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ClientDataSet1: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 168
    Data = {
      4D0000009619E0BD0100000018000000020000000000030000004D0005534947
      4C410100490000000100055749445448020002000200044E4F4D450100490000
      0001000557494454480200020032000000}
    object ClientDataSet1SIGLA: TStringField
      FieldName = 'SIGLA'
      Size = 2
    end
    object ClientDataSet1NOME: TStringField
      FieldName = 'NOME'
      Size = 50
    end
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = ClientDataSet1
    Left = 200
    Top = 168
  end
end
