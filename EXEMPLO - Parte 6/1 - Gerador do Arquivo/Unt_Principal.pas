unit Unt_Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    ProgressBar1: TProgressBar;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
type

  TLinha = record
    ID: NativeUInt;
    NomeCliente: string[50];
    DataCompra: TDate;
    Estado: string[2];
    ValorCompra: Currency;
    ChaveNFE: string[44];
    Observacoes: string[27];
  end;

const
  C_FORMATO = '%7d%-100s%10s%2s%10s%44s%27s';
var
  _arquivo           : TextFile;
  sNomeArquivo       : string;
  i                  : Integer;
  rLinha             : TLinha;
  sLinha             : string;
  iQuantidadeRegistro: NativeUInt;
begin
  iQuantidadeRegistro := Self.SpinEdit1.Value;
  try
    Self.SaveDialog1.Filter := 'Texto|*.txt';
    Self.SaveDialog1.DefaultExt := '.txt';
    if not(Self.SaveDialog1.Execute(Self.Handle)) then
    begin
      Exit;
    end;
    sNomeArquivo := Self.SaveDialog1.FileName;
    AssignFile(_arquivo, sNomeArquivo);
    Rewrite(_arquivo);

    Screen.Cursor := crHourGlass;

    with rLinha do
    begin
      NomeCliente := 'Active Delphi';
      DataCompra := Date();
      Estado := 'SP';
      ValorCompra := 2013.55;
      ChaveNFE := StringOfChar('9', 44);
      Observacoes := 'nonononono';
    end;

    Self.ProgressBar1.Position := 0;
    Self.ProgressBar1.Max := iQuantidadeRegistro;
    Self.ProgressBar1.Step := 1;
    for i := 1 to iQuantidadeRegistro do
    begin
      rLinha.ID := i;

      with rLinha do
      begin
        sLinha := Format(C_FORMATO, [ID,
                                     NomeCliente,
                                     DateToStr(DataCompra),
                                     Estado,
                                     FloatToStr(ValorCompra),
                                     ChaveNFE,
                                     Observacoes]);
      end;

      Writeln(_arquivo, sLinha);
      Self.ProgressBar1.StepIt;
    end;

    CloseFile(_arquivo);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
