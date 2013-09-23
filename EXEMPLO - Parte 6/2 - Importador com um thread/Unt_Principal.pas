unit Unt_Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ProgressBar1: TProgressBar;
    bImportar: TButton;
    Edit1: TEdit;
    procedure bImportarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ImportacaoFinalizada(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unt_Importacao;

procedure TForm1.bImportarClick(Sender: TObject);

  function _TamanhoArquivo: NativeUInt;
  var
    hArquivo: NativeUInt;
  begin
    hArquivo := FileOpen(Self.Edit1.Text, fmOpenRead);
    Result := FileSeek(hArquivo, 0, 2);
    FileClose(hArquivo);
  end;

var
  oImportador: TImportador;
  iTamArquivo: NativeUInt;
begin
  Self.bImportar.Enabled := False;

  iTamArquivo := _TamanhoArquivo;
  Self.ProgressBar1.Position := 0;
  Self.ProgressBar1.Max := iTamArquivo;

  oImportador := TImportador.Create(Self.Edit1.Text, Self.ProgressBar1.Handle);
  oImportador.FreeOnTerminate := True;
  oImportador.OnTerminate := Self.ImportacaoFinalizada;
  oImportador.Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.ImportacaoFinalizada(Sender: TObject);
var
  iTempo   : Integer;
  sMensagem: string;
begin
  iTempo := TImportador(Sender).TempoSegundos;
  sMensagem := Format('Importação finalizada em [%d] segundos!', [iTempo]);

  MessageBox(Self.Handle, PWideChar(sMensagem), 'Atenção!', MB_ICONINFORMATION + MB_OK);
  Self.bImportar.Enabled := True;
end;

end.
