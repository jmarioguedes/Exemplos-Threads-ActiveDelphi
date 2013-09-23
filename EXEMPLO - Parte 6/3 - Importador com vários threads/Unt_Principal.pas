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

uses Unt_Gerenciador;

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
  oGerenciador: TGerenciador;
begin
  Self.bImportar.Enabled := False;
  Self.ProgressBar1.Position := 0;
  Self.ProgressBar1.Max := _TamanhoArquivo;

  oGerenciador := TGerenciador.Create(Self.Edit1.Text, Self.ProgressBar1.Handle);
  oGerenciador.FreeOnTerminate := True;
  oGerenciador.OnTerminate := Self.ImportacaoFinalizada;
  oGerenciador.Start;
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
  iTempo := TGerenciador(Sender).TempoSegundos;
  sMensagem := Format('Importação finalizada em [%d] segundos!', [iTempo]);

  MessageBox(Self.Handle, PWideChar(sMensagem), 'Atenção!', MB_ICONINFORMATION + MB_OK);
  Self.bImportar.Enabled := True;
end;

end.
