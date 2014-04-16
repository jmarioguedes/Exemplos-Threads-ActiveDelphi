unit Unt_Principal;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtDlgs,
  Vcl.CheckLst;

type

  TThreadImportacao = class(TThread)
  private
    FNomeArquivo  : string;
    FTempoSegundos: NativeUInt;
  public
    constructor Create(const ANomeArquivo: string); reintroduce;
    procedure Execute; override;
    property TempoSegundos: NativeUInt read FTempoSegundos;
  end;

  TfImportacao = class(TForm)
    Button1: TButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    lTempo: TLabel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FThread: TThreadImportacao;
    procedure QuandoTerminar(Sender: TObject);
  public
    { Public declarations }
  end;

var
  fImportacao: TfImportacao;

implementation

uses
  System.Diagnostics,
  System.TypInfo;

{$R *.dfm}
{ TThreadImportacao }

constructor TThreadImportacao.Create(const ANomeArquivo: string);
begin
  inherited Create(True);
  Self.FNomeArquivo := ANomeArquivo;
end;

procedure TThreadImportacao.Execute;
const
  C_INSTRUCAO_SQL = 'INSERT INTO tbcliente (NOME_CLIENTE, CNPJ, ENDERECO, TELEFONE_1, TELEFONE_2, EMAIL) VALUES ("%s", "%s", "%s", "%s", "%s", "%s");';
var
  _csv         : TextFile;
  _sql         : TextFile;
  sArquivoSQL  : string;
  sLinha       : string;
  slAuxiliar   : TStringList;
  sInstrucaoSQL: string;
  rTempo       : TStopwatch;
begin
  inherited;
  rTempo := TStopwatch.StartNew;
  sArquivoSQL := ChangeFileExt(Self.FNomeArquivo, '.sql');

  AssignFile(_csv, Self.FNomeArquivo);
  AssignFile(_sql, sArquivoSQL);

  slAuxiliar := TStringList.Create;
  try
    Reset(_csv);
    Rewrite(_sql);

    while not Eof(_csv) do
    begin
      if Self.Terminated then
      begin
        Break;
      end;

      Readln(_csv, sLinha);
      slAuxiliar.CommaText := sLinha;

      sInstrucaoSQL := Format(C_INSTRUCAO_SQL, [slAuxiliar[0], slAuxiliar[1], slAuxiliar[2], slAuxiliar[3], slAuxiliar[4], slAuxiliar[5]]);
      Writeln(_sql, sInstrucaoSQL);
    end;
  finally
    rTempo.Stop;
    Self.FTempoSegundos := rTempo.ElapsedMilliseconds div 1000;
    slAuxiliar.Free;
    CloseFile(_csv);
    CloseFile(_sql);
  end;
end;

procedure TfImportacao.Button1Click(Sender: TObject);
var
  sNomeArquivo: string;
  iPrioridade : Integer;
begin
  Self.OpenTextFileDialog1.Title := 'Selecionar arquivo ...';
  Self.OpenTextFileDialog1.Filter := 'Arquivo CSV|*.csv';

  if (Self.OpenTextFileDialog1.Execute(Self.Handle)) then
  begin
    Self.Button1.Enabled := False;
    sNomeArquivo := Self.OpenTextFileDialog1.FileName;

    Self.FThread := TThreadImportacao.Create(sNomeArquivo);
    Self.FThread.FreeOnTerminate := True;

    iPrioridade := GetEnumValue(TypeInfo(TThreadPriority), Self.ComboBox1.Text);
    Self.FThread.Priority := TThreadPriority(iPrioridade);

    Self.FThread.OnTerminate := Self.QuandoTerminar;
    Self.FThread.Start;
  end;
end;

procedure TfImportacao.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Assigned(Self.FThread)) then
  begin
    Self.FThread.OnTerminate := nil;
    Self.FThread.FreeOnTerminate := False;
    Self.FThread.Terminate;
    Self.FThread.WaitFor;
    Self.FThread.Free;
  end;
end;

procedure TfImportacao.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TfImportacao.QuandoTerminar(Sender: TObject);
var
  oThread: TThreadImportacao;
begin
  oThread := TThreadImportacao(Sender);

  Self.lTempo.Caption := IntToStr(oThread.TempoSegundos);
  Self.FThread := nil;
  Self.Button1.Enabled := True;
end;

end.
