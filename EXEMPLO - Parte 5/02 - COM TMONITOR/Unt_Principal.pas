unit Unt_Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,
  Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls;

type

  TLocalizaUF = class(TThread)
  private
    FConteudoLabel: string;
    FLabelError   : TLabel;
    procedure EscreverLabel;
  protected
    procedure Execute; override;
  public
    constructor Create(ALabelError: TLabel);
  end;

  TForm1 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    ClientDataSet1SIGLA: TStringField;
    ClientDataSet1NOME: TStringField;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FThread1: TLocalizaUF;
    FThread2: TLocalizaUF;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.FThread1 := TLocalizaUF.Create(Self.Label1);
  Self.FThread1.Start;
  Self.Button1.Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.FThread2 := TLocalizaUF.Create(Self.Label2);
  Self.FThread2.Start;
  Self.Button2.Enabled := False;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Self.FThread1) then
  begin
    if not(Self.FThread1.Finished) then
    begin
      Self.FThread1.Terminate;
      Self.FThread1.WaitFor;
      Self.FThread1.Free;
    end;
  end;

  if Assigned(Self.FThread2) then
  begin
    if not(Self.FThread2.Finished) then
    begin
      Self.FThread2.Terminate;
      Self.FThread2.WaitFor;
      Self.FThread2.Free;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Self.ClientDataSet1 do
  begin
    Open;
    AppendRecord(['AC', 'Acre']);
    AppendRecord(['AL', 'Alagoas']);
    AppendRecord(['AP', 'Amapá']);
    AppendRecord(['AM', 'Amazonas']);
    AppendRecord(['BA', 'Bahia']);
    AppendRecord(['CE', 'Ceará']);
    AppendRecord(['DF', 'Distrito Federal']);
    AppendRecord(['ES', 'Espírito Santo']);
    AppendRecord(['GO', 'Goiás']);
    AppendRecord(['MA', 'Maranhão']);
    AppendRecord(['MT', 'Mato Grosso']);
    AppendRecord(['MS', 'Mato Grosso do Sul']);
    AppendRecord(['MG', 'Minas Gerais']);
    AppendRecord(['PA', 'Pará']);
    AppendRecord(['PB', 'Paraíba']);
    AppendRecord(['PR', 'Paraná']);
    AppendRecord(['PE', 'Pernambuco']);
    AppendRecord(['PI', 'Piauí']);
    AppendRecord(['RJ', 'Rio de Janeiro']);
    AppendRecord(['RN', 'Rio Grande do Norte']);
    AppendRecord(['RS', 'Rio Grande do Sul']);
    AppendRecord(['RO', 'Rondônia']);
    AppendRecord(['RR', 'Roraima']);
    AppendRecord(['SC', 'Santa Catarina']);
    AppendRecord(['SP', 'São Paulo']);
    AppendRecord(['SE', 'Sergipe']);
    AppendRecord(['TO', 'Tocantins']);
  end;
end;

{ TLocalizaUF }

constructor TLocalizaUF.Create(ALabelError: TLabel);
begin
  inherited Create(True);
  Self.FLabelError := ALabelError;
end;

procedure TLocalizaUF.EscreverLabel;
begin
  Self.FLabelError.Caption := Self.FConteudoLabel;
end;

procedure TLocalizaUF.Execute;
const
  C_MAX                                  = 5;
  C_UF: array [0 .. C_MAX - 1] of string = ('SP', 'BA', 'RJ', 'SE', 'AM');
var
  iPos       : Byte;
  sUF        : string;
  iQuantidade: NativeUInt;
begin
  inherited;
  iQuantidade := 0;
  while not(Self.Terminated) do
  begin
    iPos := Random(C_MAX);
    sUF := C_UF[iPos];
    System.TMonitor.Enter(Form1.ClientDataSet1);
    try
      if (Form1.ClientDataSet1.Locate('SIGLA', sUF, [])) then
      begin
        Sleep(100);
        if (sUF <> Form1.ClientDataSet1SIGLA.AsString) then
        begin
          Self.FConteudoLabel := 'Deu erro!!!';
          Self.Queue(Self.EscreverLabel);
          Abort;
        end;
        Inc(iQuantidade);
        Self.FConteudoLabel := Format('Passou [%d] vezes', [iQuantidade]);
        Self.Queue(Self.EscreverLabel);
      end;
    finally
      System.TMonitor.Exit(Form1.ClientDataSet1);
    end;
  end;
end;

end.
