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
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button16: TButton;
    Button15: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FThreads: array [0 .. 19] of TLocalizaUF;
    procedure IniciarThread(const AReferencia: Byte; ALabel: TLabel);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.RegularExpressions;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  oLabel          : TLabel;
  iReferencia     : Integer;
  sNome           : string;
  regex_Referencia: TMatch;
begin
  sNome := TButton(Sender).Name;
  regex_Referencia := TRegEx.Match(sNome, '\d+$', []);
  if (regex_Referencia.Success) then
  begin
    iReferencia := StrToIntDef(regex_Referencia.Value, 0);
    if (iReferencia > 0) then
    begin
      oLabel := TLabel(Self.FindComponent(Format('Label%d', [iReferencia])));
      Self.IniciarThread(iReferencia, oLabel);
      TButton(Sender).Enabled := False;
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  oThread: TLocalizaUF;
begin
  for oThread in Self.FThreads do
  begin
    if (Assigned(oThread)) then
    begin
      if not(oThread.Finished) then
      begin
        oThread.Terminate;
        oThread.WaitFor;
        oThread.Free;
      end;
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

procedure TForm1.IniciarThread(const AReferencia: Byte; ALabel: TLabel);
begin
  Self.FThreads[AReferencia] := TLocalizaUF.Create(ALabel);
  Self.FThreads[AReferencia].Start;
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
