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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Unt_FuncaoLog,
  Unt_DiversasThreads;

type

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Memo1: TMemo;
    Label1: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    /// <summary>
    /// Instância do primeiro objeto
    /// </summary>
    FThread1: TDiversaThread;
    /// <summary>
    /// Instância do segundo objeto
    /// </summary>
    FThread2: TDiversaThread;
    /// <summary>
    /// Método que será associado ao evento OnTerminate de cada thread
    /// </summary>
    procedure EventoFinalizacaoThread(Sender: TObject);
    /// <summary>
    /// Método responsável por alimentar o memo com eventuais erros
    /// </summary>
    procedure AlimentarMemoErro(const AReferencia: string; const AErro: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TForm1.AlimentarMemoErro(const AReferencia, AErro: string);
var
  sLinha: string;
begin
  sLinha := Format('%s -> %s', [AReferencia, AErro]);
  Memo1.Lines.Insert(0, sLinha);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.Button1.Enabled := False;
  Self.FThread1 := TDiversaThread.Create(True);
  Self.FThread1.Referencia := 'THREAD #1';
  Self.FThread1.OnTerminate := Self.EventoFinalizacaoThread;
  Self.FThread1.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.Button2.Enabled := False;
  Self.FThread2 := TDiversaThread.Create(True);
  Self.FThread2.Referencia := 'THREAD #2';
  Self.FThread2.OnTerminate := Self.EventoFinalizacaoThread;
  Self.FThread2.Start;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i      : Integer;
  rCronus: TStopwatch;
begin
  try
    rCronus := TStopwatch.StartNew;
    for i := 1 to 1000 do
    begin
      TGeraLog.Instancia.GerarLog('MAIN THREAD', Now, 'Três pratos de trigo para três tigres tristes');
    end;
    rCronus.Stop;

    Self.Label1.Caption := Format('Processado em: %d milisegundos', [rCronus.ElapsedMilliseconds]);
  except
    on E: EInOutError do
    begin
      Self.AlimentarMemoErro('MAIN THREAD', Format('Erro de I/O #%d - %s', [E.ErrorCode, SysErrorMessage(E.ErrorCode)]));
    end;
    on E: Exception do
    begin
      Self.AlimentarMemoErro('MAIN THREAD', Format('(%s) - %s', [E.ClassName, E.Message]));
    end;
  end;
end;

procedure TForm1.EventoFinalizacaoThread(Sender: TObject);
var
  oThread: TDiversaThread;
begin
  oThread := TDiversaThread(Sender);
  Self.AlimentarMemoErro(oThread.Referencia, oThread.DescricaoErro);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(Self.FThread1) and not(Self.FThread1.Finished) then
  begin
    Self.FThread1.Terminate;
    Self.FThread1.WaitFor;
  end;

  if Assigned(Self.FThread2) and not(Self.FThread2.Finished) then
  begin
    Self.FThread2.Terminate;
    Self.FThread2.WaitFor;
  end;

  if Assigned(Self.FThread1) then
  begin
    Self.FThread1.Free;
  end;

  if Assigned(Self.FThread2) then
  begin
    Self.FThread2.Free;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := DateTimeToStr(Now);
end;

end.
