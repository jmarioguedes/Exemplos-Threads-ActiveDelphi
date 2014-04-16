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
    FAfinidade    : Integer;
    FNomeArquivo  : string;
    FTempoSegundos: NativeUInt;
  public
    constructor Create(const ANomeArquivo: string); reintroduce;
    procedure DefinirAfinidade(ANumCPU: Byte);
    procedure Execute; override;
    property TempoSegundos: NativeUInt read FTempoSegundos;
  end;

  TThreadGerente = class(TThread)
  private
    FQuantidadeLinhas: NativeUInt;
    FNomeArquivo     : string;
    FTempoSegundos   : NativeUInt;
    FThreads         : array of TThreadImportacao;
    procedure QuantificarLinhas;
    procedure QuantificarLinhasTextFile;
    function DividirArquivos: TArray<string>;
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
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FThread: TThreadImportacao;
    procedure QuandoTerminar(Sender: TObject);
    procedure QuandoTerminarGerente(Sender: TObject);
  public
    { Public declarations }
  end;

var
  fImportacao: TfImportacao;

implementation

uses
  System.StrUtils,
  System.Diagnostics,
  System.TypInfo;

{$R *.dfm}
{ TThreadImportacao }

constructor TThreadImportacao.Create(const ANomeArquivo: string);
begin
  inherited Create(True);
  Self.FNomeArquivo := ANomeArquivo;
end;

procedure TThreadImportacao.DefinirAfinidade(ANumCPU: Byte);
var
  sBits   : string;
  i       : Integer;
  iTamanho: Integer;
begin
  sBits := '00000000';
  sBits[ANumCPU] := '1';
  sBits := ReverseString(sBits);
  iTamanho := Length(sBits);

  for i := iTamanho downto 1 do
  begin
    if (sBits[i] = '1') then
      Self.FAfinidade := Self.FAfinidade + (1 shl (iTamanho - i));
  end;
end;

procedure TThreadImportacao.Execute;
const
  C_INSTRUCAO_SQL = 'INSERT INTO tbcliente (NOME_CLIENTE, CNPJ, ENDERECO, TELEFONE_1, TELEFONE_2, EMAIL) VALUES ("%s", "%s", "%s", "%s", "%s", "%s");';
var
  _csv         : TextFile;
  sLinha       : string;
  slAuxiliar   : TStringList;
  sInstrucaoSQL: string;
  rTempo       : TStopwatch;
begin
  inherited;
  SetThreadAffinityMask(Self.Handle, Self.FAfinidade);

  slAuxiliar := TStringList.Create;
  rTempo := TStopwatch.StartNew;
  AssignFile(_csv, Self.FNomeArquivo);
  try
    Reset(_csv);

    while not Eof(_csv) do
    begin
      if Self.Terminated then
      begin
        Break;
      end;

      Readln(_csv, sLinha);
      slAuxiliar.CommaText := sLinha;

      sInstrucaoSQL := Format(C_INSTRUCAO_SQL, [slAuxiliar[0], slAuxiliar[1], slAuxiliar[2], slAuxiliar[3], slAuxiliar[4], slAuxiliar[5]]);
    end;
  finally
    rTempo.Stop;
    Self.FTempoSegundos := rTempo.ElapsedMilliseconds div 1000;
    slAuxiliar.Free;
    CloseFile(_csv);
  end;
end;

procedure TfImportacao.Button1Click(Sender: TObject);
var
  sNomeArquivo: string;
  iPrioridade : Integer;
  bRet        : Boolean;
begin
  Self.OpenTextFileDialog1.Title := 'Selecionar arquivo ...';
  Self.OpenTextFileDialog1.Filter := 'Arquivo CSV|*.csv';
  bRet := Self.OpenTextFileDialog1.Execute(Self.Handle);

  if (bRet) then
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

procedure TfImportacao.Button2Click(Sender: TObject);
var
  oGerente    : TThreadGerente;
  bRet        : Boolean;
  sNomeArquivo: string;
begin
  Self.OpenTextFileDialog1.Title := 'Selecionar arquivo ...';
  Self.OpenTextFileDialog1.Filter := 'Arquivo CSV|*.csv';
  bRet := Self.OpenTextFileDialog1.Execute(Self.Handle);

  if (bRet) then
  begin
    Self.Button2.Enabled := False;
    sNomeArquivo := Self.OpenTextFileDialog1.FileName;

    oGerente := TThreadGerente.Create(sNomeArquivo);
    oGerente.FreeOnTerminate := True;
    oGerente.OnTerminate := Self.QuandoTerminarGerente;
    oGerente.Start;
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

procedure TfImportacao.QuandoTerminarGerente(Sender: TObject);
var
  oThread: TThreadGerente;
begin
  oThread := TThreadGerente(Sender);

  Self.lTempo.Caption := IntToStr(oThread.TempoSegundos);
  Self.Button2.Enabled := True;
end;

{ TThreadGerente }

constructor TThreadGerente.Create(const ANomeArquivo: string);
begin
  inherited Create(True);
  Self.FNomeArquivo := ANomeArquivo;
end;

function TThreadGerente.DividirArquivos: TArray<string>;
var
  _entrada           : file;
  _saida             : file;
  aBuffer            : array [1 .. MAXWORD] of Byte;
  iQuantProcessador  : Byte;
  iQuantPorArquivo   : NativeUInt;
  i                  : NativeUInt;
  iTamanhoLido       : NativeUInt;
  iLinhasEscritas    : NativeUInt;
  iPosicaoCharRetorno: NativeUInt;
  iChar              : Integer;
begin
  iQuantProcessador := TThread.ProcessorCount;
  iQuantPorArquivo := Self.FQuantidadeLinhas div iQuantProcessador;

  SetLength(Result, iQuantProcessador);
  for i := 0 to Pred(iQuantProcessador) do
  begin
    Result[i] := Format('.\arquivo_%d.csv', [i]);
  end;

  AssignFile(_entrada, Self.FNomeArquivo);
  try
    Reset(_entrada, 1);

    for i := 0 to Pred(iQuantProcessador) do
    begin
      iLinhasEscritas := 0;

      AssignFile(_saida, Result[i]);
      Rewrite(_saida, 1);
      try
        while True do
        begin
          BlockRead(_entrada, aBuffer, MAXWORD, iTamanhoLido);
          BlockWrite(_saida, aBuffer, iTamanhoLido);

          if Eof(_entrada) then
          begin
            // Se acabou o arquivo original, fim
            Exit;
          end;

          if (i = Pred(iQuantProcessador)) then
          begin
            // Se estiver escrevendo o último arquivo, vai até o fim
            Continue;
          end;

          for iChar := 1 to iTamanhoLido do
          begin
            if (aBuffer[iChar] = Ord(#10)) then
            begin
              Inc(iLinhasEscritas);
              iPosicaoCharRetorno := iChar;
            end;
          end;

          if (iLinhasEscritas >= iQuantPorArquivo) then
          begin
            // Lê byte à byte para terminar a linha
            if (aBuffer[iTamanhoLido] <> Ord(#10)) then
            begin
              repeat
                BlockRead(_entrada, aBuffer, 1, iTamanhoLido);
                BlockWrite(_saida, aBuffer, iTamanhoLido);
              until (aBuffer[1] = Ord(#10));
            end;

            Break;
          end;
        end;
      finally
        CloseFile(_saida);
      end;

      Self.FThreads[i] := TThreadImportacao.Create(Result[i]);
      Self.FThreads[i].DefinirAfinidade(Succ(i));
      Self.FThreads[i].Start;
    end;
  finally
    CloseFile(_entrada);
  end;
end;

procedure TThreadGerente.Execute;
var
  iQuantCPU: Byte;
  aArquivos: TArray<string>;
  aHandles : array [1 .. MAXBYTE] of THandle;
  _crono   : TStopwatch;
  i        : Integer;
  cRet     : Cardinal;
begin
  inherited;
  _crono := TStopwatch.StartNew;
  try
    SetThreadAffinityMask(Self.Handle, 1);
    Self.Priority := tpTimeCritical;

    iQuantCPU := TThread.ProcessorCount;
    SetLength(Self.FThreads, iQuantCPU);

    Self.QuantificarLinhas;
    aArquivos := Self.DividirArquivos;

    for i := 0 to Pred(iQuantCPU) do
    begin
      aHandles[Succ(i)] := Self.FThreads[i].Handle;
    end;

    cRet := WaitForMultipleObjects(iQuantCPU, @aHandles, True, INFINITE);

    for i := 0 to Pred(iQuantCPU) do
    begin
      Self.FThreads[i].Free;
    end;

  finally
    _crono.Stop;
    Self.FTempoSegundos := _crono.ElapsedMilliseconds div 1000;
  end;

end;

procedure TThreadGerente.QuantificarLinhas;
var
  _manipulador: file;
  aBuffer     : array [1 .. MAXWORD] of Byte;
  iCaracter   : NativeUInt;
  iTamanhoLido: NativeUInt;
begin
  Self.FQuantidadeLinhas := 0;
  AssignFile(_manipulador, Self.FNomeArquivo);
  try
    Reset(_manipulador, 1);

    while not Eof(_manipulador) do
    begin
      BlockRead(_manipulador, aBuffer, MAXWORD, iTamanhoLido);
      for iCaracter := 1 to iTamanhoLido do
      begin
        if (aBuffer[iCaracter] = Ord(#13)) then
        begin
          Inc(Self.FQuantidadeLinhas);
        end;
      end;
    end;
  finally
    CloseFile(_manipulador);
  end;
end;

procedure TThreadGerente.QuantificarLinhasTextFile;
var
  _manipulador: TextFile;
begin
  AssignFile(_manipulador, Self.FNomeArquivo);
  Reset(_manipulador);
  Self.FQuantidadeLinhas := 0;
  while not Eof(_manipulador) do
  begin
    Readln(_manipulador);
    Inc(Self.FQuantidadeLinhas);
  end;
  CloseFile(_manipulador);
end;

end.
