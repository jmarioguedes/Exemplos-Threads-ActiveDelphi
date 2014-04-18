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
  Vcl.CheckLst,
  Data.DB,
  Data.SqlExpr,
  Data.DBXMSSQL,
  Data.DBXFirebird;

type

  TThreadEsvaziarTabela = class(TThread)
  public
    procedure Execute; override;
  end;

  /// <summary>
  /// Simula a efetiva importação dos dados
  /// </summary>
  TThreadImportacao = class(TThread)
  private
    FAfinidade       : Integer;
    FNomeArquivo     : string;
    FTempoSegundos   : NativeUInt;
    FQuantidadeLinhas: NativeUInt;
    FDatabase        : TSQLConnection;
    FQuery           : TSQLQuery;
  public
    /// <summary>
    /// Recebe o nome do arquivo alvo
    /// </summary>
    constructor Create(const ANomeArquivo: string); reintroduce;
    /// <summary>
    /// Libera as instâncias associadas
    /// </summary>
    procedure BeforeDestruction; override;
    /// <summary>
    /// Define corretamente a máscara de afinidade
    /// </summary>
    procedure DefinirAfinidade(ANumCPU: Byte);
    /// <summary>
    /// Rotina a ser executado pelo thread
    /// </summary>
    procedure Execute; override;
    /// <summary>
    /// Tempo total de execução
    /// </summary>
    property TempoSegundos: NativeUInt read FTempoSegundos;
    /// <summary>
    /// Quantidade de linhas processadas
    /// </summary>
    property QuantidadeLinhas: NativeUInt read FQuantidadeLinhas write FQuantidadeLinhas;
  end;

  /// <summary>
  /// Thread de gerenciamento das múltiplas threads
  /// </summary>
  TThreadGerente = class(TThread)
  private
    FNomeArquivo     : string;
    FTempoSegundos   : NativeUInt;
    FQuantidadeLinhas: NativeUInt;
    FEsvaziador      : TThreadEsvaziarTabela;
    FImportadores    : array of TThreadImportacao;
    /// <summary>
    /// Divide o arquivo em partes e os distribui aos threads
    /// </summary>
    procedure DividirArquivo;
  protected
    /// <summary>
    /// Utilizado para terminar os threads de importação
    /// </summary>
    procedure TerminatedSet; override;
  public
    /// <summary>
    /// Recebe o nome do arquivo alvo
    /// </summary>
    constructor Create(const ANomeArquivo: string); reintroduce;
    /// <summary>
    /// Rotina a ser executado pelo thread
    /// </summary>
    procedure Execute; override;
    /// <summary>
    /// Tempo total de execução
    /// </summary>
    property TempoSegundos: NativeUInt read FTempoSegundos;
    /// <summary>
    /// Quantidade de linhas processadas
    /// </summary>
    property QuantidadeLinhas: NativeUInt read FQuantidadeLinhas write FQuantidadeLinhas;
  end;

  TfImportacao = class(TForm)
    Button1: TButton;
    Button2: TButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    lTempo: TLabel;
    lQuantidade: TLabel;
    ComboBox1: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FThreadUnica  : TThreadImportacao;
    FThreadGerente: TThreadGerente;
    procedure QuandoTerminarThread(Sender: TObject);
    function EscolherArquivo: string;
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

procedure TThreadImportacao.BeforeDestruction;
begin
  inherited;
  Self.FQuery.Free;
  Self.FDatabase.Close;
  Self.FDatabase.Free;
end;

constructor TThreadImportacao.Create(const ANomeArquivo: string);
begin
  inherited Create(True);
  // Associa o nome do arquivo de entrada
  Self.FNomeArquivo := ANomeArquivo;

  // Cria a conexão com o banco de dados
  Self.FDatabase := TSQLConnection.Create(nil);
  with Self.FDatabase do
  begin
    LoginPrompt := False;
    DriverName := 'Firebird';
    Params.Values['Database'] := '192.168.0.132:MARIO';
    Params.Values['User_Name'] := 'sysdba';
    Params.Values['Password'] := 'masterkey';
    Open;
  end;

  // Cria e prepara a instrução SQL
  Self.FQuery := TSQLQuery.Create(nil);
  Self.FQuery.SQLConnection := Self.FDatabase;
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
  C_INSTRUCAO_SQL = 'INSERT INTO NEW_TABLE (ID, NOME_CLIENTE, CNPJ, ENDERECO, TELEFONE_1, TELEFONE_2, EMAIL) VALUES (%s, ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'');';
var
  _csv         : TextFile;
  sLinha       : string;
  slAuxiliar   : TStringList;
  sInstrucaoSQL: string;
  rTempo       : TStopwatch;
  i            : Integer;
begin
  inherited;
  rTempo := TStopwatch.StartNew;

  // Define a máscara de afinidade, se diferente de zeroS
  SetThreadAffinityMask(Self.Handle, Self.FAfinidade);

  // Instancia o StringList
  slAuxiliar := TStringList.Create;

  // Associa o maniopulador ao arquivo alvo
  AssignFile(_csv, Self.FNomeArquivo);
  try
    // Abre o arquivo para leitura
    Reset(_csv);

    // Enquanto não for o fim do arquivo ...
    while not Eof(_csv) do
    begin
      // Incrementa a quantidade de linhas processadas
      Inc(Self.FQuantidadeLinhas);

      // Se o thread foi sinalizado para terminar,
      // interrompe o ciclo
      if Self.Terminated then
      begin
        Break;
      end;

      // Lê a próxima linha
      Readln(_csv, sLinha);

      // Quebra os campos CSV
      slAuxiliar.CommaText := sLinha;

      // Monta a instrução SQL
      sInstrucaoSQL := Format(C_INSTRUCAO_SQL, [slAuxiliar[0], slAuxiliar[1], slAuxiliar[2], slAuxiliar[3], slAuxiliar[4], slAuxiliar[5], slAuxiliar[6]]);

      // Executa a instrução SQL
      Self.FQuery.SQL.Text := sInstrucaoSQL;
      Self.FQuery.ExecSQL;
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
  oEsvaziador : TThreadEsvaziarTabela;
begin
  // Seleciona o arquivo
  sNomeArquivo := Self.EscolherArquivo;

  // Desabilita os botões
  Self.Button1.Enabled := False;
  Self.Button2.Enabled := False;
  Self.ComboBox1.Enabled := False;

  // Esvazia a tabela
  oEsvaziador := TThreadEsvaziarTabela.Create(True);
  oEsvaziador.FreeOnTerminate := True;
  oEsvaziador.Start;

  WaitForSingleObject(oEsvaziador.Handle, INFINITE);

  // Determina o Ord() da prioridade do thread
  iPrioridade := GetEnumValue(TypeInfo(TThreadPriority), Self.ComboBox1.Text);

  // Configura e inicializa o thread, definindo a prioridade
  Self.FThreadUnica := TThreadImportacao.Create(sNomeArquivo);
  Self.FThreadUnica.Priority := TThreadPriority(iPrioridade);
  Self.FThreadUnica.FreeOnTerminate := True;
  Self.FThreadUnica.OnTerminate := Self.QuandoTerminarThread;
  Self.FThreadUnica.Start;
end;

procedure TfImportacao.Button2Click(Sender: TObject);
var
  sNomeArquivo: string;
begin
  // Seleciona o arquivo
  sNomeArquivo := Self.EscolherArquivo;

  // Desabilita os botões
  Self.Button1.Enabled := False;
  Self.Button2.Enabled := False;
  Self.ComboBox1.Enabled := False;

  // Configura e inicializa o thread
  Self.FThreadGerente := TThreadGerente.Create(sNomeArquivo);
  Self.FThreadGerente.FreeOnTerminate := True;
  Self.FThreadGerente.OnTerminate := Self.QuandoTerminarThread;
  Self.FThreadGerente.Start;
end;

function TfImportacao.EscolherArquivo: string;
var
  bRet: Boolean;
begin
  // Abre a caixa de diálogo para selecionar o arquivo
  Self.OpenTextFileDialog1.Title := 'Selecionar arquivo ...';
  Self.OpenTextFileDialog1.Filter := 'Arquivo CSV|*.csv';
  bRet := Self.OpenTextFileDialog1.Execute(Self.Handle);

  // Se o usuário cancelar a operação, gera erro
  if not bRet then
  begin
    raise Exception.Create('Operação cancelada pelo usuário');
  end;

  // Retorno do nome do arquivo escolhido
  Result := Self.OpenTextFileDialog1.FileName;
end;

procedure TfImportacao.FormClose(Sender: TObject; var Action: TCloseAction);
var
  oThread: TThread;
begin
  // Inicializa a variavel
  oThread := nil;

  // Se a thread única estiver sinalizada, assume
  if Assigned(Self.FThreadUnica) then
  begin
    oThread := Self.FThreadUnica;
  end;

  // Se a thread gerente estiver sinalizada, assume
  if Assigned(Self.FThreadGerente) then
  begin
    oThread := Self.FThreadGerente;
  end;

  // Se sinalizado ...
  if Assigned(oThread) then
  begin
    // "Desliga" o OnTerminate
    oThread.OnTerminate := nil;

    // "Desliga" o FreeOnTerminate
    oThread.FreeOnTerminate := False;

    // Indica o término
    oThread.Terminate;

    // Aguarda o término
    oThread.WaitFor;

    // Libera a instância
    oThread.Free;
  end;
end;

procedure TfImportacao.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TfImportacao.QuandoTerminarThread(Sender: TObject);
const
  C_TEMPO = 'Tempo: [%s] - [%d]';
var
  sTexto : string;
  iLinhas: NativeUInt;
begin
  // Se for um thread de importação ...
  if Sender is TThreadImportacao then
  begin
    sTexto := Format(C_TEMPO, ['Um thread!', Self.FThreadUnica.TempoSegundos]);
    iLinhas := Self.FThreadUnica.QuantidadeLinhas;
    Self.FThreadUnica := nil;
  end;

  // Se for a thread gerenciadora ...
  if Sender is TThreadGerente then
  begin
    sTexto := Format(C_TEMPO, ['Vários threads!', Self.FThreadGerente.TempoSegundos]);
    iLinhas := Self.FThreadGerente.QuantidadeLinhas;
    Self.FThreadGerente := nil;
  end;

  // Elementos de tela
  Self.lTempo.Caption := sTexto;
  Self.lQuantidade.Caption := FormatFloat('#,###,##0', iLinhas);
  Self.Button1.Enabled := True;
  Self.Button2.Enabled := True;
  Self.ComboBox1.Enabled := True;
end;

{ TThreadGerente }

constructor TThreadGerente.Create(const ANomeArquivo: string);
begin
  inherited Create(True);
  Self.FNomeArquivo := ANomeArquivo;
end;

procedure TThreadGerente.DividirArquivo;
const
  C_ASCII_10 = 10;
var
  _arq_entrada     : file;
  _arq_saida       : file;
  aBuffer          : array [1 .. MAXWORD] of Byte;
  iQuantProcessador: Byte;
  iTamEntrada      : NativeUInt;
  iTamPorArquivo   : NativeUInt;
  iNumProcessador  : NativeUInt;
  iTamBuffer       : NativeUInt;
  iTotalLido       : NativeUInt;
  sNomeArquivoSaida: string;
  iPosArray        : Byte;
begin
  // Associa o arquivo de entrada a um manipulador
  AssignFile(_arq_entrada, Self.FNomeArquivo);
  try
    // Vai para o início do arquivo
    Reset(_arq_entrada, 1);

    // Quantidade de processadores
    iQuantProcessador := TThread.ProcessorCount;
    SetLength(Self.FImportadores, iQuantProcessador);

    // Tamanho do arquivo em bytes
    iTamEntrada := FileSize(_arq_entrada);

    // Tamanho aproximando para cada arquivo
    iTamPorArquivo := iTamEntrada div iQuantProcessador;

    // Varre-se de tras pára frente para definir a afinidade do
    // último núcleo ao primeiro
    for iNumProcessador := iQuantProcessador downto 1 do
    begin
      // Zera a quantidade lida
      iTotalLido := 0;

      // Nome do arquivo de saída
      sNomeArquivoSaida := Format('.\arquivo_%d.csv', [iNumProcessador]);

      // Associa o arquivo de saída a um manipulador
      AssignFile(_arq_saida, sNomeArquivoSaida);

      // Coloca o arquivo em modo de escrita
      Rewrite(_arq_saida, 1);
      try
        while True do
        begin
          // Lê um bloco do arquivo de entrada
          BlockRead(_arq_entrada, aBuffer, MAXWORD, iTamBuffer);

          // Escreve este bloco no arquivo de saída
          BlockWrite(_arq_saida, aBuffer, iTamBuffer);

          // Finaliza se chegou ao fim do arquivo de entrada
          if Eof(_arq_entrada) then
          begin
            Exit;
          end;

          // Se estiver no último arquivo não precisa
          // mais verificações
          if (iNumProcessador = 1) then
          begin
            Continue;
          end;

          // Se já atingiu o tamanho proposto ...
          Inc(iTotalLido, iTamBuffer);
          if (iTotalLido >= iTamPorArquivo) then
          begin
            // Se o último caracter não for #10 continua lendo
            // byte a byte para finalizar um arquivo íntegro
            if (aBuffer[iTamBuffer] <> C_ASCII_10) then
            begin
              // Lê o arquivo continuamente até encontrar um #10
              repeat
                BlockRead(_arq_entrada, aBuffer, 1, iTamBuffer);
                BlockWrite(_arq_saida, aBuffer, iTamBuffer);
              until (aBuffer[1] = C_ASCII_10);
            end;

            // Finaliza o [while True do]
            Break;
          end;
        end;
      finally
        // Encerra o manipulador do arquivo de saída
        CloseFile(_arq_saida);

        // Determina a posição no array
        iPosArray := iNumProcessador - 1;
        // Cria um thread de importação passando o arquivo de saída
        Self.FImportadores[iPosArray] := TThreadImportacao.Create(sNomeArquivoSaida);
        // Define a afinade com o núcleo correspondente
        Self.FImportadores[iPosArray].DefinirAfinidade(iNumProcessador);
        // Aumenta a prioridade do thread
        Self.FImportadores[iPosArray].Priority := tpHigher;
        // Inicia o thread
        Self.FImportadores[iPosArray].Start;
      end;
    end;
  finally
    // Encerra o manipulador do arquivo de entrada
    CloseFile(_arq_entrada);
  end;
end;

procedure TThreadGerente.Execute;
var
  aHandles  : array [1 .. MAXBYTE] of THandle;
  _crono    : TStopwatch;
  iNumThread: Integer;
  cRet      : Cardinal;
begin
  inherited;
  _crono := TStopwatch.StartNew;
  try
    // Define a máscara de afinidade para o primeiro nucleo
    SetThreadAffinityMask(Self.Handle, 1);

    // Aumenta a prioridade do thread
    Self.Priority := tpHighest;

    // Esvazia a tabela
    Self.FEsvaziador := TThreadEsvaziarTabela.Create(True);
    Self.FEsvaziador.Start;

    WaitForSingleObject(Self.FEsvaziador.Handle, INFINITE);

    // Divide o arquivo gigante em partes e inicia o thread
    // de importação correspondente
    Self.DividirArquivo;

    // Baixa a prioridade
    Self.Priority := tpLowest;

    // Anota os manipuladores dos threads
    for iNumThread := 0 to High(Self.FImportadores) do
    begin
      aHandles[Succ(iNumThread)] := Self.FImportadores[iNumThread].Handle;
    end;

    // Aguarda o términos dos threads de importação
    cRet := WaitForMultipleObjects(Length(Self.FImportadores), @aHandles, True, INFINITE);

    // Determina a quantidade de linhas processadas
    for iNumThread := 0 to High(Self.FImportadores) do
    begin
      Inc(Self.FQuantidadeLinhas, Self.FImportadores[iNumThread].QuantidadeLinhas);
    end;

    // Força a liberação dos threads de importação
    Self.Terminate;
  finally
    _crono.Stop;
    Self.FTempoSegundos := _crono.ElapsedMilliseconds div 1000;
  end;

end;

procedure TThreadGerente.TerminatedSet;
var
  i: Integer;
begin
  inherited;

  // Libera o array de esvaziamento da tabela
  if Assigned(Self.FEsvaziador) then
  begin
    // Se NÃO estiver finalizada, indica o término
    if not(Self.FEsvaziador.Finished) then
    begin
      Self.FEsvaziador.Terminate;
      Self.FEsvaziador.WaitFor;
    end;

    // Libera a instância
    Self.FEsvaziador.Free;
  end;

  // Libera os arrays de importação
  for i := 0 to High(Self.FImportadores) do
  begin
    // Se estiver sinalizada ...
    if Assigned(Self.FImportadores[i]) then
    begin
      // Se NÃO estiver finalizada, indica o término
      if not(Self.FImportadores[i].Finished) then
      begin
        Self.FImportadores[i].Terminate;
        Self.FImportadores[i].WaitFor;
      end;

      // Libera a instância
      Self.FImportadores[i].Free;
    end;
  end;
end;

{ TThreadEsvaziarTabela }

procedure TThreadEsvaziarTabela.Execute;
var
  oDatabase: TSQLConnection;
begin
  inherited;

  oDatabase := TSQLConnection.Create(nil);
  try
    with oDatabase do
    begin
      LoginPrompt := False;
      DriverName := 'Firebird';
      Params.Values['Database'] := '192.168.0.132:MARIO';
      Params.Values['User_Name'] := 'sysdba';
      Params.Values['Password'] := 'masterkey';
      Open;
    end;

    oDatabase.ExecuteDirect('DELETE FROM NEW_TABLE');
  finally
    oDatabase.Close;
    oDatabase.Free;
  end;
end;

end.
