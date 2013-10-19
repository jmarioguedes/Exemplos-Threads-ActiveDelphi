unit Unt_Gerenciador;

interface

uses
  System.Classes, PgAccess;

type

  TGerenciador = class(TThread)
  private
    FDatabase         : TPgConnection;
    FInicio           : TDateTime;
    FFim              : TDateTime;
    FNomeArquivo      : string;
    FProgressBarHandle: THandle;
    function QuantidadeLinhasArquivo: NativeUInt;
    function GetTempoSegundos: NativeUInt;
    procedure LimparTabelaNoBanco;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANomeArquivo: string; const AProgressBarHandle: THandle); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TempoSegundos: NativeUInt read GetTempoSegundos;
    property NomeArquivo: string read FNomeArquivo;
    property ProgressBarHandle: THandle read FProgressBarHandle;
  end;

implementation

uses
  System.SysUtils, DateUtils, Winapi.Windows, Unt_Importacao;

{ TGerenciador }

procedure TGerenciador.LimparTabelaNoBanco;
var
  qLimpar: TPgQuery;
begin
  qLimpar := TPgQuery.Create(nil);
  try
    qLimpar.Connection := Self.FDatabase;
    qLimpar.SQL.Text := 'DELETE FROM mailing';
    qLimpar.ExecSQL;
  finally
    qLimpar.Free;
  end;
end;

procedure TGerenciador.AfterConstruction;
begin
  inherited;
  Self.FDatabase := TPgConnection.Create(nil);
end;

procedure TGerenciador.BeforeDestruction;
begin
  inherited;
  Self.FDatabase.Close;
  Self.FDatabase.Free;
end;

constructor TGerenciador.Create(const ANomeArquivo: string; const AProgressBarHandle: THandle);
begin
  inherited Create(True);
  Self.FInicio := Now;
  Self.FNomeArquivo := ANomeArquivo;
  Self.FProgressBarHandle := AProgressBarHandle;
end;

procedure TGerenciador.Execute;
var
  _arquivoOriginal  : TextFile;
  _arquivoBuffer    : TextFile;
  sNomeArquivoBuffer: string;
  sDirArquivo       : string;
  sLinhaArquivo     : string;

  iQuantidadeLinhas         : NativeUInt;
  iQuantidadeNucleo         : Byte;
  iQuantidadeLinhaPorArquivo: NativeUInt;
  iItemNucleo               : Byte;
  iNumeroLinha              : NativeUInt;

  aThreads      : TArray<TImportador>;
  aImportadores : array [1 .. 255] of THandle;
  iRetornoEspera: DWORD;
begin
  inherited;
  try
    Self.FDatabase.LoginPrompt := False;
    Self.FDatabase.Username := 'postgres';
    Self.FDatabase.Password := 'postgres';
    Self.FDatabase.Server := 'localhost';
    Self.FDatabase.Database := 'ActiveDelphi';
    Self.FDatabase.Open;

    Self.LimparTabelaNoBanco;

    sDirArquivo := ExtractFilePath(Self.FNomeArquivo);

    iQuantidadeLinhas := Self.QuantidadeLinhasArquivo;
    iQuantidadeNucleo := TThread.ProcessorCount;
    iQuantidadeLinhaPorArquivo := iQuantidadeLinhas div iQuantidadeNucleo;

    SetLength(aThreads, iQuantidadeNucleo);

    AssignFile(_arquivoOriginal, Self.FNomeArquivo);
    Reset(_arquivoOriginal);

    for iItemNucleo := 1 to iQuantidadeNucleo do
    begin
      sNomeArquivoBuffer := Format('%sBUFFER_%d.txt', [sDirArquivo, iItemNucleo]);
      AssignFile(_arquivoBuffer, sNomeArquivoBuffer);
      Rewrite(_arquivoBuffer);
      iNumeroLinha := 0;

      while not(Eof(_arquivoOriginal)) do
      begin
        Readln(_arquivoOriginal, sLinhaArquivo);
        Writeln(_arquivoBuffer, sLinhaArquivo);
        Inc(iNumeroLinha);

        if (iItemNucleo < iQuantidadeNucleo) then
        begin
          if (iNumeroLinha = iQuantidadeLinhaPorArquivo) then
          begin
            Break;
          end;
        end;
      end;

      CloseFile(_arquivoBuffer);

      aThreads[iItemNucleo - 1] := TImportador.Create(Self);
      aThreads[iItemNucleo - 1].FreeOnTerminate := True;
    end;
    CloseFile(_arquivoOriginal);

    for iItemNucleo := 1 to iQuantidadeNucleo do
    begin
      aThreads[iItemNucleo - 1].Start;
      aImportadores[iItemNucleo] := aThreads[iItemNucleo - 1].Handle;
    end;

    iRetornoEspera := WaitForMultipleObjects(iQuantidadeNucleo, @aImportadores, True, INFINITE);

  finally
    Self.FFim := Now;
  end;
end;

function TGerenciador.GetTempoSegundos: NativeUInt;
begin
  Result := SecondsBetween(Self.FFim, Self.FInicio);
end;

function TGerenciador.QuantidadeLinhasArquivo: NativeUInt;
const
  C_QUANTIDADE_CARACTERES_LINHA = 200 + 2;
var
  hHandleArquivo         : NativeUInt;
  iQuantidadeBytesArquivo: NativeUInt;
begin
  hHandleArquivo := FileOpen(Self.FNomeArquivo, fmOpenRead);
  iQuantidadeBytesArquivo := FileSeek(hHandleArquivo, 0, 2);
  Result := iQuantidadeBytesArquivo div C_QUANTIDADE_CARACTERES_LINHA;
  FileClose(hHandleArquivo);
end;

end.
