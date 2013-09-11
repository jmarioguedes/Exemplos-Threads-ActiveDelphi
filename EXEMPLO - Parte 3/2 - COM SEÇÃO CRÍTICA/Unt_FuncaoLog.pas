unit Unt_FuncaoLog;

interface

uses
  System.SyncObjs;

type

  /// <summary>
  /// Classe responsável pela geração de LOG
  /// </summary>
  TGeraLog = class
  private
  /// <summary>
  /// Indica o arquivo em que o LOG será gerado, no caso, no mesmo diretório do executável
  /// </summary>
    const
    C_ARQUIVO = '.\arquivo_log.txt';
  private
    /// <summary>
    /// Seção crítica para o acesso ao arquivo de log
    /// </summary>
    FCritical: TCriticalSection;
    /// <summary>
    /// Instância única do objeto
    /// </summary>
    class var FInstance: TGeraLog;
    /// <summary>
    /// Método responsável por instanciar o objeto singleton
    /// </summary>
    class function GetInstancia: TGeraLog; static;
  public
    /// <summary>
    /// Deleta o arquivo de LOG caso o mesma exista e cria a seção crítica
    /// </summary>
    procedure AfterConstruction; override;
    /// <summary>
    /// Libera a seção crítica
    /// </summary>
    procedure BeforeDestruction; override;
    /// <summary>
    /// Método que efetivamente gera o LOG
    /// </summary>
    /// <param name="AReferencia">Referência à thread chamadora</param>
    /// <param name="AData">Date e hora da geração do LOG</param>
    /// <param name="ATextoLog">Texto a ser escrito no arquivo de log</param>
    procedure GerarLog(const AReferencia: string; const AData: TDateTime; const ATextoLog: string);
    /// <summary>
    /// Método que TENTA gerar uma nova linha de LOG
    /// </summary>
    /// <param name="AReferencia">Referência à thread chamadora</param>
    /// <param name="AData">Date e hora da geração do LOG</param>
    /// <param name="ATextoLog">Texto a ser escrito no arquivo de log</param>
    function TryGerarLog(const AReferencia: string; const AData: TDateTime; const ATextoLog: string): Boolean;
    /// <summary>
    /// Referência à instância singleton
    /// </summary>
    class property Instancia: TGeraLog read GetInstancia;
  end;

implementation

uses
  System.SysUtils;

{ TGeraLog }

procedure TGeraLog.AfterConstruction;
begin
  inherited;
  DeleteFile(Self.C_ARQUIVO);
  Self.FCritical := TCriticalSection.Create;
end;

procedure TGeraLog.BeforeDestruction;
begin
  inherited;
  Self.FCritical.Free;
end;

procedure TGeraLog.GerarLog(const AReferencia: string; const AData: TDateTime; const ATextoLog: string);
var
  _arquivo  : TextFile;
  sNovaLinha: string;
begin
  sNovaLinha := Format('%s|%s|%s', [AReferencia, DateTimeToStr(AData), ATextoLog]);

  // Entra na seção crítica
  Self.FCritical.Enter;
  try
    AssignFile(_arquivo, Self.C_ARQUIVO);
    if (FileExists(Self.C_ARQUIVO)) then
    begin
      Append(_arquivo);
    end else begin
      Rewrite(_arquivo);
    end;

    Writeln(_arquivo, sNovaLinha);

    CloseFile(_arquivo);
  finally
    // Sai da seção crítica
    Self.FCritical.Release;
  end;
end;

class function TGeraLog.GetInstancia: TGeraLog;
begin
  if not(Assigned(FInstance)) then
  begin
    FInstance := TGeraLog.Create;
  end;
  Result := FInstance;
end;

function TGeraLog.TryGerarLog(const AReferencia: string; const AData: TDateTime; const ATextoLog: string): Boolean;
var
  _arquivo  : TextFile;
  sNovaLinha: string;
begin
  sNovaLinha := Format('%s|%s|%s', [AReferencia, DateTimeToStr(AData), ATextoLog]);

  // Tenta entrar na seção crítica
  Result := Self.FCritical.TryEnter;
  if (Result) then
  begin
    try
      AssignFile(_arquivo, Self.C_ARQUIVO);
      if (FileExists(Self.C_ARQUIVO)) then
      begin
        Append(_arquivo);
      end else begin
        Rewrite(_arquivo);
      end;

      Writeln(_arquivo, sNovaLinha);

      CloseFile(_arquivo);
    finally
      // Sai da seção crítica
      Self.FCritical.Release;
    end;
  end;
end;

initialization

finalization

TGeraLog.Instancia.Free;

end.
