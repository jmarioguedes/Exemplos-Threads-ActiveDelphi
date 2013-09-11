unit Unt_DiversasThreads;

interface

uses
  System.Classes,
  Unt_FuncaoLog;

type

  /// <summary>
  /// Thread que disputará o recurso de geração de LOG
  /// </summary>
  TDiversaThread = class(TThread)
  private
    FReferencia   : string;
    FDescricaoErro: string;
  public
    /// <summary>
    /// Rotina a ser executada pelo thread que eventualmente
    /// gerará uma linha no arquivo de LOG
    /// </summary>
    procedure Execute; override;
    /// <summary>
    /// Referência que será escrito no arquivo
    /// de LOG para sabermos de onde veio a linha
    /// </summary>
    property Referencia: string read FReferencia write FReferencia;
    /// <summary>
    /// Caso ocorra um erro durante a execução do thread
    /// o erro poderá ser consultado nesta propriedade
    /// </summary>
    property DescricaoErro: string read FDescricaoErro;
  end;

implementation

uses
  System.SysUtils;

{ TDiversaThread }

procedure TDiversaThread.Execute;
var
  bGerarLog: Boolean;
begin
  inherited;
  try
    // Loop enquanto o thread não for finalizado
    while not(Self.Terminated) do
    begin
      //Faz com que não haja um consumo elevado de CPU
      Sleep(10);

      //Sorteia um número e verifica se o resto da divisão por dois é zero
      bGerarLog := (Random(1000000) mod 2) = 0;

      if (bGerarLog) then
      begin
        //Invoca o método de geração de LOG
        TGeraLog.Instancia.GerarLog(Self.FReferencia, Now, 'O rato roeu a roupa do Rei de Roma');
      end;
    end;
  except
    on E: EInOutError do
    begin
      Self.FDescricaoErro := Format('Erro de I/O #%d - %s', [E.ErrorCode, SysErrorMessage(E.ErrorCode)]);
    end;
    on E: Exception do
    begin
      Self.FDescricaoErro := Format('(%s) - %s', [E.ClassName, E.Message]);
    end;
  end;
end;

end.
