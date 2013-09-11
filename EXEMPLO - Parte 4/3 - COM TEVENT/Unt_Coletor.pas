unit Unt_Coletor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs;

type

  /// <summary>
  /// Classe singleton com o propósito de liberar instâncias de objetos
  /// </summary>
  TColetorDeLixo = class(TThread)
  private
    /// <summary>
    /// Instância única desta classe
    /// </summary>
    class var FColetorDeLixo: TColetorDeLixo;
    /// <summary>
    /// Liberador da instância única desta classe
    /// </summary>
    class procedure ReleaseInstance;
    /// <summary>
    /// Instanciador da classe
    /// </summary>
    class function GetInstance: TColetorDeLixo; static;
  private
    /// <summary>
    /// Enfilerador dos objetos (FIFO) que serão liberados
    /// </summary>
    FFila: TObjectQueue<TObject>;
    /// <summary>
    /// Sinalizador para o thread
    /// </summary>
    FSinalizador: TEvent;
    /// <summary>
    /// Seção crítica para a fila de objetos
    /// </summary>
    FSecaoCritica: TCriticalSection;
    /// <summary>
    /// Quantidade de objetos liberados
    /// </summary>
    FQuantidadeLiberada: NativeUInt;
    /// <summary>
    /// Retorna a quantidade de objetos ainda a serem liberados
    /// </summary>
    function GetQuantidadeFila: NativeUInt;
    /// <summary>
    /// Processamento efetivo da fila de objetos
    /// </summary>
    procedure ProcessarFila;
  protected
    /// <summary>
    ///   Aciona o evento para evitar deixar o programa zumbi
    /// </summary>
    procedure TerminatedSet; override;
  public
    /// <summary>
    /// Aloca os recursos necessários para o funcionamento da classe
    /// </summary>
    procedure AfterConstruction; override;
    /// <summary>
    /// Desaloca os recursos
    /// </summary>
    procedure BeforeDestruction; override;
    /// <summary>
    /// Coloca um objeto na pilha, será invocada pelos outros threads
    /// </summary>
    procedure ColocarNaPilha(AObjeto: TObject);
    /// <summary>
    /// Rotina que será, efetivamente executado pelo thread
    /// </summary>
    procedure Execute; override;
    /// <summary>
    /// Exposição da instância única desta classe
    /// </summary>
    class property ColetorDeLixo: TColetorDeLixo read GetInstance;
    /// <summary>
    /// Indica a quantidade de objetos na fila
    /// </summary>
    property QuantidadeFila: NativeUInt read GetQuantidadeFila;
    /// <summary>
    /// Indica a quantidade de objetos já liberado
    /// </summary>
    property QuantidadeLiberada: NativeUInt read FQuantidadeLiberada;
  end;

implementation

uses
  System.SysUtils;

{ TExemploThread }

procedure TColetorDeLixo.AfterConstruction;
begin
  inherited;
  Self.FSinalizador := TEvent.Create(nil, False, True, '_sinalizador');
  Self.FQuantidadeLiberada := 0;
  Self.FSecaoCritica := TCriticalSection.Create;
  Self.FFila := TObjectQueue<TObject>.Create(True);
end;

procedure TColetorDeLixo.BeforeDestruction;
begin
  inherited;
  Self.FSinalizador.Free;
  Self.FSecaoCritica.Free;
  Self.FFila.Free;
end;

procedure TColetorDeLixo.ColocarNaPilha(AObjeto: TObject);
begin
  Self.FSecaoCritica.Enter;
  try
    Self.FFila.Enqueue(AObjeto);
    Self.FSinalizador.SetEvent;
  finally
    Self.FSecaoCritica.Release;
  end;
end;

procedure TColetorDeLixo.Execute;
var
  eWait: TWaitResult;
begin
  inherited;
  while not(Self.Terminated) do
  begin
    eWait := Self.FSinalizador.WaitFor(INFINITE);

    case eWait of
      wrSignaled:
        begin
          Self.ProcessarFila;
        end;
      wrTimeout:
        ;
      wrAbandoned:
        ;
      wrError:
        ;
      wrIOCompletion:
        ;
    end;

  end;
end;

class function TColetorDeLixo.GetInstance: TColetorDeLixo;
begin
  if not(Assigned(FColetorDeLixo)) then
  begin
    FColetorDeLixo := TColetorDeLixo.Create(True);
    FColetorDeLixo.Start;
  end;
  Result := FColetorDeLixo;
end;

function TColetorDeLixo.GetQuantidadeFila: NativeUInt;
begin
  Result := Self.FFila.Count;
end;

procedure TColetorDeLixo.ProcessarFila;
var
  i          : Integer;
  oTemp      : TObject;
begin
  Self.FSecaoCritica.Enter;
  try
    for i := 1 to Self.FFila.Count do
    begin
      oTemp := Self.FFila.Extract;

      oTemp.Free;
      Inc(Self.FQuantidadeLiberada);
    end;
  finally
    Self.FSecaoCritica.Release;
  end;

  // Demora artificial para verificarmos as quantidades
  Sleep(250);
end;

class procedure TColetorDeLixo.ReleaseInstance;
begin
  if (Assigned(FColetorDeLixo)) then
  begin
    FColetorDeLixo.Terminate;
    FColetorDeLixo.WaitFor;
    FColetorDeLixo.Free;
    FColetorDeLixo := nil;
  end;
end;

procedure TColetorDeLixo.TerminatedSet;
begin
  inherited;
  Self.FSinalizador.SetEvent;
end;

initialization

finalization

TColetorDeLixo.ReleaseInstance;

end.
