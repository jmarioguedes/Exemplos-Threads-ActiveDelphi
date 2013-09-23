unit Unt_Importacao;

interface

uses
  System.Classes, PgAccess;

type

  TImportador = class(TThread)
  private
    FDatabase         : TPgConnection;
    FQuery            : TPgQuery;
    FInicio           : TDateTime;
    FFim              : TDateTime;
    FArquivo          : string;
    FProgressBarHandle: THandle;
    function GetTempoSegundos: Integer;
    procedure RegistrarNoBanco(ALinhaArquivo: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const ANomeArquivo: string; const AProgressBarHandle: THandle); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TempoSegundos: Integer read GetTempoSegundos;
  end;

implementation

uses
  DateUtils, System.SysUtils, Winapi.Windows, Winapi.Messages, Winapi.CommCtrl;

{ TImportador }

procedure TImportador.AfterConstruction;
begin
  inherited;
  Self.FDatabase := TPgConnection.Create(nil);
  Self.FQuery := TPgQuery.Create(nil);

  Self.FQuery.Connection := Self.FDatabase;
  Self.FQuery.SQL.Add('INSERT INTO mailing VALUES (');
  Self.FQuery.SQL.Add(':PID ,');
  Self.FQuery.SQL.Add(':PCLIENTE ,');
  Self.FQuery.SQL.Add(':PDATACOMPRA ,');
  Self.FQuery.SQL.Add(':PESTADO ,');
  Self.FQuery.SQL.Add(':PVALORCOMPRA ,');
  Self.FQuery.SQL.Add(':PNFE ,');
  Self.FQuery.SQL.Add(':POBSERVACOES ');
  Self.FQuery.SQL.Add(')');
end;

procedure TImportador.BeforeDestruction;
begin
  inherited;
  Self.FQuery.Free;
  Self.FDatabase.Close;
  Self.FDatabase.Free;
end;

constructor TImportador.Create(const ANomeArquivo: string; const AProgressBarHandle: THandle);
begin
  inherited Create(True);
  Self.FInicio := Now;
  Self.FArquivo := ANomeArquivo;
  Self.FProgressBarHandle := AProgressBarHandle;
end;

procedure TImportador.Execute;
var
  _arquivo: TextFile;
  sLinha  : string;
begin
  inherited;
  Self.NameThreadForDebugging(Self.FArquivo);

  Self.FDatabase.LoginPrompt := False;
  Self.FDatabase.Username := 'postgres';
  Self.FDatabase.Password := 'postgres';
  Self.FDatabase.Server := 'localhost';
  Self.FDatabase.Database := 'ActiveDelphi';
  Self.FDatabase.Open;

  Self.FQuery.Prepare;

  AssignFile(_arquivo, Self.FArquivo);
  try
    Reset(_arquivo);
    while not Eof(_arquivo) do
    begin
      Readln(_arquivo, sLinha);

      Self.RegistrarNoBanco(sLinha);

      SendMessage(Self.FProgressBarHandle, PBM_DELTAPOS, Length(sLinha) + 2, 0);
    end;
  finally
    CloseFile(_arquivo);
    Self.FDatabase.Disconnect;
    Self.FFim := Now;
  end;
end;

function TImportador.GetTempoSegundos: Integer;
begin
  Result := SecondsBetween(Self.FFim, Self.FInicio);
end;

procedure TImportador.RegistrarNoBanco(ALinhaArquivo: string);
type

  TInfoLinha = record
    ID: NativeUInt;
    NomeCliente: string[50];
    DataCompra: TDate;
    Estado: string[2];
    ValorCompra: Currency;
    ChaveNFE: string[44];
    Observacoes: string[27];
  end;

var
  rInfoLinha: TInfoLinha;
begin
  rInfoLinha.ID := StrToInt(Trim(Copy(ALinhaArquivo, 1, 7)));
  rInfoLinha.NomeCliente := Trim(Copy(ALinhaArquivo, 8, 100));
  rInfoLinha.DataCompra := StrToDate(Trim(Copy(ALinhaArquivo, 108, 10)));
  rInfoLinha.Estado := Trim(Copy(ALinhaArquivo, 118, 2));
  rInfoLinha.ValorCompra := StrToCurr(Trim(Copy(ALinhaArquivo, 120, 10)));
  rInfoLinha.ChaveNFE := Trim(Copy(ALinhaArquivo, 130, 44));
  rInfoLinha.Observacoes := Trim(Copy(ALinhaArquivo, 174, 27));

  Self.FQuery.ParamByName('PID').Value := rInfoLinha.ID;
  Self.FQuery.ParamByName('PCLIENTE').Value := rInfoLinha.NomeCliente;
  Self.FQuery.ParamByName('PDATACOMPRA').Value := rInfoLinha.DataCompra;
  Self.FQuery.ParamByName('PESTADO').Value := rInfoLinha.Estado;
  Self.FQuery.ParamByName('PVALORCOMPRA').Value := rInfoLinha.ValorCompra;
  Self.FQuery.ParamByName('PNFE').Value := rInfoLinha.ChaveNFE;
  Self.FQuery.ParamByName('POBSERVACOES').Value := rInfoLinha.Observacoes;
  Self.FQuery.ExecSQL;
end;

end.
