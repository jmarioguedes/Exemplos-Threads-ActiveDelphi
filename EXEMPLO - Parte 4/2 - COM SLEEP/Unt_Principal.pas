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
  Unt_Coletor,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  oTemp: TStringList;
  i    : Word;
begin
  for i := 1 to 50000 do
  begin
    oTemp := TStringList.Create;

    { ... Algo útil ... }

    Self.Caption := Format('Objeto de #%d',[i]);
    Application.ProcessMessages;

    TColetorDeLixo.ColetorDeLixo.ColocarNaPilha(oTemp);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  C_QUANT = 'Na fila: %d - Liberados: %d';
begin
  with TColetorDeLixo.ColetorDeLixo do
  begin
    Self.Label1.Caption := Format(C_QUANT, [QuantidadeFila, QuantidadeLiberada]);
  end;
end;

end.
