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
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  hBotaoAlvo: NativeUInt;
begin
  hBotaoAlvo := StrToIntDef(Self.Edit1.Text, 0);
  PostMessage(hBotaoAlvo, BM_CLICK, 0, 0);
  ShowMessage('Comando enviado!');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  hBotaoAlvo: NativeUInt;
begin
  hBotaoAlvo := StrToIntDef(Self.Edit1.Text, 0);
  SendMessage(hBotaoAlvo, BM_CLICK, 0, 0);
  ShowMessage('Comando enviado!');
end;

end.
