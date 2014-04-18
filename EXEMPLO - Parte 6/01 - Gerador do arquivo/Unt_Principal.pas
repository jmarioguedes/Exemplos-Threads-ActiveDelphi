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
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
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
const
  C_TOTAL_LINHA = 10000;
var
  _arquivo   : TextFile;
  slLinha    : TStringList;
  i          : Integer;
  sLinha     : string;
  iTotalLinha: Integer;
begin
  Screen.Cursor := crHourGlass;
  slLinha := TStringList.Create;
  Self.ProgressBar1.Position := 0;
  Self.ProgressBar1.Step := 1;
  Self.ProgressBar1.Max := C_TOTAL_LINHA;

  try
    AssignFile(_arquivo, '.\arquivo.csv');
    Rewrite(_arquivo);

    for i := 1 to C_TOTAL_LINHA do
    begin
      slLinha.Clear;
      slLinha.Add(IntToStr(i));
      slLinha.Add(StringOfChar('n', Random(100)));
      slLinha.Add(StringOfChar('9', 14));
      slLinha.Add(StringOfChar('c', 14));
      slLinha.Add(StringOfChar('8', 11));
      slLinha.Add(StringOfChar('7', 11));
      slLinha.Add(StringOfChar('a', 20) + '@teste.com.br');

      sLinha := slLinha.CommaText;
      Writeln(_arquivo, sLinha);

      Self.ProgressBar1.StepIt;
    end;
  finally
    CloseFile(_arquivo);
    slLinha.Free;
    Screen.Cursor := crDefault;
  end;
end;

end.
