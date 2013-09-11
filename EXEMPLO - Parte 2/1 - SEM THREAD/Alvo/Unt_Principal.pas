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
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    btnLoop: TButton;
    ProgressBar1: TProgressBar;
    btnSleep: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnLoopClick(Sender: TObject);
    procedure btnSleepClick(Sender: TObject);
  private
    FQuantidade: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Diagnostics;

{$R *.dfm}

procedure TForm1.btnSleepClick(Sender: TObject);
begin
  Sleep(5000);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.Memo1.Lines.Insert(0, Format('Botão acionado! [%d]', [Self.FQuantidade]));
  Inc(Self.FQuantidade);
end;

procedure TForm1.btnLoopClick(Sender: TObject);
var
  i      : Integer;
  rCronus: TStopwatch;
  iTempo : Integer;
begin
  Screen.Cursor := crHourGlass;
  rCronus := TStopwatch.StartNew;

  Self.ProgressBar1.Max := 15;
  Self.ProgressBar1.Position := 0;
  for i := 1 to 15 do
  begin
    Sleep(1000);
    Self.ProgressBar1.StepBy(1);
  end;

  rCronus.Stop;
  Screen.Cursor := crDefault;

  iTempo := rCronus.ElapsedMilliseconds div 1000;
  Application.ProcessMessages;
  ShowMessage(Format('Processado em [%d] segundos', [iTempo]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Button1.Caption := IntToStr(Self.Button1.Handle);
end;

end.
