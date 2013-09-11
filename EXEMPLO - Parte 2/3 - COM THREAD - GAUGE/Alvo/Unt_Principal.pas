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
  Vcl.ComCtrls,
  Vcl.Samples.Gauges,
  Vcl.ExtCtrls;

type

  TLoopThread = class(TThread)
  private
    FTempoTranscorrido: Integer;
    procedure ZerarProgressBar;
    procedure IncrementarProgressBar;
  protected
    procedure Execute; override;
  public
    property TempoTranscorrido: Integer read FTempoTranscorrido;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    btnLoop: TButton;
    btnSeep: TButton;
    RadioGroup1: TRadioGroup;
    Gauge1: TGauge;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnLoopClick(Sender: TObject);
    procedure btnSeepClick(Sender: TObject);
  private
    FQuantidade: Integer;
    procedure EventOnTerminate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Diagnostics;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Inc(Self.FQuantidade);
  Self.Memo1.Lines.Insert(0, Format('Botão acionado! [%d]', [Self.FQuantidade]));
end;

procedure TForm1.btnLoopClick(Sender: TObject);
var
  oThread: TLoopThread;
begin
  Self.btnLoop.Enabled := False;

  oThread := TLoopThread.Create(True);
  oThread.OnTerminate := Self.EventOnTerminate;
  oThread.FreeOnTerminate := True;
  oThread.Start;
end;

procedure TForm1.btnSeepClick(Sender: TObject);
begin
  Sleep(5000);
end;

procedure TForm1.EventOnTerminate(Sender: TObject);
var
  iTempo: Integer;
begin
  iTempo := TLoopThread(Sender).TempoTranscorrido;
  ShowMessage(Format('Loop executado em [%d] segundos', [iTempo]));
  Self.btnLoop.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Button1.Caption := IntToStr(Self.Button1.Handle);
end;

{ TLoopThread }

procedure TLoopThread.Execute;
var
  rCronus: TStopwatch;
  i      : Integer;
  iTempo : Integer;
begin
  Self.FTempoTranscorrido := -1;
  rCronus := TStopwatch.StartNew;

  case Form1.RadioGroup1.ItemIndex of
    0:
      Self.ZerarProgressBar;
    1:
      Self.Synchronize(Self.ZerarProgressBar);
    2:
      Self.Queue(Self.ZerarProgressBar);
  end;

  for i := 1 to 15 do
  begin
    Sleep(1000);
    case Form1.RadioGroup1.ItemIndex of
      0:
        Self.IncrementarProgressBar;
      1:
        Self.Synchronize(Self.IncrementarProgressBar);
      2:
        Self.Queue(Self.IncrementarProgressBar);
    end;
  end;

  rCronus.Stop;
  iTempo := (rCronus.ElapsedMilliseconds div 1000);
  Self.FTempoTranscorrido := iTempo;
end;

procedure TLoopThread.IncrementarProgressBar;
begin
  Form1.Gauge1.AddProgress(1);
end;

procedure TLoopThread.ZerarProgressBar;
begin
  Form1.Gauge1.MaxValue := 15;
  Form1.Gauge1.Progress := 0;
end;

end.
