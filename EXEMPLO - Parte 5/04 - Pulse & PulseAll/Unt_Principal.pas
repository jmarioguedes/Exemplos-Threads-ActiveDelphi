unit Unt_Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type

  TPreencheMemo = class(TThread)
  private
    FBufferTexto: string;
    FMemo       : TMemo;
    FLabel      : TLabel;
    procedure EscreverMemo;
    procedure EscreverLabel;
  protected
    procedure Execute; override;
  public
    constructor Create(AMemo: TMemo; ALabel: TLabel);
  end;

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FConsumidores: array [1 .. 3] of TPreencheMemo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{ TPreencheMemo }

constructor TPreencheMemo.Create(AMemo: TMemo; ALabel: TLabel);
begin
  inherited Create(True);
  Self.FMemo := AMemo;
  Self.FLabel := ALabel;
end;

procedure TPreencheMemo.EscreverLabel;
begin
  Self.FLabel.Caption := Self.FBufferTexto;
end;

procedure TPreencheMemo.EscreverMemo;
var
  sLinha    : string;
  sHoraAtual: string;
begin
  sHoraAtual := TimeToStr(Time());
  sLinha := Format('%s - %s', [sHoraAtual, Self.FBufferTexto]);
  Self.FMemo.Lines.Insert(0, sLinha);
end;

procedure TPreencheMemo.Execute;
var
  bRet: Boolean;
begin
  inherited;
  while not(Self.Terminated) do
  begin

    Self.FBufferTexto := 'Enter ...';
    Self.Synchronize(Self.EscreverLabel);
    System.TMonitor.Enter(Form1.Edit1);

    try

      Self.FBufferTexto := 'Wait ...';
      Self.Synchronize(Self.EscreverLabel);
      bRet := System.TMonitor.Wait(Form1.Edit1, INFINITE);
      if (bRet) then
      begin
        Self.FBufferTexto := Form1.Edit1.Text;
        Self.Synchronize(Self.EscreverMemo);
      end;

    finally

      Self.FBufferTexto := 'Exit ...';
      Self.Synchronize(Self.EscreverLabel);
      System.TMonitor.Exit(Form1.Edit1);

    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  System.TMonitor.Pulse(Self.Edit1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  System.TMonitor.PulseAll(Self.Edit1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Self.FConsumidores[1] := TPreencheMemo.Create(Self.Memo1, Self.Label1);
  Self.FConsumidores[1].Start;
  Self.Button3.Enabled := False;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Self.FConsumidores[2] := TPreencheMemo.Create(Self.Memo2, Self.Label2);
  Self.FConsumidores[2].Start;
  Self.Button4.Enabled := False;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Self.FConsumidores[3] := TPreencheMemo.Create(Self.Memo3, Self.Label3);
  Self.FConsumidores[3].Start;
  Self.Button5.Enabled := False;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  oThread: TPreencheMemo;
begin
  for oThread in Self.FConsumidores do
  begin
    if (Assigned(oThread)) then
    begin
      if not(oThread.Finished) then
      begin
        oThread.Terminate;
      end;
    end;
  end;

  System.TMonitor.PulseAll(Self.Edit1);

  for oThread in Self.FConsumidores do
  begin
    if (Assigned(oThread)) then
    begin
      if not(oThread.Finished) then
      begin
        oThread.WaitFor;
      end;
      oThread.Free;
    end;
  end;
end;

end.
