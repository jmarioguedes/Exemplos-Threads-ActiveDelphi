program GeradorLOG;

uses
  Vcl.Forms,
  Unt_Principal in 'Unt_Principal.pas' {Form1},
  Unt_FuncaoLog in 'Unt_FuncaoLog.pas',
  Unt_DiversasThreads in 'Unt_DiversasThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
