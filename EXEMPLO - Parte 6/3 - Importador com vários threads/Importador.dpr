program Importador;

uses
  Vcl.Forms,
  Unt_Principal in 'Unt_Principal.pas' {Form1},
  Unt_Importacao in 'Unt_Importacao.pas',
  Unt_Gerenciador in 'Unt_Gerenciador.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
