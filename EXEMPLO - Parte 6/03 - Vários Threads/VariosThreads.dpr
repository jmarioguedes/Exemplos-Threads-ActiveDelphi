program VariosThreads;

uses
  Vcl.Forms,
  Unt_Principal in 'Unt_Principal.pas' {fImportacao};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfImportacao, fImportacao);
  Application.Run;
end.
