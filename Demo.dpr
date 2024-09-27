program Demo;

uses
  Vcl.Forms,
  UTeste in 'UTeste.pas' {Form1},
  TaskScheduler in 'TaskScheduler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
