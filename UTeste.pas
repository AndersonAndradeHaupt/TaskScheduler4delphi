unit UTeste;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,TaskScheduler;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
   procedure Mensagem;
    { Private declarations }
  public
    { Public declarations }
       FTaskScheduler: ITaskScheduler;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
begin

  FTaskScheduler
   .EveryDayAt('15:45:00', Mensagem )
    .OnDays([Fri])
    .Start;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTaskScheduler := TTaskScheduler.New;
end;

procedure TForm1.Mensagem;
begin
  Memo1.Lines.Add('Tarefa executada');
end;

end.
