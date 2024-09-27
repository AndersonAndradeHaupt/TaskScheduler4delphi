unit TaskScheduler;

interface

uses
  System.SysUtils, System.Classes, Vcl.ExtCtrls, System.DateUtils, Vcl.Forms, Vcl.Dialogs;

type

  TDayOfWeek = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
  TWeekDays = set of TDayOfWeek;

  ITaskScheduler = interface
    ['{D7F1CCF0-4535-47E8-AE1E-51FBFE8AB2E7}']
    function EveryDayAt(const TimeStr: string; Task: TProc): ITaskScheduler;
    function OnDays(Days: TWeekDays): ITaskScheduler;
    function Start: ITaskScheduler;
    function Stop: ITaskScheduler;
  end;

  TTaskScheduler = class(TInterfacedObject, ITaskScheduler)
  private
    FTimer: TTimer;
    FTask: TProc;
    FRunTime: TTime;
    FDays: TWeekDays;
    procedure OnTimer(Sender: TObject);
    function GetCurrentDayOfWeek: TDayOfWeek;
  public
    constructor Create;
    destructor Destroy; override;
    function EveryDayAt(const TimeStr: string; Task: TProc): ITaskScheduler;
    function OnDays(Days: TWeekDays): ITaskScheduler;
    function Start: ITaskScheduler;
    function Stop: ITaskScheduler;

    class function New: ITaskScheduler;
  end;

implementation

{ TTaskScheduler }

constructor TTaskScheduler.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnTimer;
  FDays := [Sun..Sat];
end;

destructor TTaskScheduler.Destroy;
begin
  FTimer.Free;
  inherited;
end;

function TTaskScheduler.EveryDayAt(const TimeStr: string; Task: TProc): ITaskScheduler;
begin
  FTask := Task;
  FRunTime := StrToTime(TimeStr);
  Result := Self;
end;

function TTaskScheduler.OnDays(Days: TWeekDays): ITaskScheduler;
begin
  FDays := Days;
  Result := Self;
end;

function TTaskScheduler.GetCurrentDayOfWeek: TDayOfWeek;
begin
  case DayOfWeek(Now) of
    1: Result := Sun;
    2: Result := Mon;
    3: Result := Tue;
    4: Result := Wed;
    5: Result := Thu;
    6: Result := Fri;
    7: Result := Sat;
  else
    raise Exception.Create('Dia da semana inválido');
  end;
end;

procedure TTaskScheduler.OnTimer(Sender: TObject);
var
  CurrentTime: TTime;
  CurrentDay: TDayOfWeek;
begin
  CurrentTime := TimeOf(Now);
  CurrentDay := GetCurrentDayOfWeek;


  if (CurrentDay in FDays) and (SecondsBetween(CurrentTime, FRunTime) = 0) then
  begin
    if Assigned(FTask) then
    begin
      FTask();
      FTimer.Enabled := False;
    end
    else
    begin
      ShowMessage('FTask não está atribuída durante a execução.');
    end;
  end;
end;

function TTaskScheduler.Start: ITaskScheduler;
begin
  FTimer.Enabled := True;
  Result := Self;
end;

function TTaskScheduler.Stop: ITaskScheduler;
begin
  FTimer.Enabled := False;
  Result := Self;
end;

class function TTaskScheduler.New: ITaskScheduler;
begin
  Result := TTaskScheduler.Create;
end;

end.

