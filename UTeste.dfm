object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TaskScheduler - Agendador de Tarefas'
  ClientHeight = 251
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 289
    Height = 113
    Caption = 'Agendar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 152
    Width = 289
    Height = 89
    TabOrder = 1
  end
end
