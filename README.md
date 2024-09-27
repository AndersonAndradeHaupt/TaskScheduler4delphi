# Interface TaskScheduler para Delphi

Este projeto fornece uma interface simples para agendamento de tarefas em Delphi. A interface `TaskScheduler` permite que você agende tarefas para serem executadas em um horário específico em determinados dias da semana, de forma semelhante à funcionalidade da biblioteca `schedule` do Python.

## Funcionalidades

- Agendar tarefas para serem executadas todos os dias em um horário específico.
- Especificar os dias da semana em que a tarefa deve ser executada.
- Interface simples e clara, utilizando encadeamento de métodos para facilitar o uso.

## Instalação

Para utilizar o agendador em seu projeto Delphi, siga os passos abaixo:

1. Baixe ou clone este repositório.
2. Inclua a unit `TaskScheduler.pas` em seu projeto.

## Uso

### Exemplo Básico

Veja como agendar uma tarefa para ser executada todos os dias às 14:30:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  FTaskScheduler
    .EveryDayAt('14:30', 
      procedure
      begin
        ShowMessage('Executando tarefa às 14:30!');
      end
    )
    .Start;
end;
