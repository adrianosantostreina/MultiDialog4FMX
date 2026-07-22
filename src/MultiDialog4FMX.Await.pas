unit MultiDialog4FMX.Await;

interface

uses
  System.SysUtils;

type
  /// <summary>Levantada quando ShowAndWait e chamado na main/UI thread (deadlock).</summary>
  EDialogAwaitOnMainThread = class(Exception);

procedure EnsureAwaitNotOnMainThread;

implementation

uses
  System.Classes;

procedure EnsureAwaitNotOnMainThread;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    raise EDialogAwaitOnMainThread.Create(
      'ShowAndWait n'#227'o pode ser chamado na main thread (deadlock). ' +
      'Use dentro de TTask.Run/TThread, ou use Show + SetOnResult na UI thread.');
end;

end.
