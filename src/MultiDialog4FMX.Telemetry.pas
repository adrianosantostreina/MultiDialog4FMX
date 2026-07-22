unit MultiDialog4FMX.Telemetry;

interface

uses
  MultiDialog4FMX.Interfaces;

type
  /// <summary>Dispatcher global de eventos de ciclo de vida do dialogo.</summary>
  TDialogTelemetry = class
  private
    class var FOnEvent: TDialogEventProc;
  public
    class property OnEvent: TDialogEventProc read FOnEvent write FOnEvent;
    class procedure Emit(const AInfo: TDialogEventInfo); static;
  end;

implementation

class procedure TDialogTelemetry.Emit(const AInfo: TDialogEventInfo);
begin
  if Assigned(FOnEvent) then
    FOnEvent(AInfo);
end;

end.
