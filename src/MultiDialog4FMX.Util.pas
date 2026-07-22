unit MultiDialog4FMX.Util;

interface

uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,
  MultiDialog4FMX.Factory;

type
  TMultiDialog4FMX = class
  private
    class function GetOnDialogEvent: TDialogEventProc; static;
    class procedure SetOnDialogEvent(const AValue: TDialogEventProc); static;
  public
    class function Dialog: IDialogBuilder; static;
    class property OnDialogEvent: TDialogEventProc
      read GetOnDialogEvent write SetOnDialogEvent;
  end;

implementation

{ TMultiDialog4FMX }

class function TMultiDialog4FMX.Dialog: IDialogBuilder;
begin
  Result := CreateDialog;
end;

class function TMultiDialog4FMX.GetOnDialogEvent: TDialogEventProc;
begin
  Result := TDialogTelemetry.OnEvent;
end;

class procedure TMultiDialog4FMX.SetOnDialogEvent(const AValue: TDialogEventProc);
begin
  TDialogTelemetry.OnEvent := AValue;
end;

end.
