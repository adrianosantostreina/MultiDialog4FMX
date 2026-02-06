unit MultiDialog4FMX.Util;

interface

uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Factory;

type
  TMultiDialog4FMX = class
  public
    class function Dialog: IDialogBuilder; static;
  end;

implementation

{ TMultiDialog4FMX }

class function TMultiDialog4FMX.Dialog: IDialogBuilder;
begin
  Result := CreateDialog;
end;

end.
