unit MultiDialog4FMX.Util;

interface

uses
  MultiDialog4FMX.Interfaces;

type
  TMultiDialog4FMX = class
  public
    class function Dialog: IDialogBuilder; static;
  end;

implementation

uses
  MultiDialog4FMX.Factory;

class function TMultiDialog4FMX.Dialog: IDialogBuilder;
begin
  Result := CreateDialog;
end;

end.
