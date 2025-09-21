unit MultiDialog4FMX.Util;

interface

uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Base;

type
  TMultiDialog4FMX = class
  public
    class function Dialog: IDialogBuilder; static;
  end;

implementation

uses
{$IFDEF ANDROID}
  MultiDialog4FMX.Android
{$ENDIF}
{$IFDEF MSWINDOWS}
  // Aqui futuramente entraremos com a implementação para Windows
{$ENDIF}
  ;

class function TMultiDialog4FMX.Dialog: IDialogBuilder;
begin
  Result := TAndroidDialog.Create;
end;

end.

