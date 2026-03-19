unit MultiDialog4FMX.Factory;

interface

uses
  MultiDialog4FMX.Interfaces;

// Factory function para criação de diálogos específicos por plataforma
function CreateDialog: IDialogBuilder;

implementation

uses
{$IFDEF ANDROID}
  MultiDialog4FMX.Android;
{$ELSEIF DEFINED(IOS)}
  MultiDialog4FMX.iOS;
{$ELSE}
  MultiDialog4FMX.Desktop;
{$ENDIF}

function CreateDialog: IDialogBuilder;
begin
{$IFDEF ANDROID}
  Result := TAndroidDialog.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSDialog.Create;
{$ELSE}
  Result := TDesktopDialog.Create;
{$ENDIF}
end;

end.
