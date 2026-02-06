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
  System.SysUtils;
{$ENDIF}

function CreateDialog: IDialogBuilder;
begin
{$IFDEF ANDROID}
  Result := TAndroidDialog.Create;
{$ELSEIF DEFINED(IOS)}
  raise Exception.Create('iOS não implementado ainda');
  // Result := TiOSDialog.Create;
{$ELSE}
  raise Exception.Create('Plataforma não suportada');
{$ENDIF}
end;

end.
