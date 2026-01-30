unit MultiDialog4FMX.Factory;

interface

uses
  MultiDialog4FMX.Interfaces;

function CreateDialog: IDialogBuilder;

implementation

uses
  System.SysUtils, System.TypInfo,
  MultiDialog4FMX.Android;

function CreateDialog: IDialogBuilder;
begin
{$IFDEF ANDROID}
  Result := TAndroidDialog.Create;
{$ELSE}
  raise Exception.Create('Plataforma não suportada');
{$ENDIF}
end;

end.
