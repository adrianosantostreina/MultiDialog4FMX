unit MultiDialog4FMX.Factory;

interface

uses
  MultiDialog4FMX.Interfaces;

function CreateDialog: IDialogBuilder;

implementation

uses
  System.SysUtils, System.TypInfo,
  MultiDialog4FMX.Android, MultiDialog4FMX.iOS;

function CreateDialog: IDialogBuilder;
begin
{$IFDEF ANDROID}
  Result := TAndroidDialogBuilder.Create;
{$ELSEIF Defined(IOS)}
  Result := TIosDialogBuilder.Create;
{$ELSE}
  raise Exception.Create('Plataforma não suportada');
{$ENDIF}
end;

end.
