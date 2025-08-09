unit MultiDialog4FMX.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,

  FMX.Types;

type
  IDialogButtonsBuilder = interface;
  IDialogBuilder = interface
    ['{A1B2C3D4-E5F6-47A8-9B0C-ABCDEF123456}']
    function SetTitle(const ATitle: string): IDialogBuilder;
    function SetMessage(const AMessage: string): IDialogBuilder;
    function Buttons: IDialogButtonsBuilder;
    function Show: IDialogBuilder;
  end;

  IDialogButtonsBuilder = interface
    ['{B1C2D3E4-F5A6-47B8-9A0C-ABCDEF654321}']
    function AddButton(const AText: string; const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnTap: TTapEvent; const AColor: TAlphaColor  = TAlphaColorRec.Null): IDialogButtonsBuilder; overload;
    function &End: IDialogBuilder;
  end;

implementation

end.



