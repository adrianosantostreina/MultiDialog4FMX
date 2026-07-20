unit MultiDialog4FMX.Queue;

interface

uses
  MultiDialog4FMX.Interfaces,

  FMX.Forms,

  System.Classes,
  System.SysUtils,
  System.UITypes;

type
  TDialogSnapshot = class
  private
    FForm: TCommonCustomForm;
    FTitle: string;
    FMessage: string;
    FMsgType: TMultiDialogType;
    FCancelable: Boolean;
    FFontSize: Single;
    FBorderRadius: Single;
    FAnimation: TDialogAnimation;
    FTheme: TDialogTheme;
    FCustomSVG: string;
    FCustomIconColor: TAlphaColor;
    FResultCallback: TDialogResultProc;
    FButtons: TButtonHandlerList;
  public
    constructor Create(const AForm: TCommonCustomForm; const ATitle, AMessage: string;
      const AMsgType: TMultiDialogType; const ACancelable: Boolean;
      const AFontSize, ABorderRadius: Single; const AAnimation: TDialogAnimation;
      const ATheme: TDialogTheme; const ACustomSVG: string;
      const ACustomIconColor: TAlphaColor; const AResultCallback: TDialogResultProc;
      const AButtons: TButtonHandlerList);
    destructor Destroy; override;

    property Form: TCommonCustomForm read FForm;
    property Title: string read FTitle;
    property Message: string read FMessage;
    property MsgType: TMultiDialogType read FMsgType;
    property Cancelable: Boolean read FCancelable;
    property FontSize: Single read FFontSize;
    property BorderRadius: Single read FBorderRadius;
    property Animation: TDialogAnimation read FAnimation;
    property Theme: TDialogTheme read FTheme;
    property CustomSVG: string read FCustomSVG;
    property CustomIconColor: TAlphaColor read FCustomIconColor;
    property ResultCallback: TDialogResultProc read FResultCallback;
    property Buttons: TButtonHandlerList read FButtons;
  end;

implementation

{ TDialogSnapshot }

constructor TDialogSnapshot.Create(const AForm: TCommonCustomForm;
  const ATitle, AMessage: string; const AMsgType: TMultiDialogType;
  const ACancelable: Boolean; const AFontSize, ABorderRadius: Single;
  const AAnimation: TDialogAnimation; const ATheme: TDialogTheme;
  const ACustomSVG: string; const ACustomIconColor: TAlphaColor;
  const AResultCallback: TDialogResultProc; const AButtons: TButtonHandlerList);
var
  I: Integer;
  LSource: TButtonHandler;
  LCopy: TButtonHandler;
begin
  inherited Create;
  FForm            := AForm;
  FTitle           := ATitle;
  FMessage         := AMessage;
  FMsgType         := AMsgType;
  FCancelable      := ACancelable;
  FFontSize        := AFontSize;
  FBorderRadius    := ABorderRadius;
  FAnimation       := AAnimation;
  FTheme           := ATheme;
  FCustomSVG       := ACustomSVG;
  FCustomIconColor := ACustomIconColor;
  FResultCallback  := AResultCallback;

  FButtons := TButtonHandlerList.Create(True);
  for I := 0 to AButtons.Count - 1 do
  begin
    LSource := AButtons[I];
    LCopy := TButtonHandler.Create;
    LCopy.Text             := LSource.Text;
    LCopy.ClickHandler     := LSource.ClickHandler;
    LCopy.TapHandler       := LSource.TapHandler;
    LCopy.AnonymousHandler := LSource.AnonymousHandler;
    LCopy.Color            := LSource.Color;
    LCopy.StyleLookup      := LSource.StyleLookup;
    LCopy.ModalResult      := LSource.ModalResult;
    LCopy.Timeout          := LSource.Timeout;
    // Overlay fica nil — e estado visual, comeca vazio em toda copia.
    FButtons.Add(LCopy);
  end;
end;

destructor TDialogSnapshot.Destroy;
begin
  FButtons.Free;
  inherited;
end;

end.
