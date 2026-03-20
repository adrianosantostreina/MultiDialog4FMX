unit MultiDialog4FMX.Base;

interface

uses
  MultiDialog4FMX.Interfaces,

  FMX.Types,
  FMX.Forms,
  FMX.Layouts,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;

const
  C_MaxButtonsMsg = 'O di'#225'logo suporta no m'#225'ximo 4 bot'#245'es.';

type
  // Guarda texto + handler click ou tap
  TButtonHandler = class
  private
    class var FInstanceCount: Integer;
  private
    FText: string;
    FClickHandler: TNotifyEvent;
    FTapHandler: TTapEvent;
    FAnonymousHandler: TProc;
    FColor: TAlphaColor;
    FStyleLookup: string;
    FOverlay: TLayout;
    FModalResult: TModalResult;
    FTimeout: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    class property InstanceCount: Integer read FInstanceCount;
    property Text: string read FText write FText;
    property ClickHandler: TNotifyEvent read FClickHandler write FClickHandler;
    property TapHandler: TTapEvent read FTapHandler write FTapHandler;
    property AnonymousHandler: TProc read FAnonymousHandler write FAnonymousHandler;
    property Color: TAlphaColor read FColor write FColor;
    property StyleLookup: string read FStyleLookup write FStyleLookup;
    property Overlay: TLayout read FOverlay write FOverlay;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property Timeout: Integer read FTimeout write FTimeout;
  end;
  TButtonHandlerList = TObjectList<TButtonHandler>;

  TDialogBase = class(TInterfacedObject, IDialogBuilder)
  protected
    FTitle: string;
    FMessage: string;
    FCancelable: Boolean;
    FButtonHandlers: TButtonHandlerList;
    FMsgType: TMultiDialogType;
    FFontSize: Single;
    FBorderRadius: Single;
    FAnimation: TDialogAnimation;
    FCustomSVG: string;
    FCustomIconColor: TAlphaColor;
    FResultCallback: TDialogResultProc;
    // Cada plataforma implementa sua exibição
    procedure InternalShow(const AForm: TCommonCustomForm); virtual; abstract;
    function ResolveParentForm(const AForm: TCommonCustomForm): TCommonCustomForm;
  public
    constructor Create;
    destructor Destroy; override;

    // IDialogBuilder
    function SetType(AType: TMultiDialogType): IDialogBuilder;
    function SetTitle(const ATitle: string): IDialogBuilder;
    function SetMessage(const AMessage: string): IDialogBuilder;
    function SetCancelable(const Value: Boolean): IDialogBuilder;
    function SetFontSize(const ASize: Single): IDialogBuilder;
    function SetBorderRadius(const ARadius: Single): IDialogBuilder;
    function SetAnimation(const AAnimation: TDialogAnimation): IDialogBuilder;
    function SetIcon(const ASVG: string): IDialogBuilder;
    function SetIconColor(const AColor: TAlphaColor): IDialogBuilder;
    function SetOnResult(const ACallback: TDialogResultProc): IDialogBuilder;
    function Buttons: IDialogButtonsBuilder;
    function Show: IDialogBuilder; overload;
    function Show(const AForm: TCommonCustomForm): IDialogBuilder; overload;
  end;

  // Builder aninhado de botões
  TDialogButtonsBuilder = class(TInterfacedObject, IDialogButtonsBuilder)
  private
    FParent: TDialogBase;
  public
    constructor Create(const AParent: TDialogBase);

    function AddButton(const AText: string; const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnTap: TTapEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const ATimeout: Integer; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;
    function &End: IDialogBuilder;
  end;

implementation

{ TButtonHandler }

constructor TButtonHandler.Create;
begin
  inherited;
  Inc(TButtonHandler.FInstanceCount);
  FModalResult := mrOk;
end;

destructor TButtonHandler.Destroy;
begin
  Dec(TButtonHandler.FInstanceCount);
  inherited;
end;

{ TDialogBase }

function TDialogBase.ResolveParentForm(const AForm: TCommonCustomForm): TCommonCustomForm;
begin
  if Assigned(AForm) then
    Exit(AForm);

  if Assigned(Screen.ActiveForm) then
    Exit(Screen.ActiveForm);

  Result := Application.MainForm;
end;

constructor TDialogBase.Create;
begin
  inherited;
  FButtonHandlers := TButtonHandlerList.Create(True);
  FFontSize     := 14;
  FBorderRadius := 12;
end;

destructor TDialogBase.Destroy;
begin
  FButtonHandlers.Free;
  inherited;
end;

function TDialogBase.SetType(AType: TMultiDialogType): IDialogBuilder;
begin
  FMsgType := AType;
  Result := Self;
end;

function TDialogBase.SetTitle(const ATitle: string): IDialogBuilder;
begin
  FTitle := ATitle;
  Result := Self;
end;

function TDialogBase.SetMessage(const AMessage: string): IDialogBuilder;
begin
  FMessage := AMessage;
  Result := Self;
end;

function TDialogBase.SetCancelable(const Value: Boolean): IDialogBuilder;
begin
  FCancelable := Value;
  Result := Self;
end;

function TDialogBase.SetFontSize(const ASize: Single): IDialogBuilder;
begin
  FFontSize := ASize;
  Result := Self;
end;

function TDialogBase.SetBorderRadius(const ARadius: Single): IDialogBuilder;
begin
  FBorderRadius := ARadius;
  Result := Self;
end;

function TDialogBase.SetAnimation(const AAnimation: TDialogAnimation): IDialogBuilder;
begin
  FAnimation := AAnimation;
  Result := Self;
end;

function TDialogBase.SetIcon(const ASVG: string): IDialogBuilder;
begin
  FCustomSVG := ASVG;
  Result := Self;
end;

function TDialogBase.SetIconColor(const AColor: TAlphaColor): IDialogBuilder;
begin
  FCustomIconColor := AColor;
  Result := Self;
end;

function TDialogBase.SetOnResult(const ACallback: TDialogResultProc): IDialogBuilder;
begin
  FResultCallback := ACallback;
  Result := Self;
end;

function TDialogBase.Buttons: IDialogButtonsBuilder;
begin
  Result := TDialogButtonsBuilder.Create(Self);
end;

function TDialogBase.Show: IDialogBuilder;
begin
  InternalShow(ResolveParentForm(nil));
  Result := Self;
end;

function TDialogBase.Show(const AForm: TCommonCustomForm): IDialogBuilder;
begin
  InternalShow(ResolveParentForm(AForm));
  Result := Self;
end;

{ TDialogButtonsBuilder }

constructor TDialogButtonsBuilder.Create(const AParent: TDialogBase);
begin
  inherited Create;
  FParent := AParent;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null;
  const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := AOnClick;
  Rec.TapHandler := nil;
  Rec.AnonymousHandler := nil;
  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;
  Rec.ModalResult := AModalResult;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null;
  const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := nil;
  Rec.TapHandler := nil;
  Rec.AnonymousHandler := AOnSimpleClick;
  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;
  Rec.ModalResult := AModalResult;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AColor: TAlphaColor = TAlphaColorRec.Null;
  const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder;
begin
  Result := AddButton(AText, TNotifyEvent(nil), AColor, AStyleLookup, AModalResult);
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnTap: TTapEvent; const AColor: TAlphaColor = TAlphaColorRec.Null;
  const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := nil;
  Rec.TapHandler := AOnTap;
  Rec.AnonymousHandler := nil;
  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;
  Rec.ModalResult := AModalResult;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const ATimeout: Integer; const AColor: TAlphaColor = TAlphaColorRec.Null;
  const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  Rec := TButtonHandler.Create;
  Rec.Text             := AText;
  Rec.ClickHandler     := nil;
  Rec.TapHandler       := nil;
  Rec.AnonymousHandler := nil;
  Rec.Color            := AColor;
  Rec.StyleLookup      := AStyleLookup;
  Rec.ModalResult      := AModalResult;
  Rec.Timeout          := ATimeout;

  FParent.FButtonHandlers.Add(Rec);
  Result := Self;
end;

function TDialogButtonsBuilder.&End: IDialogBuilder;
begin
  Result := FParent;
end;

end.
