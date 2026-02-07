unit MultiDialog4FMX.Base;

interface

uses
  MultiDialog4FMX.Interfaces,

  FMX.Types,
  FMX.Forms,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;

type
  // Guarda texto + handler click ou tap
  TButtonHandler = class
  public
    Text: string;
    ClickHandler: TNotifyEvent;
    TapHandler: TTapEvent;
    AnonymousHandler: TProc;
    Color: TAlphaColor;
    StyleLookup: string;
  end;
  TButtonHandlerList = TObjectList<TButtonHandler>;

  TDialogBase = class(TInterfacedObject, IDialogBuilder)
  protected
    FTitle: string;
    FMessage: string;
    FCancelable: Boolean;
    FButtonHandlers: TButtonHandlerList;
    FMsgType: TMultiDialogType;
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

    function AddButton(const AText: string; const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnTap: TTapEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    function &End: IDialogBuilder;
  end;

implementation

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
  const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create('O diálogo suporta no máximo 4 botões.');

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := AOnClick;
  Rec.TapHandler := nil;
  Rec.AnonymousHandler := nil;

  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create('O diálogo suporta no máximo 4 botões.');

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := nil;
  Rec.TapHandler := nil;
  Rec.AnonymousHandler := AOnSimpleClick;

  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string; const AColor: TAlphaColor; const AStyleLookup: string): IDialogButtonsBuilder;
begin
  // Redireciona para o overload principal passando nil no evento
  Result := AddButton(AText, TNotifyEvent(nil), AColor, AStyleLookup);
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnTap: TTapEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  if FParent.FButtonHandlers.Count >= 4 then
    raise Exception.Create('O diálogo suporta no máximo 4 botões.');

  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := nil;
  Rec.TapHandler := AOnTap;
  Rec.AnonymousHandler := nil;
  
  Rec.Color := AColor;
  Rec.StyleLookup := AStyleLookup;

  FParent.FButtonHandlers.Add(Rec);

  Result := Self;
end;


function TDialogButtonsBuilder.&End: IDialogBuilder;
begin
  Result := FParent;
end;

end.
