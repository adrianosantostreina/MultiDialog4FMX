unit MultiDialog4FMX.Base;

interface

uses
  MultiDialog4FMX.Interfaces,
  FMX.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

type
  // Guarda texto + handler click ou tap
  TButtonHandler = class
  public
    Text: string;
    ClickHandler: TNotifyEvent;
    TapHandler: TTapEvent;
  end;
  TButtonHandlerList = TObjectList<TButtonHandler>;

  TDialogBase = class(TInterfacedObject, IDialogBuilder)
  protected
    FTitle: string;
    FMessage: string;
    FButtonHandlers: TButtonHandlerList;
    // Cada plataforma implementa sua exibição
    procedure InternalShow; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    // IDialogBuilder
    function SetTitle(const ATitle: string): IDialogBuilder;
    function SetMessage(const AMessage: string): IDialogBuilder;
    function Buttons: IDialogButtonsBuilder;
    function Show: IDialogBuilder;
  end;

  // Builder aninhado de botões
  TDialogButtonsBuilder = class(TInterfacedObject, IDialogButtonsBuilder)
  private
    FParent: TDialogBase;
  public
    constructor Create(const AParent: TDialogBase);

    function AddButton(const AText: string; const AOnClick: TNotifyEvent): IDialogButtonsBuilder; overload;
    function AddButton(const AText: string; const AOnTap: TTapEvent): IDialogButtonsBuilder; overload;
    function &End: IDialogBuilder;
  end;

implementation

{ TDialogBase }

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

function TDialogBase.Buttons: IDialogButtonsBuilder;
begin
  Result := TDialogButtonsBuilder.Create(Self);
end;

function TDialogBase.Show: IDialogBuilder;
begin
  InternalShow;
  Result := Self;
end;

{ TDialogButtonsBuilder }

constructor TDialogButtonsBuilder.Create(const AParent: TDialogBase);
begin
  inherited Create;
  FParent := AParent;
end;

function TDialogButtonsBuilder.AddButton(const AText: string;
  const AOnClick: TNotifyEvent): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  Rec := TButtonHandler.Create;
  Rec.Text := AText;
  Rec.ClickHandler := AOnClick;  // ✅ Certifique-se que o handler está sendo atribuído
  Rec.TapHandler := nil;
  FParent.FButtonHandlers.Add(Rec);
  Result := Self;
end;

function TDialogButtonsBuilder.AddButton(const AText: string; const AOnTap: TTapEvent): IDialogButtonsBuilder;
var
  Rec: TButtonHandler;
begin
  Rec := TButtonHandler.Create;  // <-- Esta linha estava faltando
  Rec.Text := AText;
  Rec.ClickHandler := nil;
  Rec.TapHandler := AOnTap;
  FParent.FButtonHandlers.Add(Rec);
  Result := Self;
end;


function TDialogButtonsBuilder.&End: IDialogBuilder;
begin
  Result := FParent;
end;

end.
