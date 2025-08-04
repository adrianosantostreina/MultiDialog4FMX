unit MultiDialog4FMX.Android;

interface

uses
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  FMX.Types,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Graphics,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes;

type
  // Objeto auxiliar para TagObject
  TButtonHandlerObj = class
  public
    ClickHandler: TNotifyEvent;
    TapHandler: TTapEvent;
    Overlay: TLayout;
  end;

  TAndroidDialog = class(TDialogBase, IDialogBuilder)
  protected
    procedure InternalShow; override;
  private
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
  end;

implementation

{ TAndroidDialog }

procedure TAndroidDialog.InternalShow;
var
  Overlay: TLayout;
  BgRect: TRectangle;
  DialogRect: TRectangle;
  LblTitle, LblMsg: TLabel;
  BtnLayout: TFlowLayout;
  Rec: TButtonHandler;
  Btn: TButton;
  HandlerObj: TButtonHandlerObj;
begin
  // Overlay transparente ocupando a tela toda
  Overlay := TLayout.Create(Application.MainForm);
  Overlay.Parent := Application.MainForm;
  Overlay.Align := TAlignLayout.Contents;
  Overlay.HitTest := True;
  Overlay.BringToFront;

  // Fundo escurecido
  BgRect := TRectangle.Create(Overlay);
  BgRect.Parent := Overlay;
  BgRect.Align := TAlignLayout.Contents;
  BgRect.Fill.Color := TAlphaColorRec.Black;
  BgRect.Opacity := 0.4;
  BgRect.Stroke.Kind := TBrushKind.None;

  // Caixa de diálogo
  DialogRect := TRectangle.Create(Overlay);
  DialogRect.Parent := Overlay;
  DialogRect.Align := TAlignLayout.Center;
  DialogRect.Width := 300;
  DialogRect.Height := 180;
  DialogRect.XRadius := 12;
  DialogRect.YRadius := 12;
  DialogRect.Fill.Color := TAlphaColorRec.White;
  DialogRect.Stroke.Kind := TBrushKind.None;
  DialogRect.Padding.Rect := RectF(10, 10, 10, 10);

  // Título
  if FTitle <> '' then
  begin
    LblTitle := TLabel.Create(DialogRect);
    LblTitle.Parent := DialogRect;
    LblTitle.Align := TAlignLayout.Top;
    LblTitle.Text := FTitle;
    LblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
    LblTitle.Margins.Rect := RectF(16, 16, 16, 8);
  end;

  // Mensagem
  if FMessage <> '' then
  begin
    LblMsg := TLabel.Create(DialogRect);
    LblMsg.Parent := DialogRect;
    LblMsg.Align := TAlignLayout.Client;
    LblMsg.Text := FMessage;
    LblMsg.WordWrap := True;
    LblMsg.Margins.Rect := RectF(16, 8, 16, 8);
    LblMsg.VertTextAlign := TTextAlign.Leading;
  end;

  // Layout de botões
  BtnLayout := TFlowLayout.Create(DialogRect);
  BtnLayout.Parent := DialogRect;
  BtnLayout.Align := TAlignLayout.Bottom;
  BtnLayout.Height := 56;
  BtnLayout.Margins.Rect := RectF(10, 5, 10, 10);

  // Cria botões dinamicamente
  for Rec in FButtonHandlers do
  begin
    Btn := TButton.Create(BtnLayout);
    Btn.Parent := BtnLayout;
    Btn.Text := Rec.Text;
    Btn.Height := 40;
    Btn.Width := Btn.Canvas.TextWidth(Rec.Text) + 24;
    Btn.Margins.Right := 8;

    // Cria e configura o objeto handler corretamente
    HandlerObj := TButtonHandlerObj.Create;
    HandlerObj.ClickHandler := Rec.ClickHandler;  // ✅ Armazena a referência correta
    HandlerObj.TapHandler := Rec.TapHandler;      // ✅ Armazena a referência correta
    HandlerObj.Overlay := Overlay;

    Btn.TagObject := HandlerObj;

    // Atribui os eventos
    if Assigned(Rec.ClickHandler) then
      Btn.OnClick := ButtonClick;

    if Assigned(Rec.TapHandler) then
      Btn.OnTap := ButtonTap;
  end;
end;

procedure TAndroidDialog.ButtonClick(Sender: TObject);
var
  Obj: TButtonHandlerObj;
begin
  if (Sender is TButton) and Assigned(TButton(Sender).TagObject) then
  begin
    Obj := TButton(Sender).TagObject as TButtonHandlerObj;

    // ✅ Executa o handler original que foi passado como parâmetro
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);  // Agora deve executar corretamente

    // Fecha o diálogo      sob
    if Assigned(Obj.Overlay) then
    begin
      Obj.Overlay.Parent := nil;
      Obj.Overlay.DisposeOf;
    end;

    // Limpeza
    TButton(Sender).TagObject := nil;
    Obj.Free;
  end;
end;

procedure TAndroidDialog.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj: TButtonHandlerObj;
begin
  if (Sender is TButton) and Assigned(TButton(Sender).TagObject) then
  begin
    Obj := TButton(Sender).TagObject as TButtonHandlerObj;

    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, Point);

    if Assigned(Obj.Overlay) then
    begin
      Obj.Overlay.Parent := nil;
      Obj.Overlay.DisposeOf;
    end;

    TButton(Sender).TagObject := nil;
    Obj.Free;
  end;
end;




end.

