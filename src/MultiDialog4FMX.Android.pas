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
  FMX.TextLayout,

  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math;

type
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
    function CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
  end;

implementation

{ TAndroidDialog }

procedure TAndroidDialog.InternalShow;
const
  C_MaxDialogHeight = 400;
  C_MinDialogHeight = 200;
  C_DialogWidth = 300;
  C_TitleHeight = 40;
  C_ButtonsHeight = 56;
  C_PaddingHeight = 32;
var
  LOverlay: TLayout;
  LBgRect: TRectangle;
  LDialogRect: TRectangle;
  LLblTitle, LblMsg: TLabel;
  LBtnLayout: TFlowLayout;
  LRec: TButtonHandler;
  LBtn: TButton;
  LHandlerObj: TButtonHandlerObj;
  LMsgHeight: Single;
  LFinalHeight: Single;
  LLWidthButtos: Single;
begin
  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O número mínimo de botões é 1.')
  else if FButtonHandlers.Count > 3 then
    raise Exception.Create('O número máximo de botões é 3.');

  // LOverlay
  LOverlay := TLayout.Create(Application.MainForm);
  LOverlay.Parent := Application.MainForm;
  LOverlay.Align := TAlignLayout.Contents;
  LOverlay.HitTest := True;
  LOverlay.BringToFront;

  // Fundo escuro
  LBgRect := TRectangle.Create(LOverlay);
  LBgRect.Parent := LOverlay;
  LBgRect.Align := TAlignLayout.Contents;
  LBgRect.Fill.Color := TAlphaColorRec.Black;
  LBgRect.Opacity := 0.4;
  LBgRect.Stroke.Kind := TBrushKind.None;

  // Caixa de diálogo
  LDialogRect := TRectangle.Create(LOverlay);
  LDialogRect.Parent := LOverlay;
  LDialogRect.Align := TAlignLayout.Center;
  LDialogRect.Width := C_DialogWidth;
  LDialogRect.XRadius := 12;
  LDialogRect.YRadius := 12;
  LDialogRect.Fill.Color := TAlphaColorRec.White;
  LDialogRect.Stroke.Kind := TBrushKind.None;
  LDialogRect.Padding.Rect := RectF(4, 4, 4, 4);

  // Mensagem
  LblMsg := TLabel.Create(LDialogRect);
  LblMsg.Parent := LDialogRect;
  LblMsg.Align := TAlignLayout.Top;
  LblMsg.WordWrap := True;
  LblMsg.Margins.Rect := RectF(16, 4, 8, 8);
  LblMsg.Text := FMessage;
  LblMsg.VertTextAlign := TTextAlign.Leading;
  LblMsg.TextSettings.Font.Size := 14;
  LblMsg.StyledSettings := [TStyledSetting.Style];

  LMsgHeight := CalculateMessageHeight(FMessage, C_DialogWidth - 32, LblMsg.TextSettings.Font);
  LblMsg.Height := LMsgHeight;

  // Título
  if FTitle <> EmptyStr then
  begin
    LLblTitle := TLabel.Create(LDialogRect);
    LLblTitle.Parent := LDialogRect;
    LLblTitle.Align := TAlignLayout.Top;
    LLblTitle.Text := FTitle;
    LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
    LLblTitle.Margins.Rect := RectF(16, 4, 4, 8);
    LLblTitle.Height := C_TitleHeight;
    LLblTitle.TextSettings.Font.Size := 14;
    LLblTitle.StyledSettings := [TStyledSetting.Style];
    LLblTitle.BringToFront;
  end;

  // Layout dos botões
  LBtnLayout := TFlowLayout.Create(LDialogRect);
  LBtnLayout.Parent := LDialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_ButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 4, 4, 0);

  // Cria botões
  LLWidthButtos := (C_DialogWidth / FButtonHandlers.Count) - 24;

  for LRec in FButtonHandlers do
  begin
    LBtn := TButton.Create(LBtnLayout);
    LBtn.Parent := LBtnLayout;
    LBtn.Text := LRec.Text;
    LBtn.TextSettings.Font.Size := 14;
    LBtn.StyledSettings := [TStyledSetting.Style];
    LBtn.Height := 40;
    LBtn.Width := LLWidthButtos;
    LBtn.Margins.Right := 8;
    LBtn.TintColor := LRec.Color;

    LHandlerObj := TButtonHandlerObj.Create;
    LHandlerObj.ClickHandler := LRec.ClickHandler;
    LHandlerObj.TapHandler := LRec.TapHandler;
    LHandlerObj.Overlay := LOverlay;
    LBtn.TagObject := LHandlerObj;

    if Assigned(LRec.ClickHandler) then
      LBtn.OnClick := ButtonClick;

    if Assigned(LRec.TapHandler) then
      LBtn.OnTap := ButtonTap;
  end;

  // Altura final ajustada
  LFinalHeight := C_TitleHeight + LMsgHeight + C_ButtonsHeight + C_PaddingHeight;
  LFinalHeight := Max(LFinalHeight, C_MinDialogHeight);
  LFinalHeight := Min(LFinalHeight, C_MaxDialogHeight);
  LDialogRect.Height := LFinalHeight;
end;

function TAndroidDialog.CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    Layout.Font := AFont;
    Layout.MaxSize := TSizeF.Create(AWidth, 9999); // Altura ilimitada
    Layout.WordWrap := True;
    Layout.Text := AText;
    Layout.EndUpdate;

    Result := Layout.TextHeight + 10;
  finally
    Layout.Free;
  end;
end;

procedure TAndroidDialog.ButtonClick(Sender: TObject);
var
  Obj: TButtonHandlerObj;
begin
  if (Sender is TButton) and Assigned(TButton(Sender).TagObject) then
  begin
    Obj := TButton(Sender).TagObject as TButtonHandlerObj;
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);

    if Assigned(Obj.Overlay) then
    begin
      Obj.Overlay.Parent := nil;
      Obj.Overlay.DisposeOf;
    end;

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
*)
