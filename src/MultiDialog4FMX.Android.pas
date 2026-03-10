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
    AnonymousHandler: TProc;
    Overlay: TLayout;
  end;

  TAndroidDialog = class(TDialogBase, IDialogBuilder)
  protected
    FBtnLayout: TFlowLayout;
    procedure InternalShow(const AForm: TCommonCustomForm); override;
    function CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
    procedure OnBackgroundClick(Sender: TObject);
    procedure CloseDialog(AOverlay: TLayout);
  end;

implementation

{ TAndroidDialog }

procedure TAndroidDialog.InternalShow(const AForm: TCommonCustomForm);
  // Constants for Icon SVG Data
const
  SVG_WARNING = 'M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z';
  SVG_ERROR = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';
  SVG_INFO = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z';
  SVG_QUESTION = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2zm2.07-7.75l-.9.92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1.45-2.1 1.17-2.83l1.24-1.26c.37-.36.59-.86.59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z';
  SVG_SUCCESS = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z';

  // Layout Constants
  C_MinDialogHeight = 200;
  C_DialogWidth = 300;
  C_TitleHeight = 40;
  C_ButtonsHeight = 56;
  C_PaddingHeight = 32;
  C_IconSize = 40;
var
  LParent  : TCommonCustomForm;
  LOverlay: TLayout;
  LBgRect: TRectangle;
  LDialogRect: TRectangle;

  // Containers
  LBodyLayout: TLayout;

  // Components
  LLblTitle, LblMsg: TLabel;
  LIconPath: TPath;
  LBtnLayout: TFlowLayout;
  LRec: TButtonHandler;
  LBtn: TButton;
  LHandlerObj: TButtonHandlerObj;

  // Metrics
  LMsgHeight: Single;
  LFinalHeight: Single;
  LWidthButtons: Single;
  LRecalcMsgWidth: Single;
  LIconWidthUsed: Single;
  LBodyHeightNeeded: Single;
  LMaxScreenHeight: Single;
  LMsgFont: TFont;
  LScrollBox: TVertScrollBox;
  LIconContainer: TLayout;
  I: Integer;

begin
  LParent := ResolveParentForm(AForm);

  if not Assigned(LParent) then
    raise Exception.Create('Nenhum formulário disponível para exibir o diálogo.');

  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O número mínimo de botões é 1.');

  if FButtonHandlers.Count > 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  // LOverlay
  LOverlay := TLayout.Create(LParent);
  LOverlay.Parent := LParent;
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

  // 1. Layout dos botões (Bottom) - Criação Primeiro para garantir Dock no Rodapé
  LBtnLayout := TFlowLayout.Create(LDialogRect);
  LBtnLayout.Parent := LDialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_ButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4);
  FBtnLayout := LBtnLayout;

  // 2. Título (Top)
  if FTitle <> EmptyStr then
  begin
    LLblTitle := TLabel.Create(LDialogRect);
    LLblTitle.Parent := LDialogRect;
    LLblTitle.Align := TAlignLayout.Top;
    LLblTitle.Text := FTitle;
    LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
    LLblTitle.Margins.Rect := RectF(16, 12, 16, 4);
    LLblTitle.Height := C_TitleHeight;
    LLblTitle.TextSettings.Font.Size := 16;
    LLblTitle.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
    LLblTitle.VertTextAlign := TTextAlign.Center;
  end;

  // 3. Corpo (Client) - Usando ScrollBox para permitir rolagem se necessário
  LScrollBox := TVertScrollBox.Create(LDialogRect);
  LScrollBox.Parent := LDialogRect;
  LScrollBox.Align := TAlignLayout.Client;
  LScrollBox.Margins.Rect := RectF(0, 8, 0, 8);
  LScrollBox.ShowScrollBars := True;

  // Layout interno que vai crescer o quanto precisar (Conteúdo)
  LBodyLayout := TLayout.Create(LScrollBox);
  LBodyLayout.Parent := LScrollBox;
  LBodyLayout.Align := TAlignLayout.Top;
  LBodyLayout.Margins.Rect := RectF(16, 0, 16, 0);

  LIconWidthUsed := 0;

  // Render Icon if not custom
  if FMsgType <> TMultiDialogType.mdtCustom then
  begin
    LIconContainer := TLayout.Create(LBodyLayout);
    LIconContainer.Parent := LBodyLayout;
    LIconContainer.Align := TAlignLayout.Left;
    LIconContainer.Width := C_IconSize;
    LIconContainer.Margins.Right := 16;

    LIconPath := TPath.Create(LIconContainer);
    LIconPath.Parent := LIconContainer;
    LIconPath.Align := TAlignLayout.Top;
    LIconPath.Height := C_IconSize;
    LIconPath.Width := C_IconSize;
    LIconPath.Stroke.Kind := TBrushKind.None;

    case FMsgType of
      mdtWarning:
      begin
        LIconPath.Data.Data := SVG_WARNING;
        LIconPath.Fill.Color := TAlphaColorRec.Gold;
      end;
      mdtError:
      begin
        LIconPath.Data.Data := SVG_ERROR;
        LIconPath.Fill.Color := TAlphaColorRec.Red;
      end;
      mdtInformation:
      begin
         LIconPath.Data.Data := SVG_INFO;
         LIconPath.Fill.Color := TAlphaColorRec.Dodgerblue;
      end;
      mdtQuestion:
      begin
         LIconPath.Data.Data := SVG_QUESTION;
         LIconPath.Fill.Color := TAlphaColorRec.Limegreen;
      end;
      mdtConfirmation:
      begin
         LIconPath.Data.Data := SVG_SUCCESS;
         LIconPath.Fill.Color := TAlphaColorRec.Limegreen;
      end;
    end;

    LIconWidthUsed := C_IconSize + 16;
  end;

  // Mensagem (Client do BodyLayout)
  if FMessage <> EmptyStr then
  begin
    LblMsg := TLabel.Create(LBodyLayout);
    LblMsg.Parent := LBodyLayout;
    LblMsg.Align := TAlignLayout.Client;
    LblMsg.WordWrap := True;
    LblMsg.Text := FMessage;
    LblMsg.VertTextAlign := TTextAlign.Leading;
    LblMsg.TextSettings.Font.Size := 14;
    LblMsg.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  end;

  // Calculate Height needed
  if FMsgType <> TMultiDialogType.mdtCustom then
     LRecalcMsgWidth := C_DialogWidth - 32 - (C_IconSize + 16) - 8
  else
     LRecalcMsgWidth := C_DialogWidth - 32 - 8;

  LMsgFont := TFont.Create;
  try
    LMsgFont.Size := 14;
    LMsgHeight := CalculateMessageHeight(FMessage, LRecalcMsgWidth, LMsgFont);
  finally
    LMsgFont.Free;
  end;

  LBodyHeightNeeded := Max(LMsgHeight, C_IconSize);
  LBodyLayout.Height := LBodyHeightNeeded;

  LFinalHeight := LBodyHeightNeeded + 16 + LBtnLayout.Height + C_PaddingHeight;
  if FTitle <> EmptyStr then
     LFinalHeight := LFinalHeight + C_TitleHeight + 16;

  LFinalHeight := Max(LFinalHeight, C_MinDialogHeight);

  LMaxScreenHeight := Screen.Size.Height * 0.9;
  if LFinalHeight > LMaxScreenHeight then
    LFinalHeight := LMaxScreenHeight;

  LDialogRect.Height := LFinalHeight;

  // Lógica Cancelable
  if FCancelable then
  begin
    LBgRect.HitTest := True;
    LBgRect.OnClick := OnBackgroundClick;
  end;

  // Cria botões com Layout Responsivo
  if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and (Screen.Width < 600) then
  begin
    LBtnLayout.Height := C_ButtonsHeight * 2;
    LDialogRect.Height := LDialogRect.Height + C_ButtonsHeight;
    LWidthButtons := (C_DialogWidth / 3) - 16;
  end
  else if FButtonHandlers.Count = 1 then
  begin
     LWidthButtons := C_DialogWidth - 32;
  end
  else
  begin
    LWidthButtons := (C_DialogWidth / FButtonHandlers.Count) - 24;
  end;

  for I := 0 to FButtonHandlers.Count - 1 do
  begin
    LRec := FButtonHandlers[I];
    LBtn := TButton.Create(LBtnLayout);
    LBtn.Parent := LBtnLayout;
    LBtn.Text := LRec.Text;
    LBtn.TextSettings.Font.Size := 14;
    LBtn.StyledSettings := [TStyledSetting.Style];
    LBtn.Height := 40;

    if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and (Screen.Width < 600) and (I = 3) then
    begin
      LBtn.Width := C_DialogWidth - 32;
      LBtn.Margins.Top := 8;
    end
    else
    begin
      LBtn.Width := LWidthButtons;
    end;

    if I < FButtonHandlers.Count - 1 then
      LBtn.Margins.Right := 8
    else
      LBtn.Margins.Right := 0;

    LBtn.TintColor := LRec.Color;

    if LRec.StyleLookup <> '' then
    begin
      LBtn.StyleLookup := LRec.StyleLookup;
      LBtn.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style, TStyledSetting.FontColor, TStyledSetting.Size];
    end;

    LHandlerObj := TButtonHandlerObj.Create;
    try
      LHandlerObj.ClickHandler := LRec.ClickHandler;
      LHandlerObj.TapHandler := LRec.TapHandler;
      LHandlerObj.AnonymousHandler := LRec.AnonymousHandler;
      LHandlerObj.Overlay := LOverlay;
      LBtn.TagObject := LHandlerObj;
    except
      LHandlerObj.Free;
      raise;
    end;

    if Assigned(LRec.TapHandler) then
      LBtn.OnTap := ButtonTap
    else
      LBtn.OnClick := ButtonClick;
  end;
end;

function TAndroidDialog.CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
var
  Layout: TTextLayout;
begin
  if AText = EmptyStr then
  begin
    Result := 0;
    Exit;
  end;

  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    Layout.Font := AFont;
    Layout.MaxSize := TSizeF.Create(AWidth, 9999);
    Layout.WordWrap := True;
    Layout.Text := AText;
    Layout.EndUpdate;
    Result := Layout.TextHeight + 10;
  finally
    Layout.Free;
  end;
end;

procedure TAndroidDialog.CloseDialog(AOverlay: TLayout);
var
  I: Integer;
  LBtn: TButton;
  LObj: TButtonHandlerObj;
begin
  if not Assigned(AOverlay) then
    Exit;

  // Libera todos os TButtonHandlerObj antes de destruir a hierarquia de controles.
  // FMX não libera TagObject automaticamente ao destruir controles.
  if Assigned(FBtnLayout) then
    for I := 0 to FBtnLayout.ChildrenCount - 1 do
      if FBtnLayout.Children[I] is TButton then
      begin
        LBtn := TButton(FBtnLayout.Children[I]);
        if Assigned(LBtn.TagObject) then
        begin
          LObj := LBtn.TagObject as TButtonHandlerObj;
          LBtn.TagObject := nil;
          LObj.Free;
        end;
      end;
  FBtnLayout := nil;

  AOverlay.Parent := nil;
  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  AOverlay.DisposeOf;
  {$ELSE}
  AOverlay.Free;
  {$ENDIF}
end;

procedure TAndroidDialog.OnBackgroundClick(Sender: TObject);
var
  LObj: TFmxObject;
  LOverlay: TLayout;
begin
  if Sender is TFmxObject then
  begin
    LObj := TFmxObject(Sender).Parent;
    if LObj is TLayout then
    begin
      LOverlay := TLayout(LObj);
      CloseDialog(LOverlay);
    end;
  end;
end;

procedure TAndroidDialog.ButtonClick(Sender: TObject);
var
  Obj: TButtonHandlerObj;
  LOverlay: TLayout;
begin
  if not ((Sender is TButton) and Assigned(TButton(Sender).TagObject)) then
    Exit;

  Obj := TButton(Sender).TagObject as TButtonHandlerObj;
  LOverlay := Obj.Overlay;

  // Limpa TagObject ANTES de chamar o handler e ANTES de CloseDialog,
  // evitando use-after-free quando CloseDialog destrói a hierarquia.
  TButton(Sender).TagObject := nil;

  try
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);
    if Assigned(Obj.AnonymousHandler) then
      Obj.AnonymousHandler();
  finally
    Obj.Free;
    CloseDialog(LOverlay);
  end;
end;

procedure TAndroidDialog.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj: TButtonHandlerObj;
  LOverlay: TLayout;
begin
  if not ((Sender is TButton) and Assigned(TButton(Sender).TagObject)) then
    Exit;

  Obj := TButton(Sender).TagObject as TButtonHandlerObj;
  LOverlay := Obj.Overlay;

  TButton(Sender).TagObject := nil;

  try
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, Point);
  finally
    Obj.Free;
    CloseDialog(LOverlay);
  end;
end;


end.
