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
  FMX.Platform,

  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math;

type
  TAndroidDialog = class(TDialogBase, IDialogBuilder)
  private
    FKeepAlive: IDialogBuilder;
  protected
    FBtnLayout: TFlowLayout;
    procedure InternalShow(const AForm: TCommonCustomForm); override;
    function  CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
    procedure OnBackgroundClick(Sender: TObject);
    procedure CloseDialog(AOverlay: TLayout);
    // Sub-methods extracted from InternalShow (R6+R9)
    function  GetPlatformScale: Single;
    function  BuildOverlay(const AParent: TCommonCustomForm; out ABgRect: TRectangle): TLayout;
    function  BuildDialogRect(const AOverlay: TLayout): TRectangle;
    procedure BuildHeader(const ADialogRect: TRectangle);
    procedure BuildBody(const ADialogRect: TRectangle;
                        out AIconPresent: Boolean; out ABodyLayout: TLayout);
    procedure BuildButtons(const AOverlay: TLayout; const ADialogRect: TRectangle);
    function  CalculateFinalHeight(const ABodyLayout: TLayout;
                                   const AIconPresent: Boolean): Single;
    procedure PaintColoredBtn(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  end;

implementation

const
  // Icon SVG paths
  SVG_WARNING  = 'M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z';
  SVG_ERROR    = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';
  SVG_INFO     = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z';
  SVG_QUESTION = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2zm2.07-7.75l-.9.92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1.45-2.1 1.17-2.83l1.24-1.26c.37-.36.59-.86.59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z';
  SVG_SUCCESS  = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z';

  // Base layout constants (logical points; multiply by Screen.Scale for DPI-aware size).
  // Font sizes are NOT multiplied — FMX handles font DPI internally.
  C_BaseMinDialogHeight = 200;
  C_BaseDialogWidth     = 300;
  C_BaseTitleHeight     = 40;
  C_BaseButtonsHeight   = 56;
  C_BasePaddingHeight   = 32;
  C_BaseIconSize        = 40;
  C_BaseBtnHeight       = 40;
  C_BaseResponsiveBreak = 600;
  C_BaseTitleFontSize   = 16;

{ TAndroidDialog }

function TAndroidDialog.GetPlatformScale: Single;
var
  LScreenSvc: IFMXScreenService;
begin
  Result := 1.0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSvc) then
    Result := LScreenSvc.GetScreenScale;
end;

procedure TAndroidDialog.InternalShow(const AForm: TCommonCustomForm);
var
  LParent     : TCommonCustomForm;
  LOverlay    : TLayout;
  LDialogRect : TRectangle;
  LBgRect     : TRectangle;
  LBodyLayout : TLayout;
  LIconPresent: Boolean;
begin
  LParent := ResolveParentForm(AForm);
  if not Assigned(LParent) then
    raise Exception.Create('Nenhum formul'#225'rio dispon'#237'vel para exibir o di'#225'logo.');
  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O n'#250'mero m'#237'nimo de bot'#245'es '#233' 1.');
  if FButtonHandlers.Count > 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  LOverlay    := BuildOverlay(LParent, LBgRect);
  LDialogRect := BuildDialogRect(LOverlay);
  BuildButtons(LOverlay, LDialogRect);                         // Bottom — before Client
  BuildHeader(LDialogRect);                                    // Top
  BuildBody(LDialogRect, LIconPresent, LBodyLayout);           // Client

  LDialogRect.Height := CalculateFinalHeight(LBodyLayout, LIconPresent);

  if FCancelable then
  begin
    LBgRect.HitTest := True;
    LBgRect.OnClick := OnBackgroundClick;
  end;

  FKeepAlive := Self;
end;

function TAndroidDialog.BuildOverlay(const AParent: TCommonCustomForm;
  out ABgRect: TRectangle): TLayout;
var
  LOverlay: TLayout;
  LBgRect : TRectangle;
begin
  LOverlay := TLayout.Create(AParent);
  LOverlay.Parent := AParent;
  LOverlay.Align := TAlignLayout.Contents;
  LOverlay.HitTest := True;
  LOverlay.BringToFront;

  LBgRect := TRectangle.Create(LOverlay);
  LBgRect.Parent := LOverlay;
  LBgRect.Align := TAlignLayout.Contents;
  LBgRect.Fill.Color := TAlphaColorRec.Black;
  LBgRect.Opacity := 0.4;
  LBgRect.Stroke.Kind := TBrushKind.None;

  ABgRect := LBgRect;
  Result  := LOverlay;
end;

function TAndroidDialog.BuildDialogRect(const AOverlay: TLayout): TRectangle;
var
  LDialogRect: TRectangle;
begin
  LDialogRect := TRectangle.Create(AOverlay);
  LDialogRect.Parent := AOverlay;
  LDialogRect.Align := TAlignLayout.Center;
  LDialogRect.Width := C_BaseDialogWidth;
  LDialogRect.XRadius := FBorderRadius;
  LDialogRect.YRadius := FBorderRadius;
  LDialogRect.Fill.Color := TAlphaColorRec.White;
  LDialogRect.Stroke.Kind := TBrushKind.None;
  LDialogRect.Padding.Rect := RectF(4, 4, 4, 4);
  Result := LDialogRect;
end;

procedure TAndroidDialog.BuildHeader(const ADialogRect: TRectangle);
var
  LLblTitle: TLabel;
begin
  if FTitle = EmptyStr then
    Exit;

  LLblTitle := TLabel.Create(ADialogRect);
  LLblTitle.Parent := ADialogRect;
  LLblTitle.Align := TAlignLayout.Top;
  LLblTitle.Text := FTitle;
  LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
  LLblTitle.Margins.Rect := RectF(16, 12, 16, 4);
  LLblTitle.Height := C_BaseTitleHeight;
  LLblTitle.TextSettings.Font.Size := C_BaseTitleFontSize;
  LLblTitle.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  LLblTitle.VertTextAlign := TTextAlign.Center;
end;

procedure TAndroidDialog.BuildBody(const ADialogRect: TRectangle;
  out AIconPresent: Boolean; out ABodyLayout: TLayout);
var
  LScrollBox    : TVertScrollBox;
  LBodyLayout   : TLayout;
  LIconContainer: TLayout;
  LIconPath     : TPath;
  LblMsg        : TLabel;
  LMsgWidth     : Single;
  LMsgHeight    : Single;
  LMsgFont      : TFont;
begin
  AIconPresent := (FMsgType <> TMultiDialogType.mdtCustom);

  LScrollBox := TVertScrollBox.Create(ADialogRect);
  LScrollBox.Parent := ADialogRect;
  LScrollBox.Align := TAlignLayout.Client;
  LScrollBox.Margins.Rect := RectF(0, 8, 0, 8);
  LScrollBox.ShowScrollBars := True;

  LBodyLayout := TLayout.Create(LScrollBox);
  LBodyLayout.Parent := LScrollBox;
  LBodyLayout.Align := TAlignLayout.Top;
  LBodyLayout.Margins.Rect := RectF(16, 0, 16, 0);

  if AIconPresent then
  begin
    LIconContainer := TLayout.Create(LBodyLayout);
    LIconContainer.Parent := LBodyLayout;
    LIconContainer.Align := TAlignLayout.Left;
    LIconContainer.Width := C_BaseIconSize;
    LIconContainer.Margins.Right := 16;

    LIconPath := TPath.Create(LIconContainer);
    LIconPath.Parent := LIconContainer;
    LIconPath.Align := TAlignLayout.Top;
    LIconPath.Height := C_BaseIconSize;
    LIconPath.Width := C_BaseIconSize;
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
  end;

  if FMessage <> EmptyStr then
  begin
    LblMsg := TLabel.Create(LBodyLayout);
    LblMsg.Parent := LBodyLayout;
    LblMsg.Align := TAlignLayout.Client;
    LblMsg.WordWrap := True;
    LblMsg.Text := FMessage;
    LblMsg.VertTextAlign := TTextAlign.Leading;
    LblMsg.TextSettings.Font.Size := FFontSize;
    LblMsg.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  end;

  if AIconPresent then
    LMsgWidth := C_BaseDialogWidth - 32 - (C_BaseIconSize + 16) - 8
  else
    LMsgWidth := C_BaseDialogWidth - 40;

  LMsgFont := TFont.Create;
  try
    LMsgFont.Size := FFontSize;
    LMsgHeight := CalculateMessageHeight(FMessage, LMsgWidth, LMsgFont);
  finally
    LMsgFont.Free;
  end;

  if AIconPresent then
    LBodyLayout.Height := Max(LMsgHeight, C_BaseIconSize)
  else
    LBodyLayout.Height := LMsgHeight;

  ABodyLayout := LBodyLayout;
end;

procedure TAndroidDialog.BuildButtons(const AOverlay: TLayout;
  const ADialogRect: TRectangle);
var
  LBtnLayout    : TFlowLayout;
  LWidthButtons : Single;
  LCurrentWidth : Single;
  LRec          : TButtonHandler;
  LBtn          : TButton;
  LColorRect    : TRectangle;
  LIsResponsive : Boolean;
  LMarginRight  : Integer;
  LMarginTop    : Integer;
  I             : Integer;
begin
  LBtnLayout := TFlowLayout.Create(ADialogRect);
  LBtnLayout.Parent := ADialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_BaseButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4);
  FBtnLayout := LBtnLayout;

  // Responsive: 4 buttons in portrait — 3+1 layout (3 buttons row 1, 1 full-width row 2)
  LIsResponsive := (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and
                   (Screen.Width < C_BaseResponsiveBreak);

  if LIsResponsive then
  begin
    LBtnLayout.Height := C_BaseButtonsHeight * 2;
    LWidthButtons := Round(C_BaseDialogWidth / 3) - 16;  // = 84 dp (3×84+2×8=268 dp)
  end
  else if FButtonHandlers.Count = 1 then
    LWidthButtons := C_BaseDialogWidth - 32
  else
    LWidthButtons := Round(C_BaseDialogWidth / FButtonHandlers.Count) - 24;

  for I := 0 to FButtonHandlers.Count - 1 do
  begin
    LRec := FButtonHandlers[I];

    if LIsResponsive then
    begin
      if I = 3 then
      begin
        // 4th button: full-width second row (268 dp = 3×84+2×8)
        LCurrentWidth := C_BaseDialogWidth - 32;
        LMarginRight  := 0;
        LMarginTop    := 8;
      end
      else
      begin
        LCurrentWidth := LWidthButtons;
        LMarginRight  := IfThen(I < 2, 8, 0);  // 0,1 → gap; 2 → no gap (end of row)
        LMarginTop    := 0;
      end;
    end
    else
    begin
      LCurrentWidth := LWidthButtons;
      LMarginRight  := IfThen(I < FButtonHandlers.Count - 1, 8, 0);
      LMarginTop    := 0;
    end;

    LRec.Overlay := AOverlay;

    if LRec.Color <> TAlphaColor(0) then
    begin
      // Colored button: canvas-painted TRectangle — avoids Android TLabel/TBrush
      // rendering issue where all buttons inherit the last button's text and color.
      LColorRect := TRectangle.Create(LBtnLayout);
      LColorRect.Parent        := LBtnLayout;
      LColorRect.Fill.Kind     := TBrushKind.None;   // suppress default fill; OnPainting draws it
      LColorRect.Stroke.Kind   := TBrushKind.None;
      LColorRect.XRadius       := 4;
      LColorRect.YRadius       := 4;
      LColorRect.Height        := C_BaseBtnHeight;
      LColorRect.Width         := LCurrentWidth;
      LColorRect.HitTest       := True;
      LColorRect.Margins.Right := LMarginRight;
      LColorRect.Margins.Top   := LMarginTop;

      LColorRect.Tag        := NativeInt(LRec.Color);  // per-instance color for paint handler
      LColorRect.TagString  := LRec.Text;              // per-instance text for paint handler
      LColorRect.OnPainting := PaintColoredBtn;        // self-contained paint

      LColorRect.TagObject  := LRec;                   // click handler (unchanged)
      LColorRect.OnClick    := ButtonClick;
    end
    else
    begin
      // Default or styled button: TButton
      LBtn := TButton.Create(LBtnLayout);
      LBtn.Parent     := LBtnLayout;
      LBtn.Text       := LRec.Text;
      LBtn.TextSettings.Font.Size := FFontSize;
      LBtn.StyledSettings := [TStyledSetting.Style];
      LBtn.Height     := C_BaseBtnHeight;
      LBtn.Width      := LCurrentWidth;
      LBtn.Margins.Right := LMarginRight;
      LBtn.Margins.Top   := LMarginTop;

      if LRec.StyleLookup <> '' then
      begin
        LBtn.StyleLookup    := LRec.StyleLookup;
        LBtn.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style,
                                 TStyledSetting.FontColor, TStyledSetting.Size];
      end;

      LBtn.TagObject := LRec;

      if Assigned(LRec.TapHandler) then
        LBtn.OnTap   := ButtonTap
      else
        LBtn.OnClick := ButtonClick;
    end;
  end;
end;

function TAndroidDialog.CalculateFinalHeight(const ABodyLayout: TLayout;
  const AIconPresent: Boolean): Single;
var
  LMaxScreenHeight: Single;
begin
  Result := ABodyLayout.Height + 16 + FBtnLayout.Height + C_BasePaddingHeight;

  if FTitle <> EmptyStr then
    Result := Result + C_BaseTitleHeight + 16;

  Result := Max(Result, C_BaseMinDialogHeight);

  LMaxScreenHeight := Screen.Size.Height * 0.7;  // 70% in logical dp — no scale division
  if Result > LMaxScreenHeight then
    Result := LMaxScreenHeight;
end;

function TAndroidDialog.CalculateMessageHeight(const AText: string;
  const AWidth: Single; const AFont: TFont): Single;
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
  I          : Integer;
  LChild     : TFmxObject;
  LKeepAlive : IDialogBuilder;
begin
  if not Assigned(AOverlay) then
    Exit;

  LKeepAlive := FKeepAlive;
  FKeepAlive := nil;

  // Nil the overlay reference on each handler.
  // Handlers are owned by FButtonHandlers (TObjectList OwnsObjects=True) — do NOT free here.
  // FMX does NOT free TagObject when destroying controls.
  // Works for both TButton and TRectangle (colored button variant).
  if Assigned(FBtnLayout) then
    for I := 0 to FBtnLayout.ChildrenCount - 1 do
    begin
      LChild := FBtnLayout.Children[I];
      if LChild.TagObject is TButtonHandler then
      begin
        TButtonHandler(LChild.TagObject).Overlay := nil;
        LChild.TagObject := nil;
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
  LObj    : TFmxObject;
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
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
begin
  if not (Sender is TFmxObject) then Exit;
  LFmxObj := TFmxObject(Sender);
  if not (LFmxObj.TagObject is TButtonHandler) then Exit;

  Obj      := TButtonHandler(LFmxObj.TagObject);
  LOverlay := Obj.Overlay;

  // Clear TagObject BEFORE calling handler — prevents use-after-free when
  // CloseDialog destroys the control hierarchy.
  LFmxObj.TagObject := nil;

  try
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);
    if Assigned(Obj.AnonymousHandler) then
      Obj.AnonymousHandler();
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, PointF(0, 0));  // fallback for colored buttons with TapHandler
  finally
    Obj.Overlay := nil;   // handler owned by FButtonHandlers — do NOT free
    CloseDialog(LOverlay);
  end;
end;

procedure TAndroidDialog.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
begin
  if not (Sender is TFmxObject) then Exit;
  LFmxObj := TFmxObject(Sender);
  if not (LFmxObj.TagObject is TButtonHandler) then Exit;

  Obj      := TButtonHandler(LFmxObj.TagObject);
  LOverlay := Obj.Overlay;

  LFmxObj.TagObject := nil;

  try
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, Point);
  finally
    Obj.Overlay := nil;   // handler owned by FButtonHandlers — do NOT free
    CloseDialog(LOverlay);
  end;
end;


procedure TAndroidDialog.PaintColoredBtn(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LRect : TRectangle;
  LColor: TAlphaColor;
  LState: TCanvasSaveState;
begin
  if not (Sender is TRectangle) then
    Exit;

  LRect  := TRectangle(Sender);
  LColor := TAlphaColor(LRect.Tag);

  LState := Canvas.SaveState;
  try
    // Draw colored rounded background
    Canvas.Fill.Kind  := TBrushKind.Solid;
    Canvas.Fill.Color := LColor;
    Canvas.FillRect(ARect, LRect.XRadius, LRect.YRadius, AllCorners, 1.0);

    // Draw white centered text (Canvas.Fill.Color is used as text color by FillText)
    Canvas.Fill.Color := TAlphaColorRec.White;
    Canvas.Font.Size  := FFontSize;
    Canvas.FillText(ARect, LRect.TagString, False, 1.0, [], TTextAlign.Center,
                    TTextAlign.Center);
  finally
    Canvas.RestoreState(LState);
  end;
end;

end.
