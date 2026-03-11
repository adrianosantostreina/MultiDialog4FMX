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
  LBtnLayout   : TFlowLayout;
  LWidthButtons: Single;
  LRec         : TButtonHandler;
  LBtn         : TButton;
  I            : Integer;
begin
  LBtnLayout := TFlowLayout.Create(ADialogRect);
  LBtnLayout.Parent := ADialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_BaseButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4);
  FBtnLayout := LBtnLayout;

  // Responsive: 4 buttons in portrait — use 2 rows
  if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and
     (Round(Screen.Width / GetPlatformScale) < C_BaseResponsiveBreak) then
  begin
    LBtnLayout.Height := C_BaseButtonsHeight * 2;
    LWidthButtons := Round(C_BaseDialogWidth / 3) - 16;
  end
  else if FButtonHandlers.Count = 1 then
    LWidthButtons := C_BaseDialogWidth - 32
  else
    LWidthButtons := Round(C_BaseDialogWidth / FButtonHandlers.Count) - 24;

  for I := 0 to FButtonHandlers.Count - 1 do
  begin
    LRec := FButtonHandlers[I];
    LBtn := TButton.Create(LBtnLayout);
    LBtn.Parent := LBtnLayout;
    LBtn.Text := LRec.Text;
    LBtn.TextSettings.Font.Size := FFontSize;
    LBtn.StyledSettings := [TStyledSetting.Style];
    LBtn.Height := C_BaseBtnHeight;

    if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and
       (Round(Screen.Width / GetPlatformScale) < C_BaseResponsiveBreak) and (I = 3) then
    begin
      LBtn.Width := C_BaseDialogWidth - 32;
      LBtn.Margins.Top := 8;
    end
    else
      LBtn.Width := LWidthButtons;

    if I < FButtonHandlers.Count - 1 then
      LBtn.Margins.Right := 8
    else
      LBtn.Margins.Right := 0;

    LBtn.TintColor := LRec.Color;

    if LRec.StyleLookup <> '' then
    begin
      LBtn.StyleLookup := LRec.StyleLookup;
      LBtn.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style,
                               TStyledSetting.FontColor, TStyledSetting.Size];
    end;

    // R7: assign overlay directly to handler record (no separate TButtonHandlerObj)
    LRec.Overlay := AOverlay;
    LBtn.TagObject := LRec;

    if Assigned(LRec.TapHandler) then
      LBtn.OnTap := ButtonTap
    else
      LBtn.OnClick := ButtonClick;
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

  LMaxScreenHeight := Round(Screen.Size.Height / GetPlatformScale) * 0.9;
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
  LBtn       : TButton;
  LObj       : TButtonHandler;
  LKeepAlive : IDialogBuilder;
begin
  if not Assigned(AOverlay) then
    Exit;

  LKeepAlive := FKeepAlive;
  FKeepAlive := nil;

  // Nil the overlay reference on each handler.
  // Handlers are owned by FButtonHandlers (TObjectList OwnsObjects=True) — do NOT free here.
  // FMX does NOT free TagObject when destroying controls.
  if Assigned(FBtnLayout) then
    for I := 0 to FBtnLayout.ChildrenCount - 1 do
      if FBtnLayout.Children[I] is TButton then
      begin
        LBtn := TButton(FBtnLayout.Children[I]);
        if Assigned(LBtn.TagObject) then
        begin
          LObj := LBtn.TagObject as TButtonHandler;
          LBtn.TagObject := nil;
          LObj.Overlay := nil;
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
begin
  if not ((Sender is TButton) and Assigned(TButton(Sender).TagObject)) then
    Exit;

  Obj := TButton(Sender).TagObject as TButtonHandler;
  LOverlay := Obj.Overlay;

  // Clear TagObject BEFORE calling handler — prevents use-after-free when
  // CloseDialog destroys the control hierarchy.
  TButton(Sender).TagObject := nil;

  try
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);
    if Assigned(Obj.AnonymousHandler) then
      Obj.AnonymousHandler();
  finally
    Obj.Overlay := nil;   // handler owned by FButtonHandlers — do NOT free
    CloseDialog(LOverlay);
  end;
end;

procedure TAndroidDialog.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
begin
  if not ((Sender is TButton) and Assigned(TButton(Sender).TagObject)) then
    Exit;

  Obj := TButton(Sender).TagObject as TButtonHandler;
  LOverlay := Obj.Overlay;

  TButton(Sender).TagObject := nil;

  try
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, Point);
  finally
    Obj.Overlay := nil;   // handler owned by FButtonHandlers — do NOT free
    CloseDialog(LOverlay);
  end;
end;


end.
