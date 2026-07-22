unit MultiDialog4FMX.FMX;

interface

uses
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Telemetry,

  FMX.Types,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.TextLayout,
  FMX.Platform,
  FMX.Ani,

  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Threading;

type
  // Config-only builder — nenhum estado visual vive aqui. TDialogBase.Show ja resolve
  // form/valida botoes/monta o TDialogSnapshot/chama EnqueueSnapshot; esta classe nao
  // precisa sobrescrever nada.
  TFMXDialog = class(TDialogBase, IDialogBuilder);

  TFMXDialogInstance = class(TInterfacedObject, IDialogVisualInstance)
  protected
    FSnapshot         : TDialogSnapshot;
    FAlive            : Boolean;
    FDialogRect       : TRectangle;
    FBtnLayout        : TLayout;
    FTimeoutButton    : TFmxObject;
    FTimeoutOrigText  : string;
    FTimeoutRemaining : Integer;
    FTimeoutCancelled : Boolean;
    FResolved         : Boolean;
    FOverlay          : TLayout;
    procedure DoResolve(const AResult: TModalResult);
    function  ResolveIsDark: Boolean;
    procedure ApplyEntranceAnimation(const AOverlay: TLayout;
                                     const ADialogRect: TRectangle);
    procedure ApplyExitAnimation(const AOverlay: TLayout;
                                 const ADialogRect: TRectangle;
                                 const AOnComplete: TProc);
    procedure StartTimeoutCountdown;
    procedure UpdateTimeoutButtonText;
    procedure AutoClickTimeoutButton;
    function  CalculateMessageHeight(const AText: string; const AWidth: Single;
                                     const AFont: TFont): Single;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
    procedure OnBackgroundClick(Sender: TObject);
    procedure CloseDialog(AOverlay: TLayout);
    function  GetPlatformScale: Single;
    function  BuildOverlay(const AParent: TCommonCustomForm;
                           out ABgRect: TRectangle): TLayout;
    function  BuildDialogRect(const AOverlay: TLayout): TRectangle;
    procedure BuildHeader(const ADialogRect: TRectangle);
    procedure BuildBody(const ADialogRect: TRectangle;
                        out AIconPresent: Boolean; out ABodyLayout: TLayout);
    procedure BuildButtons(const AOverlay: TLayout;
                           const ADialogRect: TRectangle);
    function  CalculateFinalHeight(const ABodyLayout: TLayout;
                                   const AIconPresent: Boolean): Single;
    procedure PaintColoredBtn(Sender: TObject; Canvas: TCanvas;
                              const ARect: TRectF);
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    // IDialogVisualInstance
    procedure Show;
    procedure Suppress;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
  end;

implementation

const
  // Icon SVG paths
  SVG_WARNING  = 'M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z';
  SVG_ERROR    = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';
  SVG_INFO     = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z';
  // Literal quebrado em pedacos <=255 chars: Delphi <=11 limita literais de string a 255
  // elementos (E2056); o Delphi 12 removeu o limite. A concatenacao sofre constant folding,
  // entao o valor final e identico em todas as versoes (sem guarda condicional necessaria).
  // Numeros separados por espaco (ex. "-.9 .92" em vez de "-.9.92"): dois numeros com ponto
  // decimal colados sem separador (so a troca de sinal/segundo ponto marca a fronteira) faz
  // o parser de TPathData falhar com EConvertError ("X is not a valid floating point value"),
  // abortando a montagem antes de CalculateFinalHeight rodar (dialogo fica sem altura,
  // botoes "flutuam" fora da caixa). Espaco extra e um separador SVG valido, geometria identica.
  SVG_QUESTION = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2z' +
                 'm2.07-7.75l-.9 .92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1 .45-2.1 1.17-2.83l1.24-1.26c.37-.36 .59-.86 .59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z';
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

{ TFMXDialogInstance }

constructor TFMXDialogInstance.Create(const ASnapshot: TDialogSnapshot);
begin
  inherited Create;
  FSnapshot := ASnapshot;
  FAlive    := True;
end;

destructor TFMXDialogInstance.Destroy;
begin
  FSnapshot.Free;
  inherited;
end;

procedure TFMXDialogInstance.Suppress;
begin
  FAlive := False;
  FTimeoutCancelled := True;
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekSuppressed, FSnapshot, mrNone));
  DoResolve(mrNone);
end;

procedure TFMXDialogInstance.DoResolve(const AResult: TModalResult);
begin
  if FResolved then
    Exit;
  FResolved := True;
  if Assigned(FSnapshot.ResultCallback) then
    FSnapshot.ResultCallback(AResult);
end;

function TFMXDialogInstance.SnapshotId: Integer;
begin
  Result := FSnapshot.Id;
end;

procedure TFMXDialogInstance.CloseWith(const AResult: TModalResult);
begin
  if FResolved or not FAlive then
    Exit;
  DoResolve(AResult);
  if Assigned(FOverlay) then
    CloseDialog(FOverlay);
end;

function TFMXDialogInstance.ResolveIsDark: Boolean;
{$IF CompilerVersion >= 35.0}
// Delphi 11 Alexandria (CV 35.0) introduziu IFMXSystemAppearanceService/TSystemThemeKind
// (FMX.Platform). Em <= 10.4 esses identificadores nao existem (E2003), por isso a var
// e a deteccao do tema do SO ficam isoladas nesta guarda.
var
  LSvc: IFMXSystemAppearanceService;
{$IFEND}
begin
  case FSnapshot.Theme of
    dthDark:  Result := True;
    dthLight: Result := False;
  else
    // dthAuto: consulta o tema do sistema operacional
    {$IF CompilerVersion >= 35.0}
    // Delphi 11+: detecta automaticamente o tema do SO; fallback Light se indisponivel
    if TPlatformServices.Current.SupportsPlatformService(
         IFMXSystemAppearanceService, LSvc) then
      Result := LSvc.ThemeKind = TSystemThemeKind.Dark
    else
      Result := False;
    {$ELSE}
    // Delphi <= 10.4 (CV <= 34): sem servico de tema do SO — assume Light
    Result := False;
    {$IFEND}
  end;
end;

function TFMXDialogInstance.GetPlatformScale: Single;
var
  LScreenSvc: IFMXScreenService;
begin
  Result := 1.0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSvc) then
    Result := LScreenSvc.GetScreenScale;
end;

procedure TFMXDialogInstance.Show;
var
  LOverlay    : TLayout;
  LDialogRect : TRectangle;
  LBgRect     : TRectangle;
  LBodyLayout : TLayout;
  LIconPresent: Boolean;
begin
  LOverlay    := BuildOverlay(FSnapshot.Form, LBgRect);
  FOverlay    := LOverlay;
  LDialogRect := BuildDialogRect(LOverlay);
  BuildButtons(LOverlay, LDialogRect);                         // Bottom — before Client
  BuildHeader(LDialogRect);                                    // Top
  BuildBody(LDialogRect, LIconPresent, LBodyLayout);           // Client

  LDialogRect.Height := CalculateFinalHeight(LBodyLayout, LIconPresent);

  if FSnapshot.Cancelable then
  begin
    LBgRect.HitTest := True;
    LBgRect.OnClick := OnBackgroundClick;
  end;

  // Prepare initial state for entrance animation BEFORE making the dialog visible
  case FSnapshot.Animation of
    danFade:
      LOverlay.Opacity := 0;
    danScale:
    begin
      LDialogRect.Scale.X := 0.8;
      LDialogRect.Scale.Y := 0.8;
    end;
  end;

  if FSnapshot.Animation <> TDialogAnimation.danNone then
    ApplyEntranceAnimation(LOverlay, LDialogRect);
end;

function TFMXDialogInstance.BuildOverlay(const AParent: TCommonCustomForm;
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
  LBgRect.Opacity := IfThen(ResolveIsDark, 0.65, 0.40);
  LBgRect.Stroke.Kind := TBrushKind.None;

  ABgRect := LBgRect;
  Result  := LOverlay;
end;

function TFMXDialogInstance.BuildDialogRect(const AOverlay: TLayout): TRectangle;
var
  LDialogRect: TRectangle;
begin
  LDialogRect := TRectangle.Create(AOverlay);
  LDialogRect.Parent := AOverlay;
  LDialogRect.Align := TAlignLayout.Center;
  LDialogRect.Width := C_BaseDialogWidth;
  LDialogRect.XRadius := FSnapshot.BorderRadius;
  LDialogRect.YRadius := FSnapshot.BorderRadius;
  if ResolveIsDark then
  begin
    LDialogRect.Fill.Color   := $FF2D2D2D;
    LDialogRect.Stroke.Kind  := TBrushKind.Solid;
    LDialogRect.Stroke.Color := $FF444444;
  end
  else
  begin
    LDialogRect.Fill.Color   := TAlphaColorRec.White;
    LDialogRect.Stroke.Kind  := TBrushKind.Solid;
    LDialogRect.Stroke.Color := $FFE0E0E0;
  end;
  LDialogRect.Padding.Rect := RectF(4, 4, 4, 4);
  FDialogRect := LDialogRect;
  Result := LDialogRect;
end;

procedure TFMXDialogInstance.BuildHeader(const ADialogRect: TRectangle);
var
  LLblTitle: TLabel;
begin
  if FSnapshot.Title = EmptyStr then
    Exit;

  LLblTitle := TLabel.Create(ADialogRect);
  LLblTitle.Parent := ADialogRect;
  LLblTitle.Align := TAlignLayout.Top;
  LLblTitle.Text := FSnapshot.Title;
  LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
  LLblTitle.Margins.Rect := RectF(16, 12, 16, 4);
  LLblTitle.Height := C_BaseTitleHeight;
  LLblTitle.TextSettings.Font.Size := C_BaseTitleFontSize;
  if ResolveIsDark then
  begin
    LLblTitle.StyledSettings := [TStyledSetting.Style];
    LLblTitle.TextSettings.FontColor := TAlphaColorRec.White;
  end
  else
    LLblTitle.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  LLblTitle.VertTextAlign := TTextAlign.Center;
end;

procedure TFMXDialogInstance.BuildBody(const ADialogRect: TRectangle;
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
  AIconPresent := (FSnapshot.MsgType <> TMultiDialogType.mdtCustom) or (FSnapshot.CustomSVG <> '');

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

    // --- SVG ---
    // Um path SVG malformado (customizado via SetIcon, ou um builtin futuro) nao pode
    // abortar a montagem do dialogo: CalculateFinalHeight (mais abaixo em Show) so
    // roda se este metodo completar. Sem o guard, uma excecao aqui deixa o LDialogRect sem
    // altura definida e os botoes (montados antes, em BuildButtons) "flutuam" fora da caixa.
    try
      if FSnapshot.CustomSVG <> '' then
        LIconPath.Data.Data := FSnapshot.CustomSVG
      else
        case FSnapshot.MsgType of
          mdtWarning:      LIconPath.Data.Data := SVG_WARNING;
          mdtError:        LIconPath.Data.Data := SVG_ERROR;
          mdtInformation:  LIconPath.Data.Data := SVG_INFO;
          mdtQuestion:     LIconPath.Data.Data := SVG_QUESTION;
          mdtConfirmation: LIconPath.Data.Data := SVG_SUCCESS;
        end;
    except
      // Degrada para "sem icone" (container 40x40 permanece, so o path fica vazio) em vez
      // de propagar e interromper a montagem no meio.
    end;

    // --- Cor ---
    if FSnapshot.CustomIconColor <> TAlphaColor(0) then
      LIconPath.Fill.Color := FSnapshot.CustomIconColor
    else
      case FSnapshot.MsgType of
        mdtWarning:      LIconPath.Fill.Color := TAlphaColorRec.Gold;
        mdtError:        LIconPath.Fill.Color := TAlphaColorRec.Red;
        mdtInformation:  LIconPath.Fill.Color := TAlphaColorRec.Dodgerblue;
        mdtQuestion,
        mdtConfirmation: LIconPath.Fill.Color := TAlphaColorRec.Limegreen;
        mdtCustom:       LIconPath.Fill.Color := TAlphaColorRec.Gray;
      end;
  end;

  if FSnapshot.Message <> EmptyStr then
  begin
    LblMsg := TLabel.Create(LBodyLayout);
    LblMsg.Parent := LBodyLayout;
    LblMsg.Align := TAlignLayout.Client;
    LblMsg.WordWrap := True;
    LblMsg.Text := FSnapshot.Message;
    LblMsg.VertTextAlign := TTextAlign.Leading;
    LblMsg.TextSettings.Font.Size := FSnapshot.FontSize;
    if ResolveIsDark then
    begin
      LblMsg.StyledSettings := [TStyledSetting.Style];
      LblMsg.TextSettings.FontColor := TAlphaColorRec.White;
    end
    else
      LblMsg.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  end;

  if AIconPresent then
    LMsgWidth := C_BaseDialogWidth - 32 - (C_BaseIconSize + 16) - 8
  else
    LMsgWidth := C_BaseDialogWidth - 40;

  LMsgFont := TFont.Create;
  try
    LMsgFont.Size := FSnapshot.FontSize;
    LMsgHeight := CalculateMessageHeight(FSnapshot.Message, LMsgWidth, LMsgFont);
  finally
    LMsgFont.Free;
  end;

  if AIconPresent then
    LBodyLayout.Height := Max(LMsgHeight, C_BaseIconSize)
  else
    LBodyLayout.Height := LMsgHeight;

  ABodyLayout := LBodyLayout;
end;

procedure TFMXDialogInstance.BuildButtons(const AOverlay: TLayout;
  const ADialogRect: TRectangle);
const
  // Box width (300) minus box padding (4+4) minus row margins (4+4) = 284 dp usable width.
  C_RowInnerWidth = C_BaseDialogWidth - 16;
  C_BtnGap        = 8;
var
  // Plain TLayout with MANUAL X/Y positioning. TFlowLayout was used here before, but it
  // paints its children at a vertical offset that differs from the position it reports
  // on FMX <= 10.4 (Delphi 10.3.3): buttons drifted below the rounded bottom of the box
  // while AbsoluteRect still claimed they were inside. Positioning every button by hand
  // from the layout constants is deterministic and renders identically on all Delphi versions.
  LBtnLayout    : TLayout;
  LWidthButtons : Single;
  LCurrentWidth : Single;
  LRec          : TButtonHandler;
  LBtn          : TButton;
  LColorRect    : TRectangle;
  LIsResponsive   : Boolean;
  LEffectiveColor : TAlphaColor;
  LVOffset        : Single;   // vertical centering of a 40 dp button inside a 56 dp band
  LRowTotalW      : Single;
  LCurX           : Single;
  LCurY           : Single;
  I               : Integer;
begin
  LBtnLayout := TLayout.Create(ADialogRect);
  LBtnLayout.Parent := ADialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_BaseButtonsHeight;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4);
  FBtnLayout := LBtnLayout;

  // Responsive: 4 buttons in portrait — 3+1 layout (3 buttons row 1, 1 full-width row 2)
  LIsResponsive := (FSnapshot.Buttons.Count = 4) and (Screen.Width < Screen.Height) and
                   (Screen.Width < C_BaseResponsiveBreak);

  if LIsResponsive then
  begin
    LBtnLayout.Height := C_BaseButtonsHeight * 2;
    LWidthButtons := Round(C_BaseDialogWidth / 3) - 16;  // = 84 dp (3×84+2×8=268 dp)
  end
  else if FSnapshot.Buttons.Count = 1 then
    LWidthButtons := C_BaseDialogWidth - 32
  else
    LWidthButtons := Round(C_BaseDialogWidth / FSnapshot.Buttons.Count) - 24;

  LVOffset := (C_BaseButtonsHeight - C_BaseBtnHeight) / 2;  // = 8 dp

  // Horizontal start of the first row, centered within the usable width.
  if LIsResponsive then
    LRowTotalW := 3 * LWidthButtons + 2 * C_BtnGap            // row 1 = 3 buttons
  else
    LRowTotalW := FSnapshot.Buttons.Count * LWidthButtons +
                  (FSnapshot.Buttons.Count - 1) * C_BtnGap;
  LCurX := (C_RowInnerWidth - LRowTotalW) / 2;
  LCurY := LVOffset;

  FTimeoutButton    := nil;
  FTimeoutOrigText  := '';
  FTimeoutRemaining := 0;
  FTimeoutCancelled := False;

  for I := 0 to FSnapshot.Buttons.Count - 1 do
  begin
    LRec := FSnapshot.Buttons[I];

    if LIsResponsive and (I = 3) then
    begin
      // 4th button: full-width, centered on the second row (band top = 56 dp)
      LCurrentWidth := C_BaseDialogWidth - 32;
      LCurX         := (C_RowInnerWidth - LCurrentWidth) / 2;
      LCurY         := C_BaseButtonsHeight + LVOffset;
    end
    else
      LCurrentWidth := LWidthButtons;

    LRec.Overlay := AOverlay;

    LEffectiveColor := LRec.Color;
    if ResolveIsDark and (LEffectiveColor = TAlphaColor(0)) then
      LEffectiveColor := $FF555555;  // neutral dark-grey for uncoloured buttons in dark mode

    if LEffectiveColor <> TAlphaColor(0) then
    begin
      // Colored button: canvas-painted TRectangle — avoids Android TLabel/TBrush
      // rendering issue where all buttons inherit the last button's text and color.
      LColorRect := TRectangle.Create(LBtnLayout);
      LColorRect.Parent        := LBtnLayout;
      LColorRect.Align         := TAlignLayout.None;
      LColorRect.Fill.Kind     := TBrushKind.None;   // suppress default fill; OnPainting draws it
      LColorRect.Stroke.Kind   := TBrushKind.None;
      LColorRect.XRadius       := 4;
      LColorRect.YRadius       := 4;
      LColorRect.Height        := C_BaseBtnHeight;
      LColorRect.Width         := LCurrentWidth;
      LColorRect.Position.X    := LCurX;
      LColorRect.Position.Y    := LCurY;
      LColorRect.HitTest       := True;

      LColorRect.Tag        := NativeInt(LEffectiveColor);  // per-instance color for paint handler
      LColorRect.TagString  := LRec.Text;              // per-instance text for paint handler
      LColorRect.OnPainting := PaintColoredBtn;        // self-contained paint

      LColorRect.TagObject  := LRec;                   // click handler (unchanged)
      LColorRect.OnClick    := ButtonClick;

      if LRec.Timeout > 0 then
      begin
        FTimeoutButton    := LColorRect;
        FTimeoutOrigText  := LRec.Text;
        FTimeoutRemaining := LRec.Timeout;
        FTimeoutCancelled := False;
      end;
    end
    else
    begin
      // Default or styled button: TButton
      LBtn := TButton.Create(LBtnLayout);
      LBtn.Parent     := LBtnLayout;
      LBtn.Align      := TAlignLayout.None;
      LBtn.Text       := LRec.Text;
      LBtn.TextSettings.Font.Size := FSnapshot.FontSize;
      LBtn.StyledSettings := [TStyledSetting.Style];
      LBtn.Height     := C_BaseBtnHeight;
      LBtn.Width      := LCurrentWidth;
      LBtn.Position.X := LCurX;
      LBtn.Position.Y := LCurY;

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

      if LRec.Timeout > 0 then
      begin
        FTimeoutButton    := LBtn;
        FTimeoutOrigText  := LRec.Text;
        FTimeoutRemaining := LRec.Timeout;
        FTimeoutCancelled := False;
      end;
    end;

    // Advance X for the next button in the same (first) row.
    if not (LIsResponsive and (I = 3)) then
      LCurX := LCurX + LCurrentWidth + C_BtnGap;
  end;

  if Assigned(FTimeoutButton) then
    StartTimeoutCountdown;
end;

function TFMXDialogInstance.CalculateFinalHeight(const ABodyLayout: TLayout;
  const AIconPresent: Boolean): Single;
var
  LMaxScreenHeight: Single;
begin
  Result := ABodyLayout.Height + 16 + FBtnLayout.Height + C_BasePaddingHeight;

  if FSnapshot.Title <> EmptyStr then
    Result := Result + C_BaseTitleHeight + 16;

  Result := Max(Result, C_BaseMinDialogHeight);

  LMaxScreenHeight := Screen.Size.Height * 0.7;  // 70% in logical dp — no scale division
  if Result > LMaxScreenHeight then
    Result := LMaxScreenHeight;
end;

function TFMXDialogInstance.CalculateMessageHeight(const AText: string;
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

procedure TFMXDialogInstance.CloseDialog(AOverlay: TLayout);
var
  I           : Integer;
  LChild      : TFmxObject;
  LDialogRect : TRectangle;
  LSelf       : IDialogVisualInstance; // keeps Self alive until the deferred destroy runs
  LForm       : TCommonCustomForm;
  LDoDestroy  : TProc;
begin
  // Defense in depth: if the instance was already Suppress()-ed (form destroyed while
  // this call was queued), FAlive is False and nothing below is safe to touch — bail
  // out before dereferencing FBtnLayout/FDialogRect/FSnapshot. This does NOT by itself
  // protect against Self having already been deallocated (that requires a keepalive at
  // the call site — see ButtonClick/ButtonTap/OnBackgroundClick), it only protects the
  // remaining timing gap once we know we're still alive to run.
  if not FAlive then
    Exit;

  if not Assigned(AOverlay) then
    Exit;

  FTimeoutCancelled := True;
  FTimeoutButton    := nil;

  LSelf       := Self;
  LDialogRect := FDialogRect;
  FDialogRect := nil;
  LForm       := FSnapshot.Form;

  // Nil the overlay reference on each handler.
  // Handlers are owned by FSnapshot.Buttons (TObjectList OwnsObjects=True) — do NOT free here.
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

  LDoDestroy := procedure
  begin
    if not FAlive then
      Exit; // form ja foi destruido (Suppress rodou) — overlay/objetos ja podem ter sido liberados
    AOverlay.Parent := nil;
    {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
    AOverlay.DisposeOf;
    {$ELSE}
    AOverlay.Free;
    {$ENDIF}
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekClosed, FSnapshot, mrNone));
    TDialogQueueManager.Instance.NotifyClosed(LForm);
    LSelf := nil;
  end;

  if FSnapshot.Animation = TDialogAnimation.danNone then
    LDoDestroy()
  else
    ApplyExitAnimation(AOverlay, LDialogRect, LDoDestroy);
end;

procedure TFMXDialogInstance.OnBackgroundClick(Sender: TObject);
var
  LObj    : TFmxObject;
  LOverlay: TLayout;
  LSelf   : IDialogVisualInstance; // keeps Self alive until the deferred closure runs
begin
  if Sender is TFmxObject then
  begin
    LObj := TFmxObject(Sender).Parent;
    if LObj is TLayout then
    begin
      LOverlay := TLayout(LObj);
      TDialogTelemetry.Emit(MakeDialogEventInfo(dekCancelled, FSnapshot, mrCancel));
      DoResolve(mrCancel);
      // Defer CloseDialog: destroying the overlay inside a click handler leaves
      // the Win32 message pump in an inconsistent state (mouse capture stuck).
      // ForceQueue schedules execution after the current event returns.
      // LSelf is captured by the closure — this is what keeps Self (and therefore
      // the FMX controls CloseDialog is about to dereference) alive if the owning
      // form is destroyed before this closure runs; CloseDialog's own FAlive guard
      // then makes it a safe no-op in that case instead of touching freed memory.
      LSelf := Self;
      TThread.ForceQueue(nil, procedure begin if Assigned(LSelf) then CloseDialog(LOverlay); end);
    end;
  end;
end;

procedure TFMXDialogInstance.ButtonClick(Sender: TObject);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
  LSelf   : IDialogVisualInstance; // keeps Self alive until the deferred closure runs
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
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekButtonClicked, FSnapshot, Obj.ModalResult));
    DoResolve(Obj.ModalResult);
  finally
    Obj.Overlay := nil;   // handler owned by FSnapshot.Buttons — do NOT free
    // See OnBackgroundClick for why LSelf must be captured by the closure.
    LSelf := Self;
    TThread.ForceQueue(nil, procedure begin if Assigned(LSelf) then CloseDialog(LOverlay); end);
  end;
end;

procedure TFMXDialogInstance.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
  LSelf   : IDialogVisualInstance; // keeps Self alive until the deferred closure runs
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
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekButtonClicked, FSnapshot, Obj.ModalResult));
    DoResolve(Obj.ModalResult);
  finally
    Obj.Overlay := nil;   // handler owned by FSnapshot.Buttons — do NOT free
    // See OnBackgroundClick for why LSelf must be captured by the closure.
    LSelf := Self;
    TThread.ForceQueue(nil, procedure begin if Assigned(LSelf) then CloseDialog(LOverlay); end);
  end;
end;

procedure TFMXDialogInstance.PaintColoredBtn(Sender: TObject; Canvas: TCanvas;
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
    Canvas.Font.Size  := FSnapshot.FontSize;
    Canvas.FillText(ARect, LRect.TagString, False, 1.0, [], TTextAlign.Center,
                    TTextAlign.Center);
  finally
    Canvas.RestoreState(LState);
  end;
end;

{ Animações }

procedure TFMXDialogInstance.ApplyEntranceAnimation(const AOverlay: TLayout;
  const ADialogRect: TRectangle);
var
  LSelf: IDialogVisualInstance; // keeps Self alive until the deferred closure runs — same
                                 // reasoning as OnBackgroundClick/ButtonClick/ButtonTap/
                                 // StartTimeoutCountdown: without this, "if not FAlive then
                                 // Exit" below would itself be a use-after-free if the form
                                 // is destroyed before this ForceQueue closure runs.
begin
  LSelf := Self;
  case FSnapshot.Animation of
    danFade:
      TThread.ForceQueue(nil, procedure
      begin
        if not Assigned(LSelf) or not FAlive then Exit;
        TAnimator.AnimateFloat(AOverlay, 'Opacity', 1, 0.25);
      end);

    danScale:
    begin
      TAnimator.AnimateFloat(ADialogRect, 'Scale.X', 1.0, 0.3,
        TAnimationType.Out, TInterpolationType.Back);
      TAnimator.AnimateFloat(ADialogRect, 'Scale.Y', 1.0, 0.3,
        TAnimationType.Out, TInterpolationType.Back);
    end;

    danSlide:
      TThread.ForceQueue(nil, procedure
      var
        LTargetY: Single;
      begin
        if not Assigned(LSelf) or not FAlive then Exit;
        LTargetY           := ADialogRect.Position.Y;
        ADialogRect.Align  := TAlignLayout.None;
        ADialogRect.Position.X := (AOverlay.Width  - ADialogRect.Width)  / 2;
        ADialogRect.Position.Y := -ADialogRect.Height;
        TAnimator.AnimateFloat(ADialogRect, 'Position.Y', LTargetY, 0.3,
          TAnimationType.Out, TInterpolationType.Quadratic);
      end);
  end;
end;

procedure TFMXDialogInstance.ApplyExitAnimation(const AOverlay: TLayout;
  const ADialogRect: TRectangle; const AOnComplete: TProc);
// TFloatAnimation.OnFinish is TNotifyEvent (method pointer) — incompatible with
// anonymous TProc. We start the visual animations and then fire AOnComplete from
// a background thread after the matching duration so the overlay is already
// invisible when it gets freed.
var
  LDurationMs: Integer;
begin
  case FSnapshot.Animation of
    danFade:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.2);
      LDurationMs := 220;
    end;

    danScale:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.2);
      if Assigned(ADialogRect) then
      begin
        TAnimator.AnimateFloat(ADialogRect, 'Scale.X', 0.8, 0.2);
        TAnimator.AnimateFloat(ADialogRect, 'Scale.Y', 0.8, 0.2);
      end;
      LDurationMs := 220;
    end;

    danSlide:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.25);
      if Assigned(ADialogRect) then
      begin
        if ADialogRect.Align = TAlignLayout.Center then
        begin
          ADialogRect.Align      := TAlignLayout.None;
          ADialogRect.Position.X := (AOverlay.Width - ADialogRect.Width) / 2;
        end;
        TAnimator.AnimateFloat(ADialogRect, 'Position.Y', AOverlay.Height, 0.25);
      end;
      LDurationMs := 270;
    end;
  else
    LDurationMs := 0;
  end;

  // Fire the destroy callback after the animation completes.
  // Sleep runs on a pool thread; ForceQueue marshals back to the UI thread.
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(LDurationMs);
    TThread.ForceQueue(nil, procedure begin AOnComplete; end);
  end).Start;
end;

{ Timeout countdown }

procedure TFMXDialogInstance.StartTimeoutCountdown;
var
  LSelf: IDialogVisualInstance; // keeps Self alive for the whole life of the background
                                 // thread + its queued closures — without this, the thread
                                 // body/inner TThread.Queue closure below reference Self's
                                 // fields via a raw pointer with no refcount, so a form
                                 // destroyed (and Suppress-ed) mid-countdown would free the
                                 // instance out from under this thread.
begin
  UpdateTimeoutButtonText;  // mostra "(N)" imediatamente
  LSelf := Self;
  TThread.CreateAnonymousThread(procedure
  begin
    if not Assigned(LSelf) then
      Exit;
    while (FTimeoutRemaining > 0) and not FTimeoutCancelled do
    begin
      Sleep(1000);
      if FTimeoutCancelled then
        Exit;
      TThread.Queue(nil, procedure
      begin
        if not Assigned(LSelf) or FTimeoutCancelled then
          Exit;
        Dec(FTimeoutRemaining);
        if FTimeoutRemaining > 0 then
          UpdateTimeoutButtonText
        else
          AutoClickTimeoutButton;
      end);
    end;
  end).Start;
end;

procedure TFMXDialogInstance.UpdateTimeoutButtonText;
begin
  if not Assigned(FTimeoutButton) then
    Exit;

  if FTimeoutButton is TButton then
    TButton(FTimeoutButton).Text := FTimeoutOrigText + ' (' + FTimeoutRemaining.ToString + ')'
  else if FTimeoutButton is TRectangle then
  begin
    TRectangle(FTimeoutButton).TagString := FTimeoutOrigText + ' (' + FTimeoutRemaining.ToString + ')';
    TRectangle(FTimeoutButton).Repaint;
  end;
end;

procedure TFMXDialogInstance.AutoClickTimeoutButton;
begin
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekTimedOut, FSnapshot, mrNone));
  if Assigned(FTimeoutButton) then
    ButtonClick(FTimeoutButton);
end;

initialization
  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFMXDialogInstance.Create(ASnapshot);
    end);

end.
