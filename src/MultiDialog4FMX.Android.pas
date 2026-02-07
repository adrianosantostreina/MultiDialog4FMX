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
    procedure InternalShow(const AForm: TCommonCustomForm); override;
    function CalculateMessageHeight(const AText: string; const AWidth: Single; const AFont: TFont): Single;
  private
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
    procedure OnBackgroundClick(Sender: TObject);
    procedure CloseDialog(AOverlay: TLayout);
  end;

implementation

{ TAndroidDialog }

procedure TAndroidDialog.InternalShow(const AForm: TCommonCustomForm);
const
  C_MaxDialogHeight = 400;
  C_MinDialogHeight = 200;
  C_DialogWidth = 300;
  C_TitleHeight = 40;
  C_ButtonsHeight = 56;
  C_PaddingHeight = 32;
var
  LParent  : TCommonCustomForm;
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
  LWidthButtons: Single;

begin
  LParent := ResolveParentForm(AForm);

  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O número mínimo de botões é 1.');

  // Validação extra de máximo (embora o Builder já trate)
  if FButtonHandlers.Count > 4 then
    raise Exception.Create('O número máximo de botões é 4.');

  // Resolve parent form (já resolvido via ResolveParentForm no início do método)
  if not Assigned(LParent) then
    LParent := Application.MainForm;
  if not Assigned(LParent) then
    LParent := Screen.ActiveForm;
  if not Assigned(LParent) then
    raise Exception.Create('Nenhum formulário disponível para exibir o diálogo.');

  // Validação de Regra de Negócio:
  // Se houver 2 ou mais botões, ao menos um deve ter evento associado.
  if FButtonHandlers.Count >= 2 then
  begin
    var LHasEvent := False;
    for LRec in FButtonHandlers do
    begin
      if Assigned(LRec.ClickHandler) or Assigned(LRec.TapHandler) or Assigned(LRec.AnonymousHandler) then
      begin
        LHasEvent := True;
        Break;
      end;
    end;
    
    if not LHasEvent then
      raise Exception.Create('Para diálogos com múltiplos botões, ao menos um deve possuir evento associado.');
  end;

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

  // Lógica Cancelable
  if FCancelable then
  begin
    LBgRect.HitTest := True;
    LBgRect.OnClick := OnBackgroundClick;
  end;

  // Layout dos botões
  LBtnLayout := TFlowLayout.Create(LDialogRect);
  LBtnLayout.Parent := LDialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_ButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 4, 4, 0);

  // Cria botões com Layout Responsivo
  // Regra: Smartphone Portrait (Width < Height e < 600) com 4 botões
  // Linha 1: 3 botões | Linha 2: 1 botão full
  if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and (Screen.Width < 600) then
  begin
    // Lógica 3+1
    LBtnLayout.Height := C_ButtonsHeight * 2; // Dobra a altura para caber 2 linhas
    
    // 3 primeiros botões dividem a largura
    LWidthButtons := (C_DialogWidth / 3) - 16; 
  end
  else if FButtonHandlers.Count = 1 then
  begin
     // Se for apenas 1 botão, ele pode ocupar quase toda a largura
     LWidthButtons := C_DialogWidth - 32;
  end
  else
  begin
    // Lógica padrão para 2 ou 3 botões (ou 4 em landscape)
    LWidthButtons := (C_DialogWidth / FButtonHandlers.Count) - 24;
  end;

  for var I := 0 to FButtonHandlers.Count - 1 do
  begin
    LRec := FButtonHandlers[I];
    LBtn := TButton.Create(LBtnLayout);
    LBtn.Parent := LBtnLayout;
    LBtn.Text := LRec.Text;
    LBtn.TextSettings.Font.Size := 14;
    LBtn.StyledSettings := [TStyledSetting.Style];
    LBtn.Height := 40;
    
    // Ajuste específico para o 4º botão no modo Portrait (Index 3)
    if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and (Screen.Width < 600) and (I = 3) then
    begin
      LBtn.Width := C_DialogWidth - 32; // Full width com margens
      LBtn.Margins.Top := 8; // Espaço extra acima
    end
    else
    begin
      LBtn.Width := LWidthButtons;
    end;

    // Ajuste de margem: Apenas adiciona margem direita se NÃO for o último botão
    if I < FButtonHandlers.Count - 1 then
      LBtn.Margins.Right := 8
    else
      LBtn.Margins.Right := 0;

    LBtn.TintColor := LRec.Color;

    // StyleLookup
    if LRec.StyleLookup <> '' then
    begin
      LBtn.StyleLookup := LRec.StyleLookup;
      // IMPORTANTE: Para usar a cor da fonte do Estilo, precisamos INCLUIR FontColor no StyledSettings.
      // O código anterior removia tudo ou resetava errado. Vamos garantir que o estilo mande.
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

    // Se tiver TapHandler, usa OnTap.
    // Caso contrário (tem ClickHandler OU não tem nada/nil), usa OnClick para garantir fechamento.
    if Assigned(LRec.TapHandler) then
      LBtn.OnTap := ButtonTap
    else
      LBtn.OnClick := ButtonClick;
  end;

  // Altura final ajustada
  LFinalHeight := LMsgHeight + LBtnLayout.Height + C_PaddingHeight; // Usa altura real do layout de botões
  if FTitle <> EmptyStr then
    LFinalHeight := LFinalHeight + C_TitleHeight;
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

procedure TAndroidDialog.CloseDialog(AOverlay: TLayout);
begin
  if Assigned(AOverlay) then
  begin
    AOverlay.Parent := nil;
    {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
    AOverlay.DisposeOf;
    {$ELSE}
    AOverlay.Free;
    {$ENDIF}
  end;
end;

procedure TAndroidDialog.OnBackgroundClick(Sender: TObject);
var
  LObj: TFmxObject;
  LOverlay: TLayout;
begin
  // Tenta encontrar o Overlay subindo na hierarquia ou via Sender
  // O Sender é o LBgRect (TRectangle), o Parent dele é o LOverlay (TLayout)
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
begin
  if (Sender is TButton) and Assigned(TButton(Sender).TagObject) then
  begin
    Obj := TButton(Sender).TagObject as TButtonHandlerObj;
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);
      
    if Assigned(Obj.AnonymousHandler) then
      Obj.AnonymousHandler();

    CloseDialog(Obj.Overlay);

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

    CloseDialog(Obj.Overlay);

    TButton(Sender).TagObject := nil;
    Obj.Free;
  end;
end;


end.
