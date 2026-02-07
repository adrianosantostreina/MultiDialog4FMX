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
  // Constants for Icon SVG Data
const
  SVG_WARNING = 'M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z';
  SVG_ERROR = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';
  SVG_INFO = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z';
  SVG_QUESTION = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2zm2.07-7.75l-.9.92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1.45-2.1 1.17-2.83l1.24-1.26c.37-.36.59-.86.59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z';

  // Layout Constants
  C_MaxDialogHeight = 400;
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

  // 1. Layout dos botões (Bottom) - Criação Primeiro para garantir Dock no Rodapé
  LBtnLayout := TFlowLayout.Create(LDialogRect);
  LBtnLayout.Parent := LDialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom; // Fixado no rodapé
  LBtnLayout.Height := C_ButtonsHeight;
  LBtnLayout.Justify := TFlowJustify.Center;
  LBtnLayout.JustifyLastLine := TFlowJustify.Center;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4); // Margem inferior 4 (do padding do form)

  // 2. Título (Top)
  if FTitle <> EmptyStr then
  begin
    LLblTitle := TLabel.Create(LDialogRect);
    LLblTitle.Parent := LDialogRect;
    LLblTitle.Align := TAlignLayout.Top;
    LLblTitle.Text := FTitle;
    LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
    LLblTitle.Margins.Rect := RectF(16, 12, 16, 4); // Margem superior
    LLblTitle.Height := C_TitleHeight;
    LLblTitle.TextSettings.Font.Size := 16;
    LLblTitle.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
    LLblTitle.VertTextAlign := TTextAlign.Center;
  end;

  // 3. Corpo (Client) - Usando ScrollBox para permitir rolagem se necessário
  var LScrollBox := TVertScrollBox.Create(LDialogRect);
  LScrollBox.Parent := LDialogRect;
  LScrollBox.Align := TAlignLayout.Client;
  LScrollBox.Margins.Rect := RectF(0, 8, 0, 8); // Margens externas do scroll
  LScrollBox.ShowScrollBars := True;

  // Layout interno que vai crescer o quanto precisar (Conteúdo)
  LBodyLayout := TLayout.Create(LScrollBox);
  LBodyLayout.Parent := LScrollBox;
  LBodyLayout.Align := TAlignLayout.Top; // Ocupa topo do ScrollBox e cresce
  LBodyLayout.Margins.Rect := RectF(16, 0, 16, 0); // Margens internas do conteúdo
  
  LIconWidthUsed := 0;

  // Render Icon if not custom
  if FMsgType <> TMultiDialogType.mdtCustom then
  begin
    // Container do ícone (Left)
    var LIconContainer := TLayout.Create(LBodyLayout);
    LIconContainer.Parent := LBodyLayout;
    LIconContainer.Align := TAlignLayout.Left;
    LIconContainer.Width := C_IconSize;
    LIconContainer.Margins.Right := 16;
    
    LIconPath := TPath.Create(LIconContainer);
    LIconPath.Parent := LIconContainer;
    LIconPath.Align := TAlignLayout.Top;
    LIconPath.Height := C_IconSize;
    LIconPath.Width := C_IconSize;
    // LIconPath.Data.WrapMode := TWrapMode.Fit;
    LIconPath.Stroke.Kind := TBrushKind.None;
    
    // Choose Path Data and Color
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
      mdtConfirmation, mdtQuestion:
      begin
         LIconPath.Data.Data := SVG_QUESTION;
         LIconPath.Fill.Color := TAlphaColorRec.Limegreen;
      end;
    end;

    LIconWidthUsed := C_IconSize + 16; 
  end;

  // Mensagem (Client do BodyLayout)
  LblMsg := TLabel.Create(LBodyLayout);
  LblMsg.Parent := LBodyLayout;
  LblMsg.Align := TAlignLayout.Client; 
  LblMsg.WordWrap := True;
  LblMsg.Text := FMessage;
  LblMsg.VertTextAlign := TTextAlign.Leading; 
  LblMsg.TextSettings.Font.Size := 14;
  LblMsg.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];

  // Calculate Height needed
  if FMsgType <> TMultiDialogType.mdtCustom then
     LRecalcMsgWidth := C_DialogWidth - 32 - (C_IconSize + 16) - 8 // -Margins -Icon -InternalPadding
  else
     LRecalcMsgWidth := C_DialogWidth - 32 - 8;

  LMsgHeight := CalculateMessageHeight(FMessage, LRecalcMsgWidth, LblMsg.TextSettings.Font);
  
  // Height do BodyLayout (Conteúdo real)
  var LBodyHeightNeeded := Max(LMsgHeight, C_IconSize);
  LBodyLayout.Height := LBodyHeightNeeded; // O Layout interno tem altura real do conteúdo
  
  // Calcula Altura Final da Janela (Ideal)
  LFinalHeight := LBodyHeightNeeded + 16 + LBtnLayout.Height + C_PaddingHeight; 
  if FTitle <> EmptyStr then
     LFinalHeight := LFinalHeight + C_TitleHeight + 16;
     
  LFinalHeight := Max(LFinalHeight, C_MinDialogHeight);
  // LFinalHeight := Min(LFinalHeight, C_MaxDialogHeight); // REMOVIDO Limite Fixo antigo

  // Limita a altura a 90% da tela
  var LMaxScreenHeight := Screen.Size.Height * 0.9;
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
  // Regra: Smartphone Portrait (Width < Height e < 600) com 4 botões
  // Linha 1: 3 botões | Linha 2: 1 botão full
  if (FButtonHandlers.Count = 4) and (Screen.Width < Screen.Height) and (Screen.Width < 600) then
  begin
    // Lógica 3+1
    LBtnLayout.Height := C_ButtonsHeight * 2; // Dobra a altura para caber 2 linhas
    LDialogRect.Height := LDialogRect.Height + C_ButtonsHeight; // Ajusta altura total
    
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
