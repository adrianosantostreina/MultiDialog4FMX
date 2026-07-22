unit MultiDialog4FMX.Tests.Android;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.FMX,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Interfaces,
  FMX.Types,
  FMX.Graphics,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects,
  FMX.StdCtrls,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;

type
  // Expõe os membros protected de TFMXDialogInstance para os testes — mesmo padrão
  // "Cracker" já usado no projeto (protected é acessível via subclasse, mesmo em
  // outra unit, regra de visibilidade do Object Pascal).
  TFMXDialogInstanceCracker = class(TFMXDialogInstance);

  [TestFixture]
  TAndroidDialogTests = class
  public
    function MakeSnapshot(const AForm: TCommonCustomForm; const ATitle, AMessage: string;
      const AMsgType: TMultiDialogType; const AButtonCount: Integer;
      const ACustomSVG: string = ''; const ACustomIconColor: TAlphaColor = 0;
      const AFontSize: Single = 14; const ABorderRadius: Single = 12;
      const AResultCallback: TDialogResultProc = nil): TDialogSnapshot;

    [Test]
    procedure TestCalculateMessageHeight_ShortText;

    [Test]
    procedure TestCalculateMessageHeight_LongText;

    [Test]
    procedure TestCalculateMessageHeight_MultiLine;

    [Test]
    procedure TestCalculateMessageHeight_EmptyText;

    [Test]
    procedure TestShow_SubMethodsRun_FBtnLayoutAssigned;

    [Test]
    procedure TestBuildOverlay_HasContentsAlign;

    [Test]
    procedure TestBuildDialogRect_WidthIsCorrect;

    [Test]
    procedure TestBuildDialogRect_UsesBorderRadius;

    [Test]
    procedure TestBuildButtons_ChildCountMatchesHandlers;

    [Test]
    procedure TestCalculateFinalHeight_WithTitle_GreaterThanWithout;

    [Test]
    procedure TestBuildBody_UsesFontSize;

    [Test]
    procedure TestBuildBody_CustomSVG_IconPresent;

    [Test]
    procedure TestBuildBody_NoSVG_CustomType_NotPresent;

    [Test]
    procedure TestBuildBody_CustomSVG_PathDataSet;

    [Test]
    procedure TestBuildBody_CustomIconColor_Applied;

    [Test]
    procedure TestBuildBody_CustomSVG_TypeColor_Fallback;
  end;

  [TestFixture]
  TAndroidDialogCloseTests = class
  private
    FHandlerCallCount: Integer;
    FHandlerSender: TObject;
    procedure OnClickHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure TestButtonClick_CallsHandlerAndClearsTagObject;

    [Test]
    procedure TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;

    [Test]
    procedure TestCloseDialog_FreesAllRemainingTagObjects;

    [Test]
    procedure TestButtonClick_InvokesResultCallback;

    [Test]
    procedure TestButtonClick_NoCallback_NoException;

    [Test]
    procedure TestOnBackgroundClick_CallbackWithMrCancel;

    [Test]
    procedure TestButtonClick_CallbackBeforeClose;

    [Test]
    procedure TestDoResolve_IsIdempotent;
  end;

implementation

function TAndroidDialogTests.MakeSnapshot(const AForm: TCommonCustomForm;
  const ATitle, AMessage: string; const AMsgType: TMultiDialogType;
  const AButtonCount: Integer; const ACustomSVG: string;
  const ACustomIconColor: TAlphaColor; const AFontSize, ABorderRadius: Single;
  const AResultCallback: TDialogResultProc): TDialogSnapshot;
var
  LButtons: TButtonHandlerList;
  I: Integer;
begin
  LButtons := TButtonHandlerList.Create(True);
  try
    for I := 1 to AButtonCount do
      LButtons.Add(TButtonHandler.Create);
    Result := TDialogSnapshot.Create(AForm, ATitle, AMessage, AMsgType, False,
      AFontSize, ABorderRadius, TDialogAnimation.danNone, TDialogTheme.dthAuto,
      ACustomSVG, ACustomIconColor, AResultCallback, LButtons);
  finally
    LButtons.Free; // TDialogSnapshot.Create ja fez sua propria copia profunda
  end;
end;

{ TAndroidDialogTests }

procedure TAndroidDialogTests.TestCalculateMessageHeight_ShortText;
var
  Instance: TFMXDialogInstanceCracker;
  Height: Single;
  Font: TFont;
begin
  Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, '', '', mdtCustom, 0));
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := Instance.CalculateMessageHeight('Test', 300, Font);
    Assert.IsTrue(Height > 0, 'Height should be greater than 0');
    Assert.IsTrue(Height < 100, 'Short text should have small height');
  finally
    Font.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_LongText;
var
  Instance: TFMXDialogInstanceCracker;
  Height: Single;
  Font: TFont;
  LongText: string;
begin
  Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, '', '', mdtCustom, 0));
  Font := TFont.Create;
  try
    Font.Size := 14;
    LongText := 'This is a very long text that should wrap into multiple lines when displayed in the dialog. ' +
                'It contains enough characters to test the wrapping functionality of the message calculation.';
    Height := Instance.CalculateMessageHeight(LongText, 300, Font);
    Assert.IsTrue(Height > 50, 'Long text should have greater height');
  finally
    Font.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_MultiLine;
var
  Instance: TFMXDialogInstanceCracker;
  Height: Single;
  Font: TFont;
  MultiLineText: string;
begin
  Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, '', '', mdtCustom, 0));
  Font := TFont.Create;
  try
    Font.Size := 14;
    MultiLineText := 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3';
    Height := Instance.CalculateMessageHeight(MultiLineText, 300, Font);
    Assert.IsTrue(Height > 0, 'Multi-line text should have height');
  finally
    Font.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_EmptyText;
var
  Instance: TFMXDialogInstanceCracker;
  Height: Single;
  Font: TFont;
begin
  Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, '', '', mdtCustom, 0));
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := Instance.CalculateMessageHeight('', 300, Font);
    Assert.IsTrue(Height >= 0, 'Empty text should have non-negative height');
  finally
    Font.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogTests.TestShow_SubMethodsRun_FBtnLayoutAssigned;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(LForm, '', '', mdtCustom, 1));
    Instance.Show;
    try
      Assert.IsNotNull(Instance.FBtnLayout, 'FBtnLayout deve estar atribuido apos Show');
    finally
      Instance.CloseDialog(TLayout(Instance.FBtnLayout.Parent.Parent));
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildOverlay_HasContentsAlign;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBgRect: TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(LForm, '', '', mdtCustom, 0));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      Assert.AreEqual(TAlignLayout.Contents, LOverlay.Align,
        'Overlay deve ter Align = Contents');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildDialogRect_WidthIsCorrect;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(LForm, '', '', mdtCustom, 0));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Assert.IsTrue(Abs(LDialogRect.Width - 300) < 1,
        'Width deve ser 300 (logical points, sem multiplicar por scale)');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildDialogRect_UsesBorderRadius;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', '', mdtCustom, 0, '', 0, 14, 8));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Assert.AreEqual(Single(8), LDialogRect.XRadius,
        'XRadius deve refletir o BorderRadius configurado');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildButtons_ChildCountMatchesHandlers;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(LForm, '', '', mdtCustom, 3));
    LOverlay    := Instance.BuildOverlay(LForm, LBgRect);
    LDialogRect := Instance.BuildDialogRect(LOverlay);
    Instance.BuildButtons(LOverlay, LDialogRect);

    Assert.AreEqual(3, Instance.FBtnLayout.ChildrenCount,
      '3 handlers deve gerar 3 botoes em FBtnLayout');

    Instance.CloseDialog(LOverlay);
    Instance := nil;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateFinalHeight_WithTitle_GreaterThanWithout;
var
  Instance: TFMXDialogInstanceCracker;
  LBodyLayout: TLayout;
  LBtnLayoutMock: TLayout;
  LHeightWithTitle, LHeightNoTitle: Single;
begin
  LBodyLayout := TLayout.Create(nil);
  LBodyLayout.Height := 100;
  LBtnLayoutMock := TLayout.Create(nil);
  LBtnLayoutMock.Height := 56;

  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, 'Test Title', '', mdtCustom, 0));
    Instance.FBtnLayout := LBtnLayoutMock;
    LHeightWithTitle := Instance.CalculateFinalHeight(LBodyLayout, False);
    Instance := nil;

    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(nil, '', '', mdtCustom, 0));
    Instance.FBtnLayout := LBtnLayoutMock;
    LHeightNoTitle := Instance.CalculateFinalHeight(LBodyLayout, False);
    Instance := nil;

    Assert.IsTrue(LHeightWithTitle > LHeightNoTitle,
      'Dialog com titulo deve ser mais alto que sem titulo');
  finally
    LBtnLayoutMock.Free;
    LBodyLayout.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_UsesFontSize;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
  LLabel: TLabel;
  I: Integer;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', 'Test message', mdtCustom, 0, '', 0, 18));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      LLabel := nil;
      for I := 0 to LBodyLayout.ChildrenCount - 1 do
        if LBodyLayout.Children[I] is TLabel then
        begin
          LLabel := TLabel(LBodyLayout.Children[I]);
          Break;
        end;

      Assert.IsNotNull(LLabel, 'LBodyLayout deve conter um TLabel de mensagem');
      Assert.AreEqual(Single(18), LLabel.TextSettings.Font.Size,
        'Font.Size do label deve ser 18 conforme configurado no snapshot');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_CustomSVG_IconPresent;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', '', mdtCustom, 0, 'M1 2 L3 4'));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      Assert.IsTrue(LIconPresent,
        'AIconPresent deve ser True quando CustomSVG <> '''' com mdtCustom');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_NoSVG_CustomType_NotPresent;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(MakeSnapshot(LForm, '', '', mdtCustom, 0));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      Assert.IsFalse(LIconPresent,
        'AIconPresent deve ser False quando mdtCustom sem SVG customizado');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_CustomSVG_PathDataSet;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
  LPath: TPath;
  I: Integer;
  LIconContainer: TLayout;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', '', mdtCustom, 0, 'M1 2 L3 4'));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      LPath := nil;
      for I := 0 to LBodyLayout.ChildrenCount - 1 do
        if LBodyLayout.Children[I] is TLayout then
        begin
          LIconContainer := TLayout(LBodyLayout.Children[I]);
          if (LIconContainer.ChildrenCount > 0) and
             (LIconContainer.Children[0] is TPath) then
          begin
            LPath := TPath(LIconContainer.Children[0]);
            Break;
          end;
        end;

      Assert.IsNotNull(LPath, 'TPath deve existir quando CustomSVG <> ''''');
      Assert.IsTrue(LPath.Data.Data <> '',
        'TPath.Data.Data nao deve ser vazio quando CustomSVG foi definido');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_CustomIconColor_Applied;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
  LPath: TPath;
  I: Integer;
  LIconContainer: TLayout;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', '', mdtWarning, 0, '', TAlphaColorRec.Purple));
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      LPath := nil;
      for I := 0 to LBodyLayout.ChildrenCount - 1 do
        if LBodyLayout.Children[I] is TLayout then
        begin
          LIconContainer := TLayout(LBodyLayout.Children[I]);
          if (LIconContainer.ChildrenCount > 0) and
             (LIconContainer.Children[0] is TPath) then
          begin
            LPath := TPath(LIconContainer.Children[0]);
            Break;
          end;
        end;

      Assert.IsNotNull(LPath, 'TPath deve existir para mdtWarning');
      Assert.AreEqual(TAlphaColorRec.Purple, LPath.Fill.Color,
        'Fill.Color deve ser Purple (CustomIconColor tem prioridade)');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestBuildBody_CustomSVG_TypeColor_Fallback;
var
  LForm: TCommonCustomForm;
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LDialogRect: TRectangle;
  LBgRect: TRectangle;
  LIconPresent: Boolean;
  LBodyLayout: TLayout;
  LPath: TPath;
  I: Integer;
  LIconContainer: TLayout;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    Instance := TFMXDialogInstanceCracker.Create(
      MakeSnapshot(LForm, '', '', mdtWarning, 0, 'M1 2 L3 4'));
    // CustomIconColor = 0 (default)
    LOverlay := Instance.BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := Instance.BuildDialogRect(LOverlay);
      Instance.BuildBody(LDialogRect, LIconPresent, LBodyLayout);

      LPath := nil;
      for I := 0 to LBodyLayout.ChildrenCount - 1 do
        if LBodyLayout.Children[I] is TLayout then
        begin
          LIconContainer := TLayout(LBodyLayout.Children[I]);
          if (LIconContainer.ChildrenCount > 0) and
             (LIconContainer.Children[0] is TPath) then
          begin
            LPath := TPath(LIconContainer.Children[0]);
            Break;
          end;
        end;

      Assert.IsNotNull(LPath, 'TPath deve existir');
      Assert.AreEqual(TAlphaColorRec.Gold, LPath.Fill.Color,
        'Cor deve ser Gold (cor do tipo mdtWarning) quando CustomIconColor = 0');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
      Instance := nil;
    end;
  finally
    LForm.Free;
  end;
end;

{ TAndroidDialogCloseTests }

procedure TAndroidDialogCloseTests.Setup;
begin
  FHandlerCallCount := 0;
  FHandlerSender := nil;
end;

procedure TAndroidDialogCloseTests.OnClickHandler(Sender: TObject);
begin
  Inc(FHandlerCallCount);
  FHandlerSender := Sender;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_CallsHandlerAndClearsTagObject;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: TButton;
  LObj: TButtonHandler;
  LButtons: TButtonHandlerList;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0, nil, LButtons));
  LButtons.Free;

  LObj := TButtonHandler.Create;
  try
    LObj.ClickHandler := OnClickHandler;
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    Instance.FBtnLayout := LBtnLayout;
    Instance.ButtonClick(LBtn);

    Assert.AreEqual(1, FHandlerCallCount, 'ClickHandler deve ter sido chamado exatamente 1 vez');
    Assert.IsNull(LObj.Overlay, 'Overlay deve ser nil apos ButtonClick');
  finally
    LObj.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: TButton;
  LObj: TButtonHandler;
  LButtons: TButtonHandlerList;
  LExceptionPropagated: Boolean;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0, nil, LButtons));
  LButtons.Free;

  LObj := TButtonHandler.Create;
  try
    LObj.AnonymousHandler :=
      procedure
      begin
        raise Exception.Create('Erro simulado no handler');
      end;
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    Instance.FBtnLayout := LBtnLayout;

    LExceptionPropagated := False;
    try
      Instance.ButtonClick(LBtn);
    except
      on E: Exception do
        if E.Message = 'Erro simulado no handler' then
          LExceptionPropagated := True;
    end;
    Assert.IsTrue(LExceptionPropagated, 'A excecao do handler deve propagar');
    Assert.IsNull(LObj.Overlay, 'Overlay deve ser nil mesmo apos excecao no handler');
  finally
    LObj.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestCloseDialog_FreesAllRemainingTagObjects;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: array[0..2] of TButton;
  LObj: array[0..2] of TButtonHandler;
  I: Integer;
  LTagNilCount: Integer;
  LButtons: TButtonHandlerList;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;

  for I := 0 to 2 do
  begin
    LBtn[I] := TButton.Create(LBtnLayout);
    LBtn[I].Parent := LBtnLayout;
    LObj[I] := TButtonHandler.Create;
    LObj[I].Overlay := LOverlay;
    LBtn[I].TagObject := LObj[I];
  end;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0, nil, LButtons));
  LButtons.Free;
  Instance.FBtnLayout := LBtnLayout;

  LTagNilCount := 0;
  for I := 0 to LBtnLayout.ChildrenCount - 1 do
    if (LBtnLayout.Children[I] is TButton) and
       not Assigned(TButton(LBtnLayout.Children[I]).TagObject) then
      Inc(LTagNilCount);
  Assert.AreEqual(0, LTagNilCount,
    'Antes do CloseDialog todos os TagObjects devem estar atribuidos');

  Instance.CloseDialog(LOverlay);

  Assert.IsNull(Instance.FBtnLayout, 'FBtnLayout deve ser nil apos CloseDialog');

  for I := 0 to 2 do
  begin
    Assert.IsNull(LObj[I].Overlay,
      'LObj[' + IntToStr(I) + '].Overlay deve ser nil apos CloseDialog');
    LObj[I].Free;
  end;
  Instance := nil;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_InvokesResultCallback;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: TButton;
  LObj: TButtonHandler;
  LButtons: TButtonHandlerList;
  LCallbackResult: TModalResult;
  LCallbackCalled: Boolean;
begin
  LCallbackResult := mrNone;
  LCallbackCalled := False;

  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0,
      procedure(const AResult: TModalResult)
      begin
        LCallbackCalled := True;
        LCallbackResult := AResult;
      end,
      LButtons));
  LButtons.Free;

  LObj := TButtonHandler.Create;
  try
    LObj.ClickHandler := OnClickHandler;
    LObj.Overlay := LOverlay;
    LObj.ModalResult := mrOk;
    LBtn.TagObject := LObj;

    Instance.FBtnLayout := LBtnLayout;
    Instance.ButtonClick(LBtn);

    Assert.IsTrue(LCallbackCalled, 'ResultCallback deve ter sido chamado');
    Assert.AreEqual(mrOk, LCallbackResult, 'Callback deve receber mrOk');
  finally
    LObj.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_NoCallback_NoException;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: TButton;
  LObj: TButtonHandler;
  LButtons: TButtonHandlerList;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0, nil, LButtons));
  LButtons.Free;

  LObj := TButtonHandler.Create;
  try
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    Instance.FBtnLayout := LBtnLayout;

    Assert.WillNotRaise(
      procedure
      begin
        Instance.ButtonClick(LBtn);
      end);
  finally
    LObj.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestOnBackgroundClick_CallbackWithMrCancel;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBgRect: TRectangle;
  LButtons: TButtonHandlerList;
  LCallbackResult: TModalResult;
  LCallbackCalled: Boolean;
begin
  LCallbackResult := mrNone;
  LCallbackCalled := False;

  LOverlay := TLayout.Create(nil);
  LBgRect  := TRectangle.Create(LOverlay);
  LBgRect.Parent := LOverlay;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0,
      procedure(const AResult: TModalResult)
      begin
        LCallbackCalled := True;
        LCallbackResult := AResult;
      end,
      LButtons));
  LButtons.Free;

  try
    Instance.OnBackgroundClick(LBgRect);

    Assert.IsTrue(LCallbackCalled,
      'ResultCallback deve ter sido chamado no OnBackgroundClick');
    Assert.AreEqual(mrCancel, LCallbackResult,
      'OnBackgroundClick deve chamar callback com mrCancel');
  finally
    LOverlay.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_CallbackBeforeClose;
var
  Instance: TFMXDialogInstanceCracker;
  LOverlay: TLayout;
  LBtnLayout: TLayout;
  LBtn: TButton;
  LObj: TButtonHandler;
  LButtons: TButtonHandlerList;
  LCallbackCalled: Boolean;
begin
  LCallbackCalled := False;

  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0,
      procedure(const AResult: TModalResult)
      begin
        // Overlay must still exist at callback time (CloseDialog not yet called)
        LCallbackCalled := True;
      end,
      LButtons));
  LButtons.Free;

  LObj := TButtonHandler.Create;
  try
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    Instance.FBtnLayout := LBtnLayout;
    Instance.ButtonClick(LBtn);

    Assert.IsTrue(LCallbackCalled,
      'ResultCallback deve ser chamado sincronamente (antes do ForceQueue)');
  finally
    LObj.Free;
    Instance := nil;
  end;
end;

procedure TAndroidDialogCloseTests.TestDoResolve_IsIdempotent;
var
  Instance: TFMXDialogInstanceCracker;
  LButtons: TButtonHandlerList;
  LCount: Integer;
begin
  LCount := 0;
  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0,
      procedure(const AResult: TModalResult)
      begin
        Inc(LCount);
      end,
      LButtons));
  LButtons.Free;
  try
    Instance.DoResolve(mrOk);
    Instance.DoResolve(mrCancel);
    Instance.DoResolve(mrOk);
    Assert.AreEqual(1, LCount, 'Callback deve disparar no maximo uma vez');
  finally
    Instance := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAndroidDialogTests);
  TDUnitX.RegisterTestFixture(TAndroidDialogCloseTests);

end.
