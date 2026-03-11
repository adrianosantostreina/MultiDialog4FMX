unit MultiDialog4FMX.Tests.Android;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Android,
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  FMX.Types,
  FMX.Graphics,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects,
  FMX.StdCtrls,
  System.SysUtils,
  System.UITypes;

type
  [TestFixture]
  TAndroidDialogTests = class
  private
    FDialog: TAndroidDialog;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCalculateMessageHeight_ShortText;

    [Test]
    procedure TestCalculateMessageHeight_LongText;

    [Test]
    procedure TestCalculateMessageHeight_MultiLine;

    [Test]
    procedure TestCalculateMessageHeight_EmptyText;

    [Test]
    procedure TestResolveParentForm_WithExplicitForm;

    [Test]
    procedure TestResolveParentForm_WithNilForm;

    [Test]
    procedure TestInternalShow_RequiresMinimumOneButton;

    [Test]
    procedure TestInternalShow_EnforcesMaximumFourButtons;

    [Test]
    procedure TestInternalShow_TwoButtonsNoHandler_NoValidationException;

  end;

  [TestFixture]
  TAndroidDialogCloseTests = class
  private
    FDialog: TAndroidDialog;
    FHandlerCallCount: Integer;
    FHandlerSender: TObject;
    procedure OnClickHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestButtonClick_CallsHandlerAndClearsTagObject;

    [Test]
    procedure TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;

    [Test]
    procedure TestCloseDialog_FreesAllRemainingTagObjects;
  end;

  [TestFixture]
  TAndroidDialogLayoutTests = class
  private
    FDialog: TAndroidDialog;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestBuildOverlay_HasContentsAlign;

    [Test]
    procedure TestBuildDialogRect_WidthIsCorrect;

    [Test]
    procedure TestBuildButtons_ChildCountMatchesHandlers;

    [Test]
    procedure TestInternalShow_SubMethodsRun_FBtnLayoutAssigned;

    [Test]
    procedure TestCalculateFinalHeight_WithTitle_GreaterThanWithout;

    [Test]
    procedure TestBuildDialogRect_UsesBorderRadius;

    [Test]
    procedure TestBuildBody_UsesFontSize;
  end;

implementation

type
  TAndroidDialogCracker = class(TAndroidDialog);

{ TAndroidDialogTests }

procedure TAndroidDialogTests.Setup;
begin
  FDialog := TAndroidDialog.Create;
end;

procedure TAndroidDialogTests.TearDown;
begin
  FDialog := nil;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_ShortText;
var
  Height: Single;
  Font: TFont;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight('Test', 300, Font);
    Assert.IsTrue(Height > 0, 'Height should be greater than 0');
    Assert.IsTrue(Height < 100, 'Short text should have small height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_LongText;
var
  Height: Single;
  Font: TFont;
  LongText: string;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    LongText := 'This is a very long text that should wrap into multiple lines when displayed in the dialog. ' +
                'It contains enough characters to test the wrapping functionality of the message calculation.';
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight(LongText, 300, Font);
    Assert.IsTrue(Height > 50, 'Long text should have greater height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_MultiLine;
var
  Height: Single;
  Font: TFont;
  MultiLineText: string;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    MultiLineText := 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3';
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight(MultiLineText, 300, Font);
    Assert.IsTrue(Height > 0, 'Multi-line text should have height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_EmptyText;
var
  Height: Single;
  Font: TFont;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight('', 300, Font);
    Assert.IsTrue(Height >= 0, 'Empty text should have non-negative height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestResolveParentForm_WithExplicitForm;
var
  TestForm: TCommonCustomForm;
  LResult: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  TestForm := TCommonCustomForm.Create(nil);
  try
    LResult := TAndroidDialogCracker(FDialog).ResolveParentForm(TestForm);
    Assert.AreEqual(TestForm, LResult, 'Should return the explicit form provided');
  finally
    TestForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestResolveParentForm_WithNilForm;
var
  LResult: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LResult := TAndroidDialogCracker(FDialog).ResolveParentForm(nil);

  if Assigned(Application.MainForm) then
    Assert.IsNotNull(LResult, 'Should resolve to a valid form')
  else
    Assert.IsNull(LResult, 'Should return nil when no forms are active');
end;

procedure TAndroidDialogTests.TestInternalShow_RequiresMinimumOneButton;
var
  LMsg: string;
  LForm: TCommonCustomForm;
begin
  LMsg := '';
  LForm := TCommonCustomForm.Create(nil);
  try
    try
      TAndroidDialogCracker(FDialog).InternalShow(LForm);
    except
      on E: Exception do
        LMsg := E.Message;
    end;
  finally
    LForm.Free;
  end;

  Assert.AreEqual('O n'#250'mero m'#237'nimo de bot'#245'es '#233' 1.', LMsg,
    'Deve rejeitar dialogo sem botoes');
end;

procedure TAndroidDialogTests.TestInternalShow_EnforcesMaximumFourButtons;
var
  LInternalShowMsg: string;
  LBuilderMsg: string;
  LForm: TCommonCustomForm;
begin
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);

  LInternalShowMsg := '';
  LForm := TCommonCustomForm.Create(nil);
  try
    try
      TAndroidDialogCracker(FDialog).InternalShow(LForm);
    except
      on E: Exception do
        LInternalShowMsg := E.Message;
    end;
  finally
    LForm.Free;
  end;

  Assert.IsNotEmpty(LInternalShowMsg,
    'InternalShow deve rejeitar mais de 4 botoes com uma mensagem de erro');

  LBuilderMsg := '';
  try
    FDialog.Buttons
      .AddButton('1').AddButton('2').AddButton('3').AddButton('4').AddButton('5');
  except
    on E: Exception do
      LBuilderMsg := E.Message;
  end;

  Assert.AreEqual(LInternalShowMsg, LBuilderMsg,
    'Mensagem de maximo deve ser igual em InternalShow e em AddButton');
end;

procedure TAndroidDialogTests.TestInternalShow_TwoButtonsNoHandler_NoValidationException;
var
  LRaisedValidationError: Boolean;
begin
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);

  LRaisedValidationError := False;
  try
    TAndroidDialogCracker(FDialog).InternalShow(nil);
  except
    on E: Exception do
      if E.Message.Contains('ao menos um deve possuir evento associado') then
        LRaisedValidationError := True;
  end;

  Assert.IsFalse(LRaisedValidationError,
    'InternalShow nao deve rejeitar multiplos botoes sem handler');
end;

{ TAndroidDialogCloseTests }

procedure TAndroidDialogCloseTests.Setup;
begin
  FDialog := TAndroidDialog.Create;
  FHandlerCallCount := 0;
  FHandlerSender := nil;
end;

procedure TAndroidDialogCloseTests.TearDown;
begin
  FDialog := nil;
end;

procedure TAndroidDialogCloseTests.OnClickHandler(Sender: TObject);
begin
  Inc(FHandlerCallCount);
  FHandlerSender := Sender;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_CallsHandlerAndClearsTagObject;
// R7: usa TButtonHandler (nao TButtonHandlerObj).
var
  LOverlay  : TLayout;
  LBtnLayout: TFlowLayout;
  LBtn      : TButton;
  LObj      : TButtonHandler;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LObj := TButtonHandler.Create;
  try
    LObj.ClickHandler := OnClickHandler;
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;
    TAndroidDialogCracker(FDialog).ButtonClick(LBtn);

    Assert.AreEqual(1, FHandlerCallCount, 'ClickHandler deve ter sido chamado exatamente 1 vez');
    Assert.IsNull(LObj.Overlay, 'Overlay deve ser nil apos ButtonClick');
  finally
    LObj.Free;
  end;
end;

procedure TAndroidDialogCloseTests.TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;
// R7: usa TButtonHandler. Apos excecao: overlay fechado, LObj.Overlay = nil.
var
  LOverlay  : TLayout;
  LBtnLayout: TFlowLayout;
  LBtn      : TButton;
  LObj      : TButtonHandler;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LObj := TButtonHandler.Create;
  try
    LObj.AnonymousHandler :=
      procedure
      begin
        raise Exception.Create('Erro simulado no handler');
      end;
    LObj.Overlay := LOverlay;
    LBtn.TagObject := LObj;

    TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;

    var LExceptionPropagated := False;
    try
      TAndroidDialogCracker(FDialog).ButtonClick(LBtn);
    except
      on E: Exception do
        if E.Message = 'Erro simulado no handler' then
          LExceptionPropagated := True;
    end;
    Assert.IsTrue(LExceptionPropagated, 'A excecao do handler deve propagar');
    Assert.IsNull(LObj.Overlay, 'Overlay deve ser nil mesmo apos excecao no handler');
  finally
    LObj.Free;
  end;
end;

procedure TAndroidDialogCloseTests.TestCloseDialog_FreesAllRemainingTagObjects;
// R7: CloseDialog deve nular Overlay de TODOS os handlers dos botoes restantes.
var
  LOverlay    : TLayout;
  LBtnLayout  : TFlowLayout;
  LBtn        : array[0..2] of TButton;
  LObj        : array[0..2] of TButtonHandler;
  I           : Integer;
  LTagNilCount: Integer;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;

  for I := 0 to 2 do
  begin
    LBtn[I] := TButton.Create(LBtnLayout);
    LBtn[I].Parent := LBtnLayout;
    LObj[I] := TButtonHandler.Create;
    LObj[I].Overlay := LOverlay;
    LBtn[I].TagObject := LObj[I];
  end;

  TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;

  LTagNilCount := 0;
  for I := 0 to LBtnLayout.ChildrenCount - 1 do
    if (LBtnLayout.Children[I] is TButton) and
       not Assigned(TButton(LBtnLayout.Children[I]).TagObject) then
      Inc(LTagNilCount);
  Assert.AreEqual(0, LTagNilCount,
    'Antes do CloseDialog todos os TagObjects devem estar atribuidos');

  TAndroidDialogCracker(FDialog).CloseDialog(LOverlay);

  Assert.IsNull(TAndroidDialogCracker(FDialog).FBtnLayout,
    'FBtnLayout deve ser nil apos CloseDialog');

  for I := 0 to 2 do
  begin
    Assert.IsNull(LObj[I].Overlay,
      'LObj[' + IntToStr(I) + '].Overlay deve ser nil apos CloseDialog');
    LObj[I].Free;
  end;
end;

{ TAndroidDialogLayoutTests }

procedure TAndroidDialogLayoutTests.Setup;
begin
  FDialog := TAndroidDialog.Create;
end;

procedure TAndroidDialogLayoutTests.TearDown;
begin
  FDialog := nil;
end;

procedure TAndroidDialogLayoutTests.TestBuildOverlay_HasContentsAlign;
var
  LForm   : TCommonCustomForm;
  LOverlay: TLayout;
  LBgRect : TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    LOverlay := TAndroidDialogCracker(FDialog).BuildOverlay(LForm, LBgRect);
    try
      Assert.AreEqual(TAlignLayout.Contents, LOverlay.Align,
        'Overlay deve ter Align = Contents');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestBuildDialogRect_WidthIsCorrect;
var
  LForm      : TCommonCustomForm;
  LOverlay   : TLayout;
  LDialogRect: TRectangle;
  LBgRect    : TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LForm := TCommonCustomForm.Create(nil);
  try
    LOverlay := TAndroidDialogCracker(FDialog).BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := TAndroidDialogCracker(FDialog).BuildDialogRect(LOverlay);
      Assert.IsTrue(Abs(LDialogRect.Width - 300) < 1,
        'Width deve ser 300 (logical points, sem multiplicar por scale)');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestBuildButtons_ChildCountMatchesHandlers;
var
  LForm      : TCommonCustomForm;
  LOverlay   : TLayout;
  LDialogRect: TRectangle;
  LBgRect    : TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);

  LForm := TCommonCustomForm.Create(nil);
  try
    LOverlay    := TAndroidDialogCracker(FDialog).BuildOverlay(LForm, LBgRect);
    LDialogRect := TAndroidDialogCracker(FDialog).BuildDialogRect(LOverlay);
    TAndroidDialogCracker(FDialog).BuildButtons(LOverlay, LDialogRect);

    Assert.AreEqual(3, TAndroidDialogCracker(FDialog).FBtnLayout.ChildrenCount,
      '3 handlers deve gerar 3 botoes em FBtnLayout');

    TAndroidDialogCracker(FDialog).CloseDialog(LOverlay);
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestInternalShow_SubMethodsRun_FBtnLayoutAssigned;
var
  LForm : TCommonCustomForm;
  LMsg  : string;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);

  LForm := TCommonCustomForm.Create(nil);
  try
    LMsg := '';
    try
      TAndroidDialogCracker(FDialog).InternalShow(LForm);
    except
      on E: Exception do
        LMsg := E.Message;
    end;

    if LMsg <> '' then
      Assert.Fail('InternalShow raised unexpected exception: ' + LMsg)
    else
      Assert.IsNotNull(TAndroidDialogCracker(FDialog).FBtnLayout,
        'FBtnLayout deve estar atribuido apos InternalShow');
  finally
    if Assigned(TAndroidDialogCracker(FDialog).FBtnLayout) then
    begin
      var LOverlayPtr := TLayout(TAndroidDialogCracker(FDialog).FBtnLayout.Parent.Parent);
      TAndroidDialogCracker(FDialog).CloseDialog(LOverlayPtr);
    end;
    LForm.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestCalculateFinalHeight_WithTitle_GreaterThanWithout;
var
  LBodyLayout     : TLayout;
  LBtnLayoutMock  : TFlowLayout;
  LHeightWithTitle: Single;
  LHeightNoTitle  : Single;
begin
  LBodyLayout := TLayout.Create(nil);
  LBodyLayout.Height := 100;
  LBtnLayoutMock := TFlowLayout.Create(nil);
  LBtnLayoutMock.Height := 56;

  try
    TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayoutMock;

    TAndroidDialogCracker(FDialog).FTitle := 'Test Title';
    LHeightWithTitle := TAndroidDialogCracker(FDialog).CalculateFinalHeight(
      LBodyLayout, False);

    TAndroidDialogCracker(FDialog).FTitle := '';
    LHeightNoTitle := TAndroidDialogCracker(FDialog).CalculateFinalHeight(
      LBodyLayout, False);

    TAndroidDialogCracker(FDialog).FBtnLayout := nil;

    Assert.IsTrue(LHeightWithTitle > LHeightNoTitle,
      'Dialog com titulo deve ser mais alto que sem titulo');
  finally
    LBtnLayoutMock.Free;
    LBodyLayout.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestBuildDialogRect_UsesBorderRadius;
var
  LForm      : TCommonCustomForm;
  LOverlay   : TLayout;
  LDialogRect: TRectangle;
  LBgRect    : TRectangle;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  TAndroidDialogCracker(FDialog).SetBorderRadius(8);

  LForm := TCommonCustomForm.Create(nil);
  try
    LOverlay := TAndroidDialogCracker(FDialog).BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := TAndroidDialogCracker(FDialog).BuildDialogRect(LOverlay);
      Assert.AreEqual(Single(8), LDialogRect.XRadius,
        'XRadius deve refletir o BorderRadius configurado');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAndroidDialogLayoutTests.TestBuildBody_UsesFontSize;
var
  LForm       : TCommonCustomForm;
  LOverlay    : TLayout;
  LDialogRect : TRectangle;
  LBgRect     : TRectangle;
  LIconPresent: Boolean;
  LBodyLayout : TLayout;
  LLabel      : TLabel;
  I           : Integer;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  TAndroidDialogCracker(FDialog).SetFontSize(18);
  TAndroidDialogCracker(FDialog).FMessage := 'Test message';

  LForm := TCommonCustomForm.Create(nil);
  try
    LOverlay    := TAndroidDialogCracker(FDialog).BuildOverlay(LForm, LBgRect);
    try
      LDialogRect := TAndroidDialogCracker(FDialog).BuildDialogRect(LOverlay);
      TAndroidDialogCracker(FDialog).BuildBody(
        LDialogRect, LIconPresent, LBodyLayout);

      LLabel := nil;
      for I := 0 to LBodyLayout.ChildrenCount - 1 do
        if LBodyLayout.Children[I] is TLabel then
        begin
          LLabel := TLabel(LBodyLayout.Children[I]);
          Break;
        end;

      Assert.IsNotNull(LLabel, 'LBodyLayout deve conter um TLabel de mensagem');
      Assert.AreEqual(Single(18), LLabel.TextSettings.Font.Size,
        'Font.Size do label deve ser 18 conforme SetFontSize');
    finally
      LOverlay.Parent := nil;
      LOverlay.Free;
    end;
  finally
    LForm.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAndroidDialogTests);
  TDUnitX.RegisterTestFixture(TAndroidDialogCloseTests);
  TDUnitX.RegisterTestFixture(TAndroidDialogLayoutTests);

end.
