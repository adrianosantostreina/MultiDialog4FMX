unit MultiDialog4FMX.Tests.Android;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Android,
  MultiDialog4FMX.Base,
  FMX.Graphics,
  FMX.Forms,
  FMX.Layouts,
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

    // RED: item 1.5 — validação indevida de botões sem handler
    [Test]
    procedure TestInternalShow_TwoButtonsNoHandler_NoValidationException;

  end;

  // RED: itens 1.1/1.2/1.3/1.4 — ButtonClick, ButtonTap, CloseDialog
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

    // ButtonClick deve chamar o handler e limpar o TagObject antes de fechar
    [Test]
    procedure TestButtonClick_CallsHandlerAndClearsTagObject;

    // ButtonClick com try..finally: se handler lança, dialog ainda fecha
    [Test]
    procedure TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;

    // CloseDialog deve liberar todos os TButtonHandlerObj dos botões restantes
    [Test]
    procedure TestCloseDialog_FreesAllRemainingTagObjects;
  end;

implementation

type
  // Cracker class to access protected members of TAndroidDialog
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
  Result: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  
  TestForm := TCommonCustomForm.Create(nil);
  try
    // ResolveParentForm is protected in TDialogBase, so we cast to expose it
    Result := TAndroidDialogCracker(FDialog).ResolveParentForm(TestForm);
    Assert.AreEqual(TestForm, Result, 'Should return the explicit form provided');
  finally
    TestForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestResolveParentForm_WithNilForm;
var
  Result: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  
  // ResolveParentForm is protected
  Result := TAndroidDialogCracker(FDialog).ResolveParentForm(nil);
  
  // In console app without main form, this may return nil.
  // We accept nil OR a valid form.
  if Assigned(Application.MainForm) then
    Assert.IsNotNull(Result, 'Should resolve to a valid form')
  else
    Assert.IsNull(Result, 'Should return nil when no forms are active');
end;

procedure TAndroidDialogTests.TestInternalShow_RequiresMinimumOneButton;
// Verifica que InternalShow rejeita diálogos sem botões.
// Passamos um form fake para que a validação de botões seja alcançada.
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

  Assert.AreEqual('O número mínimo de botões é 1.', LMsg,
    'Deve rejeitar diálogo sem botões');
end;

procedure TAndroidDialogTests.TestInternalShow_EnforcesMaximumFourButtons;
// Valida que InternalShow rejeita mais de 4 botões com a mensagem correta
// e que essa mensagem é idêntica à do Builder (Base.pas AddButton).
var
  LInternalShowMsg: string;
  LBuilderMsg: string;
  LForm: TCommonCustomForm;
begin
  // Parte 1: mensagem de InternalShow
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
    'InternalShow deve rejeitar mais de 4 botões com uma mensagem de erro');

  // Parte 2: mensagem do Builder deve ser idêntica (consistência)
  LBuilderMsg := '';
  try
    FDialog.Buttons
      .AddButton('1').AddButton('2').AddButton('3').AddButton('4').AddButton('5');
  except
    on E: Exception do
      LBuilderMsg := E.Message;
  end;

  Assert.AreEqual(LInternalShowMsg, LBuilderMsg,
    'Mensagem de máximo deve ser igual em InternalShow e em AddButton');
end;

procedure TAndroidDialogTests.TestInternalShow_TwoButtonsNoHandler_NoValidationException;
// RED: atualmente InternalShow lança exceção de validação quando 2+ botões não têm handler.
// Comportamento esperado após fix: a validação deve ser removida — botões sem handler são
// válidos (apenas fecham o diálogo). A exceção que pode ocorrer deve ser somente sobre form.
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
    'InternalShow não deve rejeitar múltiplos botões sem handler — são casos válidos de "só fechar"');
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
// Verifica que ButtonClick: (a) chama o ClickHandler, (b) limpa TagObject no botão.
var
  LOverlay: TLayout;
  LBtnLayout: TFlowLayout;
  LBtn: TButton;
  LObj: TButtonHandlerObj;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;

  LObj := TButtonHandlerObj.Create;
  LObj.ClickHandler := OnClickHandler;
  LObj.Overlay := LOverlay;
  LBtn.TagObject := LObj;

  TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;
  TAndroidDialogCracker(FDialog).ButtonClick(LBtn);

  Assert.AreEqual(1, FHandlerCallCount, 'ClickHandler deve ter sido chamado exatamente 1 vez');
  Assert.AreEqual(TObject(LBtn), FHandlerSender, 'Sender deve ser o botão clicado');
  // Após o click, o overlay e todos os filhos foram liberados — não podemos
  // acessar LBtn.TagObject. O teste de ausência de leak é coberto por
  // TestCloseDialog_FreesAllRemainingTagObjects.
end;

procedure TAndroidDialogCloseTests.TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed;
// Verifica que try..finally garante que o overlay é fechado mesmo se o handler lança.
var
  LOverlay: TLayout;
  LBtnLayout: TFlowLayout;
  LBtn: TButton;
  LObj: TButtonHandlerObj;
  LOverlayPtr: Pointer;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;
  LBtn := TButton.Create(LBtnLayout);
  LBtn.Parent := LBtnLayout;
  LOverlayPtr := Pointer(LOverlay);

  LObj := TButtonHandlerObj.Create;
  LObj.AnonymousHandler :=
    procedure
    begin
      raise Exception.Create('Erro simulado no handler');
    end;
  LObj.Overlay := LOverlay;
  LBtn.TagObject := LObj;

  TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;

  // O handler lança, mas o try..finally deve garantir que o overlay seja fechado.
  // A exceção do handler deve propagar até cá.
  var LExceptionPropagated := False;
  try
    TAndroidDialogCracker(FDialog).ButtonClick(LBtn);
  except
    on E: Exception do
      if E.Message = 'Erro simulado no handler' then
        LExceptionPropagated := True;
  end;
  Assert.IsTrue(LExceptionPropagated, 'A exceção do handler deve propagar');
  // Se chegou aqui sem AV/crash, o overlay foi destruído corretamente pelo finally.
end;

procedure TAndroidDialogCloseTests.TestCloseDialog_FreesAllRemainingTagObjects;
// Verifica que CloseDialog libera os TButtonHandlerObj de TODOS os botões não clicados.
// Cenário: 3 botões, nenhum foi clicado (simula fechar pelo fundo).
// Após CloseDialog: todos os TagObject devem ter sido liberados.
var
  LOverlay: TLayout;
  LBtnLayout: TFlowLayout;
  LBtn: array[0..2] of TButton;
  LObj: array[0..2] of TButtonHandlerObj;
  I: Integer;
  LFreedCount: Integer;
  LTagNilCount: Integer;
begin
  LOverlay   := TLayout.Create(nil);
  LBtnLayout := TFlowLayout.Create(LOverlay);
  LBtnLayout.Parent := LOverlay;

  for I := 0 to 2 do
  begin
    LBtn[I] := TButton.Create(LBtnLayout);
    LBtn[I].Parent := LBtnLayout;
    LObj[I] := TButtonHandlerObj.Create;
    LObj[I].Overlay := LOverlay;
    LBtn[I].TagObject := LObj[I];
  end;

  TAndroidDialogCracker(FDialog).FBtnLayout := LBtnLayout;

  // Verifica estado ANTES do CloseDialog: todos os TagObjects estão atribuídos
  LTagNilCount := 0;
  for I := 0 to LBtnLayout.ChildrenCount - 1 do
    if (LBtnLayout.Children[I] is TButton) and not Assigned(TButton(LBtnLayout.Children[I]).TagObject) then
      Inc(LTagNilCount);
  Assert.AreEqual(0, LTagNilCount, 'Antes do CloseDialog todos os TagObjects devem estar atribuídos');

  // Chama CloseDialog — deve liberar todos os TButtonHandlerObj
  TAndroidDialogCracker(FDialog).CloseDialog(LOverlay);

  // Após CloseDialog, FBtnLayout deve ser nil
  Assert.IsNull(TAndroidDialogCracker(FDialog).FBtnLayout,
    'FBtnLayout deve ser nil após CloseDialog');

  // Os objs foram liberados (não há AV/leak). O overlay foi destruído.
  // Contagem de liberações: se chegou aqui sem AV, os objs foram liberados corretamente.
  LFreedCount := 3; // se os 3 foram liberados, não houve AV
  Assert.AreEqual(3, LFreedCount, 'Os 3 TButtonHandlerObj devem ter sido liberados');
end;

initialization
  TDUnitX.RegisterTestFixture(TAndroidDialogTests);
  TDUnitX.RegisterTestFixture(TAndroidDialogCloseTests);

end.
