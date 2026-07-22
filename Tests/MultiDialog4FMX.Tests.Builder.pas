unit MultiDialog4FMX.Tests.Builder;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Tests.Mocks,
  MultiDialog4FMX.Tests.Queue,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Telemetry,
  MultiDialog4FMX.Util,
  System.SysUtils,
  System.UITypes,
  FMX.Types,
  FMX.Forms;

type
  [TestFixture]
  TDialogBuilderTests = class
  private
    FDialog: TMockDialogBase;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSetTitle_StoresValue;

    [Test]
    procedure TestSetMessage_StoresValue;

    [Test]
    procedure TestSetCancelable_True;

    [Test]
    procedure TestSetCancelable_False;

    [Test]
    procedure TestFluentChaining_Multiple;

    [Test]
    procedure TestButtons_ReturnsButtonsBuilder;

    [Test]
    procedure TestButtonsEnd_ReturnsDialogBuilder;

    [Test]
    procedure TestShow_EnqueuesSnapshot;

    [Test]
    procedure TestSetType_StoresValue;

    [Test]
    procedure TestShow_ReturnsDialogBuilder;

    [Test]
    procedure TestSetFontSize_StoresValue;

    [Test]
    procedure TestSetBorderRadius_StoresValue;

    [Test]
    procedure TestSetIcon_StoresValue;

    [Test]
    procedure TestSetIconColor_StoresValue;

    [Test]
    procedure TestSetIcon_ReturnsSelf;

    [Test]
    procedure TestSetIcon_DefaultIsEmpty;

    [Test]
    procedure TestSetIconColor_DefaultIsNull;

    [Test]
    procedure TestSetIcon_WithSetType_BothStored;

    [Test]
    procedure TestSetOnResult_StoresCallback;

    [Test]
    procedure TestSetOnResult_DefaultIsNil;

    [Test]
    procedure TestSetOnResult_NilArg_NoException;

    [Test]
    procedure TestSetAnimation_StoresValue;

    [Test]
    procedure TestSetAnimation_DefaultIsNone;

    [Test]
    procedure TestSetTheme_StoresValue;

    [Test]
    procedure TestSetTheme_DefaultIsAuto;

    [Test]
    procedure TestShow_NoButtons_RaisesException;

    [Test]
    procedure TestShow_TwoButtonsNoHandler_NoException;

    [Test]
    procedure TestShowGetHandle_CloseResolvesMrCancel;

    [Test]
    procedure TestFacade_OnDialogEvent_RoutesToTelemetry;
  end;

implementation

{ TDialogBuilderTests }

procedure TDialogBuilderTests.Setup;
begin
  FDialog := TMockDialogBase.Create;
end;

procedure TDialogBuilderTests.TearDown;
begin
  FDialog := nil; // Interface reference counting will free it
end;

procedure TDialogBuilderTests.TestSetTitle_StoresValue;
begin
  FDialog.SetTitle('Test Title');
  Assert.AreEqual('Test Title', FDialog.Title);
end;

procedure TDialogBuilderTests.TestSetMessage_StoresValue;
begin
  FDialog.SetMessage('Test Message');
  Assert.AreEqual('Test Message', FDialog.Message);
end;

procedure TDialogBuilderTests.TestSetCancelable_True;
begin
  FDialog.SetCancelable(True);
  Assert.IsTrue(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestSetCancelable_False;
begin
  FDialog.SetCancelable(False);
  Assert.IsFalse(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestFluentChaining_Multiple;
var
  Result: IDialogBuilder;
begin
  Result := FDialog
    .SetTitle('Title')
    .SetMessage('Message')
    .SetCancelable(True);

  Assert.IsNotNull(Result);
  Assert.AreEqual('Title', FDialog.Title);
  Assert.AreEqual('Message', FDialog.Message);
  Assert.IsTrue(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestButtons_ReturnsButtonsBuilder;
var
  ButtonsBuilder: IDialogButtonsBuilder;
begin
  ButtonsBuilder := FDialog.Buttons;
  Assert.IsNotNull(ButtonsBuilder);
end;

procedure TDialogBuilderTests.TestButtonsEnd_ReturnsDialogBuilder;
var
  DialogBuilder: IDialogBuilder;
begin
  DialogBuilder := FDialog.Buttons.&End;
  Assert.IsNotNull(DialogBuilder);
end;

procedure TDialogBuilderTests.TestShow_EnqueuesSnapshot;
var
  LForm: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  LForm := TCommonCustomForm.Create(nil);
  try
    FDialog.Reset;
    FDialog.Buttons.AddButton('OK').&End.Show(LForm);
    Assert.IsTrue(FDialog.ShowCalled);
  finally
    LForm.Free;
  end;
end;

procedure TDialogBuilderTests.TestSetType_StoresValue;
begin
  FDialog.SetType(TMultiDialogType.mdtWarning);
  Assert.AreEqual(TMultiDialogType.mdtWarning, FDialog.MsgType);
end;

procedure TDialogBuilderTests.TestShow_ReturnsDialogBuilder;
var
  LForm: TCommonCustomForm;
  Result: IDialogBuilder;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  LForm := TCommonCustomForm.Create(nil);
  try
    Result := FDialog.Buttons.AddButton('OK').&End.Show(LForm);
    Assert.IsNotNull(Result);
  finally
    LForm.Free;
  end;
end;

procedure TDialogBuilderTests.TestSetFontSize_StoresValue;
begin
  FDialog.SetFontSize(18);
  Assert.AreEqual(Single(18), FDialog.FontSize);
end;

procedure TDialogBuilderTests.TestSetBorderRadius_StoresValue;
begin
  FDialog.SetBorderRadius(8);
  Assert.AreEqual(Single(8), FDialog.BorderRadius);
end;

procedure TDialogBuilderTests.TestSetIcon_StoresValue;
begin
  FDialog.SetIcon('M1 2 L3 4');
  Assert.AreEqual('M1 2 L3 4', FDialog.CustomSVG,
    'FCustomSVG deve armazenar o valor passado a SetIcon');
end;

procedure TDialogBuilderTests.TestSetIconColor_StoresValue;
begin
  FDialog.SetIconColor(TAlphaColorRec.Purple);
  Assert.AreEqual(TAlphaColorRec.Purple, FDialog.CustomIconColor,
    'FCustomIconColor deve armazenar a cor passada a SetIconColor');
end;

procedure TDialogBuilderTests.TestSetIcon_ReturnsSelf;
var
  LResult: IDialogBuilder;
begin
  LResult := FDialog.SetIcon('M1 2 L3 4');
  Assert.AreSame(FDialog as IDialogBuilder, LResult,
    'SetIcon deve retornar Self (cadeia fluente nao deve quebrar)');
end;

procedure TDialogBuilderTests.TestSetIcon_DefaultIsEmpty;
begin
  Assert.AreEqual('', FDialog.CustomSVG,
    'FCustomSVG deve ser vazio apos Create');
end;

procedure TDialogBuilderTests.TestSetIconColor_DefaultIsNull;
begin
  Assert.AreEqual(TAlphaColor(0), FDialog.CustomIconColor,
    'FCustomIconColor deve ser 0 (TAlphaColorRec.Null) apos Create');
end;

procedure TDialogBuilderTests.TestSetIcon_WithSetType_BothStored;
begin
  FDialog.SetType(TMultiDialogType.mdtWarning);
  FDialog.SetIcon('M1 2 L3 4');
  Assert.AreEqual(TMultiDialogType.mdtWarning, FDialog.MsgType,
    'FMsgType deve ser mdtWarning');
  Assert.AreEqual('M1 2 L3 4', FDialog.CustomSVG,
    'FCustomSVG deve coexistir com FMsgType');
end;

procedure TDialogBuilderTests.TestSetOnResult_StoresCallback;
var
  LDummy: TDialogResultProc;
begin
  LDummy := procedure(const AResult: TModalResult) begin end;
  FDialog.SetOnResult(LDummy);
  Assert.IsTrue(Assigned(FDialog.ResultCallback),
    'FResultCallback deve estar atribuido apos SetOnResult');
end;

procedure TDialogBuilderTests.TestSetOnResult_DefaultIsNil;
begin
  Assert.IsFalse(Assigned(FDialog.ResultCallback),
    'FResultCallback deve ser nil apos Create');
end;

procedure TDialogBuilderTests.TestSetOnResult_NilArg_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FDialog.SetOnResult(nil);
    end);
end;

procedure TDialogBuilderTests.TestSetAnimation_StoresValue;
begin
  FDialog.SetAnimation(TDialogAnimation.danFade);
  Assert.AreEqual(TDialogAnimation.danFade, FDialog.Animation,
    'SetAnimation deve armazenar danFade');
end;

procedure TDialogBuilderTests.TestSetAnimation_DefaultIsNone;
begin
  Assert.AreEqual(TDialogAnimation.danNone, FDialog.Animation,
    'Animação padrão deve ser danNone');
end;

procedure TDialogBuilderTests.TestSetTheme_StoresValue;
begin
  FDialog.SetTheme(TDialogTheme.dthDark);
  Assert.AreEqual(TDialogTheme.dthDark, FDialog.Theme,
    'SetTheme(dthDark) deve armazenar dthDark em FTheme');
end;

procedure TDialogBuilderTests.TestSetTheme_DefaultIsAuto;
begin
  Assert.AreEqual(TDialogTheme.dthAuto, FDialog.Theme,
    'Tema padrão deve ser dthAuto após Create');
end;

procedure TDialogBuilderTests.TestShow_NoButtons_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FDialog.Show;
    end,
    Exception);
end;

procedure TDialogBuilderTests.TestShow_TwoButtonsNoHandler_NoException;
var
  LForm: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  LForm := TCommonCustomForm.Create(nil);
  try
    FDialog.Buttons.AddButton('A').AddButton('B');
    Assert.WillNotRaise(
      procedure
      begin
        FDialog.Show(LForm);
      end);
  finally
    LForm.Free;
  end;
end;

procedure TDialogBuilderTests.TestShowGetHandle_CloseResolvesMrCancel;
var
  LForm: TCommonCustomForm;
  LHandle: IDialogHandle;
  LResult: TModalResult;
  LCalled: Boolean;
begin
  LResult := mrOk;
  LCalled := False;

  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFakeDialogInstance.Create(ASnapshot);
    end);

  LForm := TCommonCustomForm.Create(nil);
  try
    LHandle := TMultiDialog4FMX.Dialog
      .SetOnResult(procedure(const R: TModalResult)
        begin LCalled := True; LResult := R; end)
      .Buttons.AddButton('OK').&End
      .ShowGetHandle(LForm);

    Assert.IsTrue(LHandle.IsActive, 'Handle recem-criado deve estar ativo');
    LHandle.Close;   // mrCancel

    Assert.IsTrue(LCalled, 'Callback deve ter disparado no Close');
    Assert.AreEqual(mrCancel, LResult, 'Close() default resolve mrCancel');
    Assert.IsFalse(LHandle.IsActive, 'Handle nao deve mais estar ativo apos Close');
  finally
    LForm.Free;
  end;
end;

procedure TDialogBuilderTests.TestFacade_OnDialogEvent_RoutesToTelemetry;
var
  LInfo: TDialogEventInfo;
  LCalled: Boolean;
begin
  LCalled := False;
  TMultiDialog4FMX.OnDialogEvent :=
    procedure(const AInfo: TDialogEventInfo)
    begin LCalled := True; end;
  try
    LInfo := Default(TDialogEventInfo);
    LInfo.Kind := dekShown;
    TDialogTelemetry.Emit(LInfo);   // emitido pelo core -> deve chegar na fachada
    Assert.IsTrue(LCalled, 'OnDialogEvent da fachada deve receber eventos do core');
  finally
    TMultiDialog4FMX.OnDialogEvent := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogBuilderTests);

end.
