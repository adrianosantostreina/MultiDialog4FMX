unit MultiDialog4FMX.Tests.Snapshot;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Interfaces,
  FMX.Forms,
  FMX.Types,
  System.SysUtils,
  System.UITypes;

type
  [TestFixture]
  TDialogSnapshotTests = class
  private
    FForm: TCommonCustomForm;
    FSourceButtons: TButtonHandlerList;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreateFrom_CopiesAllConfigFields;

    [Test]
    procedure TestCreateFrom_DeepCopiesButtons;

    [Test]
    procedure TestDestroy_FreesOwnButtonList;

    [Test]
    procedure TestSnapshot_HasUniqueIncrementingId;
  end;

implementation

procedure TDialogSnapshotTests.Setup;
begin
  FForm := TCommonCustomForm.Create(nil);
  FSourceButtons := TButtonHandlerList.Create(True);
end;

procedure TDialogSnapshotTests.TearDown;
begin
  FSourceButtons.Free;
  FForm.Free;
end;

procedure TDialogSnapshotTests.TestCreateFrom_CopiesAllConfigFields;
var
  LCallback: TDialogResultProc;
  LSnapshot: TDialogSnapshot;
begin
  LCallback := procedure(const AResult: TModalResult) begin end;

  LSnapshot := TDialogSnapshot.Create(FForm, 'My Title', 'My Message',
    TMultiDialogType.mdtWarning, True, 18, 8, TDialogAnimation.danFade,
    TDialogTheme.dthDark, 'M1 2 L3 4', TAlphaColorRec.Purple, LCallback,
    FSourceButtons);
  try
    Assert.AreEqual(FForm, LSnapshot.Form);
    Assert.AreEqual('My Title', LSnapshot.Title);
    Assert.AreEqual('My Message', LSnapshot.Message);
    Assert.AreEqual(TMultiDialogType.mdtWarning, LSnapshot.MsgType);
    Assert.IsTrue(LSnapshot.Cancelable);
    Assert.AreEqual(Single(18), LSnapshot.FontSize);
    Assert.AreEqual(Single(8), LSnapshot.BorderRadius);
    Assert.AreEqual(TDialogAnimation.danFade, LSnapshot.Animation);
    Assert.AreEqual(TDialogTheme.dthDark, LSnapshot.Theme);
    Assert.AreEqual('M1 2 L3 4', LSnapshot.CustomSVG);
    Assert.AreEqual(TAlphaColorRec.Purple, LSnapshot.CustomIconColor);
    Assert.IsTrue(Assigned(LSnapshot.ResultCallback));
  finally
    LSnapshot.Free;
  end;
end;

procedure TDialogSnapshotTests.TestCreateFrom_DeepCopiesButtons;
var
  LSnapshot: TDialogSnapshot;
  LOriginal: TButtonHandler;
begin
  LOriginal := TButtonHandler.Create;
  LOriginal.Text := 'OK';
  LOriginal.Color := TAlphaColorRec.Blue;
  FSourceButtons.Add(LOriginal);

  LSnapshot := TDialogSnapshot.Create(FForm, '', '', TMultiDialogType.mdtCustom,
    False, 14, 12, TDialogAnimation.danNone, TDialogTheme.dthAuto, '', 0, nil,
    FSourceButtons);
  try
    Assert.AreEqual(1, LSnapshot.Buttons.Count);
    Assert.AreNotSame(LOriginal, LSnapshot.Buttons[0],
      'O snapshot deve ter sua propria copia do TButtonHandler, nao a mesma instancia');
    Assert.AreEqual('OK', LSnapshot.Buttons[0].Text);
    Assert.AreEqual(TAlphaColorRec.Blue, LSnapshot.Buttons[0].Color);

    // Muda o original DEPOIS do snapshot tirado — o snapshot nao pode ser afetado.
    LOriginal.Text := 'Mudou';
    Assert.AreEqual('OK', LSnapshot.Buttons[0].Text,
      'Snapshot deve ser imutavel em relacao a mudancas no builder original');
  finally
    LSnapshot.Free;
  end;
end;

procedure TDialogSnapshotTests.TestDestroy_FreesOwnButtonList;
var
  LBefore, LDuring: Integer;
  LSnapshot: TDialogSnapshot;
begin
  LBefore := TButtonHandler.InstanceCount;
  FSourceButtons.Add(TButtonHandler.Create);
  FSourceButtons.Add(TButtonHandler.Create);

  LSnapshot := TDialogSnapshot.Create(FForm, '', '', TMultiDialogType.mdtCustom,
    False, 14, 12, TDialogAnimation.danNone, TDialogTheme.dthAuto, '', 0, nil,
    FSourceButtons);

  LDuring := TButtonHandler.InstanceCount;
  Assert.AreEqual(LBefore + 4, LDuring,
    '2 originais + 2 copias do snapshot devem estar vivos');

  LSnapshot.Free;
  Assert.AreEqual(LBefore + 2, TButtonHandler.InstanceCount,
    'Destruir o snapshot deve liberar so as copias dele, nao os originais');
end;

procedure TDialogSnapshotTests.TestSnapshot_HasUniqueIncrementingId;
var
  LButtons: TButtonHandlerList;
  LA, LB: TDialogSnapshot;
begin
  LButtons := TButtonHandlerList.Create(True);
  try
    LButtons.Add(TButtonHandler.Create);
    LA := TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12,
      danNone, dthAuto, '', 0, nil, LButtons);
    LB := TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12,
      danNone, dthAuto, '', 0, nil, LButtons);
    try
      Assert.AreNotEqual(LA.Id, LB.Id, 'Ids devem ser distintos');
      Assert.IsTrue(LB.Id > LA.Id, 'Id deve ser crescente');
      Assert.IsTrue(LA.ElapsedMs >= 0, 'ElapsedMs deve ser nao-negativo');
    finally
      LA.Free;
      LB.Free;
    end;
  finally
    LButtons.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogSnapshotTests);

end.
