unit MultiDialog4FMX.Tests.Queue;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Interfaces,
  FMX.Forms,
  System.SysUtils,
  System.UITypes,
  System.Classes;

type
  TFakeDialogInstance = class(TInterfacedObject, IDialogVisualInstance)
  private
    class var FNextId: Integer;
  var
    FSnapshot: TDialogSnapshot;
    FShowCallCount: Integer;
    FId: Integer;
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    procedure Show;
    procedure Suppress;
    property ShowCallCount: Integer read FShowCallCount;
    property Id: Integer read FId;
  end;

  [TestFixture]
  TDialogQueueManagerTests = class
  private
    FButtons: TButtonHandlerList;
    function MakeSnapshot(const AForm: TCommonCustomForm): TDialogSnapshot;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEnqueue_FirstCall_ShowsImmediately;

    [Test]
    procedure TestEnqueue_SecondCall_SameForm_WaitsInQueue;

    [Test]
    procedure TestNotifyClosed_PopsNextFromQueue;

    [Test]
    procedure TestFormDestruction_PurgesQueueAndDeactivatesInstance;
  end;

var
  GLastCreatedInstance: TFakeDialogInstance;

implementation

{ TFakeDialogInstance }

constructor TFakeDialogInstance.Create(const ASnapshot: TDialogSnapshot);
begin
  inherited Create;
  Inc(FNextId);
  FId := FNextId;
  FSnapshot := ASnapshot;
  GLastCreatedInstance := Self;
end;

destructor TFakeDialogInstance.Destroy;
begin
  FSnapshot.Free;
  inherited;
end;

procedure TFakeDialogInstance.Show;
begin
  Inc(FShowCallCount);
end;

procedure TFakeDialogInstance.Suppress;
begin
  // no-op for the fake — the point of this fixture is fila/purga logic, not FAlive.
end;

{ TDialogQueueManagerTests }

procedure TDialogQueueManagerTests.Setup;
begin
  FButtons := TButtonHandlerList.Create(True);
  FButtons.Add(TButtonHandler.Create);
  GLastCreatedInstance := nil;
  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFakeDialogInstance.Create(ASnapshot);
    end);
end;

procedure TDialogQueueManagerTests.TearDown;
begin
  FButtons.Free;
end;

function TDialogQueueManagerTests.MakeSnapshot(const AForm: TCommonCustomForm): TDialogSnapshot;
begin
  Result := TDialogSnapshot.Create(AForm, '', '', TMultiDialogType.mdtCustom,
    False, 14, 12, TDialogAnimation.danNone, TDialogTheme.dthAuto, '', 0, nil,
    FButtons);
end;

procedure TDialogQueueManagerTests.TestEnqueue_FirstCall_ShowsImmediately;
var
  LForm: TCommonCustomForm;
begin
  LForm := TCommonCustomForm.Create(nil);
  try
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
    Assert.IsNotNull(GLastCreatedInstance);
    Assert.AreEqual(1, GLastCreatedInstance.ShowCallCount);
    Assert.IsTrue(TDialogQueueManager.Instance.DebugIsActive(LForm));
  finally
    LForm.Free;
  end;
end;

procedure TDialogQueueManagerTests.TestEnqueue_SecondCall_SameForm_WaitsInQueue;
var
  LForm: TCommonCustomForm;
  LFirstInstance: TFakeDialogInstance;
begin
  LForm := TCommonCustomForm.Create(nil);
  try
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
    LFirstInstance := GLastCreatedInstance;

    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));

    Assert.AreSame(LFirstInstance, GLastCreatedInstance,
      'Nenhuma segunda instancia deve ter sido criada enquanto a primeira esta ativa');
    Assert.AreEqual(1, TDialogQueueManager.Instance.DebugQueueLength(LForm));
  finally
    LForm.Free;
  end;
end;

procedure TDialogQueueManagerTests.TestNotifyClosed_PopsNextFromQueue;
var
  LForm: TCommonCustomForm;
  LFirstId: Integer;
begin
  LForm := TCommonCustomForm.Create(nil);
  try
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
    LFirstId := GLastCreatedInstance.Id;
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));

    TDialogQueueManager.Instance.NotifyClosed(LForm);

    Assert.AreNotEqual(LFirstId, GLastCreatedInstance.Id,
      'Uma nova instancia deve ter sido criada para o snapshot enfileirado');
    Assert.AreEqual(1, GLastCreatedInstance.ShowCallCount);
    Assert.AreEqual(0, TDialogQueueManager.Instance.DebugQueueLength(LForm));
    Assert.IsTrue(TDialogQueueManager.Instance.DebugIsActive(LForm));
  finally
    LForm.Free;
  end;
end;

procedure TDialogQueueManagerTests.TestFormDestruction_PurgesQueueAndDeactivatesInstance;
var
  LForm: TCommonCustomForm;
begin
  LForm := TCommonCustomForm.Create(nil);

  TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
  TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));

  Assert.IsTrue(TDialogQueueManager.Instance.DebugIsActive(LForm));
  Assert.AreEqual(1, TDialogQueueManager.Instance.DebugQueueLength(LForm));

  Assert.WillNotRaise(
    procedure
    begin
      LForm.Free;
    end);

  Assert.IsFalse(TDialogQueueManager.Instance.DebugIsActive(LForm),
    'Instancia ativa deve ser removida de FActive quando o form e destruido');
  Assert.AreEqual(0, TDialogQueueManager.Instance.DebugQueueLength(LForm),
    'Fila pendente deve ser purgada quando o form e destruido');
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogQueueManagerTests);

end.
