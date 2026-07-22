unit MultiDialog4FMX.Tests.Queue;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,
  MultiDialog4FMX.FMX,
  FMX.Forms,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Generics.Collections;

type
  // Expõe os membros protected de TFMXDialogInstance (FAlive, FTimeoutCancelled) para o
  // teste de regressao do C1 abaixo — mesmo padrao "Cracker" ja usado em
  // Tests/MultiDialog4FMX.Tests.Android.pas.
  TFMXDialogInstanceCracker = class(TFMXDialogInstance);

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

    [Test]
    procedure TestFormDestruction_RealInstanceWithPendingTimeoutThread_NoCrash;

    [Test]
    procedure TestTelemetry_EnqueueEmitsEnqueuedAndShown;
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

// Regressao do C1 (final review de Sprint 6): TDialogQueueManager.Notification suprime a
// instancia ativa e imediatamente a remove de FActive — que era a UNICA referencia com
// contagem de referencia mantendo a instancia viva. Sem um keepalive proprio em cada ponto
// de trabalho adiado/threaded (StartTimeoutCountdown, ButtonClick/ButtonTap/OnBackgroundClick
// via TThread.ForceQueue), a instancia era genuinamente liberada (nao so marcada FAlive :=
// False) enquanto a thread de contagem regressiva de timeout ainda podia estar dormindo —
// um use-after-free classico. Este teste exercita o caminho real (TFMXDialogInstance via
// factory de producao, nao o TFakeDialogInstance usado nos demais testes deste fixture, que
// tem Suppress como no-op e nao tem overlay/threads reais — ele nao poderia detectar este bug).
procedure TDialogQueueManagerTests.TestFormDestruction_RealInstanceWithPendingTimeoutThread_NoCrash;
var
  LForm        : TCommonCustomForm;
  LButtons     : TButtonHandlerList;
  LSnapshot    : TDialogSnapshot;
  LRealInstance: TFMXDialogInstanceCracker;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;

  LRealInstance := nil;

  // Registra a factory REAL (TFMXDialogInstance), nao a fake — precisa exercitar
  // StartTimeoutCountdown/CloseDialog/Suppress de verdade, nao um no-op.
  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      LRealInstance := TFMXDialogInstanceCracker.Create(ASnapshot);
      Result := LRealInstance;
    end);

  LForm := TCommonCustomForm.Create(nil);

  LButtons := TButtonHandlerList.Create(True);
  try
    LButtons.Add(TButtonHandler.Create);
    LButtons[0].Timeout := 3; // > 0 forca StartTimeoutCountdown a disparar a thread em segundo plano
    LSnapshot := TDialogSnapshot.Create(LForm, '', '', TMultiDialogType.mdtCustom,
      False, 14, 12, TDialogAnimation.danNone, TDialogTheme.dthAuto, '', 0, nil, LButtons);
  finally
    LButtons.Free; // TDialogSnapshot.Create ja fez sua propria copia profunda
  end;

  // Enqueue -> ShowNow -> LRealInstance.Show -> monta os controles FMX reais no form e
  // inicia a thread de contagem regressiva (Timeout=3s, ainda nao deve ter disparado).
  TDialogQueueManager.Instance.Enqueue(LForm, LSnapshot);

  Assert.IsNotNull(LRealInstance, 'A factory real deveria ter criado e mostrado uma instancia');
  Assert.IsTrue(LRealInstance.FAlive, 'Instancia recem-criada deve comecar viva');
  Assert.IsTrue(TDialogQueueManager.Instance.DebugIsActive(LForm));

  // Destroi o form ENQUANTO a thread de timeout ainda esta dormindo (Sleep(1000) dentro
  // do loop, Timeout=3s) — exatamente o cenario do C1: TDialogQueueManager.Notification
  // roda Suppress + FActive.Remove antes que a thread termine seu trabalho pendente.
  // Assert.WillNotRaise nao captura um AV real (o host DUnitX console derrubaria o
  // processo inteiro antes que qualquer assert pudesse rodar) — mas cobre qualquer
  // excecao Delphi normal levantada de forma sincrona nesta chamada.
  Assert.WillNotRaise(
    procedure
    begin
      LForm.Free;
    end,
    nil,
    'A destruicao do form nao deve levantar excecao mesmo com o timeout pendente');

  // Com o fix do C1 (LSelf capturado dentro do closure da thread de
  // StartTimeoutCountdown), LRealInstance continua sendo memoria heap valida aqui —
  // FActive.Remove(LForm) derrubou a UNICA referencia com contagem antes do fix, o que
  // teria liberado o objeto neste ponto exato. Se este Assert ler campos de memoria ja
  // liberada, o comportamento e indefinido (pode nao falhar de forma limpa) — mas o fato
  // de chegarmos ate aqui sem AV, combinado com FAlive genuinamente refletindo o Suppress
  // que acabou de rodar, e a evidencia disponivel de que a instancia sobreviveu.
  Assert.IsFalse(LRealInstance.FAlive,
    'Suppress deve ter marcado a instancia como nao viva apos a destruicao do form');
  Assert.IsFalse(TDialogQueueManager.Instance.DebugIsActive(LForm),
    'Form destruido nao deve mais ter instancia ativa registrada em FActive');

  // Da a thread em segundo plano (Sleep(1000) dentro do loop) uma chance real de acordar
  // e observar FTimeoutCancelled=True (setado por Suppress) depois que o form (e a
  // referencia de FActive) ja se foram — a janela exata que o bug original deixava como
  // use-after-free. CheckSynchronize bombeia TThread.Queue no thread principal, ja que o
  // host de console do DUnitX nao roda Application.ProcessMessages/HandleMessage.
  Sleep(1500);
  CheckSynchronize(500);

  // Chegar ate aqui sem travar o processo e a evidencia mais forte que um teste em
  // processo unico consegue produzir para esta classe de bug: um AV real na thread em
  // segundo plano provavelmente derrubaria o executable inteiro do DUnitX (sem sinalizar
  // como falha normal de um teste), entao "o executavel de teste continua rodando e
  // reporta os testes seguintes" e, na pratica, o sinal observavel de sucesso aqui — nao
  // apenas esta linha.
  Assert.Pass('Sequencia de destruicao do form + trabalho de timeout adiado completou sem crash do processo');
end;

procedure TDialogQueueManagerTests.TestTelemetry_EnqueueEmitsEnqueuedAndShown;
var
  LForm: TCommonCustomForm;
  LKinds: TList<TDialogEventKind>;
begin
  LKinds := TList<TDialogEventKind>.Create;
  try
    TDialogTelemetry.OnEvent :=
      procedure(const AInfo: TDialogEventInfo)
      begin
        LKinds.Add(AInfo.Kind);
      end;

    LForm := TCommonCustomForm.Create(nil);
    try
      TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
      Assert.IsTrue(LKinds.IndexOf(dekEnqueued) >= 0, 'deve emitir dekEnqueued');
      Assert.IsTrue(LKinds.IndexOf(dekShown) >= 0, 'deve emitir dekShown');
    finally
      LForm.Free;
    end;
  finally
    TDialogTelemetry.OnEvent := nil;
    LKinds.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogQueueManagerTests);

end.
