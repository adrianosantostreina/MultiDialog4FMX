unit MultiDialog4FMX.Tests.Await;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Util,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Await,
  MultiDialog4FMX.Queue,
  MultiDialog4FMX.FMX,
  FMX.Forms,
  System.UITypes,
  System.SysUtils,
  System.Classes;

type
  // Instancia visual fake para os testes de await: nao monta UI real. Dependendo de
  // GAutoRespond, agenda (via TThread.Queue) um CloseWith(GAwaitAutoResult) no thread
  // principal — simulando o usuario respondendo — ou nao responde (o teste resolve
  // destruindo o form -> Suppress -> mrNone).
  TFakeAwaitInstance = class(TInterfacedObject, IDialogVisualInstance)
  private
    FSnapshot: TDialogSnapshot;
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    procedure Show;
    procedure Suppress;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
  end;

  [TestFixture]
  TDialogAwaitTests = class
  public
    [Test]
    procedure TestShowAndWait_OnMainThread_Raises;

    [Test]
    procedure TestShowAndWait_ResolvesOnUIThread_ReturnsResult;

    [Test]
    procedure TestShowAndWait_FormDestroyed_ReturnsMrNone;
  end;

var
  GAwaitAutoResult: TModalResult = mrYes;
  GAutoRespond: Boolean = True;

implementation

{ TFakeAwaitInstance }

constructor TFakeAwaitInstance.Create(const ASnapshot: TDialogSnapshot);
begin
  inherited Create;
  FSnapshot := ASnapshot;
end;

destructor TFakeAwaitInstance.Destroy;
begin
  FSnapshot.Free;
  inherited;
end;

procedure TFakeAwaitInstance.Show;
var
  LSelf: IDialogVisualInstance;
begin
  if not GAutoRespond then
    Exit;
  LSelf := Self;   // keepalive ate o closure rodar
  TThread.Queue(nil,
    procedure
    begin
      LSelf.CloseWith(GAwaitAutoResult);
    end);
end;

procedure TFakeAwaitInstance.Suppress;
begin
  if Assigned(FSnapshot.ResultCallback) then
    FSnapshot.ResultCallback(mrNone);
end;

function TFakeAwaitInstance.SnapshotId: Integer;
begin
  Result := FSnapshot.Id;
end;

procedure TFakeAwaitInstance.CloseWith(const AResult: TModalResult);
begin
  if Assigned(FSnapshot.ResultCallback) then
    FSnapshot.ResultCallback(AResult);
  TDialogQueueManager.Instance.NotifyClosed(FSnapshot.Form);
end;

{ TDialogAwaitTests }

procedure TDialogAwaitTests.TestShowAndWait_OnMainThread_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMultiDialog4FMX.Dialog
        .Buttons.AddButton('OK').&End
        .ShowAndWait;   // rodando na main thread do runner
    end,
    EDialogAwaitOnMainThread);
end;

procedure TDialogAwaitTests.TestShowAndWait_ResolvesOnUIThread_ReturnsResult;
var
  LForm: TCommonCustomForm;
  LWorkerResult: TModalResult;
  LDone: Boolean;
  LSpin: Integer;
begin
  GAutoRespond := True;
  GAwaitAutoResult := mrYes;
  LWorkerResult := mrNone;
  LDone := False;

  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFakeAwaitInstance.Create(ASnapshot);
    end);

  LForm := TCommonCustomForm.Create(nil);
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        LWorkerResult := TMultiDialog4FMX.Dialog
          .Buttons.AddButton('Sim', TAlphaColorRec.Null, '', mrYes).&End
          .ShowAndWait(LForm);
        LDone := True;
      end).Start;

    // Bombeia TThread.Queue no main thread ate a worker terminar (timeout guard ~5s).
    LSpin := 0;
    while (not LDone) and (LSpin < 500) do
    begin
      CheckSynchronize(10);
      Inc(LSpin);
    end;

    Assert.IsTrue(LDone, 'A worker thread deve ter completado dentro do timeout');
    Assert.AreEqual(mrYes, LWorkerResult, 'ShowAndWait deve retornar o resultado resolvido (mrYes)');
  finally
    LForm.Free;
  end;
end;

procedure TDialogAwaitTests.TestShowAndWait_FormDestroyed_ReturnsMrNone;
var
  LForm: TCommonCustomForm;
  LWorkerResult: TModalResult;
  LDone: Boolean;
  LSpin: Integer;
begin
  // Fake que NUNCA responde sozinho: forcamos a resolucao destruindo o form.
  GAutoRespond := False;
  LWorkerResult := mrOk;
  LDone := False;

  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFakeAwaitInstance.Create(ASnapshot);
    end);

  LForm := TCommonCustomForm.Create(nil);

  TThread.CreateAnonymousThread(
    procedure
    begin
      LWorkerResult := TMultiDialog4FMX.Dialog
        .Buttons.AddButton('OK').&End
        .ShowAndWait(LForm);
      LDone := True;
    end).Start;

  // Da tempo do dialogo enfileirar/mostrar, entao destroi o form (dispara Suppress -> mrNone).
  LSpin := 0;
  while (LSpin < 50) and (not TDialogQueueManager.Instance.DebugIsActive(LForm)) do
  begin
    CheckSynchronize(10);
    Inc(LSpin);
  end;
  LForm.Free;   // Notification -> Suppress -> resolve mrNone

  LSpin := 0;
  while (not LDone) and (LSpin < 500) do
  begin
    CheckSynchronize(10);
    Inc(LSpin);
  end;

  // restaura default para nao afetar outros testes
  GAutoRespond := True;

  Assert.IsTrue(LDone, 'A worker deve desbloquear quando o form e destruido');
  Assert.AreEqual(mrNone, LWorkerResult, 'Form destruido resolve o await com mrNone');
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogAwaitTests);

end.
