# Sprint 7 (lifecycle) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Dar ao MultiDialog4FMX ciclo de vida programável — close por código, telemetria de eventos e async/await em worker thread — sem alterar a API pública existente.

**Architecture:** Sobre a fila da Sprint 6, introduz uma **resolução única** (cada diálogo resolve exatamente uma vez, com um `TModalResult`, por qualquer caminho de fechamento, inclusive `mrNone` na destruição do form). Essa fundação alimenta as três features: telemetria emite eventos nos pontos de ciclo de vida; `IDialogHandle` fecha por código via token; `ShowAndWait` bloqueia a worker thread num `TEvent` sinalizado pela resolução. Entrega em **2 fases** (A: resolução+close+telemetria; B: await) com checkpoint entre elas.

**Tech Stack:** Delphi/Object Pascal, FireMonkey (FMX), DUnitX (testes console headless), `System.Diagnostics.TStopwatch`, `System.SyncObjs.TEvent`, `System.Classes.TThread`.

## Global Constraints

- **API pública atual inalterada** — só adições. `Show`/`Show(AForm)`/`SetOnResult`/samples continuam idênticos.
- **Retrocompatível com Delphi ≤ 11** — usar apenas `TEvent`, `TModalResult`, `TStopwatch`, `System.Threading`/`TThread` (todos existem em ≥ XE7); preservar guardas `{$IF CompilerVersion >= NN}` já presentes.
- **Sem regressão** — os 87 testes da Sprint 6 continuam passando (com o ajuste de um contrato: `SetOnResult` passa a disparar também na destruição do form, com `mrNone`).
- **Encoding** — `.pas` em UTF-8 com BOM; acentos como caracteres reais, nunca `'x'+#nnn`.
- **Emissão de telemetria com custo zero** quando ninguém escuta (`if Assigned(OnEvent)`).
- **Sentinel de destruição do form** = `mrNone` (verbatim da spec).
- **Build/teste (console, contagem confiável):**
  ```
  call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
  cd Tests
  dcc32 -B -Q -E".\Win32\Debug" -N".\Win32\Debug\dcu" MultiDialog4FMX.Tests.dpr
  .\Win32\Debug\MultiDialog4FMX.Tests.exe -exit:Continue
  ```
  O runner usa RTTI e roda **toda** a suíte; cada ciclo TDD compila + roda tudo e verifica o teste alvo pelo nome no output do console (`Tests Found/Passed/Failed`).

---

# FASE A — Resolução única + Telemetria + Close programático

## Task A1: Tipos de telemetria + dispatcher

**Files:**
- Modify: `src/MultiDialog4FMX.Interfaces.pas` (adicionar tipos, após a linha 53 `TDialogResultProc`)
- Create: `src/MultiDialog4FMX.Telemetry.pas`
- Create: `Tests/MultiDialog4FMX.Tests.Telemetry.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.dpr` (registrar as 2 units novas)

**Interfaces:**
- Produces: `TDialogEventKind = (dekEnqueued, dekShown, dekButtonClicked, dekCancelled, dekTimedOut, dekClosed, dekSuppressed)`; `TDialogEventInfo` (record com `Kind, DialogType, Title, Result, ElapsedMs`); `TDialogEventProc = reference to procedure(const AInfo: TDialogEventInfo)`; `TDialogTelemetry` com `class property OnEvent` + `class procedure Emit(const AInfo: TDialogEventInfo); static`.

- [ ] **Step 1: Adicionar os tipos em `Interfaces.pas`**

No `type` de `MultiDialog4FMX.Interfaces.pas`, logo após `TDialogResultProc = reference to procedure(const AResult: TModalResult);` (linha 53):

```pascal
  TDialogEventKind = (dekEnqueued, dekShown, dekButtonClicked, dekCancelled,
                      dekTimedOut, dekClosed, dekSuppressed);

  TDialogEventInfo = record
    Kind: TDialogEventKind;
    DialogType: TMultiDialogType;
    Title: string;
    Result: TModalResult;   // valido em dekButtonClicked/dekClosed; mrNone quando nao aplica
    ElapsedMs: Int64;        // tempo desde dekEnqueued
  end;

  TDialogEventProc = reference to procedure(const AInfo: TDialogEventInfo);
```

- [ ] **Step 2: Criar a unit `MultiDialog4FMX.Telemetry.pas`** (UTF-8 com BOM)

```pascal
unit MultiDialog4FMX.Telemetry;

interface

uses
  MultiDialog4FMX.Interfaces;

type
  /// <summary>Dispatcher global de eventos de ciclo de vida do dialogo.</summary>
  TDialogTelemetry = class
  private
    class var FOnEvent: TDialogEventProc;
  public
    class property OnEvent: TDialogEventProc read FOnEvent write FOnEvent;
    class procedure Emit(const AInfo: TDialogEventInfo); static;
  end;

implementation

class procedure TDialogTelemetry.Emit(const AInfo: TDialogEventInfo);
begin
  if Assigned(FOnEvent) then
    FOnEvent(AInfo);
end;

end.
```

- [ ] **Step 3: Criar o teste `Tests/MultiDialog4FMX.Tests.Telemetry.pas`** (UTF-8 com BOM)

```pascal
unit MultiDialog4FMX.Tests.Telemetry;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,
  System.UITypes;

type
  [TestFixture]
  TDialogTelemetryTests = class
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmit_WithListener_DeliversInfo;

    [Test]
    procedure TestEmit_NoListener_DoesNotRaise;
  end;

implementation

procedure TDialogTelemetryTests.TearDown;
begin
  TDialogTelemetry.OnEvent := nil;
end;

procedure TDialogTelemetryTests.TestEmit_WithListener_DeliversInfo;
var
  LReceived: TDialogEventInfo;
  LCalled: Boolean;
  LInfo: TDialogEventInfo;
begin
  LCalled := False;
  TDialogTelemetry.OnEvent :=
    procedure(const AInfo: TDialogEventInfo)
    begin
      LCalled := True;
      LReceived := AInfo;
    end;

  LInfo := Default(TDialogEventInfo);
  LInfo.Kind := dekShown;
  LInfo.DialogType := mdtWarning;
  LInfo.Title := 'Ola';
  LInfo.Result := mrOk;
  TDialogTelemetry.Emit(LInfo);

  Assert.IsTrue(LCalled, 'O listener deve receber o evento');
  Assert.AreEqual(Ord(dekShown), Ord(LReceived.Kind));
  Assert.AreEqual('Ola', LReceived.Title);
  Assert.AreEqual(mrOk, LReceived.Result);
end;

procedure TDialogTelemetryTests.TestEmit_NoListener_DoesNotRaise;
var
  LInfo: TDialogEventInfo;
begin
  TDialogTelemetry.OnEvent := nil;
  LInfo := Default(TDialogEventInfo);
  LInfo.Kind := dekEnqueued;
  Assert.WillNotRaise(
    procedure
    begin
      TDialogTelemetry.Emit(LInfo);
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogTelemetryTests);

end.
```

- [ ] **Step 4: Registrar as units novas no `.dpr`**

Em `Tests/MultiDialog4FMX.Tests.dpr`, na cláusula `uses`: após a linha `MultiDialog4FMX.FMX in '..\src\MultiDialog4FMX.FMX.pas',` (linha 20) adicionar:

```pascal
  MultiDialog4FMX.Telemetry in '..\src\MultiDialog4FMX.Telemetry.pas',
```

E após a linha `MultiDialog4FMX.Tests.Queue in 'MultiDialog4FMX.Tests.Queue.pas';` (linha 32, última — trocar o `;` por `,` e adicionar):

```pascal
  MultiDialog4FMX.Tests.Queue in 'MultiDialog4FMX.Tests.Queue.pas',
  MultiDialog4FMX.Tests.Telemetry in 'MultiDialog4FMX.Tests.Telemetry.pas';
```

- [ ] **Step 5: Compilar e rodar — verificar que os 2 testes novos passam**

Run: build+run (bloco em Global Constraints).
Expected: `Tests Passed` sobe em 2 (de 87 para 89); `Failed : 0`; `TDialogTelemetryTests` sem falhas no output.

- [ ] **Step 6: Commit**

```bash
git add src/MultiDialog4FMX.Interfaces.pas src/MultiDialog4FMX.Telemetry.pas Tests/MultiDialog4FMX.Tests.Telemetry.pas Tests/MultiDialog4FMX.Tests.dpr
git commit -m "feat(sprint7): tipos de telemetria + dispatcher global TDialogTelemetry"
```

---

## Task A2: Snapshot ganha Id + medição de tempo

**Files:**
- Modify: `src/MultiDialog4FMX.Queue.pas` (classe `TDialogSnapshot`)
- Modify: `Tests/MultiDialog4FMX.Tests.Snapshot.pas` (adicionar 1 teste)

**Interfaces:**
- Consumes: assinatura atual de `TDialogSnapshot.Create` (13 parâmetros) — **não muda**.
- Produces: `TDialogSnapshot.Id: Integer` (read-only, sequencial, único por instância); `TDialogSnapshot.ElapsedMs: Int64`.

- [ ] **Step 1: Escrever o teste falho** em `Tests/MultiDialog4FMX.Tests.Snapshot.pas`

Adicionar ao fixture existente um método `[Test] procedure TestSnapshot_HasUniqueIncrementingId;` (declaração na classe) e a implementação:

```pascal
procedure {NomeDoFixture}.TestSnapshot_HasUniqueIncrementingId;
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
```

> Verifique o nome exato do fixture no topo de `Tests.Snapshot.pas` e use-o. Confirme que o `uses` do teste inclui `System.UITypes` (para `mdtCustom`/`danNone`/`dthAuto`); se não, adicione.

- [ ] **Step 2: Rodar — verificar que falha na compilação** (`Id`/`ElapsedMs` não existem)

Run: build+run. Expected: erro de compilação `E2003 Undeclared identifier: 'Id'`.

- [ ] **Step 3: Implementar `Id` + `ElapsedMs` em `TDialogSnapshot`**

Em `src/MultiDialog4FMX.Queue.pas`:

No `uses` da interface, adicionar `System.Diagnostics,` (para `TStopwatch`).

Na seção `private` de `TDialogSnapshot` (antes de `FForm`):

```pascal
    class var FNextId: Integer;
    FId: Integer;
    FStopwatch: TStopwatch;
```

Na seção `public`, junto às demais properties:

```pascal
    property Id: Integer read FId;
    function ElapsedMs: Int64;
```

No `constructor TDialogSnapshot.Create`, logo após `inherited Create;`:

```pascal
  Inc(FNextId);
  FId := FNextId;
  FStopwatch := TStopwatch.StartNew;
```

E adicionar o método na implementation:

```pascal
function TDialogSnapshot.ElapsedMs: Int64;
begin
  Result := FStopwatch.ElapsedMilliseconds;
end;
```

- [ ] **Step 4: Rodar — verificar que passa**

Run: build+run. Expected: `Tests Passed` +1 (90); `TestSnapshot_HasUniqueIncrementingId` OK.

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.Queue.pas Tests/MultiDialog4FMX.Tests.Snapshot.pas
git commit -m "feat(sprint7): TDialogSnapshot ganha Id sequencial + ElapsedMs"
```

---

## Task A3: Resolução única na instância visual (idempotência)

**Files:**
- Modify: `src/MultiDialog4FMX.FMX.pas` (`TFMXDialogInstance`)
- Modify: `Tests/MultiDialog4FMX.Tests.Android.pas` (adicionar testes ao fixture `TAndroidDialogCloseTests`)

**Interfaces:**
- Produces: campo `TFMXDialogInstance.FResolved: Boolean`; campo `FOverlay: TLayout`; método `protected procedure DoResolve(const AResult: TModalResult)` (idempotente: dispara `FSnapshot.ResultCallback` no máximo uma vez).
- Consumes: `FSnapshot.ResultCallback` (existente).

**Contexto:** hoje `ButtonClick`/`ButtonTap`/`OnBackgroundClick` chamam `FSnapshot.ResultCallback` diretamente. Vamos centralizar em `DoResolve`, tornando a resolução idempotente e reutilizável pelos caminhos de close/suppress das próximas tasks. Comportamento observável dos testes existentes é preservado (o callback ainda dispara uma vez com o mesmo `ModalResult`).

- [ ] **Step 1: Escrever os testes falhos** em `TAndroidDialogCloseTests` (`Tests.Android.pas`)

Declarar no fixture: `[Test] procedure TestDoResolve_IsIdempotent;` e implementar:

```pascal
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
```

> `TFMXDialogInstanceCracker` já é declarado no topo de `Tests.Android.pas`. `DoResolve` é `protected` — acessível via o Cracker.

- [ ] **Step 2: Rodar — verificar que falha** (`DoResolve` não existe)

Run: build+run. Expected: `E2003 Undeclared identifier: 'DoResolve'`.

- [ ] **Step 3: Implementar `DoResolve` + `FResolved` + `FOverlay`**

Em `src/MultiDialog4FMX.FMX.pas`, na seção `protected` de `TFMXDialogInstance` (junto aos campos, após `FTimeoutCancelled : Boolean;`):

```pascal
    FResolved : Boolean;
    FOverlay  : TLayout;
    procedure DoResolve(const AResult: TModalResult);
```

Implementação (na region de métodos protegidos):

```pascal
procedure TFMXDialogInstance.DoResolve(const AResult: TModalResult);
begin
  if FResolved then
    Exit;
  FResolved := True;
  if Assigned(FSnapshot.ResultCallback) then
    FSnapshot.ResultCallback(AResult);
end;
```

Guardar `FOverlay` em `Show` — em `procedure TFMXDialogInstance.Show`, após `LOverlay := BuildOverlay(FSnapshot.Form, LBgRect);`:

```pascal
  FOverlay := LOverlay;
```

Substituir as chamadas diretas ao callback por `DoResolve`:

Em `ButtonClick`, trocar o bloco:
```pascal
    if Assigned(FSnapshot.ResultCallback) then
      FSnapshot.ResultCallback(Obj.ModalResult);
```
por:
```pascal
    DoResolve(Obj.ModalResult);
```

Em `ButtonTap`, trocar:
```pascal
    if Assigned(FSnapshot.ResultCallback) then
      FSnapshot.ResultCallback(Obj.ModalResult);
```
por:
```pascal
    DoResolve(Obj.ModalResult);
```

Em `OnBackgroundClick`, trocar:
```pascal
      if Assigned(FSnapshot.ResultCallback) then
        FSnapshot.ResultCallback(mrCancel);
```
por:
```pascal
      DoResolve(mrCancel);
```

- [ ] **Step 4: Rodar — verificar que passa e não houve regressão**

Run: build+run. Expected: `TestDoResolve_IsIdempotent` OK; os testes `TestButtonClick_InvokesResultCallback`, `TestOnBackgroundClick_CallbackWithMrCancel`, `TestButtonClick_CallbackBeforeClose` continuam passando. Total 91, `Failed : 0`.

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.FMX.pas Tests/MultiDialog4FMX.Tests.Android.pas
git commit -m "refactor(sprint7): resolucao unica idempotente (DoResolve) na instancia visual"
```

---

## Task A4: Emissão de telemetria nos pontos de ciclo de vida + suppress resolve mrNone

**Files:**
- Modify: `src/MultiDialog4FMX.Queue.pas` (helper `MakeDialogEventInfo`; emitir `dekEnqueued`; `dekShown` no `ShowNow`)
- Modify: `src/MultiDialog4FMX.FMX.pas` (emitir `dekButtonClicked`/`dekCancelled`/`dekTimedOut`/`dekClosed`; `Suppress` resolve `mrNone` + `dekSuppressed`)
- Modify: `Tests/MultiDialog4FMX.Tests.Queue.pas` (teste de sequência de eventos + suppress callback)

**Interfaces:**
- Produces: `function MakeDialogEventInfo(const AKind: TDialogEventKind; const ASnapshot: TDialogSnapshot; const AResult: TModalResult): TDialogEventInfo;` (em `Queue.pas`, exposto na interface).
- Consumes: `TDialogTelemetry.Emit` (Task A1); `TDialogSnapshot.Id/ElapsedMs` (Task A2); `DoResolve` (Task A3).

- [ ] **Step 1: Escrever o teste falho** em `Tests.Queue.pas`

No `uses` da unit de teste, adicionar `MultiDialog4FMX.Telemetry,`. Declarar no fixture `TDialogQueueManagerTests`: `[Test] procedure TestTelemetry_EnqueueEmitsEnqueuedAndShown;` e implementar:

```pascal
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
```

> Adicionar `System.Generics.Collections` ao `uses` do teste se ainda não estiver. O `TFakeDialogInstance.Show` do fixture não emite `dekShown` sozinho — a emissão de `dekShown` é feita pelo manager em `ShowNow` (Step 3), então o fake serve.

- [ ] **Step 2: Rodar — verificar que falha**

Run: build+run. Expected: FAIL em `TestTelemetry_EnqueueEmitsEnqueuedAndShown` ("deve emitir dekEnqueued").

- [ ] **Step 3: Emitir `dekEnqueued`/`dekShown` no manager + helper**

Em `src/MultiDialog4FMX.Queue.pas`:

No `uses` da interface, adicionar `MultiDialog4FMX.Telemetry,`.

Adicionar na interface (após a declaração de `TDialogQueueManager`, antes do `implementation` não — deixar como função de unit; declarar no `interface` após os types):

```pascal
function MakeDialogEventInfo(const AKind: TDialogEventKind;
  const ASnapshot: TDialogSnapshot; const AResult: TModalResult): TDialogEventInfo;
```

Implementação:

```pascal
function MakeDialogEventInfo(const AKind: TDialogEventKind;
  const ASnapshot: TDialogSnapshot; const AResult: TModalResult): TDialogEventInfo;
begin
  Result.Kind := AKind;
  Result.DialogType := ASnapshot.MsgType;
  Result.Title := ASnapshot.Title;
  Result.Result := AResult;
  Result.ElapsedMs := ASnapshot.ElapsedMs;
end;
```

Em `TDialogQueueManager.Enqueue`, logo no início (após `begin`):

```pascal
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekEnqueued, ASnapshot, mrNone));
```

Em `TDialogQueueManager.ShowNow`, após `LInstance.Show;`:

```pascal
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekShown, ASnapshot, mrNone));
```

- [ ] **Step 4: Rodar — verificar que o teste de sequência passa**

Run: build+run. Expected: `TestTelemetry_EnqueueEmitsEnqueuedAndShown` OK.

- [ ] **Step 5: Escrever o teste falho de suppress→mrNone** em `Tests.Android.pas`

No fixture `TAndroidDialogCloseTests`, declarar `[Test] procedure TestSuppress_ResolvesWithMrNone;` e implementar:

```pascal
procedure TAndroidDialogCloseTests.TestSuppress_ResolvesWithMrNone;
var
  Instance: TFMXDialogInstanceCracker;
  LButtons: TButtonHandlerList;
  LResult: TModalResult;
  LCalled: Boolean;
begin
  LResult := mrOk;
  LCalled := False;
  LButtons := TButtonHandlerList.Create(True);
  Instance := TFMXDialogInstanceCracker.Create(
    TDialogSnapshot.Create(nil, '', '', mdtCustom, False, 14, 12, danNone, dthAuto,
      '', 0,
      procedure(const AResult: TModalResult)
      begin
        LCalled := True;
        LResult := AResult;
      end,
      LButtons));
  LButtons.Free;
  try
    Instance.Suppress;
    Assert.IsTrue(LCalled, 'Suppress deve resolver o dialogo (disparar callback)');
    Assert.AreEqual(mrNone, LResult, 'Suppress deve resolver com mrNone');
  finally
    Instance := nil;
  end;
end;
```

- [ ] **Step 6: Rodar — verificar que falha** (`Suppress` ainda não resolve)

Run: build+run. Expected: FAIL ("Suppress deve resolver o dialogo").

- [ ] **Step 7: Emitir eventos de fechamento + `Suppress` resolve `mrNone`**

Em `src/MultiDialog4FMX.FMX.pas`:

No `uses`, adicionar `MultiDialog4FMX.Telemetry,`.

`Suppress` passa a resolver e emitir:
```pascal
procedure TFMXDialogInstance.Suppress;
begin
  FAlive := False;
  FTimeoutCancelled := True;
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekSuppressed, FSnapshot, mrNone));
  DoResolve(mrNone);
end;
```

Em `ButtonClick`, imediatamente antes de `DoResolve(Obj.ModalResult);`:
```pascal
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekButtonClicked, FSnapshot, Obj.ModalResult));
```

Em `ButtonTap`, imediatamente antes de `DoResolve(Obj.ModalResult);`:
```pascal
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekButtonClicked, FSnapshot, Obj.ModalResult));
```

Em `OnBackgroundClick`, imediatamente antes de `DoResolve(mrCancel);`:
```pascal
      TDialogTelemetry.Emit(MakeDialogEventInfo(dekCancelled, FSnapshot, mrCancel));
```

Em `AutoClickTimeoutButton`, no início (antes de `if Assigned(FTimeoutButton)`):
```pascal
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekTimedOut, FSnapshot, mrNone));
```

Em `CloseDialog`, dentro de `LDoDestroy`, imediatamente antes de `TDialogQueueManager.Instance.NotifyClosed(LForm);`:
```pascal
    TDialogTelemetry.Emit(MakeDialogEventInfo(dekClosed, FSnapshot, mrNone));
```

> `AutoClickTimeoutButton` chama `ButtonClick`, que também emite `dekButtonClicked` — semântica: um timeout produz `dekTimedOut` seguido de `dekButtonClicked` (o "clique automático"). Documentado e aceito.

- [ ] **Step 8: Rodar — verificar que passa e sem regressão**

Run: build+run. Expected: `TestSuppress_ResolvesWithMrNone` OK; suíte inteira `Failed : 0`. Total ~93.

- [ ] **Step 9: Commit**

```bash
git add src/MultiDialog4FMX.Queue.pas src/MultiDialog4FMX.FMX.pas Tests/MultiDialog4FMX.Tests.Queue.pas Tests/MultiDialog4FMX.Tests.Android.pas
git commit -m "feat(sprint7): emissao de telemetria nos 7 eventos + suppress resolve mrNone"
```

---

## Task A5: Close programático na instância + manager (CloseByHandle)

**Files:**
- Modify: `src/MultiDialog4FMX.Queue.pas` (interface `IDialogVisualInstance`; `TDialogQueueManager.CloseByHandle`/`IsHandleActive`)
- Modify: `src/MultiDialog4FMX.FMX.pas` (`TFMXDialogInstance` implementa `SnapshotId`/`CloseWith`)
- Modify: `Tests/MultiDialog4FMX.Tests.Queue.pas` (`TFakeDialogInstance` implementa os novos métodos + testes)

**Interfaces:**
- Produces: `IDialogVisualInstance` ganha `function SnapshotId: Integer;` e `procedure CloseWith(const AResult: TModalResult);`. `TDialogQueueManager.CloseByHandle(const AForm: TCommonCustomForm; const AId: Integer; const AResult: TModalResult)` e `function IsHandleActive(const AForm: TCommonCustomForm; const AId: Integer): Boolean`.
- Consumes: `DoResolve`/`FOverlay`/`CloseDialog` (Task A3); `TDialogSnapshot.Id` (Task A2).

- [ ] **Step 1: Estender a interface `IDialogVisualInstance`**

Em `src/MultiDialog4FMX.Queue.pas`, na declaração de `IDialogVisualInstance`:

```pascal
  IDialogVisualInstance = interface
    ['{7F1B0A11-4E9A-4C4B-9C4B-2B4E7A0E1F10}']
    procedure Show;
    procedure Suppress;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
  end;
```

- [ ] **Step 2: Atualizar o `TFakeDialogInstance` do teste** (senão não compila)

Em `Tests/MultiDialog4FMX.Tests.Queue.pas`, na classe `TFakeDialogInstance` adicionar à parte pública:

```pascal
    FClosedWith: TModalResult;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
```

E implementar:

```pascal
function TFakeDialogInstance.SnapshotId: Integer;
begin
  Result := FSnapshot.Id;
end;

procedure TFakeDialogInstance.CloseWith(const AResult: TModalResult);
begin
  FClosedWith := AResult;
  // Simula o fim do ciclo: resolve o callback e avisa o manager (como a instancia real).
  if Assigned(FSnapshot.ResultCallback) then
    FSnapshot.ResultCallback(AResult);
  TDialogQueueManager.Instance.NotifyClosed(FSnapshot.Form);
end;
```

> Inicialize `FClosedWith := mrNone;` no construtor do fake.

- [ ] **Step 3: Implementar `SnapshotId`/`CloseWith` na instância real**

Em `src/MultiDialog4FMX.FMX.pas`, adicionar na parte `public` de `TFMXDialogInstance` (junto de `Show`/`Suppress`):

```pascal
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
```

Implementação:

```pascal
function TFMXDialogInstance.SnapshotId: Integer;
begin
  Result := FSnapshot.Id;
end;

procedure TFMXDialogInstance.CloseWith(const AResult: TModalResult);
begin
  if FResolved or not FAlive then
    Exit;
  DoResolve(AResult);
  if Assigned(FOverlay) then
    CloseDialog(FOverlay);
end;
```

- [ ] **Step 4: Escrever o teste falho de `CloseByHandle`** em `Tests.Queue.pas`

Declarar no fixture: `[Test] procedure TestCloseByHandle_Active_ClosesAndPopsQueue;` e `[Test] procedure TestCloseByHandle_Queued_RemovesFromQueue;`. Implementar:

```pascal
procedure TDialogQueueManagerTests.TestCloseByHandle_Active_ClosesAndPopsQueue;
var
  LForm: TCommonCustomForm;
  LActiveId: Integer;
begin
  LForm := TCommonCustomForm.Create(nil);
  try
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm));
    LActiveId := GLastCreatedInstance.Id;   // Id da instancia fake ativa
    // Para obter o Id do snapshot ativo, usamos SnapshotId via manager:
    Assert.IsTrue(TDialogQueueManager.Instance.IsHandleActive(
      LForm, TDialogQueueManager.Instance.DebugActiveSnapshotId(LForm)));

    TDialogQueueManager.Instance.CloseByHandle(
      LForm, TDialogQueueManager.Instance.DebugActiveSnapshotId(LForm), mrCancel);

    Assert.IsFalse(TDialogQueueManager.Instance.DebugIsActive(LForm),
      'Apos CloseByHandle do ativo, nada deve estar ativo (fila vazia)');
  finally
    LForm.Free;
  end;
end;

procedure TDialogQueueManagerTests.TestCloseByHandle_Queued_RemovesFromQueue;
var
  LForm: TCommonCustomForm;
  LQueuedSnap: TDialogSnapshot;
  LQueuedId: Integer;
begin
  LForm := TCommonCustomForm.Create(nil);
  try
    TDialogQueueManager.Instance.Enqueue(LForm, MakeSnapshot(LForm)); // vira ativo
    LQueuedSnap := MakeSnapshot(LForm);
    LQueuedId := LQueuedSnap.Id;
    TDialogQueueManager.Instance.Enqueue(LForm, LQueuedSnap);          // vai pra fila
    Assert.AreEqual(1, TDialogQueueManager.Instance.DebugQueueLength(LForm));

    TDialogQueueManager.Instance.CloseByHandle(LForm, LQueuedId, mrCancel);

    Assert.AreEqual(0, TDialogQueueManager.Instance.DebugQueueLength(LForm),
      'Snapshot enfileirado deve ser removido da fila por CloseByHandle');
    Assert.IsTrue(TDialogQueueManager.Instance.DebugIsActive(LForm),
      'O dialogo ativo original permanece ativo');
  finally
    LForm.Free;
  end;
end;
```

> `DebugActiveSnapshotId` é um acessor de teste novo (Step 5). Ele expõe o `SnapshotId` da instância ativa.

- [ ] **Step 5: Rodar — verificar que falha** (`CloseByHandle`/`IsHandleActive`/`DebugActiveSnapshotId` não existem)

Run: build+run. Expected: `E2003` para os métodos ausentes.

- [ ] **Step 6: Implementar `CloseByHandle`/`IsHandleActive` + acessor de debug no manager**

Em `src/MultiDialog4FMX.Queue.pas`, declarar em `public` de `TDialogQueueManager`:

```pascal
    procedure CloseByHandle(const AForm: TCommonCustomForm; const AId: Integer;
      const AResult: TModalResult);
    function IsHandleActive(const AForm: TCommonCustomForm; const AId: Integer): Boolean;
    function DebugActiveSnapshotId(const AForm: TCommonCustomForm): Integer;
```

Implementação:

```pascal
procedure TDialogQueueManager.CloseByHandle(const AForm: TCommonCustomForm;
  const AId: Integer; const AResult: TModalResult);
var
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
  LTemp: TArray<TDialogSnapshot>;
  LSnap: TDialogSnapshot;
  I: Integer;
begin
  // 1) e o dialogo ativo deste form?
  if FActive.TryGetValue(AForm, LInstance) and (LInstance.SnapshotId = AId) then
  begin
    LInstance.CloseWith(AResult);
    Exit;
  end;

  // 2) esta enfileirado? reconstrua a fila sem o item alvo, resolvendo-o.
  if FQueues.TryGetValue(AForm, LQueue) then
  begin
    LTemp := LQueue.ToArray;
    LQueue.Clear;
    for I := 0 to High(LTemp) do
    begin
      LSnap := LTemp[I];
      if LSnap.Id = AId then
      begin
        if Assigned(LSnap.ResultCallback) then
          LSnap.ResultCallback(AResult);
        TDialogTelemetry.Emit(MakeDialogEventInfo(dekClosed, LSnap, AResult));
        LSnap.Free;   // nunca virou instancia visual — o manager o possui
      end
      else
        LQueue.Enqueue(LSnap);
    end;
  end;
  // 3) nao encontrado -> ja resolvido -> no-op
end;

function TDialogQueueManager.IsHandleActive(const AForm: TCommonCustomForm;
  const AId: Integer): Boolean;
var
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
  LSnap: TDialogSnapshot;
begin
  if FActive.TryGetValue(AForm, LInstance) and (LInstance.SnapshotId = AId) then
    Exit(True);
  if FQueues.TryGetValue(AForm, LQueue) then
    for LSnap in LQueue do
      if LSnap.Id = AId then
        Exit(True);
  Result := False;
end;

function TDialogQueueManager.DebugActiveSnapshotId(const AForm: TCommonCustomForm): Integer;
var
  LInstance: IDialogVisualInstance;
begin
  if FActive.TryGetValue(AForm, LInstance) then
    Result := LInstance.SnapshotId
  else
    Result := -1;
end;
```

> `TQueue<T>` não permite remoção arbitrária; drenar via `ToArray`, limpar e re-enfileirar é o padrão determinístico. Iterar `for LSnap in LQueue` é suportado (enumerador do `TQueue`).

- [ ] **Step 7: Rodar — verificar que passa e sem regressão**

Run: build+run. Expected: os 2 testes de `CloseByHandle` OK; suíte `Failed : 0`. Total ~95.

- [ ] **Step 8: Commit**

```bash
git add src/MultiDialog4FMX.Queue.pas src/MultiDialog4FMX.FMX.pas Tests/MultiDialog4FMX.Tests.Queue.pas
git commit -m "feat(sprint7): close programatico na instancia + CloseByHandle no manager"
```

---

## Task A6: `IDialogHandle` + `ShowGetHandle` no builder

**Files:**
- Modify: `src/MultiDialog4FMX.Interfaces.pas` (`IDialogHandle`; `IDialogBuilder.ShowGetHandle`)
- Modify: `src/MultiDialog4FMX.Queue.pas` (classe concreta `TDialogHandle`)
- Modify: `src/MultiDialog4FMX.Base.pas` (`BuildSnapshot` helper DRY; `ShowGetHandle`)
- Modify: `Tests/MultiDialog4FMX.Tests.Builder.pas` (teste end-to-end do handle)

**Interfaces:**
- Produces: `IDialogHandle` (`Close`/`Close(AResult)`/`IsActive`); `IDialogBuilder.ShowGetHandle(const AForm: TCommonCustomForm = nil): IDialogHandle`; `TDialogHandle` (implementação).
- Consumes: `TDialogQueueManager.CloseByHandle`/`IsHandleActive` (Task A5); `TDialogSnapshot.Id` (A2).

- [ ] **Step 1: Declarar `IDialogHandle` + método no builder** (`Interfaces.pas`)

Antes de `IDialogBuilder`, adicionar:

```pascal
  /// <summary>Referencia a um dialogo disparado, para fecha-lo por codigo.</summary>
  IDialogHandle = interface
    ['{2C7A9E14-8B3D-4F6A-9E21-5D0C4B8F1A73}']
    procedure Close; overload;
    procedure Close(const AResult: TModalResult); overload;
    function IsActive: Boolean;
  end;
```

Dentro de `IDialogBuilder`, após os dois `Show`:

```pascal
    /// <summary>Exibe e devolve um handle para fechar o dialogo por codigo.</summary>
    function ShowGetHandle(const AForm: TCommonCustomForm = nil): IDialogHandle;
```

- [ ] **Step 2: Implementar `TDialogHandle`** em `Queue.pas`

Declarar no `type` (após `TDialogQueueManager`):

```pascal
  TDialogHandle = class(TInterfacedObject, IDialogHandle)
  private
    FForm: TCommonCustomForm;
    FId: Integer;
  public
    constructor Create(const AForm: TCommonCustomForm; const AId: Integer);
    procedure Close; overload;
    procedure Close(const AResult: TModalResult); overload;
    function IsActive: Boolean;
  end;
```

Implementação:

```pascal
constructor TDialogHandle.Create(const AForm: TCommonCustomForm; const AId: Integer);
begin
  inherited Create;
  FForm := AForm;
  FId := AId;
end;

procedure TDialogHandle.Close;
begin
  Close(mrCancel);
end;

procedure TDialogHandle.Close(const AResult: TModalResult);
begin
  TDialogQueueManager.Instance.CloseByHandle(FForm, FId, AResult);
end;

function TDialogHandle.IsActive: Boolean;
begin
  Result := TDialogQueueManager.Instance.IsHandleActive(FForm, FId);
end;
```

- [ ] **Step 3: DRY em `Base.pas` — extrair `BuildSnapshot` e adicionar `ShowGetHandle`**

Em `src/MultiDialog4FMX.Base.pas`, declarar em `protected`:

```pascal
    function BuildSnapshot(const AForm: TCommonCustomForm;
      out AResolvedForm: TCommonCustomForm): TDialogSnapshot;
```

e em `public` (IDialogBuilder), após os `Show`:

```pascal
    function ShowGetHandle(const AForm: TCommonCustomForm = nil): IDialogHandle;
```

Implementar `BuildSnapshot` (extrai a lógica hoje duplicada dentro de `Show`):

```pascal
function TDialogBase.BuildSnapshot(const AForm: TCommonCustomForm;
  out AResolvedForm: TCommonCustomForm): TDialogSnapshot;
begin
  AResolvedForm := ResolveParentForm(AForm);
  if not Assigned(AResolvedForm) then
    raise Exception.Create('Nenhum formul'#225'rio dispon'#237'vel para exibir o di'#225'logo.');
  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O n'#250'mero m'#237'nimo de bot'#245'es '#233' 1.');
  if FButtonHandlers.Count > 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  Result := TDialogSnapshot.Create(AResolvedForm, FTitle, FMessage, FMsgType,
    FCancelable, FFontSize, FBorderRadius, FAnimation, FTheme, FCustomSVG,
    FCustomIconColor, FResultCallback, FButtonHandlers);
end;
```

Refatorar `Show(AForm)` para usar o helper:

```pascal
function TDialogBase.Show(const AForm: TCommonCustomForm): IDialogBuilder;
var
  LForm: TCommonCustomForm;
  LSnapshot: TDialogSnapshot;
begin
  LSnapshot := BuildSnapshot(AForm, LForm);
  EnqueueSnapshot(LForm, LSnapshot);
  Result := Self;
end;
```

Implementar `ShowGetHandle`:

```pascal
function TDialogBase.ShowGetHandle(const AForm: TCommonCustomForm): IDialogHandle;
var
  LForm: TCommonCustomForm;
  LSnapshot: TDialogSnapshot;
  LId: Integer;
begin
  LSnapshot := BuildSnapshot(AForm, LForm);
  LId := LSnapshot.Id;
  EnqueueSnapshot(LForm, LSnapshot);
  Result := TDialogHandle.Create(LForm, LId);
end;
```

> `TFMXDialog = class(TDialogBase, IDialogBuilder)` em `FMX.pas` herda tudo — `ShowGetHandle` não precisa ser reimplementado lá.

- [ ] **Step 4: Escrever o teste falho** em `Tests.Builder.pas`

Este teste usa a factory fake para não montar UI real. Declarar `[Test] procedure TestShowGetHandle_CloseResolvesMrCancel;` e implementar (adaptando ao estilo do fixture existente — registrar a factory fake que resolve no `CloseWith`):

```pascal
procedure {NomeDoFixtureBuilder}.TestShowGetHandle_CloseResolvesMrCancel;
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
```

> Reutilize o `TFakeDialogInstance` — se o fixture de `Tests.Builder.pas` não o tiver, importe-o de `MultiDialog4FMX.Tests.Queue` no `uses` (ele é `public` naquela unit) ou declare um fake local equivalente com `SnapshotId`/`CloseWith`. Garanta `uses` de `MultiDialog4FMX.Util`, `MultiDialog4FMX.Queue`, `MultiDialog4FMX.Interfaces`, `FMX.Forms`, `System.UITypes`.

- [ ] **Step 5: Rodar — verificar que falha**

Run: build+run. Expected: FAIL/erro de compilação (`ShowGetHandle` recém-adicionado deve compilar; o teste falha na asserção se algo estiver errado — mas ao final deve compilar e a lógica passar após A5+A6).

- [ ] **Step 6: Rodar — verificar que passa**

Run: build+run. Expected: `TestShowGetHandle_CloseResolvesMrCancel` OK; suíte `Failed : 0`.

- [ ] **Step 7: Commit**

```bash
git add src/MultiDialog4FMX.Interfaces.pas src/MultiDialog4FMX.Queue.pas src/MultiDialog4FMX.Base.pas Tests/MultiDialog4FMX.Tests.Builder.pas
git commit -m "feat(sprint7): IDialogHandle + ShowGetHandle (close programatico end-to-end)"
```

---

## Task A7: Fachada de telemetria em `Util.pas`

**Files:**
- Modify: `src/MultiDialog4FMX.Util.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.Builder.pas` (1 teste da fachada)

**Interfaces:**
- Produces: `TMultiDialog4FMX.OnDialogEvent` (class property) que lê/escreve `TDialogTelemetry.OnEvent`.

- [ ] **Step 1: Escrever o teste falho** em `Tests.Builder.pas`

```pascal
procedure {NomeDoFixtureBuilder}.TestFacade_OnDialogEvent_RoutesToTelemetry;
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
```

> `uses` do teste precisa de `MultiDialog4FMX.Telemetry`.

- [ ] **Step 2: Rodar — verificar que falha** (`OnDialogEvent` não existe)

Run: build+run. Expected: `E2003` em `OnDialogEvent`.

- [ ] **Step 3: Implementar a fachada**

Reescrever `src/MultiDialog4FMX.Util.pas`:

```pascal
unit MultiDialog4FMX.Util;

interface

uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,
  MultiDialog4FMX.Factory;

type
  TMultiDialog4FMX = class
  private
    class function GetOnDialogEvent: TDialogEventProc; static;
    class procedure SetOnDialogEvent(const AValue: TDialogEventProc); static;
  public
    class function Dialog: IDialogBuilder; static;
    class property OnDialogEvent: TDialogEventProc
      read GetOnDialogEvent write SetOnDialogEvent;
  end;

implementation

{ TMultiDialog4FMX }

class function TMultiDialog4FMX.Dialog: IDialogBuilder;
begin
  Result := CreateDialog;
end;

class function TMultiDialog4FMX.GetOnDialogEvent: TDialogEventProc;
begin
  Result := TDialogTelemetry.OnEvent;
end;

class procedure TMultiDialog4FMX.SetOnDialogEvent(const AValue: TDialogEventProc);
begin
  TDialogTelemetry.OnEvent := AValue;
end;

end.
```

- [ ] **Step 4: Rodar — verificar que passa**

Run: build+run. Expected: `TestFacade_OnDialogEvent_RoutesToTelemetry` OK.

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.Util.pas Tests/MultiDialog4FMX.Tests.Builder.pas
git commit -m "feat(sprint7): fachada TMultiDialog4FMX.OnDialogEvent"
```

---

## Task A8: Checkpoint Fase A

**Files:** nenhum (validação).

- [ ] **Step 1: Rodar a suíte completa**

Run: build+run (bloco em Global Constraints).
Expected: `Failed : 0`, `Errored : 0`, `Leaked : 0`. Contagem ~97 (87 originais + ~10 novos).

- [ ] **Step 2: Revisar manualmente a superfície pública adicionada**

Confirmar que `IDialogBuilder`/`Show`/`SetOnResult` mantêm assinaturas originais; que só houve **adições** (`ShowGetHandle`, `IDialogHandle`, tipos de telemetria, `OnDialogEvent`).

- [ ] **Step 3: PARADA — checkpoint com o usuário**

Reportar contagem de testes e pedir OK para iniciar a Fase B (await). Não prosseguir sem confirmação.

---

# FASE B — Async/await

## Task B1: Exceção de contrato + fail-fast

**Files:**
- Create: `src/MultiDialog4FMX.Await.pas`
- Modify: `src/MultiDialog4FMX.Interfaces.pas` (`IDialogBuilder.ShowAndWait`)
- Modify: `src/MultiDialog4FMX.Base.pas` (esqueleto de `ShowAndWait` com fail-fast)
- Modify: `Tests/MultiDialog4FMX.Tests.dpr` (registrar `Await` + `Tests.Await`)
- Create: `Tests/MultiDialog4FMX.Tests.Await.pas`

**Interfaces:**
- Produces: `EDialogAwaitOnMainThread = class(Exception)`; `procedure EnsureAwaitNotOnMainThread` (em `Await.pas`); `IDialogBuilder.ShowAndWait(const AForm: TCommonCustomForm = nil): TModalResult`.

- [ ] **Step 1: Criar `MultiDialog4FMX.Await.pas`** (UTF-8 com BOM)

```pascal
unit MultiDialog4FMX.Await;

interface

uses
  System.SysUtils;

type
  /// <summary>Levantada quando ShowAndWait e chamado na main/UI thread (deadlock).</summary>
  EDialogAwaitOnMainThread = class(Exception);

procedure EnsureAwaitNotOnMainThread;

implementation

uses
  System.Classes;

procedure EnsureAwaitNotOnMainThread;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    raise EDialogAwaitOnMainThread.Create(
      'ShowAndWait n'#227'o pode ser chamado na main thread (deadlock). ' +
      'Use dentro de TTask.Run/TThread, ou use Show + SetOnResult na UI thread.');
end;

end.
```

- [ ] **Step 2: Declarar `ShowAndWait` no builder** (`Interfaces.pas`)

Dentro de `IDialogBuilder`, após `ShowGetHandle`:

```pascal
    /// <summary>Bloqueia a worker thread ate o dialogo resolver; retorna o resultado.</summary>
    function ShowAndWait(const AForm: TCommonCustomForm = nil): TModalResult;
```

- [ ] **Step 3: Esqueleto de `ShowAndWait` em `Base.pas` (só fail-fast por ora)**

`uses` da implementation de `Base.pas`: adicionar `MultiDialog4FMX.Await,`.

Declarar em `public` de `TDialogBase`:

```pascal
    function ShowAndWait(const AForm: TCommonCustomForm = nil): TModalResult;
```

Implementar (provisório — só o contrato de thread; corpo completo na Task B2):

```pascal
function TDialogBase.ShowAndWait(const AForm: TCommonCustomForm): TModalResult;
begin
  EnsureAwaitNotOnMainThread;
  Result := mrNone;   // implementacao completa na Task B2
end;
```

- [ ] **Step 4: Criar `Tests/MultiDialog4FMX.Tests.Await.pas`** com o teste de fail-fast

```pascal
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
  System.Classes,
  System.Threading;

type
  [TestFixture]
  TDialogAwaitTests = class
  public
    [Test]
    procedure TestShowAndWait_OnMainThread_Raises;
  end;

implementation

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

initialization
  TDUnitX.RegisterTestFixture(TDialogAwaitTests);

end.
```

- [ ] **Step 5: Registrar no `.dpr`**

Em `Tests/MultiDialog4FMX.Tests.dpr`, no `uses`: após `MultiDialog4FMX.Telemetry in ...` adicionar `MultiDialog4FMX.Await in '..\src\MultiDialog4FMX.Await.pas',`; e ao final da lista de testes adicionar `,\n  MultiDialog4FMX.Tests.Await in 'MultiDialog4FMX.Tests.Await.pas'` antes do `;`.

- [ ] **Step 6: Rodar — verificar que o teste de fail-fast passa**

Run: build+run. Expected: `TestShowAndWait_OnMainThread_Raises` OK.

- [ ] **Step 7: Commit**

```bash
git add src/MultiDialog4FMX.Await.pas src/MultiDialog4FMX.Interfaces.pas src/MultiDialog4FMX.Base.pas Tests/MultiDialog4FMX.Tests.Await.pas Tests/MultiDialog4FMX.Tests.dpr
git commit -m "feat(sprint7): contrato de await + fail-fast na main thread"
```

---

## Task B2: `ShowAndWait` completo (TEvent + marshalling)

**Files:**
- Modify: `src/MultiDialog4FMX.Base.pas` (corpo completo de `ShowAndWait`)
- Modify: `Tests/MultiDialog4FMX.Tests.Await.pas` (testes de resolução em worker thread)

**Interfaces:**
- Consumes: `BuildSnapshot` (Task A6); `EnqueueSnapshot`; `EnsureAwaitNotOnMainThread` (B1).

**Nota de correção:** toda a montagem que toca UI (`ResolveParentForm`, `TDialogSnapshot.Create`, `EnqueueSnapshot`) roda **dentro** do `TThread.Queue` (UI thread). A worker só cria o `TEvent`, agenda e espera. Exceções na montagem são capturadas, sinalizam o evento e re-levantam na worker (senão o `WaitFor` trava).

- [ ] **Step 1: Escrever os testes falhos** em `Tests.Await.pas`

Adicionar ao fixture `[Test] procedure TestShowAndWait_ResolvesOnUIThread_ReturnsResult;` e `[Test] procedure TestShowAndWait_FormDestroyed_ReturnsMrNone;`.

Estratégia: registrar a **factory fake** (`TFakeAwaitInstance`, declarada nesta unit) que, ao receber `Show`, agenda no thread principal (via `TThread.Queue`) um `CloseWith(mrYes)` — simulando o usuário respondendo. A worker chama `ShowAndWait` e deve receber `mrYes`. Um `TStopwatch`/timeout guard aborta se passar de 5s.

```pascal
type
  TFakeAwaitInstance = class(TInterfacedObject, IDialogVisualInstance)
  private
    FSnapshot: TDialogSnapshot;
    FAutoResult: TModalResult;
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    procedure Show;                    // agenda CloseWith(FAutoResult) no main thread
    procedure Suppress;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
  end;

var
  GAwaitAutoResult: TModalResult = mrYes;

constructor TFakeAwaitInstance.Create(const ASnapshot: TDialogSnapshot);
begin
  inherited Create;
  FSnapshot := ASnapshot;
  FAutoResult := GAwaitAutoResult;
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
  LSelf := Self;   // keepalive ate o closure rodar
  TThread.Queue(nil, procedure begin LSelf.CloseWith(GAwaitAutoResult); end);
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
```

Teste principal:

```pascal
procedure TDialogAwaitTests.TestShowAndWait_ResolvesOnUIThread_ReturnsResult;
var
  LForm: TCommonCustomForm;
  LWorkerResult: TModalResult;
  LDone: Boolean;
  LSpin: Integer;
begin
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
  GAwaitAutoResult := mrNone;   // Show agenda CloseWith(mrNone)? Nao — usamos suppress via destruicao.
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
  while (LSpin < 20) and (not TDialogQueueManager.Instance.DebugIsActive(LForm)) do
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

  Assert.IsTrue(LDone, 'A worker deve desbloquear quando o form e destruido');
  Assert.AreEqual(mrNone, LWorkerResult, 'Form destruido resolve o await com mrNone');
end;
```

> Ajuste fino: no primeiro teste `GAwaitAutoResult := mrYes` faz o fake responder sozinho. No segundo, como o fake também responderia, defina `GAwaitAutoResult := mrNone` e confie na destruição — ou introduza um modo "não responder" no fake (flag `FAutoRespond`). Se preferir clareza, adicione `class var GAutoRespond: Boolean` ao fake e no `Show` só agende o `CloseWith` quando `GAutoRespond`. Use `GAutoRespond := False` no segundo teste.

- [ ] **Step 2: Rodar — verificar que falha** (`ShowAndWait` retorna `mrNone` fixo hoje)

Run: build+run. Expected: FAIL em `TestShowAndWait_ResolvesOnUIThread_ReturnsResult` (retorna mrNone, esperado mrYes).

- [ ] **Step 3: Implementar o corpo completo de `ShowAndWait` em `Base.pas`**

`uses` da implementation de `Base.pas`: adicionar `System.SyncObjs,` e `System.Classes` (já presente) — garantir `TEvent`/`TThread`.

```pascal
function TDialogBase.ShowAndWait(const AForm: TCommonCustomForm): TModalResult;
var
  LEvent: TEvent;
  LResult: TModalResult;
  LUserCb: TDialogResultProc;
  LError: TObject;
begin
  EnsureAwaitNotOnMainThread;

  LResult := mrNone;
  LError := nil;
  LUserCb := FResultCallback;   // encadeia o callback do usuario, se houver
  LEvent := TEvent.Create(nil, True, False, '');   // manual reset, nao sinalizado
  try
    TThread.Queue(nil,
      procedure
      var
        LForm: TCommonCustomForm;
        LSnapshot: TDialogSnapshot;
      begin
        try
          LForm := ResolveParentForm(AForm);
          if not Assigned(LForm) then
            raise Exception.Create('Nenhum formul'#225'rio dispon'#237'vel para exibir o di'#225'logo.');
          if FButtonHandlers.Count < 1 then
            raise Exception.Create('O n'#250'mero m'#237'nimo de bot'#245'es '#233' 1.');
          if FButtonHandlers.Count > 4 then
            raise Exception.Create(C_MaxButtonsMsg);

          LSnapshot := TDialogSnapshot.Create(LForm, FTitle, FMessage, FMsgType,
            FCancelable, FFontSize, FBorderRadius, FAnimation, FTheme, FCustomSVG,
            FCustomIconColor,
            procedure(const R: TModalResult)
            begin
              if Assigned(LUserCb) then
                LUserCb(R);
              LResult := R;
              LEvent.SetEvent;
            end,
            FButtonHandlers);

          EnqueueSnapshot(LForm, LSnapshot);
        except
          on E: Exception do
          begin
            LError := Exception(AcquireExceptionObject);
            LEvent.SetEvent;   // desbloqueia a worker para re-levantar
          end;
        end;
      end);

    LEvent.WaitFor(INFINITE);

    if LError <> nil then
      raise Exception(LError);   // re-levanta na worker (ownership transferido)

    Result := LResult;
  finally
    LEvent.Free;
  end;
end;
```

> `AcquireExceptionObject`/re-raise transfere a exceção da UI thread para a worker sem vazamento; o `raise Exception(LError)` assume a posse e o RTL a libera ao tratar.

- [ ] **Step 4: Rodar — verificar que os testes de await passam**

Run: build+run. Expected: `TestShowAndWait_ResolvesOnUIThread_ReturnsResult` e `TestShowAndWait_FormDestroyed_ReturnsMrNone` OK; suíte `Failed : 0`, `Errored : 0`, e — crucialmente — o processo **não trava** (o timeout guard garante saída).

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.Base.pas Tests/MultiDialog4FMX.Tests.Await.pas
git commit -m "feat(sprint7): ShowAndWait completo (TEvent + marshalling na UI thread)"
```

---

## Task B3: Checkpoint Fase B + sample de demonstração (opcional)

**Files:**
- (opcional) Modify: `Samples/Delphi12.3/Windows/...UMain.pas` — botões de demo close/await/telemetria.

- [ ] **Step 1: Rodar a suíte completa**

Run: build+run.
Expected: `Failed : 0`, `Errored : 0`, `Leaked : 0`. Total ~100 testes.

- [ ] **Step 2: (Opcional) Sample Windows**

Se o usuário quiser demo visual, adicionar ao Sample três botões: (a) abre diálogo e guarda `IDialogHandle`, fecha por `TThread.ForceQueue` após 2s; (b) `ShowAndWait` dentro de `TTask.Run` mostrando o resultado num `ShowMessage`; (c) registra `TMultiDialog4FMX.OnDialogEvent` logando no `Memo`. Validar manualmente (o sample não entra na contagem de testes).

- [ ] **Step 3: PARADA — checkpoint final com o usuário**

Reportar contagem final e validação. Decidir com o usuário: push da branch + PR para `develop` (workflow feature→develop→master), e se atualiza o handoff.

---

## Self-Review (preenchido pelo autor do plano)

**Spec coverage:**
- Resolução única → Tasks A3 (idempotência) + A4 (suppress mrNone). ✓
- Telemetria (7 eventos, callback global, UI thread, ElapsedMs) → A1 (tipos/dispatcher) + A2 (ElapsedMs) + A4 (emissão) + A7 (fachada). ✓
- Close programático (IDialogHandle, token, 3 estados) → A5 (CloseByHandle/CloseWith) + A6 (handle + ShowGetHandle). ✓
- Await (ShowAndWait, fail-fast, TEvent, marshalling UI, form-destroyed→mrNone) → B1 (fail-fast) + B2 (corpo). ✓
- API pública inalterada → A6 preserva `Show`; A8/B3 revisam. ✓
- Testes por feature + guard de timeout no await → presentes. ✓

**Type consistency:** `TDialogEventInfo`/`TDialogEventKind`/`TDialogEventProc` idênticos em Interfaces/Telemetry/testes; `IDialogVisualInstance` (com `SnapshotId`/`CloseWith`) atualizada em Queue + FMX + fake; `CloseByHandle(Form,Id,Result)`/`IsHandleActive(Form,Id)` consistentes entre A5 (def) e A6 (uso); `BuildSnapshot(AForm, out AResolvedForm)` usado por `Show`/`ShowGetHandle`/(inline em)`ShowAndWait`.

**Placeholder scan:** os `{NomeDoFixture}` em Tasks A2/A6/A7 são marcadores explícitos "leia o nome real do fixture no arquivo" — únicos pontos onde o nome depende do arquivo existente; instruído inline. Sem TBD/TODO de lógica.
