# Sprint 6 Fundacional (Snapshot + Fila FIFO + Segurança no form) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `TDialogBase.Show` a per-form FIFO queue backed by an immutable config snapshot, and make form destruction with an open/queued dialog safe, without changing `IDialogBuilder`/`IDialogButtonsBuilder` or any sample code.

**Architecture:** Split the current monolithic `TFMXDialog` (config + live visual state in one object) into `TDialogSnapshot` (immutable copy of config + deep-copied buttons, new unit `MultiDialog4FMX.Queue.pas`), `TFMXDialogInstance` (pure visual/lifecycle object built from a snapshot, in `MultiDialog4FMX.FMX.pas`), and `TDialogQueueManager` (singleton FIFO per form + form-destruction guard via a `FAlive` flag checked in every deferred closure, new unit `MultiDialog4FMX.Queue.pas`). `TDialogBase.Show` now builds a snapshot and hands it to the queue manager instead of calling `InternalShow` directly.

**Tech Stack:** Delphi (Object Pascal), FireMonkey (FMX), DUnitX. Build via `Tests/build_tests.bat` (msbuild) or the `delphi-build` agent.

## Global Constraints

- `IDialogBuilder`/`IDialogButtonsBuilder` (`src/MultiDialog4FMX.Interfaces.pas`) do not change — no new methods, no signature changes.
- No sample (`Samples/Delphi10.3.3/...`, `Samples/Delphi12.3/...`) needs to change to keep compiling.
- `Show`/`Show(AForm)` remain non-blocking — they return immediately, exactly as today.
- Two `Show()` calls on the same form no longer show two overlapping dialogs — the second one waits in a FIFO queue until the first closes (decided during brainstorm, see spec).
- Never commit directly to `master` or `develop` — feature branch → PR to `develop` → PR to `master` → release tag. This plan's branch is `feature/sprint6-snapshot-queue`, created from `develop`.
- All 52 existing DUnitX tests (`Tests/test_final.txt`) must still pass at the end, plus every new test this plan adds.
- Nothing from Sprint 7 (close programático, async/await, telemetria), Sprint 8 (botões semânticos, i18n, adapter), or Sprint 9 (iOS/Windows real) belongs in this plan.
- Spec of record: `docs/superpowers/specs/2026-07-17-sprint6-fundacional-design.md`.

---

### Task 1: Branch setup

**Files:** none (git only)

- [ ] **Step 1: Create the feature branch from `develop`**

```bash
git fetch origin
git checkout -b feature/sprint6-snapshot-queue origin/develop
```

- [ ] **Step 2: Confirm the baseline is green before touching anything**

Run (delegate to the `delphi-build` agent, or run directly if `rsvars.bat`/`dcc32` are set up):
```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK` and, in `Tests/Win32/Debug/test-output.txt` (or console output), `Tests Passed : 52`, `Tests Failed : 0`, `Tests Errored : 0`. If this doesn't hold, stop and fix the baseline first — this plan assumes a green starting point.

---

### Task 2: Move `TButtonHandler`/`TButtonHandlerList` into `Interfaces.pas` (unblocks the new unit's dependency graph)

**Why this task exists:** `TDialogSnapshot` (Task 3) needs `TButtonHandler`/`TButtonHandlerList` to hold its deep-copied button list, and lives in a new unit `MultiDialog4FMX.Queue.pas` that `MultiDialog4FMX.Base.pas` will depend on (for `TDialogQueueManager.Instance.Enqueue`). If `TButtonHandler` stays in `Base.pas`, `Queue.pas` would need to `uses MultiDialog4FMX.Base`, and `Base.pas` would need to `uses MultiDialog4FMX.Queue` — a circular unit reference (Delphi error). Moving `TButtonHandler`/`TButtonHandlerList`/`C_MaxButtonsMsg` to the leaf unit `Interfaces.pas` (which nothing in this codebase depends *on* circularly) breaks the cycle: `Interfaces.pas` ← `Queue.pas` ← `Base.pas` ← `FMX.pas`, a clean one-directional chain.

**Files:**
- Modify: `src/MultiDialog4FMX.Interfaces.pas`
- Modify: `src/MultiDialog4FMX.Base.pas`

**Interfaces:**
- Produces: `TButtonHandler`, `TButtonHandlerList`, `C_MaxButtonsMsg` — now declared in `MultiDialog4FMX.Interfaces`, importable by any unit that already `uses MultiDialog4FMX.Interfaces` (every test file in this project already does).

- [ ] **Step 1: Add `FMX.Layouts` and `System.Generics.Collections` to `Interfaces.pas`'s uses clause**

In `src/MultiDialog4FMX.Interfaces.pas`, change the `uses` clause (lines 5-11) to:
```pascal
uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,

  FMX.Types,
  FMX.Forms,
  FMX.Layouts;
```

- [ ] **Step 2: Move `C_MaxButtonsMsg`, `TButtonHandler`, `TButtonHandlerList` from `Base.pas` to `Interfaces.pas`**

In `src/MultiDialog4FMX.Interfaces.pas`, right after the `uses` clause and before `type`, add:
```pascal
const
  C_MaxButtonsMsg = 'O di'#225'logo suporta no m'#225'ximo 4 bot'#245'es.';
```

Then, inside the existing `type` section, add (place it before `TMultiDialogType` or anywhere in the `type` block — order among types in the same `type` section doesn't matter in Pascal as long as forward declarations aren't needed, and none are here):
```pascal
  // Guarda texto + handler click ou tap
  TButtonHandler = class
  private
    class var FInstanceCount: Integer;
  private
    FText: string;
    FClickHandler: TNotifyEvent;
    FTapHandler: TTapEvent;
    FAnonymousHandler: TProc;
    FColor: TAlphaColor;
    FStyleLookup: string;
    FOverlay: TLayout;
    FModalResult: TModalResult;
    FTimeout: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    class property InstanceCount: Integer read FInstanceCount;
    property Text: string read FText write FText;
    property ClickHandler: TNotifyEvent read FClickHandler write FClickHandler;
    property TapHandler: TTapEvent read FTapHandler write FTapHandler;
    property AnonymousHandler: TProc read FAnonymousHandler write FAnonymousHandler;
    property Color: TAlphaColor read FColor write FColor;
    property StyleLookup: string read FStyleLookup write FStyleLookup;
    property Overlay: TLayout read FOverlay write FOverlay;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property Timeout: Integer read FTimeout write FTimeout;
  end;
  TButtonHandlerList = TObjectList<TButtonHandler>;
```

In the `implementation` section of `Interfaces.pas` (create one if there isn't a suitable spot — currently `implementation` is empty right before `end.`), add:
```pascal
{ TButtonHandler }

constructor TButtonHandler.Create;
begin
  inherited;
  Inc(TButtonHandler.FInstanceCount);
  FModalResult := mrOk;
end;

destructor TButtonHandler.Destroy;
begin
  Dec(TButtonHandler.FInstanceCount);
  inherited;
end;
```

Now delete from `src/MultiDialog4FMX.Base.pas`: the `const C_MaxButtonsMsg = ...` line (line 18), the whole `TButtonHandler = class ... end;` block and the `TButtonHandlerList = TObjectList<TButtonHandler>;` line (lines 22-50), and the `{ TButtonHandler }` implementation block (`constructor TButtonHandler.Create` / `destructor TButtonHandler.Destroy`, lines 107-120).

`Base.pas` already has `uses MultiDialog4FMX.Interfaces` (line 6), so no uses-clause change is needed there. `Base.pas` also already `uses System.Generics.Collections` (for `TButtonHandlerList` usage elsewhere) — leave that, it's harmless even though the type itself moved.

- [ ] **Step 3: Build and run the full suite — this step must be a pure no-op for behavior**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`, `Tests Passed : 52`, `Tests Failed : 0`, `Tests Errored : 0` — identical to Task 1's baseline. No test file needed changes because every test unit that touches `TButtonHandler` already imports `MultiDialog4FMX.Interfaces` directly (verified: `Tests.Mocks.pas`, `Tests.Buttons.pas`, `Tests.MemoryLeaks.pas`, `Tests.Android.pas` all already have it in their `uses`).

- [ ] **Step 4: Commit**

```bash
git add src/MultiDialog4FMX.Interfaces.pas src/MultiDialog4FMX.Base.pas
git commit -m "refactor: move TButtonHandler/TButtonHandlerList to Interfaces.pas

Breaks the circular dependency that MultiDialog4FMX.Queue.pas would
otherwise create between Base.pas and Queue.pas (Sprint 6 fundacional)."
```

---

### Task 3: `TDialogSnapshot` (new unit `MultiDialog4FMX.Queue.pas`, part 1)

**Files:**
- Create: `src/MultiDialog4FMX.Queue.pas`
- Create: `Tests/MultiDialog4FMX.Tests.Snapshot.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.dpr` (register the two new units)

**Interfaces:**
- Consumes: `TButtonHandler`, `TButtonHandlerList`, `TMultiDialogType`, `TDialogAnimation`, `TDialogTheme`, `TDialogResultProc` (all from `MultiDialog4FMX.Interfaces`, already in place after Task 2).
- Produces: `TDialogSnapshot` with public properties `Form`, `Title`, `Message`, `MsgType`, `Cancelable`, `FontSize`, `BorderRadius`, `Animation`, `Theme`, `CustomSVG`, `CustomIconColor`, `ResultCallback`, `Buttons: TButtonHandlerList`, and constructor `Create(const AForm: TCommonCustomForm; const ATitle, AMessage: string; const AMsgType: TMultiDialogType; const ACancelable: Boolean; const AFontSize, ABorderRadius: Single; const AAnimation: TDialogAnimation; const ATheme: TDialogTheme; const ACustomSVG: string; const ACustomIconColor: TAlphaColor; const AResultCallback: TDialogResultProc; const AButtons: TButtonHandlerList)`. Task 4 (`TDialogQueueManager`) and Task 5 (`TDialogBase.Show`) consume this exact constructor signature.

- [ ] **Step 1: Write the failing tests**

Create `Tests/MultiDialog4FMX.Tests.Snapshot.pas`:
```pascal
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

initialization
  TDUnitX.RegisterTestFixture(TDialogSnapshotTests);

end.
```

- [ ] **Step 2: Add both new units to `Tests/MultiDialog4FMX.Tests.dpr` and run — verify it fails to compile (unit doesn't exist yet)**

In `Tests/MultiDialog4FMX.Tests.dpr`, add to the `uses` clause (after `MultiDialog4FMX.Base in '..\src\MultiDialog4FMX.Base.pas',`):
```pascal
  MultiDialog4FMX.Queue in '..\src\MultiDialog4FMX.Queue.pas',
```
and after `MultiDialog4FMX.Tests.MemoryLeaks in 'MultiDialog4FMX.Tests.MemoryLeaks.pas';` change the trailing `;` to `,` and add:
```pascal
  MultiDialog4FMX.Tests.Snapshot in 'MultiDialog4FMX.Tests.Snapshot.pas';
```

Run:
```bash
Tests/build_tests.bat
```
Expected: `COMPILE_FAILED` — `File not found: 'MultiDialog4FMX.Queue.pas'` (or similar). This confirms the test file is wired in before the implementation exists.

- [ ] **Step 3: Create `src/MultiDialog4FMX.Queue.pas` with `TDialogSnapshot`**

```pascal
unit MultiDialog4FMX.Queue;

interface

uses
  MultiDialog4FMX.Interfaces,

  FMX.Forms,

  System.Classes,
  System.SysUtils,
  System.UITypes;

type
  TDialogSnapshot = class
  private
    FForm: TCommonCustomForm;
    FTitle: string;
    FMessage: string;
    FMsgType: TMultiDialogType;
    FCancelable: Boolean;
    FFontSize: Single;
    FBorderRadius: Single;
    FAnimation: TDialogAnimation;
    FTheme: TDialogTheme;
    FCustomSVG: string;
    FCustomIconColor: TAlphaColor;
    FResultCallback: TDialogResultProc;
    FButtons: TButtonHandlerList;
  public
    constructor Create(const AForm: TCommonCustomForm; const ATitle, AMessage: string;
      const AMsgType: TMultiDialogType; const ACancelable: Boolean;
      const AFontSize, ABorderRadius: Single; const AAnimation: TDialogAnimation;
      const ATheme: TDialogTheme; const ACustomSVG: string;
      const ACustomIconColor: TAlphaColor; const AResultCallback: TDialogResultProc;
      const AButtons: TButtonHandlerList);
    destructor Destroy; override;

    property Form: TCommonCustomForm read FForm;
    property Title: string read FTitle;
    property Message: string read FMessage;
    property MsgType: TMultiDialogType read FMsgType;
    property Cancelable: Boolean read FCancelable;
    property FontSize: Single read FFontSize;
    property BorderRadius: Single read FBorderRadius;
    property Animation: TDialogAnimation read FAnimation;
    property Theme: TDialogTheme read FTheme;
    property CustomSVG: string read FCustomSVG;
    property CustomIconColor: TAlphaColor read FCustomIconColor;
    property ResultCallback: TDialogResultProc read FResultCallback;
    property Buttons: TButtonHandlerList read FButtons;
  end;

implementation

{ TDialogSnapshot }

constructor TDialogSnapshot.Create(const AForm: TCommonCustomForm;
  const ATitle, AMessage: string; const AMsgType: TMultiDialogType;
  const ACancelable: Boolean; const AFontSize, ABorderRadius: Single;
  const AAnimation: TDialogAnimation; const ATheme: TDialogTheme;
  const ACustomSVG: string; const ACustomIconColor: TAlphaColor;
  const AResultCallback: TDialogResultProc; const AButtons: TButtonHandlerList);
var
  I: Integer;
  LSource: TButtonHandler;
  LCopy: TButtonHandler;
begin
  inherited Create;
  FForm            := AForm;
  FTitle           := ATitle;
  FMessage         := AMessage;
  FMsgType         := AMsgType;
  FCancelable      := ACancelable;
  FFontSize        := AFontSize;
  FBorderRadius    := ABorderRadius;
  FAnimation       := AAnimation;
  FTheme           := ATheme;
  FCustomSVG       := ACustomSVG;
  FCustomIconColor := ACustomIconColor;
  FResultCallback  := AResultCallback;

  FButtons := TButtonHandlerList.Create(True);
  for I := 0 to AButtons.Count - 1 do
  begin
    LSource := AButtons[I];
    LCopy := TButtonHandler.Create;
    LCopy.Text             := LSource.Text;
    LCopy.ClickHandler     := LSource.ClickHandler;
    LCopy.TapHandler       := LSource.TapHandler;
    LCopy.AnonymousHandler := LSource.AnonymousHandler;
    LCopy.Color            := LSource.Color;
    LCopy.StyleLookup      := LSource.StyleLookup;
    LCopy.ModalResult      := LSource.ModalResult;
    LCopy.Timeout          := LSource.Timeout;
    // Overlay fica nil — e estado visual, comeca vazio em toda copia.
    FButtons.Add(LCopy);
  end;
end;

destructor TDialogSnapshot.Destroy;
begin
  FButtons.Free;
  inherited;
end;

end.
```

- [ ] **Step 4: Run the tests, verify they pass**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`, and in the run output `TDialogSnapshotTests` shows 3 `Success.` entries, overall `Tests Passed : 55` (52 baseline + 3 new), `Tests Failed : 0`, `Tests Errored : 0`.

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.Queue.pas Tests/MultiDialog4FMX.Tests.Snapshot.pas Tests/MultiDialog4FMX.Tests.dpr
git commit -m "feat(sprint6): add TDialogSnapshot — immutable config copy with deep-copied buttons"
```

---

### Task 4: `TDialogQueueManager` (`MultiDialog4FMX.Queue.pas`, part 2)

**Files:**
- Modify: `src/MultiDialog4FMX.Queue.pas`
- Create: `Tests/MultiDialog4FMX.Tests.Queue.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.dpr`

**Interfaces:**
- Consumes: `TDialogSnapshot` (Task 3).
- Produces:
  - `IDialogVisualInstance` — interface with `procedure Show;` and `procedure Suppress;`. Task 6 (`TFMXDialogInstance`) implements this.
  - `TDialogInstanceFactory = reference to function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance;`
  - `TDialogQueueManager` — singleton, `class function Instance: TDialogQueueManager`, `class procedure RegisterInstanceFactory(const AFactory: TDialogInstanceFactory)`, `procedure Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot)`, `procedure NotifyClosed(const AForm: TCommonCustomForm)`. Task 5 (`TDialogBase.EnqueueSnapshot`) calls `Enqueue`; Task 6 (`TFMXDialogInstance.CloseDialog`) calls `NotifyClosed`, and `FMX.pas`'s unit `initialization` calls `RegisterInstanceFactory`.

- [ ] **Step 1: Write the failing tests**

Create `Tests/MultiDialog4FMX.Tests.Queue.pas`. It uses a lightweight fake `IDialogVisualInstance` (not the real `TFMXDialogInstance`, which doesn't exist until Task 6) so this fixture is self-contained and doesn't depend on Task 6's ordering:

```pascal
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
      FId: Integer;
      FSnapshot: TDialogSnapshot;
      FShowCallCount: Integer;
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    procedure Show;
    procedure Suppress;
    property Id: Integer read FId;
    property ShowCallCount: Integer read FShowCallCount;
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
    LFirstId := GLastCreatedInstance.Id; // capturado ANTES do NotifyClosed — a 1a instancia
                                          // e liberada dentro dele (FActive.Remove derruba o
                                          // refcount da interface a zero); guardar so o ponteiro
                                          // e comparar identidade depois seria UAF (e o alocador
                                          // do Delphi pode reciclar o endereco pra 2a instancia).
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
```

- [ ] **Step 2: Add the new test unit to `Tests/MultiDialog4FMX.Tests.dpr`**

Add `MultiDialog4FMX.Tests.Queue in 'MultiDialog4FMX.Tests.Queue.pas',` to the `uses` clause (same pattern as Task 3 Step 2).

Run `Tests/build_tests.bat`. Expected: `COMPILE_FAILED` — `IDialogVisualInstance`/`TDialogQueueManager` not found (they don't exist in `Queue.pas` yet).

- [ ] **Step 3: Add `IDialogVisualInstance`, `TDialogInstanceFactory`, and `TDialogQueueManager` to `src/MultiDialog4FMX.Queue.pas`**

Add `FMX.Types` (for `TCommonCustomForm`'s owning unit is `FMX.Forms`, already present; need `System.Generics.Collections` too) to the `uses` clause, and append the following to the `interface` section (after `TDialogSnapshot`'s declaration) and to `implementation`:

```pascal
// --- add to the uses clause of MultiDialog4FMX.Queue.pas ---
uses
  MultiDialog4FMX.Interfaces,

  FMX.Forms,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;
```

```pascal
// --- add to the type section, after TDialogSnapshot ---
  IDialogVisualInstance = interface
    ['{7F1B0A11-4E9A-4C4B-9C4B-2B4E7A0E1F10}']
    procedure Show;
    procedure Suppress;
  end;

  TDialogInstanceFactory = reference to function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance;

  TDialogQueueManager = class(TComponent)
  private
    class var FInstance: TDialogQueueManager;
    class var FFactory: TDialogInstanceFactory;
    FQueues: TObjectDictionary<TCommonCustomForm, TQueue<TDialogSnapshot>>;
    FActive: TDictionary<TCommonCustomForm, IDialogVisualInstance>;
    FWatched: TList<TCommonCustomForm>;
    procedure ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
    procedure EnsureWatched(const AForm: TCommonCustomForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Instance: TDialogQueueManager;
    class procedure RegisterInstanceFactory(const AFactory: TDialogInstanceFactory);

    procedure Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
    procedure NotifyClosed(const AForm: TCommonCustomForm);

    // Test-support accessors (protected: reachable from a same-behavior subclass in tests,
    // not part of the public API surface).
    function DebugIsActive(const AForm: TCommonCustomForm): Boolean;
    function DebugQueueLength(const AForm: TCommonCustomForm): Integer;
  end;
```

```pascal
// --- add to implementation ---
{ TDialogQueueManager }

constructor TDialogQueueManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueues := TObjectDictionary<TCommonCustomForm, TQueue<TDialogSnapshot>>.Create([doOwnsValues]);
  FActive := TDictionary<TCommonCustomForm, IDialogVisualInstance>.Create;
  FWatched := TList<TCommonCustomForm>.Create;
end;

destructor TDialogQueueManager.Destroy;
begin
  FWatched.Free;
  FActive.Free;
  FQueues.Free;
  inherited;
end;

class function TDialogQueueManager.Instance: TDialogQueueManager;
begin
  if not Assigned(FInstance) then
    FInstance := TDialogQueueManager.Create(nil);
  Result := FInstance;
end;

class procedure TDialogQueueManager.RegisterInstanceFactory(const AFactory: TDialogInstanceFactory);
begin
  FFactory := AFactory;
end;

procedure TDialogQueueManager.EnsureWatched(const AForm: TCommonCustomForm);
begin
  if FWatched.IndexOf(AForm) < 0 then
  begin
    AForm.FreeNotification(Self);
    FWatched.Add(AForm);
  end;
end;

procedure TDialogQueueManager.ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
var
  LInstance: IDialogVisualInstance;
begin
  Assert.IsTrue(Assigned(FFactory), 'TDialogQueueManager.RegisterInstanceFactory nunca foi chamado');
  LInstance := FFactory(ASnapshot);
  FActive.AddOrSetValue(AForm, LInstance);
  LInstance.Show;
end;

procedure TDialogQueueManager.Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
var
  LQueue: TQueue<TDialogSnapshot>;
begin
  EnsureWatched(AForm);

  if FActive.ContainsKey(AForm) then
  begin
    if not FQueues.TryGetValue(AForm, LQueue) then
    begin
      LQueue := TQueue<TDialogSnapshot>.Create;
      FQueues.Add(AForm, LQueue);
    end;
    LQueue.Enqueue(ASnapshot);
  end
  else
    ShowNow(AForm, ASnapshot);
end;

procedure TDialogQueueManager.NotifyClosed(const AForm: TCommonCustomForm);
var
  LQueue: TQueue<TDialogSnapshot>;
  LNext: TDialogSnapshot;
begin
  FActive.Remove(AForm);

  if FQueues.TryGetValue(AForm, LQueue) and (LQueue.Count > 0) then
  begin
    LNext := LQueue.Dequeue;
    ShowNow(AForm, LNext);
  end;
end;

procedure TDialogQueueManager.Notification(AComponent: TComponent; Operation: TOperation);
var
  LForm: TCommonCustomForm;
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
begin
  inherited;
  if Operation <> TOperation.opRemove then
    Exit;
  if not (AComponent is TCommonCustomForm) then
    Exit;

  LForm := TCommonCustomForm(AComponent);

  if FActive.TryGetValue(LForm, LInstance) then
  begin
    LInstance.Suppress;
    FActive.Remove(LForm);
  end;

  if FQueues.TryGetValue(LForm, LQueue) then
  begin
    LQueue.Clear; // TObjectDictionary com doOwnsValues nao libera os TDialogSnapshot dentro da TQueue —
                   // ver Passo 4 abaixo: a fila e uma TObjectList-like que precisa liberar os itens.
    FQueues.Remove(LForm);
  end;

  FWatched.Remove(LForm);
end;

function TDialogQueueManager.DebugIsActive(const AForm: TCommonCustomForm): Boolean;
begin
  Result := FActive.ContainsKey(AForm);
end;

function TDialogQueueManager.DebugQueueLength(const AForm: TCommonCustomForm): Integer;
var
  LQueue: TQueue<TDialogSnapshot>;
begin
  if FQueues.TryGetValue(AForm, LQueue) then
    Result := LQueue.Count
  else
    Result := 0;
end;
```

**Correção importante antes de compilar:** `TQueue<TDialogSnapshot>` (da `System.Generics.Collections`) **não libera os itens** ao ser limpo ou destruído — só `TObjectList`/`TObjectDictionary` com `doOwnsValues` fazem isso, e um `TQueue<T>` genérico não tem essa opção. Isso significa que:
- No caminho normal (`NotifyClosed` → `LQueue.Dequeue` → `ShowNow`), o snapshot tirado da fila passa a ser dono da `TFMXDialogInstance` criada (ela o libera no seu próprio `Destroy` — ver Task 6) — sem leak.
- No caminho de purga (`Notification`, acima), `LQueue.Clear` **descartaria os snapshots sem liberar memória** — leak dos `TDialogSnapshot` (e por consequência dos `TButtonHandler` copiados dentro deles). Antes de `LQueue.Clear`, itere e libere manualmente:

Replace the purge block above with:
```pascal
  if FQueues.TryGetValue(LForm, LQueue) then
  begin
    while LQueue.Count > 0 do
      LQueue.Dequeue.Free; // libera cada TDialogSnapshot pendente — nunca vai aparecer, sem callback
    FQueues.Remove(LForm);
  end;
```

(`FQueues.Remove(LForm)` still frees the now-empty `TQueue<TDialogSnapshot>` object itself, since `FQueues` is a `TObjectDictionary` with `doOwnsValues` — that part was already correct.)

Also fix the `Assert.IsTrue` call inside `ShowNow` — `Assert` there is DUnitX's `Assert`, which isn't linked/available outside test code and isn't the right tool for a production guard anyway. Replace it with a real guard:
```pascal
procedure TDialogQueueManager.ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
var
  LInstance: IDialogVisualInstance;
begin
  if not Assigned(FFactory) then
    raise Exception.Create('TDialogQueueManager.RegisterInstanceFactory nunca foi chamado — ' +
      'MultiDialog4FMX.FMX deveria ter registrado a factory na sua secao initialization.');
  LInstance := FFactory(ASnapshot);
  FActive.AddOrSetValue(AForm, LInstance);
  LInstance.Show;
end;
```

- [ ] **Step 4: Run the tests, verify they pass**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`, `TDialogQueueManagerTests` shows 4 `Success.`, overall `Tests Passed : 59` (55 from Task 3 + 4 new), `Tests Failed : 0`, `Tests Errored : 0`.

- [ ] **Step 5: Commit**

```bash
git add src/MultiDialog4FMX.Queue.pas Tests/MultiDialog4FMX.Tests.Queue.pas Tests/MultiDialog4FMX.Tests.dpr
git commit -m "feat(sprint6): add TDialogQueueManager — per-form FIFO + form-destruction guard"
```

---

### Task 5: Rewire `TDialogBase.Show` (`Base.pas`) + adjust `Tests.Mocks.pas`/`Tests.Builder.pas`

**Files:**
- Modify: `src/MultiDialog4FMX.Base.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.Mocks.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.Builder.pas`

**Interfaces:**
- Consumes: `TDialogSnapshot.Create(...)` (Task 3), `TDialogQueueManager.Instance.Enqueue(...)` (Task 4).
- Produces: `TDialogBase.EnqueueSnapshot(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot); virtual;` — Task 6's `TFMXDialog` does **not** need to override this (the default implementation is exactly what production code needs); only `TMockDialogBase` (tests) overrides it.

- [ ] **Step 1: Update the failing test first — rename and repurpose `TestShow_CallsInternalShow`**

In `Tests/MultiDialog4FMX.Tests.Builder.pas`, rename the test declaration (line 47) from:
```pascal
    [Test]
    procedure TestShow_CallsInternalShow;
```
to:
```pascal
    [Test]
    procedure TestShow_EnqueuesSnapshot;
```

And replace its implementation (lines 170-175):
```pascal
procedure TDialogBuilderTests.TestShow_CallsInternalShow;
begin
  FDialog.Reset;
  FDialog.Buttons.AddButton('OK').&End.Show;
  Assert.IsTrue(FDialog.ShowCalled);
end;
```
with:
```pascal
procedure TDialogBuilderTests.TestShow_EnqueuesSnapshot;
begin
  FDialog.Reset;
  FDialog.Buttons.AddButton('OK').&End.Show;
  Assert.IsTrue(FDialog.ShowCalled);
end;
```
(Assertion body is unchanged on purpose — `FDialog.ShowCalled`/`FDialog.Reset` stay as `TMockDialogBase` properties; only their *meaning* changes, from "was `InternalShow` called" to "was `EnqueueSnapshot` called". Step 2 updates the mock to match.)

- [ ] **Step 2: Update `TMockDialogBase` (`Tests/MultiDialog4FMX.Tests.Mocks.pas`) to override `EnqueueSnapshot` instead of `InternalShow`**

Replace (lines 19-80 of `Tests/MultiDialog4FMX.Tests.Mocks.pas`):
```pascal
  TMockDialogBase = class(TDialogBase)
  private
    FShowCalled: Boolean;
    FLastParentForm: TCommonCustomForm;
  protected
    procedure InternalShow(const AForm: TCommonCustomForm); override;
  public
```
with:
```pascal
  TMockDialogBase = class(TDialogBase)
  private
    FShowCalled: Boolean;
    FLastParentForm: TCommonCustomForm;
  protected
    procedure EnqueueSnapshot(const AForm: TCommonCustomForm;
      const ASnapshot: TDialogSnapshot); override;
  public
```

And replace the implementation:
```pascal
procedure TMockDialogBase.InternalShow(const AForm: TCommonCustomForm);
begin
  FShowCalled := True;
  FLastParentForm := AForm;
  // Don't create any UI components
end;
```
with:
```pascal
procedure TMockDialogBase.EnqueueSnapshot(const AForm: TCommonCustomForm;
  const ASnapshot: TDialogSnapshot);
begin
  FShowCalled := True;
  FLastParentForm := AForm;
  ASnapshot.Free; // nao enfileira de verdade — evita tocar no TDialogQueueManager real
end;
```

Add `MultiDialog4FMX.Queue` to the `uses` clause of `Tests/MultiDialog4FMX.Tests.Mocks.pas` (needed for the `TDialogSnapshot` type in the new method signature):
```pascal
uses
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Queue,
  FMX.Forms,
  FMX.Types,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;
```

- [ ] **Step 3: Run the tests, verify they now fail to compile (`EnqueueSnapshot` doesn't exist on `TDialogBase` yet)**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_FAILED` — `E2029`/`E2003`-style error that `TMockDialogBase.EnqueueSnapshot` has no matching virtual method to override in `TDialogBase` (or similar "method not found in base class").

- [ ] **Step 4: Rewrite `TDialogBase.Show`/`Show(AForm)` in `src/MultiDialog4FMX.Base.pas`**

Add `MultiDialog4FMX.Queue` to the `uses` clause of `Base.pas`:
```pascal
uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Queue,

  FMX.Types,
  FMX.Forms,
  FMX.Layouts,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;
```

Replace the `protected` section of `TDialogBase` (the line `procedure InternalShow(const AForm: TCommonCustomForm); virtual; abstract;`) with:
```pascal
    procedure EnqueueSnapshot(const AForm: TCommonCustomForm;
      const ASnapshot: TDialogSnapshot); virtual;
```

Replace `TDialogBase.Show`/`TDialogBase.Show(const AForm: TCommonCustomForm)` (current lines 220-230):
```pascal
function TDialogBase.Show: IDialogBuilder;
begin
  InternalShow(ResolveParentForm(nil));
  Result := Self;
end;

function TDialogBase.Show(const AForm: TCommonCustomForm): IDialogBuilder;
begin
  InternalShow(ResolveParentForm(AForm));
  Result := Self;
end;
```
with:
```pascal
function TDialogBase.Show: IDialogBuilder;
begin
  Result := Show(nil);
end;

function TDialogBase.Show(const AForm: TCommonCustomForm): IDialogBuilder;
var
  LForm: TCommonCustomForm;
  LSnapshot: TDialogSnapshot;
begin
  LForm := ResolveParentForm(AForm);
  if not Assigned(LForm) then
    raise Exception.Create('Nenhum formul'#225'rio dispon'#237'vel para exibir o di'#225'logo.');
  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O n'#250'mero m'#237'nimo de bot'#245'es '#233' 1.');
  if FButtonHandlers.Count > 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  LSnapshot := TDialogSnapshot.Create(LForm, FTitle, FMessage, FMsgType, FCancelable,
    FFontSize, FBorderRadius, FAnimation, FTheme, FCustomSVG, FCustomIconColor,
    FResultCallback, FButtonHandlers);
  EnqueueSnapshot(LForm, LSnapshot);
  Result := Self;
end;

procedure TDialogBase.EnqueueSnapshot(const AForm: TCommonCustomForm;
  const ASnapshot: TDialogSnapshot);
begin
  TDialogQueueManager.Instance.Enqueue(AForm, ASnapshot);
end;
```

Note: `Show()` (no args) now delegates to `Show(AForm)` with `nil` — `ResolveParentForm(nil)` already resolves to `Screen.ActiveForm`/`Application.MainForm` exactly as before, so behavior for the no-arg overload is unchanged; this is a small DRY cleanup (the two overloads no longer duplicate the 3 validations).

- [ ] **Step 5: Run the tests, verify everything passes**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`. **Important:** at this point `TFMXDialog` (in `FMX.pas`) still declares `procedure InternalShow(const AForm: TCommonCustomForm); override;` overriding a method that no longer exists as `abstract` on `TDialogBase` — this will not compile yet. Task 6 fixes this by rewriting `FMX.pas`. If you're executing this plan task-by-task with review gates, **Task 5's build is expected to fail at the `MultiDialog4FMX.FMX` unit** with an "override" error — that's the correct, expected state; do not attempt to hack around it here. Confirm instead that the *error* is scoped to `FMX.pas`'s `override` clause (nothing else), which is the signal that `Base.pas`/`Mocks.pas`/`Builder.pas` themselves are internally consistent so far.

- [ ] **Step 6: Commit**

```bash
git add src/MultiDialog4FMX.Base.pas Tests/MultiDialog4FMX.Tests.Mocks.pas Tests/MultiDialog4FMX.Tests.Builder.pas
git commit -m "refactor(sprint6): TDialogBase.Show builds a TDialogSnapshot and enqueues it

InternalShow is replaced by the virtual EnqueueSnapshot seam. TFMXDialog
(MultiDialog4FMX.FMX.pas) still overrides the old InternalShow at this
point and will not compile until Task 6 — expected, fixed next."
```

---

### Task 6: Split `TFMXDialog` into thin config class + `TFMXDialogInstance` (`src/MultiDialog4FMX.FMX.pas`)

This is the largest task: relocate the existing visual/lifecycle logic (currently ~740 lines) from `TFMXDialog` into a new class `TFMXDialogInstance` that reads from a `TDialogSnapshot` instead of inherited `TDialogBase` fields, add the `FAlive` guard, wire `NotifyClosed`, and register the instance factory. No visual behavior changes — this is a mechanical relocation (`F<Campo>` → `FSnapshot.<Campo>`) plus the safety additions from the spec.

**Files:**
- Modify: `src/MultiDialog4FMX.FMX.pas` (full rewrite of its content, same unit name)

**Interfaces:**
- Consumes: `TDialogSnapshot`, `IDialogVisualInstance`, `TDialogInstanceFactory`, `TDialogQueueManager` (Tasks 3-4); `TDialogBase` (unchanged from Task 5).
- Produces: `TFMXDialog = class(TDialogBase, IDialogBuilder)` (now empty — no fields, no method overrides; `TDesktopDialog`/`TAndroidDialog`/`TiOSDialog` in `Desktop.pas`/`Android.pas`/`iOS.pas` keep compiling unchanged since they only do `class(TFMXDialog)`), and `TFMXDialogInstance = class(TInterfacedObject, IDialogVisualInstance)`, consumed only internally by the factory registered in this unit's `initialization` section.

- [ ] **Step 1: Replace the entire content of `src/MultiDialog4FMX.FMX.pas`**

```pascal
unit MultiDialog4FMX.FMX;

interface

uses
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Queue,

  FMX.Types,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.TextLayout,
  FMX.Platform,
  FMX.Ani,

  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Threading;

type
  // Config-only builder — nenhum estado visual vive aqui. TDialogBase.Show ja resolve
  // form/valida botoes/monta o TDialogSnapshot/chama EnqueueSnapshot; esta classe nao
  // precisa sobrescrever nada.
  TFMXDialog = class(TDialogBase, IDialogBuilder);

  TFMXDialogInstance = class(TInterfacedObject, IDialogVisualInstance)
  protected
    FSnapshot         : TDialogSnapshot;
    FAlive            : Boolean;
    FDialogRect       : TRectangle;
    FBtnLayout        : TLayout;
    FTimeoutButton    : TFmxObject;
    FTimeoutOrigText  : string;
    FTimeoutRemaining : Integer;
    FTimeoutCancelled : Boolean;
    function  ResolveIsDark: Boolean;
    procedure ApplyEntranceAnimation(const AOverlay: TLayout;
                                     const ADialogRect: TRectangle);
    procedure ApplyExitAnimation(const AOverlay: TLayout;
                                 const ADialogRect: TRectangle;
                                 const AOnComplete: TProc);
    procedure StartTimeoutCountdown;
    procedure UpdateTimeoutButtonText;
    procedure AutoClickTimeoutButton;
    function  CalculateMessageHeight(const AText: string; const AWidth: Single;
                                     const AFont: TFont): Single;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonTap(Sender: TObject; const Point: TPointF);
    procedure OnBackgroundClick(Sender: TObject);
    procedure CloseDialog(AOverlay: TLayout);
    function  GetPlatformScale: Single;
    function  BuildOverlay(const AParent: TCommonCustomForm;
                           out ABgRect: TRectangle): TLayout;
    function  BuildDialogRect(const AOverlay: TLayout): TRectangle;
    procedure BuildHeader(const ADialogRect: TRectangle);
    procedure BuildBody(const ADialogRect: TRectangle;
                        out AIconPresent: Boolean; out ABodyLayout: TLayout);
    procedure BuildButtons(const AOverlay: TLayout;
                           const ADialogRect: TRectangle);
    function  CalculateFinalHeight(const ABodyLayout: TLayout;
                                   const AIconPresent: Boolean): Single;
    procedure PaintColoredBtn(Sender: TObject; Canvas: TCanvas;
                              const ARect: TRectF);
  public
    constructor Create(const ASnapshot: TDialogSnapshot);
    destructor Destroy; override;
    // IDialogVisualInstance
    procedure Show;
    procedure Suppress;
  end;

implementation

const
  // Icon SVG paths
  SVG_WARNING  = 'M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z';
  SVG_ERROR    = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';
  SVG_INFO     = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-6h2v6zm0-8h-2V7h2v2z';
  // Literal quebrado em pedacos <=255 chars: Delphi <=11 limita literais de string a 255
  // elementos (E2056); o Delphi 12 removeu o limite. A concatenacao sofre constant folding,
  // entao o valor final e identico em todas as versoes (sem guarda condicional necessaria).
  // Numeros separados por espaco (ex. "-.9 .92" em vez de "-.9.92"): dois numeros com ponto
  // decimal colados sem separador (so a troca de sinal/segundo ponto marca a fronteira) faz
  // o parser de TPathData falhar com EConvertError ("X is not a valid floating point value"),
  // abortando a montagem antes de CalculateFinalHeight rodar (dialogo fica sem altura,
  // botoes "flutuam" fora da caixa). Espaco extra e um separador SVG valido, geometria identica.
  SVG_QUESTION = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2z' +
                 'm2.07-7.75l-.9 .92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1 .45-2.1 1.17-2.83l1.24-1.26c.37-.36 .59-.86 .59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z';
  SVG_SUCCESS  = 'M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z';

  // Base layout constants (logical points; multiply by Screen.Scale for DPI-aware size).
  // Font sizes are NOT multiplied — FMX handles font DPI internally.
  C_BaseMinDialogHeight = 200;
  C_BaseDialogWidth     = 300;
  C_BaseTitleHeight     = 40;
  C_BaseButtonsHeight   = 56;
  C_BasePaddingHeight   = 32;
  C_BaseIconSize        = 40;
  C_BaseBtnHeight       = 40;
  C_BaseResponsiveBreak = 600;
  C_BaseTitleFontSize   = 16;

{ TFMXDialogInstance }

constructor TFMXDialogInstance.Create(const ASnapshot: TDialogSnapshot);
begin
  inherited Create;
  FSnapshot := ASnapshot;
  FAlive    := True;
end;

destructor TFMXDialogInstance.Destroy;
begin
  FSnapshot.Free;
  inherited;
end;

procedure TFMXDialogInstance.Suppress;
begin
  FAlive := False;
  FTimeoutCancelled := True;
end;

function TFMXDialogInstance.ResolveIsDark: Boolean;
{$IF CompilerVersion >= 35.0}
// Delphi 11 Alexandria (CV 35.0) introduziu IFMXSystemAppearanceService/TSystemThemeKind
// (FMX.Platform). Em <= 10.4 esses identificadores nao existem (E2003), por isso a var
// e a deteccao do tema do SO ficam isoladas nesta guarda.
var
  LSvc: IFMXSystemAppearanceService;
{$IFEND}
begin
  case FSnapshot.Theme of
    dthDark:  Result := True;
    dthLight: Result := False;
  else
    // dthAuto: consulta o tema do sistema operacional
    {$IF CompilerVersion >= 35.0}
    // Delphi 11+: detecta automaticamente o tema do SO; fallback Light se indisponivel
    if TPlatformServices.Current.SupportsPlatformService(
         IFMXSystemAppearanceService, LSvc) then
      Result := LSvc.ThemeKind = TSystemThemeKind.Dark
    else
      Result := False;
    {$ELSE}
    // Delphi <= 10.4 (CV <= 34): sem servico de tema do SO — assume Light
    Result := False;
    {$IFEND}
  end;
end;

function TFMXDialogInstance.GetPlatformScale: Single;
var
  LScreenSvc: IFMXScreenService;
begin
  Result := 1.0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSvc) then
    Result := LScreenSvc.GetScreenScale;
end;

procedure TFMXDialogInstance.Show;
var
  LOverlay    : TLayout;
  LDialogRect : TRectangle;
  LBgRect     : TRectangle;
  LBodyLayout : TLayout;
  LIconPresent: Boolean;
begin
  LOverlay    := BuildOverlay(FSnapshot.Form, LBgRect);
  LDialogRect := BuildDialogRect(LOverlay);
  BuildButtons(LOverlay, LDialogRect);                         // Bottom — before Client
  BuildHeader(LDialogRect);                                    // Top
  BuildBody(LDialogRect, LIconPresent, LBodyLayout);           // Client

  LDialogRect.Height := CalculateFinalHeight(LBodyLayout, LIconPresent);

  if FSnapshot.Cancelable then
  begin
    LBgRect.HitTest := True;
    LBgRect.OnClick := OnBackgroundClick;
  end;

  // Prepare initial state for entrance animation BEFORE making the dialog visible
  case FSnapshot.Animation of
    danFade:
      LOverlay.Opacity := 0;
    danScale:
    begin
      LDialogRect.Scale.X := 0.8;
      LDialogRect.Scale.Y := 0.8;
    end;
  end;

  if FSnapshot.Animation <> TDialogAnimation.danNone then
    ApplyEntranceAnimation(LOverlay, LDialogRect);
end;

function TFMXDialogInstance.BuildOverlay(const AParent: TCommonCustomForm;
  out ABgRect: TRectangle): TLayout;
var
  LOverlay: TLayout;
  LBgRect : TRectangle;
begin
  LOverlay := TLayout.Create(AParent);
  LOverlay.Parent := AParent;
  LOverlay.Align := TAlignLayout.Contents;
  LOverlay.HitTest := True;
  LOverlay.BringToFront;

  LBgRect := TRectangle.Create(LOverlay);
  LBgRect.Parent := LOverlay;
  LBgRect.Align := TAlignLayout.Contents;
  LBgRect.Fill.Color := TAlphaColorRec.Black;
  LBgRect.Opacity := IfThen(ResolveIsDark, 0.65, 0.40);
  LBgRect.Stroke.Kind := TBrushKind.None;

  ABgRect := LBgRect;
  Result  := LOverlay;
end;

function TFMXDialogInstance.BuildDialogRect(const AOverlay: TLayout): TRectangle;
var
  LDialogRect: TRectangle;
begin
  LDialogRect := TRectangle.Create(AOverlay);
  LDialogRect.Parent := AOverlay;
  LDialogRect.Align := TAlignLayout.Center;
  LDialogRect.Width := C_BaseDialogWidth;
  LDialogRect.XRadius := FSnapshot.BorderRadius;
  LDialogRect.YRadius := FSnapshot.BorderRadius;
  if ResolveIsDark then
  begin
    LDialogRect.Fill.Color   := $FF2D2D2D;
    LDialogRect.Stroke.Kind  := TBrushKind.Solid;
    LDialogRect.Stroke.Color := $FF444444;
  end
  else
  begin
    LDialogRect.Fill.Color   := TAlphaColorRec.White;
    LDialogRect.Stroke.Kind  := TBrushKind.Solid;
    LDialogRect.Stroke.Color := $FFE0E0E0;
  end;
  LDialogRect.Padding.Rect := RectF(4, 4, 4, 4);
  FDialogRect := LDialogRect;
  Result := LDialogRect;
end;

procedure TFMXDialogInstance.BuildHeader(const ADialogRect: TRectangle);
var
  LLblTitle: TLabel;
begin
  if FSnapshot.Title = EmptyStr then
    Exit;

  LLblTitle := TLabel.Create(ADialogRect);
  LLblTitle.Parent := ADialogRect;
  LLblTitle.Align := TAlignLayout.Top;
  LLblTitle.Text := FSnapshot.Title;
  LLblTitle.TextSettings.Font.Style := [TFontStyle.fsBold];
  LLblTitle.Margins.Rect := RectF(16, 12, 16, 4);
  LLblTitle.Height := C_BaseTitleHeight;
  LLblTitle.TextSettings.Font.Size := C_BaseTitleFontSize;
  if ResolveIsDark then
  begin
    LLblTitle.StyledSettings := [TStyledSetting.Style];
    LLblTitle.TextSettings.FontColor := TAlphaColorRec.White;
  end
  else
    LLblTitle.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  LLblTitle.VertTextAlign := TTextAlign.Center;
end;

procedure TFMXDialogInstance.BuildBody(const ADialogRect: TRectangle;
  out AIconPresent: Boolean; out ABodyLayout: TLayout);
var
  LScrollBox    : TVertScrollBox;
  LBodyLayout   : TLayout;
  LIconContainer: TLayout;
  LIconPath     : TPath;
  LblMsg        : TLabel;
  LMsgWidth     : Single;
  LMsgHeight    : Single;
  LMsgFont      : TFont;
begin
  AIconPresent := (FSnapshot.MsgType <> TMultiDialogType.mdtCustom) or (FSnapshot.CustomSVG <> '');

  LScrollBox := TVertScrollBox.Create(ADialogRect);
  LScrollBox.Parent := ADialogRect;
  LScrollBox.Align := TAlignLayout.Client;
  LScrollBox.Margins.Rect := RectF(0, 8, 0, 8);
  LScrollBox.ShowScrollBars := True;

  LBodyLayout := TLayout.Create(LScrollBox);
  LBodyLayout.Parent := LScrollBox;
  LBodyLayout.Align := TAlignLayout.Top;
  LBodyLayout.Margins.Rect := RectF(16, 0, 16, 0);

  if AIconPresent then
  begin
    LIconContainer := TLayout.Create(LBodyLayout);
    LIconContainer.Parent := LBodyLayout;
    LIconContainer.Align := TAlignLayout.Left;
    LIconContainer.Width := C_BaseIconSize;
    LIconContainer.Margins.Right := 16;

    LIconPath := TPath.Create(LIconContainer);
    LIconPath.Parent := LIconContainer;
    LIconPath.Align := TAlignLayout.Top;
    LIconPath.Height := C_BaseIconSize;
    LIconPath.Width := C_BaseIconSize;
    LIconPath.Stroke.Kind := TBrushKind.None;

    // --- SVG ---
    // Um path SVG malformado (customizado via SetIcon, ou um builtin futuro) nao pode
    // abortar a montagem do dialogo: CalculateFinalHeight (mais abaixo em Show) so
    // roda se este metodo completar. Sem o guard, uma excecao aqui deixa o LDialogRect sem
    // altura definida e os botoes (montados antes, em BuildButtons) "flutuam" fora da caixa.
    try
      if FSnapshot.CustomSVG <> '' then
        LIconPath.Data.Data := FSnapshot.CustomSVG
      else
        case FSnapshot.MsgType of
          mdtWarning:      LIconPath.Data.Data := SVG_WARNING;
          mdtError:        LIconPath.Data.Data := SVG_ERROR;
          mdtInformation:  LIconPath.Data.Data := SVG_INFO;
          mdtQuestion:     LIconPath.Data.Data := SVG_QUESTION;
          mdtConfirmation: LIconPath.Data.Data := SVG_SUCCESS;
        end;
    except
      // Degrada para "sem icone" (container 40x40 permanece, so o path fica vazio) em vez
      // de propagar e interromper a montagem no meio.
    end;

    // --- Cor ---
    if FSnapshot.CustomIconColor <> TAlphaColor(0) then
      LIconPath.Fill.Color := FSnapshot.CustomIconColor
    else
      case FSnapshot.MsgType of
        mdtWarning:      LIconPath.Fill.Color := TAlphaColorRec.Gold;
        mdtError:        LIconPath.Fill.Color := TAlphaColorRec.Red;
        mdtInformation:  LIconPath.Fill.Color := TAlphaColorRec.Dodgerblue;
        mdtQuestion,
        mdtConfirmation: LIconPath.Fill.Color := TAlphaColorRec.Limegreen;
        mdtCustom:       LIconPath.Fill.Color := TAlphaColorRec.Gray;
      end;
  end;

  if FSnapshot.Message <> EmptyStr then
  begin
    LblMsg := TLabel.Create(LBodyLayout);
    LblMsg.Parent := LBodyLayout;
    LblMsg.Align := TAlignLayout.Client;
    LblMsg.WordWrap := True;
    LblMsg.Text := FSnapshot.Message;
    LblMsg.VertTextAlign := TTextAlign.Leading;
    LblMsg.TextSettings.Font.Size := FSnapshot.FontSize;
    if ResolveIsDark then
    begin
      LblMsg.StyledSettings := [TStyledSetting.Style];
      LblMsg.TextSettings.FontColor := TAlphaColorRec.White;
    end
    else
      LblMsg.StyledSettings := [TStyledSetting.Style, TStyledSetting.FontColor];
  end;

  if AIconPresent then
    LMsgWidth := C_BaseDialogWidth - 32 - (C_BaseIconSize + 16) - 8
  else
    LMsgWidth := C_BaseDialogWidth - 40;

  LMsgFont := TFont.Create;
  try
    LMsgFont.Size := FSnapshot.FontSize;
    LMsgHeight := CalculateMessageHeight(FSnapshot.Message, LMsgWidth, LMsgFont);
  finally
    LMsgFont.Free;
  end;

  if AIconPresent then
    LBodyLayout.Height := Max(LMsgHeight, C_BaseIconSize)
  else
    LBodyLayout.Height := LMsgHeight;

  ABodyLayout := LBodyLayout;
end;

procedure TFMXDialogInstance.BuildButtons(const AOverlay: TLayout;
  const ADialogRect: TRectangle);
const
  // Box width (300) minus box padding (4+4) minus row margins (4+4) = 284 dp usable width.
  C_RowInnerWidth = C_BaseDialogWidth - 16;
  C_BtnGap        = 8;
var
  // Plain TLayout with MANUAL X/Y positioning. TFlowLayout was used here before, but it
  // paints its children at a vertical offset that differs from the position it reports
  // on FMX <= 10.4 (Delphi 10.3.3): buttons drifted below the rounded bottom of the box
  // while AbsoluteRect still claimed they were inside. Positioning every button by hand
  // from the layout constants is deterministic and renders identically on all Delphi versions.
  LBtnLayout    : TLayout;
  LWidthButtons : Single;
  LCurrentWidth : Single;
  LRec          : TButtonHandler;
  LBtn          : TButton;
  LColorRect    : TRectangle;
  LIsResponsive   : Boolean;
  LEffectiveColor : TAlphaColor;
  LVOffset        : Single;   // vertical centering of a 40 dp button inside a 56 dp band
  LRowTotalW      : Single;
  LCurX           : Single;
  LCurY           : Single;
  I               : Integer;
begin
  LBtnLayout := TLayout.Create(ADialogRect);
  LBtnLayout.Parent := ADialogRect;
  LBtnLayout.Align := TAlignLayout.Bottom;
  LBtnLayout.Height := C_BaseButtonsHeight;
  LBtnLayout.Margins.Rect := RectF(4, 0, 4, 4);
  FBtnLayout := LBtnLayout;

  // Responsive: 4 buttons in portrait — 3+1 layout (3 buttons row 1, 1 full-width row 2)
  LIsResponsive := (FSnapshot.Buttons.Count = 4) and (Screen.Width < Screen.Height) and
                   (Screen.Width < C_BaseResponsiveBreak);

  if LIsResponsive then
  begin
    LBtnLayout.Height := C_BaseButtonsHeight * 2;
    LWidthButtons := Round(C_BaseDialogWidth / 3) - 16;  // = 84 dp (3×84+2×8=268 dp)
  end
  else if FSnapshot.Buttons.Count = 1 then
    LWidthButtons := C_BaseDialogWidth - 32
  else
    LWidthButtons := Round(C_BaseDialogWidth / FSnapshot.Buttons.Count) - 24;

  LVOffset := (C_BaseButtonsHeight - C_BaseBtnHeight) / 2;  // = 8 dp

  // Horizontal start of the first row, centered within the usable width.
  if LIsResponsive then
    LRowTotalW := 3 * LWidthButtons + 2 * C_BtnGap            // row 1 = 3 buttons
  else
    LRowTotalW := FSnapshot.Buttons.Count * LWidthButtons +
                  (FSnapshot.Buttons.Count - 1) * C_BtnGap;
  LCurX := (C_RowInnerWidth - LRowTotalW) / 2;
  LCurY := LVOffset;

  FTimeoutButton    := nil;
  FTimeoutOrigText  := '';
  FTimeoutRemaining := 0;
  FTimeoutCancelled := False;

  for I := 0 to FSnapshot.Buttons.Count - 1 do
  begin
    LRec := FSnapshot.Buttons[I];

    if LIsResponsive and (I = 3) then
    begin
      // 4th button: full-width, centered on the second row (band top = 56 dp)
      LCurrentWidth := C_BaseDialogWidth - 32;
      LCurX         := (C_RowInnerWidth - LCurrentWidth) / 2;
      LCurY         := C_BaseButtonsHeight + LVOffset;
    end
    else
      LCurrentWidth := LWidthButtons;

    LRec.Overlay := AOverlay;

    LEffectiveColor := LRec.Color;
    if ResolveIsDark and (LEffectiveColor = TAlphaColor(0)) then
      LEffectiveColor := $FF555555;  // neutral dark-grey for uncoloured buttons in dark mode

    if LEffectiveColor <> TAlphaColor(0) then
    begin
      // Colored button: canvas-painted TRectangle — avoids Android TLabel/TBrush
      // rendering issue where all buttons inherit the last button's text and color.
      LColorRect := TRectangle.Create(LBtnLayout);
      LColorRect.Parent        := LBtnLayout;
      LColorRect.Align         := TAlignLayout.None;
      LColorRect.Fill.Kind     := TBrushKind.None;   // suppress default fill; OnPainting draws it
      LColorRect.Stroke.Kind   := TBrushKind.None;
      LColorRect.XRadius       := 4;
      LColorRect.YRadius       := 4;
      LColorRect.Height        := C_BaseBtnHeight;
      LColorRect.Width         := LCurrentWidth;
      LColorRect.Position.X    := LCurX;
      LColorRect.Position.Y    := LCurY;
      LColorRect.HitTest       := True;

      LColorRect.Tag        := NativeInt(LEffectiveColor);  // per-instance color for paint handler
      LColorRect.TagString  := LRec.Text;              // per-instance text for paint handler
      LColorRect.OnPainting := PaintColoredBtn;        // self-contained paint

      LColorRect.TagObject  := LRec;                   // click handler (unchanged)
      LColorRect.OnClick    := ButtonClick;

      if LRec.Timeout > 0 then
      begin
        FTimeoutButton    := LColorRect;
        FTimeoutOrigText  := LRec.Text;
        FTimeoutRemaining := LRec.Timeout;
        FTimeoutCancelled := False;
      end;
    end
    else
    begin
      // Default or styled button: TButton
      LBtn := TButton.Create(LBtnLayout);
      LBtn.Parent     := LBtnLayout;
      LBtn.Align      := TAlignLayout.None;
      LBtn.Text       := LRec.Text;
      LBtn.TextSettings.Font.Size := FSnapshot.FontSize;
      LBtn.StyledSettings := [TStyledSetting.Style];
      LBtn.Height     := C_BaseBtnHeight;
      LBtn.Width      := LCurrentWidth;
      LBtn.Position.X := LCurX;
      LBtn.Position.Y := LCurY;

      if LRec.StyleLookup <> '' then
      begin
        LBtn.StyleLookup    := LRec.StyleLookup;
        LBtn.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style,
                                 TStyledSetting.FontColor, TStyledSetting.Size];
      end;

      LBtn.TagObject := LRec;

      if Assigned(LRec.TapHandler) then
        LBtn.OnTap   := ButtonTap
      else
        LBtn.OnClick := ButtonClick;

      if LRec.Timeout > 0 then
      begin
        FTimeoutButton    := LBtn;
        FTimeoutOrigText  := LRec.Text;
        FTimeoutRemaining := LRec.Timeout;
        FTimeoutCancelled := False;
      end;
    end;

    // Advance X for the next button in the same (first) row.
    if not (LIsResponsive and (I = 3)) then
      LCurX := LCurX + LCurrentWidth + C_BtnGap;
  end;

  if Assigned(FTimeoutButton) then
    StartTimeoutCountdown;
end;

function TFMXDialogInstance.CalculateFinalHeight(const ABodyLayout: TLayout;
  const AIconPresent: Boolean): Single;
var
  LMaxScreenHeight: Single;
begin
  Result := ABodyLayout.Height + 16 + FBtnLayout.Height + C_BasePaddingHeight;

  if FSnapshot.Title <> EmptyStr then
    Result := Result + C_BaseTitleHeight + 16;

  Result := Max(Result, C_BaseMinDialogHeight);

  LMaxScreenHeight := Screen.Size.Height * 0.7;  // 70% in logical dp — no scale division
  if Result > LMaxScreenHeight then
    Result := LMaxScreenHeight;
end;

function TFMXDialogInstance.CalculateMessageHeight(const AText: string;
  const AWidth: Single; const AFont: TFont): Single;
var
  Layout: TTextLayout;
begin
  if AText = EmptyStr then
  begin
    Result := 0;
    Exit;
  end;

  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    Layout.Font := AFont;
    Layout.MaxSize := TSizeF.Create(AWidth, 9999);
    Layout.WordWrap := True;
    Layout.Text := AText;
    Layout.EndUpdate;
    Result := Layout.TextHeight + 10;
  finally
    Layout.Free;
  end;
end;

procedure TFMXDialogInstance.CloseDialog(AOverlay: TLayout);
var
  I           : Integer;
  LChild      : TFmxObject;
  LDialogRect : TRectangle;
  LSelf       : IDialogVisualInstance; // keeps Self alive until the deferred destroy runs
  LForm       : TCommonCustomForm;
  LDoDestroy  : TProc;
begin
  if not Assigned(AOverlay) then
    Exit;

  FTimeoutCancelled := True;
  FTimeoutButton    := nil;

  LSelf       := Self;
  LDialogRect := FDialogRect;
  FDialogRect := nil;
  LForm       := FSnapshot.Form;

  // Nil the overlay reference on each handler.
  // Handlers are owned by FSnapshot.Buttons (TObjectList OwnsObjects=True) — do NOT free here.
  // FMX does NOT free TagObject when destroying controls.
  // Works for both TButton and TRectangle (colored button variant).
  if Assigned(FBtnLayout) then
    for I := 0 to FBtnLayout.ChildrenCount - 1 do
    begin
      LChild := FBtnLayout.Children[I];
      if LChild.TagObject is TButtonHandler then
      begin
        TButtonHandler(LChild.TagObject).Overlay := nil;
        LChild.TagObject := nil;
      end;
    end;
  FBtnLayout := nil;

  LDoDestroy := procedure
  begin
    if not FAlive then
      Exit; // form ja foi destruido (Suppress rodou) — overlay/objetos ja podem ter sido liberados
    AOverlay.Parent := nil;
    {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
    AOverlay.DisposeOf;
    {$ELSE}
    AOverlay.Free;
    {$ENDIF}
    TDialogQueueManager.Instance.NotifyClosed(LForm);
    LSelf := nil;
  end;

  if FSnapshot.Animation = TDialogAnimation.danNone then
    LDoDestroy()
  else
    ApplyExitAnimation(AOverlay, LDialogRect, LDoDestroy);
end;

procedure TFMXDialogInstance.OnBackgroundClick(Sender: TObject);
var
  LObj    : TFmxObject;
  LOverlay: TLayout;
begin
  if Sender is TFmxObject then
  begin
    LObj := TFmxObject(Sender).Parent;
    if LObj is TLayout then
    begin
      LOverlay := TLayout(LObj);
      if Assigned(FSnapshot.ResultCallback) then
        FSnapshot.ResultCallback(mrCancel);
      // Defer CloseDialog: destroying the overlay inside a click handler leaves
      // the Win32 message pump in an inconsistent state (mouse capture stuck).
      // ForceQueue schedules execution after the current event returns.
      TThread.ForceQueue(nil, procedure begin CloseDialog(LOverlay); end);
    end;
  end;
end;

procedure TFMXDialogInstance.ButtonClick(Sender: TObject);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
begin
  if not (Sender is TFmxObject) then Exit;
  LFmxObj := TFmxObject(Sender);
  if not (LFmxObj.TagObject is TButtonHandler) then Exit;

  Obj      := TButtonHandler(LFmxObj.TagObject);
  LOverlay := Obj.Overlay;

  // Clear TagObject BEFORE calling handler — prevents use-after-free when
  // CloseDialog destroys the control hierarchy.
  LFmxObj.TagObject := nil;

  try
    if Assigned(Obj.ClickHandler) then
      Obj.ClickHandler(Sender);
    if Assigned(Obj.AnonymousHandler) then
      Obj.AnonymousHandler();
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, PointF(0, 0));  // fallback for colored buttons with TapHandler
    if Assigned(FSnapshot.ResultCallback) then
      FSnapshot.ResultCallback(Obj.ModalResult);
  finally
    Obj.Overlay := nil;   // handler owned by FSnapshot.Buttons — do NOT free
    TThread.ForceQueue(nil, procedure begin CloseDialog(LOverlay); end);
  end;
end;

procedure TFMXDialogInstance.ButtonTap(Sender: TObject; const Point: TPointF);
var
  Obj     : TButtonHandler;
  LOverlay: TLayout;
  LFmxObj : TFmxObject;
begin
  if not (Sender is TFmxObject) then Exit;
  LFmxObj := TFmxObject(Sender);
  if not (LFmxObj.TagObject is TButtonHandler) then Exit;

  Obj      := TButtonHandler(LFmxObj.TagObject);
  LOverlay := Obj.Overlay;

  LFmxObj.TagObject := nil;

  try
    if Assigned(Obj.TapHandler) then
      Obj.TapHandler(Sender, Point);
    if Assigned(FSnapshot.ResultCallback) then
      FSnapshot.ResultCallback(Obj.ModalResult);
  finally
    Obj.Overlay := nil;   // handler owned by FSnapshot.Buttons — do NOT free
    TThread.ForceQueue(nil, procedure begin CloseDialog(LOverlay); end);
  end;
end;

procedure TFMXDialogInstance.PaintColoredBtn(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LRect : TRectangle;
  LColor: TAlphaColor;
  LState: TCanvasSaveState;
begin
  if not (Sender is TRectangle) then
    Exit;

  LRect  := TRectangle(Sender);
  LColor := TAlphaColor(LRect.Tag);

  LState := Canvas.SaveState;
  try
    // Draw colored rounded background
    Canvas.Fill.Kind  := TBrushKind.Solid;
    Canvas.Fill.Color := LColor;
    Canvas.FillRect(ARect, LRect.XRadius, LRect.YRadius, AllCorners, 1.0);

    // Draw white centered text (Canvas.Fill.Color is used as text color by FillText)
    Canvas.Fill.Color := TAlphaColorRec.White;
    Canvas.Font.Size  := FSnapshot.FontSize;
    Canvas.FillText(ARect, LRect.TagString, False, 1.0, [], TTextAlign.Center,
                    TTextAlign.Center);
  finally
    Canvas.RestoreState(LState);
  end;
end;

{ Animações }

procedure TFMXDialogInstance.ApplyEntranceAnimation(const AOverlay: TLayout;
  const ADialogRect: TRectangle);
begin
  case FSnapshot.Animation of
    danFade:
      TThread.ForceQueue(nil, procedure
      begin
        if not FAlive then Exit;
        TAnimator.AnimateFloat(AOverlay, 'Opacity', 1, 0.25);
      end);

    danScale:
    begin
      TAnimator.AnimateFloat(ADialogRect, 'Scale.X', 1.0, 0.3,
        TAnimationType.Out, TInterpolationType.Back);
      TAnimator.AnimateFloat(ADialogRect, 'Scale.Y', 1.0, 0.3,
        TAnimationType.Out, TInterpolationType.Back);
    end;

    danSlide:
      TThread.ForceQueue(nil, procedure
      var
        LTargetY: Single;
      begin
        if not FAlive then Exit;
        LTargetY           := ADialogRect.Position.Y;
        ADialogRect.Align  := TAlignLayout.None;
        ADialogRect.Position.X := (AOverlay.Width  - ADialogRect.Width)  / 2;
        ADialogRect.Position.Y := -ADialogRect.Height;
        TAnimator.AnimateFloat(ADialogRect, 'Position.Y', LTargetY, 0.3,
          TAnimationType.Out, TInterpolationType.Quadratic);
      end);
  end;
end;

procedure TFMXDialogInstance.ApplyExitAnimation(const AOverlay: TLayout;
  const ADialogRect: TRectangle; const AOnComplete: TProc);
// TFloatAnimation.OnFinish is TNotifyEvent (method pointer) — incompatible with
// anonymous TProc. We start the visual animations and then fire AOnComplete from
// a background thread after the matching duration so the overlay is already
// invisible when it gets freed.
var
  LDurationMs: Integer;
begin
  case FSnapshot.Animation of
    danFade:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.2);
      LDurationMs := 220;
    end;

    danScale:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.2);
      if Assigned(ADialogRect) then
      begin
        TAnimator.AnimateFloat(ADialogRect, 'Scale.X', 0.8, 0.2);
        TAnimator.AnimateFloat(ADialogRect, 'Scale.Y', 0.8, 0.2);
      end;
      LDurationMs := 220;
    end;

    danSlide:
    begin
      TAnimator.AnimateFloat(AOverlay, 'Opacity', 0.0, 0.25);
      if Assigned(ADialogRect) then
      begin
        if ADialogRect.Align = TAlignLayout.Center then
        begin
          ADialogRect.Align      := TAlignLayout.None;
          ADialogRect.Position.X := (AOverlay.Width - ADialogRect.Width) / 2;
        end;
        TAnimator.AnimateFloat(ADialogRect, 'Position.Y', AOverlay.Height, 0.25);
      end;
      LDurationMs := 270;
    end;
  else
    LDurationMs := 0;
  end;

  // Fire the destroy callback after the animation completes.
  // Sleep runs on a pool thread; ForceQueue marshals back to the UI thread.
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(LDurationMs);
    TThread.ForceQueue(nil, procedure begin AOnComplete; end);
  end).Start;
end;

{ Timeout countdown }

procedure TFMXDialogInstance.StartTimeoutCountdown;
begin
  UpdateTimeoutButtonText;  // mostra "(N)" imediatamente
  TThread.CreateAnonymousThread(procedure
  begin
    while (FTimeoutRemaining > 0) and not FTimeoutCancelled do
    begin
      Sleep(1000);
      if FTimeoutCancelled then
        Exit;
      TThread.Queue(nil, procedure
      begin
        if FTimeoutCancelled then
          Exit;
        Dec(FTimeoutRemaining);
        if FTimeoutRemaining > 0 then
          UpdateTimeoutButtonText
        else
          AutoClickTimeoutButton;
      end);
    end;
  end).Start;
end;

procedure TFMXDialogInstance.UpdateTimeoutButtonText;
begin
  if not Assigned(FTimeoutButton) then
    Exit;

  if FTimeoutButton is TButton then
    TButton(FTimeoutButton).Text := FTimeoutOrigText + ' (' + FTimeoutRemaining.ToString + ')'
  else if FTimeoutButton is TRectangle then
  begin
    TRectangle(FTimeoutButton).TagString := FTimeoutOrigText + ' (' + FTimeoutRemaining.ToString + ')';
    TRectangle(FTimeoutButton).Repaint;
  end;
end;

procedure TFMXDialogInstance.AutoClickTimeoutButton;
begin
  if Assigned(FTimeoutButton) then
    ButtonClick(FTimeoutButton);
end;

initialization
  TDialogQueueManager.RegisterInstanceFactory(
    function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance
    begin
      Result := TFMXDialogInstance.Create(ASnapshot);
    end);

end.
```

**Nota sobre o `LSelf: IDialogVisualInstance` em `CloseDialog`:** como `TDialogQueueManager.FActive` guarda a instância só como `IDialogVisualInstance` e a remove de lá antes de rodar a animação de saída (`NotifyClosed` só é chamado dentro de `LDoDestroy`, no fim), a própria instância ficaria **sem nenhum dono** durante a janela da animação de saída (o `Sleep(LDurationMs)` em `ApplyExitAnimation` roda numa thread separada). `LSelf := Self` na pilha do closure `LDoDestroy` (que sobrevive porque é capturado pela clausura) segura essa referência viva até o próprio `LDoDestroy` terminar — substitui o antigo `FKeepAlive := Self` sem precisar de um campo dedicado, porque agora o "dono" natural (quando ativo) é `TDialogQueueManager.FActive`, e durante o fechamento quem segura é a clausura.

- [ ] **Step 2: Build**

```bash
Tests/build_tests.bat
```
Expected at this point: `COMPILE_FAILED` in `Tests.Android.pas` — it still calls `TAndroidDialogCracker(FDialog).InternalShow(...)`, `.FButtonHandlers`, `.FTitle`, `.FCustomSVG`, etc. directly on `TAndroidDialog` (`= TFMXDialog`), none of which exist anymore on the now-thin `TFMXDialog`. This is expected — Task 7 rewrites that fixture. Confirm the compiler errors are **confined to `Tests.Android.pas`** (nothing else) before moving on.

- [ ] **Step 3: Commit** (yes, with `Tests.Android.pas` still broken — Task 7 is the next task and fixes it; committing here keeps this task's diff reviewable on its own)

```bash
git add src/MultiDialog4FMX.FMX.pas
git commit -m "refactor(sprint6): split TFMXDialog into config-only class + TFMXDialogInstance

TFMXDialogInstance is built from a TDialogSnapshot instead of inheriting
live TDialogBase fields; all Build*/animation/timeout logic is relocated
unchanged (F<field> -> FSnapshot.<field>). Adds the FAlive guard and wires
NotifyClosed/RegisterInstanceFactory per the Sprint 6 spec.

Tests.Android.pas does not compile after this commit — fixed in the next
commit (Task 7 migrates it to build a TDialogSnapshot + TFMXDialogInstance
directly instead of poking TDialogBase fields on the merged object)."
```

---

### Task 7: Migrate `Tests/MultiDialog4FMX.Tests.Android.pas` to `TFMXDialogInstance` + hand-built `TDialogSnapshot`

**Files:**
- Modify: `Tests/MultiDialog4FMX.Tests.Android.pas` (full rewrite)

**Interfaces:**
- Consumes: `TDialogSnapshot.Create(...)` (Task 3), `TFMXDialogInstance` (Task 6 — same unit, `MultiDialog4FMX.FMX`, so this test unit needs `MultiDialog4FMX.FMX` in its `uses` instead of `MultiDialog4FMX.Android`).

Every test in this fixture follows the same mechanical transform: build a `TButtonHandlerList` + `TDialogSnapshot` by hand (instead of `TAndroidDialogCracker(FDialog).FButtonHandlers.Add(...)` / `.FTitle := ...`), then create a `TFMXDialogInstance` from it (instead of `TAndroidDialog.Create`), and call the same protected methods via a cracker subclass of `TFMXDialogInstance`.

- [ ] **Step 1: Replace the entire content of `Tests/MultiDialog4FMX.Tests.Android.pas`**

```pascal
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

initialization
  TDUnitX.RegisterTestFixture(TAndroidDialogTests);
  TDUnitX.RegisterTestFixture(TAndroidDialogCloseTests);

end.
```

**Notas sobre o que mudou de propósito (não de comportamento) nesta migração:**
- As antigas fixtures `TAndroidDialogLayoutTests` e `TAndroidDialogIconTests` foram fundidas em `TAndroidDialogTests` (fazia sentido separá-las quando testavam efeitos colaterais em campos herdados de `TDialogBase`; agora todas operam sobre a mesma `TFMXDialogInstanceCracker` construída a partir de um snapshot, então a separação não agregava mais).
- `TestInternalShow_RequiresMinimumOneButton`, `TestInternalShow_EnforcesMaximumFourButtons` e `TestInternalShow_TwoButtonsNoHandler_NoValidationException` **não têm mais equivalente aqui** — essas 3 validações moveram para `TDialogBase.Show` (Task 5) e já são cobertas por `Tests.Buttons.pas`/testes de `TDialogBase` existentes que exercitam `AddButton`/`Show` via `IDialogBuilder`. `TFMXDialogInstance.Show` não valida nada — recebe um snapshot já validado.
- `TestResolveParentForm_WithExplicitForm`/`TestResolveParentForm_WithNilForm` **não têm mais equivalente aqui** — `ResolveParentForm` continua em `TDialogBase` inalterado (nunca foi movido), já é testado ali; `TFMXDialogInstance` não resolve form, recebe `FSnapshot.Form` já resolvido.

- [ ] **Step 2: Run the tests, verify everything passes**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`. Count check: Task 4 left us at 59 tests. This migration removes 6 tests that no longer have a home here (3 validation tests + 2 `ResolveParentForm` tests, now redundant with existing coverage elsewhere — confirm by grep, see Step 3) and keeps the other 19 (renaming `TestInternalShow_SubMethodsRun_FBtnLayoutAssigned` to `TestShow_SubMethodsRun_FBtnLayoutAssigned`), so expect `Tests Passed : 53` (59 - 6), `Tests Failed : 0`, `Tests Errored : 0`.

- [ ] **Step 3: Confirm the 3 removed validation tests are still covered elsewhere**

```bash
grep -rn "RequiresMinimumOneButton\|EnforcesMaximumFourButtons\|TwoButtonsNoHandler" Tests/*.pas
```
Expected: no hits (they're gone, on purpose) — the underlying behavior (`TDialogBase.Show` raising on 0 or 5+ buttons, not raising on 2 handler-less buttons) is exercised by `Tests.Buttons.pas`'s `TestAddButton_MaximumFourButtons`/`TestAddButton_ExceedsMaximum_RaisesException` for the button-count boundary. If this grep step makes you uneasy that the 0-button and no-handler cases specifically aren't covered by any surviving test, that's a valid catch — add two focused tests to `Tests/MultiDialog4FMX.Tests.Builder.pas` using `TMockDialogBase` before proceeding:
```pascal
    [Test]
    procedure TestShow_NoButtons_RaisesException;

    [Test]
    procedure TestShow_TwoButtonsNoHandler_NoException;
```
```pascal
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
begin
  FDialog.Buttons.AddButton('A').AddButton('B');
  Assert.WillNotRaise(
    procedure
    begin
      FDialog.Show;
    end);
end;
```
Re-run `Tests/build_tests.bat`; expect `Tests Passed : 55`.

- [ ] **Step 4: Commit**

```bash
git add Tests/MultiDialog4FMX.Tests.Android.pas Tests/MultiDialog4FMX.Tests.Builder.pas
git commit -m "test(sprint6): migrate Tests.Android.pas to TFMXDialogInstance + hand-built snapshots

Mechanical migration — no behavior assertions changed, only how each test
builds its fixture (TDialogSnapshot + TFMXDialogInstance instead of poking
fields on the old merged TAndroidDialog/TFMXDialog object). 3 validation
tests moved to TDialogBase.Show and are covered by TestShow_NoButtons_
RaisesException / TestShow_TwoButtonsNoHandler_NoException in
Tests.Builder.pas; 2 ResolveParentForm tests stay covered where they
already were (ResolveParentForm never moved)."
```

---

### Task 8: Full suite, `Tests.MemoryLeaks.pas` closure-flow check, final review

**Files:**
- Modify (verify only, likely no changes needed): `Tests/MultiDialog4FMX.Tests.MemoryLeaks.pas`
- Modify: `Tests/MultiDialog4FMX.Tests.dpr` (already updated incrementally in Tasks 3-4 — just double check)

**Interfaces:** none new — this task is verification and closes out the branch.

- [ ] **Step 1: Re-read `Tests/MultiDialog4FMX.Tests.MemoryLeaks.pas` against the new `Show` flow**

These 5 tests use `TMockDialogBase` and only ever call `Dialog := nil` to trigger cleanup — they never call `.Show`, so they were never coupled to `InternalShow`/`EnqueueSnapshot` in the first place (confirmed by re-reading the file: no `.Show` call anywhere in `Tests.MemoryLeaks.pas`). **No changes needed here** — this step is a verification, not a modification; the spec's assumption that this fixture needed adjustment doesn't hold once you look at what it actually calls, and the plan should say so rather than edit code that doesn't need it.

- [ ] **Step 2: Full clean rebuild and full test run**

```bash
Tests/build_tests.bat
```
Expected: `COMPILE_OK`, and the full count from `Tests Found` onward should read:
- Baseline 52 (Task 1)
- \+ 3 `Tests.Snapshot` (Task 3) = 55
- \+ 4 `Tests.Queue` (Task 4) = 59
- \+ 0 net from Task 5 (rename only)
- \+ 0 net from Task 6 (no test file touched)
- \- 6 removed, \+ 2 added back in Task 7 = 55
`Tests Passed : 55`, `Tests Failed : 0`, `Tests Errored : 0`, `Tests Leaked : 0`.

If the actual number differs, don't force-match this arithmetic — trust the real DUnitX output and investigate any mismatch (most likely cause: forgot to add a new unit to `Tests.dpr`, or a duplicate `[TestFixture]` registration).

- [ ] **Step 3: Manual smoke check — confirm the new queueing behavior with a quick throwaway test**

This isn't a permanent test (delete it after), it's a sanity check that the FIFO behavior works through the *real* production path (`TDialogBase.Show` → real `TDialogQueueManager` → real `TFMXDialogInstance`), not just through the fakes/mocks used in Tasks 3-4-7. Temporarily add to `Tests.Queue.pas`:
```pascal
    [Test]
    procedure SMOKE_RealShow_SecondDialogWaitsForFirst;
```
```pascal
procedure TDialogQueueManagerTests.SMOKE_RealShow_SecondDialogWaitsForFirst;
var
  LForm: TCommonCustomForm;
  LDialog: IDialogBuilder;
begin
  // Usa a factory REAL registrada por MultiDialog4FMX.FMX (nao a fake do Setup) —
  // precisa desregistrar o RegisterInstanceFactory do fake antes.
  LForm := TCommonCustomForm.Create(nil);
  try
    LDialog := TFMXDialog.Create;
    LDialog.SetMessage('1').Buttons.AddButton('OK').&End.Show(LForm);
    LDialog := TFMXDialog.Create;
    LDialog.SetMessage('2').Buttons.AddButton('OK').&End.Show(LForm);

    Assert.AreEqual(1, LForm.ChildrenCount,
      'So o overlay do primeiro dialogo deve existir enquanto ele nao fecha');
  finally
    LForm.Free;
  end;
end;
```
Run `Tests/build_tests.bat`, confirm it passes, **then delete this test** (its `Setup` re-registers the fake factory for every other test in the fixture via `RegisterInstanceFactory`, which is a global class var — leaving a test that depends on real-factory-vs-fake-factory ordering would make the fixture order-dependent, which DUnitX doesn't guarantee). This step exists purely to give you direct evidence the end-to-end wiring works before opening the PR — not to leave a lasting regression test for it.

- [ ] **Step 4: Final full run after removing the smoke test**

```bash
Tests/build_tests.bat
```
Expected: back to `Tests Passed : 55`, `Tests Failed : 0`, `Tests Errored : 0`.

- [ ] **Step 5: Push and open the PR to `develop`**

```bash
git push -u origin feature/sprint6-snapshot-queue
gh pr create --base develop --title "Sprint 6 fundacional: Snapshot + Fila FIFO por form + seguranca na destruicao" --body "$(cat <<'EOF'
## Summary
- TDialogSnapshot: copia imutavel da config + copia profunda dos botoes, tirada no Show.
- TDialogQueueManager: fila FIFO por form (um dialogo visivel por vez); FreeNotification + guarda FAlive para destruicao segura do form.
- TFMXDialog vira config-only; TFMXDialogInstance (novo) concentra toda a logica visual, lida a partir do snapshot.
- API publica (IDialogBuilder/IDialogButtonsBuilder) inalterada; nenhum sample precisou mudar.

Spec: docs/superpowers/specs/2026-07-17-sprint6-fundacional-design.md
Plan: docs/superpowers/plans/2026-07-17-sprint6-fundacional-implementation.md

## Test plan
- [x] 52 testes originais continuam passando
- [x] 3 testes novos de TDialogSnapshot
- [x] 4 testes novos de TDialogQueueManager (incluindo o teste-chave de destruicao do form)
- [x] Tests.Android.pas migrado (25 -> 19 testes; 3 movidos para Tests.Builder.pas, 2 ja cobertos por ResolveParentForm existente)
- [x] Build limpo: COMPILE_OK, Tests Passed : 55, Failed : 0, Errored : 0, Leaked : 0
EOF
)"
```

---

## Self-Review

**Spec coverage:** (e) Snapshot → Task 3. (a) Fila FIFO → Task 4. (d) Segurança na destruição do form → Task 4 (`Notification`/purge) + Task 6 (`FAlive` guards in `LDoDestroy` and the two `ForceQueue` entrance-animation closures). `TDialogBase.Show` rewrite → Task 5. `TFMXDialogInstance` split → Task 6. Testes ajustados/novos → Tasks 3, 4, 5, 7, 8. API pública inalterada → verified in Task 6 (`TFMXDialog` unchanged signature-wise) and never touched in `Interfaces.pas` except the internal `TButtonHandler` relocation (Task 2), which is not part of the public interface types.

**Placeholder scan:** no TBD/"add validation"/"similar to Task N" left — every step shows the literal code, including the two corrections found while drafting Task 4 (the `TQueue<T>` non-owning purge leak, and the invalid `Assert.IsTrue` production-code slip).

**Type consistency:** `TDialogSnapshot.Create` signature is identical everywhere it's called (Tasks 4 test fakes, Task 5 `Base.pas`, Task 7 test migration) — `(AForm, ATitle, AMessage, AMsgType, ACancelable, AFontSize, ABorderRadius, AAnimation, ATheme, ACustomSVG, ACustomIconColor, AResultCallback, AButtons)`. `IDialogVisualInstance.Show`/`.Suppress` match between Task 4's declaration, Task 4's `TFakeDialogInstance`, and Task 6's `TFMXDialogInstance`. `TDialogQueueManager.Enqueue`/`NotifyClosed`/`DebugIsActive`/`DebugQueueLength` signatures match between Task 4's declaration and every consumer (Task 5, Task 6, Task 4's own tests).

**Gap found and fixed during self-review:** the original spec said `Tests.MemoryLeaks.pas` "adjusts to close via `NotifyClosed`" — re-reading the actual file (Task 8, Step 1) shows it never calls `.Show` at all, so there's nothing to adjust. Documented as a verification step instead of inventing a code change that isn't needed.
