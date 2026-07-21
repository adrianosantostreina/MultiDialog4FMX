# Sprint 7 (lifecycle) — Design

> Spec de design da Sprint 7. Descreve **o que** vamos construir e **por quê**; o passo-a-passo
> de implementação vai no plano (writing-plans).

_Data: 2026-07-21 · Branch: `feature/sprint7-lifecycle` (base `develop`)_

## Objetivo

Dar ao MultiDialog4FMX o **ciclo de vida programável** que o concorrente Dialog4D oferece,
mantendo a identidade do projeto (fluent builder + foco visual). Três features do roadmap
(ver `memory/project_dialog4d_competitor.md`):

- **(b) Close programático** — fechar um diálogo por código, não só por clique.
- **(g) Telemetria** — observar os eventos de ciclo de vida do diálogo num ponto único.
- **(c) Async/await** — aguardar o resultado do diálogo de forma linear numa worker thread.

**Entrega em 2 fases, com checkpoint entre elas:**
- **Fase A** — Resolução única + Close programático + Telemetria (baixo risco, apoia-se na fila).
- **Fase B** — Async/await (a parte HARD: worker-thread + `TEvent` + marshalling).

## Princípios (invariantes da sprint)

- **API pública atual inalterada** — só adições. `Show`/`SetOnResult`/samples continuam funcionando.
- **Retrocompatível com Delphi ≤ 11** — `TEvent`, `TModalResult`, `System.Threading` existem em
  todas as versões alvo; guardas condicionais existentes preservadas.
- **Sem regressão** — os 87 testes da Sprint 6 continuam passando (com o ajuste descrito abaixo).
- **Emissão de telemetria com custo zero** quando ninguém escuta.

---

## Fundação — Resolução única

**Problema.** Hoje o `FResultCallback` (de `SetOnResult`) dispara em `ButtonClick`, `ButtonTap`,
`OnBackgroundClick` — **mas não** no caminho `Suppress`/`Notification` (form destruído com diálogo
aberto/enfileirado), que purga sem callback. Isso é inofensivo hoje, mas trava o `await`: se a worker
thread espera o resultado e o form é destruído, **nada sinaliza o desbloqueio**.

**Decisão.** Cada diálogo **resolve exatamente uma vez**, com um `TModalResult`, por qualquer caminho:

| Caminho | Resultado resolvido |
|---------|--------------------|
| Botão clicado | `Button.ModalResult` |
| Overlay (cancelável) | `mrCancel` |
| Timeout (countdown) | `ModalResult` do botão de countdown |
| `Handle.Close(R)` | `R` (default `mrCancel`) |
| **Form destruído / suppress** | **`mrNone`** (sentinel: "não resolvido pelo usuário") |

Esse ponto único é a cola das três features:
- **Telemetria** emite `Closed` (ou `Suppressed`) com o resultado.
- **Await** sinaliza o `TEvent` com o resultado (inclusive `mrNone` na destruição — nunca trava).
- **Close programático** é apenas mais um caminho que aciona a resolução.

**Impacto na compat.** O `SetOnResult` passa a disparar **também** na destruição do form, com `mrNone`.
Isso é uma *melhoria* (o app pode reagir à destruição), não uma quebra. Testes existentes que
assumiam "não dispara na destruição" serão ajustados para o novo contrato.

**Invariante:** resolve-once é idempotente — chamadas subsequentes a qualquer caminho de fechamento
viram no-op após a primeira resolução.

---

## Fase A.1 — Telemetria

### Tipos novos (`MultiDialog4FMX.Interfaces.pas`)

```pascal
TDialogEventKind = (dekEnqueued, dekShown, dekButtonClicked,
                    dekCancelled, dekTimedOut, dekClosed, dekSuppressed);

TDialogEventInfo = record
  Kind: TDialogEventKind;
  DialogType: TMultiDialogType;
  Title: string;
  Result: TModalResult;   // válido em dekButtonClicked/dekClosed; mrNone quando não aplica
  ElapsedMs: Int64;       // tempo desde dekEnqueued (TStopwatch por diálogo)
end;

TDialogEventProc = reference to procedure(const AInfo: TDialogEventInfo);
```

### Eventos e pontos de emissão

| Evento | Quando | Ponto no código |
|--------|--------|-----------------|
| `dekEnqueued` | entrou na fila | `TDialogQueueManager.Enqueue` |
| `dekShown` | virou visível | `ShowNow` / `TFMXDialogInstance.Show` |
| `dekButtonClicked` | botão tocado (com `ModalResult`) | `ButtonClick`/`ButtonTap` |
| `dekCancelled` | fechou pelo overlay | `OnBackgroundClick` |
| `dekTimedOut` | fechou por countdown | `AutoClickTimeoutButton` |
| `dekClosed` | terminou de fechar (resolução final, não-suppress) | resolução única |
| `dekSuppressed` | form destruído com diálogo aberto/enfileirado | `Suppress`/`Notification` |

### Dispatcher (`MultiDialog4FMX.Telemetry.pas`, nova unit)

`TDialogTelemetry` com `class property OnEvent: TDialogEventProc` + `class procedure Emit(...)`.
Emissão **sempre na UI thread** (todos os pontos acima já rodam nela). Guardada por
`if Assigned(OnEvent)` — custo zero quando ninguém escuta.

### Registro público (`MultiDialog4FMX.Util.pas`)

```pascal
TMultiDialog4FMX.OnDialogEvent := procedure(const AInfo: TDialogEventInfo)
  begin
    Log(AInfo.Kind, AInfo.Title, AInfo.Result);
  end;
```

---

## Fase A.2 — Close programático

### `IDialogHandle` (`MultiDialog4FMX.Interfaces.pas`)

```pascal
IDialogHandle = interface
  ['{GUID a gerar na implementação}']
  procedure Close; overload;                        // resolve mrCancel
  procedure Close(const AResult: TModalResult); overload;
  function IsActive: Boolean;                        // False após resolvido
end;
```

### Obtenção — método terminal novo (não altera `Show`)

`IDialogBuilder` / `TDialogBase` ganham:

```pascal
function ShowGetHandle(const AForm: TCommonCustomForm = nil): IDialogHandle;
```

### Mecanismo

- Cada `TDialogSnapshot` ganha um `Id` (token sequencial, atribuído no `Show`).
- O handle guarda `(Id, Form)` e delega a `TDialogQueueManager.CloseByHandle(Id, Result)`.
- `CloseByHandle` cobre os três estados:
  - **ainda na fila** → remove da fila e resolve (nunca aparece);
  - **visível** → aciona o close resolvendo com `Result`;
  - **já resolvido** → no-op (`IsActive` retorna `False`).
- `TFMXDialogInstance` passa a guardar seu `FOverlay` como campo e expõe um close programático que
  entra no **mesmo** caminho de resolução única.

---

## Fase B — Async/await

### API terminal (`MultiDialog4FMX.Await.pas`, nova unit)

```pascal
function ShowAndWait(const AForm: TCommonCustomForm = nil): TModalResult;
```

Uso pretendido — fluxo linear numa worker thread, sem callback:

```pascal
TTask.Run(procedure
var LResult: TModalResult;
begin
  LResult := TMultiDialog4FMX.Dialog
    .SetType(mdtQuestion).SetTitle('Confirmar').SetMessage('Excluir?')
    .Buttons.AddButton('Sim', TAlphaColorRec.Green, '', mrYes)
            .AddButton('Não', TAlphaColorRec.Red,   '', mrNo).&End
    .ShowAndWait(Form1);            // BLOQUEIA esta worker thread
  if LResult = mrYes then
    TThread.Synchronize(nil, ExcluirRegistro);
end);
```

### Implementação (simples graças à Resolução Única)

1. **Fail-fast:** se `TThread.CurrentThread.ThreadID = MainThreadID`, levanta
   `EDialogAwaitOnMainThread` com mensagem clara (bloquear na UI thread = deadlock).
2. Cria um `TEvent` (manual reset) e prepara um callback interno que captura o resultado e faz
   `SetEvent`, **encadeando** o `SetOnResult` do usuário se houver (chama o do usuário e depois sinaliza).
3. `TThread.Queue(nil, ...)` marshalla para a **UI thread** tudo que toca a UI: resolução do parent
   form (`ResolveParentForm` acessa `Screen.ActiveForm`/`Application.MainForm` — **inseguro fora da UI
   thread**), montagem do `TDialogSnapshot` e `Enqueue`. A worker thread **não** toca objetos FMX.
4. `WaitFor(INFINITE)` na worker → desbloqueia com o resultado; libera o `TEvent`; retorna.

### Casos de borda

- **Form destruído durante o await** → resolução única `mrNone` → callback interno sinaliza →
  worker retorna `mrNone`. Nunca trava.
- **Diálogo enfileirado atrás de outro** → a worker espera até chegar a vez e o usuário responder.
  Comportamento documentado (bloqueio é o contrato do await).

---

## Estratégia de testes (DUnitX, padrão headless da Sprint 6)

- **Telemetria:** registra callback, dispara diálogo fake (`danNone`), verifica a sequência
  `dekEnqueued → dekShown → dekButtonClicked → dekClosed` e o payload (`DialogType`, `Title`, `Result`).
- **Close:** `Close` resolve `mrCancel` e zera `IsActive`; `Close` de um enfileirado o remove da fila
  (verifica `DebugQueueLength`); double-`Close` é no-op.
- **Resolução única:** form destruído → callback recebe `mrNone` + telemetria `dekSuppressed`.
- **Await:** em `TTask.Run`, resolve na UI thread e retorna o valor; fail-fast na main thread levanta
  `EDialogAwaitOnMainThread`. **Guard de timeout** em cada teste de threading para a suíte nunca travar.
- **Regressão:** 87 testes existentes continuam passando; ajustar os que assumiam que `SetOnResult`
  não dispara na destruição do form.

---

## Artefatos (resumo)

| Arquivo | Mudança |
|---------|---------|
| `MultiDialog4FMX.Interfaces.pas` | + `TDialogEventKind`, `TDialogEventInfo`, `TDialogEventProc`, `IDialogHandle`; `IDialogBuilder` ganha `ShowGetHandle` + `ShowAndWait` |
| `MultiDialog4FMX.Telemetry.pas` | **nova** — dispatcher global |
| `MultiDialog4FMX.Await.pas` | **nova** — lógica `TEvent`/worker + `EDialogAwaitOnMainThread` |
| `MultiDialog4FMX.Queue.pas` | token por snapshot, resolução única, `CloseByHandle`, emissão de telemetria |
| `MultiDialog4FMX.FMX.pas` | `FOverlay` como campo, close programático, resolução única, emissão |
| `MultiDialog4FMX.Base.pas` | `ShowGetHandle`/`ShowAndWait` terminais montando o snapshot |
| `MultiDialog4FMX.Util.pas` | fachada `TMultiDialog4FMX.OnDialogEvent` |

## Decisões (e porquê)

- **Resolução única com `mrNone` na destruição** — fundação comum; garante que o await nunca trava e
  que a telemetria/callback cobrem todos os caminhos de fechamento. (aprovado)
- **Handle com token** em vez de close por form — preciso (alcança inclusive enfileirados) e serve de
  base para o await. (aprovado)
- **Callback anônimo global** para telemetria — leve, ergonômico, testável; alinhado ao estilo fluent.
  (aprovado)
- **Fail-fast na main thread** no await — transforma deadlock silencioso em erro diagnosticável. (aprovado)

## Fora de escopo (Sprint 7)

- Botões semânticos, i18n, adapter FMX.DialogService (Sprint 8).
- iOS/Windows/macOS reais (Sprint 9).
- Multi-observer de telemetria (YAGNI — um callback global cobre logging/diagnóstico).
