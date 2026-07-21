# Sprint 6 — Fundação (Snapshot + Fila FIFO + Segurança na destruição do form)

**Data:** 2026-07-17
**Status:** Aprovado, aguardando plano de implementação
**Origem:** Roadmap comparativo com o concorrente Dialog4D (`memory/project_dialog4d_competitor.md`). Brainstorm iniciado em sessão anterior (`docs/handoff-2026-07-07`), retomado e concluído nesta sessão.

## Objetivo

Entregar os 3 itens fundacionais do Sprint 6:
- **(e) Snapshot de config no request** — a config do builder é copiada no momento do `Show`, não lida ao vivo depois.
- **(a) Fila FIFO por form** — um diálogo visível por vez por form; o próximo da fila só aparece quando o atual fecha.
- **(d) Segurança na destruição do form** — nenhum use-after-free/thread órfã quando o form é destruído com diálogo(s) aberto(s) ou enfileirado(s).

**Restrição inegociável: API 100% transparente.** Nenhuma mudança em `IDialogBuilder`/`IDialogButtonsBuilder`. Nenhum sample ou código existente precisa mudar para compilar ou continuar funcionando. `Show` continua non-blocking e retornando imediatamente.

**Comportamento novo (decidido nesta sessão):** duas chamadas de `Show` seguidas no mesmo form não empilham mais diálogos visíveis simultâneos — a segunda espera a primeira fechar (fila real, não só rede de segurança).

## Descoberta de código (atualiza análises anteriores)

O "memory leak crítico de `TButtonHandlerObj`" do laudo técnico antigo **já foi resolvido** no refactor que unificou a antiga `Android.pas` em `MultiDialog4FMX.FMX.pas` (`ButtonClick`/`ButtonTap` têm `try..finally`, `TagObject` é nilado antes do handler rodar). O risco real que sobra, e que este Sprint ataca, é outro: `TFMXDialog` (hoje descendente de `TDialogBase`) mistura **config** (`FTitle`, `FAnimation`, ...) com **estado vivo de uma exibição** (`FKeepAlive`, `FDialogRect`, `FBtnLayout`, `FTimeoutButton`, `FTimeoutRemaining`, `FTimeoutCancelled`). Isso causa dois problemas:
1. Reusar o builder (chamar `Show` duas vezes na mesma instância) sobrescreve o estado vivo do diálogo anterior.
2. Destruir o form com o diálogo aberto deixa a thread de countdown (`StartTimeoutCountdown`) e os callbacks de animação (`ApplyExitAnimation`'s `AOnComplete`) com referências a objetos potencialmente já destruídos pelo teardown do form.

## Arquitetura

Quatro componentes — um novo par de unit, dois tipos existentes reorganizados:

```
TDialogBase (Base.pas, existente)          — puro config holder; Show() tira snapshot e enfileira
TDialogSnapshot (novo, Queue.pas)          — cópia imutável da config + cópia profunda dos botões
TFMXDialogInstance (era TFMXDialog, FMX.pas) — só a exibição visual; recebe um TDialogSnapshot
TDialogQueueManager (novo, Queue.pas)      — singleton; 1 fila FIFO por form + guarda de destruição
```

Fluxo em uma frase: `Show` não desenha mais nada diretamente — tira uma foto da config (`TDialogSnapshot`) e entrega pro `TDialogQueueManager`, que decide mostrar agora ou esperar a vez.

### `TDialogSnapshot`

```pascal
TDialogSnapshot = class
public
  Form: TCommonCustomForm;       // já resolvido (ResolveParentForm) no momento do Show
  Title, Message: string;
  MsgType: TMultiDialogType;
  Cancelable: Boolean;
  FontSize, BorderRadius: Single;
  Animation: TDialogAnimation;
  Theme: TDialogTheme;
  CustomSVG: string;
  CustomIconColor: TAlphaColor;
  ResultCallback: TDialogResultProc;
  Buttons: TButtonHandlerList;    // cópia profunda — nova lista, novos TButtonHandler
  constructor CreateFrom(const ASource: TDialogBase; const AForm: TCommonCustomForm);
  destructor Destroy; override;  // libera Buttons (OwnsObjects=True)
end;
```

- `CreateFrom` cria um `TButtonHandler` novo por botão do builder original, copiando `Text`, `ClickHandler`, `TapHandler`, `AnonymousHandler`, `Color`, `StyleLookup`, `ModalResult`, `Timeout` (não copia `Overlay`, que é estado visual — começa `nil`). É essa cópia profunda que resolve "reusar o builder": depois do `Show`, mexer no builder original ou chamar `Show` de novo não afeta o snapshot já enfileirado/em exibição.
- `ResultCallback` é `reference to procedure` — cópia rasa (aponta pro mesmo closure), é o comportamento esperado.
- Dono do snapshot: a fila enquanto está esperando; a `TFMXDialogInstance` quando vira o diálogo ativo (destrói o snapshot junto consigo ao fechar).

### `TFMXDialogInstance` (renomeia `TFMXDialog`, deixa de herdar `TDialogBase`)

Recebe o `TDialogSnapshot` no construtor e guarda como campo `FSnapshot` (dono). Toda a lógica visual que já existe hoje (`BuildOverlay`, `BuildDialogRect`, `BuildHeader`, `BuildBody`, `BuildButtons`, `CalculateFinalHeight`, animações de entrada/saída, timeout, `ButtonClick`/`ButtonTap`, `PaintColoredBtn`, `CloseDialog`) é **realocada, não reescrita** — troca mecânica de `F<Campo>` herdado por `FSnapshot.<Campo>`. `Show` (método próprio da instância, sem relação com `IDialogBuilder`) executa o que hoje é `InternalShow`, menos as validações de min/max botões (que sobem pro `TDialogBase.Show`, ver abaixo).

Ganha um campo `FAlive: Boolean` (`True` no construtor) — ver mecanismo de segurança abaixo.

`CloseDialog` continua igual (nila `TagObject`s, roda animação de saída, libera o overlay); ganha uma linha no fim do `LDoDestroy`: `TDialogQueueManager.Instance.NotifyClosed(FSnapshot.Form)`.

### `TDialogQueueManager`

```pascal
TDialogQueueManager = class(TComponent)   // TComponent só para poder receber FreeNotification
private
  class var FInstance: TDialogQueueManager;
  FQueues: TObjectDictionary<TCommonCustomForm, TQueue<TDialogSnapshot>>;
  FActive: TDictionary<TCommonCustomForm, TFMXDialogInstance>;
  procedure ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
protected
  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
public
  class function Instance: TDialogQueueManager;
  procedure Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
  procedure NotifyClosed(const AForm: TCommonCustomForm);
end;
```

**`Enqueue`** (chamado pelo `TDialogBase.Show`, já com o snapshot pronto):
1. Garante `AForm.FreeNotification(Self)` — só na primeira vez que esse form aparece (checa se já é chave em `FQueues`/`FActive`).
2. Se `FActive` já tem instância pra esse form → só enfileira o snapshot na `TQueue<TDialogSnapshot>` daquele form (cria a fila se não existir).
3. Senão → `ShowNow` direto: cria a `TFMXDialogInstance`, guarda em `FActive[AForm]`, chama `.Show`.

**`NotifyClosed`** (a instância chama isso do próprio `CloseDialog`, ao iniciar a destruição):
1. Remove `AForm` de `FActive`.
2. Se a fila daquele form tem algo → tira o próximo snapshot e `ShowNow` nele (próximo diálogo aparece sozinho).
3. Se está vazia, não faz nada (fila continua registrada, vazia).

**Segurança na destruição do form (item d) — guarda `FAlive`, não dependência de ordem de destruição:**

Em vez de confiar na ordem exata em que o FMX destrói componentes filhos (frágil entre versões do Delphi), a defesa é um flag checado em **toda closure adiada**:
- Toda closure que roda depois de `TThread.Queue`/`TThread.ForceQueue`/callback de animação (`StartTimeoutCountdown`, `AutoClickTimeoutButton`, `ApplyExitAnimation`'s `AOnComplete`) começa com `if not FAlive then Exit;`, antes de tocar em qualquer `TFmxObject`.
- `Notification(AComponent, opRemove)`: quando `AComponent` é um form presente em `FActive`/`FQueues`:
  - marca `FActive[AComponent].FAlive := False` (se houver instância ativa) — **não tenta `Free`/`DisposeOf`** nada nela (o form já destrói seus próprios componentes filhos; duplicar liberaria memória duas vezes); só impede qualquer callback futuro de tocar em objetos potencialmente já liberados.
  - libera silenciosamente (sem chamar `ResultCallback`) os snapshots que só estavam na fila — nunca chegaram a aparecer, não há resultado "real" a reportar.
  - remove as entradas desse form de `FActive`/`FQueues`.

Essa proteção não depende de quando exatamente o `Notification` chega em relação à destruição dos componentes filhos — qualquer código adiado que ainda não rodou vai checar `FAlive` e desistir antes de tocar em memória potencialmente liberada.

## `TDialogBase.Show` — antes/depois

Antes:
```pascal
function TDialogBase.Show: IDialogBuilder;
begin
  InternalShow(ResolveParentForm(nil));
  Result := Self;
end;
```

Depois:
```pascal
function TDialogBase.Show: IDialogBuilder;
var
  LForm: TCommonCustomForm;
  LSnapshot: TDialogSnapshot;
begin
  LForm := ResolveParentForm(nil);
  if not Assigned(LForm) then
    raise Exception.Create('Nenhum formulário disponível para exibir o diálogo.');
  if FButtonHandlers.Count < 1 then
    raise Exception.Create('O número mínimo de botões é 1.');
  if FButtonHandlers.Count > 4 then
    raise Exception.Create(C_MaxButtonsMsg);

  LSnapshot := TDialogSnapshot.CreateFrom(Self, LForm);
  EnqueueSnapshot(LForm, LSnapshot);
  Result := Self;
end;
```

As 3 validações (form nulo, min/max botões) sobem do atual `InternalShow` para cá — continuam síncronas e lançam exceção na hora do `Show`, igual hoje (erro de uso ainda estoura na chamada, não silenciosamente numa fila).

`EnqueueSnapshot` é um método protegido **virtual** (não abstrato) em `TDialogBase`:
```pascal
procedure EnqueueSnapshot(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot); virtual;
// implementação default: TDialogQueueManager.Instance.Enqueue(AForm, ASnapshot);
```
Esse é o seam de testabilidade que substitui o antigo `InternalShow` abstrato (ver Testes).

`TDesktopDialog`/`Factory.pas` não mudam de forma nenhuma — `TDesktopDialog = class(TFMXDialog)` continua existindo; `TFMXDialog` (implementa `IDialogBuilder`, só config) fica mais fina do que hoje, já que perde todo o estado/lógica visual pra `TFMXDialogInstance`.

## Testes

**Ajuste em fixtures existentes (não reescrita):**
- `Tests.Mocks.pas` (`TMockDialogBase`) — troca `override InternalShow` por `override EnqueueSnapshot`, capturando `ShowCalled`/`LastParentForm` do mesmo jeito, sem tocar na fila real nem em UI.
- `Tests.Builder.pas` — `TestShow_CallsInternalShow` renomeia para `TestShow_EnqueuesSnapshot`, mesmo comportamento verificado.
- `Tests.MemoryLeaks.pas` — os testes que contam `TButtonHandler.FInstanceCount` ao redor de show+fechamento continuam válidos; passam a fechar via `NotifyClosed` no fim do fluxo real (o objeto que existe/morre é o mesmo, só mudou de dono).

**Fixtures novas:**
- `Tests.Snapshot.pas`:
  - `TestCreateFrom_CopiesAllConfigFields`
  - `TestCreateFrom_DeepCopiesButtons` — muda o builder original *depois* de tirar o snapshot, confirma que o snapshot não é afetado (prova a correção do "reusar o builder")
  - `TestDestroy_FreesOwnButtonList`
- `Tests.Queue.pas` (usa forms reais via `TForm.Create(nil)`, já provado viável pelas fixtures `Desktop`/`Android` existentes):
  - `TestEnqueue_FirstCall_ShowsImmediately`
  - `TestEnqueue_SecondCall_SameForm_WaitsInQueue`
  - `TestNotifyClosed_PopsNextFromQueue`
  - `TestFormDestruction_PurgesQueueAndDeactivatesInstance` — teste-chave do item (d): cria form, enfileira 2 snapshots, libera o form, confirma `FAlive = False` na instância que estava ativa e fila vazia, sem exception.

## Fora de escopo (fica para Sprints 7-9)

- Close programático (`CloseDialog` chamável de fora) — Sprint 7, item (b).
- Async/await — Sprint 7, item (c).
- Telemetria estruturada — Sprint 7, item (g).
- Botões semânticos, i18n, adapter `FMX.DialogService` — Sprint 8.
- iOS real, Windows/macOS real, testes DUnitX reais no Dialog4D-equivalente — Sprint 9.

## Diferenciais do MultiDialog4FMX preservados

Nenhum destes muda de comportamento com este Sprint: Fluent Builder, ícones SVG inline por tipo, `SetIcon`/`SetIconColor` customizados, timeout de botão, múltiplos tipos de handler por botão, animações (Fade/Scale/Slide), tema claro/escuro/auto, `StyleLookup`, layout responsivo 3+1.
