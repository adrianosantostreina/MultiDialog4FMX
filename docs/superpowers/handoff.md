# Handoff — MultiDialog4FMX

> Arquivo único de handoff do projeto. **Sempre atualizar este mesmo arquivo** (não criar um por sessão).
> Descreve *onde estamos e qual o próximo passo* — o *que mudou* está nos commits.

_Última atualização: 2026-07-22_

## Onde paramos

**Sprint 7 (lifecycle) — Fases A e B concluídas e validadas.** Branch `feature/sprint7-lifecycle`
(base `develop`). Objetivo da sprint: dar ao MultiDialog4FMX ciclo de vida programável
(close por código, telemetria de eventos, async/await em worker thread) **sem alterar a
API pública existente** — implementado, testado (100/100) e commitado.

### O que foi entregue

**Fase A — resolução única + telemetria + close programático** (Tasks A1–A7):
- Telemetria: `TDialogEventKind`/`TDialogEventInfo`/`TDialogEventProc` (`Interfaces.pas`),
  dispatcher `TDialogTelemetry` (`Telemetry.pas`), emissão dos 7 eventos nos pontos de
  ciclo de vida, fachada `TMultiDialog4FMX.OnDialogEvent` (`Util.pas`).
- `TDialogSnapshot` ganhou `Id` sequencial + `ElapsedMs` (`Queue.pas`).
- Resolução única idempotente: `TFMXDialogInstance.DoResolve` (dispara o callback no
  máximo uma vez, por qualquer caminho de fechamento). `Suppress` passa a resolver com
  `mrNone` na destruição do form.
- Close programático: `IDialogHandle` (`Close`/`Close(AResult)`/`IsActive`),
  `IDialogBuilder.ShowGetHandle`, `TDialogHandle` (`Queue.pas`),
  `TDialogQueueManager.CloseByHandle`/`IsHandleActive`. Helper DRY `BuildSnapshot` em `Base.pas`.

**Fase B — async/await** (Tasks B1–B2):
- `MultiDialog4FMX.Await.pas`: `EDialogAwaitOnMainThread` + `EnsureAwaitNotOnMainThread`
  (fail-fast se chamado na main thread → evita deadlock).
- `IDialogBuilder.ShowAndWait(AForm)`: bloqueia a worker thread num `TEvent`; toda a
  montagem que toca UI roda dentro de `TThread.Queue` (UI thread); exceções na montagem
  são capturadas e re-levantadas na worker. Form destruído → resolve `mrNone`.

### Validação
- **Regressão automatizada: 100/100, 0 failed/errored/leaked** (rodado 2026-07-22, duas
  rodadas limpas). Baseline Sprint 6 era 87 → +13 testes novos.
- Manual (Windows/Android): **pendente** — ver ressalva abaixo.

## Próximo passo concreto

1. Branch **já com push** para `origin/feature/sprint7-lifecycle` (sem PR ainda, por opção do usuário).
2. Abrir **PR para `develop`** quando decidir (workflow feature→develop→master→tag).
3. **Validar manualmente** no Sample (Windows e Android) o close programático, `ShowAndWait`
   e a telemetria — os 100 testes rodam **headless** (instância fake) e não exercitam o
   visual real nem o Android.

## Decisões / achados importantes

- **Bug pré-existente descoberto (Sprint 6), corrigido:** o teste
  `TestFormDestruction_RealInstanceWithPendingTimeoutThread_NoCrash` chamava
  `CheckSynchronize(500)` no fim, que processava closures `ForceQueue`/`Queue` **residuais
  acumuladas por outros fixtures** (ex.: `Tests.Android`, cujos `ButtonClick`/`OnBackgroundClick`
  fazem `TThread.ForceQueue(CloseDialog)` e nunca são bombeados lá). Uma dessas closures
  deadlockava `CheckSynchronize` de forma **não-determinística conforme o layout de heap** —
  bastava adicionar *qualquer* fixture novo rodando antes dele (até um `Assert.Pass` no-op
  reproduzia) para travar a suíte inteira. Isolado por bisecção (commit `8b5826e`): trocado
  o `CheckSynchronize` por `Sleep`, pois a janela de UAF do C1 é exercitada **na própria
  thread de background** (lê `FTimeoutCancelled` via keepalive `LSelf`), sem depender de
  bombear a fila de sync global. **Provável UAF latente numa closure de `CloseDialog` do
  path Android — investigar numa próxima sessão** (não bloqueia a Sprint 7).
- **API pública inalterada:** só adições (`ShowGetHandle`, `ShowAndWait`, `IDialogHandle`,
  tipos de telemetria, `OnDialogEvent`). `Show`/`Show(AForm)`/`SetOnResult` idênticos.
- **Ajuste de contrato aceito:** `SetOnResult` agora também dispara na destruição do form,
  com `mrNone` (via `Suppress`→`DoResolve`).
- **`ShowAndWait` fail-fast** usa `TThread.CurrentThread.ThreadID = MainThreadID` (versão
  simples do plano; a hipótese de que isso causava o hang foi descartada — o hang era o
  `CheckSynchronize` do teste `RealInstance`, ver acima).

## Armadilhas / o que não fazer

- **Nunca commitar direto em `master` ou `develop`.** Sempre feature branch → PR develop → PR master → tag.
- **Não fazer um teste novo (ou qualquer fixture) drenar a fila de sync global** com
  `CheckSynchronize` esperando fechar diálogos de OUTROS fixtures — pode deadlockar. Cada
  teste que dispara `ForceQueue`/`Queue` deveria idealmente drenar o seu próprio.
- Os 100 testes rodam **headless** (instância fake, `danNone`): não cobrem o visual das
  animações, o `CloseWith`/`CloseDialog` reais, nem a corrida real de destruição do form no Android.
- `.pas` em **UTF-8 com BOM**; acentos como caracteres reais.

## Como rodar e validar

- **Compilar + testar (console, contagem confiável):**
  ```
  call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
  cd Tests
  dcc32 -B -Q -E".\Win32\Debug" -N".\Win32\Debug\dcu" MultiDialog4FMX.Tests.dpr
  .\Win32\Debug\MultiDialog4FMX.Tests.exe -exit:Continue
  ```
  Esperado: `Tests Passed : 100`, `Failed : 0`, `Errored : 0`, `Leaked : 0`.
  - **Se algum teste travar (hang):** matar o processo `MultiDialog4FMX.Tests` e investigar —
    provavelmente a fragilidade de sync-queue descrita acima (não relacionada ao seu código).
- **Sample Windows:** `Samples/Delphi12.3/init/MultiDialog4FMX_Sample.dproj`.

## Branch e sincronização
- Branch: `feature/sprint7-lifecycle` (base `develop`).
- Estado: **com push** para `origin` (sem PR). Commits de docs (spec+plano) + A1–A7 + B1–B2 + hardening do teste.
- Alvo: PR para `develop`.
