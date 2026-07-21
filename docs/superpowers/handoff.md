# Handoff — MultiDialog4FMX

> Arquivo único de handoff do projeto. **Sempre atualizar este mesmo arquivo** (não criar um por sessão).
> Descreve *onde estamos e qual o próximo passo* — o *que mudou* está nos commits.

_Última atualização: 2026-07-21_

## Onde paramos

**Sprint 6 fundacional concluída e validada.** Branch `feature/sprint6-snapshot-queue`
(criada de `develop`). O objetivo da sprint — dar ao `TDialogBase.Show` uma fila FIFO
por form apoiada num snapshot imutável de config, e tornar a destruição do form segura,
sem mudar `IDialogBuilder`/`IDialogButtonsBuilder` nem os samples — está **implementado,
testado e validado manualmente no Windows e no Android**.

### O que foi entregue (Tasks 1–8 do plano)
- `TButtonHandler`/`TButtonHandlerList` movidos para `Interfaces.pas` (quebra dependência circular).
- `TDialogSnapshot` (`MultiDialog4FMX.Queue.pas`): cópia imutável da config + deep-copy dos botões, tirada no `Show`.
- `TDialogQueueManager` (`MultiDialog4FMX.Queue.pas`): fila FIFO por form (um diálogo visível por vez) + guarda de destruição do form via `FreeNotification`/`Notification` + purga da fila.
- `TFMXDialog` virou config-only; toda a lógica visual foi para `TFMXDialogInstance` (`MultiDialog4FMX.FMX.pas`), que lê do snapshot.
- Dois fixes de **use-after-free** no fim da sprint:
  - `af34a17` — destruição da instância/form com diálogo aberto ou enfileirado (guarda `FAlive`).
  - `e6b484b` (último commit) — fecha o UAF que sobrava nas animações de entrada `danFade`/`danSlide` (captura `LSelf` antes do `case`, testa `Assigned(LSelf)` dentro do closure `ForceQueue`).

### Validação
- **Regressão automatizada:** 87/87, 0 failed/errored/leaked (rodado em 2026-07-21).
- **Manual Windows (Sample):** FIFO (2º diálogo espera o 1º), animações (`danNone/danFade/danScale/danSlide`), e segurança na destruição do form (fechar/destruir form com diálogo aberto/enfileirado, inclusive no meio da animação) — todos OK.
- **Manual Android:** validado OK.

## Próximo passo concreto

1. **Push da branch + PR para `develop`** (é o fechamento do Task 8). Depois: PR `develop` → `master` → tag de release, conforme o workflow.
2. Iniciar a **Sprint 7 (lifecycle)**: close programático + async/await + telemetria (~10 dias). Ver roadmap em `memory/project_dialog4d_competitor.md`.

## Decisões tomadas (e porquê)
- **Fila FIFO em vez de sobrepor diálogos**: dois `Show()` no mesmo form não mostram dois overlays; o 2º espera o 1º fechar (decidido no brainstorm, ver spec).
- **Snapshot imutável**: desacopla a config do builder do estado visual vivo; mudar o builder depois do `Show` não afeta o diálogo já disparado.
- **Split config/instância**: `TFMXDialog` (config) vs `TFMXDialogInstance` (visual) — permite a fila guardar snapshots leves e criar/destruir instâncias visuais sob demanda.
- **`GlobalUseSkia` NÃO é dependência da lib**: `src/` usa só `TPath` nativo do FMX. Skia só aparecia como componente decorativo (`TSkSvg`) no StyleBook de um Sample; resolvido pelo usuário. **A biblioteca não depende de Skia.**

## Armadilhas / o que não fazer
- **Nunca commitar direto em `master` ou `develop`.** Sempre feature branch → PR develop → PR master → tag.
- Não commitar o lixo de teste na raiz de `Tests/` (dezenas de `*.txt`, `*.bat`, `dunitx-results.xml`) nem `.claude/`, `Errors/`, `.superpowers/` — são artefatos locais.
- Rodar os testes: o runner é **console** e precisa do flag de exit **com valor** — `-exit:Continue` (o `--exit` sem valor em `Tests/run_tests.bat` está desatualizado e falha com `ECommandLineError`).
- Os 87 testes rodam **headless** (instância fake, `danNone`): **não** cobrem o visual das animações nem a corrida real de destruição do form — isso só se valida rodando o Sample.

## Como rodar e validar
- **Compilar + testar (console, contagem confiável):**
  ```
  call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
  cd Tests
  dcc32 -B -Q -E".\Win32\Debug" -N".\Win32\Debug\dcu" MultiDialog4FMX.Tests.dpr
  .\Win32\Debug\MultiDialog4FMX.Tests.exe -exit:Continue
  ```
  Esperado: `Tests Passed : 87`, `Failed : 0`, `Errored : 0`, `Leaked : 0`.
- **Só compilar (msbuild):** `Tests/build_tests.bat` (gera o exe, mas como app GUI não imprime a contagem no console redirecionado — use o caminho dcc32 acima para ver os números).
- **Sample Windows:** `Samples/Delphi12.3/init/MultiDialog4FMX_Sample.dproj`.

## Branch e sincronização
- Branch: `feature/sprint6-snapshot-queue` (base `develop`).
- Estado no início desta sessão: 12 commits à frente de `origin/develop`, **sem upstream/push**, **sem PR**.
- Alvo: push + PR para `develop`.
