# MultiDialog4FMX — Plano de Sprints

**Data:** 2026-03-10
**Versão atual:** v1.3.0
**Branch principal:** master

---

## Sprint 1 — Imediatas ✅ CONCLUÍDA (v1.3.0)

| # | Item | Status |
|---|---|---|
| R1 | Memory leak `TButtonHandlerObj` — `CloseDialog` não libera TagObjects dos botões restantes | ✅ |
| R2 | Memory leak `TButtonHandlerObj` — sem `try..finally` em `ButtonClick`/`ButtonTap` | ✅ |
| R3 | Validação indevida em `InternalShow` — rejeita 2+ botões sem handler | ✅ |
| R4 | Remove constante e comentários mortos (`C_MaxDialogHeight = 400`) | ✅ |
| R5 | GUIDs sequenciais em `IDialogBuilder` e `IDialogButtonsBuilder` — risco de colisão | ✅ |

**Extras entregues:**
- Remove `MultiDialog4FMX.Types` (código morto — `TDialogResultProc` nunca referenciada)
- Constante `C_MaxButtonsMsg` centralizada em `Base.pas` (corrige inconsistência de encoding)
- Remove bloco de resolução de formulário duplicado em `InternalShow`
- Guard para `FMessage` vazio antes de criar `TLabel`
- `TFont` gerenciado com `try..finally` em `CalculateMessageHeight`
- 40/40 testes passando (5 novos testes adicionados)

---

## Sprint 2 — Curto Prazo ✅ CONCLUÍDA (v1.4.0)

| # | Item | Descrição |
|---|---|---|
| R6 | ✅ Refatorar `InternalShow` | God Method (~287 lin.) → 6 sub-métodos + orquestrador de ~20 linhas |
| R7 | ✅ Eliminar `TButtonHandlerObj` | Unificado com `TButtonHandler` via campo `Overlay: TLayout` |
| R8 | ✅ Expor configuração de layout | `SetFontSize` e `SetBorderRadius` na `IDialogBuilder` |
| R9 | ✅ DPI-awareness | `GetPlatformScale` via `IFMXScreenService.GetScreenScale`; constantes `C_Base*` |
| R10 | ✅ Testes de leak reais | 5 testes reescritos com `TButtonHandler.FInstanceCount` delta |

---

## Sprint 3 — Médio Prazo

| # | Item | Descrição |
|---|---|---|
| R11 | Implementar iOS | Preencher o stub `MultiDialog4FMX.iOS.pas` (atualmente vazio) |
| R12 | Implementar Windows/macOS | `CreateDialog` lança exceção em plataformas não-Android; criar implementação Desktop |
| R13 | Ícone customizável | Permitir `SetIcon(TBitmap)` ou `SetSVG(string)` além dos 5 tipos predefinidos |
| R14 | `TDialogResultProc` — callback de resultado | Implementar callback tipado de resultado (era o propósito original da `Types.pas` removida na Sprint 1) |
| ✅ R15 | Remover arquivo órfão `Proj1` | `Samples/init/` contém dois `.dpr` — `Proj1` é sobreposição indevida; remover e auditar todos os samples |
| ✅ R16 | Refatoração profissional `TButtonHandler` | Substituir hotfix de dois blocos `public` (commit `1119074`) por campos privados com prefixo `F` + propriedades públicas. `FInstanceCount` como `class var` explícito em seção `private`. Arquivo: `src/MultiDialog4FMX.Base.pas`. Verificar `ButtonClick`, `ButtonTap`, `PaintColoredBtn` em `Android.pas`. Spec: `docs/REFATORACAO_PROFISSIONAL_BUTTONHANDLER.md` |

---

## Sprint 4 — Estratégico

| # | Item | Descrição |
|---|---|---|
| R17 | Animações de entrada/saída | Ver spec completa abaixo. Spec: `docs/PLANO_ANIMACOES_DIALOGO.md` |
| R18 | Timeout de botão | `AddButton('OK', 5)` — fecha automaticamente após N segundos com contagem regressiva visível |
| R19 | Acessibilidade | `ContentDescription` nos controles para TalkBack (Android) e leitores de tela equivalentes |
| R20 | Internacionalização | Externalizar strings de erro para `resourcestring` (PT-BR e EN mínimo) |
| R21 | Publicação GetIt | Preparar o projeto para o GetIt Package Manager do RAD Studio |

### R17 — Spec de Animações de Entrada/Saída

**API:**
```pascal
TMultiDialog4FMX.Dialog
  .SetAnimation(danScale) // danNone, danFade, danScale, danSlide
  .Show;
```

**`Interfaces.pas`:** Adicionar enum `TDialogAnimation = (danNone, danFade, danScale, danSlide)` e método `SetAnimation(AAnimation: TDialogAnimation): IDialogBuilder`

**`Base.pas`:** Adicionar campo `FAnimation: TDialogAnimation` e implementação de `SetAnimation`

**`Android.pas` — Entrada** (após criação do `LDialogRect`, antes de finalizar `InternalShow`):
- `danFade`: `TAnimator.AnimateFloat(LOverlay, 'Opacity', 1, 0.25)`
- `danScale`: inicializar `LDialogRect.Scale.X/Y` em 0.8; `TAnimator.AnimateFloat(LDialogRect, 'Scale.X', 1.0, 0.3, TAnimationType.Out, TInterpolationType.Back)` + idem `Scale.Y`
- `danSlide`: animar `Position.Y` de `-LDialogRect.Height` até posição final

**`Android.pas` — Saída** (modificar `CloseDialog`):
- Se `FAnimation <> danNone`, executar animação reversa antes de destruir overlay
- Usar callback `OnFinished` da animação para executar `AOverlay.DisposeOf` e `FKeepAlive := nil`
- Garantir que `FKeepAlive := nil` ocorra somente no callback (evita AV durante transição)

**Critérios de aceite:** sem flickering na abertura; `FKeepAlive` liberado apenas após conclusão da animação de saída; 0 novos leaks

---

## Notas de Arquitetura

- Fluent chain: `TMultiDialog4FMX.Dialog.SetTitle().SetMessage().Buttons.AddButton().&End.Show`
- Plataformas: Android (implementado), iOS (stub), Windows/macOS (exceção)
- Máximo de 4 botões; layout responsivo 3+1 em portrait com 4 botões
- `InternalShow` em `Android.pas` é o método central — alvo principal da Sprint 2 (R6)
- `TButtonHandlerObj` em `Android.pas` duplica `TButtonHandler` de `Base.pas` — alvo do R7
