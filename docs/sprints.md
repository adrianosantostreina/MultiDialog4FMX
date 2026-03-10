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

## Sprint 2 — Curto Prazo 🔜 PRÓXIMA

| # | Item | Descrição |
|---|---|---|
| R6 | Refatorar `InternalShow` | Quebrar o God Method de ~275 linhas em métodos menores: `BuildOverlay`, `BuildHeader`, `BuildMessage`, `BuildButtons` |
| R7 | Eliminar `TButtonHandlerObj` | Unificar com `TButtonHandler` — duplicação dos campos `ClickHandler`, `TapHandler`, `AnonymousHandler` entre as duas classes |
| R8 | Expor configuração de layout | Permitir customização de cores, tamanho de fonte e border radius via API pública (`IDialogBuilder`) |
| R9 | DPI-awareness | Substituir valores fixos de pixels por cálculo relativo à densidade (`Screen.Scale`) |
| R10 | Testes de leak reais | Substituir os 5 testes `Assert.Pass` em `TMemoryLeakTests` por testes com rastreamento real de instâncias (FastMM4 ou contador de instâncias) |

---

## Sprint 3 — Médio Prazo

| # | Item | Descrição |
|---|---|---|
| R11 | Implementar iOS | Preencher o stub `MultiDialog4FMX.iOS.pas` (atualmente vazio) |
| R12 | Implementar Windows/macOS | `CreateDialog` lança exceção em plataformas não-Android; criar implementação Desktop |
| R13 | Ícone customizável | Permitir `SetIcon(TBitmap)` ou `SetSVG(string)` além dos 5 tipos predefinidos |
| R14 | `TDialogResultProc` — callback de resultado | Implementar callback tipado de resultado (era o propósito original da `Types.pas` removida na Sprint 1) |
| R15 | Remover arquivo órfão `Proj1` | `Samples/init/` contém dois `.dpr` — `Proj1` é sobreposição indevida; remover e auditar todos os samples |

---

## Sprint 4 — Estratégico

| # | Item | Descrição |
|---|---|---|
| R16 | Animações de entrada/saída | Fade-in/slide-up no overlay; fade-out ao fechar |
| R17 | Timeout de botão | `AddButton('OK', 5)` — fecha automaticamente após N segundos com contagem regressiva visível |
| R18 | Acessibilidade | `ContentDescription` nos controles para TalkBack (Android) e leitores de tela equivalentes |
| R19 | Internacionalização | Externalizar strings de erro para `resourcestring` (PT-BR e EN mínimo) |
| R20 | Publicação GetIt | Preparar o projeto para o GetIt Package Manager do RAD Studio |

---

## Notas de Arquitetura

- Fluent chain: `TMultiDialog4FMX.Dialog.SetTitle().SetMessage().Buttons.AddButton().&End.Show`
- Plataformas: Android (implementado), iOS (stub), Windows/macOS (exceção)
- Máximo de 4 botões; layout responsivo 3+1 em portrait com 4 botões
- `InternalShow` em `Android.pas` é o método central — alvo principal da Sprint 2 (R6)
- `TButtonHandlerObj` em `Android.pas` duplica `TButtonHandler` de `Base.pas` — alvo do R7
