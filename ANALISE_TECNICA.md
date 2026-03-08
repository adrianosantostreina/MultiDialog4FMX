# MultiDialog4FMX - Project Memory
## Overview
Delphi FireMonkey (FMX) library for custom dialogs using Fluent Builder Pattern.
- Language: Delphi / Object Pascal
- Platform: Android (primary), iOS (planned), Windows (planned)
- Branch workflow: master (stable), develop, feature branches

## Key Files (src/)
- `MultiDialog4FMX.Util.pas` — Entry point: `TMultiDialog4FMX.Dialog` → returns `IDialogBuilder`
- `MultiDialog4FMX.Interfaces.pas` — Interfaces: `IDialogBuilder`, `IDialogButtonsBuilder`; enum `TMultiDialogType`
- `MultiDialog4FMX.Base.pas` — Base classes: `TDialogBase`, `TDialogButtonsBuilder`, `TButtonHandler`
- `MultiDialog4FMX.Android.pas` — Android implementation: `TAndroidDialog.InternalShow` (builds FMX layout at runtime)
- `MultiDialog4FMX.Factory.pas` — Platform factory: `CreateDialog` (uses `{$IFDEF ANDROID}`)
- `MultiDialog4FMX.Types.pas` — `TDialogResultProc` type
- `MultiDialog4FMX.iOS.pas` — iOS stub (not implemented)

## Architecture
- Fluent chain: `TMultiDialog4FMX.Dialog.SetTitle().SetMessage().Buttons.AddButton().&End.Show`
- `TDialogBase` holds state (FTitle, FMessage, FCancelable, FButtonHandlers, FMsgType)
- `TDialogButtonsBuilder` is nested builder returning to parent via `&End`
- `InternalShow` builds entire FMX layout dynamically at runtime (overlay + dialog rect + scrollbox + buttons)
- Max 4 buttons; 3+1 responsive layout for smartphone portrait with 4 buttons

## Dialog Types (TMultiDialogType)
- `mdtCustom` (no icon), `mdtWarning` (gold), `mdtError` (red), `mdtInformation` (dodgerblue)
- `mdtQuestion` (limegreen), `mdtConfirmation` (limegreen / checkmark SVG)

## SVG Icons (inline in Android.pas)
Paths defined as constants: SVG_WARNING, SVG_ERROR, SVG_INFO, SVG_QUESTION, SVG_SUCCESS
Also has src/icons/ folder with .svg files (error, info, question, warning)

## Button Events
- `TNotifyEvent` (OnClick), `TTapEvent` (OnTap), `TProc` (anonymous), or nil (close only)
- Handler stored in `TButtonHandlerObj` attached via `TagObject` on TButton

## Current Branch
`icones` — Recent work on Question and Confirmation icons adjustments

## Technical Analysis (2026-03-07)
Full report saved at: `D:\2.2 GitHub Adriano Santos\MultiDialog4FMX\LAUDO_TECNICO.md`
Score: 🟡 REGULAR — 3,4/5,0

### Known Bugs (confirmed against source)
- **CRITICAL — Memory leak TButtonHandlerObj**: FMX does NOT free `TagObject` when destroying controls. Three leak scenarios:
  1. `ButtonClick`/`ButtonTap`: if handler throws exception, `Obj.Free` and `CloseDialog` never run (no try..finally) — Android.pas L415/L435
  2. `OnBackgroundClick`: `CloseDialog` calls `DisposeOf(LOverlay)` but all `TButtonHandlerObj` in other buttons' `TagObject` are leaked — Android.pas L397
  3. Clicking one button leaks all others' `TButtonHandlerObj`
- **HIGH — Validation vs sample bug**: `InternalShow` raises exception if 2+ buttons have no handler (Android.pas L108-122), but `OnTestNullClick` sample does exactly that (UMain.pas L345-356)
- **HIGH — Duplicate form resolution**: `ResolveParentForm` called at L89, then redundant checks at L99-104 in wrong order (MainForm before ActiveForm, opposite of what ResolveParentForm does)

### Known Issues (non-bugs)
- `TButtonHandlerObj` duplicates `TButtonHandler` fields (ClickHandler, TapHandler, AnonymousHandler)
- `C_MaxDialogHeight = 400` constant declared but never used (Android.pas L57)
- `TDialogResultProc` type defined but never referenced (Types.pas L9)
- `InternalShow` is a 275-line God Method (Android.pas L47-363)
- GUIDs are manually typed with sequential pattern — risk of collision
- Two sample .dpr files in Samples/init/ (Proj1 is leftover)
- Memory leak tests use `Assert.Pass` without FastMM4 — placebo tests
- Inconsistent exception messages for same max-buttons rule (Base.pas L149 vs Android.pas L96)
