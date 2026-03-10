# Release Notes - v1.2.0

## 🐛 Critical Bug Fixes (Correções Críticas)

- **[CRITICAL] Memory leak — TButtonHandlerObj not freed on overlay close**: `CloseDialog` now iterates all buttons in the `TFlowLayout` and frees every `TButtonHandlerObj` stored in `TagObject` before destroying the overlay. Previously, clicking one button left all other buttons' handlers leaked.
- **[CRITICAL] Memory leak — TButtonHandlerObj not freed when handler raises exception**: `ButtonClick` and `ButtonTap` now use `try..finally` to guarantee that `Obj.Free` and `CloseDialog` are always called, even if the event handler throws an exception.
- **[HIGH] Removed invalid validation for multiple buttons without handlers**: `InternalShow` no longer raises an exception when 2+ buttons have no event handler. Buttons without handlers are valid — they simply close the dialog. This unblocked the `AddButton('OK')` / `AddButton('Cancel')` usage pattern documented in the sample project.

## 🛠️ Improvements (Melhorias)

- **Removed dead code — `TDialogResultProc`**: The `MultiDialog4FMX.Types` unit contained only one type (`TDialogResultProc`) that was never referenced anywhere. The unit has been removed to eliminate dead code.
- **Fixed GUID collision risk**: `IDialogBuilder` and `IDialogButtonsBuilder` GUIDs were manually typed with a sequential pattern, risking collision. Replaced with fresh generated GUIDs.
- **Shared error message constant**: Introduced `C_MaxButtonsMsg` constant in `MultiDialog4FMX.Base` to ensure the "max 4 buttons" message is identical in both `Base.pas` and `Android.pas`, eliminating a previous encoding inconsistency.
- **Removed unused constant**: `C_MaxDialogHeight = 400` declared but never used in `Android.pas` — removed.
- **Duplicate form resolution removed**: Eliminated redundant form-resolution block in `InternalShow` that duplicated logic already handled by `ResolveParentForm`, and had incorrect priority order (MainForm before ActiveForm).
- **Guard for empty message**: `InternalShow` now skips creating the message `TLabel` when `FMessage` is empty, preventing an empty label from affecting layout height.
- **`TFont` managed with try..finally in `CalculateMessageHeight`**: Prevents a potential font object leak on exception.

## 🧪 Tests (Testes)

- Added `TAndroidDialogCloseTests` fixture with 3 new tests covering the memory leak fixes:
  - `TestButtonClick_CallsHandlerAndClearsTagObject`
  - `TestButtonClick_WhenHandlerRaises_OverlayIsStillFreed`
  - `TestCloseDialog_FreesAllRemainingTagObjects`
- Added `TestInternalShow_TwoButtonsNoHandler_NoValidationException` validating the removed handler validation.
- Added `TestCalculateMessageHeight_EmptyText` validating the empty-message guard.
- **All 40 tests pass** (40/40 — 0 failures, 0 errors).

---

# Release Notes - v1.1.0

## 🚀 New Features (Novas Funcionalidades)

- **Anonymous Methods Support**: Added overload to `AddButton` allowing inline `TProc` (anonymous procedures), simplifying event handling code.
- **FMX StyleLookup Support**: Added optional `StyleLookup` parameter to `AddButton`, allowing buttons to use specific styles from `TStyleBook`.
- **Cancelable Dialogs**: Introduced `.SetCancelable(True/False)` to control whether tapping the background overlay closes the dialog.
- **Responsive "3+1" Layout**: Automatic layout adjustment for smartphones in portrait mode when 4 buttons are used (3 in the first row, 1 full-width in the second).
- **Clean Button API**: Buttons can now be added with just text (`AddButton('OK')`) without passing explicit `nil` events. They automatically close the dialog.

## 🛠️ Fixes & Improvements (Correções e Melhorias)

- **Stability**:
  - Fixed application crash on startup caused by public/published visibility issues in FMX forms.
  - Fixed potential crash when `Application.MainForm` is nil (added fallback to `Screen.ActiveForm`).
- **Visuals**:
  - Fixed StyleLookup text color issue by correctly managing `StyledSettings`.
  - Fixed formatting/centering of single buttons in the dialog layout.
  - Fixed Unicode/Encoding issues in source files.
- **Logic**:
  - Resolved memory leaks in button handler creation logic.
  - Enforced business rule: Dialogs with 2+ buttons must have at least one event handler (exception added for safety).
  - Improved dialog height calculation for messages with empty titles.
- **Documentation**:
  - Added full XML Documentation Comments to `Interfaces` unit for better IDE IntelliSense support.

## 📦 Changes

- Updated `UMain` sample project to demonstrate all new features.
- Refactored `Android.pas` layout logic for better maintainability.
