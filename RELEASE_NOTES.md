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
