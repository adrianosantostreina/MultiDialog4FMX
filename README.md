# MultiDialog4FMX

**MultiDialog4FMX** is a lightweight, fluent, and extensible library for creating custom dialogs in **Delphi FireMonkey (FMX)** applications.
It provides a simple, chainable API to display dialogs with titles, messages, and up to **4 interactive buttons**, maintaining visual consistency and ease of use across platforms.

---

## ✨ Key Features

- **Fluent Builder Pattern** – Create dialogs in a single chain of commands.
- **Cross-Platform** – Optimized for Android, with iOS and Windows support adaptable.
- **Responsive Layout** – Automatically adjusts for Portrait/Landscape and different screen sizes (e.g., 3+1 button layout on smartphones).
- **Clean API** – Support for **Anonymous Methods**, **StyleLookup**, and **Cancelable** dialogs.
- **Up to 4 Buttons** – Flexible configuration with `OnClick` or `OnTap` events.
- **Visual Customization** – Full integration with FMX `TStyleBook`.

---

## 🚀 Usage Examples

### 1. Basic Alert
A simple dialog with a single "OK" button. The button automatically closes the dialog.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Success')
  .SetMessage('Operation completed successfully.')
  .SetCancelable(True) // Allows closing by clicking the background
  .Buttons
    .AddButton('OK')
  .&End
  .Show;
```

### 2. Confirmation Dialog
Standard Yes/No dialog with event handlers.

```delphi
procedure TFormMain.OnConfirmClick(Sender: TObject);
begin
  // Handle Yes
end;

// Usage
TMultiDialog4FMX.Dialog
  .SetTitle('Delete Item')
  .SetMessage('Are you sure you want to delete this specific item?')
  .Buttons
    .AddButton('Yes', OnConfirmClick)
    .AddButton('No') // No handler = Close dialog
  .&End
  .Show;
```

### 3. Using Anonymous Methods
Keep your code clean by defining logic inline.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Quick Action')
  .SetMessage('Choose an action to perform:')
  .Buttons
    .AddButton('Execute', 
      procedure 
      begin
        Log('Action executed via anonymous method!');
      end
    )
    .AddButton('Cancel')
  .&End
  .Show;
```

### 4. Custom Styling (FMX Styles)
Apply specific FMX Styles to buttons using `StyleLookup`.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Visual Warning')
  .SetMessage('This action uses a custom danger style.')
  .Buttons
    .AddButton('Delete', TAlphaColorRec.Null, 'dangerbutton_style') // Style name from StyleBook
    .AddButton('Cancel', TAlphaColorRec.Null, 'transparent_style')
  .&End
  .Show;
```

### 5. Responsive Layout (3+1)
On smartphones in portrait mode, adding 4 buttons automatically triggers a responsive layout (3 buttons on top, 1 full-width button below).

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Rate Us')
  .SetMessage('How was your experience?')
  .Buttons
    .AddButton('Bad', TAlphaColorRec.Red)
    .AddButton('Good', TAlphaColorRec.Orange)
    .AddButton('Great', TAlphaColorRec.Green)
    .AddButton('Skip Rating', TAlphaColorRec.LightGray) // Will appear full-width at the bottom
  .&End
  .Show;
```

---

## 📌 Installation

1. Add the Global search path to the `src` folder.
2. Add `MultiDialog4FMX.Util` to your `uses` clause in the form where you want to call the dialog.

## 🧪 Testing

To run the unit tests:
1. Open `Tests\MultiDialog4FMX.Tests.dproj` in Delphi.
2. Run using **TestInsight** (View > TestInsight) or simply Compile and Run (F9) to view the console output.
3. See [walkthrough.md](Tests/walkthrough.md) for detailed testing documentation.

---

## 📜 License

This project is distributed under the **MIT** license.

---

[🇧🇷 Leia esta documentação em Português (Read in Portuguese)](README-ptBR.md)
