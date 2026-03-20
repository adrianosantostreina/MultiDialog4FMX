unit MultiDialog4FMX.Tests.Buttons;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Tests.Mocks,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Base,
  FMX.Types,
  FMX.Graphics,
  System.Types,
  System.SysUtils,
  System.UITypes;

type
  [TestFixture]
  TButtonTests = class
  private
    FDialog: TMockDialogBase;
    FClickCalled: Boolean;
    FTapCalled: Boolean;
    FAnonymousCalled: Boolean;
    
    procedure OnTestClick(Sender: TObject);
    procedure OnTestTap(Sender: TObject; const Point: TPointF);
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestAddButton_WithNotifyEvent;
    
    [Test]
    procedure TestAddButton_WithAnonymousMethod;
    
    [Test]
    procedure TestAddButton_WithTapEvent;
    
    [Test]
    procedure TestAddButton_WithoutHandler;
    
    [Test]
    procedure TestAddButton_WithColor;
    
    [Test]
    procedure TestAddButton_WithStyleLookup;
    
    [Test]
    procedure TestAddButton_MaximumFourButtons;
    
    [Test]
    procedure TestAddButton_ExceedsMaximum_RaisesException;
    
    // [Test]
    // procedure TestShow_WithoutButtons_RaisesException;
    
    [Test]
    procedure TestButtonHandler_StoresCorrectData;

    [Test]
    procedure TestAddButton_ModalResult_DefaultIsOk;

    [Test]
    procedure TestAddButton_ModalResult_ExplicitCancel;

    [Test]
    procedure TestAddButton_NoHandler_ModalResult_Forwarded;

    [Test]
    procedure TestAddButton_AnonymousMethod_ModalResult;

    [Test]
    procedure TestAddButton_TapEvent_ModalResult;

    [Test]
    procedure TestAddButton_WithTimeout_StoresValue;

    [Test]
    procedure TestAddButton_WithTimeout_NoHandlers;

    [Test]
    procedure TestAddButton_WithTimeout_DefaultIsZero;
  end;

implementation

{ TButtonTests }

procedure TButtonTests.Setup;
begin
  FDialog := TMockDialogBase.Create;
  FClickCalled := False;
  FTapCalled := False;
  FAnonymousCalled := False;
end;

procedure TButtonTests.TearDown;
begin
  FDialog := nil;
end;

procedure TButtonTests.OnTestClick(Sender: TObject);
begin
  FClickCalled := True;
end;

procedure TButtonTests.OnTestTap(Sender: TObject; const Point: TPointF);
begin
  FTapCalled := True;
end;

procedure TButtonTests.TestAddButton_WithNotifyEvent;
begin
  FDialog.Buttons.AddButton('Test', OnTestClick);
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual('Test', FDialog.ButtonHandlers[0].Text);
  Assert.IsTrue(Assigned(FDialog.ButtonHandlers[0].ClickHandler), 'ClickHandler should be assigned');
end;

procedure TButtonTests.TestAddButton_WithAnonymousMethod;
begin
  FDialog.Buttons.AddButton('Test', 
    procedure
    begin
      FAnonymousCalled := True;
    end);
    
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual('Test', FDialog.ButtonHandlers[0].Text);
  Assert.IsTrue(Assigned(FDialog.ButtonHandlers[0].AnonymousHandler), 'AnonymousHandler should be assigned');
  
  // Execute the anonymous method to verify it works
  FDialog.ButtonHandlers[0].AnonymousHandler();
  Assert.IsTrue(FAnonymousCalled);
end;

procedure TButtonTests.TestAddButton_WithTapEvent;
begin
  FDialog.Buttons.AddButton('Test', OnTestTap);
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual('Test', FDialog.ButtonHandlers[0].Text);
  Assert.IsTrue(Assigned(FDialog.ButtonHandlers[0].TapHandler), 'TapHandler should be assigned');
end;

procedure TButtonTests.TestAddButton_WithoutHandler;
begin
  FDialog.Buttons.AddButton('Test');
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual('Test', FDialog.ButtonHandlers[0].Text);
  Assert.IsFalse(Assigned(FDialog.ButtonHandlers[0].ClickHandler), 'ClickHandler should be nil');
  Assert.IsFalse(Assigned(FDialog.ButtonHandlers[0].TapHandler), 'TapHandler should be nil');
  Assert.IsFalse(Assigned(FDialog.ButtonHandlers[0].AnonymousHandler), 'AnonymousHandler should be nil');
end;

procedure TButtonTests.TestAddButton_WithColor;
begin
  FDialog.Buttons.AddButton('Test', TAlphaColorRec.Red);
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual(TAlphaColorRec.Red, FDialog.ButtonHandlers[0].Color);
end;

procedure TButtonTests.TestAddButton_WithStyleLookup;
begin
  FDialog.Buttons.AddButton('Test', TAlphaColorRec.Null, 'custom_style');
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual('custom_style', FDialog.ButtonHandlers[0].StyleLookup);
end;

procedure TButtonTests.TestAddButton_MaximumFourButtons;
begin
  FDialog.Buttons
    .AddButton('Button 1')
    .AddButton('Button 2')
    .AddButton('Button 3')
    .AddButton('Button 4');
    
  Assert.AreEqual(4, FDialog.ButtonHandlers.Count);
end;

procedure TButtonTests.TestAddButton_ExceedsMaximum_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FDialog.Buttons
        .AddButton('Button 1')
        .AddButton('Button 2')
        .AddButton('Button 3')
        .AddButton('Button 4')
        .AddButton('Button 5'); // Should raise exception
    end,
    Exception,
    'O diálogo suporta no máximo 4 botões.');
end;

{
procedure TButtonTests.TestShow_WithoutButtons_RaisesException;
begin
  // REMOVED: TMockDialogBase does not perform validation in InternalShow.
  // Use TAndroidDialogTests for validation logic testing.
end;
}

procedure TButtonTests.TestButtonHandler_StoresCorrectData;
var
  Handler: TButtonHandler;
begin
  FDialog.Buttons.AddButton('MyButton', OnTestClick, TAlphaColorRec.Blue, 'mystyle');
  
  Handler := FDialog.ButtonHandlers[0];
  Assert.AreEqual('MyButton', Handler.Text);
  Assert.IsTrue(Assigned(Handler.ClickHandler), 'ClickHandler should be assigned');
  Assert.AreEqual(TAlphaColorRec.Blue, Handler.Color);
  Assert.AreEqual('mystyle', Handler.StyleLookup);
end;

procedure TButtonTests.TestAddButton_ModalResult_DefaultIsOk;
begin
  FDialog.Buttons.AddButton('Test', OnTestClick);
  Assert.AreEqual(mrOk, FDialog.ButtonHandlers[0].ModalResult,
    'Default ModalResult deve ser mrOk');
end;

procedure TButtonTests.TestAddButton_ModalResult_ExplicitCancel;
begin
  FDialog.Buttons.AddButton('Test', OnTestClick,
    TAlphaColorRec.Null, '', mrCancel);
  Assert.AreEqual(mrCancel, FDialog.ButtonHandlers[0].ModalResult,
    'ModalResult deve ser mrCancel quando explicitamente definido');
end;

procedure TButtonTests.TestAddButton_NoHandler_ModalResult_Forwarded;
begin
  FDialog.Buttons.AddButton('Test', TAlphaColorRec.Null, '', mrCancel);
  Assert.AreEqual(mrCancel, FDialog.ButtonHandlers[0].ModalResult,
    'Overload sem handler deve repassar AModalResult');
end;

procedure TButtonTests.TestAddButton_AnonymousMethod_ModalResult;
begin
  FDialog.Buttons.AddButton('Test',
    procedure begin end,
    TAlphaColorRec.Null, '', mrCancel);
  Assert.AreEqual(mrCancel, FDialog.ButtonHandlers[0].ModalResult,
    'Overload TProc deve repassar AModalResult');
end;

procedure TButtonTests.TestAddButton_TapEvent_ModalResult;
begin
  FDialog.Buttons.AddButton('Test', OnTestTap,
    TAlphaColorRec.Null, '', mrCancel);
  Assert.AreEqual(mrCancel, FDialog.ButtonHandlers[0].ModalResult,
    'Overload TTapEvent deve repassar AModalResult');
end;

procedure TButtonTests.TestAddButton_WithTimeout_StoresValue;
begin
  FDialog.Buttons.AddButton('OK', 5);
  Assert.AreEqual(1, FDialog.ButtonHandlers.Count);
  Assert.AreEqual(5, FDialog.ButtonHandlers[0].Timeout,
    'Timeout deve ser armazenado como 5');
end;

procedure TButtonTests.TestAddButton_WithTimeout_NoHandlers;
var
  Handler: TButtonHandler;
begin
  FDialog.Buttons.AddButton('OK', 3);
  Handler := FDialog.ButtonHandlers[0];
  Assert.IsFalse(Assigned(Handler.ClickHandler),     'ClickHandler deve ser nil');
  Assert.IsFalse(Assigned(Handler.TapHandler),       'TapHandler deve ser nil');
  Assert.IsFalse(Assigned(Handler.AnonymousHandler), 'AnonymousHandler deve ser nil');
end;

procedure TButtonTests.TestAddButton_WithTimeout_DefaultIsZero;
begin
  FDialog.Buttons.AddButton('OK');
  Assert.AreEqual(0, FDialog.ButtonHandlers[0].Timeout,
    'Botão sem timeout deve ter Timeout = 0');
end;

initialization
  TDUnitX.RegisterTestFixture(TButtonTests);

end.
