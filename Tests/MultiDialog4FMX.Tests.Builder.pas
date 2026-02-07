unit MultiDialog4FMX.Tests.Builder;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Tests.Mocks,
  MultiDialog4FMX.Interfaces,
  System.SysUtils;

type
  [TestFixture]
  TDialogBuilderTests = class
  private
    FDialog: TMockDialogBase;
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestSetTitle_StoresValue;
    
    [Test]
    procedure TestSetMessage_StoresValue;
    
    [Test]
    procedure TestSetCancelable_True;
    
    [Test]
    procedure TestSetCancelable_False;
    
    [Test]
    procedure TestFluentChaining_Multiple;
    
    [Test]
    procedure TestButtons_ReturnsButtonsBuilder;
    
    [Test]
    procedure TestButtonsEnd_ReturnsDialogBuilder;
    
    [Test]
    procedure TestShow_CallsInternalShow;
    
    [Test]
    procedure TestShow_ReturnsDialogBuilder;
  end;

implementation

{ TDialogBuilderTests }

procedure TDialogBuilderTests.Setup;
begin
  FDialog := TMockDialogBase.Create;
end;

procedure TDialogBuilderTests.TearDown;
begin
  FDialog := nil; // Interface reference counting will free it
end;

procedure TDialogBuilderTests.TestSetTitle_StoresValue;
begin
  FDialog.SetTitle('Test Title');
  Assert.AreEqual('Test Title', FDialog.Title);
end;

procedure TDialogBuilderTests.TestSetMessage_StoresValue;
begin
  FDialog.SetMessage('Test Message');
  Assert.AreEqual('Test Message', FDialog.Message);
end;

procedure TDialogBuilderTests.TestSetCancelable_True;
begin
  FDialog.SetCancelable(True);
  Assert.IsTrue(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestSetCancelable_False;
begin
  FDialog.SetCancelable(False);
  Assert.IsFalse(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestFluentChaining_Multiple;
var
  Result: IDialogBuilder;
begin
  Result := FDialog
    .SetTitle('Title')
    .SetMessage('Message')
    .SetCancelable(True);
    
  Assert.IsNotNull(Result);
  Assert.AreEqual('Title', FDialog.Title);
  Assert.AreEqual('Message', FDialog.Message);
  Assert.IsTrue(FDialog.Cancelable);
end;

procedure TDialogBuilderTests.TestButtons_ReturnsButtonsBuilder;
var
  ButtonsBuilder: IDialogButtonsBuilder;
begin
  ButtonsBuilder := FDialog.Buttons;
  Assert.IsNotNull(ButtonsBuilder);
end;

procedure TDialogBuilderTests.TestButtonsEnd_ReturnsDialogBuilder;
var
  DialogBuilder: IDialogBuilder;
begin
  DialogBuilder := FDialog.Buttons.&End;
  Assert.IsNotNull(DialogBuilder);
end;

procedure TDialogBuilderTests.TestShow_CallsInternalShow;
begin
  FDialog.Reset;
  FDialog.Buttons.AddButton('OK').&End.Show;
  Assert.IsTrue(FDialog.ShowCalled);
end;

procedure TDialogBuilderTests.TestShow_ReturnsDialogBuilder;
var
  Result: IDialogBuilder;
begin
  Result := FDialog.Buttons.AddButton('OK').&End.Show;
  Assert.IsNotNull(Result);
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogBuilderTests);

end.
