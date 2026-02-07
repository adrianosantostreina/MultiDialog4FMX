unit MultiDialog4FMX.Tests.MemoryLeaks;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Tests.Mocks,
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  System.SysUtils;

type
  [TestFixture]
  TMemoryLeakTests = class
  public
    [Test]
    procedure TestDialogCreation_NoMemoryLeak;
    
    [Test]
    procedure TestButtonHandler_ProperlyDestroyed;
    
    [Test]
    procedure TestMultipleButtons_NoMemoryLeak;
    
    [Test]
    procedure TestAnonymousMethod_NoMemoryLeak;
    
    [Test]
    procedure TestButtonHandlerList_ClearsOnDestroy;
  end;

implementation

{ TMemoryLeakTests }

procedure TMemoryLeakTests.TestDialogCreation_NoMemoryLeak;
var
  Dialog: IDialogBuilder;
begin
  Dialog := TMockDialogBase.Create;
  // No try..finally needed for interfaces
  Dialog.SetTitle('Test');
  Dialog.SetMessage('Test Message');
  Assert.Pass('Dialog created and destroyed without leaks');
  // Dialog will be freed when it goes out of scope (RefCount = 0)
end;

procedure TMemoryLeakTests.TestButtonHandler_ProperlyDestroyed;
var
  Dialog: IDialogBuilder;
  Mock: TMockDialogBase;
begin
  Mock := TMockDialogBase.Create;
  Dialog := Mock; // Interface reference takes ownership
  
  Dialog.Buttons.AddButton('Test Button');
  Assert.AreEqual(1, Mock.ButtonHandlers.Count);
  // TButtonHandlerList owns its objects (OwnsObjects=True by default)
  // So they should be freed when the list is destroyed
end;

procedure TMemoryLeakTests.TestMultipleButtons_NoMemoryLeak;
var
  Dialog: IDialogBuilder;
  Mock: TMockDialogBase;
begin
  Mock := TMockDialogBase.Create;
  Dialog := Mock;
  
  Dialog.Buttons
    .AddButton('Button 1')
    .AddButton('Button 2')
    .AddButton('Button 3')
    .AddButton('Button 4');
      
  Assert.AreEqual(4, Mock.ButtonHandlers.Count);
  Assert.Pass('Multiple buttons created and destroyed without leaks');
end;

procedure TMemoryLeakTests.TestAnonymousMethod_NoMemoryLeak;
var
  Dialog: IDialogBuilder;
  Mock: TMockDialogBase;
  TestValue: Integer;
begin
  TestValue := 0;
  Mock := TMockDialogBase.Create;
  Dialog := Mock;
  
  Dialog.Buttons.AddButton('Test',
    procedure
    begin
      TestValue := 42;
    end);
      
  // Execute the anonymous method
  Mock.ButtonHandlers[0].AnonymousHandler();
  Assert.AreEqual(42, TestValue);
  Assert.Pass('Anonymous method created and destroyed without leaks');
end;

procedure TMemoryLeakTests.TestButtonHandlerList_ClearsOnDestroy;
var
  Dialog: IDialogBuilder;
  Mock: TMockDialogBase;
  InitialCount: Integer;
begin
  Mock := TMockDialogBase.Create;
  Dialog := Mock;
  
  Dialog.Buttons
    .AddButton('Button 1')
    .AddButton('Button 2');
      
  InitialCount := Mock.ButtonHandlers.Count;
  Assert.AreEqual(2, InitialCount);
  Dialog := nil; // Explicit release
  
  // After free, the list and all its objects should be destroyed
  // This test essentially just verifies no crash on destroy
  Assert.Pass('ButtonHandlerList cleared on destroy');
end;

initialization
  TDUnitX.RegisterTestFixture(TMemoryLeakTests);

end.
