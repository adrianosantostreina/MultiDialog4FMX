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
// Dialog with no buttons: creating and destroying must leave FInstanceCount unchanged.
var
  Dialog : IDialogBuilder;
  LBefore: Integer;
begin
  LBefore := TButtonHandler.InstanceCount;
  Dialog := TMockDialogBase.Create;
  Dialog.SetTitle('Test');
  Dialog.SetMessage('Test Message');
  Dialog := nil; // releases interface, triggers Destroy -> FButtonHandlers freed
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount,
    'No handlers created, count must not change');
end;

procedure TMemoryLeakTests.TestButtonHandler_ProperlyDestroyed;
// 1 button: +1 while alive, back to baseline after dialog destroyed.
var
  Dialog  : IDialogBuilder;
  Mock    : TMockDialogBase;
  LBefore, LDuring: Integer;
begin
  LBefore := TButtonHandler.InstanceCount;

  Mock   := TMockDialogBase.Create;
  Dialog := Mock;
  Dialog.Buttons.AddButton('Test Button');

  LDuring := TButtonHandler.InstanceCount;
  Assert.AreEqual(LBefore + 1, LDuring,
    'One handler should be alive while dialog exists');

  Dialog := nil;
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount,
    'Handler count must return to baseline after dialog is destroyed');
end;

procedure TMemoryLeakTests.TestMultipleButtons_NoMemoryLeak;
// 4 buttons: +4 during, back to baseline after.
var
  Dialog  : IDialogBuilder;
  Mock    : TMockDialogBase;
  LBefore, LDuring: Integer;
begin
  LBefore := TButtonHandler.InstanceCount;

  Mock   := TMockDialogBase.Create;
  Dialog := Mock;
  Dialog.Buttons
    .AddButton('Button 1')
    .AddButton('Button 2')
    .AddButton('Button 3')
    .AddButton('Button 4');

  LDuring := TButtonHandler.InstanceCount;
  Assert.AreEqual(LBefore + 4, LDuring,
    'Four handlers should be alive while dialog exists');

  Dialog := nil;
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount,
    'Handler count must return to baseline after dialog is destroyed');
end;

procedure TMemoryLeakTests.TestAnonymousMethod_NoMemoryLeak;
// 1 anonymous-method button: execute handler, destroy dialog, count returns to baseline.
var
  Dialog   : IDialogBuilder;
  Mock     : TMockDialogBase;
  TestValue: Integer;
  LBefore  : Integer;
begin
  LBefore   := TButtonHandler.InstanceCount;
  TestValue := 0;

  Mock   := TMockDialogBase.Create;
  Dialog := Mock;
  Dialog.Buttons.AddButton('Test',
    procedure
    begin
      TestValue := 42;
    end);

  Mock.ButtonHandlers[0].AnonymousHandler();
  Assert.AreEqual(42, TestValue, 'Anonymous handler must execute correctly');

  Dialog := nil;
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount,
    'Handler count must return to baseline after dialog is destroyed');
end;

procedure TMemoryLeakTests.TestButtonHandlerList_ClearsOnDestroy;
// 2 buttons: explicit nil releases interface -> list freed -> both handlers freed.
var
  Dialog : IDialogBuilder;
  Mock   : TMockDialogBase;
  LBefore: Integer;
begin
  LBefore := TButtonHandler.InstanceCount;

  Mock   := TMockDialogBase.Create;
  Dialog := Mock;
  Dialog.Buttons
    .AddButton('Button 1')
    .AddButton('Button 2');

  Assert.AreEqual(LBefore + 2, TButtonHandler.InstanceCount,
    'Two handlers should be alive');

  Dialog := nil; // explicit release
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount,
    'Both handlers must be freed when dialog is destroyed');
end;

initialization
  TDUnitX.RegisterTestFixture(TMemoryLeakTests);

end.
