unit MultiDialog4FMX.Tests.Await;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Util,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Await,
  System.UITypes;

type
  [TestFixture]
  TDialogAwaitTests = class
  public
    [Test]
    procedure TestShowAndWait_OnMainThread_Raises;
  end;

implementation

procedure TDialogAwaitTests.TestShowAndWait_OnMainThread_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMultiDialog4FMX.Dialog
        .Buttons.AddButton('OK').&End
        .ShowAndWait;   // rodando na main thread do runner
    end,
    EDialogAwaitOnMainThread);
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogAwaitTests);

end.
