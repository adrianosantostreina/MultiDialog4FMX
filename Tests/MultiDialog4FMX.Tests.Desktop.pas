unit MultiDialog4FMX.Tests.Desktop;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Desktop,
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces;

type
  [TestFixture]
  TDesktopDialogTests = class
  public
    [Test]
    procedure TestDesktopDialog_CanInstantiate;

    [Test]
    procedure TestDesktopDialog_FluentChain_WorksWithoutShow;

    [Test]
    procedure TestDesktopDialog_ButtonHandler_NoLeak;
  end;

implementation

procedure TDesktopDialogTests.TestDesktopDialog_CanInstantiate;
var
  D: IDialogBuilder;
begin
  D := TDesktopDialog.Create;
  Assert.IsNotNull(D);
  D := nil;
end;

procedure TDesktopDialogTests.TestDesktopDialog_FluentChain_WorksWithoutShow;
var
  D: IDialogBuilder;
begin
  D := TDesktopDialog.Create;
  Assert.IsNotNull(
    D.SetTitle('Test').SetMessage('Hello').Buttons.AddButton('OK').&End
  );
  D := nil;
end;

procedure TDesktopDialogTests.TestDesktopDialog_ButtonHandler_NoLeak;
var
  D      : IDialogBuilder;
  LBefore: Integer;
begin
  LBefore := TButtonHandler.InstanceCount;
  D := TDesktopDialog.Create;
  D.Buttons.AddButton('OK').AddButton('Cancel');
  Assert.AreEqual(LBefore + 2, TButtonHandler.InstanceCount);
  D := nil;
  Assert.AreEqual(LBefore, TButtonHandler.InstanceCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TDesktopDialogTests);

end.
