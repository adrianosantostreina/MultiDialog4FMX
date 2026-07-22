unit MultiDialog4FMX.Tests.Telemetry;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,
  System.UITypes;

type
  [TestFixture]
  TDialogTelemetryTests = class
  public
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmit_WithListener_DeliversInfo;

    [Test]
    procedure TestEmit_NoListener_DoesNotRaise;
  end;

implementation

procedure TDialogTelemetryTests.TearDown;
begin
  TDialogTelemetry.OnEvent := nil;
end;

procedure TDialogTelemetryTests.TestEmit_WithListener_DeliversInfo;
var
  LReceived: TDialogEventInfo;
  LCalled: Boolean;
  LInfo: TDialogEventInfo;
begin
  LCalled := False;
  TDialogTelemetry.OnEvent :=
    procedure(const AInfo: TDialogEventInfo)
    begin
      LCalled := True;
      LReceived := AInfo;
    end;

  LInfo := Default(TDialogEventInfo);
  LInfo.Kind := dekShown;
  LInfo.DialogType := mdtWarning;
  LInfo.Title := 'Ola';
  LInfo.Result := mrOk;
  TDialogTelemetry.Emit(LInfo);

  Assert.IsTrue(LCalled, 'O listener deve receber o evento');
  Assert.AreEqual(Ord(dekShown), Ord(LReceived.Kind));
  Assert.AreEqual('Ola', LReceived.Title);
  Assert.AreEqual(mrOk, LReceived.Result);
end;

procedure TDialogTelemetryTests.TestEmit_NoListener_DoesNotRaise;
var
  LInfo: TDialogEventInfo;
begin
  TDialogTelemetry.OnEvent := nil;
  LInfo := Default(TDialogEventInfo);
  LInfo.Kind := dekEnqueued;
  Assert.WillNotRaise(
    procedure
    begin
      TDialogTelemetry.Emit(LInfo);
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TDialogTelemetryTests);

end.
