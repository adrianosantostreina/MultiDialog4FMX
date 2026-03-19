unit MultiDialog4FMX.Tests.Factory;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Factory,
  System.SysUtils;

type
  [TestFixture]
  TFactoryTests = class
  public
    [Test]
    procedure TestCreateDialog_ReturnsNonNil;

    [Test]
    procedure TestCreateDialog_ReturnsIDialogBuilder;

    [Test]
    procedure TestCreateDialog_CanChainMethods;

    {$IFDEF ANDROID}
    [Test]
    procedure TestCreateDialog_Android_CreatesCorrectType;
    {$ENDIF}

    {$IF DEFINED(IOS)}
    [Test]
    procedure TestCreateDialog_iOS_CreatesCorrectType;
    {$ENDIF}

    {$IF NOT (DEFINED(ANDROID) OR DEFINED(IOS))}
    [Test]
    procedure TestCreateDialog_Desktop_CreatesCorrectType;
    {$ENDIF}
  end;

implementation

{$IFDEF ANDROID}
uses
  MultiDialog4FMX.Android;
{$ELSEIF DEFINED(IOS)}
uses
  MultiDialog4FMX.iOS;
{$ELSE}
uses
  MultiDialog4FMX.Desktop;
{$ENDIF}

{ TFactoryTests }

procedure TFactoryTests.TestCreateDialog_ReturnsNonNil;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog;
  Assert.IsNotNull(Dialog);
end;

procedure TFactoryTests.TestCreateDialog_ReturnsIDialogBuilder;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog;
  Assert.IsNotNull(Dialog as IDialogBuilder);
end;

procedure TFactoryTests.TestCreateDialog_CanChainMethods;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog
    .SetTitle('Test')
    .SetMessage('Message')
    .SetCancelable(True);

  Assert.IsNotNull(Dialog);
end;

{$IFDEF ANDROID}
procedure TFactoryTests.TestCreateDialog_Android_CreatesCorrectType;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog;
  Assert.IsTrue(Supports(Dialog, IDialogBuilder));
end;
{$ENDIF}

{$IF DEFINED(IOS)}
procedure TFactoryTests.TestCreateDialog_iOS_CreatesCorrectType;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog;
  Assert.IsTrue(Supports(Dialog, IDialogBuilder));
end;
{$ENDIF}

{$IF NOT (DEFINED(ANDROID) OR DEFINED(IOS))}
procedure TFactoryTests.TestCreateDialog_Desktop_CreatesCorrectType;
var
  Dialog: IDialogBuilder;
begin
  Dialog := CreateDialog;
  Assert.IsTrue(Dialog is TDesktopDialog);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TFactoryTests);

end.
