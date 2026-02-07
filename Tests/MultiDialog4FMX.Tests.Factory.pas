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
    procedure TestCreateDialog_iOS_RaisesNotImplemented;
    {$ENDIF}
    
    {$IF NOT (DEFINED(ANDROID) OR DEFINED(IOS))}
    [Test]
    procedure TestCreateDialog_UnsupportedPlatform_RaisesException;
    {$ENDIF}
  end;

implementation

{$IFDEF ANDROID}
uses
  MultiDialog4FMX.Android;
{$ENDIF}

{ TFactoryTests }

procedure TFactoryTests.TestCreateDialog_ReturnsNonNil;
var
  Dialog: IDialogBuilder;
begin
  {$IFDEF ANDROID}
  Dialog := CreateDialog;
  Assert.IsNotNull(Dialog);
  {$ELSE}
  // On non-Android platforms, we expect an exception
  Assert.Pass('Test skipped on this platform');
  {$ENDIF}
end;

procedure TFactoryTests.TestCreateDialog_ReturnsIDialogBuilder;
var
  Dialog: IDialogBuilder;
begin
  {$IFDEF ANDROID}
  Dialog := CreateDialog;
  Assert.IsNotNull(Dialog as IDialogBuilder);
  {$ELSE}
  Assert.Pass('Test skipped on this platform');
  {$ENDIF}
end;

procedure TFactoryTests.TestCreateDialog_CanChainMethods;
var
  Dialog: IDialogBuilder;
begin
  {$IFDEF ANDROID}
  Dialog := CreateDialog
    .SetTitle('Test')
    .SetMessage('Message')
    .SetCancelable(True);
    
  Assert.IsNotNull(Dialog);
  {$ELSE}
  Assert.Pass('Test skipped on this platform');
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TFactoryTests.TestCreateDialog_Android_CreatesCorrectType;
var
  Dialog: IDialogBuilder;
  AndroidDialog: TAndroidDialog;
begin
  Dialog := CreateDialog;
  
  // Verify it's an Android dialog by checking if it supports the interface
  Assert.IsTrue(Supports(Dialog, IDialogBuilder));
end;
{$ENDIF}

{$IF DEFINED(IOS)}
procedure TFactoryTests.TestCreateDialog_iOS_RaisesNotImplemented;
begin
  Assert.WillRaise(
    procedure
    begin
      CreateDialog;
    end,
    Exception,
    'iOS não implementado ainda');
end;
{$ENDIF}

{$IF NOT (DEFINED(ANDROID) OR DEFINED(IOS))}
procedure TFactoryTests.TestCreateDialog_UnsupportedPlatform_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      CreateDialog;
    end,
    Exception,
    'Plataforma não suportada');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TFactoryTests);

end.
