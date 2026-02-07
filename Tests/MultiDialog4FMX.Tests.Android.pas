unit MultiDialog4FMX.Tests.Android;

interface

uses
  DUnitX.TestFramework,
  MultiDialog4FMX.Android,
  MultiDialog4FMX.Base,
  FMX.Graphics,
  FMX.Forms,
  System.SysUtils,
  System.UITypes;

type
  [TestFixture]
  TAndroidDialogTests = class
  private
    FDialog: TAndroidDialog;
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestCalculateMessageHeight_ShortText;
    
    [Test]
    procedure TestCalculateMessageHeight_LongText;
    
    [Test]
    procedure TestCalculateMessageHeight_MultiLine;
    
    [Test]
    procedure TestCalculateMessageHeight_EmptyText;
    
    [Test]
    procedure TestResolveParentForm_WithExplicitForm;
    
    [Test]
    procedure TestResolveParentForm_WithNilForm;
    
    [Test]
    procedure TestInternalShow_RequiresMinimumOneButton;
    
    [Test]
    procedure TestInternalShow_EnforcesMaximumFourButtons;
  end;

implementation

type
  // Cracker class to access protected members of TAndroidDialog
  TAndroidDialogCracker = class(TAndroidDialog);

{ TAndroidDialogTests }

procedure TAndroidDialogTests.Setup;
begin
  FDialog := TAndroidDialog.Create;
end;

procedure TAndroidDialogTests.TearDown;
begin
  FDialog := nil;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_ShortText;
var
  Height: Single;
  Font: TFont;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight('Test', 300, Font);
    Assert.IsTrue(Height > 0, 'Height should be greater than 0');
    Assert.IsTrue(Height < 100, 'Short text should have small height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_LongText;
var
  Height: Single;
  Font: TFont;
  LongText: string;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    LongText := 'This is a very long text that should wrap into multiple lines when displayed in the dialog. ' +
                'It contains enough characters to test the wrapping functionality of the message calculation.';
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight(LongText, 300, Font);
    Assert.IsTrue(Height > 50, 'Long text should have greater height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_MultiLine;
var
  Height: Single;
  Font: TFont;
  MultiLineText: string;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    MultiLineText := 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3';
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight(MultiLineText, 300, Font);
    Assert.IsTrue(Height > 0, 'Multi-line text should have height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestCalculateMessageHeight_EmptyText;
var
  Height: Single;
  Font: TFont;
begin
  Font := TFont.Create;
  try
    Font.Size := 14;
    Height := TAndroidDialogCracker(FDialog).CalculateMessageHeight('', 300, Font);
    Assert.IsTrue(Height >= 0, 'Empty text should have non-negative height');
  finally
    Font.Free;
  end;
end;

procedure TAndroidDialogTests.TestResolveParentForm_WithExplicitForm;
var
  TestForm: TCommonCustomForm;
  Result: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  
  TestForm := TCommonCustomForm.Create(nil);
  try
    // ResolveParentForm is protected in TDialogBase, so we cast to expose it
    Result := TAndroidDialogCracker(FDialog).ResolveParentForm(TestForm);
    Assert.AreEqual(TestForm, Result, 'Should return the explicit form provided');
  finally
    TestForm.Free;
  end;
end;

procedure TAndroidDialogTests.TestResolveParentForm_WithNilForm;
var
  Result: TCommonCustomForm;
begin
  if not Assigned(Application) then
  begin
    Assert.Pass('Cannot test without Application object');
    Exit;
  end;
  
  // ResolveParentForm is protected
  Result := TAndroidDialogCracker(FDialog).ResolveParentForm(nil);
  
  // In console app without main form, this may return nil.
  // We accept nil OR a valid form.
  if Assigned(Application.MainForm) then
    Assert.IsNotNull(Result, 'Should resolve to a valid form')
  else
    Assert.IsNull(Result, 'Should return nil when no forms are active');
end;

procedure TAndroidDialogTests.TestInternalShow_RequiresMinimumOneButton;
begin
  Assert.WillRaise(
    procedure
    begin
      // InternalShow is protected
      TAndroidDialogCracker(FDialog).InternalShow(nil);
    end,
    Exception,
    'O número mínimo de botões é 1.');
end;

procedure TAndroidDialogTests.TestInternalShow_EnforcesMaximumFourButtons;
begin
  // Add 5 buttons manually to FButtonHandlers (bypassing the builder validation)
  // FButtonHandlers is protected in TDialogBase.
  // We can access it via TAndroidDialogCracker if TDialogBase is in the same package/unit context, 
  // OR we use the public ButtonHandlers property IF we are using TMockDialogBase? 
  // ERROR: FDialog here is TAndroidDialog, NOT TMockDialogBase. 
  // TAndroidDialog doesn't have the "ButtonHandlers" public property we added to TMockDialogBase.
  // We need to access FButtonHandlers via the cracker too.
  
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  TAndroidDialogCracker(FDialog).FButtonHandlers.Add(TButtonHandler.Create);
  
  Assert.WillRaise(
    procedure
    begin
      TAndroidDialogCracker(FDialog).InternalShow(nil);
    end,
    Exception,
    'O diálogo suporta no máximo 4 botões.');
end;

initialization
  TDUnitX.RegisterTestFixture(TAndroidDialogTests);

end.
