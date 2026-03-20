unit MultiDialog4FMX.Tests.Mocks;

interface

uses
  MultiDialog4FMX.Base,
  MultiDialog4FMX.Interfaces,
  FMX.Forms,
  FMX.Types,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections;

type
  /// <summary>
  /// Mock implementation of TDialogBase for testing without UI dependencies
  /// </summary>
  TMockDialogBase = class(TDialogBase)
  private
    FShowCalled: Boolean;
    FLastParentForm: TCommonCustomForm;
  protected
    procedure InternalShow(const AForm: TCommonCustomForm); override;
  public
    constructor Create;
    
    /// <summary>
    /// Returns true if Show was called
    /// </summary>
    property ShowCalled: Boolean read FShowCalled;
    
    /// <summary>
    /// Returns the last parent form passed to InternalShow
    /// </summary>
    property LastParentForm: TCommonCustomForm read FLastParentForm;
    
    /// <summary>
    /// Resets the mock state for a new test
    /// </summary>
    procedure Reset;
    
    /// <summary>
    /// Public access to test ResolveParentForm logic
    /// </summary>
    function TestResolveParentForm(const AForm: TCommonCustomForm): TCommonCustomForm;
    
    /// <summary>
    /// Public access to protected fields for testing
    /// </summary>
    property Title: string read FTitle;
    property Message: string read FMessage;
    property Cancelable: Boolean read FCancelable;
    property ButtonHandlers: TButtonHandlerList read FButtonHandlers;
    property MsgType: TMultiDialogType read FMsgType;
    property FontSize: Single read FFontSize;
    property BorderRadius: Single read FBorderRadius;
    property CustomSVG: string read FCustomSVG;
    property CustomIconColor: TAlphaColor read FCustomIconColor;
    property ResultCallback: TDialogResultProc read FResultCallback;
  end;

implementation

{ TMockDialogBase }

constructor TMockDialogBase.Create;
begin
  inherited Create;
  Reset;
end;

procedure TMockDialogBase.InternalShow(const AForm: TCommonCustomForm);
begin
  FShowCalled := True;
  FLastParentForm := AForm;
  // Don't create any UI components
end;

procedure TMockDialogBase.Reset;
begin
  FShowCalled := False;
  FLastParentForm := nil;
end;

function TMockDialogBase.TestResolveParentForm(const AForm: TCommonCustomForm): TCommonCustomForm;
begin
  Result := ResolveParentForm(AForm);
end;

end.
