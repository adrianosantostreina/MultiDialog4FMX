unit MultiDialog4FMX.Queue;

interface

uses
  MultiDialog4FMX.Interfaces,
  MultiDialog4FMX.Telemetry,

  FMX.Forms,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Diagnostics,
  System.Generics.Collections;

type
  TDialogSnapshot = class
  private
    class var FNextId: Integer;
  private
    FId: Integer;
    FStopwatch: TStopwatch;
    FForm: TCommonCustomForm;
    FTitle: string;
    FMessage: string;
    FMsgType: TMultiDialogType;
    FCancelable: Boolean;
    FFontSize: Single;
    FBorderRadius: Single;
    FAnimation: TDialogAnimation;
    FTheme: TDialogTheme;
    FCustomSVG: string;
    FCustomIconColor: TAlphaColor;
    FResultCallback: TDialogResultProc;
    FButtons: TButtonHandlerList;
  public
    constructor Create(const AForm: TCommonCustomForm; const ATitle, AMessage: string;
      const AMsgType: TMultiDialogType; const ACancelable: Boolean;
      const AFontSize, ABorderRadius: Single; const AAnimation: TDialogAnimation;
      const ATheme: TDialogTheme; const ACustomSVG: string;
      const ACustomIconColor: TAlphaColor; const AResultCallback: TDialogResultProc;
      const AButtons: TButtonHandlerList);
    destructor Destroy; override;

    property Form: TCommonCustomForm read FForm;
    property Title: string read FTitle;
    property Message: string read FMessage;
    property MsgType: TMultiDialogType read FMsgType;
    property Cancelable: Boolean read FCancelable;
    property FontSize: Single read FFontSize;
    property BorderRadius: Single read FBorderRadius;
    property Animation: TDialogAnimation read FAnimation;
    property Theme: TDialogTheme read FTheme;
    property CustomSVG: string read FCustomSVG;
    property CustomIconColor: TAlphaColor read FCustomIconColor;
    property ResultCallback: TDialogResultProc read FResultCallback;
    property Buttons: TButtonHandlerList read FButtons;
    property Id: Integer read FId;
    function ElapsedMs: Int64;
  end;

  IDialogVisualInstance = interface
    ['{7F1B0A11-4E9A-4C4B-9C4B-2B4E7A0E1F10}']
    procedure Show;
    procedure Suppress;
    function SnapshotId: Integer;
    procedure CloseWith(const AResult: TModalResult);
  end;

  TDialogInstanceFactory = reference to function(const ASnapshot: TDialogSnapshot): IDialogVisualInstance;

  TDialogQueueManager = class(TComponent)
  private
    class var FInstance: TDialogQueueManager;
    class var FFactory: TDialogInstanceFactory;
    FQueues: TObjectDictionary<TCommonCustomForm, TQueue<TDialogSnapshot>>;
    FActive: TDictionary<TCommonCustomForm, IDialogVisualInstance>;
    FWatched: TList<TCommonCustomForm>;
    procedure ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
    procedure EnsureWatched(const AForm: TCommonCustomForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Instance: TDialogQueueManager;
    class procedure RegisterInstanceFactory(const AFactory: TDialogInstanceFactory);

    procedure Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
    procedure NotifyClosed(const AForm: TCommonCustomForm);

    // Test-support accessors (protected: reachable from a same-behavior subclass in tests,
    // not part of the public API surface).
    function DebugIsActive(const AForm: TCommonCustomForm): Boolean;
    function DebugQueueLength(const AForm: TCommonCustomForm): Integer;

    procedure CloseByHandle(const AForm: TCommonCustomForm; const AId: Integer;
      const AResult: TModalResult);
    function IsHandleActive(const AForm: TCommonCustomForm; const AId: Integer): Boolean;
    function DebugActiveSnapshotId(const AForm: TCommonCustomForm): Integer;
  end;

function MakeDialogEventInfo(const AKind: TDialogEventKind;
  const ASnapshot: TDialogSnapshot; const AResult: TModalResult): TDialogEventInfo;

implementation

function MakeDialogEventInfo(const AKind: TDialogEventKind;
  const ASnapshot: TDialogSnapshot; const AResult: TModalResult): TDialogEventInfo;
begin
  Result.Kind := AKind;
  Result.DialogType := ASnapshot.MsgType;
  Result.Title := ASnapshot.Title;
  Result.Result := AResult;
  Result.ElapsedMs := ASnapshot.ElapsedMs;
end;

{ TDialogSnapshot }

constructor TDialogSnapshot.Create(const AForm: TCommonCustomForm;
  const ATitle, AMessage: string; const AMsgType: TMultiDialogType;
  const ACancelable: Boolean; const AFontSize, ABorderRadius: Single;
  const AAnimation: TDialogAnimation; const ATheme: TDialogTheme;
  const ACustomSVG: string; const ACustomIconColor: TAlphaColor;
  const AResultCallback: TDialogResultProc; const AButtons: TButtonHandlerList);
var
  I: Integer;
  LSource: TButtonHandler;
  LCopy: TButtonHandler;
begin
  inherited Create;
  Inc(FNextId);
  FId := FNextId;
  FStopwatch := TStopwatch.StartNew;
  FForm            := AForm;
  FTitle           := ATitle;
  FMessage         := AMessage;
  FMsgType         := AMsgType;
  FCancelable      := ACancelable;
  FFontSize        := AFontSize;
  FBorderRadius    := ABorderRadius;
  FAnimation       := AAnimation;
  FTheme           := ATheme;
  FCustomSVG       := ACustomSVG;
  FCustomIconColor := ACustomIconColor;
  FResultCallback  := AResultCallback;

  FButtons := TButtonHandlerList.Create(True);
  for I := 0 to AButtons.Count - 1 do
  begin
    LSource := AButtons[I];
    LCopy := TButtonHandler.Create;
    LCopy.Text             := LSource.Text;
    LCopy.ClickHandler     := LSource.ClickHandler;
    LCopy.TapHandler       := LSource.TapHandler;
    LCopy.AnonymousHandler := LSource.AnonymousHandler;
    LCopy.Color            := LSource.Color;
    LCopy.StyleLookup      := LSource.StyleLookup;
    LCopy.ModalResult      := LSource.ModalResult;
    LCopy.Timeout          := LSource.Timeout;
    // Overlay fica nil — e estado visual, comeca vazio em toda copia.
    FButtons.Add(LCopy);
  end;
end;

destructor TDialogSnapshot.Destroy;
begin
  FButtons.Free;
  inherited;
end;

function TDialogSnapshot.ElapsedMs: Int64;
begin
  Result := FStopwatch.ElapsedMilliseconds;
end;

{ TDialogQueueManager }

constructor TDialogQueueManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueues := TObjectDictionary<TCommonCustomForm, TQueue<TDialogSnapshot>>.Create([doOwnsValues]);
  FActive := TDictionary<TCommonCustomForm, IDialogVisualInstance>.Create;
  FWatched := TList<TCommonCustomForm>.Create;
end;

destructor TDialogQueueManager.Destroy;
begin
  FWatched.Free;
  FActive.Free;
  FQueues.Free;
  inherited;
end;

class function TDialogQueueManager.Instance: TDialogQueueManager;
begin
  if not Assigned(FInstance) then
    FInstance := TDialogQueueManager.Create(nil);
  Result := FInstance;
end;

class procedure TDialogQueueManager.RegisterInstanceFactory(const AFactory: TDialogInstanceFactory);
begin
  FFactory := AFactory;
end;

procedure TDialogQueueManager.EnsureWatched(const AForm: TCommonCustomForm);
begin
  if FWatched.IndexOf(AForm) < 0 then
  begin
    AForm.FreeNotification(Self);
    FWatched.Add(AForm);
  end;
end;

procedure TDialogQueueManager.ShowNow(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
var
  LInstance: IDialogVisualInstance;
begin
  if not Assigned(FFactory) then
    raise Exception.Create('TDialogQueueManager.RegisterInstanceFactory nunca foi chamado — ' +
      'MultiDialog4FMX.FMX deveria ter registrado a factory na sua secao initialization.');
  LInstance := FFactory(ASnapshot);
  FActive.AddOrSetValue(AForm, LInstance);
  LInstance.Show;
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekShown, ASnapshot, mrNone));
end;

procedure TDialogQueueManager.Enqueue(const AForm: TCommonCustomForm; const ASnapshot: TDialogSnapshot);
var
  LQueue: TQueue<TDialogSnapshot>;
begin
  TDialogTelemetry.Emit(MakeDialogEventInfo(dekEnqueued, ASnapshot, mrNone));
  EnsureWatched(AForm);

  if FActive.ContainsKey(AForm) then
  begin
    if not FQueues.TryGetValue(AForm, LQueue) then
    begin
      LQueue := TQueue<TDialogSnapshot>.Create;
      FQueues.Add(AForm, LQueue);
    end;
    LQueue.Enqueue(ASnapshot);
  end
  else
    ShowNow(AForm, ASnapshot);
end;

procedure TDialogQueueManager.NotifyClosed(const AForm: TCommonCustomForm);
var
  LQueue: TQueue<TDialogSnapshot>;
  LNext: TDialogSnapshot;
begin
  FActive.Remove(AForm);

  if FQueues.TryGetValue(AForm, LQueue) and (LQueue.Count > 0) then
  begin
    LNext := LQueue.Dequeue;
    ShowNow(AForm, LNext);
  end;
end;

procedure TDialogQueueManager.Notification(AComponent: TComponent; Operation: TOperation);
var
  LForm: TCommonCustomForm;
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
begin
  inherited;
  if Operation <> TOperation.opRemove then
    Exit;
  if not (AComponent is TCommonCustomForm) then
    Exit;

  LForm := TCommonCustomForm(AComponent);

  if FActive.TryGetValue(LForm, LInstance) then
  begin
    LInstance.Suppress;
    FActive.Remove(LForm);
  end;

  if FQueues.TryGetValue(LForm, LQueue) then
  begin
    while LQueue.Count > 0 do
      LQueue.Dequeue.Free; // libera cada TDialogSnapshot pendente — nunca vai aparecer, sem callback
    FQueues.Remove(LForm);
  end;

  FWatched.Remove(LForm);
end;

function TDialogQueueManager.DebugIsActive(const AForm: TCommonCustomForm): Boolean;
begin
  Result := FActive.ContainsKey(AForm);
end;

function TDialogQueueManager.DebugQueueLength(const AForm: TCommonCustomForm): Integer;
var
  LQueue: TQueue<TDialogSnapshot>;
begin
  if FQueues.TryGetValue(AForm, LQueue) then
    Result := LQueue.Count
  else
    Result := 0;
end;

procedure TDialogQueueManager.CloseByHandle(const AForm: TCommonCustomForm;
  const AId: Integer; const AResult: TModalResult);
var
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
  LTemp: TArray<TDialogSnapshot>;
  LSnap: TDialogSnapshot;
  I: Integer;
begin
  // 1) e o dialogo ativo deste form?
  if FActive.TryGetValue(AForm, LInstance) and (LInstance.SnapshotId = AId) then
  begin
    LInstance.CloseWith(AResult);
    Exit;
  end;

  // 2) esta enfileirado? reconstrua a fila sem o item alvo, resolvendo-o.
  if FQueues.TryGetValue(AForm, LQueue) then
  begin
    LTemp := LQueue.ToArray;
    LQueue.Clear;
    for I := 0 to High(LTemp) do
    begin
      LSnap := LTemp[I];
      if LSnap.Id = AId then
      begin
        if Assigned(LSnap.ResultCallback) then
          LSnap.ResultCallback(AResult);
        TDialogTelemetry.Emit(MakeDialogEventInfo(dekClosed, LSnap, AResult));
        LSnap.Free;   // nunca virou instancia visual — o manager o possui
      end
      else
        LQueue.Enqueue(LSnap);
    end;
  end;
  // 3) nao encontrado -> ja resolvido -> no-op
end;

function TDialogQueueManager.IsHandleActive(const AForm: TCommonCustomForm;
  const AId: Integer): Boolean;
var
  LInstance: IDialogVisualInstance;
  LQueue: TQueue<TDialogSnapshot>;
  LSnap: TDialogSnapshot;
begin
  if FActive.TryGetValue(AForm, LInstance) and (LInstance.SnapshotId = AId) then
    Exit(True);
  if FQueues.TryGetValue(AForm, LQueue) then
    for LSnap in LQueue do
      if LSnap.Id = AId then
        Exit(True);
  Result := False;
end;

function TDialogQueueManager.DebugActiveSnapshotId(const AForm: TCommonCustomForm): Integer;
var
  LInstance: IDialogVisualInstance;
begin
  if FActive.TryGetValue(AForm, LInstance) then
    Result := LInstance.SnapshotId
  else
    Result := -1;
end;

initialization

finalization
  // M1: TDialogQueueManager.Instance lazily creates FInstance via Create(nil) — with no
  // owner, nothing frees it. FInstance is a private class var, but this unit's own
  // finalization section can still reach it directly (visibility is per-unit, not
  // per-class, for code living in the same unit as the class declaration).
  FreeAndNil(TDialogQueueManager.FInstance);

end.
