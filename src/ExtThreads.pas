(*
  Extended threads library for Delphi.
  Copyright (c) 2009, Andrei Borovsky. All rights reserved.
  Free for both commercial and non-commercial use.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ExtThreads;

interface

uses
  Windows, Messages, SysUtils, Classes, SyncObjs;

type

  TExtThreadProcedure = procedure;
  TExtThreadMethod = procedure of object;
  TExtThreadEvent = procedure(Sender : Pointer) of object;

  TCancelationAction = (caNone, caSleep, caTerminate, caExecute);
  TExtThreadState = (tsRunning, tsSleeping, tsTerminating, tsTerminated);

  (*  The methods of this class can be devided into 2 categories: those that are intended for calling from the thread's execute method only,
      and those that must be called only from an external thread to manage this thread. The methods from the first category are declared in the protected section,
      so that they are not seen outside the class' instance, while the methods from the second category are declared in the public section.
 *)

  TExtThread = class(TThread)
  private
    FEventList : TList;
    FCancelationAction : TCancelationAction;
    FExlusive : TRTLCriticalSection;
    FWake : THandle;
    FOnBeforePause : TExtThreadMethod;
    FOnBeforeWake : TExtThreadMethod;
    FOnBeforeExit : TExtThreadMethod;
    FOnBeforeSleep : TExtThreadMethod;
    FOnSleep : TExtThreadEvent;
    FOnPause : TExtThreadEvent;
    FOnWake : TExtThreadEvent;
    FEventToCall : TExtThreadEvent;
    FThreadState : TExtThreadState;
    FLockedInt, FLockedExt : LongWord;
    procedure ExecuteEvents;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    procedure EnterExclusive(var Lock : LongWord);
    procedure LeaveExclusive(var Lock : LongWord);
    procedure CallExtThreadEvent;
    procedure _Wake;
  protected
    (* Function: Sleep
      This method should be called from within the thread's Execute method.
      Sleep suspends the thread execution.
      The thread execution is resumed on one of the following conditions:
      - Wake is called by an external thread.
      - Quit or QuitAsync is called by an external thread.
      - SendToThread or PostToThread is called by an external thread.
      If Sleep is called between the Quit/QuitAsync call and the cancelation point the sleep returns immediately.
      OnSleep event handler is called in the main thread context after the thread has gome to sleep.
    *)
    procedure Sleep;
    (* Function: ExitThread
       This method should be called from within the thread's Execute method.
       Use this method to exit the thread's Execute method.
       OnBeforeExit event handler is called before the method returns. All pending requests from PostToThread/SendToThread are also executed.
     *)
    procedure ExitThread;
    (* Function: CancelationPoint
       This method should be called from within the thread's Execute method.
       Call this method to execute any actions requested from external thread.
       In order your thread to be manageable CancelationPoint should be called regularily within the thread's Execute method.
       When an external thread calls Pause/PauseAsync, Quit/QuitAsync, PostToThread/SendToThread, this is the place where the call is actually executed.
       - On Pause/PauseAsync OnBeforePause and OnSleep event handlers are called and the thread goes to sleep.
       - On Quit/QuitAsync OnBeforeExit handler is called and the thread's Execute method quits.
       - On PostToThread/SendToThread an appropriate procedure or method are called.
       *)
    procedure CancelationPoint;
    procedure SendToMain(Proc : TExtThreadProcedure); overload;
    procedure SendToMain(Method : TExtThreadMethod); overload;
    procedure PostToMain(Proc : TExtThreadProcedure; Sender : Pointer = nil; CanCancel : Boolean = True); overload;
    procedure PostToMain(Method : TExtThreadMethod; Sender : Pointer  = nil; CanCancel : Boolean = True); overload;
    (* Property: OnBeforePause
     OnBeforePause handler is called before the thread goes to sleep at CancellationPoint as requested by Pause or PauseAsync.
     The handler is executed in the context of the thread being paused, not the external thread.
    *)
    property OnBeforePause : TExtThreadMethod read FOnBeforePause write FOnBeforePause;
    (* Property: OnBeforeWake
     OnBeforeWake handler is called before the sleeping thread execution resumes due to calling Wake or some other condition.
     The handler is executed in the context of the thread being wakened, not the external thread.
    *)
    property OnBeforeWake : TExtThreadMethod read FOnBeforeWake write FOnBeforeWake;
    (* Property: OnBeforeExit
     OnBeforeExit handler is called before the thread terminates either due to call to ExitThread or due to call to Quit/QuitAsync.
     The handler is executed in the context of the thread being treminated, not the external thread.
    *)
    property OnBeforeExit : TExtThreadMethod read FOnBeforeWake write FOnBeforeWake;
    (* Property: OnBeforeSleep
     OnBeforeExit handler is called before the thread goes to sleep as a result of a Sleep call.
     You can use this handler to detect if the Sleep call was successful.
     The handler is executed in the context of the thread being treminated, not the external thread.
    *)
    property OnBeforeSleep : TExtThreadMethod read FOnBeforeSleep write FOnBeforeSleep;
  public
    (* if CreateSleeping is set to True the thread will run up to the first CancelationPoint and that sleep. *)
    constructor Create(CreateSleeping: Boolean);
    destructor Destroy; override;
    (* Function: Pause
       This method should be called from an external thread.
       Suspends the thread execution on the next CancelationPoint. Pause returns only after the thread is actually paused (the CancelationPoint has been reached within the thread's Execute Method) or when thread cannot go to sleep for some reason.
       If the thread is alredy in sleeping state Pause does nothing.
       Calls to Wake, Quit/QuitAsync, or SendToThread/PostToThread cause the thread to resume execution.
    *)
    procedure Pause;
    (* Function: PauseAsync
       This method should be called from an external thread.
       Suspends the thread execution. PauseAsync returns at once and the thread may still be running at the moment.
       If the thread is alredy in sleeping state Pause does nothing.
    *)
    procedure PauseAsync;
    (* Function: Wake
       This method should be called from an external thread.
       Wakes the thread after Sleep or Pause have ben called. *)
    procedure Wake;
    procedure Quit;
    procedure QuitAsync;
    (* if called after the call to QuitAsync, SendToThread does nothing *)
    procedure SendToThread(Proc : TExtThreadProcedure); overload;
    procedure SendToThread(Method : TExtThreadMethod); overload;
    procedure PostToThread(Proc : TExtThreadProcedure); overload;
    procedure PostToThread(Method : TExtThreadMethod); overload;
    property ThreadState  : TExtThreadState read FThreadState;
    property OnPause : TExtThreadEvent read FOnPause write FOnPause;
    property OnWake : TExtThreadEvent read FOnWake write FOnWake;
    property OnSleep : TExtThreadEvent read FOnSleep write FOnSleep;
  end;


implementation

type

  TEventRecord = record
    Proc : TExtThreadProcedure;
    Method: TExtThreadMethod;
    Sender : Pointer;
    Cancellable : Boolean;
  end;
  PEventRecord = ^TEventRecord;

  TExtrnEventHandler = class(TThread)
  private
    Events : TList;
    CS : TCriticalSection;
    _Evt : TEvent;
    CurrentEvent : PEventRecord;
    BlockedSender : TComponent;
    function GetEvent : PEventRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(Event : PEventRecord);
    procedure Execute; override;
    procedure ClearEvents(Sender : Pointer);
    procedure BlockEvents(Sender : Pointer);
    procedure UnblockEvents(Sender : Pointer);
  end;

var
  ExtrnEventHandler : TExtrnEventHandler;
  RCount : Integer;

  constructor TextThread.Create(CreateSleeping: Boolean);
  begin
    if CreateSleeping then
      FCancelationAction := caSleep
    else
      FCancelationAction := caNone;
    FLockedInt := 0;
    FLockedExt := 0;
    FThreadState := tsRunning;
    InitializeCriticalSection(FExlusive);
    FWake := CreateEvent(nil, True, False, nil);
    inherited Create(False);
    FEventList := TList.Create;
  end;

  destructor TextThread.Destroy;
  begin
    DeleteCriticalSection(FExlusive);
    CloseHandle(FWake);
    FEventList.Free;
    inherited;
  end;

  procedure TextThread.EnterExclusive;
  begin
    if Lock = 0 then
      EnterCriticalSection(FExlusive);
    Inc(Lock);
  end;

  procedure TextThread.LeaveExclusive;
  begin
    Dec(Lock);
    if Lock = 0 then
      LeaveCriticalSection(FExlusive);
  end;

  procedure TextThread.Resume;
  begin
  end;

  procedure TextThread.Suspend;
  begin
  end;

  procedure TextThread.Terminate;
  begin
  end;

  procedure TExtThread.CallExtThreadEvent;
  begin
    try
      FEventToCall(Self)
    finally
    end;
  end;

  procedure TExtThread.Sleep;
  begin
    EnterExclusive(FLockedInt);
    if FCancelationAction <> caNone then // Do not Sleep!
    begin
      LeaveExclusive(FLockedInt);
      Exit;
    end;
    ResetEvent(FWake);
    FThreadState := tsSleeping;
    if Assigned(FOnBeforeSleep) then FOnBeforeSleep;
    if Assigned(FOnSleep) then
    begin
      FEventToCall := FOnSleep;
      Synchronize(CallExtThreadEvent);
    end;
    LeaveExclusive(FLockedInt);
    WaitForSingleObject(FWake, INFINITE);
    ResetEvent(FWake);
    FThreadState := tsRunning;
  end;

  procedure TExtThread._Wake;
  begin
    SetEvent(FWake);
  end;

  procedure TExtThread.Wake;
  begin
    EnterExclusive(FLockedExt);
    if FCancelationAction = caSleep then FCancelationAction := caNone;
    _Wake;
    LeaveExclusive(FLockedExt);
  end;

  procedure TExtThread.PauseAsync;
  begin
    EnterExclusive(FLockedExt);
    if (FCancelationAction = caNone) and (FThreadState <> tsSleeping) then
       FCancelationAction := caSleep;
    _Wake;
    LeaveExclusive(FLockedExt);
  end;

  procedure TExtThread.Pause;
  begin
    PauseAsync;
    while FCancelationAction = caSleep do
    begin
      CheckSynchronize;
      SwitchToThread;
    end;
  end;

  procedure TExtThread.QuitAsync;
  begin
    EnterExclusive(FLockedExt);
    FCancelationAction := caTerminate;
    FThreadState := tsTerminating;
    _Wake;
    LeaveExclusive(FLockedExt);
  end;

  procedure TExtThread.Quit;
  begin
    QuitAsync;
    while ThreadState <> tsTerminated do
    begin
      CheckSynchronize; // CheckSynchronize(100)?
      SwitchToThread;
     end;
  end;

  procedure TExtThread.ExecuteEvents;
  var
    EvntRec : PEventRecord;
  begin
    while FEventList.Count <> 0 do
    begin
      EvntRec := FEventList.Items[0];
      FEventList.Delete(0);
     if Assigned(EvntRec.Proc) then EvntRec.Proc
     else if Assigned(EvntRec.Method) then EvntRec.Method;
     Dispose(EvntRec);
   end;
  end;

  procedure TExtThread.CancelationPoint;
  begin
    while FCancelationAction <> caNone do
    begin
      EnterExclusive(FLockedInt);
      if FCancelationAction = caSleep then
      begin
        if Assigned(FOnBeforePause) then FOnBeforePause;
        ResetEvent(FWake);
        FCancelationAction := caNone;
        FThreadState := tsSleeping;
        if Assigned(FOnPause) then
        begin
          FEventToCall := FOnPause;
          Synchronize(CallExtThreadEvent);
        end;
        LeaveExclusive(FLockedInt);
        WaitForSingleObject(FWake, INFINITE);
        ResetEvent(FWake);
        FThreadState := tsRunning;
        if Assigned(FOnBeforeWake) then FOnBeforeWake;
        if Assigned(FOnWake) then
        begin
          FEventToCall := FOnPause;
          Synchronize(CallExtThreadEvent);
        end;
        continue;
      end;
      if FCancelationAction = caTerminate then
      begin
        ExecuteEvents;
        if Assigned(FOnBeforeExit) then FOnBeforeExit;
        FThreadState := tsTerminated;
        LeaveExclusive(FLockedInt);
        Exit;
      end;
      if FCancelationAction = caExecute then
      begin
        ExecuteEvents;
        FCancelationAction := caNone;
      end;
      LeaveExclusive(FLockedInt);
    end;
  end;

  procedure TExtThread.ExitThread;
  begin
    EnterExclusive(FLockedInt);
    ExecuteEvents;
    if Assigned(FOnBeforeExit) then FOnBeforeExit;
    FThreadState := tsTerminated;
    FCancelationAction := caTerminate;
    LeaveExclusive(FLockedInt);
    Exit;
  end;

  procedure TExtThread.SendToMain(Proc: TExtThreadProcedure);
  begin
    EnterExclusive(FLockedInt);
    Synchronize(Proc);
    LeaveExclusive(FLockedInt);
  end;

  procedure TExtThread.SendToMain(Method : TExtThreadMethod);
  begin
    EnterExclusive(FLockedInt);
    Synchronize(Method);
    LeaveExclusive(FLockedInt);
  end;

  procedure TExtThread.PostToMain(Proc : TExtThreadProcedure; Sender : Pointer  = nil; CanCancel : Boolean = True);
  var
    Evt : PEventRecord;
  begin
    EnterExclusive(FLockedInt);
    New(Evt);
    Evt.Proc := Proc;
    Evt.Method := nil;
    Evt.Sender := Sender;
    Evt.Cancellable := CanCancel;
    ExtrnEventHandler.AddEvent(Evt);
    LeaveExclusive(FLockedInt);
  end;

  procedure TExtThread.PostToMain(Method : TExtThreadMethod; Sender : Pointer  = nil; CanCancel : Boolean = True);
  var
    Evt : PEventRecord;
  begin
    EnterExclusive(FLockedInt);
    New(Evt);
    Evt.Proc := nil;
    Evt.Method := Method;
    Evt.Sender := Sender;
    Evt.Cancellable := CanCancel;
    ExtrnEventHandler.AddEvent(Evt);
    LeaveExclusive(FLockedInt);
  end;

  procedure TExtThread.PostToThread(Proc: TExtThreadProcedure);
  var
    EvntRec : PEventRecord;
  begin
    EnterExclusive(FLockedExt);
    if not (FCancelationAction in [caNone, caSleep, caExecute]) then
    begin
      LeaveExclusive(FLockedExt);
      Exit;
    end;
    FCancelationAction := caExecute;
    New(EvntRec);
    EvntRec.Proc := Proc;
    EvntRec.Method := nil;
    FEventList.Add(EvntRec);
    _Wake;
    LeaveExclusive(FLockedExt);
  end;

  procedure TExtThread.SendToThread(Proc: TExtThreadProcedure);
  begin
    PostToThread(Proc);
    while FCancelationAction = caExecute do // TODO: Add event handler for each call
    begin
      CheckSynchronize; // CheckSynchronize(100)?
      SwitchToThread;
    end;
  end;

  procedure TExtThread.PostToThread(Method : TExtThreadMethod);
  var
    EvntRec : PEventRecord;
  begin
    EnterExclusive(FLockedExt);
    if not (FCancelationAction in [caNone, caSleep, caExecute]) then
    begin
      LeaveExclusive(FLockedExt);
      Exit;
    end;
    FCancelationAction := caExecute;
    New(EvntRec);
    EvntRec.Proc := nil;
    EvntRec.Method := Method;
    FEventList.Add(EvntRec);
    _Wake;
    LeaveExclusive(FLockedExt);
  end;

  procedure TExtThread.SendToThread(Method : TExtThreadMethod);
  begin
    PostToThread(Method);
    while FCancelationAction = caExecute do // TODO: Add event handler for each call
    begin
      CheckSynchronize; // CheckSynchronize(100)?
      SwitchToThread;
    end;
  end;

procedure CreateEventHandler;
begin
  if RCount = 0 then
  begin
    ExtrnEventHandler := TExtrnEventHandler.Create;
    ExtrnEventHandler.FreeOnTerminate := False;
  end;
  Inc(RCount);
end;

procedure ReleaseEventHandler;
begin
  Dec(RCount);
  if RCount = 0 then
  begin
    ExtrnEventHandler.Terminate;
    ExtrnEventHandler._Evt.Release;
    ExtrnEventHandler.WaitFor;
    ExtrnEventHandler.Free;
  end;
end;

  constructor TExtrnEventHandler.Create;
  begin
    inherited Create(False);
    Events := TList.Create;
    CS := TCriticalSection.Create;
    _Evt := TEvent.Create(nil, False, False, 'mainevent');
  end;

  destructor TExtrnEventHandler.Destroy;
  begin
    Events.Free;
    CS.Free;
    _Evt.Free;
  end;

  procedure TExtrnEventHandler.ClearEvents;
  var
    i : Integer;
    e : PEventRecord;
  begin
    CS.Enter;
    i := 0;
    while i < Events.Count do
    begin
      e := Events.Items[i];
      if e.Sender = Sender then
      begin
        Events.Extract(e);
        if not e.Cancellable then
        begin
          if Assigned(e.Proc) then
            Synchronize(e.Proc)
          else
            Synchronize(e.Method);
        end;
        Dispose(e);
      end
      else
        Inc(i);
    end;
    CS.Leave;
  end;

  procedure TExtrnEventHandler.BlockEvents;
  begin
    CS.Enter;
    BlockedSender := Sender;
    CS.Leave;
  end;

  procedure TExtrnEventHandler.UnblockEvents;
  begin
    CS.Enter;
    BlockedSender := nil;
    CS.Leave;
  end;


  procedure TExtrnEventHandler.AddEvent;
  begin
    CS.Enter;
    if Event.Sender <> BlockedSender then
      Events.Add(Event);
    CS.Leave;
    _Evt.SetEvent;
  end;

  function TExtrnEventHandler.GetEvent;
  begin
    CS.Enter;
    Result := Events.First;
    Events.Extract(Result);
    CS.Leave;
  end;

  procedure TExtrnEventHandler.Execute;
  begin
    while not Terminated do
    begin
      while Events.Count > 0 do
      begin
        CurrentEvent := GetEvent;
        if Assigned(CurrentEvent.Proc) then
          Synchronize(CurrentEvent.Proc)
        else
          Synchronize(CurrentEvent.Method);
        Dispose(CurrentEvent);
      end;
      _Evt.WaitFor(100);
    end;
  end;

initialization
   RCount := 0;
   CreateEventHandler;
finalization
   ReleaseEventHandler;
end.
