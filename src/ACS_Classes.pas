(*
  This file is a part of New Audio Components package 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Classes;

(* Title: ACS_Classes 
    Ancestor classes for all input and output components. *)

interface

uses

{$IFDEF WIN32}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  ACS_Tags;

type

  TOutputStatus = (tosPlaying, tosPaused, tosIdle);

  TFileOutputMode = (foRewrite = 0, foAppend);

  TOutputFunc = function(Abort : Boolean):Boolean of object;

  //TThreadDoneEvent = procedure of object;

  TThreadExceptionEvent = procedure(Sender : TComponent) of object;

  THandleException = procedure(Sender : TComponent; const Msg : String) of object;

  TOutputDoneEvent = procedure(Sender : TComponent) of object;

  TOutputProgressEvent = procedure(Sender : TComponent) of object;

{$IFDEF LINUX}
// File access mask constants
const

  famUserRead = 64;
  famUserWrite = 128;
  famGroupRead = 8;
  famGroupWrite = 16;
  famOthersRead = 1;
  famOthersWrite = 2;

{$ENDIF}

type

  EAuException = class(Exception)
  end;

  (* Class: TAuFileStream
      TFileStream analog that handles Unicode. *)

  TAuFileStream = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word); overload;
    constructor Create(const FileName: WideString; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
  end;

  (* Class: TAuThread
      Custom TThread descendant that does something. *)

  TAuThread = class(TThread)
  private
    ErrorMsg : String;
    procedure CallOnException;
    procedure RaiseDoneEvent;
  public
    DoNotify : Boolean; // Flag that determines if OnDone should be raised a the end of output
                        // default value is True, may be set to Dalse in Stop method.
    Stopped : Boolean;  // Flag that tells when the output is actually stopped.
                        // Used when DoNotify is set to False.
    Parent : TComponent;
//    bSuspend : Boolean;
    Stop : Boolean;
    HandleException : THandleException;
    Delay : Integer;
    procedure Execute; override;
  end;

(* Class: TAuInput
    The ancestor class for all input components. *)

  TAuInput = class(TComponent)
  protected
    FPosition : Int64;
    FSize : Int64; // total _uncompressed_ audio stream size in btes
    FSampleSize : Word; // size of one frame in bytes (fot 16 bps stereo FSampleSize is 4).
    Busy : Boolean;
    BufStart, BufEnd : Integer;
    DataCS : TCriticalSection;
    _EndOfStream : Boolean;
    (* We don't declare the buffer variable here
     because different descendants may need different buffer sizes *)
    function GetBPS : Integer; virtual; abstract;
    function GetCh : Integer; virtual; abstract;
    function GetSR : Integer; virtual; abstract;
    function GetTotalTime : Integer; virtual;
    procedure InitInternal; virtual; abstract;
    procedure FlushInternal; virtual; abstract;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData(var Buffer : Pointer; var Bytes : Integer); virtual;
    (* Function: CopyData 
        Writes no more than BufferSize data into Buffer 
        
      Parameters:
      
        Buffer: Pointer - the buffer to write to
        BufferSize: Integer - the number of bytes to write
    *)
    function CopyData(Buffer : Pointer; BufferSize : Integer) : Integer;
    (* Function: FillBuffer
        The same as <CopyData> but tries to fill the Buffer. EOF is set to 
        True if end of data was reached while filling the buffer, the buffer 
        itself may still contain valid data.
        
      Parameters:
      
        Buffer: Pointer - the buffer to write to
        BufferSize: Integer - the number of bytes to write      
        var EOF: Boolean - set to True if end of data was reached while filling 
          the buffer.
     
      Returns:
      
        Integer - Number of bytes written
    *)
    function FillBuffer(Buffer : Pointer; BufferSize : Integer; var EOF : Boolean) : Integer;
    procedure Reset; virtual;

    (* Procedure: Init
     This method prepares input component for reading data.
     Note: usually this method is called internally by the output or converter component to which the input component is assigned. You can call this method if you want to get direct access to the audio stream. In such a case the sequence of calls should look like this:

     InputComponent.Init;
     InputComponent.GetData(...); // in a loop
     InputComponent.Flush;
    *)
    procedure Init;// virtual;

    (*
    Procedure: Flush

    This method closes the current input (opened with <Init>), clearing up all temporary structures alocated during data transfer.
    Note: usually this method is called internally by the output or converter component to which the input component is assigned. You can call this method if you want to get direct access to the audio stream. In such a case the sequence of calls should look like this:

    InputComponent.Init;
    InputComponent.GetData(...); // in a loop
    InputComponent.Flush;
    *)
    procedure Flush;
    procedure _Lock;
    procedure _Unlock;
    procedure _Pause; virtual;
    procedure _Resume; virtual;
    property BitsPerSample : Integer read GetBPS;
    property Position : Int64 read FPosition;
    property SampleRate : Integer read GetSR;
    property Channels : Integer read GetCh;

    (* Property: Size
        A read only property which returns input data size in bytes.
        The value of tis property becomes valid after <Init> has been called.
        For some inputs (like <TDXAudioIn>) the data size may be not known in advance.
        In this case Size returns -1  *)
    property Size : Int64 read FSize;

    (* Property: TotalTime
        A read only property which returns input playback time in seconds.
        TotalTime value may be valid only if the <Size> of the input is known.
    *)
    property TotalTime : Integer read GetTotalTime;
  end;

(* Class: TAuOutput
    The ancestor class for all output components. *)

  TAuOutput = class(TComponent)
  protected
    FExceptionMessage : String;
    CanOutput : Boolean;
    CurProgr : Integer;
    Thread : TAuThread;
    FInput : TAuInput;
    FOnDone : TOutputDoneEvent;
    FOnProgress : TOutputProgressEvent;
    Busy : Boolean;  // Set to true by Run and to False by WhenDone.
    FOnThreadException : TThreadExceptionEvent;
   // InputLock : Boolean;
    function GetPriority : {$IFDEF LINUX} Integer; {$ENDIF} {$IFDEF WIN32} TThreadPriority; {$ENDIF}
//    function GetSuspend : Boolean;
    function GetProgress : Integer;
    procedure SetInput(vInput : TAuInput); virtual;
    procedure SetPriority(Priority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF});
//    procedure SetSuspend(v : Boolean);
    procedure WhenDone; // Calls descendant's Done method
    function GetTE : Integer;
    function GetStatus : TOutputStatus;
    function DoOutput(Abort : Boolean):Boolean; virtual; abstract;
    procedure Done; virtual; abstract; // Calls FInput.Flush
    procedure Prepare; virtual; abstract; // Calls FInput.init
    function GetDelay : Integer;
    procedure SetDelay(Value : Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleException(Sender : TComponent; const ErrorMessage : String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WIN32}
    procedure Abort;
    {$ENDIF}
    (* Procedure: Pause
       Pauses the output. *)
    procedure Pause;
    (* Procedure: Resume
        Resumes previously paused output. *)
    procedure Resume;
    (* Procedure: Run
        After an input component has been assigned, call Run to start audio-processing chain. *)
    procedure Run;
    (* Procedure: Stop
      Stops the busy component or does nothing if the component is idle. The
      method returns at once and OnDone event is raised when output is
      actually finished.

      Parameters:
         Async: Boolean = True - if this parameter value is set to True (the
         default) Stop is called in asynchronous mode. If this parameter is
         set to False the Stop method is called in blocking mode. It returns
         only after the output is actualy done. No event is raised in this
         case.
     *)
    procedure Stop(Async : Boolean = True);
    property Delay : Integer read GetDelay write SetDelay;
    property ThreadPriority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF} read GetPriority write SetPriority;
    property Progress : Integer read GetProgress;
    property Status : TOutputStatus read GetStatus;
    property TimeElapsed : Integer read GetTE;
    property ExceptionMessage : String read FExceptionMessage;
  published
    property Input : TAuInput read Finput write SetInput;
    property OnDone : TOutputDoneEvent read FOnDone write FOndone;
    property OnProgress : TOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException : TThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

(* Class: TAuStreamedInput
    A descendant of <TAuInput> to deal with streams. *)

  TAuStreamedInput = class(TAuInput)
  protected
    FStream : TStream;
    FStreamAssigned : Boolean;
    FSeekable : Boolean;
    procedure SetStream(aStream : TStream); virtual;
  public
    property Seekable : Boolean read FSeekable write FSeekable;
    property Stream : TStream read FStream write SetStream;
    constructor Create(AOwner: TComponent); override;
  end;

(* Class: TAuStreamedOutput
    A descendant of <TAuOutput> to deal with streams. *)

  TAuStreamedOutput = class(TAuOutput)
  protected
    FStream : TStream;
    FStreamAssigned : Boolean;
    procedure SetStream(aStream : TStream);
  public
    property Stream : TStream read FStream write SetStream;
  end;

  (* Class: TAuFileIn
      A descendant of <TAuStreamedInput> to deal with files. *)

  TAuFileIn = class(TAuStreamedInput)
  private
    procedure SetWideFileName(const FN : WideString);
    procedure SetFileName(const FN : TFileName);
  protected
    OpenCS : TCriticalSection;
    FFileName : TFileName; // Used only for display
    FWideFileName : WideString;
    FOpened : Integer;
    FValid : Boolean;
    FBPS : Integer;  // bits per sample
    FSR : Integer;   // sample rate
    FChan : Integer; // Number of channels
    FTime : Integer;
    FLoop : Boolean;
    FStartSample, FEndSample : Int64;
    FTotalSamples : Int64;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTime : Integer;
    function GetValid : Boolean;
    function GetTotalSamples : Int64;
    procedure SetStream(aStream : TStream); override;

    (* Note on FSize calculation:
      FSize is calculated in OpenFile method as the FULL file size.
      More precise calculations regarding StartSample/EndSample are done in Init. *)
     
    (* Procedure: OpenFile
        Opens the file in FileName if it is not already open. For performance 
        reasons the file is opened when any of its data is accessed the first 
        time and is then kept open until it is done with. The descendants' 
        FileOpen implementations use the FOpened constant to check if the file         is already opened. This method should fill FChan, FBPS, FSR, and FSize with values.
    *)
    procedure OpenFile; virtual; abstract;
    procedure CloseFile; virtual; abstract;
    function GetTotalTime : Integer; override;
    procedure Reset; override;
    function SeekInternal(var SampleNum : Int64) : Boolean; virtual; abstract;
    procedure FlushInternal; override;
    procedure InitInternal; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Seek(SampleNum : Int64) : Boolean;
    procedure GetData(var Buffer : Pointer; var Bytes : Integer); override;
    function SetStartTime(Minutes, Seconds : Integer) : Boolean;
    function SetEndTime(Minutes, Seconds : Integer) : Boolean;
    procedure Jump(Offs : Integer);
    property Time : Integer read GetTime;
    property TotalSamples : Int64 read GetTotalSamples;
    property Valid : Boolean read GetValid;
    (* Property: WideFileName
        Allows you to handle file names in Unicode. Setting its value
        overrides the value set to FileName. *)
    property WideFileName : WideString read FWideFileName write SetWideFileName;
  published
    property EndSample : Int64 read FEndSample write FEndSample;
    (* Property: Filename
        File name in 8-bit encoding. Setting this property's value overrides
        the value set to <WideFileName>. *)
    property FileName : TFileName read FFileName write SetFileName stored True;
    property Loop : Boolean read FLoop write FLoop;
    property StartSample : Int64 read FStartSample write FStartSample;
  end;

  TAuTaggedFileIn = class(TAuFileIn)
  private
    FId3v1Tags: TId3v1Tags;
    FId3v2Tags: TId3v2Tags;
    FAPEv2Tags: TAPEv2Tags;

    procedure SetId3v1Tags(Value: TId3v1Tags);
    procedure SetId3v2Tags(Value: TId3v2Tags);
    procedure SetAPEv2Tags(Value: TAPEv2Tags);
  protected
(* Ross--- *)
    property _Id3v1Tags: TId3v1Tags read FId3v1Tags write SetId3v1Tags;
    property _Id3v2Tags: TId3v2Tags read FId3v2Tags write SetId3v2Tags;
    property _APEv2Tags: TAPEv2Tags read FAPEv2Tags write SetAPEv2Tags;
(* ---Ross *)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  (* Class: TAuFileOut
      A descendant of <TAuStreamedOutput> to deal with files. *)

  TAuFileOut = class(TAuStreamedOutput)
  private
    procedure SetWideFileName(const FN : WideString);
    procedure SetFileName(const FN : TFileName);
  protected
    FFileName : TFileName; // Used for display only
    FWideFileName : WideString;
    FFileMode : TFileOutputMode;
    FAccessMask : Integer;
    procedure SetFileMode(aMode : TFileOutputMode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF LINUX}
    property AccessMask : Integer read FAccessMask write FAccessMask;
{$ENDIF}
    property WideFileName : WideString read FWideFileName write SetWideFileName;
  published
    property FileMode : TFileOutputMode read FFileMode write SetFileMode;
    property FileName : TFileName read FFileName write SetFileName;
  end;

  TAuTaggedFileOut = class(TAuFileOut)
  private
(* Ross--- *)
    FId3v1Tags: TId3v1Tags;
    FId3v2Tags: TId3v2Tags;
    FAPEv2Tags: TAPEv2Tags;

    procedure SetId3v1Tags(Value: TId3v1Tags);
    procedure SetId3v2Tags(Value: TId3v2Tags);
    procedure SetAPEv2Tags(Value: TAPEv2Tags);
(* ---Ross *)
  protected
(* Ross--- *)
    property Id3v1Tags: TId3v1Tags read FId3v1Tags write SetId3v1Tags;
    property Id3v2Tags: TId3v2Tags read FId3v2Tags write SetId3v2Tags;
    property APEv2Tags: TAPEv2Tags read FAPEv2Tags write SetAPEv2Tags;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
(* ---Ross *)
  end;

  TAuConverter = class(TAuInput)
  protected
    FInput : TAuInput;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInput(aInput : TAuInput); virtual;
  public
    procedure GetData(var Buffer : Pointer; var Bytes : Integer); override;
  published
    property Input : TAuInput read FInput write SetInput;
  end;

const

  STREAM_BUFFER_SIZE = $80000;

type

  // Circular buffer with TStream interface

  TACSBufferMode = (bmBlock, bmReport);

  TEventType = (etOnProgress, etOnDone);

  TEventRecord = record
    _type : TEventType;
    Sender : TComponent;
    case i : Integer of
      1 : (DoneEvent : TOutputDoneEvent; Thread : TAuThread;);
      2 : (ProgressEvent : TOutputProgressEvent;);
   end;

   PEventRecord = ^TEventRecord;

  TEventHandler = class(TThread)
  private
    Events : TList;
    CS : TCriticalSection;
    _Evt : TEvent;
    CurrentEvent : PEventRecord;
    BlockedSender : TComponent;
    procedure AddEvent(Event : PEventRecord);
    function GetEvent : PEventRecord;
    procedure CallHandler;
  public
    constructor Create;
    destructor Destroy;
    procedure PostOnProgress(Sender : TComponent; Handler :TOutputProgressEvent);
    procedure PostOnDone(Sender : TComponent; Thread : TAuThread; Handler :TOutputProgressEvent);
    procedure Execute; override;
    procedure ClearEvents(Sender : TComponent);
    procedure BlockEvents(Sender : TComponent);
    procedure UnblockEvents(Sender : TComponent);
  end;

procedure CreateEventHandler;
procedure ReleaseEventHandler;

var
  EventHandler : TEventHandler;


implementation

var
  RCount : Integer;

procedure CreateEventHandler;
begin
  if RCount = 0 then
    EventHandler := TEventHandler.Create;
  Inc(RCount);
end;

procedure ReleaseEventHandler;
begin
  Dec(RCount);
  if RCount = 0 then
    EventHandler.Free;

end;

  constructor TEventHandler.Create;
  begin
    inherited Create(False);
    Events := TList.Create;
    CS := TCriticalSection.Create;
    _Evt := TEvent.Create(nil, False, False, 'mainevent');
  end;

  destructor TEventHandler.Destroy;
  begin
    Events.Free;
    CS.Free;
    _Evt.Free;
  end;

  procedure TEventHandler.ClearEvents;
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
        Dispose(e);
      end
      else
        Inc(i);
    end;
    CS.Leave;
  end;

  procedure TEventHandler.BlockEvents;
  begin
    CS.Enter;
    BlockedSender := Sender;
    CS.Leave;
  end;

  procedure TEventHandler.UnblockEvents;
  begin
    CS.Enter;
    BlockedSender := nil;
    CS.Leave;
  end;


  procedure TEventHandler.AddEvent;
  begin
    CS.Enter;
    if Event.Sender <> BlockedSender then
      Events.Add(Event);
    CS.Leave;
    _Evt.SetEvent;
  end;

  function TEventHandler.GetEvent;
  begin
    CS.Enter;
    Result := Events.First;
    Events.Extract(Result);
    CS.Leave;
  end;

  procedure TEventHandler.CallHandler;
  begin
    if CurrentEvent._type = etOnDone then
        CurrentEvent.DoneEvent(CurrentEvent.Sender)
    else
    if CurrentEvent._type = etOnProgress then
        CurrentEvent.ProgressEvent(CurrentEvent.Sender);
  end;

  procedure TEventHandler.Execute;
  begin
    while not Terminated do
    begin
      while Events.Count > 0 do
      begin
        CurrentEvent := GetEvent;
        if CurrentEvent._type = etOnDone then
        begin
          while not CurrentEvent.Thread.Suspended do
            Sleep(10);
          ClearEvents(CurrentEvent.Sender);
        end;
        Synchronize(CallHandler);
        Dispose(CurrentEvent);
      end;
      _Evt.WaitFor(100);
    end;
  end;

  procedure TEventHandler.PostOnDone;
  var
    e : PEventRecord;
  begin
    New(e);
    e._type := etOnDone;
    e.Sender := Sender;
    e.Thread := Thread;
    e.DoneEvent := Handler;
    AddEvent(e);
  end;

  procedure TEventHandler.PostOnProgress;
  var
    e : PEventRecord;
  begin
    New(e);
    e._type := etOnProgress;
    e.Sender := Sender;
    e.DoneEvent := Handler;
    AddEvent(e);
  end;

  constructor TAuInput.Create;
  begin
    inherited Create(AOwner);
    DataCS := TCriticalSection.Create;
  end;

  destructor TAuInput.Destroy;
  begin
    DataCS.Free;
    inherited Destroy;
  end;

  procedure TAuInput.Init;
  begin
    DataCS.Enter;
    try
      _EndOfStream := False;
      InitInternal;
    finally
      DataCS.Leave;
    end;
  end;

  procedure TAuInput.Flush;
  begin
    DataCS.Enter;
    try
      FlushInternal;
    finally
      DataCS.Leave;
    end;
  end;

  procedure TAuInput.GetData;
  begin
    DataCS.Enter;
    try
      GetDataInternal(Buffer, Bytes);
    finally
      DataCS.Leave;
    end;
  end;



  procedure TAuThread.RaiseDoneEvent;
  begin
     if Assigned(TAuOutput(Parent).FOnDone) then
        EventHandler.PostOnDone(Parent, Self, TAuOutput(Parent).FOnDone);
   end;


  procedure TAuThread.Execute;
  var
    ParentComponent : TAuOutput;
    Res : Boolean;
  begin
    ParentComponent := TAuOutput(Parent);
    while not Terminated do
    begin
      if Delay > 5 then sleep(Delay);
      try
        if ParentComponent.Progress <> ParentComponent.CurProgr then
        begin
          ParentComponent.CurProgr := ParentComponent.Progress;
          if Assigned(ParentComponent.FOnProgress)
            then EventHandler.PostOnProgress(ParentComponent, ParentComponent.FOnProgress);
        end;
        Res := ParentComponent.DoOutput(Stop);
        if Stop or (not Res) then
        begin
          Stop := False;
          ParentComponent.WhenDone;
          if DoNotify then
            RaiseDoneEvent;
          Stopped := True;
          if not Terminated then Self.Suspend;
        end;
      except
        on E : Exception do
        begin
          ParentComponent.WhenDone;
          if DoNotify then
            RaiseDoneEvent;
          Stopped := True; // Stop := False;
          ErrorMsg := E.Message;
          Synchronize(CallOnException);
          if not Terminated then Self.Suspend;
        end;
      end;
    end;
  end;

  constructor TAuOutput.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
    Thread := TAuThread.Create(True);
    Thread.Parent := Self;
    Thread.DoNotify := True;
    Thread.FreeOnTerminate := True;
    Thread.HandleException := HandleException;
     CreateEventHandler;
    end;
  end;

  destructor TAuOutput.Destroy;
  begin
      if not (csDesigning in ComponentState) then
      begin
        Stop(False);
        ReleaseEventHandler;
      end;
    inherited Destroy;
  end;

  procedure TAuOutput.WhenDone;
  begin
    if not Busy then Exit;
    CanOutput := False;
    Done;
    Busy := False;
  end;

  procedure TAuOutput.Run;
  begin
    FExceptionMessage := '';
    if Busy then raise EAuException.Create('Component is Busy');
    if not Assigned(FInput) then raise EAuException.Create('Input is not assigned');
    try
      Busy := True;
      Prepare;
      Thread.Stop := False;
      CanOutput := True;
      if Thread.Suspended then Thread.Resume;
    except
      on E : Exception do
      begin
        try
          if not Thread.Suspended then
          begin
            Stop;
          end else
          begin
            WhenDone;
            Thread.RaiseDoneEvent;
          end;
        except
        end;
        Busy := False;
        HandleException(Self, E.Message);
      end;
    end;
  end;

  procedure TAuOutput.Stop;
  begin
    Thread.DoNotify := Async;
    Thread.Stopped := False;
    Thread.Stop := True;
    if not Async then
    begin
      EventHandler.BlockEvents(Self);
      EventHandler.ClearEvents(Self);
      while (not Thread.Suspended) and (not Thread.Stopped) do
      begin
        if Thread.Delay > 5 then sleep(Delay);
        CheckSynchronize; // to release possible deadlocks
      end;
      EventHandler.UnblockEvents(Self);
      Thread.DoNotify := True;
    end;
  end;

  function TAuOutput.GetStatus;
  begin
    if Busy then
    begin
      if Self.Thread.Suspended then Result := tosPaused
      else Result := tosPlaying;
    end else Result := tosIdle;
  end;

  procedure TAuOutput.SetPriority;
  begin
    Thread.Priority := Priority;
  end;

  function TAuOutput.GetPriority;
  begin
    Result := Thread.Priority;
  end;

  procedure TAuOutput.SetInput;
  begin
    if Busy then
    begin
      Stop(False);
      FInput := vInput;
      Run;
    end else
    FInput := vInput;
  end;

  function  TAuOutput.GetProgress;
  begin
    if not Assigned(Finput) then
    begin
      Result := 0;
      Exit;
    end;
    case Finput.Size of
      0: Result := 0;
      -1: Result := -1;
      else Result := Round((FInput.Position/FInput.Size)*100);
    end;
  end;

  procedure TAuOutput.Pause;
  begin
    if FInput = nil then
      raise EAuException.Create('Input is not assigned.');
    FInput._Lock;
    try
      FInput._Pause;
      If not Thread.Suspended then Thread.Suspend;
    finally
      FInput._Unlock;
    end;
  end;

  procedure TAuOutput.Resume;
  begin
    If Thread.Suspended and Busy then
    begin
      FInput._Resume;
      Thread.Resume;
    end;  
  end;


  constructor TAuStreamedInput.Create;
  begin
    inherited Create(AOwner);
    FSeekable := True;
  end;

  function TAuFileIn.GetBPS;
  begin
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FBPS;
    end else Result := FBPS;
  end;

  function TAuFileIn.GetCh;
  begin
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FChan;
    end else Result := FChan;
  end;

  function TAuFileIn.GetSR;
  begin
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FSR;
    end else Result := FSR;
  end;

  function TAuFileIn.GetTime;
  begin
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FTime;
    end else Result := FTime;
  end;

  function TAuFileIn.GetValid;
  begin
(* Ross---
    if (not FStreamAssigned) and (FileName = '') then
 ---Ross *)
(* Ross--- *)
    if (not FStreamAssigned) and (WideFileName = '') then
(* ---Ross *)
    begin
      Result := False;
    end else
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FValid;
    end else Result := FValid;
  end;

  procedure TAuFileIn.InitInternal;
  begin
    if Busy then raise EAuException.Create('The component is Busy');
    if not FStreamAssigned then
    if FWideFileName = '' then raise EAuException.Create('The file name is not assigned');
    Busy := True;

    FPosition := 0;

    OpenFile; // After calling this method we should know FChan, FBPS, FSR, and FSize

    FSampleSize := FChan*FBPS div 8;
    FTotalSamples := FSize div FSampleSize;
    FTime := FTotalSamples div FSR;

    if StartSample <> 0 then Seek(StartSample);
    if (StartSample <> 0) or (FEndSample <> -1) then
    begin
      if FEndSample = -1 then
        FTotalSamples :=  FTotalSamples - FStartSample + 1
      else
         FTotalSamples := FEndSample - FStartSample + 1;
    end;

    BufStart := 1;
    BufEnd := 0;
  end;

  procedure TAuFileIn.FlushInternal;
  begin
    CloseFile;
    FStartSample := 0;
    FEndSample := -1;
    Busy := False;
  end;

  function TAuFileIn.Seek;
  begin
    if not FSeekable then
    begin
      Result := False;
      Exit;
    end;
    DataCS.Enter;
    if not Busy then
    begin
      StartSample := SampleNum;
      FPosition := SampleNum*FSampleSize;
      EndSample := -1;
      Result := True;
    end else
    begin
      try
        Result := SeekInternal(SampleNum);
        FPosition := SampleNum*FSampleSize;
      except
      end;
    end;
    DataCS.Leave;
  end;

  procedure TAuFileIn.Jump;
  var
    Curpos : Double;
    Cursample : Integer;
  begin
    if (not FSeekable) or (FSize = 0) then Exit;
    Curpos := FPosition/FSize + offs/100;
    if Curpos < 0 then Curpos := 0;
    if Curpos > 1 then Curpos := 1;
    Cursample := Round(Curpos*FTotalSamples);
    Seek(Cursample);
  end;

  function TAuOutput.GetTE;
  begin
     if not Assigned(FInput) then
     Result := 0
     else
     Result := Round(FInput.Position/((FInput.BitsPerSample shr 3) *FInput.Channels*FInput.SampleRate));
  end;

  function TAuOutput.GetDelay;
  begin
    if Assigned(Thread) then Result := Thread.Delay;
  end;

  procedure TAuOutput.SetDelay;
  begin
    if Assigned(Thread) then
    if Value <= 100 then Thread.Delay := Value;
  end;

  function TAuInput.GetTotalTime;
  begin
    Result := 0;  // Default result for the streams.
  end;

  function TAuFileIn.GetTotalTime;
  begin
    OpenFile;
    if (SampleRate = 0) or (Channels = 0) or (BitsPerSample = 0) then Exit;
    Result := Round(Size/(SampleRate*Channels*(BitsPerSample shr 3)));
//    CloseFile;
  end;

  procedure TAuStreamedInput.SetStream;
  begin
    FStream := aStream;
    if FStream <> nil then FStreamAssigned := True
    else FStreamAssigned := False;
  end;

  procedure TAuStreamedOutput.SetStream;
  begin
    FStream := aStream;
    if FStream <> nil then FStreamAssigned := True
    else FStreamAssigned := False;
  end;

  procedure TAuOutput.Notification;
  begin
    // Remove the following two lines if they cause troubles in your IDE
    if (AComponent = FInput) and (Operation = opRemove )
    then Input := nil;
    inherited Notification(AComponent, Operation);
  end;

  procedure TAuInput.Reset;
  begin
    try
      Flush;
    except
    end;
    Busy := False;
  end;

  procedure TAuOutput.HandleException;
  begin
   CanOutput := False;
   Busy := False;
   Self.FExceptionMessage := ErrorMessage;
   if Assigned(FOnThreadException) then FOnThreadException(Self);
  end;

  procedure TAuFileIn.Reset;
  begin
    inherited Reset;
    FOpened := 0;
  end;


  constructor TAuFileOut.Create;
  begin
    inherited Create(AOwner);
    {$IFDEF LINUX}
    FAccessMask := $1B6; // rw-rw-rw-
    {$ENDIF}
  end;

  procedure TAuFileOut.SetFileMode;
  begin
    FFileMode := foRewrite;
  end;

  procedure TAuConverter.Notification;
  begin
    // Remove the following two lines if they cause troubles in your IDE
    if (AComponent = FInput) and (Operation = opRemove )
    then Input := nil;
    inherited Notification(AComponent, Operation);
  end;

  procedure TAuConverter.SetInput;
  begin
    if aInput = Self then Exit;
    if Busy then
    begin
      raise EAuException.Create('Converter components cannot change input on the fly.');
  (*    NewInput := aInput;
      NewInput.Init;
      OldInput := FInput;
      while InputLock do;
      InputLock := True;
      FInput := NewInput;
      InputLock := False;
      OldInput.Flush; *)
    end else
    FInput := aInput;
  end;

  function TAuFileIn.SetStartTime;
  var
    Sample : Integer;
  begin
    Result := False;
    if not FSeekable then Exit;
    OpenFile;
//    CloseFile;
    Sample := (Minutes*60+Seconds)*FSR;
    if Sample > FTotalSamples then Exit;
    FStartSample := Sample;
    Result := True;
  end;

  function TAuFileIn.SetEndTime;
  var
    Sample : Integer;
  begin
    Result := False;
    if not FSeekable then Exit;
    OpenFile;
//    CloseFile;
    Sample := (Minutes*60+Seconds)*FSR;
    if Sample > FTotalSamples then Exit;
    FEndSample := Sample;
    Result := True;
  end;

  constructor TAuFileIn.Create;
  begin
    inherited Create(AOwner);
    FStartSample := 0;
    FEndSample := -1;
    OpenCS := TCriticalSection.Create;
  end;

  destructor TAuFileIn.Destroy;
  begin
    CloseFile;
    OpenCS.Free;
    inherited Destroy;
  end;
 
  procedure TAuThread.CallOnException;
  begin
    HandleException(Parent as TComponent, ErrorMsg);
  end;

(*  procedure TAuThread.CallOnDone;
  begin
    if Assigned((Parent as TAuOutput).FOnDone) then
       (Parent as TAuOutput).FOnDone(Parent);
  end; *)

{$IFDEF WIN32}
  procedure TAuOutput.Abort;
  begin
    TerminateThread(Thread.Handle, 0);
    WhenDone;
  end;
{$ENDIF}

  function TAuInput.CopyData;
  var
    P : Pointer;
  begin
    Result := BufferSize;
    GetData(P, Result);
    if P <> nil then
      Move(P^, Buffer^, Result);
  end;

  function TAuInput.FillBuffer;
  var
    P : PByteArray;
    r : Integer;
  begin
    P := Buffer;
    r := BufferSize;
    Result := 0;
    while (BufferSize - Result > 0) and (r <> 0) do
    begin
      r := CopyData(@P[Result], BufferSize - Result);
      Result := Result + r;
    end;
    EOF := r = 0;
  end;
  
  constructor TAuFileStream.Create(const FileName: WideString; Mode: Word);
  begin
    Create(FileName, Mode, 0);
  end;

  constructor TAuFileStream.Create(const FileName: WideString; Mode: Word; Rights: Cardinal);
  const
    AccessMode: array[0..2] of LongWord = (
      GENERIC_READ,
      GENERIC_WRITE,
      GENERIC_READ or GENERIC_WRITE);
    ShareMode: array[0..4] of LongWord = (
      0,
      0,
      FILE_SHARE_READ,
      FILE_SHARE_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE);
  begin
    if Mode = fmCreate then
    begin
      inherited Create(
      CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
      if FHandle < 0 then
        raise EAuException.Create(Format('Cannot create file %s', [FileName]));
    end
    else
    begin
      inherited Create(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
      if FHandle < 0 then
        raise EAuException.Create(Format('Cannot open file %s', [FileName]));
    end;
  end;

  destructor TAuFileStream.Destroy;
  begin
    if FHandle >= 0 then FileClose(FHandle);
    inherited Destroy;
  end;

(* Ross--- *)
  procedure TAuTaggedFileIn.SetId3v1Tags(Value: TId3v1Tags);
  begin
    FId3v1Tags.Assign(Value);
  end;

  procedure TAuTaggedFileIn.SetId3v2Tags(Value: TId3v2Tags);
  begin
    FId3v2Tags.Assign(Value);
  end;

  procedure TAuTaggedFileIn.SetAPEv2Tags(Value: TAPEv2Tags);
  begin
    FAPEv2Tags.Assign(Value);
  end;

(* ---Ross *)
  procedure TAuFileIn.SetWideFileName;
  begin
    CloseFile;
//    StartSample := 0;
//    EndSample := -1;
    FWideFileName := FN;
    FFileName := '';
  end;

  procedure TAuFileIn.SetFileName;
  begin
    CloseFile;
//    StartSample := 0;
//    EndSample := -1;
    FWideFileName := FN;
    FFileName := FN;
  end;

(* Ross--- *)
  procedure TAuTaggedFileOut.SetId3v1Tags(Value: TId3v1Tags);
  begin
    FId3v1Tags.Assign(Value);
  end;

  procedure TAuTaggedFileOut.SetId3v2Tags(Value: TId3v2Tags);
  begin
    FId3v2Tags.Assign(Value);
  end;

  procedure TAuTaggedFileOut.SetAPEv2Tags(Value: TAPEv2Tags);
  begin
    FAPEv2Tags.Assign(Value);
  end;

(* ---Ross *)
  procedure TAuFileOut.SetWideFileName;
  begin
    FWideFileName := FN;
    FFileName := '';
  end;

  procedure TAuFileOut.SetFileName;
  begin
    FWideFileName := FN;
    FFileName := FN;
  end;

  constructor TAuTaggedFileIn.Create;
  begin
    inherited Create(AOwner);
(* Ross--- *)
    FId3v1Tags := TId3v1Tags.Create();
    FId3v2Tags := TId3v2Tags.Create();
    FAPEv2Tags := TAPEv2Tags.Create();
(* ---Ross *)
  end;

  destructor TAuTaggedFileIn.Destroy;
  begin
(* Ross--- *)
    FId3v1Tags.Free();
    FId3v2Tags.Free();
    FAPEv2Tags.Free();
(* ---Ross *)
    inherited Destroy;
  end;

  constructor TAuTaggedFileOut.Create;
  begin
    inherited Create(AOwner);
(* Ross--- *)
    FId3v1Tags := TId3v1Tags.Create();
    FId3v2Tags := TId3v2Tags.Create();
    FAPEv2Tags := TAPEv2Tags.Create();
(* ---Ross *)
  end;

  destructor TAuTaggedFileOut.Destroy;
  begin
    FId3v1Tags.Free;
    FId3v2Tags.Free;
    FAPEv2Tags.Free;
    inherited Destroy;
  end;

  function TAuFileIn.GetTotalSamples;
  begin
    OpenFile;
    Result := FTotalSamples;
//    CloseFile;
  end;

procedure TAuFileIn.SetStream;
begin
  CloseFile;
  inherited SetStream(aStream);
end;

procedure TAuInput._Lock;
begin
  DataCS.Enter;
end;

procedure TAuInput._Unlock;
begin
  DataCS.Leave;
end;

procedure TAuInput._Pause;
begin
// Nothing to do here, may be overridden in descendats.
end;

procedure TAuInput._Resume;
begin
// Nothing to do here, may be overridden in descendats.
end;


procedure TAuFileIn.GetData;
begin
  DataCS.Enter;
  try
    if _EndOfStream then
    begin
      Buffer :=  nil;
      Bytes := 0;
    end else
    begin
      GetDataInternal(Buffer, Bytes);
      if Bytes = 0 then
        _EndOfStream := True
      else
      begin
        Inc(FPosition, Bytes);
        if (FSize > 0) and (FPosition >= FSize) then
          _EndOfStream := True;
      end;
      if _EndOfStream and FLoop and FSeekable then
      begin
        _EndOfStream := False;
        SeekInternal(FStartSample);
        FPosition := 0;
      end;
    end;
  finally
    DataCS.Leave;
  end;
end;

procedure TAuConverter.GetData;
begin
  DataCS.Enter;
  try
    if _EndOfStream then
    begin
      Buffer :=  nil;
      Bytes := 0;
    end else
    begin
      GetDataInternal(Buffer, Bytes);
      if Bytes = 0 then
        _EndOfStream := True
    end;
  finally
    DataCS.Leave;
  end;
end;



initialization

 RCount := 0;

 //EventHandler := TEventHandler.Create;

finalization

//  if EventHandler <> nil then
//    EventHandler.Free;

end.
