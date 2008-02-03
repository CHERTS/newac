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

  TGenericEvent = procedure(Sender : TComponent) of object;

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
    PauseEvent : TEvent;
    procedure CallOnException;
    procedure RaiseDoneEvent;
  public
    DoNotify : Boolean; // Flag that determines if OnDone should be raised a the end of output
                        // default value is True, may be set to False in Stop method.
    Stopped : Boolean;  // Flag that tells when the output is actually stopped.
                        // Used when DoNotify is set to False.
    Parent : TComponent;
//    bSuspend : Boolean;
    Stop : Boolean;
    SetPause, Paused : Boolean;
    HandleException : THandleException;
    Delay : Integer;
    constructor Create;
    destructor Destroy; override;
    procedure WaitForPause;
    procedure Execute; override;
  end;

(* Class: TAuInput
    The ancestor class for all input components. *)

  TAuInput = class(TComponent)
  protected
    FPosition : Int64;
    FSize : Int64; // total _uncompressed_ audio stream size in bytes
    FSampleSize : Word; // size of one frame in bytes (fot 16 bps stereo FSampleSize is 4).
    Busy : Boolean;
    BufStart, BufEnd : LongWord;
    DataCS : TCriticalSection;
    _EndOfStream : Boolean;
    (* We don't declare the buffer variable here
     because different descendants may need different buffer sizes *)
    function GetBPS : LongWord; virtual; abstract;
    function GetCh : LongWord; virtual; abstract;
    function GetSR : LongWord; virtual; abstract;
    function GetTotalTime : LongWord; virtual;
    function GetTotalSamples : Int64; virtual;
    procedure InitInternal; virtual; abstract;
    procedure FlushInternal; virtual; abstract;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Procedure: GetData
        This method retrieves input data. You specify the number of bytes you
        want to get, but you may get less and it should not be considered as
        an end of input indication. When the end of input is reached GetData
        sets Buffer to nil and Bytes to 0.


      Parameters:

        Buffer - This is the variable where GetData will put a pointer to a
          data buffer. Unlike many other data reading functions GetData
          doesn't take our buffer pointer but provides you with its own.
        Bytes - When you call GetData you pass to Bytes the number of bytes
          you want to get. When the method returns the Bytes variable holds
          the number of bytes in the Buffer.

      Note:
      Usually you should not call this method directly.
    *)
    procedure GetData(var Buffer : Pointer; var Bytes : LongWord); virtual;
    (* Function: CopyData 
        Writes no more than BufferSize data into Buffer 
        
      Parameters:
      
        Buffer: Pointer - the buffer to write to
        BufferSize: Integer - the number of bytes to write
    *)
    function CopyData(Buffer : Pointer; BufferSize : Integer) : LongWord;
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
    function FillBuffer(Buffer : Pointer; BufferSize : LongWord; var EOF : Boolean) : LongWord;
    procedure Reset; virtual;

    (* Procedure: Init
     This method prepares input component for reading data. Usually this
     method is called internally by the output or converter component to which
     the input component is assigned. You can call this method if you want to
     get direct access to the audio stream. In such a case the sequence of
     calls should look like this.

  > InputComponent.Init;
  > InputComponent.GetData(...); // in a loop
  > InputComponent.Flush;
    *)
    procedure Init;// virtual;

    (* Procedure: Flush

    This method closes the current input (opened with <Init>), clearing up all
    temporary structures allocated during data transfer. Usually this method is
    called internally by the output or converter component to which the input
    component is assigned. You can call this method if you want to get direct
    access to the audio stream. In such a case the sequence of calls should
    look like this.

  > InputComponent.Init;
  > InputComponent.GetData(...); // in a loop
  > InputComponent.Flush;
    *)
    procedure Flush;
    procedure _Lock;
    procedure _Unlock;
    procedure _Pause; virtual;
    procedure _Resume; virtual;
    (* Property: BitsPerSample
       The number of bits per sample in the input stream. Possible values are 8, 16, and 24.*)
    property BitsPerSample : LongWord read GetBPS;
    (* Property: Position
       The current reading position in the input stream in bytes.*)
    property Position : Int64 read FPosition;
    (* Property: SampleRate
       The input stream sample rate in Herz.*)
    property SampleRate : LongWord read GetSR;
    (* Property: Channels
       The number of channels in the input stream. Possible values are 1 (mono), 2 (stereo)... and may be more.*)
    property Channels : LongWord read GetCh;

    (* Property: Size
        A read only property which returns input data size in bytes.
        The value of this property becomes valid after <Init> has been called.
        For some inputs (like <TDXAudioIn>) the data size may be not known in advance. In this case Size returns -1  *)
    property Size : Int64 read FSize;

    (* Property: TotalSamples
        A read only property which returns number of samples (frames) in the input stream.
        TotalSamples value may be valid only if the <Size> of the input is known.
    *)
     property TotalSamples : Int64 read GetTotalSamples;
    (* Property: TotalTime
        A read only property which returns input playback time in seconds.
        TotalTime value may be valid only if the <Size> of the input is known.
    *)
    property TotalTime : LongWord read GetTotalTime;
  end;

(* Class: TAuOutput
    The ancestor class for all output components. *)

  TAuOutput = class(TComponent)
  protected
    FStopped : Boolean; // indicates that the output is terminated by calling Stop
                        // So that Done could know the output is stopped forcibly. Currently only TWaveOut usese this.
                        // Set to True by Stop and to False in WhenDone.
    FExceptionMessage : String;
    CanOutput : Boolean;
    CurProgr : Integer;
    Thread : TAuThread;
    FInput : TAuInput;
    FOnDone : TOutputDoneEvent;
    FOnProgress : TOutputProgressEvent;
    Busy : Boolean;  // Set to true by Run and to False by WhenDone.
    FOnThreadException : TThreadExceptionEvent;
    function GetPriority : {$IFDEF LINUX} Integer; {$ENDIF} {$IFDEF WIN32} TThreadPriority; {$ENDIF}
    function GetProgress : Integer;
    procedure SetInput(vInput : TAuInput); virtual;
    procedure SetPriority(Priority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF});
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
        After an input component has been assigned, call Run to start the audio processing chain. When called, Run returns at once while the actual audio processing goes on in the different thread. You will get <OnProgress> events while audio processing continues and an <OnDone> event when the job is done.*)
    procedure Run;
    (* Procedure: Stop
      Stops the busy component or does nothing if the component is idle.

      Parameters:
         Async: Boolean = True - If this parameter value is set to True (the
         default), Stop is called in an asynchronous mode. In this mode the
         method returns at once and OnDone event is raised when output is
         actually finished.
         If this parameter is set to False the Stop method
         is called in blocking mode. In this mode it returns only after the output
         is actually done. No event is raised in this case.*)
    procedure Stop(Async : Boolean = True);
    property Delay : Integer read GetDelay write SetDelay;
    (* Property: ThreadPriority
        This property allows you to set the priority of the output thread.*)
    property ThreadPriority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF} read GetPriority write SetPriority;
    (* Property: Progress
        Read Progress to get the output progress in percents.
        This value is meaningful only after the input component has been set
        and only if the input component can tell the size of its stream.*)
    property Progress : Integer read GetProgress;
    (* Property: Status
        This read only property indicates the output component's current status.
        Possible values are:

        tosPlaying: the component is performing its task;
        tosPaused: the component is paused (the Pause method was called);
        tosIdle: the component is idle;*)
    property Status : TOutputStatus read GetStatus;
    (* Property: TimeElapsed
       The time in seconds that has passed since the playback was started.
       Useful for real time components like <TDXAudioOut>.*)
    property TimeElapsed : Integer read GetTE;
    (* Property: ExceptionMessage
       Most exceptions that may occur during NewAC operation are suppressed.
       If an exception occurs, the operation is stopped and the <OnThreadException> event is raised.
       ExceptionMessage holds the exception  text.*)
    property ExceptionMessage : String read FExceptionMessage;
  published
    (* Property: Input
       This property allows you to set the input component for the output component.
       The valid input components must be descendants of TAuInput.*)
    property Input : TAuInput read Finput write SetInput;
    (* Property: OnDone
       OnDone event is raised when the component has finished its job or was stopped asynchronously.
       From this event handler you can perform any action on the output component, even remove the component itself!*)
    property OnDone : TOutputDoneEvent read FOnDone write FOndone;
    (* Property: OnProgress
       OnProgress event is raised periodically to indicate output progress.
       Use <Progress> property to get the progress value.
       OnProgress event is sent asynchronously and you can perform any action on the output component from the event handler.*)
    property OnProgress : TOutputProgressEvent read FOnProgress write FOnProgress;
    (* Property: OnThreadException
       This event is raised if an exception has occurred.*)
    property OnThreadException : TThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

(* Class: TAuStreamedInput
    A descendant of <TAuInput> to deal with streams. *)

  TAuStreamedInput = class(TAuInput)
  protected
    FStream : TStream;
    FStreamAssigned : Boolean;
    FSeekable : Boolean;
    FStartSample, FEndSample : Int64;
    FLoop : Boolean;
    FTotalSamples : Int64;
    procedure SetStream(aStream : TStream); virtual;
    (* You have to override the SeekInternal method whether your input component is seekable or not.
      if your component is  not seekable then you can write a method like this:
      function TMyComponent.SeekInternal(var SampleNum : Int64) : Boolean;
      begin
        Result := False;
      end;
      If you want to make your component seekable you have to implement a real seeking in this function. *)
    function SeekInternal(var SampleNum : Int64) : Boolean; virtual; abstract;
     (* Property: EndSample
        Set this property's value to the sample (frame) you want the input to
        stop playing at. By default it is set to -1 which indicates "play to
        the end of input." Changing this property value has an effect only
        when the component is idle. *)
    property EndSample : Int64 read FEndSample write FEndSample;
    (* Property: Loop
        If set to True, the input loops (i.e. starts again from the beginning
        after it is finished). *)
    property Loop : Boolean read FLoop write FLoop;
    (* Property: StartSample
        Set this property's value to the sample (frame) you want the input to
        start playing from. By default it is set to 0. Calling the <Seek> method when the component is idle has the same effect.
        Note that when you set StartSample and <EndSample> properties you define a subrange of the input data.
        All further operations, such as playback and <Seek>ing will be performed within this subrange.
        The StartSample and <EndSample> values also affect the <TotalSamples> and <Size> values, returned by the component.*)
    property StartSample : Int64 read FStartSample write FStartSample;
  public


    (* Property: Seekable
      This read only property indicates when the input is seekable. *)
    property Seekable : Boolean read FSeekable;
    (* Property: Stream
      Use this property to set the input data stream for the input component.
      Any TStream descendant may be used as a data source. Note that if you
      set Stream, you own it, that is you have to create, destroy and position
      the stream explicitly. In TAuFileIn descendants the value assigned to
      this property takes over the FileName property, i. e. if both Stream and
      FileName properties are assigned, the stream and not the file will be
      used for the actual input. To unassign this property set it to nil. If
      the stream is seekable it will be reset to the beginning at the end of
      the playback. *)
    property Stream : TStream read FStream write SetStream;
    procedure GetData(var Buffer : Pointer; var Bytes : LongWord); override;
    (* Function: Seek
        This method allows you to change the current playing position in the
        the input component. If the input component is stopped or paused,
        calling Seek sets the sample from which the playback will begin. Note
        that not all inputs are seekable.

      Parameters:
        SampleNum - The number of sample (frame) to play from. This number is
          set relative to the value of <StartSample>.

      Returns:
        Boolean - A False value indicates that either a seek failed (you are
          seeking beyond the end of file or the <EndSample> value) or that input stream is not
          seekable.
    *)
    function Seek(SampleNum : Int64) : Boolean;
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
      A descendant of <TAuStreamedInput> to deal with files and streams.
      All the components that read files descend from this class.*)

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
    FBPS : LongWord;  // bits per sample
    FSR : LongWord;   // sample rate
    FChan : LongWord; // Number of channels
    FTime : LongWord;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
//    function GetTime : Integer;
    function GetValid : Boolean;
    function GetTotalSamples : Int64; override;
    procedure SetStream(aStream : TStream); override;

    (* Note on FSize calculation:
      FSize is calculated in OpenFile method as the FULL file size.
      More precise calculations regarding StartSample/EndSample are done in Init. *)

    (* Procedure: OpenFile
        Opens the file or stream if it is not already open. For performance
        reasons the file is opened when any of its data is accessed the first
        time and is then kept open until it is done with. The descendants'
        FileOpen implementations use the FOpened constant to check if the file
        is already opened.

        Note:
        This method is called internally by <TAuInput.Init>, you should never
        call it directly. *)
    procedure OpenFile; virtual; abstract;
    (* Procedure: CloseFile
        Closes the file opened with <OpenFile>. Sets the FOpened constant to
        0.

        Note:
        This method is called internally by <TAuInput.Flush>, you should never
        call it directly. *)
    procedure CloseFile; virtual; abstract;
    function GetTotalTime : LongWord; override;
    procedure FlushInternal; override;
    procedure InitInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: SetStartTime
      This function is a wrapper around StartSample property, provided for convenience.
      It allows you to set playback start position in minutes and seconds.
      Parameters:
        Minutes
        Seconds
    Note: calls
    >SetStartTime(1, 30);
    and
    >SetStartTime(0, 90);
    are both allowed.
    *)
    function SetStartTime(Minutes, Seconds : LongWord) : Boolean;
    (* Function: SetEndTime
      This function is a wrapper around EndSample property, provided for convenience.
      It allows you to set playback stop position in minutes and seconds.
      Parameters:
        Minutes
        Seconds
    *)
    function SetEndTime(Minutes, Seconds : LongWord) : Boolean;
    (* Procedure: Jump
        This method, being a wrapper around <Seek>, simpifies navigation in the input stream.
        Calling Jump moves you backward or forward relative to the current position.
        Jump may be called either before starting playback (in this case the playback will be started from the position specified) or during the playback.
      Parameters:
        Offs - the amount of file contents, in percent, that will be skipped.
        Positive value skips forward, negative value skips backward.
        For example calling Jump(-100) always sets the playing position at the beginning of the file.
        Note: Use <Seek> for more exact positioning.
    *)
    procedure Reset; override;
    procedure Jump(Offs : Integer);
//    property Time : Integer read GetTime;
    (* Property: Valid
      Read this property to determine if the file is valid.
      It is a good practice to check this property before performing other operations on audio stream.
      Note however that True returned by Valid doesn't guarantee the file is fully playable.
      It indicates only that the fille could be opened successfully and the file headers were correct. *)
    property Valid : Boolean read GetValid;
    (* Property: WideFileName
        Allows you to handle file names in Unicode. Setting its value
        overrides the value set to FileName. *)
    property WideFileName : WideString read FWideFileName write SetWideFileName;
  published
    (* Property: Filename
        File name in 8-bit encoding. Setting this property's value overrides
        the value set to <WideFileName>. *)
    property FileName : TFileName read FFileName write SetFileName stored True;
  end;

  (* Class: TAuTaggedFileIn
      Descends from <TAuFileIn>, this class is an ancestor of the file input components that use ID3V* tags. *)

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
      A descendant of <TAuStreamedOutput> to deal with files and streams. *)

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
    (* Property: FileMode
       This property can take one of two values foRewrite (default) and
       foAppend. In the foRewrite mode the new file overwrites the previous
       one (if it existed). In the foAppend mode the new content is added to
       the existing. Currently only <TWaveOut> and <TVorbisOut> components
       support this mode. *)
    property FileMode : TFileOutputMode read FFileMode write SetFileMode;
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF LINUX}
    property AccessMask : Integer read FAccessMask write FAccessMask;
{$ENDIF}
    (* Property: WideFileName
        Allows you to handle file names in Unicode. Setting its value
        overrides the value set to FileName. *)
    property WideFileName : WideString read FWideFileName write SetWideFileName;
  published
    (* Property: Filename
        File name in 8-bit encoding. Setting this property's value overrides
        the value set to <WideFileName>. *)
    property FileName : TFileName read FFileName write SetFileName;
  end;

  (* Class: TAuTaggedFileOut
      Descends from <TAuFileOut>, this class is an ancestor of the file output
      components that use Id3v* tags. *)

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

   (* Class: TAuConverter
      Descends from <TAuInput>, the base class for all converter components. *)

  TAuConverter = class(TAuInput)
  protected
    FInput : TAuInput;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInput(aInput : TAuInput); virtual;
  public
    procedure GetData(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure _Pause; override;
    procedure _Resume; override;
  published
    (* Property: Input
       Like the output components converters can be assigned an input.
       Unlike the output components converters themselves can be input sources (for output components and other converters. *)
    property Input : TAuInput read FInput write SetInput;
  end;

const

  STREAM_BUFFER_SIZE = $80000;

type

  // Circular buffer with TStream interface

  TACSBufferMode = (bmBlock, bmReport);

  TEventType = (etOnProgress, etOnDone, etGeneric);

  TEventRecord = record
    _type : TEventType;
    Sender : TComponent;
    case i : Integer of
      1 : (DoneEvent : TOutputDoneEvent; Thread : TAuThread;);
      2 : (ProgressEvent : TOutputProgressEvent;);
      3 : (GenericEvent : TGenericEvent;);
   end;

   PEventRecord = ^TEventRecord;

  (*
   This is an internal class, no urgent need to document it.
   *)

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
    destructor Destroy; override;
    procedure PostOnProgress(Sender : TComponent; Handler :TOutputProgressEvent);
    procedure PostOnDone(Sender : TComponent; Thread : TAuThread; Handler :TOutputProgressEvent);
    procedure PostGenericEvent(Sender : TComponent; Handler : TGenericEvent);
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
  begin
    EventHandler := TEventHandler.Create;
    EventHandler.FreeOnTerminate := False;
  end;
  Inc(RCount);
end;

procedure ReleaseEventHandler;
begin
  Dec(RCount);
  if RCount = 0 then
  begin
    EventHandler.Terminate;
    EventHandler._Evt.Release;
    EventHandler.WaitFor;
    EventHandler.Free;
  end;
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
        CurrentEvent.ProgressEvent(CurrentEvent.Sender)
    else
    if CurrentEvent._type = etGeneric then
        CurrentEvent.GenericEvent(CurrentEvent.Sender);
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
    e.ProgressEvent := Handler;
    AddEvent(e);
  end;

  procedure TEventHandler.PostGenericEvent;
  var
    e : PEventRecord;
  begin
    New(e);
    e._type := etGeneric;
    e.Sender := Sender;
    e.GenericEvent := Handler;
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
    except
      _EndOfStream := True;
    end;
    DataCS.Leave;
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

  constructor TAuThread.Create;
  begin
    inherited Create(True);
    PauseEvent := TEvent.Create(nil, False, False, 'pause_event');
  end;

  destructor TAuThread.Destroy;
  begin
    PauseEvent.Free;
    inherited Destroy;
  end;

  procedure TAuThread.WaitForPause;
  begin
    PauseEvent.WaitFor(10000);
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
        if SetPause then
        begin
          if (not Paused) and (not Stop) then
            PauseEvent.SetEvent;
          Paused := True;
          Res := True;
          Sleep(50);
        end else
        begin
          if Paused and (not Stop) then
            PauseEvent.SetEvent;
          Paused := False;
          Res := ParentComponent.DoOutput(Stop);
        end;  
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
      FStopped := False;
      Thread := TAuThread.Create;
      Thread.Parent := Self;
      Thread.DoNotify := True;
      Thread.FreeOnTerminate := False;
      Thread.HandleException := HandleException;
     CreateEventHandler;
    end;
  end;

  destructor TAuOutput.Destroy;
  begin
      if not (csDesigning in ComponentState) then
      begin
        Stop(False);
        Thread.Terminate;
        while Thread.Suspended do
          Thread.Resume;
        Thread.WaitFor;
        Thread.Free;
        ReleaseEventHandler;
      end;
    inherited Destroy;
  end;

  procedure TAuOutput.WhenDone;
  begin
    if not Busy then Exit;
    CanOutput := False;
    Done;
    FStopped := False;
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
      if Thread.Suspended then
      begin
        Thread.SetPause := False;
        Thread.Paused := False;
        Thread.Resume;
        Thread.PauseEvent.ResetEvent;
      end;
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
    FStopped := True;
    Thread.DoNotify := Async;
    Thread.Stopped := False;
    Thread.Stop := True;
    if Thread.Paused then
      Thread.SetPause := False;
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
      if Self.Thread.Paused then Result := tosPaused
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
    If Busy then
    begin
      Thread.SetPause := True;
      Thread.WaitForPause;
      FInput._Pause;
    end;
  end;

  procedure TAuOutput.Resume;
  begin
    If Busy then
    begin
      FInput._Resume;
      Thread.SetPause := False;
      Thread.WaitForPause;
    end;
  end;


  constructor TAuStreamedInput.Create;
  begin
    inherited Create(AOwner);
    FStartSample := 0;
    FEndSample := -1;
    FSeekable := True;
    FTotalSamples := 0;
  end;

  function TAuFileIn.GetBPS;
  begin
    OpenFile; // Open the file if it is not already opened
    Result := FBPS;
  end;

  function TAuFileIn.GetCh;
  begin
    OpenFile; // Open the file if it is not already opened
    Result := FChan;
  end;

  function TAuFileIn.GetSR;
  begin
    OpenFile; // Open the file if it is not already opened
    Result := FSR;
  end;

(*  function TAuFileIn.GetTime;
  begin
    if FSeekable then
    begin
      OpenFile; // Open the file if it is not already opened
      Result := FTime;
    end else Result := FTime;
  end;*)

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

    if FStartSample > 0 then
    begin
     Seek(FStartSample);
     FPosition := 0;
    end; 
    if (FStartSample > 0) or (FEndSample <> -1) then
    begin
      if FEndSample > FTotalSamples then FEndSample := -1;
      if FEndSample = -1 then
        FTotalSamples :=  FTotalSamples - FStartSample + 1
      else
         FTotalSamples := FEndSample - FStartSample + 1;
      FSize := FTotalSamples*FSampleSize;
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

  function TAuStreamedInput.Seek;
  begin
    Result := False;
    if not FSeekable then
    begin
      Exit;
    end;
    if (FTotalSamples <> 0) and (SampleNum > FTotalSamples)  then
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
        Inc(SampleNum, FStartSample);
        Result := SeekInternal(SampleNum);
        FPosition := (SampleNum - FStartSample)*FSampleSize;
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
    if Assigned(Thread) then Result := Thread.Delay
    else Result := 0;
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
    Result := 0;
    OpenFile;
    if (SampleRate = 0) or (Channels = 0) or (BitsPerSample = 0) then Exit;
    Result := Round(Size/(SampleRate*Channels*(BitsPerSample shr 3)));
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
    _Lock;
    try
      if Busy then
      begin
        FInput.Flush;
        aInput.Init;
        FInput := aInput;
      end else
      FInput := aInput;
    finally
      _Unlock;
    end;  
  end;

  function TAuFileIn.SetStartTime;
  var
    Sample : LongWord;
  begin
    Result := False;
    if not FSeekable then Exit;
    OpenFile;
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
    Sample := (Minutes*60+Seconds)*FSR;
    if Sample > FTotalSamples then Exit;
    FEndSample := Sample;
    Result := True;
  end;

  constructor TAuFileIn.Create;
  begin
    inherited Create(AOwner);
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
    r : LongWord;
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
    if FSize > 0 then
    FTotalSamples := FSize div (FChan*FBPS div 8)
    else FTotalSamples := -1;
    Result := FTotalSamples;
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
// Nothing to do here, may be overridden in descendants.
end;

procedure TAuInput._Resume;
begin
// Nothing to do here, may be overridden in descendants.
end;


procedure TAuStreamedInput.GetData;
var
  tmpBytes : LongWord;
begin
  DataCS.Enter;
  tmpBytes := Bytes;
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
      if _EndOfStream and FLoop then
      begin
        _EndOfStream := False;
        if FSeekable then
          SeekInternal(FStartSample)
        else
        begin
          Flush;
          Init;
        end;
        FPosition := 0;
        Bytes := tmpBytes;
        GetDataInternal(Buffer, Bytes);
        if Bytes = 0 then
          _EndOfStream := True
          else
          begin
            Inc(FPosition, Bytes);
              if (FSize > 0) and (FPosition >= FSize) then
            _EndOfStream := True;
         end;
      end;  // if _EndOfStream and FLoop then
    end; // if _EndOfStream then ... else
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
      Inc(FPosition, Bytes);
      if Bytes = 0 then
        _EndOfStream := True
    end;
  finally
    DataCS.Leave;
  end;
end;

procedure TAuConverter._Pause;
begin
  FInput._Pause;
end;

procedure TAuConverter._Resume;
begin
  FInput._Resume;
end;

function TAuInput.GetTotalSamples;
begin
  Result := 0;
end;  

initialization

 RCount := 0;

 //EventHandler := TEventHandler.Create;

finalization

//  if EventHandler <> nil then
//    EventHandler.Free;

end.
