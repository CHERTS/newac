(*
  This file is a part of New Audio Components package v 1.8.1
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Misc;

(* Title: ACS_Misc
    Some input components which descend directly from <TAuInput> or <TAuOutput>.  *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, SyncObjs

 {$IFDEF WIN32}
  , Windows
  {$ENDIF}

  {$IFDEF LINUX}
  , libc, smpeg, LibAO
  {$ENDIF};

const
  BUF_SIZE = $4000;

type

  TBuffer = array[0..0] of Byte;
  PBuffer = ^TBuffer;

  TOnBufferDone = procedure(Sender : TComponent; var DataBuffer : Pointer; var DataSize : LongWord; var RepeatCount : Integer) of object;

  TAudioProcessorInitEvent = procedure(Sender : TComponent; var TotalSize : Int64) of object;
  TAudioProcessorFlushEvent = procedure(Sender : TComponent) of object;

  TGetParameterEvent64 = procedure(Sender : TComponent; var Param : Int64) of object;
  TGetParameterEvent32 = procedure(Sender : TComponent; var Param : LongWord) of object;

  TGetDataEvent = procedure(Sender : TComponent; var Buffer : Pointer; var Bytes : LongWord) of object;

  (* Class: TMemoryIn
    A descendant of <TAuInput> which reads audio data from a memory block that
    you provide. It is analogous to TStreamIn when reading from TMemoryStream,
    the only difference is that a pointer to a memory block is used instead of
    a TMemoryStream object. *)

  TMemoryIn = class(TAuInput)
  private
    FBuffer : PBuffer;
    FDataSize : LongWord;
    Busy : Boolean;
    BufStart, BufEnd : LongWord;
    FBPS, FSR, FChan : LongWord;
    FRepeatCount : Integer;
    FOnBufferDone : TOnBufferDone;
    function GetBuffer : Pointer;
    procedure SetBuffer(v : Pointer);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: DataBuffer
      Use this property to assign a pointer pointing to a data block, audio
      data will be read from. The data block IS NOT created by this component.
      You create it and fill it with data. You must not free the memory block
      pointed to by DataBuffer until you get an OnDone event from the output
      component that reads from this input. The memory block pointed to by
      DataBuffer should be <DataSize> in length. *)
    property DataBuffer : Pointer read GetBuffer write SetBuffer;
    (* Property: DataSize
      Use this property to set the size of the <DataBuffer> in bytes. *)
    property DataSize : LongWord read FDataSize write FDataSize;
  published
    (* Property: InBitsPerSample
      Use this property to tell the component the number of bits per sample
      for the audio data stored in the <DataBuffer>. *)
    property InBitsPerSample : LongWord read GetBPS write FBPS;
    (* Property: InChannels
      Use this property to tell the component the number of channels for the
      audio data stored in the <DataBuffer>. *)
    property InChannels : LongWord read GetCh write FChan;
    (* Property: InSampleRate
      Use this property to tell the component the sample rate of the audio
      data stored in the <DataBuffer>. *)
    property InSampleRate : LongWord read GetSR write FSR;
    (* Property: RepeatCount
      Use this property to tell the component how many times the contents of
      the <DataBuffer> should be replayed before the component reports the end
      of data. The default value for this property is 1. If this property is
      set to -1 the component will replay the buffer endlessly until it is
      stopped. *)
    property RepeatCount : Integer read FRepeatCount write FRepeatCount;
    (* Property: OnBufferDone
      This event is raised when the comoponent has played its current buffer contents and is about to report the end of input.
      Using this property you can renew data buffer so that the component continues playback.
      The event hander arguments are the pointer to the new buffer, its length and the repeat count.
      You can assign new values to these arguments or leave the previous ones.
      If <RepeatCount> is greater than 1, the OnBufferDone event handler is called only after the contents of the buffer has been repeated <RepeatCount> number of times.
      The event handler will never be called if <RepeatCount> is -1. *)
    property OnBufferDone : TOnBufferDone read FOnBufferDone write FOnBufferDone;
  end;

  TAudioProcessor = class(TAuConverter)
  private
    FOnInit : TAudioProcessorInitEvent;
    FOnFlush : TAudioProcessorFlushEvent;
    FOnGetData : TGetDataEvent;
    FOnGetSampleRate : TGetParameterEvent32;
    FOnGetBitsPerSample : TGetParameterEvent32;
    FOnGetChannels : TGetParameterEvent32;
    FOnGetTotalTime : TGetParameterEvent32;
    FOnGetSize : TGetParameterEvent64;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    function GetTotalTime : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
  published
    property OnFlush : TAudioProcessorFlushEvent read FOnFlush write FOnFlush;
    property OnGetBitsPerSample : TGetParameterEvent32 read FOnGetBitsPerSample write FOnGetBitsPerSample;
    property OnGetChannels : TGetParameterEvent32 read FOnGetChannels write FOnGetChannels;
    property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
    property OnGetSampleRate : TGetParameterEvent32 read FOnGetSampleRate write FOnGetSampleRate;
    property OnGetSize : TGetParameterEvent64 read FOnGetSize write FOnGetSize;
    property OnGetTotalTime : TGetParameterEvent32 read FOnGetTotalTime write FOnGetTotalTime;
    property OnInit : TAudioProcessorInitEvent read FOnInit write FOnInit;
  end;

  {$IFDEF LINUX}

  TMPEGIn = class(TACSFileIn)
  private
    _M : Pointer;
    buf : array[1..BUF_SIZE] of Byte;  // ring buffer
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
  end;

  TAOLive = class(TACSOutput)
  private
    _device : PAODevice;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    FVolume : Byte;
    FDrivers : TStringList;
    FCurrentDriver, FDefaultDriver : String;
    procedure SetDriver(const aDriver : String);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsDevicePlayable(const Dev : String) : Boolean;
    property DefaultDriver : String read FDefaultDriver;
    property Driver : String read FCurrentDriver write SetDriver;
    property Drivers : TStringList read FDrivers;
  published
    property Volume : Byte read FVolume write FVolume stored True;
  end;

{$ENDIF}

  TNULLOut = class(TAuOutput)
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  end;

  (* Class: TInputItem

     Descends from TCollectionItem.

     Objects of this class are the elements of the <TInputList.InputItems>
     collection. *)

  TInputItem = class(TCollectionItem)
  protected
    FInput : TAuInput;
    function GetOwner : TPersistent; override;
  published
    (* Property: Input
       Use this property to assign an input component to the collection
       element. If a playing TInputList component reaches a TInputItem without
       an Input assigned, it raises an exception. *)
    property Input : TAuInput read FInput write FInput;
  end;

  TInputItems = class(TOwnedCollection)
  end;

  TInputChangedEvent = procedure(Sender : TComponent) of object;

  (* Class: TInputList

     Descends from <TAuInput>.

     This component can play consecutively audio data from several attached
     input components. It is not a good choice for building a player's
     playlist as all the input audio sources attached to the component must
     have the same audio stream parameters (sample rate, bits per sample,
     number of channels), it is rather a tool for concatenating audio data
     from several different sources as the audio is played seamlessly when the
     component switches from one input source to the other. *)

  TInputList = class(TAuInput)
  private
    FCurrentInput : Integer;
    FInputItems : TInputItems;
    CS : TCriticalSection;
    FOnInputChanged : TInputChangedEvent;
    FIndicateProgress : Boolean;
    procedure SetCurrentInput(aInput : Integer);
    procedure SetInputItems(aItems : TInputItems);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: CurrentInput
       Use this property to get the index of the audio source being played or
       to set the index of the audio source to be played. If you assign a new
       value to this property the new input will start playing at once. The
       valid values for this property range from 0  to <InputItems>.Count - 1
       *)
    property CurrentInput : Integer read FCurrentInput write SetCurrentInput;
  published
    (* Property: IndicateProgress
       Use this property to tell the component if it should report playback
       progress on the current item being played. *)
    property IndicateProgress : Boolean read FIndicateProgress write FIndicateProgress;
    (* Property: InputItems
       This property is the collection of <TInputItem> elements that describe
       attached input components. *)
    property InputItems : TInputItems read FInputItems write SetInputItems;
    (* Property: OnInputChanged
       Raised when the component has finished playing one input source and is
       starting to play the next one. It is not raised when you change the
       value of the <CurrentInput> property. *)
    property OnInputChanged : TInputChangedEvent read FOnInputChanged write FOnInputChanged;
  end;


implementation

{$IFDEF LINUX}

var
  AOInitialized : Integer = 0;

{$ENDIF}

  constructor TMemoryIn.Create;
  begin
    inherited Create(AOwner);
    FRepeatCount := 1;
  end;

  destructor TMemoryIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TMemoryIn.GetBPS;
  begin
    if (FBPS in [8, 16, 24, 32]) = False  then FBPS := 16;
    Result := FBPS;
  end;

  function TMemoryIn.GetCh;
  begin
    Result := FChan;
  end;

  function TMemoryIn.GetSR;
  begin
    if (FSR < 2000) or (FSR > 96000) then FSR := 8000;
    Result := FSR;
  end;

  procedure TMemoryIn.InitInternal;
  begin
    FPosition := 0;
    BufEnd := FDataSize;
    BufStart := 0;
    if FRepeatCount >= 0 then
    {$WARNINGS OFF}
      FSize := FDataSize*FRepeatCount
    else
    {$WARNINGS ON}
      FSize := -1;
    Busy := True;
  end;

  procedure TMemoryIn.FlushInternal;
  begin
    Busy := False;
    FDataSize := 0;
  end;

  procedure TMemoryIn.GetDataInternal;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if not Assigned(FBuffer) then
    begin
      Bytes := 0;
      Buffer := nil;
      Exit;
    end;
    if BufStart >= BufEnd then
    begin
      if (FDataSize = 0) or ((FSize > 0) and (FPosition >= FSize)) then
      begin
        if Assigned(FOnBufferDone) then
        begin
          FOnBufferDone(Self, Pointer(FBuffer), FDataSize, FRepeatCount);
          {$WARNINGS OFF}
          if (FBuffer = nil) or (FDataSize*FRepeatCount = 0) then
          {$WARNINGS ON}
          begin
            Bytes := 0;
            Buffer := nil;
            Exit;
          end;
        end else
        begin
          Bytes := 0;
          Buffer := nil;
          Exit;
        end;
      end;
      BufStart := 0;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @FBuffer[BufStart];
    Inc(BufStart, Bytes);
    Inc(FPosition, Bytes);
  end;

  function TMemoryIn.GetBuffer;
  begin
    Result := Pointer(FBuffer);
  end;

  procedure TMemoryIn.SetBuffer;
  begin
    FBuffer := PBuffer(v);
  end;


{$IFDEF LINUX}

  constructor TMPEGIn.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TMPEGIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMPEGIn.Init;
  begin
    inherited Init;
    SMPEG_play(_M);
  end;

  procedure TMPEGIn.OpenFile;
  var
    info : SMPEG_info;
    spec : SDL_AudioSpec;
  begin
    if FOpened = 0 then
    begin
      LoadLibrary;
      (* the next call is needed just to make sure
        the SDL library is loaded *)
      _M := SMPEG_new(PChar(FFileName), info, 1);
      SMPEG_delete(_M);
      FValid := True;
      _M := SMPEG_new(PChar(FFileName), info, 0);
      if info.has_audio <> 1 then
      begin
        FValid := False;
        Exit;
      end;
      FTime := Round(info.total_time);
      SMPEG_wantedSpec(_M, spec);
      FSR := spec.freq;
      FBPS := 16;
      FChan := spec.channels;
      FSize := FTime*2*FChan*FSR;
    end;
    Inc(FOpened);
  end;

  procedure TMPEGIn.CloseFile;
  begin
    if FOpened = 1 then
    begin
      if SMPEG_status(_M) = SMPEG_PLAYING then
      SMPEG_stop(_M);
      SMPEG_delete(_M);
      UnloadLibrary;
    end;
    if FOpened > 0 then Dec(FOpened);
  end;

  function TMPEGIn.GetData;
  var
    l, offs : Integer;
    tmp : Single;
  begin
    if not Busy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if FOffset <> 0 then
      begin
        offs := Round((FOffset/100)*FSize);
        FPosition := FPosition + offs;
        if FPosition < 0 then FPosition := 0
        else if FPosition > FSize then FPosition := FSize;
        if FOffset < 0 then
        begin
          SMPEG_rewind(_M);
          SMPEG_play(_M);
          tmp := (FPosition/FSize)*FTime;
          SMPEG_skip(_M, tmp);
        end;
        tmp := (FOffset/100)*FTime;
        SMPEG_skip(_M, tmp);
        FOffset := 0;
      end;
      BufStart := 1;
      FillChar(Buf, SizeOf(Buf), 0);
      l := SMPEG_playAudio(_M, @Buf[1], BUF_SIZE);
      if l = 0 then
      begin
        if FLoop then
        begin
          Flush;
          Init;
//          SMPEG_rewind(_M);
//          SMPEG_play(_M);
//          FPosition := 0;
          l := SMPEG_playAudio(_M, @Buf[1], BUF_SIZE);
        end else
        begin
          Result := 0;
          Exit;
        end;
      end;
      BufEnd := l;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(Buf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  constructor TAOLive.Create;
  var
    DrList : PPAOInfo;
    DrCount, i : Integer;
    Info : PAOInfo;
  begin
    if not LibaoLoaded then
    raise EACSException.Create(LibaoPath + ' library could not be loaded.');
    inherited Create(AOwner);
    if AOInitialized = 0 then
    ao_initialize;
    Inc(AOInitialized);
    FDrivers := TStringList.Create;
    DrList := ao_driver_info_list(DrCount);
    for i := 0 to DrCount-1 do
    begin
      if DrList^._type = AO_TYPE_LIVE then
      begin
        FDrivers.Add(String(DrList^.short_name));
      end;
      Inc(DrList);
    end;
    Info := ao_driver_info(ao_default_driver_id);
    FDefaultDriver := Info.short_name;
    FVolume := 255;
  end;

  destructor TAOLive.Destroy;
  begin
    FDrivers.Free;
    if AOInitialized = 1 then
    ao_shutdown;
    Dec(AOInitialized);
    inherited Destroy;
  end;

  procedure TAOLive.Prepare;
  var
    did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
    Info : PAOInfo;
  begin
    FInput.Init;
    if FCurrentDriver = '' then
    begin
      did := ao_default_driver_id;
      Info := ao_driver_info(did);
      FCurrentDriver := Info.short_name;
    end
    else did := ao_driver_id(@FCurrentDriver[1]);
    opt := nil;
    sf.bits := Finput.BitsPerSample;
    sf.rate := Finput.SampleRate;
    sf.channels := Finput.Channels;
    sf.byte_format := AO_FMT_NATIVE;
    _device := ao_open_live(did, @sf, opt);
    FreeOptionsList(Opt);
    if _device = nil then
    raise EACSException.Create('Cannot play on the "'+FCurrentDriver+'" device.');
  end;

  procedure TAOLive.Done;
  begin
    Finput.Flush;
    if _device <> nil then
    ao_close(_device);
  end;

function TAOLive.DoOutput;
var
  Len, i : Integer;
  P : Pointer;
  P1 : PBuffer8;
  P2 : PBuffer16;
begin
  // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  Len := 0;
  if Abort then
  begin
    ao_close(_device);
    _device := nil;
    Result := False;
    Exit;
  end;
  try
    P := @Buffer[0];
    Len := Finput.GetData(P, BUF_SIZE);
    if FVolume < 255 then
    begin
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @Buffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := Round(P2[i]*(FVolume/255));
      end else
      begin
        P1 := @Buffer[0];
        for i := 0 to Len - 1 do
        P1[i] := Round(P1[i]*(FVolume/255));
      end;
    end;
    ao_play(_device, P, Len);
  except
  end;
  if Len > 0 then Result := True
  else Result := False;
end;

  procedure TAOLive.SetDriver;
  begin
    if IsDevicePlayable(aDriver) then
    FCurrentDriver := aDriver;
  end;

  function TAOLive.IsDevicePlayable;
  var
    i, did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
  begin
    Result := True;
    if Dev = '' then Exit;
    if Busy then
    raise EACSException.Create('Component is busy.');
    for i := 0 to FDrivers.Count-1 do
    if FDrivers.Strings[i] = Dev then
    begin
      did := ao_driver_id(@Dev[1]);
      sf.bits := 16;
      sf.rate := 22050;
      sf.channels := 2;
      sf.byte_format := AO_FMT_NATIVE;
      opt := nil;
      _device := ao_open_live(did, @sf, opt);
      if _device <> nil then
      begin
        ao_close(_device);
        FreeOptionsList(Opt);
        Exit;
      end else Break;
    end;
    Result := False;
  end;

{$ENDIF}


  function TAudioProcessor.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetBitsPerSample) then FOnGetBitsPerSample(Self, Result) else
    Result := FInput.BitsPerSample;
  end;

  function TAudioProcessor.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetSampleRate) then FOnGetSampleRate(Self, Result) else
    Result := FInput.SampleRate;
  end;

  function TAudioProcessor.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetChannels) then FOnGetChannels(Self, Result) else
    Result := FInput.Channels;
  end;

  function TAudioProcessor.GetTotalTime;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetTotalTime) then FOnGetTotalTime(Self, Result) else
    Result := FInput.TotalTime;
  end;

  procedure TAudioProcessor.GetDataInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetData) then FOnGetData(Self, Buffer, Bytes)
    else FInput.GetData(Buffer, Bytes);
   Inc(FPosition, Bytes);
  end;

  procedure TAudioProcessor.InitInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnInit) then FOnInit(Self, FSize)
    else
    begin
      FInput.Init;
      if Assigned(FOnGetSize) then FOnGetSize(Self, FSize)
      else FSize := Finput.Size;
    end;
    Busy := True;
    FPosition := 0;
  end;

  procedure TAudioProcessor.FlushInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnFlush) then FOnFlush(Self)
    else FInput.Flush;
    Busy := False;
  end;

procedure TNULLOut.Prepare;
begin
  if not Assigned(FInput) then
  raise EAuException.Create('Input is not assigned.');
  FInput.Init;
end;

function TNULLOut.DoOutput;
var
  Res : LongWord;
  Ptr : Pointer;
begin
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  Res := BUF_SIZE;
  Finput.GetData(Ptr, Res);
  if Res > 0 then Result := True
  else
  begin
    Result := False;
    Exit;
  end;
end;

procedure TNULLOut.Done;
begin
  FInput.Flush;
end;

function TInputItem.GetOwner;
begin
  Result := Collection;
end;

constructor TInputList.Create;
begin
  inherited Create(AOwner);
  FInputItems := TInputItems.Create(Self, TInputItem);
  FPosition := 0;
  FSize := -1;
  FIndicateProgress := True;
  if not (csDesigning in ComponentState) then
    CS := TCriticalSection.Create;
end;

destructor TInputList.Destroy;
begin
  if not (csDesigning in ComponentState) then
  CS.Free;
  FInputItems.Free;
  Inherited Destroy;
end;

procedure TInputList.SetCurrentInput;
var
  I : TInputItem;
begin
  if aInput <> 0 then
  if (aInput < 0) or (aInput >= FInputItems.Count) then
  raise EAuException.Create('List index out of bounds: ' + IntToStr(aInput));
  if Busy then
  begin
    CS.Enter;
    I := TInputItem(InputItems.Items[FCurrentInput]);
    I.Input.Flush;
    I := TInputItem(InputItems.Items[aInput]);
    I.Input.Init;
    if FIndicateProgress then
    FSize := I.Input.Size
    else FSize := -1;
    FPosition := 0;
    CS.Leave;
  end;
  FCurrentInput := aInput;
end;

function TInputList.GetBPS;
var
  I : TInputItem;
begin
  Result := 0;
  if Busy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.BitsPerSample;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.BitsPerSample;
  end;
end;

function TInputList.GetCh;
var
  I : TInputItem;
begin
  Result := 0;
  if Busy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.Channels;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.Channels;
  end;
end;

function TInputList.GetSR;
var
  I : TInputItem;
begin
  Result := 0;
  if Busy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.SampleRate;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.SampleRate;
  end;
end;

procedure TInputList.InitInternal;
var
  I : TInputItem;
begin
  if Busy then
  raise EAuException.Create('The component is busy.');
  if InputItems.Count = 0 then
  raise EAuException.Create('No input items in the list.');
  I := TInputItem(InputItems.Items[FCurrentInput]);
  if not Assigned(I.Input) then
  raise EAuException.Create('No input assigned to the input item '+IntToStr(FCurrentInput));
  Busy := True;
  I.Input.Init;
  if FIndicateProgress then
  FSize := I.Input.Size
  else FSize := -1;
  FPosition := 0;
end;

procedure TInputList.FlushInternal;
var
  I : TInputItem;
begin
  I := TInputItem(InputItems.Items[FCurrentInput]);
  I.Input.Flush;
  FCurrentInput := 0;
  Busy := False;
end;

procedure TInputList.GetDataInternal;
var
  I : TInputItem;
  BytesTmp : LongWord;
begin
  CS.Enter;
  try
    BytesTmp := Bytes;
    I := TInputItem(InputItems.Items[FCurrentInput]);
    I.Input.GetData(Buffer, Bytes);
    while Bytes = 0 do
    begin
      if FCurrentInput < InputItems.Count -1 then
      begin
        I.Input.Flush;
        Inc(FCurrentInput);
        if Assigned(FonInputChanged) then
          EventHandler.PostGenericEvent(Self, FOnInputChanged);
        I := TInputItem(InputItems.Items[FCurrentInput]);
        if not Assigned(I.Input) then
          raise EAuException.Create('No input assigned to the input item '+IntToStr(FCurrentInput));
        I.Input.Init;
        if FIndicateProgress then
          FSize := I.Input.Size
        else FSize := -1;
        FPosition := 0;
        Bytes := BytesTmp;
        I.Input.GetData(Buffer, Bytes);
      end else Break;
    end;
    if FIndicateProgress then
      FPosition := I.Input.Position;
  finally
    CS.Leave;
  end;
end;

procedure TInputList.SetInputItems;
begin
  FInputItems.Assign(aItems);
end;
end.
