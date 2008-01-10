(*
  This file is a part of New Audio Components package v 1.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Revision: 1.4 $ $Date: 2007/09/04 17:03:11 $ *)

unit ACS_Misc;

(* Title: ACS_Misc
    Miscellaneous classes which don't fit anywhere else.  *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes

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

  TOnBufferDone = procedure(Sender : TComponent) of object;

  TAudioProcessorInitEvent = procedure(Sender : TComponent; var TotalSize : Int64) of object;
  TAudioProcessorFlushEvent = procedure(Sender : TComponent) of object;

  TGetParameterEvent64 = procedure(Sender : TComponent; var Param : Int64) of object;
  TGetParameterEvent32 = procedure(Sender : TComponent; var Param : LongWord) of object;

  TGetDataEvent = procedure(Sender : TComponent; var Buffer : Pointer; var Bytes : LongWord) of object;

  TMemoryIn = class(TAuInput)
  private
    FBuffer : PBuffer;
    FDataSize : Integer;
    FOnBufferDone : TOnBufferDone;
    Busy : Boolean;
    BufStart, BufEnd : LongWord;
    FBPS, FSR, FChan : LongWord;
    function GetBuffer : Pointer;
    procedure SetBuffer(v : Pointer);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    property DataBuffer : Pointer read GetBuffer write SetBuffer;
    property DataSize : Integer read FDataSize write FDataSize;
  published
    property GlobalSize : Int64 read FSize write FSize;
    property InBitsPerSample : LongWord read GetBPS write FBPS;
    property InChannels : LongWord read GetCh write FChan;
    property InSampleRate : LongWord read GetSR write FSR;
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

  TInputItem = class(TCollectionItem)
  protected
    FInput : TAuInput;
    function GetOwner : TPersistent; override;
  published
    property Input : TAuInput read FInput write FInput;
  end;

  TInputItems = class(TOwnedCollection)
  end;

  TInputChangedEvent = procedure(Sender : TComponent; var Index : Integer; var Continue : Boolean) of object;

  TInputList = class(TAuInput)
  private
    FCurrentInput : Integer;
    FInputItems : TInputItems;
    {$IFDEF WIN32}
    CS : TRTLCriticalSection;
    {$ENDIF}
    FOnInputChanged : TInputChangedEvent;
    FIndicateProgress : Boolean;
    procedure SetCurrentInput(aInput : Integer);
    procedure SetInputItems(aItems : TInputItems);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    property CurrentInput : Integer read FCurrentInput write SetCurrentInput;
  published
    property IndicateProgress : Boolean read FIndicateProgress write FIndicateProgress;
    property InputItems : TInputItems read FInputItems write SetInputItems;
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
    FSize := -1;
  end;

  destructor TMemoryIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TMemoryIn.GetBPS;
  begin
    if (FBPS in [8, 16]) = False  then FBPS := 16;
    Result := FBPS;
  end;

  function TMemoryIn.GetCh;
  begin
    if (FChan in [1..2]) = False then FChan := 1;
    Result := FChan;
  end;

  function TMemoryIn.GetSR;
  begin
    if (FSR < 4000) or (FSR > 48000) then FSR := 8000;
    Result := FSR;
  end;

  procedure TMemoryIn.InitInternal;
  begin
    FPosition := 0;
    BufEnd := FDataSize;
    BufStart := 1;
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
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      if FDataSize = 0 then
      begin
        if Assigned(FOnBufferDone) then FOnBufferDone(Self)
        else
        begin
          Bytes := 0;
          Buffer := nil;
          Exit;
        end;
      end;
      BufEnd := FDataSize;
      if FDataSize = 0 then
      begin
        Bytes := 0;
        Buffer := nil;
        Exit;
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @FBuffer[BufStart-1];
    Inc(BufStart, Bytes);
    Inc(FPosition, Bytes);
    Dec(FDataSize, Bytes);
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
  {$IFDEF WIN32}
  InitializeCriticalSection(CS);
  {$ENDIF}
end;

destructor TInputList.Destroy;
begin
  FInputItems.Free;
  {$IFDEF WIN32}
  DeleteCriticalSection(CS);
  {$ENDIF}
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
    {$IFDEF WIN32}
    EnterCriticalSection(CS);
    {$ENDIF}
    I := TInputItem(InputItems.Items[FCurrentInput]);
    I.Input.Flush;
    I := TInputItem(InputItems.Items[aInput]);
    I.Input.Init;
    if FIndicateProgress then
    FSize := I.Input.Size
    else FSize := -1;
    FPosition := 0;
    {$IFDEF WIN32}
    LeaveCriticalSection(CS);
    {$ENDIF}
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
  Continue : Boolean;
  OriginalBytes : Integer;
begin
  {$IFDEF WIN32}
  EnterCriticalSection(CS);
  {$ENDIF}
  I := TInputItem(InputItems.Items[FCurrentInput]);
  OriginalBytes := Bytes;
  Bytes := I.Input.CopyData(Buffer, OriginalBytes);
  while Bytes = 0 do
  begin
    if FCurrentInput < InputItems.Count -1 then
    begin
      I.Input.Flush;
      Inc(FCurrentInput);
      Continue := True;
      if Assigned(FonInputChanged) then
      FonInputChanged(Self, FCurrentInput, Continue);
      if Continue then
      begin
        I := TInputItem(InputItems.Items[FCurrentInput]);
        if not Assigned(I.Input) then
        raise EAuException.Create('No input assigned to the input item '+IntToStr(FCurrentInput));
        I.Input.Init;
        if FIndicateProgress then
        FSize := I.Input.Size
        else FSize := -1;
        FPosition := 0;
        Bytes := I.Input.CopyData(Buffer, OriginalBytes);
      end else Break;
    end else Break;
  end;
  if FIndicateProgress then
  FPosition := I.Input.Position;
  {$IFDEF WIN32}
  LeaveCriticalSection(CS);
  {$ENDIF}
end;

procedure TInputList.SetInputItems;
begin
  FInputItems.Assign(aItems);
end;
end.
