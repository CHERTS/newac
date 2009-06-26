(*
  This file is a part of New Audio Components package 2.0
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit AuASIO;

interface

uses
  SysUtils, Classes, Forms, SyncObjs, ACS_Types, ACS_Procs, ACS_Classes, Windows, AsioList, OpenAsio, Asio;

type

  TASIOPositionEvent = procedure(Sender : TComponent; SamplePosition, TimeStamp : Int64) of object;
  TASIOBufferSize = (absPreferred, absMinimum, absMaximum);

  (* Class: TASIOAudioOut
      Performs audio playback using low latency ASIO drivers.
      Descends from <TAuOutput>.
      On Windows ASIO drivers bypass some OS layaers which makes them suitable for a real-time audio processing.
      You will need an ASIO audo driver too use this component.
      Free ASIO driver that can be installed on top of any WDM (Windows) driver is available at http://www.asio4all.com.
      This component also requires openasio.dll which you will find along with other third-party NewAC libraries.
      One important feature of ASIO drivers is that they offer only a limited choise of sampling rates and sample formats.
      It is your software that should tune itself up to an ASIO driver and not vice versa.
 *)

  TASIOAudioOut = class(TAuOutput)
  private
    device : IOpenASIO;
    Devices : TAsioDriverList;
    Chan, SR, BPS : Integer;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
//    FOnUnderrun : TUnderrunEvent;
    FLatency, FBufferSize : Integer;
    FSupportedChannels : Integer;
    FOutputChannels : Integer;
    FOutputBPS : Integer;
    FFloat,  FPacked32 : Boolean;
    ASIOStarted : Boolean;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FOnSampleRateChanged : TGenericEvent;
    FOnLatencyChanged : TGenericEvent;
    FOnDriverReset : TGenericEvent;
    FNewSampleRate : Integer;
    DoReset : Boolean;
    DevStopped : Boolean;
    FOnPositionChanged : TASIOPositionEvent;
    FASIOBufferSize : TASIOBufferSize;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    function GetOutputBPS : Integer;
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
    function FillBuffer(var EOF : Boolean) : Integer;
    function GetSampleRate : Integer;
    procedure SetSampleRate(SR : Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure ProcessBuffer(sender : TComponent);
    procedure CallProcessBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: ReleaseASIODriver
        Call this method to release hold on ASIO driver so thst other applications could use audio subsystem.
        Driver is released automatically when the audio playback is done. *)
    procedure ReleaseASIODriver;
    procedure Pause;
    procedure Resume;
    (* Function: IsSampleRateSupported
        Returns True if the specified sample rate is supported by the driver and False otherwise.  *)
    function IsSampleRateSupported(SR : Integer) : Boolean;
    (* Function: ShowSetupDlg
        Brings up ASIO properties dialog window.  *)
    procedure ShowSetupDlg;
    (* Property: DeviceCount
         This read only property returns the number of logical ASIO devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: OutputBPS
         This read only property returns the number of bits per sample supported by the driver. Whatever this value is TASIOAudioOut also supports 16 bps input using internal conversion. *)
    property OutputBPS : Integer read GetOutputBPS;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver. You can use input with less channels than this value but not more.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property MaxOutputChannels : Integer read GetMaxOutputChannels;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property Latency : Integer read GetLatency;
    (* Property: SampleRate
         Use this property to change the ASIO driver sample rate while doing playback.  Note however that not all sample rates are supported.
         If the requested sample rate is not supported, the property's value doesn't change. *)
    property SampleRate : Integer read GetSampleRate write SetSampleRate;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: ASIOBufferSize
         Use this property to select the ASIO buffer size. Available options are absMinimum - minimum allowed buffer size, absPreferred - preferred buffer size, absMaximum - maximum aloowed buffer size.
         Larger buffer sizes increase latency. *)
    property ASIOBufferSize : TASIOBufferSize read FASIOBufferSize write FASIOBufferSize;
    property OnSampleRateChanged : TGenericEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnDriverReset : TGenericEvent read FOnDriverReset write FOnDriverReset;
    (* Property: OnPositionChanged
         If assigned this event handler is called every time  the system is about to fetch a new audio data block.
         The blocks are about 512 audio samlples in size so the operation is almost immediate.
         The event handler is passed the current sample number and a timestamp from the beginning of playback.
         Note that unlike most other NewAC events, this event is a real-time one. Performing some lengthy operation in this event handler may cause gaps in playback. *)
    property OnPositionChanged : TASIOPositionEvent read FOnPositionChanged write FOnPositionChanged;
  end;

  TASIOAudioIn = class(TAuInput)
  private
    device : IOpenASIO;
    Devices : TAsioDriverList;
    _BufSize : Integer;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : LongWord;
    FMaxChan : Integer;
    FSamplesToRead : Int64;
    FRecTime : Integer;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FLatency : Integer;
    BytesInBuf : Integer;
    FOnLatencyChanged : TGenericEvent;
    ASIOStarted : Boolean;
    FASIOBufferSize : TASIOBufferSize;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    procedure SetRecTime(aRecTime : Integer);
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
    procedure SetSampleRate(SR : LongWord);
  protected
    function GetTotalTime : LongWord; override;
    function GetTotalSamples : Int64; override;
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure _Pause; override;
    procedure _Resume; override;
    (* Function: IsSampleRateSupported
        Returns True if the specified sample rate is supported by the driver and False otherwise.  *)
    function IsSampleRateSupported(SR : Integer) : Boolean;
    (* Property: DeviceCount
         This read only property returns the number of logical DirectSound
         input devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName[Number : Integer]
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver. You can use input with less channels than this value but not more.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property MaxOutputChannels : Integer read GetMaxOutputChannels;
    (* Property: MaxOutputChannels
         This read only property returns the maximum number of output channels provided by the driver.
         ASIO properties setup may change this value (see <ShowSetupDlg>). *)
    property Latency : Integer read GetLatency;
    (* Property: InSampleRate
        Use this property to set the sample rate of the audio stream the
        component will provide. Possible values range from 4000 to 128000
        (depends on the capabilities of your hardware). *)
     property InSampleRate : LongWord read GetSR write SetSampleRate;
  published
    (* Property: SamplesToRead
         Use this property to set the number of samples (frames) the component
         should record. If you set this property value to -1 the component
         will be endlessly recording until you stop it. *)
    property SamplesToRead : Int64 read FSamplesToRead write FSamplesToRead;
    (* Property: DeviceNumber
         Use this property to select the recording device by number. The
         property default value is 0 which corresponds to the default audio
         input device in your system. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: InChannels
        Use this property to set the number of channels in the audio stream
        the component will provide. Possible values are 1 (mono), and 2
        (stereo). *)
    property InChannels : LongWord read GetCh write FChan stored True;
    (* Property: RecTime
         Use this property to set the recording duration (in seconds). If set
         this property overrides the value of <BytesToRead>. If you set this
         property value to -1 (the default) the component will be endlessly
         recording until you stop it. *)
    property RecTime : Integer read FRecTime write SetRecTime;
    (* Property: ASIOBufferSize
         Use this property to select the ASIO buffer size. Available options are absMinimum - minimum allowed buffer size, absPreferred - preferred buffer size, absMaximum - maximum aloowed buffer size.
         Larger buffer sizes increase latency. *)
    property ASIOBufferSize : TASIOBufferSize read FASIOBufferSize write FASIOBufferSize;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
  end;


  //IMPORTANT
  //     ASIOTrue :  begin EventHandler.PostGenericEvent(OutputComponent, OutputComponent.ProcessBuffer); sleep(2); end;
 //     ASIOFalse :  OutputComponent.ProcessBuffer(OutputComponent);



implementation


var
  OutputComponent : TASIOAudioOut;
  InputComponent : TASIOAudioIn;
  GStop : Boolean = False;
  CallOutputReady : Boolean = True;
  BufferIndex : Integer;
  iBuf : array[0..$FFFF] of Byte;
  oBuf : array[0..$FFFF] of Byte;
  WriteIndex, ReadIndex, ReadOffset : LongWord;

procedure TASIOAudioOut.ProcessBuffer(sender : TComponent);
var
  s1, s2 : TASIOInt64;
  OldStopped : Bool;
begin
   if Device = nil then Exit;
   OldStopped := Thread.Stopped;
   Thread.Stopped := False;
   if Assigned(FOnPositionChanged) then
   begin
     Device.GetSamplePosition(s1, s2);
     FOnPositionChanged(Self, (s1.hi shl 32) + s1.lo, (s2.hi shl 32) + s2.lo)
   end;
   FillBuffer(GStop);
   if FOutputChannels = 2 then
     DeinterleaveStereo32(@iBuf, BufferInfo[0].buffers[BufferIndex], BufferInfo[1].buffers[BufferIndex], OutputComponent.FBufferSize)
   else
   begin
     FastCopyMem(BufferInfo[0].buffers[BufferIndex], @iBuf, OutputComponent.FBufferSize*4);
     FastCopyMem(BufferInfo[1].buffers[BufferIndex], @iBuf, OutputComponent.FBufferSize*4);
   end;
   if CallOutputReady then
      CallOutputReady := TASIOAudioOut(sender).Device.OutputReady <> ASE_NotPresent;
   Thread.Stopped := OldStopped;
end;

procedure AsioBufferSwitchOutput(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  BufferIndex := doubleBufferIndex;
   case directProcess of
     ASIOFalse :  begin EventHandler.PostNonGuiEvent(OutputComponent, OutputComponent.ProcessBuffer); sleep(2); end;
     ASIOTrue :  OutputComponent.ProcessBuffer(OutputComponent);
   end;
end;

procedure AsioSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
  OutputComponent.FNewSampleRate := Round(sRate);
  if Assigned(OutputComponent.FOnSampleRateChanged) then
    EventHandler.PostGenericEvent(OutputComponent, OutputComponent.FOnSampleRateChanged);
end;

function AsioMessage(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        OutputComponent.DoReset := True;
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        OutputComponent.DoReset := True;
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        if Assigned(OutputComponent.FOnLatencyChanged) then
        EventHandler.PostGenericEvent(OutputComponent, OutputComponent.FOnLatencyChanged);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 1;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  params.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;
  Result := nil;
end;

procedure AsioBufferSwitchInput(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  if GStop  then
    Exit;
  if InputComponent.FChan = 1 then
    FastCopyMem(@oBuf[(WriteIndex mod 4)*$4000], InputComponent.BufferInfo[0].buffers[doubleBufferIndex], InputComponent._BufSize*4)
  else
  begin
    InterleaveStereo32(InputComponent.BufferInfo[0].buffers[doubleBufferIndex], InputComponent.BufferInfo[1].buffers[doubleBufferIndex],
       @oBuf[(WriteIndex mod 4)*$4000], InputComponent._BufSize);
  end;
  Inc(InputComponent.FPosition, InputComponent._BufSize*4);
  if (InputComponent.FSamplesToRead >= 0) then
    if InputComponent.FPosition >= InputComponent.FSize then
        GStop := True;
  Inc(WriteIndex);
end;

procedure AsioSampleRateDidChange2(sRate: TASIOSampleRate); cdecl;
begin
end;

function AsioMessage2(selector, value: longint; message: pointer; opt: pdouble): longint; cdecl;
begin
  Result := 0;

  case selector of
    kAsioSelectorSupported    :   // return 1 if a selector is supported
      begin
        case value of
          kAsioEngineVersion        :  Result := 1;
          kAsioResetRequest         :  Result := 1;
          kAsioBufferSizeChange     :  Result := 0;
          kAsioResyncRequest        :  Result := 1;
          kAsioLatenciesChanged     :  Result := 1;
          kAsioSupportsTimeInfo     :  Result := 1;
          kAsioSupportsTimeCode     :  Result := 1;
          kAsioSupportsInputMonitor :  Result := 0;
        end;
      end;
    kAsioEngineVersion        :  Result := 2;   // ASIO 2 is supported
    kAsioResetRequest         :
      begin
        InputComponent.ASIODone;
        InputComponent.ASIOInit;
        Result := 1;
      end;
    kAsioBufferSizeChange     :
      begin
        InputComponent.ASIODone;
        InputComponent.ASIOInit;
        Result := 1;
      end;
    kAsioResyncRequest        :  ;
    kAsioLatenciesChanged     :
      begin
        if Assigned(InputComponent.FOnLatencyChanged) then
        EventHandler.PostGenericEvent(InputComponent, InputComponent.FOnLatencyChanged);
        Result := 1;
      end;
    kAsioSupportsTimeInfo     :  Result := 1;
    kAsioSupportsTimeCode     :  Result := 0;
    kAsioSupportsInputMonitor :  ;
  end;
end;

function AsioBufferSwitchTimeInfo2(var params: TASIOTime; doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  params.timeInfo.flags := kSystemTimeValid or kSamplePositionValid;
  Result := nil;
end;



constructor TASIOAudioOut.Create;
begin
  inherited Create(AOwner);
  OutputComponent := Self;
  Self.Devices := nil;
  ListAsioDrivers(Self.Devices);
  FDeviceCount := Length(Devices);
  FOutputChannels := 2;
  Callbacks.bufferSwitch := AuAsio.AsioBufferSwitchOutput;
  Callbacks.sampleRateDidChange := AuAsio.AsioSampleRateDidChange;
  Callbacks.asioMessage := AuAsio.AsioMessage;
  Callbacks.bufferSwitchTimeInfo := AuAsio.AsioBufferSwitchTimeInfo;
end;

destructor TASIOAudioOut.Destroy;
begin
  AsioDone;
  SetLength(Devices, 0);
  inherited Destroy;
end;

procedure TASIOAudioOut.SetDeviceNumber(i : Integer);
begin
  if FDeviceCount = 0 then Exit;
  if (i < 0) or (i >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  FDeviceNumber := i;
end;

function TASIOAudioOut.GetDeviceName(Number : Integer) : String;
begin
  if (Number < 0) or (Number >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [Number]));
  Result := String(@Devices[Number].name[0]);
end;

procedure TASIOAudioOut.Prepare;
begin
  FInput.Init;
  FOutputChannels := Finput.Channels;
  ASIOInit;
  Chan := FOutputChannels;
  SR := FInput.SampleRate;
  if Device.CanSampleRate(SR) <> ASE_OK then
    raise EAuException.Create(Format('ASIO driver doesn''t support sample rate of %d. Use resampler.', [SR]))
  else  Device.SetSampleRate(Round(SR));
  BPS := FInput.BitsPerSample;
  if BPS <> FOutputBPS then
    if BPS <> 16 then
       raise EAuException.Create(Format('ASIO driver cannot handle %d BPS directly. Use BPS converter.', [BPS]));
  GStop := False;
  DoReset := False;
  AsioBufferSwitchOutput(1, AsioTrue);
  FastCopyMem(BufferInfo[0].buffers[1], BufferInfo[0].buffers[0], FBufferSize);
  Device.Start;
  DevStopped := False;
end;

function TASIOAudioOut.DoOutput(Abort: Boolean) : Boolean;
begin
  if Abort or GStop then
  begin
    if not DevStopped then
    begin
     if Device <> nil then
      Device.Stop;
      DevStopped := True;
    end;
    Result := False;
    Exit;
  end;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if DoReset then
  begin
    DoReset := False;
    AsioDone;
    AsioInit;
    if Assigned(FOnDriverReset) then
        EventHandler.PostGenericEvent(Self, FOnDriverReset);
  end;
  sleep(100);
  Result := True;
end;

procedure TASIOAudioOut.Done;
begin
  GStop := True;
  if not DevStopped then
  begin
    if Device <> nil then
    Device.Stop;
    DevStopped := True;
  end;
  AsioDone;
  DoReset := False;
  FInput.Flush;
end;

procedure TASIOAudioOut.ASIOInit;
var
  i, Dummie, min, max, pref : Integer;
  chi : TAsioChannelInfo;
begin
  FFloat := False;
  FPacked32 := False;
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Device.Init(TForm(Self.Owner).Handle)) then
        begin
          Device := nil;  // RELEASE
          raise EAuException.Create('Failed to initialize ASIO device');
        end;
      end else
        raise EAuException.Create('Failed to open ASIO device');
  end else
    raise EAuException.Create('Failed to open ASIO device');
  Device.GetChannels(Dummie, FSupportedChannels);
  Device.GetBufferSize(min, max, pref, Dummie);
  case FASIOBufferSize of
    absPreferred: FBufferSize := pref;
    absMinimum: FBufferSize := min;
    absMaximum: FBufferSize := max;
  end;
  if (FoutputChannels < 1) or (FOutputChannels > FSupportedChannels) then
     raise EAuException.Create(Format('ASIO: %d channels are not available.', [FOutputChannels]));
  for i := 0  to FOutputChannels - 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  // TODO: Add multichannel support
  if Device.CreateBuffers(@BufferInfo, 2, FBufferSize, Callbacks) <> ASE_OK then
     raise EAuException.Create('ASIO: failed to create output buffers.');
  chi.channel := 0;
  chi.isInput := ASIOFalse;
  Device.GetChannelInfo(chi);
   case chi.vType of
      ASIOSTInt16LSB   :  FoutputBPS := 16;
      ASIOSTInt24LSB   :  FoutputBPS := 24;
      ASIOSTInt32LSB   :  FoutputBPS := 32;
      ASIOSTFloat32LSB :
                        begin
                          FoutputBPS := 32;
                          FFloat := True;
                        end;
    ASIOSTInt32LSB16 :
                        begin
                          FoutputBPS := 16;
                          FPacked32 := True;
                        end;
    ASIOSTInt32LSB24 :
                        begin
                          FoutputBPS := 24;
                          FPacked32 := True;
                        end;
    else raise EAuException.Create('ASIO: Unsupported sample format.');
  end;
  Device.GetLatencies(Dummie, FLatency);
  ASIOStarted := True;
end;

procedure TASIOAudioOut.ASIODone;
begin
  if not ASIOStarted then Exit;
  ASIOStarted := False;
  if Device <> nil then
  Device.DisposeBuffers;
  Device := nil;
end;

procedure TASIOAudioOut.ReleaseASIODriver;
begin
  ASIODone;
end;

function TASIOAudioOut.GetOutputBPS;
begin
  ASIOInit;
  Result := FoutputBPS;
end;

function TASIOAudioOut.GetMaxOutputChannels;
begin
  ASIOInit;
  Result := FSupportedChannels;
end;

function TASIOAudioOut.GetLatency;
begin
  ASIOInit;
  Result := FLatency;
end;


function TASIOAudioOut.IsSampleRateSupported(SR: Integer) : Boolean;
var
  D : Double;
begin
  ASIOInit;
  D := SR;
  Result := Device.CanSampleRate(D) <> 0;
end;

function  TASIOAudioOut.FillBuffer(var EOF: Boolean) : Integer;
var
  i,  count : Integer;
  Buf16 : PBuffer16;
  Buf32 : PBuffer32;
begin
  if (BPS = 16) and (OutputBPS = 32) then
  begin
    FillChar(iBuf, (FBufferSize shl 2)*FOutputChannels, 0);
    Result := FInput.FillBuffer(@iBuf[0], (FBufferSize shl 1)*FOutputChannels, EOF);
    count := FBufferSize*FOutputChannels;
    Buf16 := @iBuf;
    Buf32 := @iBuf;
    for i := Count - 1 downto 0 do
      Buf32[i] := Buf16[i] shl 16;
    Exit;
  end;

  Result := FInput.FillBuffer(@iBuf[0], (FBufferSize shl 2)*FOutputChannels, EOF);
end;

procedure TASIOAudioOut.CallProcessBuffer;
var
 m : TGenericEvent;
begin
  m := ProcessBuffer;
  EventHandler.PostGenericEvent(TComponent(Self), m);
end;

procedure TASIOAudioOut.Pause;
begin
  inherited Pause;
  Device.Stop;
end;

procedure TASIOAudioOut.Resume;
begin
  Device.Start;
  inherited Resume;
end;

procedure TASIOAudioOut.ShowSetupDlg;
begin
  ASIODone;
  ASIOInit;
  Device.ControlPanel;
  ASIODone;
  ASIOInit;
end;

procedure TASIOAudioOut.SetSampleRate;
begin
  if Device <> nil then
    Device.SetSampleRate(SR);
end;

function TASIOAudioOut.GetSampleRate;
var
  D : Double;
begin
  if Device <> nil then
    Device.GetSampleRate(D)
  else D := 44100;
  Result := Round(D);
end;

constructor TASIOAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InputComponent := Self;
  Self.Devices := nil;
  ListAsioDrivers(Self.Devices);
  FDeviceCount := Length(Devices);
  Self.FChan := 2;
  Self.FFreq := 44100;
  FSize := -1;
  FRecTime := -1;
  FSamplesToRead := -1;
  Callbacks.bufferSwitch := AuAsio.AsioBufferSwitchInput;
  Callbacks.sampleRateDidChange := AuAsio.AsioSampleRateDidChange2;
  Callbacks.asioMessage := AuAsio.AsioMessage2;
  Callbacks.bufferSwitchTimeInfo := AuAsio.AsioBufferSwitchTimeInfo2;
  ASIOStarted := False;
end;

destructor TASIOAudioIn.Destroy;
begin
  ASIODone;
  inherited Destroy;
end;

procedure TASIOAudioIn.ASIOInit;
var
  i, Dummie, min, max, pref : Integer;
  chi : TAsioChannelInfo;
begin
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Device.Init(TForm(Self.Owner).Handle)) then
        begin
          Device := nil;  // RELEASE
          raise EAuException.Create('Failed to initialize ASIO device');
        end;
      end else
        raise EAuException.Create('Failed to open ASIO device');
  end else
    raise EAuException.Create('Failed to open ASIO device');
  Device.GetChannels(Dummie, FMaxChan);
  Device.GetBufferSize(min, max, pref, Dummie);
  case FASIOBufferSize of
    absPreferred: _BufSize := pref;
    absMinimum: _BufSize := min;
    absMaximum: _BufSize := max;
  end;
  if (FChan < 1) or (FChan > FMaxChan) then
     raise EAuException.Create(Format('ASIO: %d channels are not available.', [FMaxChan]));
  for i := 0  to FChan - 1 do
  begin
    BufferInfo[i].isInput := ASIOTrue;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  // TODO: Add multichannel support
  if Device.CreateBuffers(@BufferInfo, 2, _BufSize, Callbacks) <> ASE_OK then
     raise EAuException.Create('ASIO: failed to create output buffers.');
  chi.channel := 0;
  chi.isInput := ASIOFalse;
  Device.GetChannelInfo(chi);
   case chi.vType of
      ASIOSTInt16LSB   :  FBPS := 16;
      ASIOSTInt24LSB   :  FBPS := 24;
      ASIOSTInt32LSB   :  FBPS := 32;
    else raise EAuException.Create('ASIO: Unsupported sample format.');
  end;
  Device.GetLatencies(Dummie, FLatency);
  FSampleSize := FBPS*FChan div 8;
  ASIOStarted := True;
end;

procedure TASIOAudioIn.ASIODone;
begin
  if not ASIOStarted then Exit;
  ASIOStarted := False;
  if Device <> nil then
  Device.DisposeBuffers;
  Device := nil;
end;

function TASIOAudioIn.GetMaxOutputChannels;
begin
  ASIOInit;
  Result := FMaxChan;
end;

function TASIOAudioIn.GetLatency;
begin
  ASIOInit;
  Result := FLatency;
end;


function TASIOAudioIn.IsSampleRateSupported(SR: Integer) : Boolean;
var
  D : Double;
begin
  ASIOInit;
  D := SR;
  Result := Device.CanSampleRate(D) <> 0;
end;

procedure TASIOAudioIn.SetDeviceNumber(i : Integer);
begin
  if FDeviceCount = 0 then Exit;
  if (i < 0) or (i >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  FDeviceNumber := i;
end;

function TASIOAudioIn.GetDeviceName(Number : Integer) : String;
begin
  if (Number < 0) or (Number >= FDeviceCount) then
    raise EAuException.Create(Format('Device number out of range: %d', [Number]));
  Result := String(@Devices[Number].name[0]);
end;

procedure TASIOAudioIn.SetSampleRate;
begin
  ASIOInit;
  Device.SetSampleRate(SR);
  FFreq := GetSR;
end;

function TASIOAudioIn.GetTotalTime : LongWord;
var
  BytesPerSec : Integer;
begin
  ASIOInit;
  BytesPerSec := Self.InSampleRate*FSampleSize;
  if FSamplesToRead < 0 then Result := 0
  else
  Result := Round(FSamplesToRead/BytesPerSec);
end;

function TASIOAudioIn.GetTotalSamples : Int64;
begin
  Result := FSamplesToRead;
end;

function TASIOAudioIn.GetBPS : LongWord;
begin
  ASIOInit;
  Result := FBPS;
end;

function TASIOAudioIn.GetCh : LongWord;
begin
  Result := FChan;
end;

function TASIOAudioIn.GetSR : LongWord;
var
  D : Double;
begin
  ASIOInit;
  Device.GetSampleRate(D);
  Result := Round(D);
end;

procedure TASIOAudioIn.SetRecTime;
begin
  FRecTime := aRecTime;
{$WARNINGS OFF}
  if FRecTime >= 0 then FSamplesToRead := FRecTime*FFreq
  else FSamplesToRead := -1;
 {$WARNINGS ON}
end;

procedure TASIOAudioIn.InitInternal;
begin
  if Busy then raise EAuException.Create('The component is busy');
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
{$WARNINGS OFF}
  if FRecTime > 0 then FSamplesToRead := FRecTime*FFreq;
{$WARNINGS ON}
  FPosition := 0;
  WriteIndex := 0;
  ReadIndex := 0;
  ReadOffset := 0;
  GStop := False;
  Busy := True;
  ASIOInit;
  FSampleSize := FChan*FBPS div 8;
  if FSamplesToRead > 0 then
    FSize := FSamplesToRead*FSampleSize
  else
    FSize := -1;
  BytesInBuf := _BufSize*FChan*FBPS div 8;
  Device.Start;
end;

procedure TASIOAudioIn.FlushInternal;
begin
  GStop := True;
  Device.Stop;
  ASIODone;
  Busy := False;
end;

procedure TASIOAudioIn.GetDataInternal;
begin
  if not Busy then  raise EAuException.Create('The Stream is not opened');
  if GStop then
  begin
    Buffer := nil;
    Bytes := 0;
    Exit;
  end;
  if ReadIndex >= WriteIndex then
    Sleep(50);
  if ReadIndex >= WriteIndex then
     raise EAuException.Create('Reading data from ASIO failed.');
  Buffer := @oBuf[(ReadIndex mod 4)*$400 + ReadOffset];
  if Bytes >= BytesInBuf - ReadOffset then
  begin
    Bytes := BytesInBuf - ReadOffset;
    ReadOffset := 0;
    Inc(ReadIndex);
  end else
    Inc (ReadOffset, Bytes);
end;

procedure TASIOAudioIn._Pause;
begin
  if Device <> nil then
    Device.Stop;
end;

procedure TASIOAudioIn._Resume;
begin
  if Device <> nil then
    Device.Start;
end;

end.
