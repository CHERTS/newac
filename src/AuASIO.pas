unit AuASIO;

interface

uses
  SysUtils, Classes, Forms, SyncObjs, ACS_Types, ACS_Procs, ACS_Classes, Windows, AsioList, OpenAsio, Asio;

type
  TASIOAudioOut = class(TAuOutput)
  private
    OutputEvent : TEvent;
    device : IOpenASIO;
    Devices : TAsioDriverList;
    Chan, SR, BPS : Integer;
    Buf : PBuffer8;
    FInternalBufSize : Integer;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FUnderruns, _TmpUnderruns : LongWord;
//    FOnUnderrun : TUnderrunEvent;
    FLatency, FBufferSize : Integer;
    FSupportedChannels, FSupportedBPS, FOutputSampleRate : Integer;
    FOutputChannels : Integer;
    FOutputBPS : Integer;
    FFloat,  FPacked32 : Boolean;
    ASIOStarted : Boolean;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    FDirectOperation  : Boolean;
    FOnSampleRateChanged : TGenericEvent;
    FOnLatencyChanged : TGenericEvent;
    FOnDriverReset : TGenericEvent;
    FNewSampleRate : Integer;
    DoReset : Boolean;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    function GetOutputBPS : Integer;
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
    function FillBuffer(Buffer : Pointer; var EOF : Boolean) : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure ProcessBuffer(sender : TASIOAudioOut);
    procedure CallProcessBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReleaseASIODriver;
    procedure Pause;
    procedure Resume;
    public function IsSampleRateSupported(SR : Integer) : Boolean;
    (* Property: DeviceCount
         This read only property returns the number of logical ASIO devices. *)
    property DeviceCount : Integer read FDeviceCount;
    (* Property: DeviceName
         This read only array property returns the name of the device
         specified by its number. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property DeviceName[Number : Integer] : String read GetDeviceName;
    (* Property: Underruns
         This read only property returns the number of internal buffer
         underruns that have occurred during playback. *)
    property Underruns : LongWord read FUnderruns;
    property OutputBPS : Integer read GetOutputBPS;
    property MaxOutputChannels : Integer read GetMaxOutputChannels;
    property Latency : Integer read GetLatency;
    property NewSampleRate : Integer read FNewSampleRate;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    property DirectOperation : Boolean read FDirectOperation write FDirectOperation;
    (* Property: OnUnderrun
         OnUnderrun event is raised when the component has run out of data.
         This can happen if the component receives data at slow rate from a
         slow CD-ROM unit or a network link. You will also get OnUnderrun
         event when unpausing paused playback (this is a normal situation).
         Usually TDXAudioOut successfully recovers from underruns by itself,
         but this causes pauses in playback so if you start to receive
         OnUnderrun events, you may try to increase the speed rate of data
         passing to the component, if you can. Yo can check the <Underruns>
         property for the total number of underruns. *)
//    property OnUnderrun : TUnderrunEvent read FOnUnderrun write FOnUnderrun;
    property OnSampleRateChanged : TGenericEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnLatencyChanged : TGenericEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnDriverReset : TGenericEvent read FOnDriverReset write FOnDriverReset;
  end;


implementation

var
  OutputComponent : TASIOAudioOut;
  GStop : Boolean = False;
  GBuffer : Pointer;
  BufferIndex : Integer;

procedure TASIOAudioOut.ProcessBuffer(sender : TASIOAudioOut);
var
  s1, s2 : TASIOInt64;
  b1, b2 : PBuffer32;
  i : Integer;
begin
   Device.GetSamplePosition(s1, s2);
   FillBuffer(BufferInfo[0].buffers[BufferIndex], GStop);
   FastCopyMem(BufferInfo[1].buffers[BufferIndex], BufferInfo[0].buffers[BufferIndex], OutputComponent.FBufferSize*4);
   b1 := BufferInfo[0].buffers[BufferIndex];
   b2 := BufferInfo[1].buffers[BufferIndex];
   sender.Device.OutputReady;
   for i:= 0 to OutputComponent.FBufferSize - 1 do
   begin
     if Odd(i) then b2[i] := 0
     else b1[i] := 0;
   end;

end;

procedure AsioBufferSwitchOutput(doubleBufferIndex: longint; directProcess: TASIOBool); cdecl;
begin
  BufferIndex := doubleBufferIndex;
  if OutputComponent.FDirectOperation then
  begin
   case directProcess of
     ASIOFalse :  OutputComponent.CallProcessBuffer;
     ASIOTrue  :  OutputComponent.ProcessBuffer(OutputComponent);
   end;
   end else
      OutputComponent.OutputEvent.SetEvent;
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
  FDirectOperation := True;
  OutputEvent := TEvent.Create(nil, False, False, 'ASIOevt');
end;

destructor TASIOAudioOut.Destroy;
begin
  AsioDone;
  SetLength(Devices, 0);
  OutputEvent.Free;
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
  if Device.CanSampleRate(Round(SR)) = ASIOFalse then
    raise EAuException.Create(Format('ASIO driver doesn''t support sample rate of %d. Use resampler.', [SR]))
  else  Device.SetSampleRate(Round(SR));
  BPS := FInput.BitsPerSample;
  if BPS <> FOutputBPS then
    if BPS <> 16 then
       raise EAuException.Create(Format('ASIO driver cannot handle %d BPS directly. Use BPS converter.', [BPS]));
  GStop := False;
  DoReset := False;
  if not FDirectOperation then
  begin
    GetMem(Buf, FBufferSize);
    FInternalBufSize := FBufferSize;
  end;
  FillBuffer(BufferInfo[0].buffers[0], GStop);
  FastCopyMem(BufferInfo[0].buffers[1], BufferInfo[0].buffers[0], FBufferSize);
  Device.Start;
end;

function TASIOAudioOut.DoOutput(Abort: Boolean) : Boolean;
begin
  if Abort or GStop then
  begin
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
  if not FDirectOperation then
  begin
    if FBufferSize > FInternalBufSize then
    begin
      FreeMem(Buf);
      GetMem(Buf, FBufferSize);
      FInternalBufSize := FBufferSize;
    end;
    FillBuffer(Buf, GStop);
    OutputEvent.WaitFor(INFINITE);
    FastCopyMem(BufferInfo[0].buffers[BufferIndex], Buf, FBufferSize);
  end else
  begin
    sleep(100);
  end;
end;

procedure TASIOAudioOut.Done;
begin
  GStop := True;
  Device.Stop;
  AsioDone;
  if not FDirectOperation then
     FreeMem(Buf);
  DoReset := False;
  FInput.Flush;
end;

procedure TASIOAudioOut.ASIOInit;
var
  i, Dummie : Integer;
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
  Device.GetBufferSize(Dummie, Dummie, FBufferSize, Dummie);
  if (FoutputChannels < 1) or (FOutputChannels > FSupportedChannels) then
     raise EAuException.Create(Format('ASIO: %d channels are not available.', [FOutputChannels]));
  for i := 0  to FOutputChannels - 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
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

function  TASIOAudioOut.FillBuffer(Buffer: Pointer; var EOF: Boolean) : Integer;
var
  i,  Len, count : Integer;
  Buf16 : PBuffer16;
  Buf32 : PBuffer32;
begin
  if (BPS = 16) and (OutputBPS = 32) then
  begin
    FillChar(Buffer^, FBufferSize, 0);
    Len := FInput.FillBuffer(Buffer, FBufferSize shr 1, EOF);
    count := FBufferSize shr 2;
    Buf16 := Buffer;
    Buf32 := Buffer;
    for i := Count - 1 downto 0 do
      Buf32[i] := Buf16[i] shl 16;
    Len := Len shl 1;
    Exit;
  end;

  Result := FInput.FillBuffer(Buffer, FBufferSize shl 2, EOF);
end;

procedure TASIOAudioOut.CallProcessBuffer;
var
 m : TMethod;
begin
  m.Code := Self.MethodAddress('ProcessBuffer');
  m.Data := Self;
  EventHandler.PostGenericEvent(TComponent(Self), TGenericEvent(m));
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

end.
