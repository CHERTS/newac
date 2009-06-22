unit AuASIO;

interface

uses
  SysUtils, Classes, Forms, ACS_Types, ACS_Classes, Windows, AsioList, OpenAsio, Asio;

type
  TASIOAudioOut = class(TAuOutput)
  private
    FPollingInterval : LongWord;
    device : IOpenASIO;
    Devices : TAsioDriverList;
    Chan, SR, BPS : Integer;
    EndOfInput, StartInput : Boolean;
    Buf : PBuffer8;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FillByte : Byte;
    FUnderruns, _TmpUnderruns : LongWord;
    FOnUnderrun : TUnderrunEvent;
    FLatency, FBufferSize : Integer;
    FSupportedChannels, FSupportedBPS, FOutputSampleRate : Integer;
    FOutputChannels : Integer;
    FOutputBPS : Integer;
    FFloat,  FPacked32 : Boolean;
    ASIOStarted : Boolean;
    BufferInfo : array [0..16] of TAsioBufferInfo;
    Callbacks         : TASIOCallbacks;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    procedure ASIOInit;
    procedure ASIODone;
    function GetOutputBPS : Integer;
    function GetMaxOutputChannels : Integer;
    function GetLatency : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReleaseASIODriver;
    procedure Pause;
    procedure Resume;
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
    public function IsSampleRateSupported(SR : Integer) : Boolean;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
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
    property OnUnderrun : TUnderrunEvent read FOnUnderrun write FOnUnderrun;
  end;


implementation

constructor TASIOAudioOut.Create;
begin
  inherited Create(AOwner);
  FFramesInBuffer := $10000;
  dåvices := nil;
  ListAsioDrivers(Dåvices);
  FDeviceCount := Length(Devices);
  FOutputChannels := 2;
end;

destructor TASIOAudioOut.Destroy;
begin
  CloseDriver;
  SetLength(driverlist, 0);
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
    raise EAuException.Create(Format('Device number out of range: %d', [i]));
  Result := String(@Devices[Number].name[0]);
end;

procedure TASIOAudioOut.Prepare;
begin
  FInput.Init;
  FOutputChannels := Finput.Channels;
  ASIOInit;
  Ch := FOutputChannels;
  SR := FInput.SampleRate;
  if not Device.CanSampleRate(Double(SR)) then
    raise EAuException.Create('ASIO driver doesn''t support sample rate of %d. Use resampler.', [SR])
  else  Device.SetSampleRate(Double(SR));
  BPS := FInput.BitsPerSample;
  if BPS <> FOutputBPS then
    if BPS <> 16 then
       raise EAuException.Create('ASIO driver cannot handle %d BPS directly. Use BPS converter.', [BPS])

end;

procedure TASIOAudioOut.ASIOInit;
var
  i, Dummie : Integer;
  chi : TAsioChannelInfo;
begin
  FFloat := False;
  Packed32 := False;
  if ASIOStarted then Exit;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid ASIO device number');
  if OpenAsioCreate(Devices[FDeviceNumber].id, Device) then
  begin
      if (Device <> nil) then
      begin
        if not Succeeded(Driver.Init(Handle)) then
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
  OpenDevice;
  for i := 0  to FOutputChannels - 1 do
  begin
    BufferInfo[i].isInput := ASIOFalse;
    BufferInfo[i].channelNum := i;
    BufferInfo[i].buffers[0] := nil;
    BufferInfo[i].buffers[1] := nil;
  end;
  if Device.CreateBuffers(@BufferInfo, FOutputChannels, FBufferSize, Callbacks) <> ASE_OK then
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
begin
  ASIOInit;
  Result := Device.CanSampleRate(Double(SR)) <> 0;
end;

end.
