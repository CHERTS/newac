(*
  This file is a part of New Audio Components package v. 2.6
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewACDSAudio;

interface
uses
  SysUtils, Classes, Forms, FastMove, ACS_Types, ACS_Classes, Windows, MMSystem, DSAudio, _DirectSound;

type

  TDSAudioOut = class(TAuOutput)
  private
    Freed : Boolean;
    FFrameSize : Word;
    FLatency : LongWord;
    FFramesInBuffer : LongWord;
    DS : DSOut;
    Devices : DSW_Devices;
    Chan, SR, BPS : LongWord;
    EndOfInput, StartInput : Boolean;
    Buf : PBuffer8;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    _BufSize : Integer;
    FillByte : Byte;
    FUnderruns : LongWord;
    FOnUnderrun : TGenericEvent;
    FVolume : longint; //DW - for more reliable volume control
    FPrefetchData : Boolean;
    FSpeedFactor : Single;
    procedure SetDeviceNumber(i : Integer);
    function GetDeviceName(Number : Integer) : String;
    function GetVolume : Integer;
    procedure SetVolume(value : Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;
    (* Procedure: Jump
        This method, being a wrapper around <Seek>, simpifies navigation in
        the input stream. Calling Jump moves you backward or forward relative
        to the current position. Jump may be called either before starting
        playback (in this case the playback will be started from the position
        specified) or during the playback.

      Parameters:
        Offs - the amount of file contents, in in units of 1/1000 of the content length, that will be skipped.
        Positive value skips forward, negative value skips backward.
        For example calling Jump(-1000) always sets the playing position at the
        beginning of the file and Jump(100) will skip forward to 1/10 of the file.
        Note:
        Use <Seek> for more exact positioning.
    *)
    procedure Jump(Offs : Integer);
    (* Property: DeviceCount
         This read only property returns the number of logical output DirectSound devices. *)
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
    (* Property: Volume
         Use this property to set or get the volume of the sound being played.
         The default value is 0 which corresponds to the original volume of
         the sound. Valid values range from -10000 (silence) to 0. The Volume
         property allows you to make the played sound softer than the original
         one, but not louder. *)
    property Volume : Integer read GetVolume write SetVolume;
  published
    (* Property: DeviceNumber
         Use this property to select the playback device by number. The
         default value is 0 which corresponds to the default audio output
         device in your system. Valid numbers range from 0 to <DeviceCount> -
         1. *)
    property DeviceNumber : Integer read FDeviceNumber write SetDeviceNumber;
    (* Property: Latency
         This property sets the audio latency (the delay between the moment the audio data is passed to the component and the moment it is played.
         The latency is set in milliseconds.
         This is a convenience property that overrides the <FramesInBuffer> and the <PollingInterval>. If the Latency is greater than zero these properties are ignored.
         The reasonable values for this property lie in the range between 50 (0.05 second) and 250 (0.25 second). *)
    property Latency : LongWord read FLatency write FLatency;
    (* Property: PrefetchData
       This property tells the component whenever the audio data should be prefetched while playing. Prefetching data makes it run more smoothly and allows lower buffre sizes (see <FramesInBuffer>). *)
    property PrefetchData : Boolean read FPrefetchData write FPrefetchData;
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
    property OnUnderrun : TGenericEvent read FOnUnderrun write FOnUnderrun;
    property SpeedFactor : Single read FSpeedFactor write FSpeedFactor;
  end;


implementation

function _Min(x1, x2 : Integer) : Integer;
begin
  if x1 < x2 then
    Result := x1
  else
    Result := x2;
end;


procedure TDSAudioOut.SetDeviceNumber(i : Integer);
begin
  FDeviceNumber := i
end;

function TDSAudioOut.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

procedure TDSAudioOut.SetVolume;
begin
  FVolume := Value; //DW
  DSSetVolume(DS, value);
end;

function TDSAudioOut.GetVolume;
begin
  DSGetVolume(DS, Result);
  FVolume := Result; //DW
end;

procedure TDSAudioOut.Done;
begin
  if not Freed then
  begin
    DSTerminateOutput(DS);
    FreeMem(Buf);
    Freed := True;
  end;
  Finput.Flush;
  Freed := True;
end;

function TDSAudioOut.DoOutput;
var
  Len : LongWord;
  lb : Integer;
//  Res : HRESULT;
  PlayTime, CTime : LongWord;
  TmpBuf : Pointer;
begin
  Result := True;
  if not Busy then Exit;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if StartInput then
  begin
    Len := FInput.FillBuffer(Buf, _BufSize, EndOfInput);
    DSWriteBlock(DS, @Buf[0], Len);
    Volume := FVolume; //DW
    DSStartOutput(DS);
    StartInput := False;
  end;
  if Abort then
  begin
    DSStopOutput(DS);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  if EndOfInput then
  begin
    CanOutput := False;
    PlayTime := Round(_BufSize/(Chan*(BPS div 8)*SR))*1000;
    CTime := 0;
    while CTime < PlayTime do
    begin
      Sleep(100);
      DSFillEmptySpace(DS, FillByte);
      Inc(CTime, 100);
    end;
    DSStopOutput(DS);
    Result := False;
    Exit;
  end;

  Len := _BufSize div 2;
  if FPrefetchData then
    Finput._Prefetch(Len);
  if WaitForCursor(DS, 0) then
  begin
    Inc(FUnderruns);
    ResetEvent(DS.events[1]);
    if Assigned(FOnUnderrun) then
      EventHandler.PostGenericEvent(Self, FOnUnderrun);
  end;
  if Abort then
  begin
    DSStopOutput(DS);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  DSQueryOutputSpace(DS, lb);
  if lb <> 0 then
  begin
  if FPrefetchData then
  begin
   // Len := _Min(lb, _BufSize);
    FInput.GetData(TmpBuf, Len);
  end else
  begin
    Len := FInput.FillBuffer(Buf, Len, EndOfInput);
    TmpBuf := Buf;
  end;
  EndOfInput := Len = 0;
  DSWriteBlock(DS, TmpBuf, Len);
  if EndOfInput then
  begin
    DSFillEmptySpace(DS, FillByte);
    Exit;
  end;

  Len := _BufSize div 2;
  if FPrefetchData then
    Finput._Prefetch(Len);
  end;
  if WaitForCursor(DS, 1) then
  begin
    Inc(FUnderruns);
    ResetEvent(DS.events[0]);
    if Assigned(FOnUnderrun) then
      EventHandler.PostGenericEvent(Self, FOnUnderrun);
  end;
  if Abort then
  begin
    DSStopOutput(DS);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  DSQueryOutputSpace(DS, lb);
  if lb = 0 then Exit;
  if FPrefetchData then
  begin
//    Len := _Min(lb, _BufSize);
    FInput.GetData(TmpBuf, Len);
  end else
  begin
    Len := FInput.FillBuffer(Buf, Len, EndOfInput);
    TmpBuf := Buf;
  end;
  EndOfInput := Len = 0;
  DSWriteBlock(DS, TmpBuf, Len);
  if EndOfInput then
  begin
    DSFillEmptySpace(DS, FillByte);
    Exit;
  end;
end;

constructor TDSAudioOut.Create;
begin
  inherited Create(AOwner);
  FSpeedFactor := 1;
  FLatency := 60;
  FVolume := 0; //DW
  if not (csDesigning in ComponentState) then
  begin
    DSEnumerateOutputDevices(@Devices);
    FDeviceCount := Devices.devcount;
    Thread.Priority := tpHighest;
  end;
  FPrefetchData := True;
end;

destructor TDSAudioOut.Destroy;
begin
  inherited Destroy;
end;

procedure TDSAudioOut.Prepare;
var
  Res : HResult;
  Wnd : HWND;
  Form : TForm;
  FormatExt : TWaveFormatExtensible;
begin
  Freed := False;
  if (FDeviceNumber >= FDeviceCount) then raise EAuException.Create('Invalid device number');
  FInput.Init;
  Chan := FInput.Channels;
  SR := FInput.SampleRate;
  if FSpeedFactor <> 1 then
    SR := Round(SR*FSpeedFactor);
  BPS := FInput.BitsPerSample;
  if FLatency < 10 then Flatency := 10;
  FFramesInBuffer := FLatency*SR div 2000;
  FFramesInBuffer := FFramesInBuffer*2;
  Res := DSInitOutputDevice(DS, @(Devices.dinfo[FDeviceNumber].guid));
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound device');
  if Owner is TForm then
  begin
    Form := Owner as TForm;
    Wnd := Form.Handle;
  end else Wnd := 0;
  {$WARNINGS OFF}
  _BufSize := Integer(FFramesInBuffer*(BPS shr 3)*Chan);
  {$WARNINGS ON}
  GetMem(Buf, _BufSize);
  if BPS <> 8 then
    FillByte := 0
  else
    FillByte := 128;
//    Res := DSW_InitOutputBuffer(DSW, Wnd, BPS, SR, Chan, _BufSize);
  FillChar(FormatExt, SizeOf(FormatExt), 0);
  if (Chan < 3) then
  begin
    FormatExt.Format.wFormatTag := 1; //WAVE_FORMAT_PCM;
    FormatExt.Format.cbSize := 0;
    Res := DSInitOutputBuffer(DS, Wnd, BPS, SR, Chan, _BufSize);
  end else
  begin
    FormatExt.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
    FormatExt.Format.cbSize := SizeOf(FormatExt) - SizeOf(FormatExt.Format);
    FormatExt.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
    if Chan = 2 then
       FormatExt.dwChannelMask := $3;
    if Chan = 6 then
      FormatExt.dwChannelMask := $3F;
    if Chan = 8 then
      FormatExt.dwChannelMask := $FF;
    FormatExt.Format.nChannels := Chan;
    FormatExt.Format.nSamplesPerSec := SR;
    FormatExt.Format.wBitsPerSample := BPS;
    FormatExt.Format.nBlockAlign := Chan*BPS shr 3;
    FormatExt.Format.nAvgBytesPerSec :=  SR*FormatExt.Format.nBlockAlign;
    Res := DSInitOutputBufferEx(DS, Wnd, FormatExt, _BufSize);
  end;
  if Res <> 0 then raise EAuException.Create('Failed to create DirectSound buffer' + IntToHex(Res, 8));
  StartInput := True;
  EndOfInput := False;
  FFrameSize := (BPS shr 3)*Chan;
end;

procedure TDSAudioOut.Pause;
begin
  inherited Pause;
  if EndOfInput then Exit;
  DSStopOutput(DS);
end;

procedure TDSAudioOut.Resume;
begin
  if EndOfInput then Exit;
  DSStartOutput(DS);
  inherited Resume;
end;

procedure TDSAudioOut.Jump(Offs : Integer);
begin
  Pause;
  DSFillEmptySpace(DS, FillByte);
  if Assigned(Finput) then
  begin
    FInput.EmptyCache;
    if FInput is TAuConverter then
      TAuConverter(FInput)._Jump(Offs);
    if FInput is TAuFileIn then
      TAuFileIn(FInput)._Jump(Offs);
  end;
  Self.Resume;
end;


end.
