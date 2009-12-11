(*
  This file is a part of New Audio Components package v 2.3
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewACIndicators;

(* Title: NewACIndicators
    Components that allow to implement gain/volume indicators which may provide the dynamic gain/loudness data for your GUI. *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Procs, SyncObjs, GainAnalysis, Windows;

type

  TIndicatorEvent = procedure(Sender : TComponent) of object;

  (* Class: TGainIndicator
      Calculates the perceived gain (loudness).
      Descends from <TAuConverter>.
      This component provides you the perceived gain for the audio data passing through.
      One possible use of this component is in an audio-recording program to indicate the level of the incoming signal.
      See the DirectSoundRecorder demo for an example.
      The component requires libgain.dll.
   *)

  TGainIndicator = class(TAuConverter)
  private
    FISR : LongWord;
    FGainValue : Double;
    //FScaleFactor : Double;
    FInterval : LongWord;
    FElapsed : LongWord;
    FOnGainData : TIndicatorEvent;
    InBuffer : array of Single;
    LBuffer, RBuffer : array of Double;
    FBufferSize : LongWord;
    FSampleSize : Word;
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    (* Property: GainValue
      Returns the curreent gain value. This value is calculated in abstract units in logarithmic scale and varies from 0 (silence) to 60 (the maximum loudness). *)
    property GainValue : Double read FGainValue;
  published
    (* Property: Interval
      Use this property to set the interval (in milliseconds) between two gain value updates and <OnGainData> events.
      This value sets the minimal interval between updates. The actual interval could be slightly longer depending on the system load. *)
    property Interval : LongWord read FInterval write FInterval;
    (* Property: OnGainData
      OnGainData event is called periodically with the period of approximately the <Interval> milliseconds.
      You can use this event to update your GUI gain indicators with the current <GainValue>.
      The general responsiveness of the GUI gain indicator depends on the <Interval> and on the system I/O latency.
      For the smooth operation the latency should be set to about of 0.05 second and the <Interval> should be set to about 40.
      See the TDxAudioIn/TDxAudioOut FramesInBuffer and PollingInterval properties for setting the latency under DirectSound. *)
    property OnGainData : TIndicatorEvent read FOnGainData write FOnGainData;
  end;

implementation

  constructor TGainIndicator.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FInterval := 100
  end;

procedure TGainIndicator.InitInternal;
begin
  LoadLibGain;
  if not LibGainLoaded then
    raise EAuException.Create(Format('Could not load the %s library.', [LibGainPath]));
  Busy := True;
  FPosition := 0;
  FInput.Init;
  if FInput.Channels > 2 then
  begin
    FInput.Flush;
    Busy := False;
    raise EAuException.Create('Only mono or stereo soures are supported.');
  end;
  FISR := FInput.SampleRate;
  if InitGainAnalysis(FISR) <> GAIN_ANALYSIS_OK then
  begin
    FInput.Flush;
    Busy := False;
    raise EAuException.Create(Format('Failed to set up gain analysis. Possible cause: sample rate %d is not supported.', [FISR]));
  end;
  FSampleSize := FInput.BitsPerSample div 8;
  //FScaleFactor := 1;
end;

procedure TGainIndicator.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  i : Integer;
  SamplesRead, FramesRead : LongWord;
begin
  Finput.GetData(Buffer, Bytes);
  if Bytes = 0 then
  begin
    Exit;
  end;
  if (Buffer = nil) or (Bytes = 0) then
    Exit;
  SamplesRead := Bytes div FSampleSize;
  if SamplesRead > FBufferSize then
  begin
    FBufferSize := SamplesRead;
    SetLength(InBuffer, FBufferSize);
    SetLength(LBuffer, FBufferSize);
    SetLength(RBuffer, FBufferSize);
  end;
  case FSampleSize of
    1 : ByteToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    2 : SmallIntToSingle(PBuffer16(Buffer), @InBuffer[0], SamplesRead);
    3 : Int24ToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    4 : Int32ToSingle(PBuffer32(Buffer), @InBuffer[0], SamplesRead);
  end;
  FramesRead := SamplesRead div FInput.Channels;
  if Finput.Channels = 2 then
  begin
    for i := 0 to FramesRead - 1 do
    begin
      RBuffer[i] := InBuffer[i*2]*$8000;
      LBuffer[i] := InBuffer[i*2+1]*$8000;
    end;
  end else
    for i := 0 to FramesRead - 1 do
  begin
    RBuffer[i] := InBuffer[i]*$8000;
    LBuffer[i] := RBuffer[i];
  end;
  if AnalyzeSamples(@LBuffer[0], @RBuffer[0], FramesRead, 2) = GAIN_ANALYSIS_ERROR then
    raise EAuException.Create('Gain analysis failed');
  FElapsed := FElapsed + Round(FramesRead/FISR*100000);
  if FElapsed >= FInterval*100 then
  begin
    FElapsed := 0;
    FGainValue := GetTitleGain;
    if FGainValue > 32 then FGainValue := 32;
    FGainValue := 32 - FGainValue;
    if FGainValue > 100 then FGainValue := 0; //FScaleFactor := FScaleFactor/2;
    if Assigned(FOnGainData) then
       EventHandler.PostGenericEvent(Self, FOnGainData);
  end;
end;

procedure TGainIndicator.FlushInternal;
begin
  Finput.Flush;
  Busy := False;
  FGainValue := 0;
end;


end.
