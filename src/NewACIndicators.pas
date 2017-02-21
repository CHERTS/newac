(*
  This file is a part of New Audio Components package v 2.3
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: NewACIndicators.pas 1166 2010-02-01 14:43:35Z andrei.borovsky $ *)

unit NewACIndicators;

(* Title: NewACIndicators
    Components that allow to implement gain/volume indicators which may provide the dynamic gain/loudness data for your GUI. *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Procs, SyncObjs, GainAnalysis, Windows, Math;

const AverSize = 16; {inserted by DJ VK}
const FFTSize = 4096; {inserted by DJ VK}

type

  TSpectrumSize = (ss8pt,ss16pt,ss32pt,ss64pt,ss128pt,ss256pt);  {inserted by DJ VK}
  TSpectrumType = (stLinear, stLogarithm);                       {inserted by DJ VK}

  TIndicatorEvent = procedure(Sender : TComponent) of object;

  (* Class: TGainIndicator
      Calculates the perceived gain (loudness).
      Descends from <TAuConverter>.
      This component provides you the perceived gain for the audio data passing through.
      One possible use of this component is in an audio-recording program to indicate the level of the incoming signal.
      This component may also be useful when doing precise gain control. Only one instance of the component may be used in an application.
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
      Returns the current gain value. This value is calculated in abstract units in logarithmic scale and varies from 0 (silence) to 60 (the maximum loudness). *)
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
      For the smooth operation the latency should be set to about of 0.05 second and the <Interval> should be set to about 50.
      See the TDxAudioIn/TDxAudioOut FramesInBuffer and PollingInterval properties for setting the latency under DirectSound. *)
    property OnGainData : TIndicatorEvent read FOnGainData write FOnGainData;
  end;

  (* Class: TFastGainIndicator
      Calculates the perceived gain (loudness).
      Descends from <TAuConverter>.
      This component provides you the gain (averaged sum of squares) for the audio data passing through.
      One possible use of this component is in an audio-recording program to indicate (roughly) the level of the incoming signal.
      Use TGainIndicator for more precise measurements.
   *)

  TFastGainIndicator = class(TAuConverter)
  private
    Accum : Double;
    Counter : Int64;
    FISR : LongWord;
    FGainValue : Word;
    FInterval : LongWord;
    FElapsed : LongWord;
    FOnGainData : TIndicatorEvent;
    InBuffer : array of Single;
    FBufferSize : LongWord;
    FSampleSize : Word;
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    (* Property: GainValue
      Returns the current gain value. This value is calculated in abstract units in logarithmic scale and varies from 0 (absolute silence) to 10000 (the maximum loudness). *)
    property GainValue : Word read FGainValue;
  published
    (* Property: Interval
      Use this property to set the interval (in milliseconds) between two gain value updates and <OnGainData> events.
      This value sets the minimal interval between updates. The actual interval could be slightly longer depending on the system load. *)
    property Interval : LongWord read FInterval write FInterval;
    (* Property: OnGainData
      OnGainData event is called periodically with the period of approximately the <Interval> milliseconds.
      You can use this event to update your GUI gain indicators with the current <GainValue>.
      The general responsiveness of the GUI gain indicator depends on the <Interval> and on the system I/O latency.
      For the smooth operation the latency should be set to about of 0.05 second and the <Interval> should be set to about 50.
      See the TDxAudioIn/TDxAudioOut FramesInBuffer and PollingInterval properties for setting the latency under DirectSound. *)
    property OnGainData : TIndicatorEvent read FOnGainData write FOnGainData;
  end;


  (* Class: TSpectrumIndicator
      This component calculates a rough 8-point spectrum of the audio data passing through. It may be used to build a simple audio visualisation.
      Descends from <TAuConverter>.
   *)

  TSpectrumIndicator = class(TAuConverter)
  private
    FCount : LongWord;
    FLevels : array[0..7] of Single;
    FShadowLevels : array[0..7] of Single;
    //FScaleFactor : Double;
    FInterval : LongWord;
    FElapsed : LongWord;
    FOnGainData : TIndicatorEvent;
    FSampleSize : Word;
    function GetLevels(Index : LongWord) : Single;
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    (* Property: Levels
      Returns 8 level values for the spectrum. The index value should be in the range of 0-7.
      The levels are logarithmic scale values ranging from -40 (minimum) to 100 (maximum).
      In practice you can consider anything below zero as silence. *)
    property Levels[Index : LongWord] : Single read GetLevels;
  published
    (* Property: Interval
      Use this property to set the interval (in milliseconds) between two data updates and <OnGainData> events.
      This value sets the minimal interval between updates. The actual interval could be slightly longer depending on the system load. *)
    property Interval : LongWord read FInterval write FInterval;
    (* Property: OnGainData
      OnGainData event is called periodically with the period of approximately the <Interval> milliseconds.
      You can use this event to update your GUI spectrum indicators with the current <Levels>.
      The general responsiveness of the GUI indicator depends on the <Interval> and on the system I/O latency.
      For the smooth operation the latency should be set to about of 0.05 second and the <Interval> should be set to about 50.
      See the TDxAudioIn/TDxAudioOut FramesInBuffer and PollingInterval properties for setting the latency under DirectSound. *)
    property OnGainData : TIndicatorEvent read FOnGainData write FOnGainData;
  end;

  {inserted by DJ VK}
  TExSpectrumIndicator = class(TAuConverter)
  private
    FCount : LongWord;
    FLevels : array[0..255] of Double;
    FPeaks : array[0..255] of Double;
    FFreqs : array[0..255] of Single;
    FShadowLevels : array[0..AverSize-1, 0..(FFTSize-1)] of Double;
    FShadowPeaks : array[0..(FFTSize-1)] of Double;
    FWindow : array[0..(FFTSize-1)] of Double;
    FOffsetFloatBuf : array[0..(FFTSize-1)] of Double;
    FOffsetPoints : LongWord;
    FShadowArrayPos : LongWord;
    FShadowArrayFilled :  Boolean;
    FSpectrumInertia : LongWord;
    FPeakInertia : LongWord;
    FInterval : LongWord;
    FElapsed : LongWord;
    FOnGainData : TIndicatorEvent;
    FSampleSize : Word;
    FSpectrumSize: TSpectrumSize;
    FSpectrumType: TSpectrumType;
    FSSize : Integer;
    FSpectrumInertiaCoef : Single;
    FPeakInertiaCoef : Single;
    function GetFreqs(Index : LongWord) : Double;
    function GetLevels(Index : LongWord) : Double;
    function GetPeaks(Index : LongWord) : Double;
    procedure SetSpectrumSize(Value : TSpectrumSize);
    procedure SetSpectrumType(Value : TSpectrumType);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    (* Property: Levels
      Returns 256 level values for the spectrum. The index value should be in the range of 0-255.
      The levels are logarithmic scale values ranging from -40 (minimum) to 100 (maximum).
      In practice you can consider anything below zero as silence. *)
    property Freqs[Index : LongWord] : Double read GetFreqs;
    property Levels[Index : LongWord] : Double read GetLevels;
    property Peaks[Index : LongWord] : Double read GetPeaks;
  published
    (* Property: Interval
      Use this property to set the interval (in milliseconds) between two data updates and <OnGainData> events.
      This value sets the minimal interval between updates. The actual interval could be slightly longer depending on the system load. *)
    property Interval : LongWord read FInterval write FInterval;
    (* Property: OnGainData
      OnGainData event is called periodically with the period of approximately the <Interval> milliseconds.
      You can use this event to update your GUI spectrum indicators with the current <Levels>.
      The general responsiveness of the GUI indicator depends on the <Interval> and on the system I/O latency.
      For the smooth operation the latency should be set to about of 0.05 second and the <Interval> should be set to about 50.
      See the TDxAudioIn/TDxAudioOut FramesInBuffer and PollingInterval properties for setting the latency under DirectSound. *)
    property OnGainData : TIndicatorEvent read FOnGainData write FOnGainData;
    property SpectrumSize : TSpectrumSize read FSpectrumSize write SetSpectrumSize;
    property SpectrumType : TSpectrumType read FSpectrumType write SetSpectrumType;
    property SpectrumInertia : LongWord read FSpectrumInertia write FSpectrumInertia;
    property PeakInertia : LongWord read FPeakInertia write FPeakInertia;
  end;
  {end of insertion}

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
  FSize := FInput.Size;
  FPosition := 0;
end;

procedure TGainIndicator.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  i : Integer;
  SamplesRead, FramesRead : LongWord;
begin
  FPosition := FInput.Position;
  Finput.GetData(Buffer, Bytes);
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
    FElapsed := 0; //FElapsed - FInterval*100;
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
  FGainValue := 0;
  if Assigned(FOnGainData) then
     EventHandler.PostGenericEvent(Self, FOnGainData);
  Finput.Flush;
  Busy := False;
end;

  constructor TFastGainIndicator.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FInterval := 100
  end;

procedure TFastGainIndicator.InitInternal;
begin
  Busy := True;
  Accum := 0;
  Counter := 0;
  FPosition := 0;
  FInput.Init;
  if FInput.Channels > 2 then
  begin
    FInput.Flush;
    Busy := False;
    raise EAuException.Create('Only mono or stereo soures are supported.');
  end;
  FISR := FInput.SampleRate;
  FSampleSize := FInput.BitsPerSample div 8;
  FSize := FInput.Size;
  FPosition := 0;
end;

procedure TFastGainIndicator.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  i : Integer;
  SamplesRead : LongWord;
  CW : LongWord;
begin
  FPosition := FInput.Position;
  Finput.GetData(Buffer, Bytes);
  if (Buffer = nil) or (Bytes = 0) then
    Exit;
  SamplesRead := Bytes div FSampleSize;
  if SamplesRead > FBufferSize then
  begin
    FBufferSize := SamplesRead;
    SetLength(InBuffer, FBufferSize);
  end;
  case FSampleSize of
    1 : ByteToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    2 : SmallIntToSingle(PBuffer16(Buffer), @InBuffer[0], SamplesRead);
    3 : Int24ToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    4 : Int32ToSingle(PBuffer32(Buffer), @InBuffer[0], SamplesRead);
  end;
  CW := 0;
  SetSingleFPUPrecision(@CW);
  for i := 3 to SamplesRead - 1 do
  begin
    Accum := Accum + Sqr(InBuffer[i] - InBuffer[i-3]);
    Inc(Counter);
  end;
  FElapsed := FElapsed + Round((SamplesRead div Finput.Channels)/FISR*100000);
  if FElapsed >= FInterval*100 then
  begin
    FElapsed := 0; //FElapsed - FInterval*100;
    FGainValue := Round(Log10(1+ 1000000*Accum/Counter)*20);
    Accum := 0;
    Counter := 0;
    if Assigned(FOnGainData) then
       EventHandler.PostGenericEvent(Self, FOnGainData);
  end;
  RestoreCW(@CW);
end;

procedure TFastGainIndicator.FlushInternal;
begin
  FGainValue := 0;
  if Assigned(FOnGainData) then
     EventHandler.PostGenericEvent(Self, FOnGainData);
  Finput.Flush;
  Busy := False;
end;



  constructor TSpectrumIndicator.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FInterval := 100
  end;

procedure TSpectrumIndicator.InitInternal;
var
  i : Integer;
begin
  Busy := True;
  FPosition := 0;
  FInput.Init;
  FSampleSize := FInput.BitsPerSample div 8;
  FCount := 0;
  for i := 0 to 7 do Flevels[i] := 0;
  FSize := FInput.Size;
  FPosition := 0;
end;

{$DEFINE PTS16 }

const
{$IFDEF PTS16}
   Points = 16;
{$ENDIF}
{$IFDEF PTS32}
   Points = 32;
{$ENDIF}

procedure TSpectrumIndicator.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
const
  SamplesInBuf = 1536;
var
  i, j, Ch : Integer;
  SamplesRead, FramesRead : LongWord;
  InFloatBuf : array[0..SamplesInBuf-1] of Single;
  OutFloatBuf : array[0..SamplesInBuf-1] of Single;
  YFloatBuf : array[0..SamplesInBuf-1] of Single;
  InCmplx : array[0..Points-1] of TComplexSingle;
  CW : LongWord;
begin
  FPosition := FInput.Position;
  if Bytes > FSampleSize*SamplesInBuf then Bytes := FSampleSize*SamplesInBuf;
  Finput.GetData(Buffer, Bytes);
  if (Buffer = nil) or (Bytes = 0) then
    Exit;
  SamplesRead := Bytes div FSampleSize;
  case FSampleSize of
    1 : ByteToSingle(PBuffer8(Buffer), @InFloatBuf[0], SamplesRead);
    2 : SmallIntToSingle(PBuffer16(Buffer), @InFloatBuf[0], SamplesRead);
    3 : Int24ToSingle(PBuffer8(Buffer), @InFloatBuf[0], SamplesRead);
    4 : Int32ToSingle(PBuffer32(Buffer), @InFloatBuf[0], SamplesRead);
  end;
  FramesRead := SamplesRead div FInput.Channels;
  Ch := FInput.Channels;
  CW := 0;
  SetSingleFPUPrecision(@CW);
  for i := 0 to FramesRead - 1 do
  begin
    OutFloatBuf[i] := 0;
    for j := 0 to Ch - 1 do
      OutFloatBuf[i] := OutFloatBuf[i] + InFloatBuf[Ch*i +j];
    OutFloatBuf[i] := OutFloatBuf[i]/Ch;
  end;
  YFloatBuf[0] := OutFloatBuf[0];
  for i := 1 to FramesRead - 1 do
  YFloatBuf[i] := 0.9*(YFloatBuf[i-1] + OutFloatBuf[i] - OutFloatBuf[i-1]);

  for i := 0 to (FramesRead div Points) - 1 do
  begin
    for j := 0 to Points-1 do
    begin
      InCmplx[j].Re := YFloatBuf[i*Points + j];
      InCmplx[j].Im := 0;
    end;
//    try
    ComplexFFTSingle(@InCmplx, Points, 1);
    Inc(FCount);
//    except
//      Exit;
//    end;
    if Points = 16 then
{$IFDEF PTS16}
    for j := 0 to 7 do
       FShadowLevels[j] := FShadowLevels[j] + Sqrt(Sqr(InCmplx[j].Re) + Sqr(InCmplx[j].Im))
 {$ENDIF}
 {$IFDEF PTS32}
    for j := 0 to 7 do
       FShadowLevels[j] := FShadowLevels[j] + (Sqrt(Sqr(InCmplx[j*2].Re) + Sqr(InCmplx[j*2].Im)) + Sqrt(Sqr(InCmplx[j*2+1].Re) + Sqr(InCmplx[j*2+1].Im)));
 {$ENDIF}
  end;
  RestoreCW(@CW);
  FElapsed := FElapsed + Round(FramesRead/FInput.SampleRate*100000);
  if FElapsed >= FInterval*100 then
  begin
    FElapsed := 0; //FElapsed - FInterval*100;
    if FCount <> 0 then

    for j := 0 to 7 do
    begin
       FLevels[j] := (Log10(0.125*FShadowLevels[j]/(FCount)*Sin((j+0.5)*Pi/16) + 1e-4)+3)*40;
       FShadowLevels[j] := 0;
    end;
    FCount := 0;
    if Assigned(FOnGainData) then
       EventHandler.PostGenericEvent(Self, FOnGainData);
  end;
end;

procedure TSpectrumIndicator.FlushInternal;
begin
  Finput.Flush;
  Busy := False;
end;

function TSpectrumIndicator.GetLevels(Index: Cardinal) : Single;
begin
  if Index > 7 then
    Result := 0
  else
    Result := FLevels[Index];
end;

{inserted by DJ VK}
{TExSpectrumIndicator}

  constructor TExSpectrumIndicator.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FInterval := 100;
    FSpectrumType := stLinear;
    FSpectrumSize := ss8pt;
    FSSize := 8;
    FSpectrumInertia := 350;
    FPeakInertia := 500;
  end;

procedure TExSpectrumIndicator.InitInternal;
var
  i, j : Integer;
  Alpha : Double;
begin
  Busy := True;
  FPosition := 0;
  FInput.Init;
  FSampleSize := FInput.BitsPerSample div 8;
  FCount := 0;
  FSize := FInput.Size;
  FPosition := 0;
  FOffsetPoints := 0;
  for i := 0 to 255 do
  begin
    FLevels[i] := -100.0;
    FPeaks[i] := -100.0;
    FFreqs[i] := 0.0;
  end;   
  for j := 0 to AverSize - 1 do
    for i := 0 to (FFTSize - 1) do
      FShadowLevels[j, i] := 5.4e-11;  //2.2e-7;
  FShadowArrayPos := 0;
  FShadowArrayFilled := false;
  Alpha := 0.16;
  for i := 0 to FFTSize do
    FWindow[i] := 0.5 * ((1.0 - Alpha) - Cos(TwoPi * i / (FFTSize - 1)) + Alpha * Cos(2.0 * TwoPi * i / (FFTSize - 1)));
  FSpectrumInertiaCoef := FSpectrumInertia / FInterval;
  FPeakInertiaCoef := FPeakInertia / FInterval;
end;

procedure TExSpectrumIndicator.SetSpectrumSize(Value : TSpectrumSize);
begin
  if(Value <> FSpectrumSize) then
  begin
    FSpectrumSize := Value;
  case FSpectrumSize of
    ss8pt : FSSize := 8;
    ss16pt : FSSize := 16;
    ss32pt : FSSize := 32;
    ss64pt : FSSize := 64;
    ss128pt : FSSize := 128;
    ss256pt : FSSize := 256;
    end;
  end;
end;

procedure TExSpectrumIndicator.SetSpectrumType(Value : TSpectrumType);
begin
  if(Value <> FSpectrumType) then
  begin
    FSpectrumType := Value;
  end;
end;

procedure TExSpectrumIndicator.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
const
  SamplesInBuf = 2*FFTSize;
var
  i, j, k, Ch : Integer;
  BytesOffset : Integer;
  SamplesRead, FramesRead : LongWord;
  InFloatBuf : array[0..SamplesInBuf-1] of Single;
  OutFloatBuf : array[0..SamplesInBuf-1] of Double;
  YFloatBuf : array[0..SamplesInBuf-1] of Double;
  AvOutLevels : array[0..255] of Double;
  AvOutPeaks : array[0..255] of Double;
  InCmplx : array[0..FFTSize-1] of TComplex;
  Points, PointsToProc, Aver: Integer;
  CurrLevel, CurrPeak : Double;
  CurrFrInd : Integer;
begin
  FPosition := FInput.Position;
  Ch := FInput.Channels;
  BytesOffset := FOffsetPoints * Ch * FSampleSize;

  if(Bytes + BytesOffset  > FSampleSize * SamplesInBuf) then Bytes := FSampleSize*SamplesInBuf-BytesOffset;
  Finput.GetData(Buffer, Bytes);
  if (Buffer = nil) or (Bytes = 0) then
    Exit;
  SamplesRead := Bytes div FSampleSize;
  case FSampleSize of
    1 : ByteToSingle(PBuffer8(Buffer), @InFloatBuf[0], SamplesRead);
    2 : SmallIntToSingle(PBuffer16(Buffer), @InFloatBuf[0], SamplesRead);
    3 : Int24ToSingle(PBuffer8(Buffer), @InFloatBuf[0], SamplesRead);
    4 : Int32ToSingle(PBuffer32(Buffer), @InFloatBuf[0], SamplesRead);
  end;
  FramesRead := SamplesRead div FInput.Channels;

  for i:= 0 to FOffsetPoints-1 do
    OutFloatBuf[i] := FOffsetFloatBuf[i];

  for i := 0 to FramesRead - 1 do
  begin
    OutFloatBuf[i + FOffsetPoints] := 0;
    for j := 0 to Ch - 1 do
      OutFloatBuf[i + FOffsetPoints] := OutFloatBuf[i + FOffsetPoints] + InFloatBuf[Ch*i +j];
    OutFloatBuf[i + FOffsetPoints] := OutFloatBuf[i + FOffsetPoints]/Ch;
  end;

  Points := FFTSize;
  PointsToProc := ((FramesRead + FOffsetPoints) div Points)*Points;
  FOffsetPoints := FramesRead + FOffsetPoints - PointsToProc;

  for i := 0 to FOffsetPoints-1 do
    FOffsetFloatBuf[i] := OutFloatBuf[i + PointsToProc];

  for i := 0 to PointsToProc-1 do
  YFloatBuf[i] := OutFloatBuf[i];

  for i := 0 to (PointsToProc div Points) - 1 do
  begin
    for j := 0 to Points-1 do
    begin
      InCmplx[j].Re := YFloatBuf[i*Points + j] * FWindow[j];
      InCmplx[j].Im := 0;
    end;
	
    ComplexFFT(@InCmplx, Points, 1);

    for j := 0 to FFTSize-1 do
    begin
      CurrLevel := Sqrt(Sqr(InCmplx[j].Re) + Sqr(InCmplx[j].Im));
      FShadowLevels[FShadowArrayPos, j] := CurrLevel;
      if(FShadowPeaks[j] < CurrLevel) then FShadowPeaks[j] := CurrLevel;
    end;
    Inc(FShadowArrayPos);
    if(FShadowArrayPos = AverSize) then
    begin
      if not FShadowArrayFilled then FShadowArrayFilled := True;
      FShadowArrayPos := 0;
    end;
  end;
               
  FElapsed := FElapsed + Round(FramesRead / FInput.SampleRate * 100000);
  if FElapsed >= FInterval * 100 then
  begin
    FElapsed := 0; //FElapsed - FInterval*100;

    for j := 0 to FSSize-1 do
    begin
      AvOutLevels[j] := 0.0;
      AvOutPeaks[j] := 0.0;
      FFreqs[j] := 0.0;
    end;
    if FShadowArrayFilled then Aver := AverSize
    else Aver := FShadowArrayPos;
    for j := 0 to FFTSize div 2 - 1 do
    begin
      CurrLevel := 0.0;
      CurrPeak := 0.0;
      if Aver <> 0 then
      begin
        for k:= 0 to Aver - 1 do
        begin
          CurrLevel := CurrLevel + FShadowLevels[k, j] / FFTSize;
          if(CurrPeak  < FShadowLevels[k, j] / FFTSize) then CurrPeak := FShadowLevels[k, j] / FFTSize;
        end;
        CurrLevel := CurrLevel / Aver;
      end;

      case FSpectrumType of
        stLinear :
        begin
          CurrFrInd := (2 * j * FSSize) div FFTSize;
        end;
        stLogarithm :
        begin
          CurrFrInd := Trunc(1 + (FSSize-1)*log10(j+1)/log10(FFTSize/2));
        end;
      end;

      if(AvOutLevels[CurrFrInd] < CurrLevel) then AvOutLevels[CurrFrInd] := CurrLevel;
      if( AvOutPeaks[CurrFrInd] < CurrPeak)  then  AvOutPeaks[CurrFrInd] := CurrPeak;
      FFreqs[CurrFrInd] := (j+1) * FInput.SampleRate / (FFTSize);
    end;

    for j := 0 to FSSize-1 do
    begin
      CurrLevel := AvOutLevels[j];
      CurrPeak := AvOutPeaks[j];
      if(Currlevel < 5.4e-11) then CurrLevel := - 100.0    // 2.2e-7
      else CurrLevel := (Log10(CurrLevel)) * 20.0 + 85.8;// 33.07;

      if(CurrPeak < 5.4e-11) then CurrPeak := - 100.0      // 2.2e-7
      else CurrPeak := (Log10(CurrPeak)) * 20.0 + 85.8; //33.07;

      FLevels[j] := FLevels[j] + (CurrLevel - FLevels[j]) / FSpectrumInertiaCoef;
      if(FPeaks[j] < CurrPeak) then FPeaks[j] := CurrPeak
      else FPeaks[j] := FPeaks[j] + (CurrPeak - FPeaks[j]) / FPeakInertiaCoef;
    end;
    if Assigned(FOnGainData) then
      EventHandler.PostGenericEvent(Self, FOnGainData);
  end;
end;

procedure TExSpectrumIndicator.FlushInternal;
begin
  Finput.Flush;
  Busy := False;
end;

function TExSpectrumIndicator.GetFreqs(Index: Cardinal) : Double;
begin
  Result := FFreqs[Index];
end;

function TExSpectrumIndicator.GetLevels(Index: Cardinal) : Double;
begin
  if Index > FSSize then
    Result := -100.0
  else
    Result := FLevels[Index];
end;

function TExSpectrumIndicator.GetPeaks(Index: Cardinal) : Double;
begin
  if Index > FSSize then
    Result := -100.0
  else
    Result := FPeaks[Index];
end;
{end of insertion}

end.
