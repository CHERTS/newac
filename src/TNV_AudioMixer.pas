
(* $Id: TNV_AudioMixer.pas (C)DJ VK 2016 $ *)

unit TNV_AudioMixer;

interface

{$WARNINGS OFF}

uses
  Classes, {SysUtils,} TNV_AudioClasses,
  Math;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type
  TStereoMode = (LRtoLR, LRtoRL, LRtoLL, LRtoRR, LRtoAverLR);

  TInputMixerUnit = class(TProcUnit)
  private
    FInputBalance : Double;
    FSoftClip : Boolean;
    FSoftClipLevel : Double;
    FChannelShift : Double;
    FMixerMode : TStereoMode;
    FMatrixMidLevel : Double;
    FMatrixMidPanorama : Double;
    FMatrixSideLevel : Double;
    FMatrixSideBalance : Double;
    FMuteLeft : Boolean;
    FMuteRight : Boolean;
    FInverseLeft : Boolean;
    FInverseRight : Boolean;
    FMuteMiddle : Boolean;
    FMuteSide : Boolean;
    FInverseMiddle : Boolean;
    FInverseSide : Boolean;

    FShiftBuffer : array of Double;
    FShiftBufferSize : Integer;
    FShiftBufferPos : Integer;
    FSoftClipShape : Double;

    procedure SetInputBalance(Value : Double);
    procedure SetSoftClip(Value : Boolean);
    procedure SetSoftClipLevel(Value : Double);
    procedure SetChannelShift(Value : Double);
    procedure SetMixerMode(Value : TStereoMode);
    procedure SetMatrixMidLevel(Value : Double);
    procedure SetMatrixMidPanorama(Value : Double);
    procedure SetMatrixSideLevel(Value : Double);
    procedure SetMatrixSideBalance(Value : Double);
    procedure SetMuteLeft(Value : Boolean);
    procedure SetMuteRight(Value : Boolean);
    procedure SetInverseLeft(Value : Boolean);
    procedure SetInverseRight(Value : Boolean);
    procedure SetMuteMiddle(Value : Boolean);
    procedure SetMuteSide(Value : Boolean);
    procedure SetInverseMiddle(Value : Boolean);
    procedure SetInverseSide(Value : Boolean);

  protected
    procedure Changed;

  published
    property Bypass;
    property InputGain;
    property InputBalance : Double read FInputBalance write SetInputBalance;
    property SoftClip : Boolean read FSoftClip write SetSoftClip;
    property SoftClipLevel : Double read FSoftClipLevel write SetSoftClipLevel;
    property ChannelShift : Double read FChannelShift write SetChannelShift;
    property MixerMode : TStereoMode read FMixerMode write SetMixerMode;
    property MatrixMidLevel : Double read FMatrixMidLevel write SetMatrixMidLevel;
    property MatrixMidPanorama : Double read FMatrixMidPanorama write SetMatrixMidPanorama;
    property MatrixSideLevel : Double read FMatrixSideLevel write SetMatrixSideLevel;
    property MatrixSideBalance : Double read FMatrixSideBalance write SetMatrixSideBalance;
    property MuteLeft : Boolean read FMuteLeft write SetMuteLeft;
    property MuteRight : Boolean read FMuteRight write SetMuteRight;
    property InverseLeft : Boolean read FInverseLeft write SetInverseLeft;
    property InverseRight : Boolean read FInverseRight write SetInverseRight;
    property MuteMiddle : Boolean read FMuteMiddle write SetMuteMiddle;
    property MuteSide : Boolean read FMuteSide write SetMuteSide;
    property InverseMiddle : Boolean read FInverseMiddle write SetInverseMiddle;
    property InverseSide : Boolean read FInverseSide write SetInverseSide;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TInputMixerUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;

    property OutputGain : Double read FOutputGain;
  end;

  TOutputMixerUnit = class(TProcUnit)
  private
    FOutputBalance : Double;
    FStereoPhase : Double;
    FStereoBase : Double;
    FOutputMute : Boolean;

    FStereoEnhancer : Boolean;
    FEnhancerDelay : Double;
    
    FEnhancerFeedbackAmp : Double;
    FEnhancerCrossfeedAmp : Double;
    FEnhancerDryAmp : Double;

    FEnhBuffer : array [0..1] of array of Double;
    FEnhBufferSize : Integer;
    FEnhBufferPos : Integer;

    FPhaseSinCoef : Double;
    FPhaseCosCoef : Double;

    procedure SetOutputBalance(Value : Double);
    procedure SetStereoPhase(Value : Double);
    procedure SetStereoBase(Value : Double);
    procedure SetOutputMute(Value : Boolean);
    procedure SetStereoEnhancer(Value : Boolean);
    procedure SetEnhancerDelay(Value : Double);
    procedure SetEnhancerFeedbackAmp(Value : Double);
    procedure SetEnhancerCrossfeedAmp(Value : Double);
    procedure SetEnhancerDryAmp(Value : Double);
  protected
    procedure Changed;

  published
    property Bypass;
    property OutputGain;
    property OutputBalance : Double read FOutputBalance write SetOutputBalance;
    property StereoPhase : Double read FStereoPhase write SetStereoPhase;
    property StereoBase : Double read FStereoBase write SetStereoBase;
    property OutputMute : Boolean read FOutputMute write SetOutputMute;
    property StereoEnhancer : Boolean read FStereoEnhancer write SetStereoEnhancer;
    property EnhancerDelay : Double read FEnhancerDelay write SetEnhancerDelay;
    property EnhancerFeedbackAmp : Double read FEnhancerFeedbackAmp write SetEnhancerFeedbackAmp;
    property EnhancerCrossfeedAmp : Double read FEnhancerCrossfeedAmp write SetEnhancerCrossfeedAmp;
    property EnhancerDryAmp : Double read FEnhancerDryAmp write SetEnhancerDryAmp;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TOutputMixerUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;

    property InputGain : Double read FInputGain;
  end;

//////////////////////////////////

implementation

//////////////////////////////////

  constructor TInputMixerUnit.Create(AOwner: TComponent);
  begin
    inherited;
    FInputBalance := 0.0;
    FSoftClip := false;
    FSoftClipLevel := 1.0;
    FChannelShift := 0.0;
    FMixerMode := LRtoLR;
    FMatrixMidLevel := 1.0;
    FMatrixMidPanorama := 0.0;
    FMatrixSideLevel := 1.0;
    FMatrixSideBalance := 0.0;
    FMuteLeft := false;
    FMuteRight := false;
    FInverseLeft := false;
    FInverseRight := false;
    FMuteMiddle := false;
    FMuteSide := false;
    FInverseMiddle := false;
    FInverseSide := false;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TInputMixerUnit.Destroy;
  begin
    ResetUnit();
    inherited;
  end;

  procedure TInputMixerUnit.Assign(Source: TInputMixerUnit);
  var i : Integer;
  begin
    inherited Assign(Source as TProcUnit);
    FInputBalance := Source.FInputBalance;
    FSoftClip := Source.FSoftClip;
    FSoftClipLevel := Source.FSoftClipLevel;
    FChannelShift := Source.FChannelShift;
    FMixerMode := Source.FMixerMode;
    FMatrixMidLevel := Source.FMatrixMidLevel;
    FMatrixMidPanorama := Source.FMatrixMidPanorama;
    FMatrixSideLevel := Source.FMatrixSideLevel;
    FMatrixSideBalance := Source.FMatrixSideBalance;
    FMuteLeft := Source.FMuteLeft;
    FMuteRight := Source.FMuteRight;
    FInverseLeft := Source.FInverseLeft;
    FInverseRight := Source.FInverseRight;
    FMuteMiddle := Source.FMuteMiddle;
    FMuteSide := Source.FMuteSide;
    FInverseMiddle := Source.FInverseMiddle;
    FInverseSide := Source.FInverseSide;

    FSoftClipShape := Source.FSoftClipShape;

    FShiftBufferSize := Source.FShiftBufferSize;
    FShiftBufferPos := Source.FShiftBufferPos;
    SetLength(FShiftBuffer, FShiftBufferSize);
    for i :=0 to FShiftBufferSize do FShiftBuffer[i] := Source.FShiftBuffer[i];
  end;

  procedure TInputMixerUnit.InitUnit(sr, ch: Integer);
  var i : Integer;
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    FSoftClipShape := 1.0 / arctan(FSoftClipLevel);
    FShiftBufferSize := Trunc(0.1 * FSampleRate);
    FShiftBufferPos := 0;
    SetLength(FShiftBuffer, FShiftBufferSize);
    for i := 0 to FShiftBufferSize - 1 do FShiftBuffer[i] := 0.0;
    ResetUnitBuffer();
  end;

  procedure TInputMixerUnit.ResetUnitBuffer();
  var i : Integer;
  begin
    for i := 0 to FShiftBufferSize - 1 do FShiftBuffer[i] := 0.0;
  end;

  procedure TInputMixerUnit.ResetUnit();
  begin
    SetLength(FShiftBuffer, 0);
  end;

  procedure TInputMixerUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inpL, inpR, procM, procS : Double;
    nbuf : Integer;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
    end;
    if(Bypass) then Exit;

    inpL := sL * FInputGain;
    inpR := sR * FInputGain;
    if(FChannelsCnt = 1) then inpR := inpL;  //simple mono to stereo

    // balance in
    if (FInputBalance <> 0.0) then
    begin
      inpL := inpL * (1.0 - max(0.0, FInputBalance));
      inpR := inpR * (1.0 + min(0.0, FInputBalance));
    end;

    //soft clip
    if(FSoftClip) then
    begin
      inpL := FSoftClipShape * arctan(inpL * FSoftClipLevel);
      inpR := FSoftClipShape * arctan(inpR * FSoftClipLevel);
    end;

    //stereo matrix
    if(FChannelsCnt > 1) then
    begin
      case FMixerMode of
        LRtoLL: inpR := inpL;
        LRtoRR: inpL := inpR;
        LRtoAverLR:
          begin
            inpL := (inpL + inpR) / 2.0;
            inpR := inpL;
          end;
        LRtoRL:
          begin
           procM := inpL;
           inpL := inpR;
           inpR := procM;
          end;
      end;
      if(FmixerMode <= LRtoRL) then
      begin
        procM := (inpL + inpR) * 0.5;
        procS := (inpL - inpR) * 0.5;
        if FMuteMiddle then procM := 0.0;
        if FMuteSide then procS := 0.0;
        if FInverseMiddle then procM := -procM;
        if FInverseSide then procS := -procS;
        inpL := procM * FMatrixMidLevel * min(1.0, 1.0 - FMatrixMidPanorama)
              + procS * FMatrixSideLevel * min(1.0, 1.0 - FMatrixSideBalance);
        inpR := procM * FMatrixMidLevel * min(1.0, 1.0 + FMatrixMidPanorama)
              - procS * FMatrixSideLevel * min(1.0, 1.0 + FMatrixSideBalance);
      end;
    end;

    // mute
    if FMuteLeft then inpL := 0.0;
    if FMuteRight then inpR := 0.0;

    // phase
    if FInverseLeft then inpL := -inpL;
    if FInverseRight then inpR := -inpR;

    // delay
    FShiftBuffer[FShiftBufferPos] := inpL;
    FShiftBuffer[FShiftBufferPos + 1] := inpR;
    if(FChannelShift <> 0.0) then
    begin
      nbuf := Trunc(FSampleRate * (abs(FChannelShift) / 1000.0));
      nbuf := nbuf - (nbuf mod 2);
      if(FChannelShift < 0.0) then
        inpL := FShiftBuffer[(FShiftBufferPos - nbuf + FShiftBufferSize) mod FShiftBufferSize]
      else
        inpR := FShiftBuffer[(FShiftBufferPos - nbuf + 1 + FShiftBufferSize) mod FShiftBufferSize];
    end;
    FShiftBufferPos := (FShiftBufferPos + 2) mod FShiftBufferSize;

    sL := inpL * FOutputGain;
    sR := inpR * FOutputGain;
  end;

  procedure TInputMixerUnit.SetInputBalance(Value : Double);
  begin
    if(FInputBalance <> Value) then
    begin
      if(Value < -1.0) then Value := -1.0;
      if(Value > 1.0) then Value := 1.0;
      FInputBalance := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetSoftClip(Value : Boolean);
  begin
    if(FSoftClip <> Value) then
    begin
      FSoftClip := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetSoftClipLevel(Value : Double);
  begin
    if(FSoftClipLevel <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 1.0) then Value := 1.0;
      FSoftClipLevel := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetChannelShift(Value : Double);
  begin
    if(FChannelShift <> Value) then
    begin
      if(Value < -20.0) then Value := -20.0;
      if(Value > 20.0) then Value := 20.0;
      FChannelShift := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMixerMode(Value : TStereoMode);
  begin
    if(FMixerMode <> Value) then
    begin
      FMixerMode := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMatrixMidLevel(Value : Double);
  begin
    if(FMatrixMidLevel <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FMatrixMidLevel := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMatrixMidPanorama(Value : Double);
  begin
    if(FMatrixMidPanorama <> Value) then
    begin
      if(Value < -1.0) then Value := -1.0;
      if(Value > 1.0) then Value := 1.0;
      FMatrixMidPanorama := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMatrixSideLevel(Value : Double);
  begin
    if(FMatrixSideLevel <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FMatrixSideLevel := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMatrixSideBalance(Value : Double);
  begin
    if(FMatrixSideBalance <> Value) then
    begin
      if(Value < -1.0) then Value := -1.0;
      if(Value > 1.0) then Value := 1.0;
      FMatrixSideBalance := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMuteLeft(Value : Boolean);
  begin
    if(FMuteLeft <> Value) then
    begin
      FMuteLeft := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMuteRight(Value : Boolean);
  begin
    if(FMuteRight <> Value) then
    begin
      FMuteRight := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetInverseLeft(Value : Boolean);
  begin
    if(FInverseLeft <> Value) then
    begin
      FInverseLeft := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetInverseRight(Value : Boolean);
  begin
    if(FInverseRight <> Value) then
    begin
      FInverseRight := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMuteMiddle(Value : Boolean);
  begin
    if(FMuteMiddle <> Value) then
    begin
      FMuteMiddle := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetMuteSide(Value : Boolean);
  begin
    if(FMuteSide <> Value) then
    begin
      FMuteSide := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetInverseMiddle(Value : Boolean);
  begin
    if(FInverseMiddle <> Value) then
    begin
      FInverseMiddle := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.SetInverseSide(Value : Boolean);
  begin
    if(FInverseSide <> Value) then
    begin
      FInverseSide := Value;
	    Changed();
    end;
  end;

  procedure TInputMixerUnit.Changed();
  begin
    if(FActive) then ReInitFlag := true
    else InitUnit(FSampleRate, FChannelsCnt);
  end;

//////////////////////////////////////////

  constructor TOutputMixerUnit.Create(AOwner: TComponent);
  begin
    inherited;
    FOutputBalance := 0.0;
    FStereoPhase := 0.0;
    FStereoBase := 0.0;
    FOutputMute := false;
    FStereoEnhancer := false;
    FEnhancerDelay := 20.0;
    FEnhancerFeedbackAmp := 0.3;
    FEnhancerCrossfeedAmp := 0.3;
    FEnhancerDryAmp := 0.8;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TOutputMixerUnit.Destroy;
  begin
    ResetUnit();
    inherited;
  end;

  procedure TOutputMixerUnit.Assign(Source: TOutputMixerUnit);
  var i : Integer;
  begin
    inherited Assign(Source as TProcUnit);
    FOutputBalance := Source.FOutputBalance;
    FStereoPhase := Source.FStereoPhase;
    FStereoBase := Source.FStereoBase;
    FOutputMute := Source.FOutputMute;
    FStereoEnhancer := Source.FStereoEnhancer;
    FEnhancerDelay := Source.FEnhancerDelay;
    FEnhancerFeedbackAmp := Source.FEnhancerFeedbackAmp;
    FEnhancerCrossfeedAmp := Source.FEnhancerCrossfeedAmp;
    FEnhancerDryAmp := Source.FEnhancerDryAmp;

    FPhaseSinCoef := Source.FPhaseSinCoef;
    FPhaseCosCoef := Source.FPhaseCosCoef;

    FEnhBufferSize := Source.FEnhBufferSize;
    FEnhBufferPos := Source.FEnhBufferPos;
    SetLength(FEnhBuffer[0], FEnhBufferSize);
    SetLength(FEnhBuffer[1], FEnhBufferSize);
    for i :=0 to FEnhBufferSize do
    begin
     FEnhBuffer[0][i] := Source.FEnhBuffer[0][i];
     FEnhBuffer[1][i] := Source.FEnhBuffer[1][i];
    end;
  end;

  procedure TOutputMixerUnit.InitUnit(sr, ch: Integer);
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    FEnhBufferSize := Trunc(FEnhancerDelay * FSampleRate / 1000.0) + 1;
    FEnhBufferPos := 0;
    SetLength(FEnhBuffer[0], FEnhBufferSize);
    SetLength(FEnhBuffer[1], FEnhBufferSize);
    FPhaseSinCoef := sin(FStereoPhase * Pi / 180);
    FPhaseCosCoef := cos(FStereoPhase * Pi / 180);
    ResetUnitBuffer();
  end;

  procedure TOutputMixerUnit.ResetUnitBuffer();
  var i : Integer;
  begin
    for i := 0 to FEnhBufferSize - 1 do
    begin
      FEnhBuffer[0][i] := 0.0;
      FEnhBuffer[1][i] := 0.0;
    end;
  end;

  procedure TOutputMixerUnit.ResetUnit();
  begin
    SetLength(FEnhBuffer[0], 0);
    SetLength(FEnhBuffer[1], 0);
  end;

  procedure TOutputMixerUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inpL, inpR, procL, procR : Double;
    sb : Double;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
    end;
    if(Bypass) then Exit;
	
    inpL := sL * FInputGain;
    inpR := sR * FInputGain;

    // stereo base
    if(FStereoBase <> 0.0) then
    begin
      sb := FStereoBase;
      if(sb < 0.0) then sb := 0.5 * sb;
      procL := inpL + sb * inpL - sb * inpR;
      procR := inpR + sb * inpR - sb * inpL;
      inpL := procL;
      inpR := procR;
    end;

    // stereo phase
    if(FStereoPhase <> 0.0) then
    begin
      procL := FPhaseCosCoef* inpL - FPhaseSinCoef * inpR;
      procR := FPhaseSinCoef * inpL + FPhaseCosCoef * inpR;
      inpL := procL;
      inpR := procR;
    end;

    // stereo ehhance
    FEnhBuffer[0][FEnhBufferPos] := inpL;
    FEnhBuffer[1][FEnhBufferPos] := inpR;
    if FStereoEnhancer then
    begin
      procL := FEnhancerDryAmp* inpL - FEnhancerCrossfeedAmp * inpR - FEnhancerFeedbackAmp
              * FEnhBuffer[1][(FEnhBufferPos + 1) mod FEnhBufferSize];
      procR := FEnhancerDryAmp * inpR - FEnhancerCrossfeedAmp * inpL - FEnhancerFeedbackAmp
              * FEnhBuffer[0][(FEnhBufferPos + 1) mod FEnhBufferSize];
      inpL := procL;
      inpR := procR;
    end;
    FEnhBufferPos := (FEnhBufferPos + 1) mod FEnhBufferSize;

    // balance out
    if(FOutputBalance <> 0.0) then
    begin
      procL := inpL * (1.0 - max(0.0, FOutputBalance));
      procR := inpR * (1.0 + min(0.0, FOutputBalance));
      inpL := procL;
      inpR := procR;
    end;

    //mute
    if(FOutputMute) then
    begin
      inpL := 0.0;
      inpR := 0.0;
    end;

    // level
    sL := inpL * FOutputGain;
    sR := inpR * FOutputGain;
  end;

  procedure TOutputMixerUnit.SetOutputBalance(Value : Double);
  begin
    if(FOutputBalance <> Value) then
    begin
      if(Value < -1.0) then Value := -1.0;
      if(Value > 1.0) then Value := 1.0;
      FOutputBalance := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetStereoPhase(Value : Double);
  begin
    if(FStereoPhase <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 360.0) then Value := 360.0;
      FStereoPhase := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetStereoBase(Value : Double);
  begin
    if(FStereoBase <> Value) then
    begin
      if(Value < -1.0) then Value := -1.0;
      if(Value > 1.0) then Value := 1.0;
      FStereoBase := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetOutputMute(Value : Boolean);
  begin
    if(FOutputMute <> Value) then
    begin
      FOutputMute := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetStereoEnhancer(Value : Boolean);
  begin
    if(FStereoEnhancer <> Value) then
    begin
      FStereoEnhancer := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetEnhancerDelay(Value : Double);
  begin
    if(FEnhancerDelay <> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 50.0) then Value := 50.0;
      FEnhancerDelay := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetEnhancerFeedbackAmp(Value : Double);
  begin
    if(FEnhancerFeedbackAmp <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 0.9) then Value := 0.9;
      FEnhancerFeedbackAmp := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetEnhancerCrossfeedAmp(Value : Double);
  begin
    if(FEnhancerCrossfeedAmp <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 0.8) then Value := 0.8;
      FEnhancerCrossfeedAmp := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.SetEnhancerDryAmp(Value : Double);
  begin
    if(FEnhancerDryAmp <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 1.0) then Value := 1.0;
      FEnhancerDryAmp := Value;
	    Changed();
    end;
  end;

  procedure TOutputMixerUnit.Changed();
  begin
    if(FActive) then ReInitFlag := true
    else InitUnit(FSampleRate, FChannelsCnt);
  end;

{$WARNINGS ON}

end.


