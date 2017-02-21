
(* $Id: TNV_AudioDynamics.pas (C)DJ VK 2016 $ *)

unit TNV_AudioDynamics;

interface

{$WARNINGS OFF}

uses
  Classes, Math, TNV_AudioFilters, TNV_AudioClasses;


const
  BUF_SIZE = $4000;
  BufSize = $6000;

type
  TSingleArray = array of Single;
  PSingleArray = ^TSingleArray;
  TDoubleArray = array of Double;
  PDoubleArray = ^TDoubleArray;

  TDynMode = (dynCompressor, dynExpander);
  TDetectionMode = (detRMS, detPeak);
  TStereoLinkMode = (slAverage, slLouder);
  TDeesserMode = (deWide, deSplit);
  TDeeserFilterEnum = (dsHPF, dsLPF, dsPK);

  TDynProcessor = class(TProcUnit)
  private
    FDynMode : TDynMode;

    FAttackTime : Double;
    FReleaseTime : Double;
    FThreshold : Double;
    FRatio : Double;
    FKnee : Double;
    FMakeupGain : Double;
    FDetection : TDetectionMode;
    FStereoLink : TStereoLinkMode;

    FAttackCoef : Double;
    FReleaseCoef : Double;
    FLinThreshold : Double;
    FLinSlope : Double;
    FLinKneeStart : Double;
    FAdjKneeStart : Double;
    FLinKneeStop : Double;
    FThres : Double;
    FKneeStart : Double;
    FKneeStop : Double;

    procedure SetAttackTime(Value : Double);
    procedure SetReleaseTime(Value : Double);
    procedure SetThreshold(Value : Double);
    procedure SetRatio(Value : Double);
    procedure SetKnee(Value : Double);
    procedure SetMakeupGain(Value : Double);
    procedure SetDetection(Value : TDetectionMode);
    procedure SetStereoLink(Value : TStereoLinkMode);

    procedure DynInit();
    procedure DynProc(var inL : Double; var inR : Double); overload;
    procedure DynProc(var inL : Double; var inR : Double; detL, detR : Double); overload;
    function HermiteInterpolation(x, x0, x1, p0, p1, m0, m1 : Double) : Double;
  protected
    FMaxGateReduction : Double;
    function OutputDynGain(linSlope : Double; rms : Boolean) : Double;
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property AttackTime : Double read FAttackTime write SetAttackTime;
    property ReleaseTime : Double read FReleaseTime write SetReleaseTime;
    property Threshold : Double read FThreshold write SetThreshold;
    property Ratio : Double read FRatio write SetRatio;
    property Knee : Double read FKnee write SetKnee;
    property MakeupGain : Double read FMakeupGain write SetMakeupGain;
    property Detection : TDetectionMode read FDetection write SetDetection;
    property StereoLink : TStereoLinkMode read FStereoLink write SetStereoLink;
  public
    constructor Create(AOwner : TComponent; Mode : TDynMode);
    destructor Destroy; override;
    procedure Assign(Source: TDynProcessor);
  end;

  TCompressorUnit = class(TDynProcessor)
  private
    FMix : Double;
    procedure SetMix(Value : Double);
  protected
    procedure Changed; override;
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property Mix : Double read FMix write SetMix;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TCompressorUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
    function GetOutLevel(inp : Double): Double;
  end;

  TGateUnit = class(TDynProcessor)
  private
    procedure SetMaxGateReduction(Value : Double);
  protected
    procedure Changed; override;
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property MaxGateReduction : Double read FMaxGateReduction write SetMaxGateReduction;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TGateUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
    function GetOutLevel(inp : Double): Double;
  end;

  TDeesserUnit = class(TDynProcessor)
  private
    FMode : TDeesserMode;
    FLaxity : Double;
    FSplitFreq : Double;
    FSplitLevel : Double;
    FPeakFreq : Double;
    FPeakLevel : Double;
    FPeakQ : Double;
    FSClisten : Boolean;

    FLowPassFilter : TBiquadFilter;
    FHighPassFilter : TBiquadFilter;
    FPeakFilter : TBiquadFilter;

    procedure SetMode(Value : TDeesserMode);
    procedure SetLaxity(Value : Double);
    procedure SetSplitFreq(Value : Double);
    procedure SetSplitLevel(Value : Double);
    procedure SetPeakFreq(Value : Double);
    procedure SetPeakLevel(Value : Double);
    procedure SetPeakQ(Value : Double);
    procedure SetSClisten(Value : Boolean);

  protected
    procedure Changed; override;
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property Mode : TDeesserMode read FMode write SetMode;
    property Laxity : Double read FLaxity write SetLaxity;
    property SplitFreq : Double read FSplitFreq write SetSplitFreq;
    property SplitLevel : Double read FSplitLevel write SetSplitLevel;
    property PeakFreq : Double read FPeakFreq write SetPeakFreq;
    property PeakLevel : Double read FPeakLevel write SetPeakLevel;
    property PeakQ : Double read FPeakQ write SetPeakQ;
    property SClisten : Boolean read FSClisten write SetSClisten;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TDeesserUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
    function GetAFR(f, sr : Double; findex : TDeeserFilterEnum): Double;

    property AttackTime : Double read FAttackTime;
    property ReleaseTime : Double read FReleaseTime;
    property Knee : Double read FKnee;
    property StereoLink : TStereoLinkMode read FStereoLink;

  end;

  //////////////////////////////////////

  implementation

  //////////////////////////////////////

  constructor TDynProcessor.Create(AOwner: TComponent; Mode : TDynMode);
  begin
    FDynMode := Mode;
    inherited Create(AOwner);
  end;

  destructor TDynProcessor.Destroy;
  begin
    inherited;
  end;

  procedure TDynProcessor.Assign(Source: TDynProcessor);
  begin
    inherited Assign(Source as TProcUnit);
    FDynMode := Source.FDynMode;

    FAttackTime := Source.FAttackTime;
    FReleaseTime := Source.FReleaseTime;
    FThreshold := Source.FThreshold;
    FRatio := Source.FRatio;
    FKnee := Source.FKnee;
    FMakeupGain := Source.FMakeupGain;
    FDetection := Source.FDetection;
    FStereoLink := Source.FStereoLink;

    FAttackCoef := Source.FAttackCoef;
    FReleaseCoef := Source.FReleaseCoef;
    FLinThreshold := Source.FLinThreshold;
    FLinSlope := Source.FLinSlope;
    FLinKneeStart := Source.FLinKneeStart;
    FAdjKneeStart := Source.FAdjKneeStart;
    FLinKneeStop := Source.FLinKneeStop;
    FThres := Source.FThres;
    FKneeStart := Source.FKneeStart;
    FKneeStop := Source.FKneeStop;
  end;

  procedure TDynProcessor.DynInit();
  var
    LinKneeSqrt : Double;
  begin
    FAttackCoef := min(1.0, 1.0 / (FAttackTime * FSampleRate / 4000.0));
    FReleaseCoef := min(1.0, 1.0 / (FReleaseTime * FSampleRate / 4000.0));
    FLinThreshold := FThreshold;
    if (FDetection = detRMS) and (FDynMode = dynExpander) then FLinThreshold := FLinThreshold * FLinThreshold;
    FLinSlope := 0.0;
    LinKneeSqrt := sqrt(FKnee);
    FLinKneeStart := FLinThreshold / LinKneeSqrt;
    FAdjKneeStart := FLinKneeStart * FLinKneeStart;
    FLinKneeStop := FLinThreshold * LinKneeSqrt;
    FThres := ln(FLinThreshold);
    FKneeStart := ln(FLinKneeStart);
    FKneeStop := ln(FLinKneeStop);
  end;

  procedure TDynProcessor.DynProc(var inL : Double; var inR : Double);
  begin
    DynProc(inL, inR, inL, inR);
  end;

  procedure TDynProcessor.DynProc(var inL : Double; var inR : Double; detL, detR : Double);
  var
    absample, gain : Double;
  begin            // thor's compressor module
    gain := 1.0;
    absample := detL;
    if(FChannelsCnt > 1) then
    begin
      case FStereoLink of
        slAverage: absample := (abs(detL) + abs(detR)) * 0.5;
        slLouder: if absample < abs(detR) then absample := abs(detR);
      end;
    end;
    if FDetection = detRMS then absample := absample * absample;
    //dsp::sanitize(linSlope);
    if(absample > FLinSlope) then
      FLinSlope := FLinSlope + (absample - FLinSlope) * FAttackCoef
    else FLinSlope := FLinSlope + (absample - FLinSlope) *  FReleaseCoef;
    if(FLinSlope > 0.0) then gain := OutputDynGain(FLinSlope, (FDetection = detRMS));
    inL := inL * gain * FMakeupGain;
    if(FChannelsCnt > 1) then inR := inR * gain * FMakeupGain;
  end;

  function TDynProcessor.OutputDynGain(linSlope : Double; rms : Boolean) : Double;
  var
    lim, gain, delta, slope, tratio : Double;
    FakeInfinity : Double;
  begin            //thor's work
    FakeInfinity := 65536.0 * 65536.0;
    if(FDynMode = dynCompressor) then
    begin
      if rms then lim := FAdjKneeStart
      else lim := FLinKneeStart;
      if(linSlope > lim) then
      begin
        slope := ln(linSlope);
        if rms then slope := 0.5 * slope;
        if(abs(ratio - FakeInfinity) < 1.0) then
        begin
          gain := FThres;
          delta := 0.0;
        end
        else
        begin
          gain := (slope - FThres) / FRatio + FThres;
          delta := 1.0 / FRatio;
        end;
        if(FKnee > 1.0) and (slope < FKneeStop) then
          gain := HermiteInterpolation(slope, FKneeStart, FKneeStop,
                   FKneeStart, (FKneeStop - FThres) / FRatio + FThres, 1.0, delta);
        Result := exp(gain - slope);
      end
      else Result := 1.0;
    end
    else
    begin
      if(linSlope < FLinKneeStop) then
      begin
        slope := ln(linSlope);
        tratio := FRatio;
        if(abs(FRatio - FakeInfinity) < 1.0) then tratio := 1000.0;
        gain := (slope - FThres) * tratio + FThres;
        delta := tratio;
        if(FKnee > 1.0) and (slope > FKneeStart) then
          gain := HermiteInterpolation(slope, FKneeStart, FKneeStop,
                  (FKneeStart - FThres) * tratio  + FThres, FKneeStop, delta, 1.0);
        Result := max(FMaxGateReduction, exp(gain - slope));
      end
      else Result := 1.0;
    end;
  end;

  function TDynProcessor.HermiteInterpolation(x, x0, x1, p0, p1, m0, m1 : Double) : Double;
  var
    width, t, t2, t3 : Double;
    ct0, ct1, ct2, ct3 : Double;
  begin
    width := x1 - x0;
    t := (x - x0) / width;
    m0 := m0 * width;
    m1 := m1 * width;
    t2 := t*t;
    t3 := t2*t;
    ct0 := p0;
    ct1 := m0;
    ct2 := -3 * p0 - 2 * m0 + 3 * p1 - m1;
    ct3 := 2 * p0 + m0  - 2 * p1 + m1;
    Result := ct3 * t3 + ct2 * t2 + ct1 * t + ct0;
  end;

  procedure TDynProcessor.SetAttackTime(Value : Double);
  begin
    if(FAttackTime <> Value) then
    begin
      if(Value < 0.01) then Value := 0.01;
      if(Value > 2000.0) then Value := 2000.0;
      FAttackTime := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetReleaseTime(Value : Double);
  begin
    if(FReleaseTime <> Value) then
    begin
      if(Value < 0.01) then Value := 0.01;
      if(Value > 2000.0) then Value := 2000.0;
      FReleaseTime := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetThreshold(Value : Double);
  begin
    if(FThreshold <> Value) then
    begin
      if(Value < 0.0009765625) then Value := 0.0009765625;
      if(Value > 1.0) then Value := 1.0;
      FThreshold := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetRatio(Value : Double);
  begin
    if(FRatio <> Value) then
    begin
      if(Value < 1.0) then Value := 1.0;
      if(Value > 20.0) then Value := 20.0;
      FRatio := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetKnee(Value : Double);
  begin
    if(FKnee <> Value) then
    begin
      if(Value < 1.0) then Value := 1.0;
      if(Value > 8.0) then Value := 8.0;
      FKnee := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetMakeupGain(Value : Double);
  begin
    if(FMakeupGain <> Value) then
    begin
      if(Value < 1.0) then Value := 1.0;
      if(Value > 64.0) then Value := 64.0;
      FMakeupGain := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetDetection(Value : TDetectionMode);
  begin
    if(FDetection <> Value) then
    begin
      FDetection := Value;
      Changed();
    end;
  end;

  procedure TDynProcessor.SetStereoLink(Value : TStereoLinkMode);
  begin
    if(FStereoLink <> Value) then
    begin
      FStereoLink := Value;
      Changed();
    end;
  end;

  ///////////////////////////////////

  constructor TCompressorUnit.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner, dynCompressor);
    FAttackTime := 20.0;
    FReleaseTime := 250.0;
    FThreshold := 0.125;
    FRatio := 2;
    FKnee := 2.828427;
    FMakeupGain := 2.0;
    FDetection := detRMS;
    FStereoLink := slAverage;

    FMix := 1.0;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TCompressorUnit.Destroy;
  begin
    inherited;
  end;

  procedure TCompressorUnit.Assign(Source: TCompressorUnit);
  begin
    inherited Assign(Source as TDynProcessor);
    FMix := Source.FMix;
  end;

  procedure TCompressorUnit.InitUnit(sr, ch: Integer);
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    DynInit();
  end;

  procedure TCompressorUnit.ResetUnitBuffer();
  begin
  end;

  procedure TCompressorUnit.ResetUnit();
  begin
  end;

  procedure TCompressorUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp: array [0..1] of Double;
    j : Integer;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
    end;
    if(Bypass) then Exit;
    for j :=  0 to FChannelsCnt - 1 do
    begin
      case j of
        0: inp[j] := sL;
        1: inp[j] := sR;
      end;
      outp[j] := inp[j] * FInputGain;
    end;
    if(FChannelsCnt < 2) then outp[1] := 0.0;
    DynProc(outp[0], outp[1]);
    for j :=  0 to FChannelsCnt - 1 do
    begin
      outp[j] := outp[j] * FMix + inp[j] * (1.0 - FMix);
      case j of
        0: sL := outp[j] * FOutputGain;
        1: sR := outp[j] * FOutputGain;
      end;
    end;
  end;

  function TCompressorUnit.GetOutLevel(inp : Double): Double;
  begin
    Result := inp * OutputDynGain(inp, false) * FMakeupGain;
  end;

  procedure TCompressorUnit.SetMix(Value : Double);
  begin
    if(FMix <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 1.0) then Value := 1.0;
      FMix := Value;
      Changed();
    end;
  end;

  procedure TCompressorUnit.Changed;
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
  end;

  ///////////////////////////////////

  constructor TGateUnit.Create(AOwner: TComponent);
  begin
    inherited Create(Aowner, dynExpander);
    FAttackTime := 20.0;
    FReleaseTime := 250.0;
    FThreshold := 0.125;
    FRatio := 2;
    FKnee := 2.828427;
    FMakeupGain := 1.0;
    FDetection := detRMS;
    FStereoLink := slAverage;

    FMaxGateReduction := 1.0 / 16.0;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TGateUnit.Destroy;
  begin
    inherited;
  end;

  procedure TGateUnit.Assign(Source: TGateUnit);
  begin
    inherited Assign(Source as TDynProcessor);
    FMaxGateReduction := Source.FMaxGateReduction;
  end;

  procedure TGateUnit.InitUnit(sr, ch: Integer);
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    DynInit();
  end;

  procedure TGateUnit.ResetUnitBuffer();
  begin
  end;

  procedure TGateUnit.ResetUnit();
  begin
  end;

  procedure TGateUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp: array [0..1] of Double;
    j : Integer;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
    end;
    if(Bypass) then Exit;
    for j :=  0 to FChannelsCnt - 1 do
    begin
      case j of
        0: inp[j] := sL;
        1: inp[j] := sR;
      end;
      outp[j] := inp[j] * FInputGain;
    end;
    if(FChannelsCnt < 2) then outp[1] := 0.0;
    DynProc(outp[0], outp[1]);
    for j :=  0 to FChannelsCnt - 1 do
    begin
      outp[j] := outp[j] * FOutputGain;
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  function TGateUnit.GetOutLevel(inp : Double): Double;
  var
    slope: Double;
    rms : Boolean;
  begin
    slope := inp;
    rms := (FDetection = detRMS);
    if rms then slope := slope * slope;
    Result := inp * OutputDynGain(slope, rms) * FMakeupGain;
  end;

  procedure TGateUnit.SetMaxGateReduction(Value : Double);
  begin
    if(FMaxGateReduction <> Value) then
    begin
      if(Value < 0.000015849) then Value := 0.000015849;    //-96db
      if(Value > 1.0) then Value := 1.0;
      FMaxGateReduction := Value;
      Changed();
    end;
  end;

  procedure TGateUnit.Changed;
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
  end;

  ///////////////////////////////////

  constructor TDeesserUnit.Create(AOwner: TComponent);
  begin
    inherited Create(Aowner, dynCompressor);
    FAttackTime := 15.0;
    FReleaseTime := 20.0;
    FThreshold := 0.125;
    FRatio := 3;
    FKnee := 2.828427;
    FMakeupGain := 1.0;
    FDetection := detRMS;
    FStereoLink := slAverage;

    FMode := deWide;
    FLaxity := 15;
    FSplitFreq := 6000;
    FSplitLevel := 0.0;
    FPeakFreq := 4500;
    FPeakLevel := 0.0;
    FPeakQ := 1;
    FSClisten := false;

    FLowPassFilter := TBiquadFilter.Create(AOwner);
    FHighPassFilter := TBiquadFilter.Create(AOwner);
    FPeakFilter := TBiquadFilter.Create(AOwner);

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TDeesserUnit.Destroy;
  begin
    FLowPassFilter.Free;
    FHighPassFilter.Free;
    FPeakFilter.Free;
    inherited;
  end;

  procedure TDeesserUnit.Assign(Source: TDeesserUnit);
  begin
    inherited Assign(Source as TDynProcessor);

    FMode := Source.FMode;
    FLaxity := Source.FLaxity;
    FSplitFreq := Source.FSplitFreq;
    FSplitLevel := Source.FSplitLevel;
    FPeakFreq := Source.FPeakFreq;
    FPeakLevel := Source.FPeakLevel;
    FPeakQ := Source.FPeakQ;
    FSClisten := Source.FSClisten;

    FLowPassFilter.Assign(Source.FLowPassFilter);
    FHighPassFilter.Assign(Source.FHighPassFilter);
    FPeakFilter.Assign(Source.FPeakFilter);
  end;

  procedure TDeesserUnit.InitUnit(sr, ch: Integer);
  var
    FC1, FC2 : Double ;
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    
    FC1 := FSplitFreq * (1.0 - 0.17);
    FC2 := FSplitFreq * (1.0 + 0.17);
    FHighPassFilter.Init(FC1, 0.707, FSampleRate, FSplitLevel, Biquad_HPF, QisQ);
    FLowPassFilter.Init(FC2, 0.707, FSampleRate, 0.0, Biquad_LPF, QisQ);
    FPeakFilter.Init(FPeakFreq, FPeakQ, FSampleRate, FPeakLevel, Biquad_PKF, QisQ);

    FAttackTime := FLaxity;
    FReleaseTime := 1.33 * FLaxity;
    FKnee := 2.8;
    FStereoLink := slAverage;
  end;

  procedure TDeesserUnit.ResetUnitBuffer();
  begin
    FHighPassFilter.ResetBuffer();
    FLowPassFilter.ResetBuffer();
    FPeakFilter.ResetBuffer();
  end;

  procedure TDeesserUnit.ResetUnit();
  begin
  end;

  procedure TDeesserUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp : array [0..1] of Double;
    AC, SC, RC, MC : array [0..1] of Double;
    j : Integer;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
    end;
    if(Bypass) then Exit;
    for j :=  0 to FChannelsCnt - 1 do
    begin
      case j of
        0: inp[j] := sL;
        1: inp[j] := sR;
      end;
      inp[j] := inp[j] * FInputGain;
      outp[j] := 0.0;
      AC[j] := inp[j];
      SC[j] := inp[j];
      RC[j] := inp[j];
      MC[j] := inp[j];
      
      FPeakFilter.ProcFilter(SC[j], j);
      MC[j] := SC[j];
      if FMode = deSplit then
      begin
        //hpL.sanitize();
        //hpR.sanitize();
        FHighPassFilter.ProcFilter(RC[j], j);
        FLowPassFilter.ProcFilter(AC[j], j);
      end;
    end;
    if(FChannelsCnt < 2) then
    begin
      AC[1] := 0.0;
      RC[1] := 0.0;
      SC[1] := 0.0;
      MC[1] := 0.0;
    end;
    if (FMode = deSplit) then DynProc(RC[0], RC[1], SC[0], SC[1])
    else DynProc(AC[0], AC[1], SC[0], SC[1]);
    for j :=  0 to FChannelsCnt - 1 do
    begin
      if FMode = deSplit then AC[j] := AC[j] * FOutputGain + RC[j]
      else  AC[j] := AC[j] * FOutputGain;
      if FSClisten then outp[j] := MC[j]
      else outp[j] := AC[j];  //* FOutputGain - I dont know why not here.
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  function TDeesserUnit.GetAFR(f, sr : Double; findex : TDeeserFilterEnum): Double;
  var afr: Double;
  begin
    case findex of
      dsHPF: afr := FHighPassFilter.GetAFR(f, sr);
      dsLPF: afr := FLowPassFilter.GetAFR(f, sr);
      dsPK: afr := FPeakFilter.GetAFR(f, sr);
    end;
    Result := 20 * log10(afr);
  end;

  procedure TDeesserUnit.SetMode(Value : TDeesserMode);
  begin
    if(FMode <> Value) then
    begin
      FMode := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetLaxity(Value : Double);
  begin
    if(FLaxity <> Value) then
    begin
      if(Value < 1.0) then Value := 1.0;
      if(Value > 100.0) then Value := 100.0;
      FLaxity := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetSplitFreq(Value : Double);
  begin
    if(FSplitFreq <> Value) then
    begin
      if(Value < 10.0) then Value := 10.0;
      if(Value > 18000.0) then Value := 18000.0;
      FSplitFreq := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetSplitLevel(Value : Double);
  begin
    if(FSplitLevel <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;
      if(Value > 36.0) then Value := 36.0;
      FSplitLevel := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetPeakFreq(Value : Double);
  begin
    if(FPeakFreq <> Value) then
    begin
      if(Value < 10.0) then Value := 10.0;
      if(Value > 18000.0) then Value := 18000.0;
      FPeakFreq := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetPeakLevel(Value : Double);
  begin
    if(FPeakLevel <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;
      if(Value > 36.0) then Value := 36.0;
      FPeakLevel := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetPeakQ(Value : Double);
  begin
    if(FPeakQ <> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 100.0) then Value := 100.0;
      FPeakQ := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.SetSClisten(Value : Boolean);
  begin
    if(FSClisten <> Value) then
    begin
      FSClisten := Value;
      Changed();
    end;
  end;

  procedure TDeesserUnit.Changed;
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
  end;


{$WARNINGS ON}

end.
