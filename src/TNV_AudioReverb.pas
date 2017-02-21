
(* $Id: TNV_AudioReverb.pas (C)DJ VK 2016 $ *)

unit TNV_AudioReverb;

interface

{$WARNINGS OFF}

uses
  Classes, {SysUtils,} TNV_AudioClasses, TNV_AudioFilters,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Math;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TRoomSize = (room_small, room_medium, room_large, room_tunnel, room_smooth, room_experimental);

  TReverbUnit = class(TProcUnit)
  private
    FPreDelay : Double;
    FDecay : Double;           //time
    FHighFreqDamp : Double;    //cutoff
    FDiffusion : Double;
    FDryAmount : Double;       //par_dry,
    FWetAmount : Double;       //par_amount,
    FLowFreqCut : Double;      //par_treblecut,
    FHighFreqCut : Double;     //par_basscut,
    FRoomSize : TRoomSize; //type

    FLowPassFilter : TOnePoleFilter;     //left_lo, right_lo,
    FHighPassFilter : TOnePoleFilter;    //left_hi, right_hi;
    FLPFilter : TOnePoleFilter;     // lp_left, lp_right;
    FPreDelayAmount : Integer;    //int predelay_amt;
    FTimes : array [0..1, 0..5] of Integer; //int tl[6], tr[6];
    FDec : array [0..1, 0..5] of Double;    //float ldec[6], rdec[6];
    FB : Double;
    FOld : array [0..1] of Double; // float old_left, old_right;
    FDelayBufferPos : array [0..1, 0..5] of Integer;
    FPreDelayBufferPos : array [0..1] of Integer;
    //FDelayBuffer : array [0..1, 0..5, 0..2047] of Double; //simple_delay<2048, float> apL1 ... apR6;
    //FPreDelayBuffer : array [0..1, 0..131071] of Double; //simple_delay<131072, dsp::stereo_sample<float> > pre_delay;
    FDelayBuffer : array [0..1, 0..5] of array of Double; //simple_delay<2048, float> apL1 ... apR6;
    FPreDelayBuffer : array [0..1] of array of Double;
    FPhase :  Cardinal; //fixed_point<unsigned int, 25> phase, dphase;   << 25 bit uint
    FDPhase : Cardinal;
    FSineTable : array [0..128] of Integer;  // sine_table<int, 128, 10000> sine;
    //dsp::gain_smoothing amount, dryamount;

    procedure SetPreDelay(Value : Double);
    procedure SetDecay(Value : Double);
    procedure SetHighFreqDamp(Value : Double);
    procedure SetDiffusion(Value : Double);
    procedure SetDryAmount(Value : Double);
    procedure SetWetAmount(Value : Double);
    procedure SetLowFreqCut(Value : Double);
    procedure SetHighFreqCut(Value : Double);
    procedure SetRoomSize(Value : TRoomSize);
   // procedure OnFilterChanged(Sender: TObject);
    procedure CombAllPassFilter(var proc : Double; ddelay, cindex, i : Integer);
  protected
    procedure Changed;
    
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property PreDelay : Double read FPreDelay write SetPreDelay;
    property Decay : Double read FDecay write SetDecay;
    property HighFreqDamp : Double read FHighFreqDamp write SetHighFreqDamp;
    property Diffusion : Double read FDiffusion write SetDiffusion;
    property DryAmount : Double read FDryAmount write SetDryAmount;
    property WetAmount : Double read FWetAmount write SetWetAmount;
    property LowFreqCut : Double read FLowFreqCut write SetLowFreqCut;
    property HighFreqCut : Double read FHighFreqCut write SetHighFreqCut;
    property RoomSize : TRoomSize read FRoomSize write SetRoomSize;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TReverbUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
  end;

//////////////////////////////////

implementation

  constructor TReverbUnit.Create(AOwner: TComponent);
  var i : Integer;
  begin
    inherited;
    FPreDelay := 0.0;
    FDecay := 1.5;
    FHighFreqDamp := 5000;
    FDiffusion := 0.5;
    FDryAmount := 1.0;
    FWetAmount := 0.25;
    FLowFreqCut := 300;
    FHighFreqCut := 5000;
    FRoomSize := room_large;

    FLowPassFilter := TOnePoleFilter.Create(AOwner);
    FHighPassFilter := TOnePoleFilter.Create(AOwner);
    FLPFilter := TOnePoleFilter.Create(AOwner);

    for i := 0 to 128 do FSineTable[i] := Trunc(10000.0  * sin(2.0 * i * Pi / 128));

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TReverbUnit.Destroy;
  var i, j : Integer;
  begin
    for i := 0 to 1 do
    begin
      for j := 0 to 5 do
      begin
        SetLength(FDelayBuffer[i, j], 0);
      end;
      SetLength(FPreDelayBuffer[i], 0);
    end;
    FLowPassFilter.Free;
    FHighPassFilter.Free;
    FLPFilter.Free;
    inherited;
  end;

  procedure TReverbUnit.Assign(Source: TReverbUnit);
  var i, j, k : Integer;
  begin
    inherited Assign(Source as TProcUnit);
    FPreDelay := Source.FPreDelay;
    FDecay := Source.FDecay;
    FHighFreqDamp := Source.FHighFreqDamp;
    FDiffusion := Source.FDiffusion;
    FDryAmount := Source.FDryAmount;
    FWetAmount := Source.FWetAmount;
    FLowFreqCut := Source.FLowFreqCut;
    FHighFreqCut := Source.FHighFreqCut;
    FRoomSize := Source.FRoomSize;

    FLowPassFilter.Assign(Source.FLowPassFilter);
    FHighPassFilter.Assign(Source.FHighPassFilter);
    FLPFilter.Assign(Source.FLPFilter);
    FPreDelayAmount := Source.FPreDelayAmount;
    FB := Source.FB;
    FPhase :=  Source.FPhase;
    FDPhase := Source.FDPhase;

    for i := 0 to 1 do
    begin
      for j := 0 to 5 do
      begin
        FTimes[i, j] := Source.FTimes[i, j];
        FDec[i, j] := Source.FDec[i, j];
        FDelayBufferPos[i, j] := Source.FDelayBufferPos[i, j];
        for k := 0 to 2047 do FDelayBuffer[i, j, k] := Source.FDelayBuffer[i, j, k];
      end;
      for j := 0 to 131071 do FPreDelayBuffer[i, j] := Source.FPreDelayBuffer[i, j];
      FPreDelayBufferPos[i] := Source.FPreDelayBufferPos[i];
      FOld[i] := Source.FOld[i];
    end;
  end;

  procedure TReverbUnit.InitUnit(sr, ch: Integer);
  var
    FC, FD : Double;
    i, j : Integer;
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    FB := 1.0 - 0.3 / (FDecay * FSampleRate / 44100.0); // fb = pow(1.0f/4096.0f, (float)(1700/(time*sr)));
    FLPFilter.Init(FHighFreqDamp, FSampleRate, OnePole_LPF);
    FPhase := 0;
    FDPhase := (1 shl 25);
    FDPhase := Trunc(FDPhase * 0.5 * 128.0 / sr);
    case FRoomSize of     //update_times();
      room_small:
        begin
          FTimes[0, 0] :=  397 shl 16;
          FTimes[1, 0] :=  383 shl 16;
          FTimes[0, 1] :=  457 shl 16;
          FTimes[1, 1] :=  429 shl 16;
          FTimes[0, 2] :=  549 shl 16;
          FTimes[1, 2] :=  631 shl 16;
          FTimes[0, 3] :=  649 shl 16;
          FTimes[1, 3] :=  756 shl 16;
          FTimes[0, 4] :=  773 shl 16;
          FTimes[1, 4] :=  803 shl 16;
          FTimes[0, 5] :=  877 shl 16;
          FTimes[1, 5] :=  901 shl 16;
        end;
      room_medium:
        begin
          FTimes[0, 0] :=  697 shl 16;
          FTimes[1, 0] :=  783 shl 16;
          FTimes[0, 1] :=  957 shl 16;
          FTimes[1, 1] :=  929 shl 16;
          FTimes[0, 2] :=  649 shl 16;
          FTimes[1, 2] :=  531 shl 16;
          FTimes[0, 3] := 1049 shl 16;
          FTimes[1, 3] := 1177 shl 16;
          FTimes[0, 4] :=  473 shl 16;
          FTimes[1, 4] :=  501 shl 16;
          FTimes[0, 5] :=  587 shl 16;
          FTimes[1, 5] :=  681 shl 16;
        end;
      room_large:
        begin
          FTimes[0, 0] :=  697 shl 16;
          FTimes[1, 0] :=  783 shl 16;
          FTimes[0, 1] :=  957 shl 16;
          FTimes[1, 1] :=  929 shl 16;
          FTimes[0, 2] :=  649 shl 16;
          FTimes[1, 2] :=  531 shl 16;
          FTimes[0, 3] := 1249 shl 16;
          FTimes[1, 3] := 1377 shl 16;
          FTimes[0, 4] := 1573 shl 16;
          FTimes[1, 4] := 1671 shl 16;
          FTimes[0, 5] := 1877 shl 16;
          FTimes[1, 5] := 1781 shl 16;
        end;
      room_tunnel:
        begin
          FTimes[0, 0] := 1097 shl 16;
          FTimes[1, 0] := 1087 shl 16;
          FTimes[0, 1] := 1057 shl 16;
          FTimes[1, 1] := 1031 shl 16;
          FTimes[0, 2] := 1049 shl 16;
          FTimes[1, 2] := 1039 shl 16;
          FTimes[0, 3] := 1083 shl 16;
          FTimes[1, 3] := 1055 shl 16;
          FTimes[0, 4] := 1075 shl 16;
          FTimes[1, 4] := 1099 shl 16;
          FTimes[0, 5] := 1003 shl 16;
          FTimes[1, 5] := 1073 shl 16;
        end;
      room_smooth:
        begin
          FTimes[0, 0] :=  197 shl 16;
          FTimes[1, 0] :=  133 shl 16;
          FTimes[0, 1] :=  357 shl 16;
          FTimes[1, 1] :=  229 shl 16;
          FTimes[0, 2] :=  549 shl 16;
          FTimes[1, 2] :=  431 shl 16;
          FTimes[0, 3] :=  949 shl 16;
          FTimes[1, 3] := 1277 shl 16;
          FTimes[0, 4] := 1173 shl 16;
          FTimes[1, 4] := 1671 shl 16;
          FTimes[0, 5] := 1477 shl 16;
          FTimes[1, 5] := 1881 shl 16;
        end;
      room_experimental:
        begin
          FTimes[0, 0] :=  197 shl 16;
          FTimes[1, 0] :=  133 shl 16;
          FTimes[0, 1] :=  257 shl 16;
          FTimes[1, 1] :=  179 shl 16;
          FTimes[0, 2] :=  549 shl 16;
          FTimes[1, 2] :=  431 shl 16;
          FTimes[0, 3] :=  619 shl 16;
          FTimes[1, 3] :=  497 shl 16;
          FTimes[0, 4] := 1173 shl 16;
          FTimes[1, 4] := 1371 shl 16;
          FTimes[0, 5] := 1577 shl 16;
          FTimes[1, 5] := 1881 shl 16;
        end;
    end;
    FD := 1000.0 + 2400.0 * FDiffusion;
    for i := 0 to 1 do
      for j := 0 to 5 do
      FDec[i, j] := exp(-1.0 * (FTimes[i, j] shr 16) / FD);
    //amount.set_inertia(*params[par_amount]);
    //dryamount.set_inertia(*params[par_dry]);
    FC := max(20.0, FLowFreqCut);
    FC := min(FSampleRate * 0.49, FC);
    FHighPassFilter.Init(fc, FSampleRate, OnePole_LPF);
    FC := max(20.0, FHighFreqCut);
    FC := min(FSampleRate * 0.49, FC);
    FLowPassFilter.Init(fc, FSampleRate, OnePole_LPF);
    FPreDelayAmount := Trunc(FSampleRate * FPreDelay * (1.0 / 1000.0) + 1.0);
    for i := 0 to 1 do
    begin
      for j := 0 to 5 do
      begin
        SetLength(FDelayBuffer[i, j], 2048);
      end;
      SetLength(FPreDelayBuffer[i], 131072);
    end;
    ResetUnitBuffer();
  end;

  procedure TReverbUnit.ResetUnitBuffer();
  var i, j, k : Integer;
  begin
    for i := 0 to 1 do
    begin
      for j := 0 to 5 do
      begin
        FTimes[i, j] := 0;
        FDec[i, j] := 0.0;
        FDelayBufferPos[i, j] := 0;
        for k := 0 to 2047 do FDelayBuffer[i, j, k] := 0.0;
      end;
      for j := 0 to 131071 do FPreDelayBuffer[i, j] := 0.0;
      FPreDelayBufferPos[i] := 0;
      FOld[i] := 0.0;
    end;

    FLPFilter.ResetBuffer();
  end;

  procedure TReverbUnit.ResetUnit();
  var i, j : Integer;
  begin
    for i := 0 to 1 do
    begin
      for j := 0 to 5 do
      begin
        SetLength(FDelayBuffer[i, j], 0);
      end;
      SetLength(FPreDelayBuffer[i], 0);
    end;
  end;

  procedure TReverbUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, proc, outp: array [0..1] of Double;
    j, lfo, ppos, fbits : Integer;
    rout : Double;
    ipart, fpart : Cardinal;
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
      proc[j] := inp[j];

      //pre_delay.process(proc[j], FPreDelayAmount); inline T process(T idata, int delay)
      if (FPreDelayAmount >= 0) and (FPreDelayAmount < 131072) then  //assert( >= 0 && <= N-1);
      begin
        ppos := FPreDelayBufferPos[j] + 131072 - FPreDelayAmount;
        if (ppos >= 131072) then ppos := ppos - 131072;
        proc[j] := FPreDelayBuffer[j, ppos];
        FPreDelayBuffer[j, FPreDelayBufferPos[j]] := proc[j];
        FPreDelayBufferPos[j] := FPreDelayBufferPos[j] + 1;
        if (FPreDelayBufferPos[j] >= 131072) then
          FPreDelayBufferPos[j] := FPreDelayBufferPos[j] - 131072;
      end
      else Exit;

      FHighPassFilter.ProcFilter(proc[j], j);
      FLowPassFilter.ProcFilter(proc[j], j);
      
      //reverb.process(proc[j]);
      ipart := FPhase shr 25;
      // the interpolated LFO might be an overkill here  // inline U lerp_by_fract_int(U v1, U v2)
      fbits := FPhase and ((1 shl 25) - 1);
      fpart := fbits shr 11;  //shift = abs(14-25) = 11     //fpart = fpart<14>();
      if (fpart >= 0) and (fpart <= (1 shl 14)) then
      begin
        lfo := FSineTable[ipart + 1] - FSineTable[ipart];
        lfo := FSineTable[ipart] + Trunc(1.0 * lfo * fpart / (1 shl 14));
      end
      else lfo := 0;
      //else Exit;
      lfo := lfo shr 2;
      FPhase := FPhase + FDPhase;
      proc[j] := proc[j] + FOld[1 - j];  
      CombAllPassFilter(proc[j], -45 * lfo, j, 0);  //process_allpass_comb_lerp16
      CombAllPassFilter(proc[j], 47 * lfo, j, 1);
      rout := proc[j];
      CombAllPassFilter(proc[j], 54 * lfo, j, 2);
      CombAllPassFilter(proc[j], - 69 * lfo, j, 3);
      CombAllPassFilter(proc[j], 69 * lfo, j, 4);
      CombAllPassFilter(proc[j], - 46 * lfo, j, 5);
      proc[j] := proc[j] * FB;
      FLPFilter.ProcFilter(proc[j], j);
      FOld[j]:= proc[j];
      //sanitize(FOld[j]);
      proc[j] := rout;

      outp[j] := inp[j] * FDryAmount + proc[j] * FWetAmount;  //dry = dryamount.get();
      outp[j] := outp[j] * FOutputGain;         //wet = amount.get();
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  procedure TReverbUnit.CombAllPassFilter(var proc : Double; ddelay, cindex, i : Integer);
  var                                        //proc = T in
    ffb, Fract16, old, cur: Double;
    ppos, pppos : Integer;
    delay, idelay : Integer;    //Cardinal (unsigned)
  begin
    delay := FTimes[cindex, i] + ddelay;
    ffb := FDec[cindex, i];
    Fract16 := (delay and $FFFF) * (1.0 / 65536.0);
    idelay := delay shr 16;

    //inline void get_interp(U &odata, int idelay, float udelay)
    if (idelay < 0) or (idelay >= 2048) then Exit; //  assert(idelay >= 0 && idelay <= N-1);
    ppos := FDelayBufferPos[cindex, i] + 2048 - idelay;
    if (ppos >= 2048) then ppos := ppos - 2048;
    pppos := (ppos + 2048 - 1);
    if (pppos >= 2048) then pppos := pppos - 2048;

    //inline T lerp(T v1, T v2, U mix)  linear interpolation
    old := FDelayBuffer[cindex, i, ppos] + (FDelayBuffer[cindex, i, pppos]
          - FDelayBuffer[cindex, i, ppos]) * Fract16;

    cur := proc + ffb * old;
    //sanitize(cur);

    //inline void put(T idata)
    FDelayBuffer[cindex, i, FDelayBufferPos[cindex, i]] := cur;
    FDelayBufferPos[cindex, i] := FDelayBufferPos[cindex, i] + 1;
    if (FDelayBufferPos[cindex, i] >= 2048) then
      FDelayBufferPos[cindex, i] := FDelayBufferPos[cindex, i] - 2048;

    proc := old - fb * cur;
  end;

  procedure TReverbUnit.SetPreDelay(Value : Double);
  begin
    if(FPreDelay <> Value) then
    begin
      if(Value < 0) then Value := 0;
      if(Value > 500) then Value := 500;
      FPreDelay := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetDecay(Value : Double);
  begin
    if(FDecay <> Value) then
    begin
      if(Value < 0.4) then Value := 0.4;
      if(Value > 15) then Value := 15;
      FDecay := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetHighFreqDamp(Value : Double);
  begin
    if(FHighFreqDamp <> Value) then
    begin
      if(Value < 20) then Value := 20;
      if(Value > 20000) then Value := 20000;
      FHighFreqDamp := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetDiffusion(Value : Double);
  begin
    if(FDiffusion <> Value) then
    begin
      if(Value < 0) then Value := 0;
      if(Value > 1.0) then Value := 1.0;
      FDiffusion := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetDryAmount(Value : Double);
  begin
    if(FDryAmount <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FDryAmount := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetWetAmount(Value : Double);
  begin
    if(FWetAmount <> Value) then
    begin
      if(Value < 0.015625) then Value := 0.015625;
      if(Value > 64) then Value := 64;
      FWetAmount := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetLowFreqCut(Value : Double);
  begin
    if(FLowFreqCut <> Value) then
    begin
      if(Value < 20) then Value := 20;
      if(Value > 20000) then Value := 20000;
      FLowFreqCut := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetHighFreqCut(Value : Double);
  begin
    if(FHighFreqCut <> Value) then
    begin
      if(Value < 20) then Value := 20;
      if(Value > 20000) then Value := 20000;
      FHighFreqCut := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.SetRoomSize(Value : TRoomSize);
  begin
    if(FRoomSize <> Value) then
    begin
      FRoomSize := Value;
      Changed();
    end;
  end;

  procedure TReverbUnit.Changed();
  begin
    //if(Busy) then ReInitFlag := true
    //else
    InitUnit(FSampleRate, FChannelsCnt);
  end;

{$WARNINGS ON}

end.
