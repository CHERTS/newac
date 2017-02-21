
(* $Id: TNV_AudioFilters.pas (C)DJ VK 2016 $ *)

unit TNV_AudioFilters;

interface

{$WARNINGS OFF}

uses
  Classes, Math;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TBiquadFilterType = (Biquad_LPF, Biquad_HPF, Biquad_LSF, Biquad_HSF, Biquad_PKF,Biquad_BPF, Biquad_BSF, Biquad_APF, Biquad_BPF_CS, Biquad_BPF_CZP);
  TOnePoleFilterType = (OnePole_LPF, OnePole_HPF, OnePole_APF);
  TQParamType = (QisQ, QisBW, QisS);
  TPerOctaveReduction = (red12db, red24db, red36db, red48db);
  TEQBand = (Band_2Octave, Band_Octave, Band_HalfOctave, Band_ThirdOctave);
  TBandFilterType = (butterworth, chebyshev_type1, chebyshev_type2, butterworth_no_boost, chebyshev_type1_no_boost);

  TBiquadFilter = class(TPersistent)  // direct form II biquad
  private
    a : array[0..2] of Double;
    b : array[0..2] of Double;
    w : array[0..1, 0..1] of Double;  // memory for 2 previous states
  protected
    InBand : Boolean;
    FOnChange: TNotifyEvent;
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TBiquadFilter);
    procedure Init(fc, q, sr, gain : Double; ftype : TBiquadFilterType; qtype : TQParamType);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; cindex : Integer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetAFR(f, sr : Double): Double;
  end;

  TOnePoleFilter = class(TPersistent)  // direct form II biquad
  private
    a : array[0..1] of Double;
    b : array[0..1] of Double;
    Sinp : array[0..1] of Double;  // memory for 1 previous states
    Soutp : array[0..1] of Double;  // memory for 1 previous states
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TOnePoleFilter);
    procedure Init(fc, sr : Double; ftype : TOnePoleFilterType);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; cindex : Integer);
  end;

  TFilterClass = class(TPersistent)
  private
    procedure OnBiquadChanged(Sender: TObject);
  protected
    FOnChange: TNotifyEvent;
    procedure Changed;
  public
    function GetAFR(freq, sr : Double): Double; virtual; abstract;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TLPHPFilter = class(TFilterClass)
  private
    FActive : Boolean;
    FCutoffFreq : Double;
    FQFactor : Double;
    FBandWidth : Double;
    FGain : Double;
    FPerOctaveReduction : TPerOctaveReduction;
    FFilter : array [0..3] of TBiquadFilter;
    procedure SetActive(Value : Boolean);
    procedure SetPerOctaveReduction(Value : TPerOctaveReduction);
    procedure SetCutoffFreq(Value : Double);
    procedure SetQFactor(Value : Double);
    procedure SetBandWidth(Value : Double);
    procedure SetGain(Value : Double);
  published
    property PerOctaveReduction : TPerOctaveReduction read FPerOctaveReduction write SetPerOctaveReduction default red24db;
    property Active : Boolean read FActive write SetActive default false;
    property CutoffFreq : Double read FCutoffFreq write SetCutoffFreq;
    property QFactor : Double read FQFactor write SetQFactor;
    property BandWidth : Double read FBandWidth write SetBandWidth;
    property Gain : Double read FGain write SetGain;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
    procedure Assign(Source: TLPHPFilter);
    procedure Init(LowPass : Boolean; sr : Double);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; findex, cindex : Integer);
    function GetAFR(freq, sr : Double): Double; override;
  end;

  TLSHSFilter = class(TFilterClass)
  private
    FActive : Boolean;
    FCenterFreq : Double;
    FQFactor : Double;
    FBandWidth : Double;
    FSlope : Double;
    FGain : Double;
    FFilter : TBiquadFilter;
    procedure SetActive(Value : Boolean);
    procedure SetCenterFreq(Value : Double);
    procedure SetQFactor(Value : Double);
    procedure SetBandWidth(Value : Double);
    procedure SetSlope(Value : Double);
    procedure SetGain(Value : Double);
  published
    property Active : Boolean read FActive write SetActive default false;
    property CenterFreq : Double read FCenterFreq write SetCenterFreq;
    property QFactor : Double read FQFactor write SetQFactor;
    property BandWidth : Double read FBandWidth write SetBandWidth;
    property Slope : Double read FSlope write SetSlope;
    property Gain : Double read FGain write SetGain;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
    procedure Assign(Source: TLSHSFilter);
    procedure Init(LowShelf : Boolean; sr : Double);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; cindex : Integer);
    function GetAFR(freq, sr : Double): Double; override;
  end;

  TPKFilter = class(TFilterClass)
  private
    FActive : Boolean;
    FCenterFreq : Double;
    FQFactor : Double;
    FBandWidth : Double;
    FGain : Double;
    FFilter : TBiquadFilter;
    procedure SetActive(Value : Boolean);
    procedure SetCenterFreq(Value : Double);
    procedure SetQFactor(Value : Double);
    procedure SetBandWidth(Value : Double);
    procedure SetGain(Value : Double);
  published
    property Active : Boolean read FActive write SetActive default false;
    property CenterFreq : Double read FCenterFreq write SetCenterFreq;
    property QFactor : Double read FQFactor write SetQFactor;
    property BandWidth : Double read FBandWidth write SetBandWidth;
    property Gain : Double read FGain write SetGain;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
    procedure Assign(Source: TPKFilter);
    procedure Init(sr : Double);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; cindex : Integer);
    function GetAFR(freq, sr : Double): Double; override;
  end;

  TBandFilterSection = record
    Sa : array[0..4] of Double;
    Sb : array[0..4] of Double;
	  Sinp, Soutp : array[0..1, 0..3] of Double;
  end;

  TBandPassFilter = class(TPersistent)
  private
    FActive : Boolean;
    FCenterFreq : Double;
    FLowFreq : Double;
    FHighFreq : Double;
	  FGain : Double;
    FSections : array [0..1] of TBandFilterSection;
    FSectionsCnt : Integer;
    
    procedure SetCenterFreq(Value : Double);
    procedure SetLowFreq(Value : Double);
    procedure SetHighFreq(Value : Double);
    procedure SetGain(Value : Double);
  protected

    FOnChange: TNotifyEvent;
    procedure Changed;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
    procedure Assign(Source: TBandPassFilter);
    procedure Init(sr : Double; ftype : TBandFilterType);
    procedure ResetBuffer();
    procedure ProcFilter(var proc : Double; cindex : Integer);
    function GetAFR(f, sr : Double): Double;
    property CenterFreq : Double read FCenterFreq write SetCenterFreq;
    property LowFreq : Double read FLowFreq write SetLowFreq;
    property HighFreq : Double read FHighFreq write SetHighFreq;
    property Gain : Double read FGain write SetGain;
	  property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  //////////////////////////////////////

  implementation

  //////////////////////////////////////

  constructor TBiquadFilter.Create(AOwner: TComponent);
  var i : Integer;
  begin
    for i := 0 to 2 do
    begin
      a[i] := 0.0;
      b[i] := 0.0;
    end;
    ResetBuffer();
    InBand := False;
  end;

  destructor TBiquadFilter.Destroy;
  begin

  end;

  procedure TBiquadFilter.Assign(Source: TBiquadFilter);
  var i, j : Integer;
  begin
    for i := 0 to 2 do
    begin
      a[i] := Source.a[i];
      b[i] := Source.b[i];
    end;
    for i := 0 to 1 do
      for j := 0 to 1 do
        w[i, j] := Source.w[i, j];
    InBand := Source.InBand;
  end;

  procedure TBiquadFilter.Init(fc, q, sr, gain : Double; ftype : TBiquadFilterType; qtype : TQParamType);
  var
    GA, omega, sn, cs, alpha, beta, inv, invA: Double;
    FNaik :Double;
  begin
    InBand := false;
    FNaik := sr / 2.0;
    if fc >= FNaik then
    begin
      Exit;
    end;
    GA := power(10.0, gain / 40.0);
    omega := fc * 2.0 * Pi / sr;
    sn := sin(omega);
    cs := cos(omega);
    case QType of
      QisQ:
      begin
        alpha := sn / (2.0 * q);
        beta := sqrt(GA) / q;
      end;
      QisBW:
      begin
        if sn <> 0.0 then
        begin
          q := 0.5 / sinh(ln(2.0) / 2.0 * q * omega / sn);
          alpha := sn * sinh(ln(2.0) / 2.0 * q * omega / sn);
          beta := sqrt(GA) * 2.0 * sinh(ln(2.0) / 2.0 * q * omega / sn);
        end;
      end;
      QisS:
      begin
        beta := sqrt((GA * GA + 1.0)/q - (GA - 1.0) * (GA - 1.0));
      end;
    end;
    inv := 1.0 / (1.0 + alpha);
    invA := 1.0 / (1.0 + alpha / GA);
    case ftype of
      Biquad_LPF:
        begin
          a[0] := (1.0 - cs) * 0.5 * inv * GA * GA;
          a[1] := a[0] + a[0];
          a[2] := a[0];
          b[0] := 1.0;
          b[1] := -2.0 * cs * inv;
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_HPF:
        begin
          a[0] := (1.0 + cs) * 0.5 * inv * GA * GA;
          a[1] := - a[0] - a[0];
          a[2] := a[0];
          b[0] := 1.0;
          b[1] := -2.0 * cs * inv;
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_LSF:
        begin
          invA := 1.0 / ((GA + 1.0) + (GA - 1.0) * cs + beta * sn);
          a[0] := GA * ((GA+1) - (GA - 1.0)* cs + beta * sn) * invA;
          a[1] := 2.0 * GA * ((GA - 1.0) - (GA + 1.0) * cs) * invA;
          a[2] := GA * ((GA + 1.0) - (GA - 1.0)* cs - beta * sn) * invA;
          b[0] := 1.0;
          b[1] := -2.0 * ((GA - 1.0) + (GA + 1.0) * cs) * invA;
          b[2] := ((GA + 1.0) + (GA - 1.0) * cs - beta * sn) * invA;
        end;
      Biquad_HSF:
        begin
          invA := 1.0 / ((GA + 1.0) - (GA - 1.0) * cs + beta * sn);
          a[0] := GA * ((GA+1) + (GA - 1.0)* cs + beta * sn) * invA;
          a[1] := - 2.0 * GA * ((GA - 1.0) + (GA + 1.0) * cs) * invA;
          a[2] := GA * ((GA + 1.0) + (GA - 1.0)* cs - beta * sn) * invA;
          b[0] := 1.0;
          b[1] := 2.0 * ((GA - 1.0) - (GA + 1.0) * cs) * invA;
          b[2] := ((GA + 1.0) - (GA - 1.0) * cs - beta * sn) * invA;
        end;
      Biquad_PKF: // (invA -> inv) by DJ VK
        begin
          a[0] := (1.0 + alpha * GA) * invA;
          a[1] := -2.0 * cs * invA;
          a[2] := (1.0 - alpha * GA) * invA;
          b[0] := 1.0;
          b[1] := -2.0 * cs * invA;
          b[2] := (1.0 - alpha / GA) * invA;
        end;
      Biquad_BPF:
        begin
          a[0] := alpha * inv;
          a[1] := 0.0;
          a[2] := - a[0];
          b[0] := 1.0;
          b[1] := (-2.0 * cs * inv);
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_BSF:
        begin
          a[0] := inv;
          a[1] := -2.0 * cs * inv;
          a[2] := a[0];
          b[0] := 1.0;
          b[1] := (-2.0 * cs * inv);
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_APF:
        begin
          a[0] := (1.0 - alpha) * inv;
          a[1] := -2.0 * cs * inv;
          a[2] := 1.0;
          b[0] := 1.0;
          b[1] := -2.0 * cs * inv;
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_BPF_CS:
        begin
          a[0] := q * alpha * inv;
          a[1] := 0.0;
          a[2] := a[0];
          b[0] := 1.0;
          b[1] := -2.0 * cs * inv;
          b[2] := (1.0 - alpha) * inv;
        end;
      Biquad_BPF_CZP:
        begin
          a[0] := alpha * inv;
          a[1] := 0.0;
          a[2] := - a[0];
          b[0] := 1.0;
          b[1] := -2.0 * cs * inv;
          b[2] := (1.0 - alpha) * inv;
        end;
    end;
    InBand := true;
    ResetBuffer();
  end;

  procedure TBiquadFilter.ResetBuffer();
  var i, j : Integer;
  begin
    for i := 0 to 1 do
      for j := 0 to 1 do
        w[i, j] := 0.0;
  end;

  procedure TBiquadFilter.ProcFilter(var proc : Double; cindex : Integer);
  var
    InSamp, tmp, OutSamp : Double;
  begin
    if not InBand then Exit
    else
    begin
      InSamp := proc;
      //    dsp::sanitize_denormal(InSamp);
      //    dsp::sanitize(InSamp);
      //    dsp::sanitize(W[cindex, 1]);
      //    dsp::sanitize(W[cindex, 1]);
      tmp := InSamp - w[cindex, 0] * B[1] - w[cindex, 1] * B[2];
      OutSamp := tmp * A[0] + w[cindex, 0] * A[1] + w[cindex, 1] * A[2];
      w[cindex, 1] := w[cindex, 0];
      w[cindex, 0] := tmp;
      proc := OutSamp;
    end;
  end;

  function TBiquadFilter.GetAFR(f, sr : Double): Double;
  var
    xr, xi, zr, zi, yr, yi, omega, v :Double;
  begin
    omega := (f * 2.0* Pi) / sr;
    zr :=  cos(omega);
    zi :=  -sin(omega);
    xr := (a[0] + a[1] * zr + a[2] * (zr * zr - zi * zi));
    xi := (a[1] * zi + a[2] * (zr * zi + zi * zr));
    yr := (1.0  + b[1] * zr + b[2] * (zr * zr - zi * zi));
    yi := (b[1] * zi + b[2] * (zr * zi + zi * zr));
    v := (yr * yr + yi * yi);
    zr := (xr * yr + xi * yi) / v;
    zi := (xi * yr - xr * yi) / v;
    Result := sqrt(zr * zr + zi * zi);
  end;

  procedure TBiquadFilter.Changed;
  begin
    if Assigned(FOnChange) then
    FOnChange(Self);
  end;

  ///////////////////////////////////

  constructor TOnePoleFilter.Create(AOwner: TComponent);
  var i :Integer;
  begin
    for i := 0 to 1 do
    begin
      a[i] := 0.0;
      b[i] := 0.0;
    end;
    ResetBuffer();
  end;

  destructor TOnePoleFilter.Destroy;
  begin

  end;

  procedure TOnePoleFilter.Assign(Source: TOnePoleFilter);
  var i : Integer;
  begin
    for i := 0 to 1 do
    begin
      a[i] := Source.a[i];
      b[i] := Source.b[i];
      Sinp[i] := Source.Sinp[i];
      Soutp[i] := Source.Soutp[i];
    end;
    
  end;

  procedure TOnePoleFilter.Init(fc, sr : Double; ftype : TOnepoleFilterType);
  var
    omega, x, q: Double;
    //FNaik :Double;
  begin
    //FNaik := sr / 2.0;
    //if fc >= FNaik then
    //begin
    //  Exit;
    //end;
    omega := fc * Pi / (sr * 2.0);
    x := tan(omega);
    q := 1.0 / (1.0 + x);
    case ftype of
      OnePole_LPF:
        begin
          a[0] := x * q;
          a[1] := x * q;
          b[0] := 1.0;
          b[1] := (x - 1) * q;
        end;
      OnePole_HPF:
        begin
          a[0] := q;
          a[1] := -a[0];
          b[0] := 1.0;
          b[1] := (x - 1) * q;
        end;
      OnePole_APF:
        begin
          a[0] := (x - 1) * q;
          a[1] := 1.0;
          b[0] := 1.0;
          b[1] := (x - 1) * q;
        end;
    end;
    ResetBuffer();
  end;

  procedure TOnePoleFilter.ResetBuffer();
  var i :Integer;
  begin
    for i := 0 to 1 do
    begin
      Sinp[i] := 0.0;
      Soutp[i] := 0.0;
    end;
  end;

  procedure TOnePoleFilter.ProcFilter(var proc : Double; cindex : Integer);
  var
    InSamp, OutSamp : Double;
  begin
    //if not InBand then Exit
    //else
    begin
      InSamp := proc;
      //    dsp::sanitize_denormal(InSamp);
      //    dsp::sanitize(InSamp);
      //    dsp::sanitize(W[cindex, 1]);
      //    dsp::sanitize(W[cindex, 1]);
      OutSamp := InSamp * A[0] + Sinp[cindex] * A[1] - Soutp[cindex] * B[1];
      Sinp[cindex] := InSamp;
      Soutp[cindex] := OutSamp;
      proc := OutSamp;
    end;
  end;
  ///////////////////////////////////

  procedure TFilterClass.OnBiquadChanged(Sender: TObject);
  begin
    Changed();
  end;

  procedure TFilterClass.Changed;
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;

//////////////////////////////////

  constructor TLPHPFilter.Create(AOwner: TComponent);
  var i : Integer;
  begin
    FActive := false;
    FCutoffFreq := 1000.0;
    FQFactor := 1.0 / sqrt(2.0);
    FBandWidth := 0.0;
    FGain := 0.0;
    FPerOctaveReduction := red24db;
    for i := 0 to 3 do
    begin
      FFilter[i] := TBiquadFilter.Create(AOwner);
      FFilter[i].OnChange := OnBiquadChanged;
    end;
  end;

  destructor TLPHPFilter.Destroy;
  var i : Integer;
  begin
    for i := 0 to 3 do
    begin
      FFilter[i].Free;
    end;
  end;

  procedure TLPHPFilter.Assign(Source: TLPHPFilter);
  var i : Integer;
  begin
    FActive := Source.FActive;
    FCutoffFreq := Source.FCutoffFreq;
    FQFactor := Source.FQFactor;
    FBandWidth := Source.FBandWidth;
    FGain := Source.FGain;
    FPerOctaveReduction := Source.FPerOctaveReduction;
    for i := 0 to 3 do
    begin
      FFilter[i].Assign(Source.FFilter[i]);
    end;
  end;

  procedure TLPHPFilter.Init(LowPass : Boolean; sr : Double);
  var i : Integer;
  begin
    for i := 0 to 3 do
    if LowPass then
      FFilter[i].Init(FCutoffFreq, FQFactor, sr, FGain, Biquad_LPF, QisQ)
    else
      FFilter[i].Init(FCutoffFreq, FQFactor, sr, FGain, Biquad_HPF, QisQ);
  end;

  procedure TLPHPFilter.ResetBuffer();
  var i : Integer;
  begin
    for i := 0 to 3 do
      FFilter[i].ResetBuffer();
  end;

  procedure TLPHPFilter.ProcFilter(var proc : Double; findex, cindex : Integer);
  begin
    FFilter[findex].ProcFilter(proc, cindex);
  end;

  procedure TLPHPFilter.SetActive(Value : Boolean);
  begin
    if(FActive <> Value) then
    begin
      FActive := Value;
	    Changed();
    end;
  end;

  procedure TLPHPFilter.SetPerOctaveReduction(Value : TPerOctaveReduction);
  begin
    if(FPerOctaveReduction <> Value) then
    begin
      FPerOctaveReduction := Value;
	    Changed();
    end;
  end;

  procedure TLPHPFilter.SetCutoffFreq(Value : Double);
  begin
    if(FCutoffFreq <> Value) then
    begin
      if(Value < 10.0) then Value := 10.0;
      if(Value > 22000.0) then Value := 22000.0;
      FCutoffFreq := Value;
	    Changed();
    end;
  end;

  procedure TLPHPFilter.SetQFactor(Value : Double);
  begin
    if(FQFactor <> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 10.0) then Value := 10.0;
      FQFactor := Value;
	    Changed();
    end;
  end;

  procedure TLPHPFilter.SetBandWidth(Value : Double);
  begin
    if(FBandWidth <> Value) then
    begin
      //if(Value < 0.1) then Value := 0.1;
      //if(Value > 100000.0) then Value := 100000.0;
      FBandWidth := Value;
	    Changed();
    end;
  end;

  procedure TLPHPFilter.SetGain(Value : Double);
  begin
    if(FGain <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;
      if(Value > 36.0) then Value := 36.0;
      FGain := Value;
	    Changed();
    end;
  end;

  function TLPHPFilter.GetAFR(freq, sr : Double): Double;
  var afr : Double;
  begin
    afr := FFilter[0].GetAFR(freq, sr);
    case FPerOctaveReduction of
      red24db: afr := afr * afr;
      red36db: afr := afr * afr * afr;
      red48db: afr := afr * afr * afr * afr;
    end;
    Result := afr;
  end;

  /////////////////////////////////////////////////

  constructor TLSHSFilter.Create(AOwner: TComponent);
  begin
    FActive := false;
    FCenterFreq := 1000.0;
    FQFactor := 1.0 / sqrt(2.0);
    FBandWidth := 0.0;
    FSlope := 1.0;
    FGain := 0.0;
    FFilter := TBiquadFilter.Create(AOwner);
    FFilter.OnChange := OnBiquadChanged;
  end;

  destructor TLSHSFilter.Destroy;
  begin
    FFilter.Free;
  end;

  procedure TLSHSFilter.Assign(Source: TLSHSFilter);
  begin
    FActive := Source.FActive;
    FCenterFreq := Source.FCenterFreq;
    FQFactor := Source.FQFactor;
    FBandWidth := Source.FBandWidth;
    FSlope := Source.FSlope;
    FGain := Source.Gain;
    FFilter.Assign(Source.FFilter);
  end;

  procedure TLSHSFilter.Init(LowShelf : Boolean; sr : Double);
  begin
    if LowShelf then FFilter.Init(FCenterFreq, FQFactor, sr, FGain, Biquad_LSF, QisQ)
    else FFilter.Init(FCenterFreq, FQFactor, sr, FGain, Biquad_HSF, QisQ);
  end;

  procedure TLSHSFilter.ResetBuffer();
  begin
    FFilter.ResetBuffer();
  end;

  procedure TLSHSFilter.ProcFilter(var proc : Double; cindex : Integer);
  begin
    FFilter.ProcFilter(proc, cindex);
  end;

  procedure TLSHSFilter.SetActive(Value : Boolean);
  begin
    if(FActive <> Value) then
    begin
      FActive := Value;
	    Changed();
    end;
  end;

  procedure TLSHSFilter.SetCenterFreq(Value : Double);
  begin
    if(FCenterFreq <> Value) then
    begin
      if(Value < 10.0) then Value := 10.0;
      if(Value > 22000.0) then Value := 22000.0;
      FCenterFreq := Value;
	    Changed();
    end;
  end;

  procedure TLSHSFilter.SetQFactor(Value : Double);
  begin
    if(FQFactor <> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 10.0) then Value := 10.0;
      FQFactor := Value;
	    Changed();
    end;
  end;

  procedure TLSHSFilter.SetBandWidth(Value : Double);
  begin
    if(FBandWidth <> Value) then
    begin
      FBandWidth := Value;
	    Changed();
    end;
  end;

  procedure TLSHSFilter.SetSlope(Value : Double);
  begin
    if(FSlope <> Value) then
    begin
      FSlope := Value;
	    Changed();
    end;
  end;

  procedure TLSHSFilter.SetGain(Value : Double);
  begin
    if(FGain <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;
      if(Value > 36.0) then Value := 36.0;
      FGain := Value;
	    Changed();
    end;
  end;

  function TLSHSFilter.GetAFR(freq, sr : Double): Double;
  begin
    Result := FFilter.GetAFR(freq, sr);
  end;

  /////////////////////////////////////////////////

  constructor TPKFilter.Create(AOwner: TComponent);
  begin
    FActive := false;
    FCenterFreq := 1000.0;
    FQFactor := 1.0 / sqrt(2.0);
    FBandWidth := 0.0;
    FGain := 0.0;
    FFilter := TBiquadFilter.Create(AOwner);
    FFilter.OnChange := OnBiquadChanged;
  end;

  destructor TPKFilter.Destroy;
  begin
    FFilter.Free;
  end;

  procedure TPKFilter.Assign(Source: TPKFilter);
  begin
    FActive := Source.FActive;
    FCenterFreq := Source.FCenterFreq;
    FQFactor := Source.FQFactor;
    FBandWidth := Source.FBandWidth;
    FGain := Source.Gain;
    FFilter.Assign(Source.FFilter);
  end;

  procedure TPKFilter.Init(sr : Double);
  begin
    FFilter.Init(FCenterFreq, FQFactor, sr, FGain, Biquad_PKF, QisQ);
  end;

  procedure TPKFilter.ResetBuffer();
  begin
    FFilter.ResetBuffer();
  end;

  procedure TPKFilter.ProcFilter(var proc : Double; cindex : Integer);
  begin
    FFilter.ProcFilter(proc, cindex);
  end;

  procedure TPKFilter.SetActive(Value : Boolean);
  begin
    if(FActive <> Value) then
    begin
      FActive := Value;
	    Changed();
    end;
  end;

  procedure TPKFilter.SetCenterFreq(Value : Double);
  begin
    if(FCenterFreq <> Value) then
    begin
      if(Value < 10.0) then Value := 10.0;
      if(Value > 22000.0) then Value := 22000.0;
      FCenterFreq := Value;
	    Changed();
    end;
  end;

  procedure TPKFilter.SetQFactor(Value : Double);
  begin
    if(FQFactor <> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 10.0) then Value := 10.0;
      FQFactor := Value;
	    Changed();
    end;
  end;

  procedure TPKFilter.SetBandWidth(Value : Double);
  begin
    if(FBandWidth <> Value) then
    begin
      FBandWidth := Value;
	    Changed();
    end;
  end;

  procedure TPKFilter.SetGain(Value : Double);
  begin
    if(FGain <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;
      if(Value > 36.0) then Value := 36.0;
      FGain := Value;
	    Changed();
    end;
  end;

  function TPKFilter.GetAFR(freq, sr : Double): Double;
  begin
    Result := FFilter.GetAFR(freq, sr);
  end;

  ////////////////////////////////////

  constructor TBandPassFilter.Create(AOwner: TComponent);
  var i, j : Integer;
  begin
    FActive := false;
    FCenterFreq := 1000.0;
    FLowFreq := 500.0;
    FHighFreq := 2000.0;
	  FGain := 0.0;
    FSectionsCnt := 2;
    for i := 0 to FSectionsCnt - 1 do
      with FSections[i] do
    begin
      for j := 0 to 4 do
      begin
        Sa[j] := 0.0;
        Sb[j] := 0.0;
      end;
    end;
    ResetBuffer();
  end;

  destructor TBandPassFilter.Destroy;
  begin

  end;

  procedure TBandPassFilter.Assign(Source: TBandPassFilter);
  var i, j, k : Integer;
  begin
    FActive := Source.FActive;
    FCenterFreq := Source.FCenterFreq;
    FLowFreq := Source.FLowFreq;
    FHighFreq := Source.FHighFreq;
	  FGain := Source.FGain;
    FSectionsCnt := Source.FSectionsCnt;
    for i := 0 to 1 do
      with FSections[i] do
    begin
      for j := 0 to 4 do
      begin
        Sa[j] := Source.FSections[i].Sa[j];
        Sb[j] := Source.FSections[i].Sb[j];
      end;
      for j := 0 to 1 do
        for k := 0 to 3 do
        begin
          Sinp[j, k] := Source.FSections[i].Sinp[j, k];
          Soutp[j, k] := Source.FSections[i].Soutp[j, k];
        end;
    end;
  end;

  procedure TBandPassFilter.Init(sr: Double; ftype : TBandFilterType);
  var
    i, r, N : Integer;
    W0, W1, W2, WB, DW : Double;
    GG, GG0, GGB, g, g0, D, c0 : Double;
    a, b, c, s, ui : Double;
    alfa, beta, epsilon : Double;
  begin
    if(FGain = 0.0) then
    begin
      FActive := false;
      Exit;
    end;
    W1 := 2.0 * Pi * (FLowFreq / sr);
    W2 := 2.0 * Pi * (FHighFreq / sr);
    //if(W1 < 0) then W1 := 0;
    //if(W2 > Pi) then W2 := Pi;
    case ftype of
      butterworth_no_boost:
        begin
          if(W1 > 0.5 * Pi) then
          begin
            W1 := W1 + 0.03 * W1;
            W2 := W2 - 0.01 * W2;
          end
        end;
      chebyshev_type1_no_boost:
        begin
          W1 := W1 * 1.02;
          W2 := W2 / 1.02;
        end;
    end;
    DW := W2 - W1;
    WB := tan(DW / 2.0);
    //W0 := arccos(sin(W1 + W2) / (sin(W1) + sin(W2)));
    W0 := 2.0 * Pi * (FCenterFreq / sr);
    //if(W1 = 0) then W0 := 0;
    //if(W2 = Pi) then W0 := Pi;
    N := 4; //order
    r := N mod 2;                  //Get number of analog sections
    FSectionsCnt := (N - r) div 2;
    case ftype of
      butterworth:
        begin
          if(FGain >= 6.0) then GGb := FGain - 3.0;
          if(FGain <= -6.0) then GGb := FGain + 3.0;
          if(FGain > -6.0) and (FGain < 6) then GGb := FGain * 0.5;
        end;
      butterworth_no_boost:
        begin
          GGb := FGain * 0.48;
        end;
      chebyshev_type1, chebyshev_type1_no_boost:
        begin
          if(FGain >= 6.0) then GGb := FGain - 1.0;
          if(FGain <= -6.0) then GGb := FGain + 1.0;
          if(FGain > -6.0) and (FGain < 6) then GGb := FGain * 0.9;
        end;
      chebyshev_type2:
        begin
          if(FGain >= 6.0) then GGb := 3.0;
          if(FGain <= -6.0) then GGb := - 3.0;
          if(FGain > -6.0) and (FGain < 6) then GGb := FGain * 0.3;
        end;
    end;
    GG := power(10.0, FGain /20.0);  //Convert gains to linear scale
    GGb := power(10.0, GGb /20.0);
    GG0 := power(10.0, 0.0 /20.0);  // reference 0db
    epsilon := sqrt((GG * GG - GGb * GGb) / (GGb * GGb - GG0 * GG0));
    g := power(GG, 1.0 / N);
    g0 := power(GG0, 1.0 / N);
    c0 := cos(W0);
    if (W0 = 0) then c0 := 1;
    if (W0 = Pi/2) then c0 := 0;
    if (W0 = Pi) then c0 := -1;
    case ftype of
      butterworth, butterworth_no_boost:
        begin
          beta := power(epsilon, -1.0 / N) * WB;
        end;
      chebyshev_type1, chebyshev_type1_no_boost:
        begin
	        alfa := power(1.0 / epsilon + sqrt(1.0 + power(epsilon, -2.0)), 1.0 / N);
	        beta :=	power(GG / epsilon + GGb * sqrt(1.0 + power(epsilon, -2.0)), 1.0 / N);
	        a := 0.5 * (alfa - 1.0 / alfa);
	        b := 0.5 * (beta - g0 * g0 * (1.0 / beta));
        end;
      chebyshev_type2:
        begin
	        alfa := power(epsilon + sqrt(1.0 + epsilon * epsilon), 1.0 / N);
	        beta := power(GG0 * epsilon + GGb * sqrt(1 + epsilon*epsilon), 1.0/N);
	        a := (alfa - 1.0 / alfa) / 2.0;
	        b := (beta - g * g / beta) / 2.0;
        end;
    end;
    for i := 0 to FSectionsCnt - 1 do
    begin
      with FSections[i] do
      begin
        ui := (2.0 * (i + 1.0) - 1.0) / N;
        c := cos(ui * Pi / 2.0);
	      s := sin((2.0 * (i + 1.0) - 1.0) * Pi / (2.0 * N));
        case ftype of
          butterworth, butterworth_no_boost:
            begin
	            D := beta * beta + 2 * s * beta + 1;

              Sb[0] := (g * g * beta * beta + 2 * g* g0 * s * beta + g0 * g0) / D;
	            Sb[1] := -4 * c0 * (g0 * g0 + g * g0 * s * beta) / D;
	            Sb[2] := 2 * (g0 * g0 * (1 + 2 * c0 * c0) - g * g * beta * beta) / D;
	            Sb[3] := -4 * c0*(g0 * g0 - g * g0 * s * beta) / D;
	            Sb[4] := (g * g * beta * beta - 2 * g* g0 * s * beta + g0 * g0) / D;

	            Sa[0] := 1;
	            Sa[1] := -4 * c0 * (1 + s * beta) / D;
	            Sa[2] := 2 * (1 + 2 * c0 * c0 - beta * beta) / D;
	            Sa[3] := -4 * c0*(1 - s * beta) / D;
	            Sa[4] := (beta * beta - 2 * s * beta + 1) / D;
            end;
          chebyshev_type1, chebyshev_type1_no_boost:
            begin
              D := (a * a + c * c) * WB * WB + 2.0 * a * s * WB + 1;

              Sb[0] := ((b*b + g0*g0 * c*c) * WB*WB + 2 * g0 * b * s * WB + g0*g0) / D;
	            Sb[1] := -4 * c0 * (g0 * g0 + g0 * b * s * WB) / D;
	            Sb[2] := 2 * (g0*g0 * (1 + 2 * c0*c0) - (b*b + g0*g0 * c*c) * WB*WB) / D;
	            Sb[3] := -4 * c0 * (g0 * g0 - g0 * b * s * WB) / D;
	            Sb[4] := ((b*b + g0*g0 * c*c)* WB*WB - 2 * g0 * b * s * WB + g0 * g0) / D;

	            Sa[0] := 1;
	            Sa[1] := -4 * c0 * (1 + a * s * WB) / D;
	            Sa[2] := 2 * (1 + 2 * c0 * c0 - (a * a + c * c) * WB * WB) / D;
	            Sa[3] := -4 * c0 * (1 - a * s * WB) / D;
	            Sa[4] := ((a * a + c * c) * WB * WB - 2 * a * s * WB + 1) / D;
            end;
          chebyshev_type2:
            begin
              D := WB * WB + 2 * a * s * WB + a * a + c * c;

              Sb[0] := (g*g * WB*WB + 2 * g * b * s * WB + b*b + g*g * c*c) / D;
	            Sb[1] := -4 * c0 * (b*b + g*g * c*c + g * b * s * WB) / D;
	            Sb[2] := 2 * ((b*b + g*g * c*c) * (1 + 2 * c0*c0) - g*g * WB*WB) / D;
	            Sb[3] := -4 * c0 * (b*b + g*g * c*c - g * b * s * WB) / D;
	            Sb[4] := (g*g * WB*WB - 2 * g * b * s * WB + b*b + g*g * c*c) / D;

	            Sa[0] := 1;
	            Sa[1] := -4 * c0 * (a * a + c * c + a * s * WB) / D;
	            Sa[2] := 2 * ((a * a + c * c) * (1 + 2 * c0 * c0) - WB * WB) / D;
	            Sa[3] := -4 * c0 * (a * a + c * c  - a * s * WB) / D;
                                        { ^ not s*s - error in hpeq.pdf ?}
	            Sa[4] := (WB * WB - 2 * a * s * WB + a * a + c * c) / D;
            end;
        end;
      end;
    end;
    FActive := true;
    ResetBuffer();
  end;

  procedure TBandPassFilter.ResetBuffer();
  var i, j, k : Integer;
  begin
    for i := 0 to 1 do
      with FSections[i] do
    begin
      for j := 0 to 1 do
        for k := 0 to 3 do
        begin
          Sinp[j, k] := 0.0;
          Soutp[j, k] := 0.0;
        end;
    end;
  end;

  procedure TBandPassFilter.ProcFilter(var proc : Double; cindex : Integer);
  var
    i : Integer;
    inp, outp : double;
  begin
    if not FActive then Exit;
    inp := proc;
    for i := 0 to FSectionsCnt - 1 do
    begin
      with FSections[i] do
      begin
        outp := Sb[0] * inp + Sb[1] * Sinp[cindex, 0]  + Sb[2] * Sinp[cindex, 1]
                            + Sb[3] * Sinp[cindex, 2]  + Sb[4] * Sinp[cindex, 3]
                            - Sa[1] * Soutp[cindex, 0] - Sa[2] * Soutp[cindex, 1]
                            - Sa[3] * Soutp[cindex, 2] - Sa[4] * Soutp[cindex, 3];
	      Sinp[cindex, 3] := Sinp[cindex, 2];
	      Sinp[cindex, 2] := Sinp[cindex, 1];
	      Sinp[cindex, 1] := Sinp[cindex, 0];
	      Sinp[cindex, 0] := inp;
	      Soutp[cindex, 3] := Soutp[cindex, 2];
	      Soutp[cindex, 2] := Soutp[cindex, 1];
	      Soutp[cindex, 1] := Soutp[cindex, 0];
	      Soutp[cindex, 0] := outp;
      end;
      inp := outp;
    end;
    proc := outp;
  end;

  function TBandPassFilter.GetAFR(f, sr : Double): Double;
  var
    i : Integer;
    xr, xi, zr, zi, omega, v :Double;
    mr, mi, nr, ni: array [0..1] of Double;
    tr, ti, qr, qi: Double;
  begin
    if not FActive then
    begin
      Result := 1.0;
      Exit;
    end;
    omega := (f * 2.0* Pi) / sr;
    zr :=  cos(omega);           // (z) re
    zi :=  -sin(omega);          // (z) im
    xr := (zr * zr - zi * zi);   // (z^2)re
    xi := 2 * zr * zi;           // (z^2)im
    for i := 0 to FSectionsCnt - 1 do
      with FSections[i] do
      begin           // z^3 = z*(z^2) = x*z   z^4 = (z^2)*(z^2) = x*x
        mr[i] := Sb[0] + Sb[1] * zr + Sb[2] * xr + Sb[3] * (zr * xr - zi * xi)
               + Sb[4] * (xr * xr - xi * xi);
        mi[i] := Sb[1] * zi + Sb[2] * xi + Sb[3] * (zr * xi + zi * xr)
               + Sb[4] * 2 * xr * xi;
        nr[i] := Sa[0] + Sa[1] * zr + Sa[2] * xr + Sa[3] * (zr * xr - zi * xi)
               + Sa[4] * (xr * xr - xi * xi);
        ni[i] := Sa[1] * zi + Sa[2] * xi + Sa[3] * (zr * xi + zi * xr)
               + Sa[4] * 2 * xr * xi;
      end;
    for i := 1 to FSectionsCnt - 1 do
    begin       //  H(z) = Ï(m(z)/n(z)) = Ï(m(z))/Ï(n(z)) = p/q
      tr := mr[0] * mr[i] - mi[0] * mi[i];
      ti := mi[0] * mr[i] + mr[0] * mi[i];
      qr := nr[0] * nr[i] - ni[0] * ni[i];
      qi := ni[0] * nr[i] + nr[0] * ni[i];
      mr[0] := tr;
      mi[0] := ti;
      nr[0] := qr;
      nr[0] := qi;
    end;
    v := (qr * qr + qi * qi);
    zr := (tr * qr + ti * qi) / v;
    zi := (ti * qr - tr * qi) / v;
    Result := sqrt(zr * zr + zi * zi);
  end;

  procedure TBandPassFilter.SetCenterFreq(Value : Double);
  begin
    if(FCenterFreq <> Value) then
    begin
      //if(Value < 10.0) then Value := 10.0;
      //if(Value > 22000.0) then Value := 22000.0;
      FCenterFreq := Value;
      Changed();
    end;
  end;

  procedure TBandPassFilter.SetLowFreq(Value : Double);
  begin
    if(FLowFreq <> Value) then
    begin
      FLowFreq := Value;
      Changed();
    end;
  end;

  procedure TBandPassFilter.SetHighFreq(Value : Double);
  begin
    if(FHighFreq <> Value) then
    begin
      FHighFreq := Value;
      Changed();
    end;
  end;

  procedure TBandPassFilter.SetGain(Value : Double);
  begin
    if(FGain <> Value) then
    begin
      if(Value < -36.0) then Value := -36.0;   //dB
      if(Value > 36.0) then Value := 36.0;
      FGain := Value;
      Changed();
    end;
  end;

  procedure TBandPassFilter.Changed();
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;


{$WARNINGS ON}

end.
