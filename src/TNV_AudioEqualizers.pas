
(* $Id: TNV_AudioEqualizers.pas (C)DJ VK 2016 $ *)

unit TNV_AudioEqualizers;

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, TNV_AudioFilters, TNV_AudioClasses,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Math;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TParamFilterEnum = (ALL = -1, HPF = 0, LPF, HSF, LSF, PK1, PK2, PK3, PK4, PK5);

  TParametricEQUnit = class(TProcUnit)
  private
    FHighPassFilter : TLPHPFilter;
    FLowPassFilter : TLPHPFilter;
    FLowShelfFilter : TLSHSFilter;
    FHighShelfFilter : TLSHSFilter;
    FPeakFilter1 : TPKFilter;
    FPeakFilter2 : TPKFilter;
    FPeakFilter3 : TPKFilter;
    FPeakFilter4 : TPKFilter;
    FPeakFilter5 : TPKFilter;

    ReInitIndex : TParamFilterEnum;
    //FKeepGliding : Integer;
    //mutable int last_peak;
    procedure SetLowPassFilter(Value : TLPHPFilter);
    procedure SetHighPassFilter(Value : TLPHPFilter);
    procedure SetLowShelfFilter(Value : TLSHSFilter);
    procedure SetHighShelfFilter(Value : TLSHSFilter);
    procedure SetPeakFilter1(Value : TPKFilter);
    procedure SetPeakFilter2(Value : TPKFilter);
    procedure SetPeakFilter3(Value : TPKFilter);
    procedure SetPeakFilter4(Value : TPKFilter);
    procedure SetPeakFilter5(Value : TPKFilter);
    procedure OnFilterChanged(Sender: TObject);
  protected
    procedure Changed;

  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property HighPassFilter : TLPHPFilter read FHighPassFilter write SetHighPassFilter;
    property LowPassFilter : TLPHPFilter read FLowPassFilter write SetLowPassFilter;
    property LowShelfFilter : TLSHSFilter read FLowShelfFilter write SetLowShelfFilter;
    property HighShelfFilter : TLSHSFilter read FHighShelfFilter write SetHighShelfFilter;
    property PeakFilter1 : TPKFilter read FPeakFilter1 write SetPeakFilter1;
    property PeakFilter2 : TPKFilter read FPeakFilter2 write SetPeakFilter2;
    property PeakFilter3 : TPKFilter read FPeakFilter3 write SetPeakFilter3;
    property PeakFilter4 : TPKFilter read FPeakFilter4 write SetPeakFilter4;
    property PeakFilter5 : TPKFilter read FPeakFilter5 write SetPeakFilter5;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TParametricEQUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;

    function GetAFR(f : Double; sr : Integer; findex : TParamFilterEnum): Double;
  end;

  TGraphicEQUnit = class(TProcUnit)
  private
    FFilterType : TBandFilterType;
    FEQBand : TEQBand;

    FFilterCnt : Integer;
    FFilters : array[0..30] of TBandPassFilter;

    ReInitIndex : Integer;

    procedure SetFilterType(Value : TBandFilterType);
    procedure SetEQBand(Value : TEQBand);
    function GetBandFreq(Index: Integer) : Double;
    function GetBandGain(Index: Integer) : Double;
    procedure SetBandGain(Index: Integer; Amp : Double);
    //procedure OnFilterChanged(Sender: TObject);
  protected
    procedure Changed;
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property FilterType : TBandFilterType read FFilterType write SetFilterType;
    property EQBand : TEQBand read FEQBand write SetEQBand;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TGraphicEQUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
    function GetAFR(f : Double; sr, findex : Integer): Double;
    function GetBandCount(): Integer;
    property BandGain[Index: Integer] : Double read GetBandGain write SetBandGain;
    property BandFreq[Index: Integer] : Double read GetBandFreq;
  end;


implementation

  /////////////////////////////////////////////////

  constructor TParametricEQUnit.Create(AOwner: TComponent);
  begin
    inherited;
    ReInitIndex := ALL;
    FHighPassFilter := TLPHPFilter.Create(AOwner);
    FHighPassFilter.OnChange := OnFilterChanged;
    FLowPassFilter := TLPHPFilter.Create(AOwner);
    FLowPassFilter.OnChange := OnFilterChanged;
    FLowShelfFilter := TLSHSFilter.Create(AOwner);
    FLowShelfFilter.OnChange := OnFilterChanged;
    FHighShelfFilter := TLSHSFilter.Create(AOwner);
    FHighShelfFilter.OnChange := OnFilterChanged;
    FPeakFilter1 := TPKFilter.Create(AOwner);
    FPeakFilter1.OnChange := OnFilterChanged;
    FPeakFilter2 := TPKFilter.Create(AOwner);
    FPeakFilter2.OnChange := OnFilterChanged;
    FPeakFilter3 := TPKFilter.Create(AOwner);
    FPeakFilter3.OnChange := OnFilterChanged;
    FPeakFilter4 := TPKFilter.Create(AOwner);
    FPeakFilter4.OnChange := OnFilterChanged;
    FPeakFilter5 := TPKFilter.Create(AOwner);
    FPeakFilter5.OnChange := OnFilterChanged;

    FHighPassFilter.CutoffFreq := 100;
    FLowPassFilter.CutoffFreq := 18000;
    FLowShelfFilter.CenterFreq := 500;
    FHighShelfFilter.CenterFreq := 15000;
    FPeakFilter1.CenterFreq := 1000;
    FPeakFilter2.CenterFreq := 2000;
    FPeakFilter3.CenterFreq := 4000;
    FPeakFilter4.CenterFreq := 8000;
    FPeakFilter5.CenterFreq := 12000;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    ReInitIndex := ALL;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TParametricEQUnit.Destroy;
  begin
    FLowPassFilter.Free;
    FHighPassFilter.Free;
    FLowShelfFilter.Free;
    FHighShelfFilter.Free;
    FPeakFilter1.Free;
    FPeakFilter2.Free;
    FPeakFilter3.Free;
    FPeakFilter4.Free;
    FPeakFilter5.Free;
    inherited;
  end;

  procedure TParametricEQUnit.Assign(Source: TParametricEQUnit);
  begin
    FHighPassFilter.Assign(Source.FHighPassFilter);
    FLowPassFilter.Assign(Source.FLowPassFilter);
    FLowShelfFilter.Assign(Source.FLowShelfFilter);
    FHighShelfFilter.Assign(Source.FHighShelfFilter);
    FPeakFilter1.Assign(Source.FPeakFilter1);
    FPeakFilter2.Assign(Source.FPeakFilter2);
    FPeakFilter3.Assign(Source.FPeakFilter3);
    FPeakFilter4.Assign(Source.FPeakFilter4);
    FPeakFilter5.Assign(Source.FPeakFilter5);
  end;

  procedure TParametricEQUnit.InitUnit(sr, ch: Integer);
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    if (ReInitIndex = ALL) then
    begin
      FLowPassFilter.Init(true, sr);
      FHighPassFilter.Init(false, sr);
      FLowShelfFilter.Init(true, sr);
      FHighShelfFilter.Init(false, sr);
      FPeakFilter1.Init(sr);
      FPeakFilter2.Init(sr);
      FPeakFilter3.Init(sr);
      FPeakFilter4.Init(sr);
      FPeakFilter5.Init(sr);
      Exit;
    end;
    case ReInitIndex of
      HPF: FHighPassFilter.Init(false, sr);
      LPF: FLowPassFilter.Init(true, sr);
      LSF: FLowShelfFilter.Init(true, sr);
      HSF: FHighShelfFilter.Init(false, sr);
      PK1: FPeakFilter1.Init(sr);
      PK2: FPeakFilter2.Init(sr);
      PK3: FPeakFilter3.Init(sr);
      PK4: FPeakFilter4.Init(sr);
      PK5: FPeakFilter5.Init(sr);
    end;
  end;

  procedure TParametricEQUnit.ResetUnitBuffer();
  begin
    FLowPassFilter.ResetBuffer();
    FHighPassFilter.ResetBuffer();
    FLowShelfFilter.ResetBuffer();
    FHighShelfFilter.ResetBuffer();
    FPeakFilter1.ResetBuffer();
    FPeakFilter2.ResetBuffer();
    FPeakFilter3.ResetBuffer();
    FPeakFilter4.ResetBuffer();
    FPeakFilter5.ResetBuffer();
  end;

  procedure TParametricEQUnit.ResetUnit();
  begin
  end;

  procedure TParametricEQUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp, proc: array [0..1] of Double;
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
      outp[j] := 0.0;
      proc[j] := inp[j] * FInputGain;
      if FHighPassFilter.Active then
      begin
        FHighPassFilter.ProcFilter(proc[j], 0, j);
        if FHighPassFilter.PerOctaveReduction >= red24db then
           FHighPassFilter.ProcFilter(proc[j], 1, j);
        if FHighPassFilter.PerOctaveReduction >= red36db then
           FHighPassFilter.ProcFilter(proc[j], 2, j);
        if FHighPassFilter.PerOctaveReduction = red48db then
           FHighPassFilter.ProcFilter(proc[j], 3, j);
      end;
      if FLowPassFilter.Active then
      begin
        FLowPassFilter.ProcFilter(proc[j], 0, j);
        if FLowPassFilter.PerOctaveReduction >= red24db then
           FLowPassFilter.ProcFilter(proc[j], 1, j);
        if FLowPassFilter.PerOctaveReduction >= red36db then
           FLowPassFilter.ProcFilter(proc[j], 2, j);
        if FLowPassFilter.PerOctaveReduction = red48db then
           FLowPassFilter.ProcFilter(proc[j], 3, j);
      end;
      if FLowShelfFilter.Active then FLowShelfFilter.ProcFilter(proc[j], j);
      if FHighShelfFilter.Active then FLowShelfFilter.ProcFilter(proc[j], j);
      if FPeakFilter1.Active then FPeakFilter1.ProcFilter(proc[j], j);
      if FPeakFilter2.Active then FPeakFilter2.ProcFilter(proc[j], j);
      if FPeakFilter3.Active then FPeakFilter3.ProcFilter(proc[j], j);
      if FPeakFilter4.Active then FPeakFilter4.ProcFilter(proc[j], j);
      if FPeakFilter5.Active then FPeakFilter5.ProcFilter(proc[j], j);
      outp[j] := proc[j] * FOutputGain;
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  function TParametricEQUnit.GetAFR(f : Double; sr : Integer; findex : TParamFilterEnum): Double;
  var FilterSR, afr: Double;
  begin
    if(sr = 0) then FilterSR := FSampleRate
    else FilterSR := sr;
    case findex of
      HPF: afr := FHighPassFilter.GetAFR(f, FilterSR);
      LPF: afr := FLowPassFilter.GetAFR(f, FilterSR);
      LSF: afr := FLowShelfFilter.GetAFR(f, FilterSR);
      HSF: afr := FHighShelfFilter.GetAFR(f, FilterSR);
      PK1: afr := FPeakFilter1.GetAFR(f, FilterSR);
      PK2: afr := FPeakFilter2.GetAFR(f, FilterSR);
      PK3: afr := FPeakFilter3.GetAFR(f, FilterSR);
      PK4: afr := FPeakFilter4.GetAFR(f, FilterSR);
      PK5: afr := FPeakFilter5.GetAFR(f, FilterSR);
    end;
    Result := 20 * log10(afr);
  end;

  procedure TParametricEQUnit.SetLowPassFilter(Value : TLPHPFilter);
  begin
    FLowPassFilter.Assign(Value);
    ReInitIndex := LPF;
	  Changed;
  end;

  procedure TParametricEQUnit.SetHighPassFilter(Value : TLPHPFilter);
  begin
    FHighPassFilter.Assign(Value);
    ReInitIndex := HPF;
	  Changed;
  end;

  procedure TParametricEQUnit.SetLowShelfFilter(Value : TLSHSFilter);
  begin
    FLowShelfFilter.Assign(Value);
    ReInitIndex := LSF;
	  Changed;
  end;

  procedure TParametricEQUnit.SetHighShelfFilter(Value : TLSHSFilter);
  begin
    FHighShelfFilter.Assign(Value);
    ReInitIndex := HSF;
	  Changed;
  end;

  procedure TParametricEQUnit.SetPeakFilter1(Value : TPKFilter);
  begin
    FPeakFilter1.Assign(Value);
    ReInitIndex := PK1;
	  Changed;
  end;

  procedure TParametricEQUnit.SetPeakFilter2(Value : TPKFilter);
  begin
    FPeakFilter2.Assign(Value);
    ReInitIndex := PK2;
	  Changed;
  end;

  procedure TParametricEQUnit.SetPeakFilter3(Value : TPKFilter);
  begin
    FPeakFilter3.Assign(Value);
    ReInitIndex := PK3;
	  Changed;
  end;

  procedure TParametricEQUnit.SetPeakFilter4(Value : TPKFilter);
  begin
    FPeakFilter4.Assign(Value);
    ReInitIndex := PK4;
	  Changed;
  end;

  procedure TParametricEQUnit.SetPeakFilter5(Value : TPKFilter);
  begin
    FPeakFilter5.Assign(Value);
    ReInitIndex := PK5;
	  Changed;
  end;

  procedure TParametricEQUnit.OnFilterChanged(Sender: TObject);
  begin
    if (Sender is TLPHPFilter) then
    begin
      if((Sender as TLPHPFilter) = FHighPassFilter) then ReInitIndex := HPF;
      if((Sender as TLPHPFilter) = FLowPassFilter) then ReInitIndex := LPF;
    end;
    if (Sender is TLSHSFilter) then
    begin
      if((Sender as TLSHSFilter) = FHighShelfFilter) then ReInitIndex := HSF;
      if((Sender as TLSHSFilter) = FLowShelfFilter) then ReInitIndex := LSF;
    end;
    if (Sender is TPKFilter) then
    begin
      if((Sender as TPKFilter) = FPeakFilter1) then ReInitIndex := PK1;
      if((Sender as TPKFilter) = FPeakFilter2) then ReInitIndex := PK2;
      if((Sender as TPKFilter) = FPeakFilter3) then ReInitIndex := PK3;
      if((Sender as TPKFilter) = FPeakFilter4) then ReInitIndex := PK4;
      if((Sender as TPKFilter) = FPeakFilter5) then ReInitIndex := PK5;
    end;
	  Changed;
  end;

  procedure TParametricEQUnit.Changed();
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
  end;

  ////////////////////////////

  constructor TGraphicEQUnit.Create;
  var i: Integer;
  begin
    inherited;
	  FEQBand := Band_ThirdOctave;
	  FFilterCnt := 31;
    FFilterType := butterworth_no_boost;
    for i := 0 to 30 do
    begin
      FFilters[i] := TBandPassFilter.Create(AOwner);
      //FFilters[i].OnChange := OnFilterChanged;
    end;

    FSampleRate := 44100;
    FChannelsCnt := 2;
    ReInitIndex := -1;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TGraphicEQUnit.Destroy;
  var i: Integer;
  begin
    for i := 0 to 30 do
    begin
      FFilters[i].Free;
    end;
    inherited;
  end;

  procedure TGraphicEQUnit.Assign(Source: TGraphicEQUnit);
  var i: Integer;
  begin
    FFilterType := Source.FFilterType;
    FEQBand := Source.FEQBand;
    FFilterCnt := Source.FFilterCnt;
    for i := 0 to 30 do FFilters[i].Assign(Source.FFilters[i]);
  end;

  procedure TGraphicEQUnit.InitUnit(sr, ch: Integer);
  var
    FNaik : Double;
	  i, Pos1k: Integer;
    FreqStep, FreqBandFactor: Double;
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    if(ReInitIndex >= 0) then
    begin
      FFilters[ReInitIndex].Init(FSampleRate , FFilterType);
      Exit;
    end;
	  case FEQBand of
		  Band_2Octave:
		  begin
			  FFilterCnt := 6;
        Pos1k := 3;
        FreqStep := 4.0;
        FreqBandFactor := 2;
		  end;
		  Band_Octave:
		  begin
			  FFilterCnt := 11;
        Pos1k := 6;
        FreqStep := 2.0;
        FreqBandFactor := sqrt(2.0);
		  end;
		  Band_HalfOctave:
		  begin
			  FFilterCnt := 20;
        Pos1k := 11;
        FreqStep := sqrt(2.0);
        FreqBandFactor := power(2.0, 0.25);
		  end;
		  Band_ThirdOctave:
		  begin
			  FFilterCnt := 31;
        Pos1k := 17;
        FreqStep := power(2.0, 1.0 / 3.0);
        FreqBandFactor := power(2.0, 1.0 / 6.0);
		  end;
	  end;
    FNaik := FSampleRate / 2.0;
	  for i := 0 to FFilterCnt - 1 do
	  begin
      with FFilters[i] do
      begin
        CenterFreq := 1000.0 * power(FreqStep, i - Pos1k);
        LowFreq := CenterFreq / FreqBandFactor;
        HighFreq := CenterFreq * FreqBandFactor;
        Init(FSampleRate , FFilterType);
        if(CenterFreq < double(FNaik)) then
        begin
        end;
        //else
      end;
    end;
  end;

  procedure TGraphicEQUnit.ResetUnitBuffer();
  var i: Integer;
  begin
    for i := 0 to FFilterCnt - 1 do
	  begin
      FFilters[i].ResetBuffer;
    end;
  end;

  procedure TGraphicEQUnit.ResetUnit();
  begin
  end;

  procedure TGraphicEQUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp: array [0..1] of Double;
    proc: array [0..1] of Double;
    i, j : Integer;
  begin
    if ReInitFlag then
    begin
      ReInitFlag := false;
      InitUnit(FSampleRate, FChannelsCnt);
      ReInitIndex := -1;
    end;
    if(Bypass) then Exit;
    for j :=  0 to FChannelsCnt - 1 do
    begin
      case j of
        0: inp[j] := sL;
        1: inp[j] := sR;
      end;
      proc[j] := inp[j] * FInputGain;
      for i := 0 to FFilterCnt - 1 do
      begin
        FFilters[i].ProcFilter(proc[j], j);
      end;
      outp[j] := proc[j];  // switching filter type use  proc[j] * sw[j].get_ramp()
      outp[j] := outp[j] * FOutputGain;
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  function TGraphicEQUnit.GetAFR(f: Double; sr, findex : Integer): Double;
  var FilterSR: Double;
  begin
    //if (findex < 0) or (findex >= FFilterCnt) then
    //  raise EAuException.Create('Incorrect Band Index');
    if(sr = 0) then FilterSR := FSampleRate
    else FilterSR := sr;
    Result := 20 * log10(FFilters[findex].GetAFR(f, FilterSR));
  end;

  procedure TGraphicEQUnit.SetFilterType(Value : TBandFilterType);
  begin
    if(FFilterType <> Value) then
    begin
      FFilterType := Value;
      ReInitIndex := -1;
      Changed();
    end;
  end;

  procedure TGraphicEQUnit.SetEQBand(Value : TEQBand);
  begin
    if(FEQBand <> Value) then
    begin
      FEQBand := Value;
      ReInitIndex := -1;
      Changed();
    end;
  end;

  function TGraphicEQUnit.GetBandCount(): Integer;
  begin
    Result := FFilterCnt;
  end;

  function TGraphicEQUnit.GetBandFreq;
  begin
    //if (Index < 0) or (Index >= FFilterCnt) then
    //  raise EAuException.Create('Incorrect Band Index');
    Result := FFilters[Index].CenterFreq;
  end;

  function TGraphicEQUnit.GetBandGain;
  begin
    //if (Index < 0) or (Index >= FFilterCnt) then
    //  raise EAuException.Create('Incorrect Band Index');
    Result := FFilters[Index].Gain;
  end;

  procedure TGraphicEQUnit.SetBandGain;
  begin
    //if (Index < 0) or (Index >= FFilterCnt) then
    //  raise EAuException.Create('Incorrect Band Index');
    if FFilters[Index].Gain <> Amp then
    begin
      if(Amp < -36.0) then Amp := -36.0;
      if(Amp > 36.0) then Amp := 36;
      FFilters[Index].Gain := Amp;
      ReInitIndex := Index;
      Changed();
    end;
  end;

  {procedure TGraphicEQUnit.OnFilterChanged(Sender: TObject);
  var i : Integer;
  begin
    if (Sender is TBandPassFilter) then
      for i := 0 to FFilterCnt do
        if((Sender as TBandPassFilter) = FFilters[i]) then
        begin
          ReInitIndex := i;
          Changed;
        end;  
  end;}

  procedure TGraphicEQUnit.Changed();
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
      ReInitIndex := -1;
  end;

{$WARNINGS ON}

end.
