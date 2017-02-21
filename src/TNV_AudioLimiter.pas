
(* $Id: TNV_AudioLimiter.pas (C)DJ VK 2016 $ *)

unit TNV_AudioLimiter;

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

  TLimiterUnit = class(TProcUnit)
  private
    FLookaheadTime : Double;
    FReleaseTime : Double;
    FLimit : Double;
    FASC : Boolean;
    FASCCoef : Double;  // 0 to 1
    FOversampling : Integer;

    FFilters : array [0..1, 0..1] of TBiquadFilter;
    FBuffer : array [0..1] of array of Double;
    FNextDeltaBuffer : array of Double;
    FNextPosBuffer : array of Integer;
    FOverAllBufferSize : Integer;
    FBufferPos : Integer;
    FResamplerFilters : Integer;
    FResamplerFactor : Integer;
    FAttack : Double;
    FRelease : Double;
    FDebug : Boolean;
    FAsc_Coef : Double;
    FDelta : Double;
    FAtt : Double;
    FNextLen : Integer;
    FNextIter :  Integer;
    FBufferSize : Integer;
    FSanitize : Boolean;
    FAscV : Double;
    FAscC : Integer;
    FAscPos : Integer;
    FAscChanged : Boolean;

    procedure SetLookaheadTime(Value : Double);
    procedure SetReleaseTime(Value : Double);
    procedure SetLimit(Value : Double);
    procedure SetASC(Value : Boolean);
    procedure SetASCCoef(Value : Double);
    procedure SetOversampling(Value : Integer);

    procedure UpSample(Inp : Double; Outp : PDblArray; cindex : Integer);
    procedure DownSample(Inp : PDblArray; var Outp : Double; cindex : Integer);
    procedure LimitProc(var Left : Double; var Right : Double);
  protected
    procedure Changed();
  published
    property Bypass;
    property InputGain;
    property OutputGain;
    property LookaheadTime : Double read FLookaheadTime write SetLookaheadTime;
    property ReleaseTime : Double read FReleaseTime write SetReleaseTime;
    property Limit : Double read FLimit write SetLimit;
    property ASC : Boolean read FASC write SetASC;
    property ASCCoef : Double read FASCCoef write SetASCCoef;
    property Oversampling : Integer read FOversampling write SetOversampling;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TLimiterUnit);
    procedure InitUnit(sr, ch: Integer); override;
    procedure ResetUnitBuffer(); override;
    procedure ResetUnit(); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
  end;

  //////////////////////////////////////

  implementation

  //////////////////////////////////////

  constructor TLimiterUnit.Create(AOwner: TComponent);
  begin
    inherited;
    FLookaheadTime := 5;
    FReleaseTime := 50;
    FLimit := 1.0;
    FASC := true;
    FASCCoef := 0.5;
    FOversampling := 1;

    FFilters[0, 0] := TBiquadFilter.Create(AOwner);
    FFilters[0, 1] := TBiquadFilter.Create(AOwner);
    FFilters[1, 0] := TBiquadFilter.Create(AOwner);
    FFilters[1, 1] := TBiquadFilter.Create(AOwner);

    FSampleRate := 44100;
    FChannelsCnt := 2;
    InitUnit(FSampleRate, FChannelsCnt);
  end;

  destructor TLimiterUnit.Destroy;
  begin
    FFilters[0, 0].Free;
    FFilters[0, 1].Free;
    FFilters[1, 0].Free;
    FFilters[1, 1].Free;
    SetLength(FBuffer[0], 0);
    SetLength(FBuffer[1], 0);
    SetLength(FNextDeltaBuffer, 0);
    SetLength(FNextPosBuffer, 0);
    inherited;
  end;

  procedure TLimiterUnit.Assign(Source: TLimiterUnit);
  begin
    inherited Assign(Source as TProcUnit);
    FLookaheadTime := Source.FLookaheadTime;
    FReleaseTime := Source.FReleaseTime;
    FLimit := Source.FLimit;
    FASC := Source.FASC;
    FASCCoef := Source.FASCCoef;
    FOversampling := Source.FOversampling;
  end;

  procedure TLimiterUnit.InitUnit(sr, ch: Integer);
  var
    LSR, FC, Q  : Double;
    i, BS: Integer;
  begin
    FSampleRate := sr;
    FChannelsCnt := ch;
    FResamplerFilters := 2;
    FResamplerFactor := min(16, max(1, FOversampling));
    LSR := 1.0 * FSampleRate * FResamplerFactor;
    FC := max(25000.0, 0.5 * FSampleRate);
    Q := 0.8;
    //init filters
    FFilters[0, 0].Init(FC, Q, LSR, 0.0, Biquad_LPF, QisQ);
    FFilters[0, 1].Init(FC, Q, LSR, 0.0, Biquad_LPF, QisQ);
    FFilters[1, 0].Init(FC, Q, LSR, 0.0, Biquad_LPF, QisQ);
    FFilters[1, 1].Init(FC, Q, LSR, 0.0, Biquad_LPF, QisQ);

    FAttack := FLookaheadTime / 1000.0;
    FRelease := FReleaseTime / 1000.0;
    FDebug := true;
    FAsc_Coef := power(0.5, -2.0 * (FASCCoef - 0.5));
    FOverAllBufferSize := Trunc(0.1 * FSampleRate {* FChannelsCnt}) + 1{FChannelsCnt}; // buffer size attack rate multiplied by 2 channels
    SetLength(FBuffer[0], FOverAllBufferSize);
    SetLength(FBuffer[1], FOverAllBufferSize);
    SetLength(FNextDeltaBuffer, FOverAllBufferSize);
    SetLength(FNextPosBuffer, FOverAllBufferSize);
    FBufferPos := 0;
    FDelta := 0.0;
    FAtt := 1.0;
    FNextLen := 0;
    FNextIter := 0;
    for i := 0 to FOverAllBufferSize - 1 do FNextPosBuffer[i] := -1; //$ffffffff
    BS := Trunc(FSampleRate * FAttack {* FChannelsCnt});
    FBufferSize := BS {- (BS mod FChannelsCnt)};   // buffer size attack rate
    FSanitize := true;
    FAscV := 0.0;
    FAscC := 0;
    FAscPos := FBufferPos;
    FAscChanged := true;

    ResetUnitBuffer();
  end;

  procedure TLimiterUnit.ResetUnitBuffer();
  var i : Integer;
  begin
    for i := 0 to FOverAllBufferSize - 1 do
    begin
      FNextDeltaBuffer[i] := 0.0;
      FBuffer[0, i] := 0.0;
      FBuffer[1, i] := 0.0;
    end;
  end;

  procedure TLimiterUnit.ResetUnit();
  begin
    SetLength(FBuffer[0], 0);
    SetLength(FBuffer[1], 0);
    SetLength(FNextDeltaBuffer, 0);
    SetLength(FNextPosBuffer, 0);
  end;

  procedure TLimiterUnit.ProcSample(var sL : Double; var sR : Double);
  var
    inp, outp: array [0..1] of Double;
    i, j : Integer;
    samples_p : array [0..1] of PDblArray;
    samples : array [0..1] of TDblArray;
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
      outp[j] := inp[j];
      setlength(samples[j], FResamplerFactor);
      samples_p[j] := @samples[j];
      UpSample(outp[j], samples_p[j], j);  // upsampling
    end;
    for i := 0 to FOversampling - 1 do
    begin  // process gain reduction
      if FChannelsCnt < 2 then samples[1][i] := 0.0;
      LimitProc(samples[0][i], samples[1][i]);
    end;
    for j :=  0 to FChannelsCnt - 1 do
    begin
      DownSample(samples_p[j], outp[j], j);     // downsampling
      outp[j] := min(max(outp[j], -FLimit), FLimit); //make shure NOTHING is above limit
      outp[j] := (outp[j] / FLimit) * FoutputGain;         // autolevel and out level
      case j of
        0: sL := outp[j];
        1: sR := outp[j];
      end;
    end;
  end;

  procedure TLimiterUnit.UpSample(Inp : Double; Outp : PDblArray; cindex : Integer);  // upsampling
  var
    tmp : TDblArray;
    i, f : Integer;
  begin
    tmp := Outp^;
    tmp[0] := Inp;
    if (FResamplerFactor > 1) then
    begin
      for f := 0 to FResamplerFilters - 1 do
        FFilters[0 , f].ProcFilter(tmp[0], cindex);
      for i := 1 to FResamplerFactor - 1 do
      begin
        tmp[i] := Inp;
        for f := 0 to FResamplerFilters - 1 do
          FFilters[0 , f].ProcFilter(tmp[i], cindex);
      end;
    end;
  end;

  procedure TLimiterUnit.DownSample(Inp : PDblArray; var Outp : Double; cindex : Integer);  // downsampling
  var
    tmp : TDblArray;
    i, f : Integer;
  begin
    tmp := Inp^;
    if (FResamplerFactor > 1) then
      for i := 0 to FResamplerFactor - 1 do
         for f := 0 to FResamplerFilters - 1 do
           FFilters[1 , f].ProcFilter(tmp[i], cindex);
    Outp := tmp[0];
  end;

  procedure TLimiterUnit.LimitProc(var Left : Double; var Right : Double);
  var
    Peak, Peak1, Delta1, Delta2 : Double;
    Att1, AAtt, RDelta, RDelta1, RDelta2 : Double;
    Found : Boolean;
    i, j, NextPos , DeltaPos, OutPos: Integer;
  begin
    if(FSanitize) then
    begin
      // if we're sanitizing (zeroing) the buffer on attack time change,
      // don't write the samples to the buffer
      FBuffer[0, FBufferPos] := 0.0;
      FBuffer[1, FBufferPos{ + 1}] := 0.0;
    end
    else
    begin
      FBuffer[0, FBufferPos] := Left;       // fill lookahead buffer
      FBuffer[1, FBufferPos{ + 1}] := Right;
    end;

    Peak := max(abs(Left), abs(Right)); // input Peak - impact higher channel
    if(Peak > FLimit) then
    begin
	  if(FASC) then //if asc active
      begin                     //add an eventually appearing Peak to the asc fake buffer
        FAscV := FAscV + Peak;
        FAscC := FAscC + 1;
      end;
      if(Peak < 1.0e-10) then Peak := 1.0e-10; 
      Att1 := min(FLimit / Peak, 1.0); // calc the attenuation needed to reduce incoming Peak
      RDelta := (1.0 - Att1) / (FSampleRate * FRelease); // calc release without any asc to keep all relevant Peaks
      Delta1 := (FLimit / Peak - FAtt) / FBufferSize {* FChannelsCnt}; // calc the delta for walking to incoming Peak attenuation
      if(Delta1 < FDelta) then
      begin
        // is the delta more important than the actual one?
        // if so, we can forget about all stored deltas (because they can't
        // be more important - we already checked that earlier) and use this
        // delta now. and we have to create a release delta in nextpos buffer
        FNextPosBuffer[0] := FBufferPos;
        FNextPosBuffer[1] := -1;
        FNextDeltaBuffer[0] := RDelta;
        FNextLen := 1;
        FNextIter := 0;
        FDelta := Delta1;
      end
      else
      begin
        // we have a Peak on input its delta is less important than the
        // actual delta. But what about the stored deltas we're following?
        Found := false;
        for i := FNextIter  to  FNextIter + FNextLen - 1 do
        begin
          j := i mod FBufferSize;  // walk through our nextpos buffer
          NextPos := FNextPosBuffer[j];
          // calculate a delta for the next stored Peak
          // is the Left or the Right channel on this position more  important?
          Peak1 := max(abs(FBuffer[0, NextPos]), abs(FBuffer[1, NextPos]));
          if(Peak1 < 1.0e-10) then Peak1 := 1.0e-10;
          // calc a delta to use to reach our incoming Peak from the stored position
          DeltaPos := ((FBufferSize - FNextPosBuffer[j] + FBufferPos) mod FBufferSize) {/ FChannelsCnt};
          if(DeltaPos = 0) then
          begin

          end;
          Delta1 := (FLimit / Peak - FLimit / Peak1) / DeltaPos;
          if(Delta1 < FNextDeltaBuffer[j]) then // if the buffered delta is more important than the delta
          begin                                 // used to reach our Peak from the stored position,
            FNextDeltaBuffer[j] := Delta1;  // store the new delta at that position and stop the loop
            Found := true;
            Break;
          end;
        end;
        if(Found) then
        begin                                         // there was something more important in the next-buffer.
          FNextLen := i - FNextIter + 1;              // throw away any position and delta after the important position
          FNextPosBuffer[(FNextIter + FNextLen) mod FBufferSize] := FBufferPos;
          FNextDeltaBuffer[(FNextIter + FNextLen) mod FBufferSize] := RDelta;  //add a new release delta
          FNextPosBuffer[(FNextIter + FNextLen + 1) mod FBufferSize] := -1; // set the next following position value to -1 (cleaning up the nextpos buffer)
          FNextLen := FNextLen + 1;  // and raise the length of our nextpos buffer for keeping the release value
        end;
      end;
    end;

    OutPos := (FBufferPos + 1{+FChannelsCnt}) mod FBufferSize;
    Left := FBuffer[0, OutPos];      // switch Left and Right pointers
    Right := FBuffer[1, OutPos {+1}]; //in buffer to output position
    Peak := max(abs(Left), abs(Right));               // if a Peak leaves the buffer, remove it from asc fake buffer
    if(Peak < 1.0e-10) then Peak := 1.0e-10;
    if(FBufferPos = FAscPos) and not FAscChanged then FAscPos := -1;   // but only if we're not sanitizing asc buffer
    if FASC and (FAscPos = -1) and (Peak > FLimit) then
    begin
      FAscV := FAscV - Peak;
      FAscC := FAscC - 1;
    end;
    FAtt := FAtt + FDelta;   // change the attenuation level
    Left := Left * FAtt;    // ...and calculate output from it
    Right := Right * FAtt;

    if(FNextPosBuffer[FNextIter] = OutPos) then
    begin
      // if we reach a buffered position, change the actual delta and erase
      // this (the first) element from nextpos and nextdelta buffer
      if(FASC) then
      begin
        // set delta to asc influenced release delta
        RDelta1 := (1.0 - FAtt) / (FSampleRate * FRelease); // calc a release delta from attenuation
        if FASC and (FAscC > 0) then
        begin
           if(abs(FAsc_Coef * FAscV) < 1.0e-10) then AAtt := 0.0  // calc the att for average input to walk to
           else AAtt := FLimit / (FAsc_Coef * FAscV) * FAscC;     // if we use asc (att of average signal)
          if(AAtt > FAtt) then
          begin
            RDelta2 := max((AAtt - FAtt) / (FSampleRate * FRelease), RDelta1 / 10); // check if releasing to average level
            if(RDelta2 < RDelta1) then RDelta1 := RDelta2;                          // of peaks is steeper than releasing to 1.f
          end;
        end;
        FDelta := RDelta1;
        if(FNextLen > 1) then
        begin
          // if there are more positions to walk to, calc delta to next
          // position in buffer and compare it to release delta (keep
          // changes between peaks below asc steepness)
          NextPos := FNextPosBuffer[(FNextIter + 1) mod FBufferSize];
          Peak1 := max(abs(FBuffer[0, NextPos]), abs(FBuffer[1, NextPos{ + 1}]));
          if(Peak1 < 1.0e-10) then Peak1 := 1.0e-10;
          DeltaPos := ((FBufferSize + NextPos - OutPos) mod FBufferSize) {/ FChannelsCnt};
          if(DeltaPos = 0) then
          begin

          end;
          Delta2 := (FLimit / Peak1 - FAtt) / DeltaPos;
          if(Delta2 < FDelta) then FDelta := Delta2;
        end;
      end
      else                                      // if no asc
      begin
        FDelta := FNextDeltaBuffer[FNextIter];  //set delta from nextdelta buffer
        FAtt := FLimit / Peak;                  //fix the attenuation
      end;
      FNextLen := FNextLen - 1;          // remove first element from circular nextpos buffer
      FNextPosBuffer[FNextIter] := -1;
      FNextIter := (FNextIter + 1) mod FBufferSize;
    end;

    if (FAtt > 1.0) then             // release time seems over,
    begin
      FAtt := 1.0;                //reset attenuation and delta
      FDelta := 0.0;
      FNextIter := 0;
      FNextLen := 0;
      FNextPosBuffer[0] := -1;
    end;
    if(FSanitize) then       // main limiting party is over, let's cleanup the puke
    begin
      Left := 0.0;          // we're sanitizing? then send 0.f as output
      Right := 0.0;
    end;
    if(FAtt <= 0.0) then                           // security personnel pawing your values
    begin
      FAtt := 0.0000000000001;                    // if this happens we're doomed!!
      FDelta := (1.0 - FAtt) / (FSampleRate * FRelease); // may happen on manually lowering attack
    end;
    if(FAtt <> 1.0) and (1 - FAtt < 0.0000000000001) then FAtt := 1.0;  // denormalize att
    if(FDelta <> 0.0) and (abs(FDelta) < 0.00000000000001) then FDelta := 0.0;  // denormalize delta
    Left := Left + 1e-18;    //denormal(Left); // post treatment (denormal, limit)
    Left := Left - 1e-18;
    Right := Right + 1e-18;    //denormal(Right);
    Right := Right - 1e-18;
    FBufferPos := OutPos;  // step forward in our sample ring buffer
    if FSanitize and (FBufferPos = 0) then FSanitize := false;   // sanitizing is always done after a full cycle through the lookahead buffer
    FAscChanged := false;
  end;

  procedure TLimiterUnit.SetLookaheadTime(Value : Double);
  begin
    if(FLookaheadTime<> Value) then
    begin
      if(Value < 0.1) then Value := 0.1;
      if(Value > 10) then Value := 10;
      FLookaheadTime := Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.SetReleaseTime(Value : Double);
  begin
    if(FReleaseTime <> Value) then
    begin
      if(Value < 1.0) then Value := 1.0;
      if(Value > 1000) then Value := 1000;
      FReleaseTime := Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.SetLimit(Value : Double);
  begin
    if(FLimit <> Value) then
    begin
      if(Value < 0.0625) then Value := 0.0625;
      if(Value > 1.0) then Value := 1.0;
      FLimit := Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.SetASC(Value : Boolean);
  begin
    if(FASC <> Value) then
    begin
      FASC:= Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.SetASCCoef(Value : Double);
  begin
    if(FASCCoef <> Value) then
    begin
      if(Value < 0.0) then Value := 0.0;
      if(Value > 1.0) then Value := 1.0;
      FASCCoef := Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.SetOversampling(Value : Integer);
  begin
    if(FOversampling <> Value) then
    begin
      if(Value < 1) then Value := 1;
      if(Value > 4) then Value := 4;
      FOversampling := Value;
      Changed();
    end;
  end;

  procedure TLimiterUnit.Changed();
  begin
    if(FActive) then ReInitFlag := true
    else
      InitUnit(FSampleRate, FChannelsCnt);
  end;

{$WARNINGS ON}

end.
