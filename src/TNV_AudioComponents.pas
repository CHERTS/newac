
(* $Id: TNV_AudioComponents.pas (C)DJ VK 2016 $ *)

unit TNV_AudioComponents;

interface

{$WARNINGS OFF}

uses
  Classes, Math, ACS_Types, ACS_Procs, ACS_Classes,
  TNV_AudioClasses, TNV_AudioDynamics, TNV_AudioEqualizers,
  TNV_AudioFilters, TNV_AudioLimiter, TNV_AudioReverb,
  TNV_AudioMixer;

const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TNVCompressor = class(TProcPlugin)
  private
    FCompressor : TCompressorUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVGate = class(TProcPlugin)
  private
    FGate : TGateUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVDeesser = class(TProcPlugin)
  private
    FDeesser : TDeesserUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;

  end;

  TNVLimiter = class(TProcPlugin)
  private
    FLimiter : TLimiterUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVReverb = class(TProcPlugin)
  private
    FReverb : TReverbUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVParametricEQ = class(TProcPlugin)
  private
    FEqualizer : TParametricEQUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVGraphicEQ = class(TProcPlugin)
  private
    FEqualizer : TGraphicEQUnit;
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
  published

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

  TNVVoiceProcessor = class(TConvPlugin)
  private
    FInputMixer : TInputMixerUnit;
    FOutputMixer : TOutputMixerUnit;
    FParametricEQ : TParametricEQUnit;
    FGraphicEQ : TGraphicEQUnit;
    FCompressor : TCompressorUnit;
    FGate : TGateUnit;
    FDeesser : TDeesserUnit;
    FLimiter : TLimiterUnit;
    FReverb : TReverbUnit;

    procedure SetInputMixer(Value : TInputMixerUnit);
    procedure SetOutputMixer(Value : TOutputMixerUnit);
    procedure SetParametricEQ(Value : TParametricEQUnit);
    procedure SetGraphicEQ(Value : TGraphicEQUnit);
    procedure SetCompressor(Value : TCompressorUnit);
    procedure SetGate(Value : TGateUnit);
    procedure SetDeesser(Value : TDeesserUnit);
    procedure SetLimiter(Value : TLimiterUnit);
    procedure SetReverb(Value : TReverbUnit);
  protected
    procedure InitProc(); override;
    procedure ResetProc(); override;
    procedure ProcData(var Sample : TDblArray); override;
    procedure ProcSample(var sL : Double; var sR : Double); override;
    procedure SampleRateChanged; override;
  published
    property InputMixer : TInputMixerUnit read FInputMixer write SetInputMixer;
    property OutputMixer : TOutputMixerUnit read FOutputMixer write SetOutputMixer;
    property ParametricEQ : TParametricEQUnit read FParametricEQ write SetParametricEQ;
    property GraphicEQ : TGraphicEQUnit read FGraphicEQ write SetGraphicEQ;
    property Compressor : TCompressorUnit read FCompressor write SetCompressor;
    property Gate : TGateUnit read FGate write SetGate;
    property Deesser : TDeesserUnit read FDeesser write SetDeesser;
    property Limiter : TLimiterUnit read FLimiter write SetLimiter;
    property Reverb : TReverbUnit read FReverb write SetReverb;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
  end;

//////////////////////////////////

implementation

//////////////////////////////////

  constructor TNVCompressor.Create(AOwner: TComponent);
  begin
    inherited;
    FCompressor := TCompressorUnit.Create(AOwner);
  end;

  destructor TNVCompressor.Destroy;
  begin
    FCompressor.Free;
    inherited;
  end;

  procedure TNVCompressor.InitProc();
  begin
    if Assigned(FInput) then
      FCompressor.InitUnit(FInput.SampleRate, FInput.Channels)
    else FCompressor.InitUnit(44100, 2);
    FCompressor.SetActive(true);
  end;

  procedure TNVCompressor.ResetProc();
  begin
    FCompressor.SetActive(false);
    FCompressor.ResetUnit();
  end;

  procedure TNVCompressor.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      ///////////////////////
      Smp[j] := outp[j];
    end;
  end;

////////////////////////////////////

  constructor TNVGate.Create(AOwner: TComponent);
  begin
    inherited;
    FGate := TGateUnit.Create(AOwner);
  end;

  destructor TNVGate.Destroy;
  begin
    FGate.Free;
    inherited;
  end;

  procedure TNVGate.InitProc();
  begin
    if Assigned(FInput) then
      FGate.InitUnit(FInput.SampleRate, FInput.Channels)
    else FGate.InitUnit(44100, 2);
    FGate.SetActive(true);
  end;

  procedure TNVGate.ResetProc();
  begin
    FGate.SetActive(false);
    FGate.ResetUnit();
  end;

  procedure TNVGate.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      ///////////////////////
      Smp[j] := outp[j];
    end;
  end;

////////////////////////////////////

  constructor TNVDeesser.Create(AOwner: TComponent);
  begin
    inherited;
    FDeesser := TDeesserUnit.Create(AOwner);
  end;

  destructor TNVDeesser.Destroy;
  begin
    FDeesser.Free;
    inherited;
  end;

  procedure TNVDeesser.InitProc();
  begin
    if Assigned(FInput) then
      FDeesser.InitUnit(FInput.SampleRate, FInput.Channels)
    else FDeesser.InitUnit(44100, 2);
    FDeesser.SetActive(true);
  end;

  procedure TNVDeesser.ResetProc();
  begin
    FDeesser.SetActive(false);
    FDeesser.ResetUnit();
  end;

  procedure TNVDeesser.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      //////////////////
      Smp[j] := outp[j];
    end;
  end;

////////////////////////////////////

  constructor TNVLimiter.Create(AOwner: TComponent);
  begin
    inherited;
    FLimiter := TLimiterUnit.Create(AOwner);
  end;

  destructor TNVLimiter.Destroy;
  begin
    FLimiter.Free;
    inherited;
  end;

  procedure TNVLimiter.InitProc();
  begin
    if Assigned(FInput) then
      FLimiter.InitUnit(FInput.SampleRate, FInput.Channels)
    else FLimiter.InitUnit(44100, 2);
    FLimiter.SetActive(true);
  end;

  procedure TNVLimiter.ResetProc();
  begin
    FLimiter.SetActive(false);
    FLimiter.ResetUnit();
  end;
   
  procedure TNVLimiter.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      //////////////////
      Smp[j] := outp[j];
    end;
  end;

////////////////////////////////////

  constructor TNVReverb.Create(AOwner: TComponent);
  begin
    inherited;
    FReverb := TReverbUnit.Create(AOwner);
  end;

  destructor TNVReverb.Destroy;
  begin
    FReverb.Free;
    inherited;
  end;

  procedure TNVReverb.InitProc();
  begin
    if Assigned(FInput) then
      FReverb.InitUnit(FInput.SampleRate, FInput.Channels)
    else FReverb.InitUnit(44100, 2);
    FReverb.SetActive(true);
  end;  

  procedure TNVReverb.ResetProc();
  begin
    FReverb.SetActive(false);
    FReverb.ResetUnit();
  end;

  procedure TNVReverb.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      //////////////////
      Smp[j] := outp[j];
    end;
  end;
  
////////////////////////////////////

  constructor TNVParametricEQ.Create(AOwner: TComponent);
  begin
    inherited;
    FEqualizer := TParametricEQUnit.Create(AOwner);
  end;

  destructor TNVParametricEQ.Destroy;
  begin
    FEqualizer.Free;
    inherited;
  end;

  procedure TNVParametricEQ.InitProc();
  begin
    if Assigned(FInput) then
      FEqualizer.InitUnit(FInput.SampleRate, FInput.Channels)
    else FEqualizer.InitUnit(44100, 2);
    FEqualizer.SetActive(true);
  end;

  procedure TNVParametricEQ.ResetProc();
  begin
    FEqualizer.SetActive(false);
    FEqualizer.ResetUnit();
  end;

  procedure TNVParametricEQ.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      //////////////////
      Smp[j] := outp[j];
    end;
  end;

////////////////////////////////////

  constructor TNVGraphicEQ.Create(AOwner: TComponent);
  begin
    inherited;
    FEqualizer := TGraphicEQUnit.Create(AOwner);
  end;

  destructor TNVGraphicEQ.Destroy;
  begin
    FEqualizer.Free;
    inherited;
  end;

  procedure TNVGraphicEQ.InitProc();
  begin
    if Assigned(FInput) then
      FEqualizer.InitUnit(FInput.SampleRate, FInput.Channels)
    else FEqualizer.InitUnit(44100, 2);
    FEqualizer.SetActive(true);
  end;

  procedure TNVGraphicEQ.ResetProc();
  begin
    FEqualizer.SetActive(false);
    FEqualizer.ResetUnit();
  end;

  procedure TNVGraphicEQ.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to SamplesInFrame - 1 do
    begin
      outp[j] := Smp[j];
      //////////////////
      Smp[j] := outp[j];
    end;
  end;
  
////////////////////////////////////

  constructor TNVVoiceProcessor.Create(AOwner: TComponent);
  begin
    inherited;
    FInputMixer := TInputMixerUnit.Create(AOwner);
    FOutputMixer := TOutputMixerUnit.Create(AOwner);
    FParametricEQ := TParametricEQUnit.Create(AOwner);
    FGraphicEQ := TGraphicEQUnit.Create(AOwner);
    FCompressor := TCompressorUnit.Create(AOwner);
    FGate := TGateUnit.Create(AOwner);
    FDeesser := TDeesserUnit.Create(AOwner);
    FLimiter := TLimiterUnit.Create(AOwner);
    FReverb := TReverbUnit.Create(AOwner);
  end;

  destructor TNVVoiceProcessor.Destroy;
  begin
    FInputMixer.Free;
    FOutputMixer.Free;
    FParametricEQ.Free;
    FGraphicEQ.Free;
    FCompressor.Free;
    FGate.Free;
    FDeesser.Free;
    FLimiter.Free;
    FReverb.Free;
    inherited;
  end;

  procedure TNVVoiceProcessor.InitProc();
  var SR, CH : Integer;
  begin
    SR := FOutputMinSampleRate; //44100;
    CH := 2;
    if Assigned(FInput) then
    begin
      SR := FProcSampleRate;
      CH := FProcChannels;
    end;
    
    FInputMixer.InitUnit(SR, CH);
    FOutputMixer.InitUnit(SR, CH);
    FParametricEQ.InitUnit(SR, CH);
    FGraphicEQ.InitUnit(SR, CH);
    FCompressor.InitUnit(SR, CH);
    FGate.InitUnit(SR, CH);
    FDeesser.InitUnit(SR, CH);
    FLimiter.InitUnit(SR, CH);
    FReverb.InitUnit(SR, CH);

    FInputMixer.SetActive(true);
    FOutputMixer.SetActive(true);
    FParametricEQ.SetActive(true);
    FGraphicEQ.SetActive(true);
    FCompressor.SetActive(true);
    FGate.SetActive(true);
    FDeesser.SetActive(true);
    FLimiter.SetActive(true);
    FReverb.SetActive(true);

  end;

  procedure TNVVoiceProcessor.ResetProc();
  begin
    FInputMixer.SetActive(false);
    FOutputMixer.SetActive(false);
    FParametricEQ.SetActive(false);
    FGraphicEQ.SetActive(false);
    FCompressor.SetActive(false);
    FGate.SetActive(false);
    FDeesser.SetActive(false);
    FLimiter.SetActive(false);
    FReverb.SetActive(false);

    FInputMixer.ResetUnit();
    FOutputMixer.ResetUnit();
    FParametricEQ.ResetUnit();
    FGraphicEQ.ResetUnit();
    FCompressor.ResetUnit();
    FGate.ResetUnit();
    FDeesser.ResetUnit();
    FLimiter.ResetUnit();
    FReverb.ResetUnit();
  end;

  procedure TNVVoiceProcessor.ProcData;
  var
    Smp : TDblArray;
    outp : array [0..1] of Double;
    j : Integer;
  begin
    Smp := Sample;
    for j :=  0 to FProcChannels - 1 do
    begin
      outp[j] := Smp[j];
    end;
    if(FProcChannels = 1) then outp[1] := 0.0;
    ProcSample(outp[0], outp[1]);
    for j :=  0 to FProcChannels - 1 do
    begin
      Smp[j] := outp[j];
    end;
  end;

  procedure TNVVoiceProcessor.ProcSample(var sL : Double; var sR : Double);
  begin
    FInputMixer.ProcSample(sL, sR);
    FDeesser.ProcSample(sL, sR);
    FGate.ProcSample(sL, sR);
    FGraphicEQ.ProcSample(sL, sR);
    FParametricEQ.ProcSample(sL, sR);
    FCompressor.ProcSample(sL, sR);
    FLimiter.ProcSample(sL, sR);
    FReverb.ProcSample(sL, sR);
    FOutputMixer.ProcSample(sL, sR);
  end;

  procedure TNVVoiceProcessor.SetInputMixer(Value : TInputMixerUnit);
  begin
    if(FInputMixer <> Value) then
    begin
      FInputMixer.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetOutputMixer(Value : TOutputMixerUnit);
  begin
    if(FOutputMixer <> Value) then
    begin
      FOutputMixer.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetParametricEQ(Value : TParametricEQUnit);
  begin
    if(FParametricEQ <> Value) then
    begin
      FParametricEQ.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetGraphicEQ(Value : TGraphicEQUnit);
  begin
    if(FGraphicEQ <> Value) then
    begin
      FGraphicEQ.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetCompressor(Value : TCompressorUnit);
  begin
    if(FCompressor <> Value) then
    begin
      FCompressor.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetGate(Value : TGateUnit);
  begin
    if(FGate <> Value) then
    begin
      FGate.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetDeesser(Value : TDeesserUnit);
  begin
    if(FDeesser <> Value) then
    begin
      FDeesser.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetLimiter(Value : TLimiterUnit);
  begin
    if(FLimiter <> Value) then
    begin
      FLimiter.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SetReverb(Value : TReverbUnit);
  begin
    if(FReverb <> Value) then
    begin
      FReverb.Assign(Value);
      Changed();
    end;
  end;

  procedure TNVVoiceProcessor.SampleRateChanged;
  var SR, CH : Integer;
  begin
    SR := FOutputMinSampleRate; //44100;
    CH := 2;
    FInputMixer.InitUnit(SR, CH);
    FOutputMixer.InitUnit(SR, CH);
    FParametricEQ.InitUnit(SR, CH);
    FGraphicEQ.InitUnit(SR, CH);
    FCompressor.InitUnit(SR, CH);
    FGate.InitUnit(SR, CH);
    FDeesser.InitUnit(SR, CH);
    FLimiter.InitUnit(SR, CH);
    FReverb.InitUnit(SR, CH);
  end;

{$WARNINGS ON}

end.
