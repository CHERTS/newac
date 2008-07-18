(*
  This file is a part of New Audio Components package 1.7
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Converters;

(* Title: ACS_Converters
    Classes which convert bit depth, sample rates, and stereo/mono. *)

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, Windows, ACS_Types, ACS_Procs, ACS_Classes, _MSACM, MMSystem, Math;

const
  BUF_SIZE = $8000;

  KERNEL_WIDTH = 64;

  SD_BUF_SIZE = 2048;

  MaxAudioInput = 2048*6; // To handle 24 bps streams;

  KL = 15;

  // Noise shaping constants

  LPCoeff : array[0..KL-1] of Single = (0.903696, -0.474938, -0.189386, 0.298093, -0.0818197, 0.0645497, -0.153755, 0.139183, 0, 0, 0, 0, 0, 0, 0);
  LPCoeff1 : array[0..KL-1] of Single =(0.00650087, 0.0254105, -0.119435, 0.245838, -0.358504, 0.418265, -0.409635,
  0.91, -0.686898, 0.411852, -0.170389, 0.0206225, 0.0282275, -0.013329, -0.00536082);

type

  TDA = array[0..63] of Double;
  PDA = ^TDA;

 (* Class: TRateConverter
     Descends from <TAuConverter>. TRateConverter is an audio resampler.
     It uses windowed sinc kernel for changing audio sample rate and supports only 16-bit input.
     This component is kept for backward compatibility. In new projects it is recommended to use other resampler components instead.
  *)

  TRateConverter = class(TAuConverter)
  private
    FOutSampleRate : LongWord;
    WantedSize : LongWord;
    remainder : Integer;
    InBufM, OutBufM : PBuffer16;
    InBufS, OutBufS : PStereoBuffer16;
    DAM : array of Double;
    DAS : array of TStereoSampleD;
    Kernel : array of Double;
    FKernelWidth : LongWord;
    FFilterWindow : TFilterWindowType;
    Tail : Pointer;
    LBS : TStereoSample16;
    function ConvertFreqs16Mono(InSize : Integer): Integer;
    function ConvertFreqs16Stereo(InSize : Integer): Integer;
    procedure SetOutSampleRate(aSR : LongWord);
    procedure SetKernelWidth(aKW : LongWord);
  protected
    function GetBPS : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  published
    (* Property: FilterWindow
      Use this property to set the window type for the filter. *)
    property FilterWindow : TFilterWindowType read FFilterWindow write FFilterWindow;
    (* Property: KernelWidth
      Use this property to set the width of the sinc kernel in points. *)
    property KernelWidth : LongWord read FKernelWidth write SetKernelWidth;
    (* Property: OutSampleRate
      Use this property to set the output sample rate. *)
    property OutSampleRate : LongWord read FOutSampleRate write SetOutSampleRate;
  end;

  TStereoBalance = class(TAuConverter)
  private
    _Buffer : array[0..BUF_SIZE-1] of Byte;
    FBalance : Single;
    procedure SetBalance(a : Single);
  protected
    function GetCh : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Balance : Single read FBalance write SetBalance;
  end;

  TNormalizerStoreMode = (nsmFile, nsmMemory, nsmNone);

   (* Class: TNormalizer
     Descends from <TAuConverter>. TNormalizer scales audio data so that the highest peak in data is mapped to the maximum sample value avaiable. *)

  TNormalizer = class(TAuConverter)
  private
    TmpOutput : TStream;
    FStoreMode : TNormalizerStoreMode;
    _k : Double;
    _BytesPerSample : Word;
    InBuf : PBuffer8;
    _BufSize : LongWord;
    FTmpFileName : String;
    FEnabled, _Enabled : Boolean;
    procedure SetStoreMode(value : TNormalizerStoreMode);
    function GetSample(Buffer : PBuffer8; Count : Integer) : Int64;
    procedure SetSample(var Sample : Int64; Buffer : PBuffer8; Count : Integer);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: StoreMode
       TNormalizer needs a preprocessing data stage. Depending on the StoreMode value the component can work in several modes.
       - nsmFile - the intermediate data is stored in a disc file (see the <TmpFileName> property).
       - nsmMemory - the intermediate data is stored in RAM.
       - nsmNone - no intermediate data is stored. In this mode the component reads the data from its input twice. This mode works only for inputs that can reproduce its data (TSreamedIn descendants). *)
    property StoreMode : TNormalizerStoreMode read FStoreMode write SetStoreMode;
    (* Property: TmpFileName
       The name of the temporary file that stores intermediate data in the nsmFile mode.  *)
    property TmpFileName : String read FTmpFileName write FTmpFileName;
    (* Property: Enabled
       If this property is set to False the component passes data through without scaling.  *)
    property Enabled : Boolean read FEnabled write FEnabled;
  end;

  TDitheringAlgorithm = (dtaRectangular, dtaTriangular, dtaShaped1, dtaShaped2);

 (* Class: TDitherer
     Descends from <TAuConverter>. TDitherer converter adds dithering noise to the audio data passing through it.
     Dithering is usefull when performing audio samples truncation (with <TAudioConverter> for example) and other DSP operatins that involve rounding off.
     TDitherer uses seeveral dithering algorithms but none of them is perfect. I will be grateful for any dithering algorithms additions.
     If you want to use TDitherer together with <TAudioConverter> you should place it before <TAudioConverter> in the audio-processing chain. *)

  TDitherer = class(TAuConverter)
  private
    _BytesPerSample : Integer;
    FDitheringDepth : Word;
    FDitheringAlgorithm : TDitheringAlgorithm;
    TriState : array[0..7] of Integer;
    LPState : array[0..7] of array [0..kl] of Integer;
    function GetSample(Buffer : PBuffer8; Count : Integer) : Int64;
    procedure SetSample(var Sample : Int64; Buffer : PBuffer8; Count : Integer);
    procedure SetDitheringDepth(value : Word);
    procedure SetDitheringAlgorithm(value : TDitheringAlgorithm);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: DitheringDepth
    Use this property to set the number of bits that will contain the dithering noise.
    For example, if you want to truncate audio samples from 24 bits per sample to 16 bits, set this value to 8.
    If the DitheringDepth value is set to 0 (the default  value), the component switches to pass-through mode
    in which audio data will be passed through unmodified. *)
    property DitheringDepth : Word read FDitheringDepth write SetDitheringDepth;
    (* Property: DitheringAlgorithm
    Use this property to set the dithering algorithm. Possible values are:
    - dtaRectangular (rectangle noise)
    - dtaTriangular (triangle noise)
    - dtaShaped1
    - dtaShaped2 *)
    property DitheringAlgorithm : TDitheringAlgorithm read FDitheringAlgorithm write SetDitheringAlgorithm;
  end;


  TFastResampler = class(TAuConverter)
  private
    A, B : array of Single;
    X, Y : array[0..7] of array of Single;
    IFrames, OFrames, ISize, OSize, MaxSize, MaxFrames : LongWord;
    FOutSampleRate : Word;
    offsX, OffsY : Integer;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OutSampleRate : Word read FOutSampleRate write FOutSampleRate;
  end;




  (* Class: TAudioConverter
     Descends from <TAuConverter>. TAudioConverter component may be used for
     changing the number of channels and number of bits per sample in an audio
     stream.*)

  TAudioConverter = class(TAuConverter)
  private
    OutBuf : PBuffer8;
    FloatBuf : PBufferSingle;
    OutFrames : Integer;
    InBytesPerSample, OutBytesPerSample : Integer;
    FMode : TMSConverterMode;
    FOutBitsPerSample : Integer;
    FOutChannels : Integer;
    ICh, IBPS : Integer;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Mode
      Mode property affects the way the mono stream is converted into stereo.
      If mode is msmMonoToBoth (which is the default) the data from the mono
      channel is copied into both stereo channels. If the mode is
      msmMonoToLeft or msmMonoToRight the data from the mono channel is put to
      one of the stereo channels while the other channel plays silence.*)
    property Mode : TMSConverterMode read FMode write FMode;
    (* Property: OutBitsPerSample
       Use this property to set the number of bits per sample in the resulting
       audio stream. The valid values are 0, 8, 16, 24, and 32. If the property is set
       to 0 (the default) the converter preserves the original stream's number
       of bits per sample.*)
    property OutBitsPerSample : Integer read FOutBitsPerSample write FOutBitsPerSample;
    (* Property: OutChannels
       Use this property to set the number of channels in the resulting audio
       stream. The valid values are 0,  1 (mono), and 2 (stereo). If the
       property is set to 0 (the default value) the converter preserves the
       original stream's number of channels. If the number of input channels is greater than 2
       the component cannot change it. Only number of bits per sample can be changed in this case. *)
    property OutChannels : Integer read FOutChannels write FOutChannels;
  end;

  (* Class: TACMConverter
     Descends from <TAuConverter>, an ACM-based converter. It may be used for
     changing the number of channels and number of bits per sample in an audio
     stream and resampling audio (at low quality). Unlike <TAudioConverter>,
     TACMConverter doesn't work with 24 bps audio streams. *)

  TACMConverter = class(TAuConverter)
  private
    FOutBitsPerSample : Integer;
    FOutChannels : Integer;
    FOutSampleRate : Integer;
    ICh, IBPS, ISR : Integer;
    _Stream : HACMStream;
    IBufSize, OBufSize : LongWord;
    IBuf, OBuf : PBuffer8;
    OBufStart, OBufEnd : LongWord;
    EndOfInput, Prepared : Boolean;
    _Header : ACMSTREAMHEADER;
    procedure GetDataBlock(StartFrom : LongWord);
    procedure RegetDataBlock;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: OutBitsPerSample
       Use this property to set the number of bits per sample in the resulting
       audio stream. The valid values are 0, 8, 16. If the property is set to
       0 (the default) the converter preserves the original stream's number of
       bits per sample.*)
    property OutBitsPerSample : Integer read FOutBitsPerSample write FOutBitsPerSample;
    (* Property: OutChannels
       Use this property to set the number of channels in the resulting audio
       stream. The valid values are 0, 1 (mono), and 2 (stereo). If the
       property is set to 0 (the default value) the converter preserves the
       original stream's number of channels.*)
    property OutChannels : Integer read FOutChannels write FOutChannels;
    (* Property: OutSampleRate
       Use this property to set thesample rate of the resulting audio stream. The valid values are 0 and something between 2000-12000. If the property is set to 0 (the default value) the converter preserves the original stream's sample rate. *)
    property OutSampleRate : Integer read FOutSampleRate write FOutSampleRate;
  end;


implementation

  function SHGetFolderPathA(hwndOwner : HWND; nFolder : Integer; hToken : THANDLE; dwFlags : DWORD; pszPath : PChar) : HResult; stdcall; external 'shell32.dll';

  function TRateConverter.ConvertFreqs16Mono(InSize : Integer): Integer;
  var
    i, step, j, k, s, m : Integer;
    D : Double;
    TailMono : PBuffer16;
    TailMonoD : PDoubleArray;
  begin
    TailMono := Tail;
    s := InSize shr 1;
    if FInput.SampleRate > FOutSampleRate then
    begin
      step := FInput.SampleRate - FOutSampleRate;
      j := 0;
      if remainder < 0 then remainder := FOutSampleRate;
      for i := 0 to s - 1 do
      begin
        if remainder > FOutSampleRate then Dec(remainder, FOutSampleRate)
        else begin
          D := 0;
          for k := 0 to FKernelWidth - 1 do
          if i-k >= 0 then
          D := D + InBufM[i-k]*Kernel[FKernelWidth - 1 - k]
          else
          D := D + TailMono[FKernelWidth-1+i-k]*Kernel[FKernelWidth - 1 - k];
          OutBufM[j] := Round(D);
          Inc(j);
          Inc(remainder, step);
        end;
      end;
      for i := 0 to FKernelWidth-2 do TailMono[i] := InBufM[i+s-FKernelWidth+1]
    end else
    begin
      TailMonoD := Tail;
      FillChar(DAM[0], Length(DAM)*8, 0);
      for i := 0 to FKernelWidth-2 do
      begin
        DAM[i] := TailMonoD[i];
        TailMonoD[i] := 0;
      end;
      Step := Finput.SampleRate;
      j := 0;
      if remainder < 0 then remainder := 0;
      while remainder < FOutSampleRate do
      begin
        m := Round(((FOutSampleRate - remainder)*LBS.Left +  remainder*InBufM[0])/FOutSampleRate);
        for k := 0 to FKernelWidth-1 do
        DAM[j+k] := DAM[j+k] + m*Kernel[k];
        Inc(j);
        Inc(remainder, step);
      end;
      Dec(remainder, FOutSampleRate);
      for i := 0 to s - 2 do
      begin
        while remainder < FOutSampleRate do
        begin
          m := Round(((FOutSampleRate - remainder)*InBufM[i] +  remainder*InBufM[i+1])/FOutSampleRate);
          for k := 0 to FKernelWidth-1 do
          DAM[j+k] := DAM[j+k] + m*Kernel[k];
          Inc(j);
          Inc(remainder, step);
        end;
        Dec(remainder, FOutSampleRate);
      end;
      LBS.Left := InBufM[s-1];
      for i := 0 to j-1 do
      OutBufM[i] := Round(DAM[i]);
      for i := 0 to FKernelWidth-2 do TailMonoD[i] := DAM[i+j];
    end;
    Result := j shl 1;
  end;

  function TRateConverter.ConvertFreqs16Stereo(InSize : Integer): Integer;
  var
    i, step, j, k, s, m1, m2 : Integer;
    D1, D2 : Double;
    TailStereo : PStereoBuffer16;
    TailStereoD : PStereoBufferD;
  begin
    TailStereo := Tail;
    s := InSize shr 2;
    if FInput.SampleRate > FOutSampleRate then
    begin
      step := FInput.SampleRate - FOutSampleRate;
      j := 0;
      if remainder < 0 then remainder := FOutSampleRate;
      for i := 0 to s - 1 do
      begin
        if remainder > FOutSampleRate then Dec(remainder, FOutSampleRate)
        else begin
          D1 := 0;
          D2 := 0;
          for k := 0 to FKernelWidth - 1 do
          if i-k >= 0 then
          begin
            D1 := D1 + InBufS[i-k].Left*Kernel[FKernelWidth - 1 - k];
            D2 := D2 + InBufS[i-k].Right*Kernel[FKernelWidth - 1 - k];
          end else
          begin
            D1 := D1 + TailStereo[FKernelWidth-1+i-k].Left*Kernel[FKernelWidth - 1 - k];
            D2 := D2 + TailStereo[FKernelWidth-1+i-k].Right*Kernel[FKernelWidth - 1 - k];
          end;
          OutBufS[j].Left := Round(D1);
          OutBufS[j].Right := Round(D2);
          Inc(j);
          Inc(remainder, step);
        end;
      end;
      for i := 0 to FKernelWidth-2 do TailStereo[i] := InBufS[i+s-FKernelWidth+1]
      //Move(InBufS[s-FKernelWidth+1], TailStereo[0], FKernelWidth-1);
    end else
    begin
      TailStereoD := Tail;
      FillChar(DAS[0], Length(DAS)*16, 0);
      for i := 0 to FKernelWidth-2 do
      begin
        DAS[i] := TailStereoD[i];
        TailStereoD[i].Left := 0;
        TailStereoD[i].Right := 0;
      end;
      Step := Finput.SampleRate;
      j := 0;
      if remainder < 0 then remainder := 0;
      while remainder < FOutSampleRate do
      begin
        m1 := Round(((FOutSampleRate - remainder)*LBS.Left +  remainder*InBufS[0].Left)/FOutSampleRate);
        m2 := Round(((FOutSampleRate - remainder)*LBS.Right +  remainder*InBufS[0].Right)/FOutSampleRate);
        for k := 0 to FKernelWidth-1 do
        begin
          DAS[j+k].Left := DAS[j+k].Left + m1*Kernel[k]; //InBufS[i].Left*Kernel[k];
          DAS[j+k].Right := DAS[j+k].Right + m2*Kernel[k]; //InBufS[i].Right*Kernel[k];
        end;
        Inc(j);
        Inc(remainder, step);
      end;
      Dec(remainder, FOutSampleRate);
      for i := 0 to s - 2 do
      begin
        while remainder < FOutSampleRate do
        begin
          m1 := Round(((FOutSampleRate - remainder)*InBufS[i].Left +  remainder*InBufS[i+1].Left)/FOutSampleRate);
          m2 := Round(((FOutSampleRate - remainder)*InBufS[i].Right +  remainder*InBufS[i+1].Right)/FOutSampleRate);
          for k := 0 to FKernelWidth-1 do
          begin
           DAS[j+k].Left := DAS[j+k].Left + m1*Kernel[k]; //InBufS[i].Left*Kernel[k];
           DAS[j+k].Right := DAS[j+k].Right + m2*Kernel[k]; //InBufS[i].Right*Kernel[k];
          end;
          Inc(j);
          Inc(remainder, step);
        end;
        Dec(remainder, FOutSampleRate);
      end;
      LBS := InBufS[s-1];
      for i := 0 to j-1 do
      begin
        OutBufS[i].Left := Round(DAS[i].Left);
        OutBufS[i].Right := Round(DAS[i].Right);
      end;
      for i := 0 to FKernelWidth-2 do TailStereoD[i] := DAS[i+j];
    end;
    Result := j shl 2;
  end;

  function GCD(a, b : Integer) : Integer;
  var
    p, q, r : Integer;
  begin
    p := a;
    q := b;
    r := p mod q;
    while r <> 0 do
    begin
      p := q;
      q := r;
      r := p mod q;
    end;
    Result := q;
  end;

  constructor TRateConverter.Create;
  begin
    inherited Create(AOwner);
    FOutSampleRate := 22050;
    FKernelWidth := 30;
    FFilterWindow := fwBlackman;
  end;

  destructor TRateConverter.Destroy;
  begin
    Kernel := nil;
    DAS := nil;
    DAM := nil;
    inherited Destroy;
  end;

  function TRateConverter.GetBPS;
  begin
    Result := 16;
  end;

  function TRateConverter.GetSR;
  begin
    Result := FOutSampleRate;
  end;

  procedure TRateConverter.InitInternal;
  var
    Ratio : Single;
    TailSize : Integer;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    if FInput.BitsPerSample <> 16 then
      raise EAuException.Create('This resampler can hanndle 16 bps input only');
    Busy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    Ratio := FOutSampleRate/Finput.SampleRate;
    if Ratio > 1. then
    WantedSize := (Trunc(BUF_SIZE/Ratio) shr 2) * 4
    else WantedSize := BUF_SIZE;
    if Finput.Channels = 1  then
    begin
      GetMem(InBufM, WantedSize);
      GetMem(OutBufM, BUF_SIZE);
      if Ratio < 1. then
      TailSize := (KernelWidth-1)*2
      else
      begin
        SetLength(DAM, (BUF_SIZE div 2)+KernelWidth);
        TailSize := (KernelWidth-1)*8;
      end;
      FillChar(DAM[0], Length(DAM)*Sizeof(DAM[0]), 0);
    end else
    begin
      GetMem(InBufS, WantedSize);
      GetMem(OutBufS, BUF_SIZE);
      if Ratio < 1. then
      TailSize := (KernelWidth-1)*4
      else
      begin
        SetLength(DAS, (BUF_SIZE div 4)+KernelWidth);
        TailSize := (KernelWidth-1)*16;
      end;
    end;
    GetMem(Tail, TailSize);
    FillChar(Tail^, TailSize, 0);
    FSize := Round(FInput.Size*Ratio);
    remainder := -1;
    if Ratio > 1. then Ratio := 1/Ratio;
    Ratio := Ratio*0.4;
    SetLength(Kernel, FKernelWidth);
    CalculateSincKernel(@Kernel[0], Ratio, FKernelWidth, FFilterWindow);
  end;

  procedure TRateConverter.FlushInternal;
  begin
    FreeMem(Tail);
    FInput.Flush;
    if Finput.Channels = 1  then
    begin
      FreeMem(InBufM);
      FreeMem(OutBufM);
    end else
    begin
      FreeMem(InBufS);
      FreeMem(OutBufS);
    end;
    Busy := False;
  end;

  procedure TRateConverter.GetDataInternal;
  var
    l : Integer;
    InSize : Integer;
    P : PBuffer8;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      if FInput.Channels = 1 then P := Pointer(InBufM)
      else P := Pointer(InBufS);
      l := Finput.CopyData(@P[0], WantedSize);
      if l = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        l := Finput.CopyData(@P[InSize], WantedSize - InSize);
        Inc(InSize, l);
      end;
//      if l = 0 then _EndOfStream := True;
      if Self.Channels = 1 then
      begin
        BufEnd := ConvertFreqs16Mono(InSize);
      end else
      begin
        BufEnd := ConvertFreqs16Stereo(InSize);
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    if FInput.Channels = 1 then P := Pointer(OutBufM)
    else P := Pointer(OutBufS);
    Buffer := @P[BufStart-1];
    Inc(BufStart, Bytes);
//    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TRateConverter.SetOutSampleRate(aSR : LongWord);
  begin
    if (aSR > 0) and (not Busy) then FOutSampleRate := aSR;
  end;

  procedure TRateConverter.SetKernelWidth;
  begin
    if (aKW > 1) and (not Busy) then FKernelWidth := aKW;
  end;

  constructor TStereoBalance.Create;
  begin
    inherited Create(AOwner);
    FBalance := 0.5;
  end;

  destructor TStereoBalance.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TStereoBalance.SetBalance;
  begin
    if (a >= 0) and (a <=1) then FBalance := a;
  end;

  function TStereoBalance.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    if FInput.Channels <= 2 then
      Result := 2
    else
      Result := FInput.Channels;
  end;

  procedure TStereoBalance.InitInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    if FInput.Channels <= 2 then
    begin
      if FInput.Channels = 2 then FSize := FInput.Size
      else FSize := FInput.Size*2;
    end
    else
      FSize := FInput.Size;
    FPosition := 0;
  end;

  procedure TStereoBalance.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  procedure TStereoBalance.GetDataInternal;
  var
    WantedSize, i : LongWord;
    P16 : PBuffer16;
    P8 : PBuffer8;
    P24 : PBuffer24;
    I1 : Integer;
    Diff : Double;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if FInput.Channels > 2 then
    begin
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if FInput.Channels = 2 then
    begin
      WantedSize := Bytes;
      Finput.GetData(Buffer, WantedSize);
    end  
    else
    begin
      if Bytes > BUF_SIZE then Bytes := BUF_SIZE;
      WantedSize := Bytes shr 1;
      WantedSize := Finput.CopyData(@_Buffer[0], WantedSize);
    end;
    if WantedSize = 0 then
    begin
      Bytes := 0;
      Buffer := nil;
      Exit;
    end;
    if FInput.Channels = 1 then
    begin
      if FInput.BitsPerSample = 8 then
      begin
        P8 := @_Buffer[0];
        for i := WantedSize*2-1 downto 1 do P8[i] := P8[i shr 1];
      end else
      if FInput.BitsPerSample = 16 then
      begin
        P16 := @_Buffer[0];
        for i := WantedSize-1 downto 1 do
        P16[i] := P16[i shr 1];
      end else
      begin
        P24 := @_Buffer[0];
        for i := (WantedSize*2 div 3)-1 downto 1 do P24[i] := P24[i shr 1];
      end;
      WantedSize := WantedSize*2;
      Buffer := @_Buffer[0];
    end;
    if FInput.BitsPerSample = 8 then
    begin
      P8 := Buffer;
      if FBalance > 0.5 then
      begin
        Diff := 1-Balance;
        for i := 0 to (WantedSize shr 1) -1 do
        P8[i*2] := Round(P8[i*2]*Diff);
      end else
      begin
        for i := 0 to (WantedSize shr 1) -1 do
        P8[i*2+1] := Round(P8[i*2+1]*FBalance);
      end;
    end else
    if FInput.BitsPerSample = 16 then
    begin
      P16 := Buffer;
      if FBalance > 0.5 then
      begin
        Diff := 1-Balance;
        for i := 0 to (WantedSize shr 2) -1 do
        P16[i*2] := Round(P16[i*2]*Diff);
      end else
      begin
        for i := 0 to (WantedSize shr 2) -1 do
        P16[i*2+1] := Round(P16[i*2+1]*FBalance);
      end;
    end else
    begin
      P24 := Buffer;
      if FBalance > 0.5 then
      begin
        Diff := 1-Balance;
        for i := 0 to (WantedSize div 6) -1 do
        begin
          I1 := 0;
          Move(P24^[i*2], I1, 3);
          I1 := Round(I1*Diff);
          Move(I1, P24^[i*2], 3);
        end;
      end else
      begin
        for i := 0 to (WantedSize div 6) -1 do
        begin
          I1 := 0;
          Move(P24^[i*2 + 1], I1, 3);
          //I1 := Round(I1*FBalance);
          Move(I1, P24^[i*2 + 1], 3);
        end;
      end;
    end;
    Bytes := WantedSize;
    //FPosition := Round(FSize/FInput.Size)*FInput.Position;
  end;

  constructor TAudioConverter.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TAudioConverter.Destroy;
  begin
    inherited Destroy;
  end;

  function TAudioConverter.GetBPS;
  begin
    if FOutBitsPerSample <> 0 then
      Result := FOutBitsPerSample
    else
      if Assigned(FInput) then
        Result := FInput.BitsPerSample
      else
        Result := 0;
  end;

  function TAudioConverter.GetCh;
  begin
    if not Assigned(FInput) then
    begin
      Result := 0;
      Exit;
    end;
    if (FOutChannels in [1..2]) and (FInput.Channels in [1..2]) then
      Result := FOutChannels
    else
      Result := FInput.Channels;
  end;

  function TAudioConverter.GetSR;
  begin
    if Assigned(FInput) then
      Result := FInput.SampleRate
    else
      Result := 0;
  end;

  procedure TAudioConverter.InitInternal;
  var
    k, k1 : LongWord;
//    OutSamples : Integer;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned');
    FInput.Init;
    Busy := True;
    FPosition := 0;
    BufStart := 0;
    BufEnd := 0;
    IBPS := GetBPS;
    ICh := GetCh;
    k := FInput.BitsPerSample*FInput.Channels;
    k1 := IBPS*ICh;
    InBytesPerSample := FInput.BitsPerSample div 8;
    OutBytesPerSample := IBPS div 8;
    if FInput.Size >= 0 then
    begin
      FSize := Round(FInput.Size*k1/k);
      FSize := FSize - (FSize mod (k1 shr 3));
    end else
      FSize := -1;
    OutFrames := 0;
    OutBuf := nil;
    FloatBuf := nil;
  end;

  procedure TAudioConverter.FlushInternal;
  begin
    FInput.Flush;
    if OutBuf <> nil then FreeMem(OutBuf);
    if FloatBuf <> nil then FreeMem(FloatBuf);    
    Busy := False;
  end;

  procedure TAudioConverter.GetDataInternal;
  var
    SamplesReq : Integer;
    FramesReq : Integer;
    BytesReq : LongWord;
    B : Pointer;
  begin
    if (FOutBitsPerSample = 0) and (FOutChannels = 0) then
    begin
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    SamplesReq := Bytes div OutBytesPerSample;
    FramesReq := SamplesReq div ICh;
    BytesReq := FramesReq*InBytesPerSample*Finput.Channels;
    FInput.GetData(B, BytesReq);
    if BytesReq = 0 then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    SamplesReq := BytesReq div InBytesPerSample;
    FramesReq := SamplesReq div Finput.Channels;
    Bytes := FramesReq*OutBytesPerSample*ICh;
    if FramesReq > OutFrames then
    begin
      if OutBuf <> nil then FreeMem(OutBuf);
      if FloatBuf <> nil then FreeMem(FloatBuf);
      OutFrames := FramesReq;
      GetMem(OutBuf, OutFrames*OutBytesPerSample*ICh);
      if ICh > FInput.Channels then
        GetMem(FloatBuf, OutFrames*SizeOf(Single)*ICh)
      else
        GetMem(FloatBuf, OutFrames*SizeOf(Single)*FInput.Channels);
    end;
    if Finput.BitsPerSample = 8 then
      ByteToSingle(B, FloatBuf, FramesReq*FInput.Channels)
    else
    if Finput.BitsPerSample = 16 then
      SmallIntToSingle(B, FloatBuf, FramesReq*FInput.Channels)
    else
    if Finput.BitsPerSample = 24 then
      Int24ToSingle(B, FloatBuf, FramesReq*FInput.Channels)
    else
    if Finput.BitsPerSample = 32 then
      Int32ToSingle(B, FloatBuf, FramesReq*FInput.Channels);
    if (ICh = 2) and (FInput.Channels = 1) then
      SingleMonoToStereo(FloatBuf, FramesReq)
    else
    if (ICh = 1) and (FInput.Channels = 2) then
      SingleStereoToMono(FloatBuf, FramesReq);
    if IBPS = 8 then
      SingleToByte(FloatBuf, OutBuf, FramesReq*ICh)
    else
    if IBPS = 16 then
      SingleToSmallInt(FloatBuf, Pointer(OutBuf), FramesReq*ICh)
    else
    if IBPS = 24 then
      SingleToInt24(FloatBuf, OutBuf, FramesReq*ICh)
    else
    if IBPS = 32 then
      SingleToInt32(FloatBuf, Pointer(OutBuf), FramesReq*ICh);
    Buffer := OutBuf;
  end;


  constructor TACMConverter.Create;
  begin
    inherited Create(AOwner);
    IBufSize := 3145728; // 3 megabytes
  end;

  destructor TACMConverter.Destroy;
  begin
    inherited Destroy;
  end;

  function TACMConverter.GetBPS;
  begin
    if FOutBitsPerSample = 0 then
      if Assigned(FInput) then
         FOutBitsPerSample := FInput.BitsPerSample;
    Result := FOutBitsPerSample;
  end;

  function TACMConverter.GetCh;
  begin
    if FOutChannels = 0 then
      if Assigned(FInput) then
         FOutChannels := FInput.Channels;
    Result := FOutChannels;
  end;

  function TACMConverter.GetSR;
  begin
    if FOutSampleRate = 0 then
      if Assigned(FInput) then
         FOutSampleRate := FInput.SampleRate;
    Result := FOutSampleRate;
  end;

  procedure TACMConverter.GetDataBlock(StartFrom : LongWord);
  var
    Len : LongWord;
  begin
    _Header.cbStruct := SizeOf(_Header);
    _Header.fdwStatus := 0;
    _Header.dwUser := 0;
    Len := FInput.FillBuffer(@IBuf[StartFrom], IBufSize - StartFrom, EndOfInput);
    _Header.pbSrc := PByte(IBuf);
    _Header.cbSrcLength := Len + StartFrom;
    _Header.cbSrcLengthUsed := 0;
    _Header.dwSrcUser := 0;
    _Header.pbDst := PByte(OBuf);
    _Header.cbDstLength := OBufSize;
    _Header.cbDstLengthUsed := 0;
    _Header.dwDstUser := 0;
    acmStreamPrepareHeader(_Stream, _Header, 0);
    if EndOfInput = False then
      acmStreamConvert(_Stream, _Header, ACM_STREAMCONVERTF_BLOCKALIGN)
    else
      acmStreamConvert(_Stream, _Header, 0);
    OBufStart := 0;
    OBufEnd := _Header.cbDstLengthUsed;
    Prepared := True;
  end;

  procedure TACMConverter.RegetDataBlock;
  var
    StartFrom : LongWord;
  begin
    StartFrom := _Header.cbSrcLength - _Header.cbSrcLengthUsed;
    Move(IBuf[_Header.cbSrcLengthUsed], IBuf[0], StartFrom);
    if Prepared then
    begin
      acmStreamUnprepareHeader(_Stream, _Header, 0);
      Prepared := False;
    end;
    if (not EndOFInput) or (StartFrom <> 0) then
    GetDataBlock(StartFrom);
  end;

  procedure TACMConverter.InitInternal;
  var
    IBytesPerSample, OBytesPerSample, ik, ok : Integer;
    SrcWave, DestWave : TWaveFormatEx;
    res : Integer;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    Busy := True;
    FInput.Init;
    FPosition := 0;
    ICh := FInput.Channels;
    IBPS := FInput.BitsPerSample;
    ISR := FInput.SampleRate;
    IBytesPerSample := ICh*IBPS div 8;
    OBytesPerSample := BitsPerSample*Channels div 8;
    ik := IBytesPerSample*ISR;
    ok := OBytesPerSample*SampleRate;
    FSize := Round(FInput.Size*ok/ik);
    SrcWave.wFormatTag := 1;
    SrcWave.nChannels := ICh;
    SrcWave.nSamplesPerSec := ISR;
    SrcWave.nAvgBytesPerSec := ik;
    SrcWave.nBlockAlign := IBytesPerSample;
    SrcWave.wBitsPerSample := IBPS;
    SrcWave.cbSize := 0;
    DestWave.wFormatTag := 1;
    DestWave.nChannels := OutChannels;
    DestWave.nSamplesPerSec := SampleRate;
    DestWave.nAvgBytesPerSec := ok;
    DestWave.nBlockAlign := OBytesPerSample;
    DestWave.wBitsPerSample := OutBitsPerSample;
    DestWave.cbSize := 0;
    res := acmStreamOpen(_Stream, nil, SrcWave, DestWave, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME);
    if res <> 0 then
      raise EAuException.Create('Failed to set up converter ' + IntTOStr(res));
    acmStreamSize(_Stream, IBufSize, OBufSize, ACM_STREAMSIZEF_SOURCE);
    GetMem(IBuf, IBufSize);
    GetMem(OBuf, OBufSize);
    EndOfInput := False;
    Prepared := False;
  end;

  procedure TACMConverter.FlushInternal;
  begin
    FInput.Flush;
    acmStreamClose(_Stream, 0);
    FreeMem(IBuf);
    FreeMem(OBuf);
    Busy := False;
  end;

  procedure TACMConverter.GetDataInternal;
  begin
    if OBufStart >= OBufEnd then
    begin
      RegetDataBlock;
      if OBufStart >= OBufEnd then
      begin
        if FPosition < FSize then
        begin
          OBufEnd := FSize - FPosition;
          FillChar(OBuf[0], OBufEnd, 0);
          OBufStart := 0;
        end else
        begin
          Buffer := nil;
          Bytes := 0;
          Exit;
        end;
      end; // if OBufStart >= OBufEnd then
    end; // if OBufStart >= OBufEnd then
    if Bytes > OBufEnd - OBufStart then Bytes := OBufEnd - OBufStart;
    Buffer := @OBuf[OBufStart];
    Inc(OBufStart, Bytes);
  end;

  constructor TDitherer.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TDitherer.Destroy;
  begin
    inherited;
  end;

  procedure TDitherer.SetDitheringDepth;
  begin
    if not Busy then FDitheringDepth := value;
  end;

  procedure TDitherer.SetDitheringAlgorithm;
  begin
    if not Busy then FDitheringAlgorithm := value;
  end;

  procedure TDitherer.InitInternal;
  var
    i, j : Integer;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Busy := True;
    FInput.Init;
    FPosition := 0;
    FSize := FInput.Size;
    _BytesPerSample := GetBPS div 8;
    Randomize;
    if FDitheringAlgorithm = dtaTriangular then
      for i := 0 to 7 do TriState[i] := Round((Random - 0.5) * 2 * (1 shl FDitheringDepth));
    if FDitheringAlgorithm in [dtaShaped1, dtaShaped2] then
      for i := 0 to 7 do
        for j := 0 to kl do
          LPState[i][j] := 0;
  end;

  function TDitherer.GetSample;
  begin
    case _BytesPerSample of
      1 : Result := Buffer[Count];
      2 : Result := PSmallInt(@Buffer[Count*2])^;
      3 : Result := (PSmallInt(@Buffer[Count*3 + 1])^ * 256) + ShortInt(Buffer[Count*3]);
      4 : Result := PInteger(@Buffer[Count*4])^;
    end;
  end;

  procedure TDitherer.SetSample;
  begin
    if Sample > Integer(((1 shl (_BytesPerSample*8-1))-1)) then
      Sample := Integer((1 shl (_BytesPerSample*8-1))-1);
    if Sample < Integer(-(1 shl (_BytesPerSample*8-1))) then
      Sample := Integer(-(1 shl (_BytesPerSample*8-1)));
    case _BytesPerSample of
      1 : Buffer[Count] := Sample;
      2 : PSmallInt(@Buffer[Count*2])^ := Sample;
      3 :
      begin
        Buffer[Count*3] := Sample and $ff;
        PSmallInt(@Buffer[Count*3 + 1])^ := SmallInt(Sample shr 8);
      end;
      4 : PInteger(@Buffer[Count*4])^ := Sample;
    end;
  end;

  procedure TDitherer.GetDataInternal;
  var
    i, j, k, samples, cursample, ch, frames, Noise : Integer;
    Sample : int64;
    //OldSample : int64;
    X : Single;
  begin
    FInput.GetData(Buffer, Bytes);
    if FDitheringDepth = 0 then
      Exit;
    cursample := 0;
    samples := Bytes div _BytesPerSample;
    ch := FInput.Channels;
    frames := samples div ch;
    case FDitheringAlgorithm of
      dtaRectangular:
      begin
        for i := 0 to frames -1 do
          for j := 0 to ch - 1 do
          begin
            Sample := GetSample(Buffer, cursample);
            Noise := Round((Random - 0.5) * 2 * (1 shl FDitheringDepth));
            Sample := Sample + Noise;
            SetSample(Sample, Buffer, cursample);
            Inc(cursample);
          end;
      end;
      dtaTriangular:
      begin
        for i := 0 to frames -1 do
          for j := 0 to ch - 1 do
          begin
            Sample := GetSample(Buffer, cursample);
            Noise := Round((Random - 0.5) * 2 * (1 shl FDitheringDepth));
            Sample := Sample + Noise - TriState[j];
            TriState[j] := Noise;
            SetSample(Sample, Buffer, cursample);
            Inc(cursample);
          end;
      end;
      dtaShaped1:
      begin
        for i := 0 to frames -1 do
          for j := 0 to ch - 1 do
          begin
            Sample := GetSample(Buffer, cursample);
            Noise := Round((Random - 0.5) * 2 * (1 shl FDitheringDepth)); //+ Round((Random - 0.5) * 2 * (1 shl FDitheringDepth));
            X := 0;
            for k := 0 to kl do
              X := X + LPState[j][k]*LPcoeff[k];
//            OldSample := Sample;
//            if X > 1 shl FDitheringDepth then
//            X := 1 shl FDitheringDepth
//            else
//            if X < -(1 shl FDitheringDepth) then
//            X := -(1 shl FDitheringDepth);
            Sample := Round(Sample + X); //LPState[j][0]*LPcoeff[0] + LPState[j][1]*LPcoeff[1] + LPState[j][2]*LPcoeff[2] + LPState[j][3]*LPcoeff[3] + LPState[j][4]*LPcoeff[4]);
            SetSample(Sample, Buffer, cursample);
            for k := kl downto 1 do LPState[j][k] := LPState[j][k-1];
            LPState[j][0] := Noise;
            Inc(cursample);
          end;
      end;
      dtaShaped2:
      begin
        for i := 0 to frames -1 do
          for j := 0 to ch - 1 do
          begin
            Sample := GetSample(Buffer, cursample);
            Noise := Round((Random - 0.5) * 2 * (1 shl FDitheringDepth));
            X := 0;
            for k := 0 to kl do
              X := X + LPState[j][k]*LPcoeff1[k];
            Sample := Round(Sample + X);
            SetSample(Sample, Buffer, cursample);
            for k := kl downto 1 do LPState[j][k] := LPState[j][k-1];
            LPState[j][0] := Noise;
            Inc(cursample);
          end;
      end;
    end;
  end;

  procedure TDitherer.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  constructor TNormalizer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FTmpFileName := 'norm.tmp';
    FEnabled := True;
  end;

  destructor TNormalizer.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TNormalizer.SetStoreMode;
  begin
    if not Busy then
      FStoreMode := value;
  end;

  function TNormalizer.GetSample;
  begin
    case _BytesPerSample of
      1 : Result := Buffer[Count];
      2 : Result := PSmallInt(@Buffer[Count*2])^;
      3 : Result := (PSmallInt(@Buffer[Count*3 + 1])^ * 256) + ShortInt(Buffer[Count*3]);
      4 : Result := PInteger(@Buffer[Count*4])^;
    end;
  end;

  procedure TNormalizer.SetSample;
  begin
    if Sample > Integer(((1 shl (_BytesPerSample*8-1))-1)) then
      Sample := Integer((1 shl (_BytesPerSample*8-1))-1);
    if Sample < Integer(-(1 shl (_BytesPerSample*8-1))) then
      Sample := Integer(-(1 shl (_BytesPerSample*8-1)));
    case _BytesPerSample of
      1 : Buffer[Count] := Sample;
      2 : PSmallInt(@Buffer[Count*2])^ := Sample;
      3 :
      begin
        Buffer[Count*3] := Sample and $ff;
        PSmallInt(@Buffer[Count*3 + 1])^ := SmallInt(Sample shr 8);
      end;
      4 : PInteger(@Buffer[Count*4])^ := Sample;
    end;
  end;

  procedure TNormalizer.InitInternal;
  var
    sc, i : Integer;
    Buf : Pointer;
    Bytes : LongWord;
    MaxVal, MaxSample, TmpSample : Int64;
    FullName : String;
    UserPath : array[0..512] of Char;

  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Busy := True;
    FInput.Init;
    FPosition := 0;
    FSize := FInput.Size;
    _Enabled := FEnabled;
    if not _Enabled then Exit;
    _BytesPerSample := GetBPS div 8;
    MaxVal := 1 shl (GetBPS - 1);
    MaxSample := 0;
    if FStoreMode in [nsmFile, nsmMemory] then
    begin
      if FStoreMode = nsmFile then
      begin
        FullName := '';
        if (Length(FTmpFileName) > 2) and (FTmpFileName[2] <> ':') then
          if SHGetFolderPathA(0, $1a, 0, 0, @UserPath[0]) = 0 then
          begin
            FullName := PChar(@UserPath[0]);
            if FullName[length(FullName)] <> '\' then FullName := FullName + '\';
          end;
        FTmpFileName := FullName + FTmpFileName;
        TmpOutput := TFileStream.Create(FTmpFileName, fmCreate);
      end else
        TmpOutput := TMemoryStream.Create;
      _BufSize := $10000*_BytesPerSample;
      GetMem(InBuf, _BufSize);
    end;
    FInput.GetData(Buf, Bytes);
    while Bytes <> 0 do
    begin
      SC := Bytes div _BytesPerSample;
      for i := 0 to SC -1 do
      begin
        TmpSample := Abs(GetSample(Buf, i));
        if TmpSample > MaxSample then MaxSample := TmpSample;
      end;
      if FStoreMode in [nsmFile, nsmMemory] then
        TmpOutput.Write(Buf^, Bytes);
      FInput.GetData(Buf, Bytes);  
    end;
    if MaxSample = 0 then
      _k := 1
    else
      _k := MaxVal/MaxSample;
    if FStoreMode = nsmNone then
    begin
      FInput.Flush;
      FInput.Init;
    end
    else
      TmpOutput.Seek(0, soFromBeginning)
  end;

  procedure TNormalizer.GetDataInternal;
  var
    i, SC : Integer;
    Sample : Int64;
  begin
    if not _Enabled then
    begin
//      Bytes := Bytes - (Bytes mod _BytesPerSample);
      Finput.GetData(Buffer, Bytes);
      Exit;
    end;
    if  FStoreMode = nsmNone then
    begin
      Finput.GetData(Buffer, Bytes);
      if Bytes = 0 then Exit;
    end else
    begin
      SC := Bytes div _BytesPerSample;
      if SC > $10000 then SC := $10000;
      Bytes := SC * _BytesPerSample;
      if TmpOutput.Size <= TmpOutput.Position then
      begin
        Bytes := 0;
        Buffer := nil;
        Exit;
      end;
      if TmpOutput.Size - TmpOutput.Position < Integer(Bytes) then
        Bytes := TmpOutput.Size - TmpOutput.Position;
      Bytes := TmpOutput.Read(InBuf^, Bytes);
      Buffer := InBuf;
    end;
    SC := Bytes div _BytesPerSample;
    for i := 0 to SC - 1 do
    begin
      Sample := GetSample(Buffer, i);
      Sample := Round(Sample * _k);
      SetSample(Sample, Buffer, i);
    end;
  end;

  procedure TNormalizer.FlushInternal;
  begin
    FInput.Flush;
    if FEnabled then
    if FStoreMode in [nsmFile, nsmMemory] then
    begin
      TmpOutput.Free;
      FreeMem(InBuf);
      if FStoreMode = nsmFile then DeleteFile(PChar(FTmpFileName));
    end;
    Busy := False;
  end;

  constructor TFastResampler.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TFastResampler.Destroy;
  begin
    Inherited Destroy;
  end;

  function TFastResampler.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TFastResampler.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TFastResampler.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FOutSampleRate;
  end;

  procedure TFastResampler.InitInternal;
  var
    i : Integer;
    k : Single;
  const
    NP : Integer = 8;
  procedure Swap(var S1, S2 : Single);
  var
    Tmp : Single;
  begin
    Tmp := S1;
    S1 := S2;
    S2 := Tmp;
  end;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    SamplesInFrame := Finput.Channels;
    FPosition := 0;
    IFrames := FInput.SampleRate div 5;
    OFrames := FOutSampleRate div 5;
    ISize := IFrames*FrameSize;
    OSize := OFrames*FrameSize;
    if OFrames > IFrames then
      MaxFrames := OFrames
    else
      MaxFrames := IFrames;
    MaxSize := MaxFrames*FrameSize;
    SetLength(_Buffer, MaxSize);
    SetLength(InputBuffer, MaxFrames*SamplesInFrame);
    if ISize > OSize then
      k := OSize/ISize
    else
      k := ISize/OSize;
    SetLength(A, NP + 4);
    SetLength(B, NP + 4);
    for i := 0 to NP + 3 do
    begin
      A[i] := 0;
      B[i] := 0;
    end;
    CalculateChebyshev(0.7*k/2, 10, NP, False, A, B);
    SetLength(A, NP + 1);
    SetLength(B, NP);

    SetLength(A, 1);
    SetLength(B, 1);
    A[0] := 0.14;
    B[0] := 0.86;
    for i := 0 to Length(A) div 2 -1 do
      Swap(A[i], A[Length(A) - i -1]);
    for i := 0 to Length(B) div 2 -1 do
      Swap(B[i], B[Length(B) - i -1]);
    OffsX := Length(A) - 1;
    OffsY := Length(B);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], MaxFrames*SampleSize + OffsX);
      FillChar(X[i][0], OffsX*SizeOf(Single), 0);
      SetLength(Y[i], MaxFrames*SampleSize + OffsY);
      FillChar(Y[i][0], OffsY*SizeOf(Single), 0);
    end;
    BufStart := 0;
    BufEnd := 0;
    if FInput.Size > 0 then
      FSize := Round(FInput.Size*OSize/ISize)
    else
      FSize := FInput.Size;
  end;

  procedure TFastResampler.FlushInternal;
  var
    i : Integer;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], 0);
      SetLength(Y[i], 0);
    end;
    Busy := False;
  end;

  procedure TFastResampler.GetDataInternal;
  var
    i, j, k, SamplesRead, FramesRead, Residue : Integer;
    P : PBufferSingle;
    Acc : Single;
    EOF : Boolean;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := FInput.FillBuffer(@_Buffer[0], ISize, EOF);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      SamplesRead := BufEnd div SampleSize;
      FramesRead := BufEnd div FrameSize;
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : ByteToSingle(PBuffer8(_Buffer), P, SamplesRead);
        2 : SmallIntToSingle(PBuffer16(_Buffer), P, SamplesRead);
        3 : Int24ToSingle(PBuffer8(_Buffer), PBufferSingle(P), SamplesRead);
        4 : Int32ToSingle(PBuffer32(_Buffer), PBufferSingle(P), SamplesRead);
      end;
      if OSize > ISize then
      begin
        k := 0;
        i := 0;
        Residue := 0;
        while i < SamplesRead do
        begin
          Residue := Residue - ISize;
          if Residue >= 0 then
          begin
            X[k mod SamplesInFrame][OffsX + k div SamplesInFrame] := 0; //Random(1)*0.05;
          end else
          begin
            X[k mod SamplesInFrame][OffsX + k div SamplesInFrame] := InputBuffer[i];
            Inc(i);
            Residue := Residue + OSize;
          end;
          Inc(k);
        end; // while i < SamplesRead do
        SamplesRead := k;
        FramesRead := SamplesRead div SamplesInFrame;
        for i := 0 to FramesRead -1 do
          for j :=  0 to SamplesInFrame - 1 do
          begin
            Acc := 0;
            MultAndSumSingleArrays(@(X[j][i]), @A[0], Acc, OffsX + 1);
            MultAndSumSingleArrays(@(Y[j][i]), @B[0], Acc, OffsY);
            Y[j][i + OffsY] := Acc;
          end;
        for i := OffsY to FramesRead + OffsY -1 do
          for j :=  0 to SamplesInFrame - 1 do
             InputBuffer[(i - OffsY)*SamplesInFrame + j] :=  Y[j][i];
        for j :=  0 to SamplesInFrame - 1 do
        begin
          for i := 0 to OffsX - 1 do
            X[j][i] := X[j][i + FramesRead];
          for i := 0 to OffsY - 1 do
            Y[j][i] := Y[j][i + FramesRead];
        end;
      end else // if OSize > ISize then
      begin
        for i := 0 to SamplesRead -1 do
          X[i mod SamplesInFrame][OffsX + i div SamplesInFrame] := InputBuffer[i];
        for i := 0 to FramesRead -1 do
          for j :=  0 to SamplesInFrame - 1 do
          begin
            Acc := 0;
            MultAndSumSingleArrays(@(X[j][i]), @A[0], Acc, OffsX + 1);
            MultAndSumSingleArrays(@(Y[j][i]), @B[0], Acc, OffsY);
            Y[j][i + OffsY] := Acc;
          end;
        i := OffsY;
        k := 0;
        Residue := 0;
        while i < FramesRead + OffsY do
        begin
          Residue := Residue - OSize;
          if Residue < 0 then
          begin
            for j :=  0 to SamplesInFrame - 1 do
              InputBuffer[k*SamplesInFrame + j] :=  Y[j][i];
            Residue := Residue + ISize;
            Inc(k);
          end;
          Inc(i);
        end; // while i < FramesRead + OffsY do
        for j :=  0 to SamplesInFrame - 1 do
        begin
          for i := 0 to OffsX - 1 do
            X[j][i] := X[j][i + FramesRead];
          for i := 0 to OffsY - 1 do
            Y[j][i] := Y[j][i + FramesRead];
        end;
        FramesRead := k;
        SamplesRead :=  FramesRead*SamplesInFrame;
      end; // if OSize > ISize then ... else
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
      end;
      BufEnd := SamplesRead*SampleSize;
    end; // if BufStart >= BufEnd then
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;


{$WARNINGS ON}

end.
