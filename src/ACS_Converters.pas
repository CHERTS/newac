(*
  This file is a part of New Audio Components package 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Converters;

(* Title: ACS_Converters
    Classes which convert bit depth, sample rates, and stereo/mono. *)

interface

uses
  Classes, SysUtils, Windows, ACS_Types, ACS_Procs, ACS_Classes, _MSACM, MMSystem, Math;

const
  BUF_SIZE = $8000;

  KERNEL_WIDTH = 64;

  SD_BUF_SIZE = 2048;

  MaxAudioInput = 2048*6; // To handle 24 bps streams;

type

  TDA = array[0..63] of Double;
  PDA = ^TDA;


  TRateConverter = class(TAuConverter)
  private
    FOutSampleRate : Integer;
    WantedSize : Integer;
    remainder : Integer;
    InBufM, OutBufM : PBuffer16;
    InBufS, OutBufS : PStereoBuffer16;
    DAM : array of Double;
    DAS : array of TStereoSampleD;
    Kernel : array of Double;
    FKernelWidth : Integer;
    FFilterWindow : TFilterWindowType;
    Tail : Pointer;
    LBS : TStereoSample16;
    function ConvertFreqs16Mono(InSize : Integer): Integer;
    function ConvertFreqs16Stereo(InSize : Integer): Integer;
    procedure SetOutSampleRate(aSR : Integer);
    procedure SetKernelWidth(aKW : Integer);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  published
    property FilterWindow : TFilterWindowType read FFilterWindow write FFilterWindow;
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    property OutSampleRate : Integer read FOutSampleRate write SetOutSampleRate;
  end;

  TMSConverter = class(TAuConverter)
  private
    WantedSize : Integer;
    InOutBuf : array[1..BUF_SIZE] of Byte;
    FMode : TMSConverterMode;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Mode : TMSConverterMode read FMode write FMode;
  end;

  TStereoBalance = class(TAuConverter)
  private
    _Buffer : array[0..BUF_SIZE-1] of Byte;
    FBalance : Single;
    procedure SetBalance(a : Single);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Balance : Single read FBalance write SetBalance;
  end;

  (* Class: TAudioConverter
     Descends from <TAuConverter>. TAudioConverter component may be used for
     changing the number of channels and number of bits per sample in an audio
     stream.*)

  TAudioConverter = class(TAuConverter)
  private
    FInPlace : Boolean;
    WantedSize, OutputSize : Integer;
    InOutBuf : PBuffer8;
    FMode : TMSConverterMode;
    FOutBitsPerSample : Integer;
    FOutChannels : Integer;
    ICh, IBPS : Integer;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
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
       audio stream. The valid values are 0, 8, 16, 24. If the property is set
       to 0 (the default) the converter preserves the original stream's number
       of bits per sample.*)
    property OutBitsPerSample : Integer read FOutBitsPerSample write FOutBitsPerSample;
    (* Property: OutChannels
       Use this property to set the number of channels in the resulting audio
       stream. The valid values are 0,  1 (mono), and 2 (stereo). If the
       property is set to 0 (the default value) the converter preserves the
       original stream's number of channels. *)
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
    OBufStart, OBufEnd : Integer;
    EndOfInput, Prepared : Boolean;
    _Header : ACMSTREAMHEADER;
    procedure GetDataBlock(StartFrom : Integer);
    procedure RegetDataBlock;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
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

  function TRateConverter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.Channels;
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
      if l = 0 then _EndOfStream := True;
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
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TMSConverter.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TMSConverter.Destroy;
  begin
    inherited Destroy;
  end;

  function TMSConverter.GetBPS;
  begin
    Result := 16;
  end;

  function TMSConverter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    if FInput.Channels = 1 then Result := 2
    else Result := 1;
  end;

  function TMSConverter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TMSConverter.InitInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    if FInput.Channels = 2 then WantedSize := BUF_SIZE else
    WantedSize := BUF_SIZE shr 1;
    if FInput.Channels = 2 then
    FSize := FInput.Size shr 1
    else FSize := FInput.Size shl 1;
  end;

  procedure TMSConverter.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  procedure TMSConverter.GetDataInternal;
  var
    l : Integer;
    InSize : Integer;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      l := Finput.CopyData(@InOutBuf[1], WantedSize);
      if l = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      InSize := l;
      while (l<>0) and (InSize < WantedSize) do
      begin
        l := Finput.CopyData(@InOutBuf[InSize+1], WantedSize - InSize);
        Inc(InSize, l);
      end;
      if l = 0 then _EndOfStream := True;
      if FInput.Channels = 2 then
      begin
        ConvertStereoToMono16(@InOutBuf[1], InSize);
        BufEnd := InSize shr 1;
      end else
      begin
        ConvertMonoToStereo16(@InOutBuf[1], InSize, FMode);
        BufEnd := InSize shl 1;
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @InOutBuf[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  procedure TRateConverter.SetOutSampleRate(aSR : Integer);
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

  function TStereoBalance.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TStereoBalance.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := 2;
  end;

  function TStereoBalance.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TStereoBalance.InitInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    Busy := True;
    if FInput.Channels = 2 then FSize := FInput.Size
    else FSize := FInput.Size*2;
    FPosition := 0;
  end;

  procedure TStereoBalance.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  procedure TStereoBalance.GetDataInternal;
  var
    WantedSize, i : Integer;
    P16 : PBuffer16;
    P8 : PBuffer8;
    P24 : PBuffer24;
    I1 : Integer;
    Diff : Double;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
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
    FPosition := Round(FSize/FInput.Size)*FInput.Position;
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
        Result := -1;
  end;

  function TAudioConverter.GetCh;
  begin
    if FOutChannels <> 0 then
      Result := FOutChannels
    else
      if Assigned(FInput) then
        Result := FInput.Channels
      else
        Result := -1;
  end;

  function TAudioConverter.GetSR;
  begin
    if Assigned(FInput) then
      Result := FInput.SampleRate
    else
      Result := -1;
  end;

  procedure TAudioConverter.InitInternal;
  var
    k, k1 : Integer;
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
    WantedSize := MaxAudioInput - (MaxAudioInput mod (FInput.Channels * (FInput.BitsPerSample shr 3)));
    k := FInput.BitsPerSample*FInput.Channels;
    k1 := IBPS*ICh;
    if (k1 < k) and (FInput.BitsPerSample <> 24) then FInPlace := True
    else
    begin
      FInPlace := False;
      OutputSize := ((k1 div k) + 1)*WantedSize;
      GetMem(InOutBuf, OutputSize);
    end;
    FSize := Round(FInput.Size*k1/k);
    FSize := FSize - (FSize mod (k1 shr 3));
  end;

  procedure TAudioConverter.FlushInternal;
  begin
    FInput.Flush;
    if not FInPlace then
      FreeMem(InOutBuf);
    Busy := False;
  end;

  procedure TAudioConverter.GetDataInternal;
  var
    l : Integer;
    InSize : Integer;
    Ptr : Pointer;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := 0;
      if FInPlace then
      begin
        l := WantedSize;
        FInput.GetData(Ptr, l);
        InOutBuf := Ptr;
        if l = 0 then
        begin
          Buffer := nil;
          Bytes := 0;
          Exit;
        end;
        InSize := l;
      end else // if FInPlace
      begin
        l := Finput.CopyData(@InOutBuf[0], WantedSize);
        if l = 0 then
        begin
          Buffer := nil;
          Bytes := 0;
          Exit;
        end;
        InSize := l;
        while (l<>0) and (InSize < WantedSize) do
        begin
          l := Finput.CopyData(@InOutBuf[InSize], WantedSize - InSize);
          Inc(InSize, l);
        end;
        if l = 0 then _EndOfStream := True;
      end; //  if FInPlace ... else
      BufEnd := InSize;

      if (FInput.Channels = 2) and (ICh = 1) then
      begin
        if FInput.BitsPerSample = 8 then
           ConvertStereoToMono8(@InOutBuf[0], BufEnd);
        if FInput.BitsPerSample = 16 then
          ConvertStereoToMono16(@InOutBuf[0], BufEnd);
        if FInput.BitsPerSample = 24 then
          ConvertStereoToMono24(@InOutBuf[0], BufEnd);
        BufEnd := BufEnd shr 1;
      end else
      if (FInput.Channels = 1) and (ICh = 2) then
      begin
        if FInput.BitsPerSample = 8 then
          ConvertMonoToStereo8(@InOutBuf[0], BufEnd, FMode);
        if FInput.BitsPerSample = 16 then
          ConvertMonoToStereo16(@InOutBuf[0], BufEnd, FMode);
        if FInput.BitsPerSample = 24 then
          ConvertMonoToStereo24(@InOutBuf[0], BufEnd, FMode);
        BufEnd := BufEnd shl 1;
      end;

      if (FInput.BitsPerSample = 24) and (IBPS <> 24) then
      begin
        Convert24To16(@InOutBuf[0], BufEnd);
        BufEnd := (BufEnd div 3)*2;
      end;

      if (FInput.BitsPerSample = 8) and (IBPS <> 8) then
      begin
        Convert8To16(@InOutBuf[0], BufEnd);
        BufEnd := BufEnd shl 1;
      end else
      if (FInput.BitsPerSample <> 8) and (IBPS = 8) then
      begin
        Convert16To8(@InOutBuf[0], BufEnd);
        BufEnd := BufEnd shr 1;
      end;

      if (FInput.BitsPerSample <> 24) and (IBPS = 24) then
      begin
        Convert16To24(@InOutBuf[0], BufEnd);
        BufEnd := (BufEnd shr 1)*3;
      end;
    end;

    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @InOutBuf[BufStart];
    Inc(BufStart, Bytes);
    //FPosition := Round(FInput.Position*(FSize/FInput.Size));
    Inc(FPosition, Bytes);
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

  procedure TACMConverter.GetDataBlock(StartFrom : Integer);
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
    Len, StartFrom : LongWord;
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
    res := acmStreamOpen(_Stream, 0, SrcWave, DestWave, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME);
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
    Inc(FPosition, Bytes);
  end;

end.
