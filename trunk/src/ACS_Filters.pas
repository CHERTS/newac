(*
  This file is a part of New Audio Components package 1.8
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_Filters;

(* Title: ACS_Filters
    Classes which run filters on audio data. *)

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes,
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Math;


const
  BUF_SIZE = $4000;
  BufSize = $6000;

type

  TFilterType = (ftBandPass, ftBandReject, ftHighPass, ftLowPass, ftAllPass);

  TBWFilter = class(TAuConverter)
  private
    a3 : array[0..2] of Double;
    b2 : array[0..1] of Double;
    x0, x1, y0, y1 : array[0..1] of Double;
    FLowFreq, FHighFreq : Integer;
    FAmplification : Word;
    FFilterType : TFilterType;
    InBuf : array[1..BUF_SIZE] of Byte;
    procedure SetHighFreq(aFreq : Integer);
    procedure SetLowFreq(aFreq : Integer);
    procedure SetAmplification(Ampl : Word);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  published
    property Amplification : Word read FAmplification write SetAmplification;
    property FilterType : TFilterType read FFilterType write FFilterType;
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    property LowFreq : Integer read FLowFreq write SetLowFreq;
  end;

  (* Class: TSincFilter
    This component implements a windowed-sinc filter.
    The component's input should be 1 or 2 channels 16 bps or 32 bps audio.
    A descendant of <TAuConverter>. *)


  TSincFilter = class(TAuConverter)
  private
    {$IFDEF WIN32}
    CS : TRTLCriticalSection;
    {$ENDIF}
    Kernel : array of Double;
    DA : PDoubleArray;
    DAS : PStereoBufferD;
    inBuf : array[1..BUF_SIZE] of Byte;
    FFilterType : TFilterType;
    FKernelWidth : Integer;
    FLowFreq, FHighFreq : Integer;
    FWindowType  : TFilterWindowType;
    procedure SetFilterType(aFT  : TFilterType);
    procedure SetKernelWidth(aKW : Integer);
    procedure SetWindowType(aWT : TFilterWindowType);
    procedure SetHighFreq(aFreq : Integer);
    procedure SetLowFreq(aFreq : Integer);
    procedure CalculateFilter;
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
    procedure GetKernel(var K : PDoubleArray);
  published
    (* Property: FilterType
     Use this property to set the desired filter type: low-pass, high-pass, band-pass, or band-reject. *)
    property FilterType : TFilterType read FFilterType write SetFilterType;
    (* Property: HighFreq
     Use this property to set the high cut-off frequency. This property applies to high-pass, band-pass, and band-reject filters and should always (even for low-pass) be greater than <LowFreq>. *)
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    (* Property: KernelWidth
     Use this property to set the number of points in the filter kernel. *)
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    (* Property: LowFreq
     Use this property to set the low cut-off frequency. This property applies to low-pass, band-pass, and band-reject filters and should always (even for high-pass) be less than <HighFreq>. *)
    property LowFreq : Integer read FLowFreq write SetLowFreq;
    (* Property: WindowType
     Use this property to set the type of the window applied to the filter kernel. *)
    property WindowType  : TFilterWindowType read FWindowType write SetWindowType;
  end;

  TChebyshevFilter = class(TAuConverter)
  private
    A, B : array of Single;
    X, Y : array[0..7] of array of Single;
    FRipple : Single;
    FHighFreq, FLowFreq : Word;
    FNumberOfPoles  : Word;
    FFilterType : TFilterType;
    offsX, OffsY : Integer;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    StartSample, EndSample : Integer;
    procedure CalculateFilter;
    procedure SetNumPoles(NP : Word);
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
    property FilterType : TFilterType read FFilterType write FFilterType;
    property NumberOfPoles : Word read FNumberOfPoles write SetNumPoles;
    property HighFreq : Word read FHighFreq write FHighFreq;
    property LowFreq : Word read FLowFreq write FLowFreq;
    property Ripple : Single read FRipple write FRipple;
  end;


implementation

  constructor TBWFilter.Create;
  begin
    inherited Create(AOwner);
    FFilterType := ftBandPass;
    FAmplification := 1;
  end;

  destructor TBWFilter.Destroy;
  begin
    inherited Destroy;
  end;

  function TBWFilter.GetBPS;
  begin
    Result := 16;
  end;

  function TBWFilter.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.Channels;
  end;

  function TBWFilter.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TBWFilter.SetHighFreq;
  begin
    FHighFreq := aFreq;
  end;

  procedure TBWFilter.SetLowFreq;
  begin
    FLowFreq := aFreq;
  end;

  procedure TBWFilter.SetAmplification;
  begin
    if Ampl > 0 then FAmplification := Ampl;
  end;

  procedure TBWFilter.InitInternal;
  var
    C, D : Double;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input not assigned');
    FInput.Init;
    if ((FHighFreq - FlowFreq) < 0) or (((FHighFreq - FlowFreq) * 2) >= FInput.SampleRate) then
    begin
      FInput.Flush;
      raise EAuException.Create('Illegal frequency');
    end;
    Busy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    FSize := FInput.Size;
    x0[0] := 0.0;
    x0[1] := 0.0;
    x1[0] := 0.0;
    x1[1] := 0.0;
    y0[0] := 0.0;
    y0[1] := 0.0;
    y1[0] := 0.0;
    y1[1] := 0.0;
    case FFilterType of
      ftBandPass :
      begin
        C := 1 / Tan(Pi * (FHighFreq-FLowFreq+1) / FInput.SampleRate);
        D := 2 * Cos(2 * Pi * ((FHighFreq+FLowFreq) shr 1) / FInput.SampleRate);
        a3[0] := 1 / (1 + C);
        a3[1] := 0.0;
        a3[2] := -a3[0];
        b2[0] := -C * D * a3[0];
        b2[1] := (C - 1) * a3[0];
      end;
      ftBandReject:  // This doesn't seem to work well
      begin
        C := Tan(Pi * (FHighFreq-FLowFreq+1) / FInput.SampleRate);
        D := 2 * Cos(2 * Pi * ((FHighFreq+FLowFreq) shr 1) / FInput.SampleRate);
        a3[0] := 1 / (1 + C);
        a3[1] := -D * a3[0];
        a3[2] := a3[0];
        b2[0] := a3[1];
        b2[1] := (1 - C) * a3[0];
      end;
      ftLowPass:
      begin
        C := 1 / Tan(Pi * FLowFreq / FInput.SampleRate);
        a3[0] := 1 / (1 + Sqrt(2) * C + C * C);
        a3[1] := 2 * a3[0];
        a3[2] := a3[0];
        b2[0] := 2 * (1 - C * C) * a3[0];
        b2[1] := (1 - Sqrt(2) * C + C * C) * a3[0];
      end;
      ftHighPass:
      begin
        C := Tan(Pi * FHighFreq / FInput.SampleRate);
        a3[0] := 1 / (1 + Sqrt(2) * C + C * C);
        a3[1] := -2 * a3[0];
        a3[2] := a3[0];
        b2[0] := 2 * (C * C - 1) * a3[0];
        b2[1] := (1 - Sqrt(2) * C + C * C) * a3[0];
      end;
    end;
  end;

  procedure TBWFilter.FlushInternal;
  begin
    FInput.Flush;
    Busy := False;
  end;

  procedure TBWFilter.GetDataInternal;
  var
    i : Integer;
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
    arg, res : Double;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      BufEnd := FInput.CopyData(@InBuf[1], BUF_SIZE);
      if BufEnd = 0 then
      begin
        Bytes := 0;
        Buffer := nil;
        Exit;
      end;
      if Self.Channels = 1 then
      begin
        InBufMono := @InBuf[1];
        for i := 0 to (BufEnd shr 1) - 1 do
        begin
          arg := InBufMono[i];
          res := a3[0] * arg + a3[1] * x0[0] + a3[2] * x1[0] -
                 b2[0] * y0[0] - b2[1] * y1[0];
          InBufMono[i] := Round(res);
          x1[0] := x0[0];
          x0[0] := arg;
          y1[0] := y0[0];
          y0[0] := res;
          InBufMono[i] := FAmplification * InBufMono[i];
        end;
      end else
      begin
        InBufStereo := @InBuf[1];
        for i := 0 to (BufEnd shr 2) - 1 do
        begin
          arg := InBufStereo[i].Left;
          res := a3[0] * arg + a3[1] * x0[0] + a3[2] * x1[0] -
                 b2[0] * y0[0] - b2[1] * y1[0];
          InBufStereo[i].Left := Round(res);
          x1[0] := x0[0];
          x0[0] := arg;
          y1[0] := y0[0];
          y0[0] := res;
          arg := InBufStereo[i].Right;
          res := a3[0] * arg + a3[1] * x0[1] + a3[2] * x1[1] -
                 b2[0] * y0[1] - b2[1] * y1[1];
          InBufStereo[i].Right := Round(res);
          x1[1] := x0[1];
          x0[1] := arg;
          y1[1] := y0[1];
          y0[1] := res;
          InBufStereo[i].Right := FAmplification * InBufStereo[i].Right;
          InBufStereo[i].Left := FAmplification * InBufStereo[i].Left;
        end;
      end;
    end;
    if Bytes > (BufEnd - BufStart + 1) then
    Bytes := BufEnd - BufStart + 1;
    Buffer := @InBuf[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
 //   Inc(FPosition, Result);
  end;

  constructor TSincFilter.Create;
  begin
    inherited Create(AOwner);
    FKernelWidth := 63;
    FWindowType := fwBlackman;
    FLowFreq := 8000;
    FHighFreq := 16000;
    DA := nil;
    DAS := nil;
    {$IFDEF WIN32}
    InitializeCriticalSection(CS);
    {$ENDIF}
  end;

  destructor TSincFilter.Destroy;
  begin
    Kernel := nil;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    {$IFDEF WIN32}
    DeleteCriticalSection(CS);
    {$ENDIF}
    Inherited Destroy;
  end;

  procedure TSincFilter.CalculateFilter;
  var
    Kernel1, Kernel2 : array of Double;
    CutOff : Double;
    i, j : Integer;
  begin
    if csDesigning in ComponentState then Exit;
    if not Assigned(FInput) then Exit;
    if (FLowFreq > FInput.SampleRate/2) or (FHighFreq > FInput.SampleRate/2) then
    raise EAuException.Create('Cut-off frequencies are greater than the half of the sample rate.');
    {$IFDEF WIN32}
    EnterCriticalSection(CS);
    {$ENDIF}
    case FilterType of
      ftLowPass:
      begin
        SetLength(Kernel, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel[0], CutOff, FKernelWidth, FWindowType);
      end;
      ftHighPass:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := -Kernel[i];
        Kernel[(FKernelWidth shr 1)] := Kernel[(FKernelWidth shr 1)] + 1;
      end;
      ftBandPass:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, 2*FKernelWidth - 1);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Double), 0);
        for i := 0 to KernelWidth - 1 do
        for j := 0 to KernelWidth - 1 do
        Kernel[i+j] := Kernel[i+j] + Kernel1[i]*Kernel2[j];
//        SetLength(Kernel, FKernelWidth);
        FKernelWidth := 2*FKernelWidth - 1;
        for i := 0 to KernelWidth - 1 do
          Kernel[i] := Kernel[i]*10;
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftBandReject:
      begin
        if not Odd(FKernelWidth) then Inc(FKernelWidth);
        SetLength(Kernel1, FKernelWidth);
        CutOff := FHighFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel1[0], CutOff, FKernelWidth, FWindowType);
        for i := 0 to FKernelWidth - 1 do
        Kernel1[i] := -Kernel1[i];
        Kernel1[(FKernelWidth shr 1)] := Kernel1[(FKernelWidth shr 1)] + 1;
        SetLength(Kernel2, FKernelWidth);
        CutOff := FLowFreq/FInput.SampleRate;
        CalculateSincKernel(@Kernel2[0], CutOff, FKernelWidth, FWindowType);
        SetLength(Kernel, FKernelWidth);
        for i := 0 to FKernelWidth - 1 do
        Kernel[i] := Kernel1[i] + Kernel2[i];
        Kernel1 := nil;
        Kernel2 := nil;
      end;
      ftAllPass :
      begin
        SetLength(Kernel, FKernelWidth);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Double), 0);
        Kernel[FKernelWidth shr 1] := 1;
      end;
    end;
    {$IFDEF WIN32}
    LeaveCriticalSection(CS);
    {$ENDIF}
  end;

  procedure TSincFilter.SetFilterType;
  begin
    FFilterType := aFT;
    if Busy then CalculateFilter;
  end;

  procedure TSincFilter.SetKernelWidth;
  begin
    if aKW > 2 then
    if not Busy then FKernelWidth := aKW;
  end;

  procedure TSincFilter.SetWindowType;
  begin
    FWindowType := aWT;
    if Busy then CalculateFilter;
  end;

  procedure TSincFilter.SetHighFreq;
  begin
    if aFreq > 0 then
    FHighFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FHighFreq > Finput.SampleRate div 2 then
    FHighFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FLowFreq := FHighFreq;
    if Busy then CalculateFilter;
  end;

  procedure TSincFilter.SetLowFreq;
  begin
    if aFreq > 0 then
    FLowFreq := aFreq;
    if csDesigning in ComponentState then Exit;
    if Assigned(Finput) then
    if FlowFreq > Finput.SampleRate div 2 then
    FLowFreq := Finput.SampleRate div 2;
    if FHighFreq < FLowFreq then
    FHighFreq := FLowFreq;
    if Busy then CalculateFilter;
  end;

  function TSincFilter.GetBPS;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TSincFilter.GetCh;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TSincFilter.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TSincFilter.InitInternal;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    if not (Finput.BitsPerSample in [16,32]) then
       raise EAuException.Create('Only 16 or 32 bps input is accepted');
    FPosition := 0;
    CalculateFilter;
    if FInput.Channels = 1 then
    begin
      GetMem(DA, ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double));
      FillChar(DA[0], ((BUF_SIZE div 2)+FKernelWidth-1)*SizeOf(Double), 0);
    end else
    begin
      GetMem(DAS, ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double));
      FillChar(DAS[0], ((BUF_SIZE div 2)+(FKernelWidth-1)*2)*SizeOf(Double), 0);
    end;
    BufStart := 1;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TSincFilter.FlushInternal;
  begin
    FInput.Flush;
    if DA <> nil then FreeMem(DA);
    if DAS <> nil then FreeMem(DAS);
    DA := nil;
    DAS := nil;
    Busy := False;
  end;

  procedure TSincFilter.GetDataInternal;
  var
    i, j, NumSamples : Integer;
    InBufMono32 : PBuffer32;
    InBufStereo32 : PStereoBuffer32;
    InBufMono16 : PBuffer16;
    InBufStereo16 : PStereoBuffer16;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      {$IFDEF WIN32}
      EnterCriticalSection(CS);
      {$ENDIF}
      BufStart := 1;
      BufEnd := FInput.CopyData(@InBuf[1], BUF_SIZE);
      if BufEnd = 0 then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      if FInput.BitsPerSample = 16 then
      begin
        if FInput.Channels = 1 then
        begin
          InBufMono16 := @InBuf[1];
          NumSamples := BufEnd div 2;
          for i := 0 to NumSamples-1 do
            for j := 0 to FKernelWidth-1 do
              DA[i+j] := DA[i+j] + InbufMono16[i]*Kernel[j];
          for i := 0 to NumSamples-1 do
          InBufMono16[i] := Round(DA[i]);
          FillChar(DA[0], NumSamples*SizeOf(Double), 0);
          Move(DA[NumSamples], DA[0], (FKernelWidth-1)*SizeOf(Double));
        end else
        begin
          InBufStereo16 := @InBuf[1];
          NumSamples := BufEnd div 4;
          for i := 0 to NumSamples-1 do
            for j := 0 to FKernelWidth-1 do
            begin
              DAS[i+j].Left := DAS[i+j].Left + InbufStereo16[i].Left*Kernel[j];
              DAS[i+j].Right := DAS[i+j].Right + InbufStereo16[i].Right*Kernel[j];
            end;
          for i := 0 to NumSamples-1 do
          begin
            InBufStereo16[i].Left := Round(DAS[i].Left);
            InBufStereo16[i].Right := Round(DAS[i].Right);
          end;
          FillChar(DAS[0], NumSamples*2*SizeOf(Double), 0);
          for i := 0 to FKernelWidth-2 do
          begin
            DAS[i] := DAS[NumSamples+i];
            DAS[NumSamples+i].Left := 0;
            DAS[NumSamples+i].Right := 0;
          end;
        end;
      end else
      if FInput.BitsPerSample = 32 then
      begin
        if FInput.Channels = 1 then
        begin
          InBufMono32 := @InBuf[1];
          NumSamples := BufEnd div 4;
          for i := 0 to NumSamples-1 do
            for j := 0 to FKernelWidth-1 do
              DA[i+j] := DA[i+j] + InbufMono32[i]*Kernel[j];
          for i := 0 to NumSamples-1 do
          InBufMono32[i] := Round(DA[i]);
          FillChar(DA[0], NumSamples*SizeOf(Double), 0);
          Move(DA[NumSamples], DA[0], (FKernelWidth-1)*SizeOf(Double));
        end else
        begin
          InBufStereo32 := @InBuf[1];
          NumSamples := BufEnd div 8;
          for i := 0 to NumSamples-1 do
            for j := 0 to FKernelWidth-1 do
            begin
              DAS[i+j].Left := DAS[i+j].Left + InbufStereo32[i].Left*Kernel[j];
              DAS[i+j].Right := DAS[i+j].Right + InbufStereo32[i].Right*Kernel[j];
            end;
          for i := 0 to NumSamples-1 do
          begin
            InBufStereo32[i].Left := Round(DAS[i].Left);
            InBufStereo32[i].Right := Round(DAS[i].Right);
          end;
          FillChar(DAS[0], NumSamples*2*SizeOf(Double), 0);
          for i := 0 to FKernelWidth-2 do
          begin
            DAS[i] := DAS[NumSamples+i];
            DAS[NumSamples+i].Left := 0;
            DAS[NumSamples+i].Right := 0;
          end;
        end;
      end;

      {$IFDEF WIN32}
      LeaveCriticalSection(CS);
      {$ENDIF}
    end;
    if Bytes > (BufEnd - BufStart + 1) then
      Bytes := BufEnd - BufStart + 1;
    Buffer := @InBuf[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TSincFilter.GetKernel;
  begin
    K := @Kernel[0];
  end;

  constructor TChebyshevFilter.Create;
  begin
    inherited Create(AOwner);
    FRipple := 0.5;
    FFilterType := ftLowPass;
    NumberOfPoles := 6;
  end;

  destructor TChebyshevFilter.Destroy;
  begin
    Inherited Destroy;
  end;

  function TChebyshevFilter.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TChebyshevFilter.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TChebyshevFilter.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TChebyshevFilter.CalculateFilter;
  var
    A1, B1, A2, B2 : array of Single;
    i, j : Integer;
    CutOff, Tmp : Single;
  procedure Swap(var S1, S2 : Single);
  begin
    Tmp := S1;
    S1 := S2;
    S2 := Tmp;
  end;
  begin
    SetLength(A, 33);
    SetLength(B, 33);
    for i := 0 to 32 do
    begin
      A[i] := 0;
      B[i] := 0;
    end;
    case FFilterType of
      ftLowpass :
      begin
        CutOff := FLowFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency shouild be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, False, A, B);
        SetLength(A, FNumberOfPoles + 1);
        SetLength(B, FNumberOfPoles);
      end;
      ftHighPass:
      begin
        CutOff := FHighFreq/FInput.SampleRate;
        if CutOff >= 0.5 then
          raise EAuException.Create('Cut-off frequency shouild be less than half of sample rate');
        CalculateChebyshev(CutOff, FRipple, FNumberOfPoles, True, A, B);
        SetLength(A, FNumberOfPoles + 1);
        SetLength(B, FNumberOfPoles);
      end;
      ftBandPass:
      begin
        FFilterType := ftLowpass;
        CalculateFilter;
        SetLength(A1, Length(A));
        for i := 0  to Length(A) - 1 do
          A1[i] := A[i];
        SetLength(B1, Length(B));
        for i := 0  to Length(B) - 1 do
          B1[i] := B[i];
        FFilterType := ftHighPass;
        CalculateFilter;
        SetLength(A2, Length(A));
        for i := 0  to Length(A) - 1 do
          A2[i] := A[i];
        SetLength(B2, Length(B));
        for i := 0  to Length(B) - 1 do
          B2[i] := B[i];
        FFilterType := ftBandPass;
        SetLength(A, Length(A)*2);
        for i := 0 to Length(A) - 1 do
          A[i] := 0;
        for i := 0 to Length(A2) - 1 do
          for j := 0 to Length(A1) - 1 do
            A[i + j] := A[i + j] + A1[i]*A2[j];
        SetLength(B, Length(B)*2);
        for i := 0 to Length(B) - 1 do
          B[i] := 0;
        for i := 0 to Length(B) div 2 - 1 do
          for j := 0 to Length(B) div 2 - 1 do
            B[i + j] := B[i + j] + B1[i]*B2[j];
      end;
      ftBandReject:
      begin
        FFilterType := ftLowpass;
        CalculateFilter;
        SetLength(A1, Length(A));
        for i := 0  to Length(A) - 1 do
          A1[i] := A[i];
        SetLength(B1, Length(B));
        for i := 0  to Length(B) - 1 do
          B1[i] := B[i];
        FFilterType := ftHighPass;
        CalculateFilter;
        SetLength(A2, Length(A));
        for i := 0  to Length(A) - 1 do
          A2[i] := A[i];
        SetLength(B2, Length(B));
        for i := 0  to Length(B) - 1 do
          B2[i] := B[i];
        FFilterType := ftBandReject;
        SetLength(A, Length(A)*2);
        for i := 0 to Length(A) - 1 do
          A[i] := 0;
        for i := 0 to Length(A1) - 1 do
          for j := 0 to Length(B2) - 1 do
            A[i + j] := A[i + j] + A1[i]*B2[j] + A2[i]*B1[j];
        SetLength(B, Length(B)*2);
        for i := 0 to Length(B) - 1 do
          B[i] := 0;
        for i := 0 to Length(B) div 2 - 1 do
          for j := 0 to Length(B) div 2 - 1 do
            B[i + j] := B[i + j] + B1[i]*B2[j];
      end;
    end;
      for i := 0 to Length(A) div 2 -1 do
         Swap(A[i], A[Length(A) - i -1]);
      for i := 0 to Length(B) div 2 -1 do
        Swap(B[i], B[Length(B) - i -1]);
  end;

  procedure TChebyshevFilter.InitInternal;
  var
    i : Integer;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    SamplesInFrame := Finput.Channels;
    FPosition := 0;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    CalculateFilter;
    OffsX := Length(A) - 1;
    OffsY := Length(B);
    for i := 0 to SamplesInFrame - 1 do
    begin
      SetLength(X[i], BufSize div SampleSize + OffsX);
      FillChar(X[i][0], OffsX*SizeOf(Single), 0);
      SetLength(Y[i], BufSize div SampleSize + OffsY);
      FillChar(Y[i][0], OffsY*SizeOf(Single), 0);
    end;
    BufStart := 0;
    BufEnd := 0;
    FSize := FInput.Size;
  end;

  procedure TChebyshevFilter.FlushInternal;
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

  procedure TChebyshevFilter.GetDataInternal;
  var
    i, j, k, SamplesRead, FramesRead : Integer;
    P : PBufferSingle;
    Acc : Single;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if BufStart >= BufEnd then
    begin
      BufStart := 0;
      BufEnd := FInput.CopyData(@_Buffer[0], BufSize);
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
      P := PBufferSingle(@InputBuffer[0]);
      case SampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
      end;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TChebyshevFilter.SetNumPoles(NP : Word);
  begin
    FNumberOfPoles := ((NP + 1) shr 1)*2;
    if FNumberOfPoles > 12 then FNumberOfPoles := 12;
  end;




{$WARNINGS ON}

end.
