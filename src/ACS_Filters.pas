(*
  This file is a part of New Audio Components package 1.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Revision: 1.5 $ $Date: 2007/11/26 20:56:26 $ *)

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
    procedure GetKernel(var K : PDoubleArray);
  published
    property FilterType : TFilterType read FFilterType write SetFilterType;
    property HighFreq : Integer read FHighFreq write SetHighFreq;
    property KernelWidth : Integer read FKernelWidth write SetKernelWidth;
    property LowFreq : Integer read FLowFreq write SetLowFreq;
    property WindowType  : TFilterWindowType read FWindowType write SetWindowType;
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
    if FFilterType = ftLowPass then
    FHighFreq := 0
    else FHighFreq := aFreq;
  end;

  procedure TBWFilter.SetLowFreq;
  begin
    if FFilterType = ftHighPass then
    FLowFreq := 0
    else FLowFreq := aFreq;
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
    FKernelWidth := 31;
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
        SetLength(Kernel, 2*FKernelWidth);
        FillChar(Kernel[0], Length(Kernel)*SizeOf(Double), 0);
        for i := 0 to KernelWidth - 1 do
        for j := 0 to KernelWidth - 1 do
        Kernel[i+j] := Kernel[i+j] + Kernel1[i]*Kernel2[j];
        SetLength(Kernel, FKernelWidth);
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
    InBufMono : PBuffer16;
    InBufStereo : PStereoBuffer16;
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
      if FInput.Channels = 1 then
      begin
        InBufMono := @InBuf[1];
        NumSamples := BufEnd div 2;
        for i := 0 to NumSamples-1 do
        for j := 0 to FKernelWidth-1 do
        DA[i+j] := DA[i+j] + InbufMono[i]*Kernel[j];
        for i := 0 to NumSamples-1 do
        InBufMono[i] := Round(DA[i]);
        BufEnd := NumSamples*2;
        FillChar(DA[0], NumSamples*SizeOf(Double), 0);
        Move(DA[NumSamples], DA[0], (FKernelWidth-1)*SizeOf(Double));
      end else
      begin
        InBufStereo := @InBuf[1];
        NumSamples := BufEnd div 4;
        for i := 0 to NumSamples-1 do
        for j := 0 to FKernelWidth-1 do
        begin
          DAS[i+j].Left := DAS[i+j].Left + InbufStereo[i].Left*Kernel[j];
          DAS[i+j].Right := DAS[i+j].Right + InbufStereo[i].Right*Kernel[j];
        end;
        for i := 0 to NumSamples-1 do
        begin
          InBufStereo[i].Left := Round(DAS[i].Left);
          InBufStereo[i].Right := Round(DAS[i].Right);
        end;
        BufEnd := NumSamples*4;
        FillChar(DAS[0], NumSamples*2*SizeOf(Double), 0);
        for i := 0 to FKernelWidth-2 do
        begin
          DAS[i] := DAS[NumSamples+i];
          DAS[NumSamples+i].Left := 0;
          DAS[NumSamples+i].Right := 0;
        end;
        //Move(DAS[NumSamples], DAS[0], (FKernelWidth-1)*2*SizeOf(Double));
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


{$WARNINGS ON}

end.
