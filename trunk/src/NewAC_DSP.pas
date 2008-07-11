(*
  This file is a part of New Audio Components package v 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewAC_DSP;

(* Title: NewAC_DSP
    This unit contains components performing different DSP-related tasks. (c) 2008, Andrei
    Borovsky (anb@symmetrica.net). All rights reserved. See the LICENSE file
    for more details. *)


interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ACS_Types, FFTReal, Math;

const
  BufSize : Integer = $6000;

type

(* Class: TFrequencyAnalysis
     This component generates input's frequency spectrum using averaged real DFT.
     TFrequencyAnalysis is an output component but unlike other output components
     it doesn't provide audio data. TFrequencyAnalysis' output is an audio frequency spectrum.
     Descends from <TAuOutput>.*)

  TFrequencyAnalysis = class(TAuOutput)
  private
    _N : Word;
    Core : TFFTReal;
    FWindow : TFilterWindowType;
    FStartSample, FEndSample : Int64;
    _Data : array[0..7] of array of Single;
    TmpData, OutData, InputData, W : array of Single;
    MaxChannels : Integer;
    FDataSize : Int64;
    _Buffer : PByte;
    FNumSamples : Int64;
    FCurSample : Int64;
    ChunkCount : Integer;
    FSeparator : Char;
    function GetMagnitude(Channel, Index : Word) : Single;
    function GetLogMagnitude(Channel, Index : Word) : Single;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: SaveMagnitude
       This method saves magnitude series obtained at channel Channel to the file specified by FileName. *)
    procedure SaveMagnitude(Channel : Word; const FileName : String);
    (* Function: SaveLogMagnitude
       This method saves logarithmed magnitude series obtained at channel Channel to the file specified by FileName. *)
    procedure SaveLogMagnitude(Channel : Word; const FileName : String);
    (* Property: Magnitude
       Returns the value of the magnitude scpecified by channel number and index.
       Valid indeces range from 0 to <N>/2 - 1. *)
    property Magnitude[Channel, Index : Word] : Single read GetMagnitude;
    (* Property: LogMagnitude
       Returns the logarithm of the magnitude scpecified by channel number and index.
       Valid indeces range from 0 to <N>/2 - 1. *)
    property LogMagnitude[Channel, Index : Word] : Single read GetLogMagnitude;
    (* Separator: Magnitude
       Use this property to specify the character used to delimit the values being saved to a file. *)
    property Separator : Char read FSeparator write FSeparator;
  published
    (* Property: N
       The number of input points for performing real DFT.
       Magnitude calculation produces N/2 values that represent the frequency distrbution between 0 and samplerate/2. *)
    property N : Word read _N write _N;
    (* Property: Window
       Use this proeprty to select the type of the window applied to the input data. *)
    property Window : TFilterWindowType read FWindow write FWindow;
    property StartSample : Int64 read FStartSample write FStartSample;
    property EndSample : Int64 read FEndSample write FEndSample;
  end;

(* Class: TConvolver
     This component performs convolution.
     Descends from <TAuOutput>.*)

  TConvolver = class(TAuConverter)
  private
    Kernel : array of Single;
    InputBuffer, OutputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    StartSample, EndSample : Integer;
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
    (* Function SetKernel
       Call this method to set the convolution kernel (impulse response function). *)
    procedure SetKernel(const K : array of Single);
  end;

  TDifferenceEquation = class(TAuConverter)
  private
    _A, _B : array of Single;
    X, Y : array[0..7] of array of Single;
    offsX, OffsY : Integer;
    InputBuffer : array of Single;
    SampleSize, FrameSize, SamplesInFrame : Word;
    _Buffer : array of Byte;
    StartSample, EndSample : Integer;
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
    procedure SetCoefficients(const A, B : array of Single);
  end;


implementation

  constructor TFrequencyAnalysis.Create(AOwner: TComponent);
  begin
    inherited;
    FStartSample := 0;
    FEndSample := -1;
    MaxChannels := -1;
    FSeparator := ' ';
  end;

  destructor TFrequencyAnalysis.Destroy;
  begin
    inherited;
  end;

  procedure TFrequencyAnalysis.Prepare;
  var
    i, j : Integer;
  begin
    if FInput = nil then raise EAuException.Create('Input is not assigned');
    FInput.Init;
    MaxChannels := FInput.Channels -1;
    FDataSize := _N*(FInput.BitsPerSample div 8)*FInput.Channels;
    GetMem(_Buffer, FDataSize);
    SetLength(W, _N);
    case FWindow of
      fwHamming : HammingWindowS(@W[0], _N, False);
      fwHann : HannWindowS(@W[0], _N, False);
      fwBlackman : BlackmanWindowS(@W[0], _N, False);
    end;
    SetLength(TmpData, _N);
    SetLength(OutData, _N);
    SetLength(InputData, _N*(MaxChannels + 1));
    for i := 0 to MaxChannels do
    begin
      SetLength(_Data[i], _N div 2);
      FillChar(_Data[i][0], _N*2, 0);
    end;
    Core := TFFTReal.Create(_N);
    if FInput is TAuFileIn then
    begin
      TAuFileIn(FInput).Seek(FStartSample);
      if FEndSample = -1 then
        FNumSamples := Finput.TotalSamples - FStartSample
      else
        FNumSamples := FEndSample - FStartSample;
    end else
    FNumSamples := High(Int64) - 1;
    FCurSample := FStartSample;
    ChunkCount := 0;
  end;

  procedure TFrequencyAnalysis.Done;
  var
    i, j : Integer;
  begin
    FInput.Flush;
    FreeMem(_Buffer);
    SetLength(W, 0);
    SetLength(TmpData, 0);
    SetLength(InputData, 0);
    SetLength(OutData, 0);
    if ChunkCount <> 0 then
      for i := 0 to MaxChannels do
        for j := 0 to _N div 2 - 1 do
          _Data[i][j] := _Data[i][j]/(ChunkCount*_N);
    Core.Free;
  end;

  function TFrequencyAnalysis.DoOutput(Abort : Boolean) : Boolean;
  var
    FEOF : Boolean;
    Len : LongWord;
    i, j : Integer;
    P : PBufferSingle;
  begin
    if (FCurSample >= FStartSample + FNumSamples) or Abort then
    begin
      Result := False;
      Exit;
    end;
    Len := FInput.FillBuffer(Pointer(_Buffer), FDataSize, FEOF);
    if FEOF and (Len < FDataSize) then
    begin
      FCurSample := FStartSample + FNumSamples;
      Result := False;
      Exit;
    end;
    P := PBufferSingle(@InputData[0]);
    case FInput.BitsPerSample of
      8 : ByteToSingle(PBuffer8(_Buffer), P, _N*(MaxChannels + 1));
      16 : SmallIntToSingle(PBuffer16(_Buffer), P, _N*(MaxChannels + 1));
      24 : Int24ToSingle(PBuffer8(_Buffer), PBufferSingle(P), _N*(MaxChannels + 1));
      32 : Int32ToSingle(PBuffer32(_Buffer), PBufferSingle(P), _N*(MaxChannels + 1));
    end;
    for i := 0 to MaxChannels do
    begin
      for j := 0 to _N -1 do
         TmpData[j] := InputData[j*(MaxChannels + 1) + i];
      MultSingleArrays(@TmpData[0], @W[0], _N);
      Core.do_fft(@OutData[0], @TmpData[0]);
      for j := 1 to _N div 2 -1 do
        _Data[i][j] := _Data[i][j] + Hypot(OutData[j], OutData[j + N div 2]);
      _Data[i][0] := _Data[i][0] + Abs(OutData[0])*2;
    end;
    Inc(FCurSample, _N);
    Inc(ChunkCount);
  end;


  function TFrequencyAnalysis.GetMagnitude(Channel, Index : Word) : Single;
  begin
    Result := _Data[Channel, Index];
  end;

  function TFrequencyAnalysis.GetLogMagnitude(Channel, Index : Word) : Single;
  begin
    if GetMagnitude(Channel, Index) <> 0 then
      Result := Log10(GetMagnitude(Channel, Index))
    else
      Result := MinSingle;
  end;

  procedure TFrequencyAnalysis.SaveMagnitude(Channel : Word; const FileName : String);
  var
    F : System.Text;
    i : Integer;
    OldSep : Char;
  begin
    System.Assign(F, FileName);
    System.Rewrite(F);
    OldSep := DecimalSeparator;
    DecimalSeparator := '.';
    for i := 0 to _N div 2 - 2 do
      Write(F, FloatToStrF(GetMagnitude(Channel, i), ffFixed, 7, 7), FSeparator);
    WriteLn(F, FloatToStrF(GetMagnitude(Channel, _N div 2 -1), ffFixed, 7, 7));
    DecimalSeparator := OldSep;
    System.Close(F);
  end;


  procedure TFrequencyAnalysis.SaveLogMagnitude(Channel : Word; const FileName : String);
  var
    F : System.Text;
    i : Integer;
    OldSep : Char;
  begin
    System.Assign(F, FileName);
    System.Rewrite(F);
    OldSep := DecimalSeparator;
    DecimalSeparator := '.';
    for i := 0 to _N div 2 - 2 do
      Write(F, FloatToStrF(GetLogMagnitude(Channel, i), ffFixed, 7, 7), FSeparator);
    WriteLn(F, FloatToStrF(GetLogMagnitude(Channel, _N div 2 -1), ffFixed, 7, 7));
    DecimalSeparator := OldSep;
    System.Close(F);
  end;

  constructor TConvolver.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TConvolver.Destroy;
  begin
    Inherited Destroy;
  end;

  function TConvolver.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TConvolver.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TConvolver.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TConvolver.InitInternal;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Busy := True;
    FInput.Init;
    SampleSize := FInput.BitsPerSample div 8;
    FrameSize := SampleSize*FInput.Channels;
    FPosition := 0;
    SetLength(_Buffer, BufSize);
    SetLength(InputBuffer, BufSize div SampleSize);
    SetLength(OutputBuffer, BufSize div SampleSize + (Length(Kernel) - 1)*FInput.Channels);
    FillChar(OutputBuffer[0], Length(OutputBuffer)*SizeOf(Single), 0);
    BufStart := 0;
    BufEnd := 0;
    SamplesInFrame := Finput.Channels;
    FSize := FInput.Size;
  end;

  procedure TConvolver.FlushInternal;
  begin
    FInput.Flush;
    SetLength(_Buffer, 0);
    SetLength(InputBuffer, 0);
    SetLength(OutputBuffer, 0);
    Busy := False;
  end;

  procedure TConvolver.GetDataInternal;
  var
    i, j, k, SamplesRead, FramesRead : Integer;
    P : PBufferSingle;
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
      for i := 0 to FramesRead -1 do
       for j := 0 to Length(Kernel) - 1 do
         for k := 0 to SamplesInFrame - 1 do
           OutputBuffer[(i + j)*SamplesInFrame + k] := OutputBuffer[(i + j)*SamplesInFrame + k] + InputBuffer[i*SamplesInFrame + k]*Kernel[j];
      P := PBufferSingle(@OutputBuffer[0]);
      case SampleSize of
        1 : SingleToByte(P, PBuffer8(_Buffer), SamplesRead);
        2 : SingleToSmallInt(P, PBuffer16(_Buffer), SamplesRead);
        3 : SingleToInt24(P, PBuffer8(_Buffer), SamplesRead);
        4 : SingleToInt32(P, PBuffer32(_Buffer), SamplesRead);
      end;
      for i := 0 to SamplesRead - 1 do
        OutputBuffer[i] := 0;
      for i := 0 to (Length(Kernel) -1)*SamplesInFrame - 1 do
      begin
        OutputBuffer[i] := OutputBuffer[i + SamplesRead];
        OutputBuffer[i + SamplesRead] := 0;
      end;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @_Buffer[BufStart];
    Inc(BufStart, Bytes);
    FPosition := Round(FInput.Position*(FSize/FInput.Size));
  end;

  procedure TConvolver.SetKernel;
  var
    i : Integer;
  begin
    SetLength(Kernel, Length(K));
    for i := 0 to  Length(Kernel) - 1 do
      Kernel[i] := K[i];
  end;

  constructor TDifferenceEquation.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TDifferenceEquation.Destroy;
  begin
    Inherited Destroy;
  end;

  function TDifferenceEquation.GetBPS;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.BitsPerSample;
  end;

  function TDifferenceEquation.GetCh;
  begin
    if not Assigned(Input) then
      raise EAuException.Create('Input is not assigned');
    Result := FInput.Channels;
  end;

  function TDifferenceEquation.GetSR;
  begin
    if not Assigned(Input) then
    raise EAuException.Create('Input is not assigned');
    Result := FInput.SampleRate;
  end;

  procedure TDifferenceEquation.InitInternal;
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
    OffsX := Length(_A) - 1;
    OffsY := Length(_B);
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

  procedure TDifferenceEquation.FlushInternal;
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

  procedure TDifferenceEquation.GetDataInternal;
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
          MultAndSumSingleArrays(@(X[j][i]), @_A[0], Acc, OffsX + 1);
          MultAndSumSingleArrays(@(Y[j][i]), @_B[0], Acc, OffsY);
          Y[j][i + OffsY] := Acc;
        end;
      for i := 0 to FramesRead -1 do
        for j :=  0 to SamplesInFrame - 1 do
          InputBuffer[i*SamplesInFrame + j] :=  Y[j][i];
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

  procedure TDifferenceEquation.SetCoefficients;
  var
    i : Integer;
  begin
    SetLength(_A, Length(A));
    for i := 0 to  Length(A) - 1 do
      _A[i] := A[Length(A) - 1 - i];
    SetLength(_B, Length(B));
    for i := 0 to  Length(B) - 1 do
      _B[i] := B[Length(B) - 1 - i];
  end;


end.
