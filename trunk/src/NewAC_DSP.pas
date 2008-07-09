(*
  This file is a part of New Audio Components package v 1.8
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewAC_DSP;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ACS_Types, FFTReal, Math;

type

  TOutputFormat = (ofmtPascal, ofmtMathematica);

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
    procedure SaveMagnitude(Channel : Word; const FileName : String);
    procedure SaveLogMagnitude(Channel : Word; const FileName : String);
    property Magnitude[Channel, Index : Word] : Single read GetMagnitude;
    property LogMagnitude[Channel, Index : Word] : Single read GetLogMagnitude;
    property Separator : Char read FSeparator write FSeparator;
  published
    property N : Word read _N write _N;
    property Window : TFilterWindowType read FWindow write FWindow;
    property StartSample : Int64 read FStartSample write FStartSample;
    property EndSample : Int64 read FEndSample write FEndSample;
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
    Result := Log10(GetMagnitude(Channel, Index));
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
    for i := 0 to _N div 2 - 2 do
      Write(F, FloatToStrF(GetLogMagnitude(Channel, i), ffFixed, 7, 7), FSeparator);
    WriteLn(F, FloatToStrF(GetLogMagnitude(Channel, _N div 2 -1), ffFixed, 7, 7));
    DecimalSeparator := OldSep;
    System.Close(F);
  end;


end.
