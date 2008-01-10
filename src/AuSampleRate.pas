(*
  This file is a part of New Audio Components package v 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

(*
  Currently TResampler cannot perform resampling at 24 bps.
  The component can accept 24 bit input but it will produce only 16 bit output.
  This doesn't apply to the pass through mode (when the input sample rate
  is the same as the output sample rate) in which the audio data is passed
  unmodified.
*)

unit AuSampleRate;

(* Title: AuSampleRate
    Components which implement resampling of audio data. *)

interface

uses

  Classes, SysUtils, ACS_Types, ACS_Procs, ACS_Classes, libsamplerate, Math;

const

  InitialBufferSize = $100000;
  IOBufSize = $200000;

type

  TResamplerQuality = (rqBest, rqMedium, rqFastest);

  (* Class: TResampler
     A high-quality audio resampler.
     Descends from <TAuConverter>.
     Requires libsamplerate.dll.
     More information on this resampler can be found at http://www.mega-nerd.com/SRC/.
     Currently TResampler cannot perform resampling at 24 bps.
     The component can accept 24 bit input but it will produce only 16 bit output.
     This doesn't apply to the pass through mode (when the input sample rate
     is the same as the output sample rate) in which the audio data is passed
     unmodified.*)

  TResampler = class(TAuConverter)
  private
    FPassThrough : Boolean;
    _State : Pointer;
    Data : SRC_DATA;
    FOutSampleRate : Integer;
    FQuality : TResamplerQuality;
    InputBuffer : array [0..IOBufSize - 1] of Byte;
    IBufferEnd : Integer; // Points to the position after the last byte of the array
    OutputBuffer : array [0..IOBufSize - 1] of Byte;
    OBufferStart, // Points to the position to read from
    OBufferEnd : Integer; // Points to the position after the last byte of the array
    IFloatBuffer : array[0..InitialBufferSize - 1] of Single;
    OFloatBuffer : array[0..IOBufSize - 1] of Single;
    EndOfInput : Boolean;
    procedure SetOutSampleRate(aSR : Integer);
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
    (* Property: Quality
       Use this property to set the trade-off between resampling quality and speed.
       Possible values are rqBest, rqMedium, and rqFastest, of which rqBest sets the best quality but slow resampling time
       and rqFastest sets the lower (but still very good) quality and the fastest resampling time.*)
    property Quality : TResamplerQuality read FQuality write FQuality;
    (* Property: OutSampleRate
      Use this property to set the sample rate for the resulting audio stream.
      The output sample rate may be 256 times greater or 256 times les than the input stream sample rate.*)
    property OutSampleRate : Integer read FOutSampleRate write SetOutSampleRate;
  end;


implementation

  constructor TResampler.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning	in ComponentState) then
    begin
      if not LibsamplerateLoaded then
        raise EAuException.Create(LibsampleratePath + ' library could not be loaded.');
    end;
  end;

  destructor TResampler.Destroy;
  begin
    inherited Destroy;
  end;

  function TResampler.GetCh;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    Result := FInput.Channels;
  end;

  function TResampler.GetBPS;
  begin
    if not Assigned(FInput) then
      raise EAuException.Create('Input not assigned');
    if (FInput.BitsPerSample = 24) and (not FPassThrough) then
      Result := 16
    else
      Result := FInput.BitsPerSample;
  end;

  function  TResampler.GetSR;
  begin
    Result := FOutSampleRate;
  end;

  procedure TResampler.SetOutSampleRate;
  begin
    if (aSR > 2000) and (aSR < 1000000) then
      FOutSampleRate := aSR
    else
      FOutSampleRate := 44100;
  end;

  procedure TResampler.InitInternal;
  var
    Quality, error : Integer;
  begin
    if Busy then
      raise EAuException.Create('The component is busy.');
    EndOfInput := False;
    IBufferEnd := 0;
    OBufferStart := 0;
    OBufferEnd := 0;
    Busy := True;
    FInput.Init;
    FPosition := 0;
    Data.src_ratio := FOutSampleRate/Finput.SampleRate;
    if src_is_valid_ratio(Data.src_ratio) = 0 then
      raise EAuException.Create(Format('Frequences ratio %d is invalid', [Data.src_ratio]));
    Data.data_in := @IFloatBuffer;
    Data.data_out := @OFloatBuffer;
    case FQuality of
      rqBest : Quality := SRC_SINC_BEST_QUALITY;
      rqMedium : Quality := SRC_SINC_MEDIUM_QUALITY;
      rqFastest : Quality := SRC_SINC_FASTEST;
    end;
    _State := src_new(Quality, FInput.Channels, error);
    if error <> 0 then
      raise EAuException.Create('Failed to initialize the resampler');
   // src_set_ratio(_State, FOutSampleRate/Finput.SampleRate);
   if FOutSampleRate = FInput.SampleRate then
     FPassThrough := True
   else
    FPassThrough := False;
    if FPassThrough then
      FSize := FInput.Size
    else
    begin
      FSize := Round(FInput.Size * FOutSampleRate/Finput.SampleRate);
      if FInput.BitsPerSample = 24 then
      begin
        FSize := Round(FSize*2/3);
        FSize := FSize - (FSize mod (2*FInput.Channels));
      end else
        FSize := FSize - (FSize mod (Finput.BitsPerSample* FInput.Channels div 8));
    end;
  end;


  procedure SingleToSmallInt(_in : PFLOATARRAY; _out : PSHORTARRAY; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := Floor(_in[i] * $8000);
  end;

  procedure SmallIntToSingle(_in : PSHORTARRAY; _out : PFLOATARRAY; len : Integer);
  var
    i : Integer;
  begin
    for i := 0 to len - 1 do
      _out[i] := _in[i]/$8000;
  end;

  procedure TResampler.GetDataInternal(var Buffer : Pointer; var Bytes : Integer);
  var
    l, ilen, i : Integer;
    res : Integer;
  begin
    if FPassThrough then
    begin
    // Pass through.
      FInput.GetData(Buffer, Bytes);
      Exit;
    end;
    if OBufferStart >= OBufferEnd then
    begin
      OBufferStart := 0;
      if (IBufferEnd < IOBufSize) and (not EndOfInput) then
      begin
        if FInput.BitsPerSample = 16 then
          l := FInput.CopyData(@InputBuffer[IBufferEnd], IOBufSize - IBufferEnd);
        if FInput.BitsPerSample = 8 then
        begin
          l := FInput.CopyData(@InputBuffer[IBufferEnd], (IOBufSize - IBufferEnd) div 2);
          Convert8To16(@InputBuffer[IBufferEnd], l);
          l := l*2;
        end;
        if FInput.BitsPerSample = 24 then
        begin
          l := (IOBufSize - IBufferEnd) - ((IOBufSize - IBufferEnd) mod 6);
          l := FInput.CopyData(@InputBuffer[IBufferEnd], l);
          Convert24To16(@InputBuffer[IBufferEnd], l);
          l := (l div 3)*2;
        end;
        if l = 0 then
           EndOfInput := True;
        Inc(IBufferEnd, l);
      end;
      if IBufferEnd = 0 then
      begin
        EndOfInput := True;
        OBufferEnd := FSize - FPosition;
        if OBufferEnd < 0 then OBufferEnd := 0;
        FillChar(OutputBuffer[0], OBufferEnd, 0);
      end else
      begin
        ilen := IBufferEnd;
        if IBufferEnd > InitialBufferSize then
          ilen := InitialBufferSize;
        src_short_to_float_array(@InputBuffer, @IFloatBuffer, ilen);
        //SmallIntToSingle(@InputBuffer, @IFloatBuffer, ilen div 2);
        Data.input_frames := (ilen div 2) div FInput.Channels;
        Data.output_frames := (IOBufSize div 2) div FInput.Channels;
        if EndOfInput and (IBufferEnd <= InitialBufferSize) then
          Data.end_of_input := 1
        else
          Data.end_of_input := 0;
        res := src_process(_State, Data);
        if res <> 0 then
        begin
          EndOfInput := True;
          Buffer := nil;
          Bytes := 0;
          raise EAuException.Create(src_strerror(res));
        end;
        SingleToSmallInt(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
        //src_float_to_short_array(@OFloatBuffer, @OutputBuffer, Data.output_frames_gen * FInput.Channels);
        OBufferEnd := Data.output_frames_gen * FInput.Channels * 2;
        ilen := Data.nput_frames_used  * FInput.Channels * 2;
        for i := ilen to IBufferEnd - 1 do
          InputBuffer[i - ilen] := InputBuffer[i];
        Dec(IBufferEnd, ilen);
        if FInput.BitsPerSample = 8 then
        begin
          Convert16To8(@OutputBuffer, OBufferEnd);
          OBufferEnd := OBufferEnd div 2;
        end;
      end;
    end; // OBufferStart >= OBufferEnd
    if OBufferEnd = 0 then
    begin
      EndOfInput := True;
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    if Bytes > OBufferEnd - OBufferStart then
      Bytes := OBufferEnd - OBufferStart;
    Buffer := @OutputBuffer[OBufferStart];
    Inc(OBufferStart, Bytes);
    Inc(FPosition, Bytes);
  end;

  procedure TResampler.FlushInternal;
  begin
//    FSize := 0;
    FInput.Flush;
    src_delete(_State);
    Busy := False;
  end;


end.
