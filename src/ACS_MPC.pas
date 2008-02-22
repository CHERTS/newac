(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
  *************************************************************

   TMPCIn component is written by Sergei Borisov, <jr_ross@mail.ru>
*)


unit ACS_MPC;

(* $Id *)

interface

uses
  Classes,
  libmpdec,
  ACS_Classes,
  ACS_Tags;

const
  DefaultMPCBitsPerSample = 16;

type

  (* class TMPCIn *)

  TMPCInBuffer = array of Byte;

  TMPCIn = class(TAuTaggedFileIn)
  private
    FDecoder: TMPCDecoder;
    FBuffer: TMPCInBuffer;
    FBufferRest: TMPCInBuffer;

    procedure SetBPS(Value: Cardinal);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BPS: Cardinal read FBPS write SetBPS default DefaultMPCBitsPerSample;
  end;

implementation

uses
  SysUtils, Math;

type
  t_byte_sample = type Byte;
  t_byte_sample_array = packed array [0 .. 0] of t_byte_sample;
  p_byte_sample_array = ^t_byte_sample_array;

  t_int16_sample = type Smallint;
  t_int16_sample_array = packed array [0 .. 0] of t_int16_sample;
  p_int16_sample_array = ^t_int16_sample_array;

  t_int24_sample = packed record
    lo: Word;
    hi: Shortint;
  end;
  t_int24_sample_array = packed array [0 .. 0] of t_int24_sample;
  p_int24_sample_array = ^t_int24_sample_array;

  t_int32_sample = type Integer;
  t_int32_sample_array = packed array [0 .. 0] of t_int32_sample;
  p_int32_sample_array = ^t_int32_sample_array;

const
  byte_sample_base = t_byte_sample((High(t_byte_sample) shr 1) + 1);

{ class TMPCIn }

constructor TMPCIn.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not libMPDec_Loaded then
    raise EAuException.Create(libMPDec_Name + ' library could not be loaded.');

  SetBPS(DefaultMPCBitsPerSample);
end;

procedure TMPCIn.SetBPS(Value: Cardinal);
begin
  OpenCS.Enter();
  try
    if FOpened = 0 then
      if Value < 8 then
        FBPS := 8
      else
      if Value > 32 then
        FBPS := 32
      else
        FBPS := ((Value + 7) shr 3) shl 3;
  finally
    OpenCS.Leave();
  end;
end;

procedure TMPCIn.OpenFile;
begin
  OpenCS.Enter();
  try
    if FOpened = 0 then begin
      FValid := False;

      if not FStreamAssigned then
        if FWideFileName = '' then
          raise EAuException.Create('File name is not assigned')
        else
          FStream := TAuFileStream.Create(
            FWideFileName, fmOpenRead or fmShareDenyWrite);

      FDecoder := TMPCDecoder.Create(FStream);

      FValid := True;
      FSeekable := True;

      FSR := FDecoder.SampleRate;
      FChan := FDecoder.NumChannels;
      FTotalSamples := FDecoder.NumSamples;

      FPosition := 0;
      if FTotalSamples >= 0 then
        FSize := FTotalSamples * ((FBPS + 7) shr 3) * FChan
      else
        FSize := - 1;

      Inc(FOpened);
    end;
  finally
    OpenCS.Leave();
  end;
end;

procedure TMPCIn.CloseFile;
begin
  OpenCS.Enter();
  try
    if FOpened > 0 then begin
      Dec(FOpened);

      if FOpened = 0 then begin
        try
          FDecoder.Free();
        finally
          FDecoder := nil;
        end;

        if not FStreamAssigned then
          FreeAndNil(FStream);

        SetLength(FBuffer, 0);
      end;
    end;
  finally
    OpenCS.Leave;
  end;
end;

function TMPCIn.SeekInternal(var Sample: Int64): Boolean;
begin
  Result := FDecoder.Seek(Sample);
end;

procedure TMPCIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
type
  t_int24_sample_in_int32 = packed record
    sample: t_int24_sample;
    sign: Shortint;
  end;
  p_int24_sample_in_int32 = ^t_int24_sample_in_int32;
const
  int24_sample_high: t_int24_sample = (
    lo: High(int24_sample_high.lo); hi: High(int24_sample_high.hi));
  int24_sample_low: t_int24_sample = (
    lo: Low(int24_sample_low.lo); hi: Low(int24_sample_low.hi));
var
  new_bytes, status, samples, n: Cardinal;
  buf: TMPCDecoderBuffer;
  i: Integer;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
  p_int32_samples: p_int32_sample_array;
  temp: t_int32_sample;
begin
  if not Busy then
    raise EAuException.Create('File for input is not opened!');

  if Bytes > 0 then begin
    if FSize >= 0 then begin
      new_bytes := FSize - FPosition;
      if Bytes > new_bytes then
        Bytes := new_bytes;
    end;

    FBuffer := FBufferRest;

    while Bytes > Cardinal(Length(FBuffer)) do begin
      FillChar(buf[Low(buf)], Length(buf) * SizeOf(buf[Low(buf)]), 0);

      status := FDecoder.Decode(buf);
      if (status = 0) or (status = Cardinal(Integer(- 1))) then begin
        Bytes := Length(FBuffer);

        Break;
      end;

      samples := status * FDecoder.NumChannels;
      new_bytes := samples * ((FBPS + 7) shr 3);

      n := Length(FBuffer);
      SetLength(FBuffer, n + new_bytes);

      case (FBPS + 7) shr 3 of
        1: begin // 8 bits per sample
          p_byte_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_byte_samples[i] := High(p_byte_samples[i])
            else
            if buf[i] <= - 1 then
              p_byte_samples[i] := Low(p_byte_samples[i])
            else
              p_byte_samples[i] := byte_sample_base +
                Floor(buf[i] * byte_sample_base);
        end;
        2: begin // 16 bits per sample
          p_int16_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int16_samples[i] := High(p_int16_samples[i])
            else
            if buf[i] <= - 1 then
              p_int16_samples[i] := Low(p_int16_samples[i])
            else
              p_int16_samples[i] := Floor(buf[i] * High(p_int16_samples[i]));
        end;
        3: begin // 24 bits per sample
          p_int24_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int24_samples[i] := int24_sample_high
            else
            if buf[i] <= - 1 then
              p_int24_samples[i] := int24_sample_low
            else begin
              temp := Floor(buf[i] * High(temp)) shr 8;

              p_int24_samples[i] := t_int24_sample_in_int32(temp).sample;
            end;
        end;
        4: begin // 32 bits per sample
          p_int32_samples := @(FBuffer[n]);
          for i := 0 to samples - 1 do
            if buf[i] >= 1 then
              p_int32_samples[i] := High(p_int32_samples[i])
            else
            if buf[i] <= - 1 then
              p_int32_samples[i] := Low(p_int32_samples[i])
            else
              p_int32_samples[i] := Floor(buf[i] * High(p_int32_samples[i]));
        end;
      end;
    end;

    if Cardinal(Length(FBuffer)) > Bytes then begin
      SetLength(FBufferRest, Cardinal(Length(FBuffer)) - Bytes);
      Move(FBuffer[Bytes], FBufferRest[0], Length(FBufferRest));
    end
    else
      SetLength(FBufferRest, 0);

    Buffer := @(FBuffer[0]);
  end;
end;

end.

