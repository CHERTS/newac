(*
  This file is a part of New Audio Components package v 1.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  ****************************************************************************

  TWVIn and TWVOut components are written by Sergei Borisov, <jr_ross@mail.ru>
*)

unit ACS_WavPack;

interface

uses
  Classes,
  WavPackDLL,
  ACS_Classes,
  ACS_Tags;

type

  { class TWVIn }

  TWVIn = class(TAuTaggedFileIn)
  private
    FCorrectionsStreamAssigned: Boolean;
    FCorrectionsStream: TStream;

    FDecoder: TWavpackDecoder;
//    FEOF: Boolean;
    FBuffer: array of Byte;

    procedure SetCorrectionsStream(Value: TStream);
    function GetId3v1Tags : TId3v1Tags;
    function GetAPEv2Tags : TAPEv2Tags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CorrectionsStream: TStream read FCorrectionsStream write SetCorrectionsStream;
    property Id3v1Tags : TId3v1Tags read GetId3v1Tags;
    property APEv2Tags : TAPEv2Tags read GetAPEv2Tags;
  published
  end;

  { class TWVOut }

  TWVCompressionLevel = (wvclFast, wvclHigh, wvclVeryHigh);

  TWVOut = class(TAuTaggedFileOut)
  private
    FCorrectionsStreamAssigned: Boolean;
    FCorrectionsStream: TStream;

    FJointStereo: Boolean;
    FCompressionLevel: TWVCompressionLevel;
    FHybridMode: Boolean;
    FBitrate: Single;

    FEncoder: TWavpackEncoder;
    FEOF: Boolean;
    FBufferInStart: Integer;
    FBufferIn: array of Byte;
    FBufferOut: array of Byte;

    procedure SetCorrectionsStream(Value: TStream);
  protected
    procedure Prepare; override;
    procedure Done; override;

    function DoOutput(Abort: Boolean): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CorrectionsStream: TStream read FCorrectionsStream write SetCorrectionsStream;

  published
    property JointStereo: Boolean read FJointStereo write FJointStereo default False;
    property CompressionLevel: TWVCompressionLevel read FCompressionLevel write FCompressionLevel default wvclFast;
    property HybridMode: Boolean read FHybridMode write FHybridMode default False;
    property Bitrate: Single read FBitrate write FBitrate;

    property APEv2Tags;
  end;


implementation

uses
  SysUtils, Math, Variants;

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

  t_float_sample = type Single;
  t_float_sample_array = packed array [0 .. 0] of t_float_sample;
  p_float_sample_array = ^t_float_sample_array;

const
  byte_sample_base = t_byte_sample((High(t_byte_sample) shr 1) + 1);

{ class TWVIn }

constructor TWVIn.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not WavpackDLL_Loaded then
    raise EAuException.Create(WavpackDLL_Name + ' library could not be loaded.');
end;

destructor TWVIn.Destroy;
begin
  FDecoder.Free();
  if not FStreamAssigned then
    FStream.Free();
  if not FCorrectionsStreamAssigned then
    FCorrectionsStream.Free();

  inherited;
end;

procedure TWVIn.SetCorrectionsStream(Value: TStream);
begin
  if not Busy and (FCorrectionsStream <> Value) then begin
    if (FCorrectionsStream <> nil) and not FCorrectionsStreamAssigned then
      FCorrectionsStream.Free();

    FCorrectionsStream := Value;
    FCorrectionsStreamAssigned := (FCorrectionsStream <> nil);
  end;
end;

procedure TWVIn.OpenFile;
var
  i, n: Integer;
  tag_id: String;
begin
  OpenCS.Enter;
  try
  if FOpened = 0 then
  begin
    FValid := False;

    if not FStreamAssigned then
      if FWideFileName = '' then
        raise EAuException.Create('File name is not assigned')
      else
        FStream := TAuFileStream.Create(
          FWideFileName, fmOpenRead or fmShareDenyWrite);

    if not FCorrectionsStreamAssigned then
      try
        FCorrectionsStream := TAuFileStream.Create(
          ChangeFileExt(FWideFileName, '.wvc'), fmOpenRead or fmShareDenyWrite);
      except
        FCorrectionsStream := nil;
      end;

    if FCorrectionsStream = nil then
      FDecoder := TWavpackDecoder.Create(FStream,
        nil,
        [wvofTags])
    else
      FDecoder := TWavpackDecoder.Create(FStream,
        FCorrectionsStream,
        [wvofWVC, wvofTags]);
    if FDecoder = nil then
      raise EAuException.Create('Failed to initialize WavPack decoder.');

    FValid := True;
    FSeekable := True;
//    FEOF := False;

    FBPS := FDecoder.BitsPerSample;
    FSR := FDecoder.SampleRate;
    FChan := FDecoder.NumChannels;

    FPosition := 0;
    FSize := FDecoder.NumSamples * (FBPS div 8) * FChan;

    if [wvmfValidTag] * FDecoder.Mode = [wvmfValidTag] then
      if [wvmfAPETag] * FDecoder.Mode = [] then begin // id3v1 tags
        _Id3v1Tags.Artist := FDecoder.Tags['artist'];
        _Id3v1Tags.Album := FDecoder.Tags['album'];
        if TryStrToInt(FDecoder.Tags['year'], n) then
          _Id3v1Tags.Year := n;
        if TryStrToInt(FDecoder.Tags['track'], n) then
          _Id3v1Tags.Track := n;
        _Id3v1Tags.Title := FDecoder.Tags['title'];
        _Id3v1Tags.Comment := FDecoder.Tags['comment'];

        _APEv2Tags.Clear();
      end
      else begin
        _Id3v1Tags.Clear();

        for i := 0 to _APEv2Tags.IdCount - 1 do begin
          tag_id := _APEv2Tags.Ids[i];
          _APEv2Tags[tag_id] := FDecoder.Tags[tag_id];
        end;
      end
    else begin
      _Id3v1Tags.Clear();
      _APEv2Tags.Clear();
    end;
    Inc(FOpened);
  end;
  finally
    OpenCS.Leave;
  end;
end;

procedure TWVIn.CloseFile;
begin
  OpenCS.Enter;
  try
  if FOpened > 0 then begin
    FOpened := 0;

    if FOpened = 0 then begin
      FreeAndNil(FDecoder);
      if (not FStreamAssigned) and (FStream <> nil) then begin
        FreeAndNil(FStream);
        FreeAndNil(FCorrectionsStream);
      end;

      SetLength(FBuffer, 0);
    end;
  end;
  finally
    OpenCS.Leave;
  end;
end;

function TWVIn.SeekInternal(var Sample: Int64): Boolean;
begin
  OpenFile();
  try
    Result := FDecoder.SeekSample(Sample);
  finally
  end;
end;

procedure TWVIn.GetDataInternal(var Buffer: Pointer; var Bytes: Integer);
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
  new_bytes, new_buffer_size,
  in_sample_size, out_sample_size,
  samples, samples_read,
  i: Integer;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
  p_int32_samples: p_int32_sample_array;
  p_float_samples: p_float_sample_array;
  temp: t_int32_sample;
begin
  if not Busy then
    raise EAuException.Create('The Stream is not opened');

  if Bytes > 0 then
  begin

    try
      if FSize >= 0 then begin
        new_bytes := FSize - FPosition;
        if Bytes > new_bytes then
          Bytes := new_bytes;
      end;

      in_sample_size := SizeOf(p_int32_samples[Low(p_int32_samples^)]) * FChan;
      out_sample_size := (FBPS shr 3) * FChan;
      samples := Bytes div out_sample_size;
      Bytes := samples * out_sample_size;

      new_buffer_size := samples * in_sample_size;
      if Length(FBuffer) < new_buffer_size then
        SetLength(FBuffer, new_buffer_size);

      Buffer := @(FBuffer[0]);

      samples_read := FDecoder.UnpackSamples(Buffer, samples);

      if samples_read < samples then begin
        if samples_read > 0 then begin
          samples := samples_read;
          Bytes := samples * out_sample_size;
        end
        else begin
          Buffer := nil;
          Bytes := 0;

          Exit;
        end;
      end;

      if [wvmfFloat] * FDecoder.Mode = [wvmfFloat] then begin
        p_float_samples := Buffer;
        case FBPS shr 3 of
          1: begin // 8 bits per sample
            p_byte_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              if p_float_samples[i] >= 1 then
                p_byte_samples[i] := High(p_byte_samples[i])
              else
              if p_float_samples[i] <= - 1 then
                p_byte_samples[i] := Low(p_byte_samples[i])
              else
                p_byte_samples[i] := byte_sample_base +
                  Floor(p_float_samples[i] * byte_sample_base);
          end;
          2: begin // 16 bits per sample
            p_int16_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              if p_float_samples[i] >= 1 then
                p_int16_samples[i] := High(p_int16_samples[i])
              else
              if p_float_samples[i] <= - 1 then
                p_int16_samples[i] := Low(p_int16_samples[i])
              else
                p_int16_samples[i] := Floor(p_float_samples[i] * High(p_int16_samples[i]));
          end;
          3: begin // 24 bits per sample
            p_int24_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              if p_float_samples[i] >= 1 then
                p_int24_samples[i] := int24_sample_high
              else
              if p_float_samples[i] <= - 1 then
                p_int24_samples[i] := int24_sample_low
              else begin
                temp := Floor(p_float_samples[i] * High(temp)) shr 8;

                p_int24_samples[i] := t_int24_sample_in_int32(temp).sample;
              end;
          end;
          4: begin // 32 bits per sample
            p_int32_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              if p_float_samples[i] >= 1 then
                p_int32_samples[i] := High(p_int32_samples[i])
              else
              if p_float_samples[i] <= - 1 then
                p_int32_samples[i] := Low(p_int32_samples[i])
              else
                p_int32_samples[i] := Floor(p_float_samples[i] * High(p_int32_samples[i]));
          end;
        end;
      end
      else begin
        p_int32_samples := Buffer;
        case FBPS shr 3 of
          1: begin // 8 bits per sample
            p_byte_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              p_byte_samples[i] := byte_sample_base +
                t_byte_sample(p_int32_samples[i]);
          end;
          2: begin // 16 bits per sample
            p_int16_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              p_int16_samples[i] := p_int32_samples[i];
          end;
          3: begin // 24 bits per sample
            p_int24_samples := Buffer;
            for i := 0 to (samples_read * FChan) - 1 do
              p_int24_samples[i] := t_int24_sample_in_int32(p_int32_samples[i]).sample;
          end;
          {4: begin // 32 bits per sample
            // nothing to do
          end;}
        end;
      end;

    finally
    end;
  end;
end;


{ class TWVOut }

constructor TWVOut.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not WavpackDLL_Loaded then
    raise EAuException.Create(WavpackDLL_Name + ' library could not be loaded.');
end;

destructor TWVOut.Destroy;
begin
  FEncoder.Free();
  if not FStreamAssigned then
    FStream.Free();
  if not FCorrectionsStreamAssigned then
    FCorrectionsStream.Free();

  inherited;
end;

procedure TWVOut.SetCorrectionsStream(Value: TStream);
begin
  if not Busy and (FCorrectionsStream <> Value) then begin
    if (FCorrectionsStream <> nil) and not FCorrectionsStreamAssigned then
      FCorrectionsStream.Free();

    FCorrectionsStream := Value;
    FCorrectionsStreamAssigned := (FCorrectionsStream <> nil);
  end;
end;

procedure TWVOut.Prepare;
const
  flags_for_compression_levels: array [TWVCompressionLevel] of TwvConfigFlags = (
    [wvcfFastFlag], [wvcfHighFlag], [wvcfVeryHighFlag]);
var
  bytes_per_sample: Integer;
  flags: TwvConfigFlags;
begin
  FEOF := False;

  if not FStreamAssigned then
    if FWideFileName = '' then
      raise EAuException.Create('File name is not assigned')
    else
      FStream := TAuFileStream.Create(
        FWideFileName, fmCreate or fmShareDenyWrite);

  if FHybridMode then
    if not FCorrectionsStreamAssigned then
      try
        FCorrectionsStream := TAuFileStream.Create(
          ChangeFileExt(FWideFileName, '.wvc'), fmCreate or fmShareDenyWrite);
      except
        FCorrectionsStream := nil;
      end;

  FInput.Init();

  FEncoder := TWavpackEncoder.Create(FStream, FCorrectionsStream);

  FEncoder.BitsPerSample := FInput.BitsPerSample;
  FEncoder.NumChannels := FInput.Channels;
  FEncoder.SampleRate := FInput.SampleRate;

  flags := flags_for_compression_levels[FCompressionLevel];
  if FJointStereo then
    flags := flags + [wvcfJointStereo, wvcfJointOverride];
  if FHybridMode then begin
    flags := flags + [wvcfHybridFlag, wvcfBitrateKbps, wvcfCreateWVC, wvcfOptimizeWVC];

    FEncoder.Bitrate := FBitrate;
  end;
  FEncoder.Flags := flags;

  bytes_per_sample := FInput.BitsPerSample shr 3;
  if FInput.BitsPerSample and $7 > 0 then
    Inc(bytes_per_sample);
  FEncoder.BytesPerSample := bytes_per_sample;

  case FInput.Channels of
    1: FEncoder.ChannelMask := 4; // MONO
    2: FEncoder.ChannelMask := 3; // STEREO
    else
      FEncoder.ChannelMask := 0;
  end;

  if not FEncoder.Init(FInput.Size div (FInput.Channels * (FInput.BitsPerSample shr 3))) then
    raise EAuException.CreateFmt(
      'Wavpack file Init failed! (error: "%s")', [FEncoder.LastError]);
end;

procedure TWVOut.Done;
var
  i: integer;
  tag_id: String;
  tag_value: Variant;
begin
  if FEncoder <> nil then begin
    if not APEv2Tags.Empty then begin
      for i := 0 to APEv2Tags.IdCount - 1 do begin
        tag_id := APEv2Tags.Ids[i];
        tag_value := APEv2Tags[tag_id];
        if VarIsType(tag_value, [varString, varOleStr]) then
          FEncoder.Tags[tag_id] := tag_value;
      end;
      FEncoder.WriteTags();
    end;

    FreeAndNil(FEncoder);
  end;

  FInput.Flush();

  if not FStreamAssigned then
    FreeAndNil(FStream);
  if not FCorrectionsStreamAssigned then
    FreeAndNil(FCorrectionsStream);
end;

function TWVOut.DoOutput(Abort: Boolean) : Boolean;
type
  t_int24_sample_in_int32 = packed record
    lo: Word;
    hi: Smallint;
  end;
  p_int24_sample_in_int32 = ^t_int24_sample_in_int32;
var
  buffer: Pointer;
  frame_size, bytes, buffer_in_size, buffer_out_size, frames, samples, i: Integer;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
  p_int32_samples: p_int32_sample_array;
begin
  Result := CanOutput;
  if not Result then
    Exit; 

  FEOF := FEOF or Abort;
  if FEOF then begin
    Result := False;

    Exit;
  end;

  frame_size := FInput.Channels * (FInput.BitsPerSample shr 3);
  bytes := 1024 * frame_size;
  FInput.GetData(buffer, bytes);
  Result := (bytes > 0);
  if Result then begin
    buffer_in_size := FBufferInStart + bytes;
    if Length(FBufferIn) < buffer_in_size then
      SetLength(FBufferIn, buffer_in_size);
    Move(buffer^, FBufferIn[FBufferInStart], bytes);

    frames := buffer_in_size div frame_size;
    samples := frames * FInput.Channels;

    buffer_out_size := samples * SizeOf(p_int32_samples[Low(p_int32_samples^)]);
    if Length(FBufferOut) < buffer_out_size then
      SetLength(FBufferOut, buffer_out_size);

    p_int32_samples := @(FBufferOut[0]);
    case FInput.BitsPerSample shr 3 of
      1: begin // 8 bits per sample
        p_byte_samples := @(FBufferIn[0]);
        for i := 0 to samples - 1 do
          p_int32_samples[i] :=
            (t_int32_sample(p_byte_samples[i]) - t_int32_sample(byte_sample_base)) and $FF;
      end;
      2: begin // 16 bits per sample
        p_int16_samples := @(FBufferIn[0]);
        for i := 0 to samples - 1 do
          p_int32_samples[i] := p_int16_samples[i]; 
      end;
      3: begin // 24 bits per sample
        p_int24_samples := @(FBufferIn[0]);
        for i := 0 to samples - 1 do begin
          t_int24_sample_in_int32(p_int32_samples[i]).lo := p_int24_samples[i].lo;
          t_int24_sample_in_int32(p_int32_samples[i]).hi := p_int24_samples[i].hi;
        end;
      end;
      {4: begin // 32 bits per sample
        // nothing to do
      end;}
    end;

    Result :=
      FEncoder.PackSamples(p_int32_samples, frames) and
      FEncoder.FlushSamples();

    bytes := frames * frame_size;
    if bytes < buffer_in_size then begin
      FBufferInStart := buffer_in_size - bytes;
      Move(FBufferIn[bytes], FBufferIn[0], FBufferInStart);
    end
    else
      FBufferInStart := 0;
  end;

  FEOF := not Result;
end;

function TWVIn.GetId3v1Tags;
begin
  OpenFile;
  Result := _Id3v1Tags;
//  CloseFile;
end;

function TWVIn.GetAPEv2Tags;
begin
  OpenFile;
  Result := _APEv2Tags;
//  CloseFile;
end;

end.

