(*
  This file is a part of New Audio Components package v 1.4
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
  *************************************************************

   TTTAIn and TTTAOut components are written by Sergei Borisov, <jr_ross@mail.ru>
*)


unit ACS_TTA;

(* $Id$ *)

(* Title: ACS_TTA
    TTA (True Audio codec) encoder and decoder components.
    Requires TTALib.dll. *)


interface

uses
  Windows, Classes,
  TTALib,
  ACS_Classes,
  ACS_Tags;

type

  { class TTTAIn }

  (* Class: TTTAIn
    The TTA decoder component, descends from <TAuTaggedFileIn>. Note that
    TTA files are not seekable. Requires TTALib.dll.
  *)
  TTTAIn = class(TAuTaggedFileIn)
  private
    FFile: HFILE;
    FDecoder: TTTADecoder;
    FEOF: Boolean;
    FBuffer: array of Byte;
    FBufferRest: array of Byte;

    function GetId3v1Tags: TId3v1Tags;

    procedure InitDecoder;
    procedure DoneDecoder;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;

    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: LongWord); override;
  public
    constructor Create(AOwner: TComponent); override;

  (* Property: Id3v1Tags
       This property returns the file tags in Id3v1Tags format. *)
    property Id3v1Tags: TId3v1Tags read GetId3v1Tags;
  end;

  { class TTTAOut }

    (* Class: TTTAOut
      The TTA encoder component, descends from <TAuTaggedFileOut>. 
      Requires TTALib.dll. *)

  TTTAOut = class(TAuTaggedFileOut)
  private
    FFile: HFILE;
    FEncoder: TTTAEncoder;
    FEOF: Boolean;
    FBufferInStart: LongWord;
    FBufferIn: array of Byte;
    FBufferOut: array of Byte;
  protected
    procedure Prepare; override;
    procedure Done; override;

    function DoOutput(Abort: Boolean): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    (* Property: Id3v1Tags
         This property allows you to set the file tags in Id3v1Tags format. *)
    property Id3v1Tags;
  end;

implementation

uses
  SysUtils, Variants;

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
  max_frame_count = 1024;
  byte_sample_base = t_byte_sample((High(t_byte_sample) shr 1) + 1);

function CreateTempFileName(const Prefix: WideString): WideString;
var
  temp_file_path: WideString;
begin
  SetLength(temp_file_path, MAX_PATH);
  GetTempPathW(Length(temp_file_path), @(temp_file_path[1]));
  SetLength(Result, MAX_PATH);
  GetTempFileNameW(@(temp_file_path[1]), @(Prefix[1]), 0, @(Result[1]));
end;


{ class TTTAIn }

constructor TTTAIn.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not TTALib_Loaded then
    raise EAuException.Create(TTALib_Name + ' library could not be loaded.');

  FSeekable := True;
end;

function TTTAIn.GetId3v1Tags: TId3v1Tags;
begin
  Result := _Id3v1Tags;
end;

procedure TTTAIn.InitDecoder;
begin
  FDecoder := TTTADecoder.Create(FFile);

  FSeekable := False;

  FPosition := 0;
  FEOF := False;
end;

procedure TTTAIn.DoneDecoder;
begin
  FreeAndNil(FDecoder);

  FSeekable := True;

  if FFile <> INVALID_HANDLE_VALUE then begin
    CloseHandle(FFile);
    FFile := INVALID_HANDLE_VALUE;
  end;

  SetLength(FBuffer, 0);
end;

procedure TTTAIn.OpenFile;
const
  access: array [Boolean] of Cardinal = (
    GENERIC_READ, GENERIC_READ or GENERIC_WRITE);
  share: array [Boolean] of Cardinal = (
    FILE_SHARE_READ, 0);
  disposition: array [Boolean] of Cardinal = (
    OPEN_EXISTING, CREATE_ALWAYS);
  attrs: array [Boolean] of Cardinal = (
    FILE_ATTRIBUTE_NORMAL,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE);
  error: array [Boolean] of String = (
    'Unable to open file for input!', 'Unable to create temporary file!');
var
  file_name: WideString;
  size: Cardinal;
  h_mapping: THandle;
  p_mapping: Pointer;
begin
  OpenCS.Enter();
  try
    if FOpened = 0 then begin
      FValid := False;

      if FStreamAssigned then
        file_name := CreateTempFileName('pcm')
      else
      if FWideFileName <> '' then
        file_name := FWideFileName
      else
        raise EAuException.Create('File name is not defined!');

      FFile := CreateFileW(
        @(file_name[1]),
        access[FStreamAssigned],
        share[FStreamAssigned],
        nil,
        disposition[FStreamAssigned],
        attrs[FStreamAssigned] or FILE_FLAG_RANDOM_ACCESS,
        0);
      if FFile = INVALID_HANDLE_VALUE then
        raise EAuException.Create(error[FStreamAssigned]);

      if FStreamAssigned then begin
        size := FStream.Size;
        if size > 0 then begin
          h_mapping := CreateFileMapping(
            FFile,
            nil,
            PAGE_READWRITE,
            0, size,
            nil);
          if h_mapping <> 0 then
            try
              p_mapping := MapViewOfFile(h_mapping, FILE_MAP_WRITE, 0, 0, 0);
              if p_mapping <> nil then
                try
                  FStream.Seek(0, soFromBeginning);
                  FStream.Read(p_mapping^, size);
                finally
                  UnmapViewOfFile(p_mapping);
                end;
            finally
              CloseHandle(h_mapping);
            end
          else
            raise EAuException.Create('Unable to map temporary file to memory!');
        end;
      end;

      _Id3v1Tags.ReadFromFile(FFile);

      SetFilePointer(FFile, 0, nil, FILE_BEGIN);

      try
        InitDecoder();
      except
        CloseHandle(FFile);
        FFile := INVALID_HANDLE_VALUE;

        raise;
      end;

      FValid := True;

      FBPS := FDecoder.BitsPerSample;
      FSR := FDecoder.SampleRate;
      FChan := FDecoder.NumChannels;
      FTotalSamples := FDecoder.DataLength;

      if FTotalSamples >= 0 then begin
        FSize := FTotalSamples * ((FBPS + 7) shr 3) * FChan;
        FTime := FTotalSamples div FSR;
      end
      else
        FSize := - 1;
      Inc(FOpened);
    end;
  finally
    OpenCS.Leave();
  end;
end;

procedure TTTAIn.CloseFile;
begin
  OpenCS.Enter();
  try
    if FOpened > 0 then begin
      Dec(FOpened);

      if FOpened = 0 then
        DoneDecoder();
    end;
  finally
    OpenCS.Leave();
  end;
end;

function TTTAIn.SeekInternal(var Sample: Int64): Boolean;
begin
  Result := False;
end;

procedure TTTAIn.GetDataInternal(var Buffer: Pointer; var Bytes: LongWord);
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
  new_bytes,
  frame_size, frames, n, i: LongWord;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
{$IFDEF USE_IEEE_FLOAT}
  p_int32_samples: p_int32_sample_array;
{$ENDIF}
  p_in_samples: p_int32_sample_array;
begin
  if not Busy then
    raise EAuException.Create('File for input is not opened!');

  if Bytes > 0 then begin

    if FSize >= 0 then begin
      new_bytes := FSize - FPosition;
      if Bytes > new_bytes then
        Bytes := new_bytes;
    end;

    frame_size := ((FBPS + 7) shr 3) * FChan;

    SetLength(FBuffer, Length(FBufferRest));
    Move(FBufferRest[0], FBuffer[0], Length(FBuffer));

    while Bytes > LongWord(Length(FBuffer)) do begin
      frames := FDecoder.GetBlock(@p_in_samples);
      if frames = 0 then begin
        Bytes := Length(FBuffer);

        FEOF := True;

        Break;
      end;

      n := Length(FBuffer);
      SetLength(FBuffer, n + (frames * frame_size));

      case (FBPS + 7) shr 3 of
        1: begin // 8 bits per sample
          p_byte_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_byte_samples[i] := byte_sample_base +
              t_byte_sample(p_in_samples[i]);
        end;
        2: begin // 16 bits per sample
          p_int16_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_int16_samples[i] := p_in_samples[i];
        end;
        3: begin // 24 bits per sample
          p_int24_samples := @(FBuffer[n]);
          for i := 0 to (frames * FChan) - 1 do
            p_int24_samples[i] := t_int24_sample_in_int32(p_in_samples[i]).sample;
        end;
{$IFDEF USE_IEEE_FLOAT}
        4: begin // 32 bits per sample
          ?
        end;
{$ENDIF}
      end;
    end;

    if LongWord(Length(FBuffer)) > Bytes then begin
      SetLength(FBufferRest, LongWord(Length(FBuffer)) - Bytes);
      Move(FBuffer[Bytes], FBufferRest[0], Length(FBufferRest));
    end
    else
      SetLength(FBufferRest, 0);

    Buffer := @(FBuffer[0]);

  end;
end;


{ class TTTAOut }

constructor TTTAOut.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not TTALib_Loaded then
    raise EAuException.Create(TTALib_Name + ' library could not be loaded.');
end;

procedure TTTAOut.Prepare;
const
  attrs: array [Boolean] of Cardinal = (
    FILE_ATTRIBUTE_NORMAL,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE);
  error: array [Boolean] of String = (
    'Unable to create file for output!', 'Unable to create temporary file!');
{$IFDEF USE_IEEE_FLOAT}
  wave_formats: array [Boolean] of Word = (
    WAVE_FORMAT_PCM, WAVE_FORMAT_IEEE_FLOAT);
{$ENDIF}
var
  file_name: WideString;
  frame_size: Integer;
begin
  FEOF := False;

  if FStreamAssigned then
    file_name := CreateTempFileName('tta')
  else
  if FWideFileName <> '' then
    file_name := FWideFileName
  else
    raise EAuException.Create('File name is not defined!');

  FFile := CreateFileW(
    @(file_name[1]),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    CREATE_ALWAYS,
    attrs[FStreamAssigned] or FILE_FLAG_RANDOM_ACCESS,
    0);
  if FFile = INVALID_HANDLE_VALUE then
    raise EAuException.Create(error[FStreamAssigned]);

  FInput.Init();
  try
    if FInput.BitsPerSample < 32 then begin
      frame_size := FInput.Channels * ((FInput.BitsPerSample + 7) shr 3);

      try
        FEncoder := TTTAEncoder.Create(
          FFile,
          False,
{$IFDEF USE_IEEE_FLOAT}
          wave_formats[FInput.BitsPerSample = 32],
{$ELSE}
          WAVE_FORMAT_PCM,
{$ENDIF}
          FInput.Channels, FInput.BitsPerSample, FInput.SampleRate,
          FInput.Size div frame_size);
      except
        CloseHandle(FFile);
        FFile := INVALID_HANDLE_VALUE;
      end;
    end
    else
      raise EAuException.Create('Defined BitsPerSample is not supported!');
  except
    FInput.Reset();

    raise;
  end;
end;

procedure TTTAOut.Done;
var
  size: Cardinal;
  h_mapping: THandle;
  p_mapping: Pointer;
begin
  FreeAndNil(FEncoder);

  if FFile <> INVALID_HANDLE_VALUE then
    try
      ID3v1Tags.WriteToFile(FFile);

      if FStreamAssigned then begin
        size := GetFileSize(FFile, nil);
        if size > 0 then begin
          h_mapping := CreateFileMapping(
            FFile,
            nil,
            PAGE_READONLY,
            0, size,
            nil);
          if h_mapping <> 0 then
            try
              p_mapping := MapViewOfFile(h_mapping, FILE_MAP_READ, 0, 0, 0);
              if p_mapping <> nil then
                try
                  if FFileMode = foRewrite then
                    FStream.Seek(0, soFromBeginning)
                  else // foAppend
                    FStream.Seek(0, soFromEnd);

                  FStream.Write(p_mapping^, size);
                finally
                  UnmapViewOfFile(p_mapping);
                end;
            finally
              CloseHandle(h_mapping);
            end
          else
            raise EAuException.Create('Unable to map temporary file to memory!');
        end;
      end
      else
        FlushFileBuffers(FFile);
    finally
      CloseHandle(FFile);
      FFile := INVALID_HANDLE_VALUE;
    end;

  FInput.Flush();

  if not FStreamAssigned then
    FreeAndNil(FStream);
end;

function TTTAOut.DoOutput(Abort: Boolean) : Boolean;
type
  t_int24_sample_in_int32 = packed record
    lo: Word;
    hi: Smallint;
  end;
  p_int24_sample_in_int32 = ^t_int24_sample_in_int32;
{$IFDEF USE_IEEE_FLOAT}
  t_int32_sample_in_int64 = packed record
    sample: {Integer}Cardinal;
    dumb: Integer;
  end;
  p_int32_sample_in_int64 = ^t_int32_sample_in_int64;
  t_int64_sample_array = packed array [0 .. 0] of t_int32_sample_in_int64;
  p_int64_sample_array = ^t_int64_sample_array;*)
{$ENDIF}
var
  buffer: Pointer;
  frame_size, bytes, buffer_in_size, buffer_out_size, frames, samples, i: LongWord;
  p_byte_samples: p_byte_sample_array;
  p_int16_samples: p_int16_sample_array;
  p_int24_samples: p_int24_sample_array;
  p_int32_samples: p_int32_sample_array;
{$IFDEF USE_IEEE_FLOAT}
  p_int64_samples: p_int64_sample_array;
{$ENDIF}
begin
  Result := CanOutput;
  if not Result then
    Exit; 

  FEOF := FEOF or Abort;
  if FEOF then begin
    Result := False;

    Exit;
  end;

  frame_size := FInput.Channels * ((FInput.BitsPerSample + 7) shr 3);
  bytes := max_frame_count * frame_size;
  FInput.GetData(buffer, bytes);
  Result := (bytes > 0);
  if Result then begin
    buffer_in_size := FBufferInStart + bytes;
    if LongWord(Length(FBufferIn)) < buffer_in_size then
      SetLength(FBufferIn, buffer_in_size);
    Move(buffer^, FBufferIn[FBufferInStart], bytes);

    frames := buffer_in_size div frame_size;
    samples := frames * FInput.Channels;

    buffer_out_size := samples * SizeOf(p_int32_samples[Low(p_int32_samples^)]);
{$IFDEF USE_IEEE_FLOAT}
    if FInput.BitsPerSample = 32 then begin
      buffer_out_size := buffer_out_size shl 1;

      if Length(FBufferOut) < buffer_out_size then
        SetLength(FBufferOut, buffer_out_size);

      p_int64_samples := @(FBufferOut[0]);
      p_int32_samples := @(FBufferIn[0]);
      for i := 0 to samples - 1  do begin
        if p_int32_samples[i] = 0 then
          p_int64_samples[i].sample := 0
        else begin
          if p_int32_samples[i] < 0 then
            p_int64_samples[i].sample := $8000000
          else
            p_int64_samples[i].sample := 0;
?          p_int64_samples[i].sample := p_int64_samples[i].sample or
            (p_int32_samples[i] div 32768)
            

          p_int64_samples[i].sample := 1 / p_int32_samples[i];
        end;
        p_int64_samples[i].dumb := 0;
      end;
    end
    else begin
{$ENDIF}
      if LongWord(Length(FBufferOut)) < buffer_out_size then
        SetLength(FBufferOut, buffer_out_size);

      p_int32_samples := @(FBufferOut[0]);
      case (FInput.BitsPerSample + 7) shr 3 of
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
      end;
{$IFDEF USE_IEEE_FLOAT}
    end;
{$ENDIF}

    Result := FEncoder.CompressBlock(@(FBufferOut[0]), frames);

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

end.

