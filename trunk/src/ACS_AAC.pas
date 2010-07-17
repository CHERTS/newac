(*
  This file is a part of New Audio Components package v 2.6
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_AAC;

(* Title: ACS_AAC
    Components for the AAC playback. *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, ACS_WinMedia, mp4ff, neaac;


type

(* Class: TMpgIn
   Yet another mp3 file/stream decoder that uses libmpg123.dll.
   Descends from <TAuTaggedFileIn>.
   This decoder provides better sound than the Windows one but doesn't suppoer the precise positioning.
 *)

  TMP4In = class(TAuTaggedFileIn)
  private
      MP4Handle : mp4ff_t;
      cbs : mp4ff_callback_t;
      R : PChar;
      HDecoder : NeAACDecHandle;
      FTrack : Integer;
      FBytesRead, FBOffset : LongWord;
      FSampleId, FSamples : Integer;
      FTimescale : LongWord;

      _Buf : array[0..1024*1024-1] of Byte;
      FBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    (* Property: Bitrate
    Reurns the file's bitrate in kbps. *)
    property Bitrate : LongWord read FBitrate;
  end;


implementation

  function CBMP4Read(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Read(buffer^, Integer(length)));
    except
      Result := 0;
    end;
  end;

  function CBMP4Write(user_data : Pointer; buffer : Pointer; length : LongWord) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Write(buffer^, Integer(length)));
    except
      Result := 0;
    end;
  end;

  function CBMP4Seek(user_data : Pointer; Position : Int64) : LongWord; cdecl;
  begin
    try
      Result := LongWord(TMP4In(user_data).FStream.Seek(Position, soBeginning));
    except
      Result := 0;
    end;
  end;

  function CBMP4Truncate(user_data : Pointer) : LongWord; cdecl;
  begin
      Result := 0;
  end;


  procedure TMP4In.OpenFile;
  var
    config : NeAACDecConfigurationPtr;
    buffer : PByte;
    buffer_size : LongWord;
    samplerate : LongWord;
    channels : Byte;
    useAacLength : LongWord;
    initial : LongWord;
    framesize : LongWord;
    mp4ASC : mp4AudioSpecificConfig;
    f, seconds : Double;
    tag : PAnsiChar;
  begin
    Loadmp4ff;
    if not Libmp4ffLoaded then
    raise EAuException.Create(Libmp4ffPath + ' library could not be loaded.');
    Loadneaac;
    if not LibneaacLoaded then
    raise EAuException.Create(LibneaacPath + ' library could not be loaded.');
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      cbs.read := CBMP4Read;
      cbs.write := CBMP4Write;
      cbs.seek := CBMP4Seek;
      cbs.truncate := CBMP4Truncate;
      cbs.user_data := Pointer(Self);
      hDecoder := NeAACDecOpen();
      config := NeAACDecGetCurrentConfiguration(hDecoder);
      case config.outputFormat of
       FAAD_FMT_16BIT : FBPS := 16;
       FAAD_FMT_24BIT : FBPS := 24;
       FAAD_FMT_32BIT : FBPS := 32;
       else  raise EAuException.Create('Unsupported sample format.');
      end;
      config.downMatrix := 1; //!!!!
      NeAACDecSetConfiguration(hDecoder, config);
      MP4Handle := mp4ff_open_read(@cbs);
      R := MP4Handle;
      if MP4Handle = nil then
        raise EAuException.Create('Unable to open file.');
      FTrack := GetAACTrack(MP4Handle);
      if FTrack < 0 then
        raise EAuException.Create('Unable to find correct AAC sound track in the MP4 file.');
      buffer := nil;
      mp4ff_get_decoder_config(MP4Handle, FTrack, buffer, buffer_size);
//      Channels := 2;
      if NeAACDecInit2(HDecoder, buffer, buffer_size, samplerate, channels) < 0 then
      begin
        NeAACDecClose(hDecoder);
        mp4ff_close(MP4Handle);
        raise EAuException.Create('Error initializing decoder library.');
      end;
      FSR := mp4ff_get_audio_type(MP4Handle, FTrack); //samplerate;
      FSR := mp4ff_get_sample_rate(MP4Handle, FTrack); //samplerate;
      FChan := mp4ff_get_channel_count(MP4Handle, FTrack);
//      FChan := channels;
      FTimescale := mp4ff_time_scale(MP4Handle, FTrack);
      framesize := 1024;
      useAacLength := 0;
      if buffer <> nil then
      begin
        if NeAACDecAudioSpecificConfig(buffer, buffer_size, mp4ASC) >= 0 then
        begin
          if mp4ASC.frameLengthFlag = 1 then framesize := 960;
          if mp4ASC.sbr_present_flag = 1 then framesize := framesize*2;
        end;
        mp4ff_free_decoder_config(buffer);
      end;
      FSamples := mp4ff_num_samples(MP4Handle, FTrack);
      f := 1024.0;
      if mp4ASC.sbr_present_flag = 1 then
         f := f * 2.0;
      seconds := FSamples*(f-1.0)/mp4ASC.samplingFrequency;
   //   FChan :=  2; //mp4ASC.channelsConfiguration;
      FSR :=  mp4ASC.samplingFrequency;
      FTotalSamples := Trunc(seconds*mp4ASC.samplingFrequency);
      FSize := FTotalSamples*FChan*(FBPS div 8);
      if Fsize > 0 then
        FSeekable := True;
      (*
        j = mp4ff_meta_get_num_items(infile);
        for (k = 0; k < j; k++)
        {
            if (mp4ff_meta_get_by_index(infile, k, &item, &tag))
            {
                if (item != NULL && tag != NULL)
                {
                    faad_fprintf(stderr, "%s: %s\n", item, tag);
                    free(item); item = NULL;
                    free(tag); tag = NULL;
                }
            }
        *)
      _CommonTags.Clear;
      mp4ff_meta_get_artist(MP4Handle, tag);
      _CommonTags.Artist := tag;
      mp4ff_meta_get_title(MP4Handle, tag);
      _CommonTags.Title := tag;
      mp4ff_meta_get_album(MP4Handle, tag);
      _CommonTags.Album := tag;
      mp4ff_meta_get_date(MP4Handle, tag);
      _CommonTags.Year := tag;
      mp4ff_meta_get_genre(MP4Handle, tag);
      _CommonTags.Genre := tag;
      mp4ff_meta_get_track(MP4Handle, tag);
      _CommonTags.Track := tag;

      FValid := True;
      FBOffset := 0;
      FBytesRead := 0;
      FSampleId := 0;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TMP4In.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    dur : LongWord;
    rc : Integer;
    audio_buffer : PByte;
    audio_buffer_size : LongWord;
    sample_buffer : Pointer;
    frameInfo : NeAACDecFrameInfo;
    sample_count : LongWord;
  begin
    if FSampleId > FSamples then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    if FBOffset = FBytesRead then
    begin
    frameInfo.samples := 0;
    while frameInfo.samples = 0 do
    begin
    dur := mp4ff_get_sample_duration(MP4Handle, FTrack, FSampleId);
    audio_buffer_size := 0;
    audio_buffer := nil;
    rc := mp4ff_read_sample(MP4Handle, FTrack, FSampleId, audio_buffer,  audio_buffer_size);
    if rc = 0 then
    begin
      if FPosition >= FSize then
      begin
        Buffer := nil;
        Bytes := 0;
        Exit;
      end;
      raise EAuException.Create('Error reading data.');
    end;
    sample_buffer := NeAACDecDecode(hDecoder, @frameInfo, audio_buffer, audio_buffer_size);
    if audio_buffer <> nil then mp4ff_free_decoder_config(audio_buffer);
    if frameInfo.error <> 0 then
      raise EAuException.Create('Error reading data.');

(*            if (!noGapless)
        {
            if (sampleId == 0) dur = 0;

            if (useAacLength || (timescale != samplerate)) {
                sample_count = frameInfo.samples;
            } else {
                sample_count = (unsigned int)(dur * frameInfo.channels);
                if (sample_count > frameInfo.samples)
                    sample_count = frameInfo.samples;

                if (!useAacLength && !initial && (sampleId < numSamples/2) && (sample_count != frameInfo.samples))
                {
                    faad_fprintf(stderr, "MP4 seems to have incorrect frame duration, using values from AAC data.\n");
                    useAacLength = 1;
                    sample_count = frameInfo.samples;
                }
            }

            if (initial && (sample_count < framesize*frameInfo.channels) && (frameInfo.samples > sample_count))
                delay = frameInfo.samples - sample_count;
        } else {
            sample_count = frameInfo.samples;
        } *)

    sample_count := frameInfo.samples;
//  sample_count := (dur * frameInfo.channels);
    FBytesRead := sample_count*frameInfo.channels;//*(FBPS div 8);
    Move(sample_buffer^, _Buf[0], FBytesRead);
    FBOffset := 0;
    Inc(FSampleId);
    end;
    end; // if FBOffset = FBytesRead then
    if Bytes > FBytesRead - FBOffset then
      Bytes := FBytesRead - FBOffset;
    Buffer := @_Buf[FBOffset];
    Inc(FBOffset, Bytes);
  end;

  procedure TMP4In.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      NeAACDecClose(hDecoder);
      mp4ff_close(MP4Handle);
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  function TMP4In.SeekInternal(var SampleNum: Int64) : Boolean;
  var
    aac_sample : Integer;
  begin
    Result := False;
    if FSeekable then
    begin
      if SampleNum > FTotalSamples then SampleNum := FTotalSamples;
      aac_sample := Trunc(SampleNum/FTotalSamples*FSamples);
      mp4ff_find_sample(MP4Handle, FTrack, 0, aac_sample);
      FPosition := mp4ff_get_sample_position(MP4Handle, FTrack, aac_sample);
      SampleNum := FPosition div (FChan*(FBPS div 8));
      Result := True;
    end;
  end;



end.


