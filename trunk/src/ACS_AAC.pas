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
      HDecoder : NeAACDecHandle;
      FTrack : Integer;
      FBytesRead, FBOffset : LongWord;
//      _Buf : array[0.._lBufSize - 1] of Byte;
      FBitrate : LongWord;
      procedure FindTags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
    function GetId3v1Tags : TId3v1Tags;
    function GetId3v2Tags : TId3v2Tags;
  public
    (* Property: Id3v1Tags
    Reurns the file's Id3v1Tags if available.  *)
    property Id3v1Tags : TId3v1Tags read GetId3v1Tags;
    (* Property: Id3v2Tags
    Reurns the file's Id3v2Tags if available.
    If Id3v2Tags are not present in the file, the Id3v1Tags values are returned by this proeprty. *)
    property Id3v2Tags : TId3v2Tags read GetId3v2Tags;
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
    cbs : mp4ff_callback_t;
    config : NeAACDecConfigurationPtr;
    buffer : Pointer;
    buffer_size : Integer;
    samplerate : LongWord;
    channels : Byte;
    useAacLength : LongWord;
    initial : LongWord;
    framesize : LongWord;
    timescale : LongWord;
    mp4ASC : mp4AudioSpecificConfig;
  begin
    Loadmp4ff;
    if not Libmp4ffLoaded then
    raise EAuException.Create(Libmp4ffPath + ' library could not be loaded.');
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
      config.outputFormat := FAAD_FMT_16BIT; //!!!
      FBPS := 16;
      config.downMatrix := 1; //!!!!
      NeAACDecSetConfiguration(hDecoder, config);
      MP4Handle := mp4ff_open_read(@cbs);
      if MP4Handle = nil then
        raise EAuException.Create('Unable to open file.');
      FTrack := GetAACTrack(MP4Handle);
      if FTrack < 0 then
        raise EAuException.Create('Unable to find correct AAC sound track in the MP4 file.');
      buffer := nil;
      mp4ff_get_decoder_config(MP4Handle, FTrack, buffer, buffer_size);
      if NeAACDecInit2(HDecoder, buffer, buffer_size, samplerate, channels) < 0 then
      begin
        NeAACDecClose(hDecoder);
        mp4ff_close(MP4Handle);
        raise EAuException.Create('Error initializing decoder library.');
      end;
      FSR := samplerate;
      FChan := channels;
      timescale := mp4ff_time_scale(MP4Handle, FTrack);
      framesize := 1024;
      useAacLength := 0;
      if buffer <> nil then
      begin
        if NeAACDecAudioSpecificConfig(buffer, buffer_size, mp4ASC) >= 0 then
        begin
          if mp4ASC.frameLengthFlag = 1 then framesize := 960;
          if mp4ASC.sbr_present_flag = 1 then framesize := framesize*2;
        end;
        FreeMem(buffer);
      end;


//      mpg123_seek(FHandle, 0, 2);
      mpg123_set_filesize(FHandle, FStream.Size);
      FTotalSamples := mpg123_length(FHandle);
      FSize := FTotalSamples*FChan*2;
      if Fsize > 0 then
        FSeekable := True;

      mpg123_info(FHandle, @fi);
      FBitrate := fi.bitrate;

      if FBytesRead = 0 then
      begin
        FStream.Read(iBuf, Inbufsize);
        err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, @_Buf[0], _lBufSize, @FBytesRead);
      end;
      FValid := True;
      FBOffset := 0;
      FBytesRead := 0;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TMpgIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  const
    Inbufsize = _lBufSize div 10;
  var
    err : LongInt;
    iBuf : array[0..Inbufsize - 1] of Byte;
  begin
    if FStream.Position >= FStream.Size then
    begin
      Buffer := nil;
      Bytes := 0;
      Exit;
    end;
    Buffer := @_Buf[FBOffset];
    if FBytesRead = 0 then
    begin
      err := mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _lBufSize - FBOffset, @FBytesRead);
      FBOffset := FBOffset + FBytesRead;
      while (FBOffset < _lBufSize) and (FBytesRead <> 0) do
      begin
        err := mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _lBufSize - FBOffset, @FBytesRead);
        FBOffset := FBOffset + FBytesRead;
      end;
      if (err = MPG123_ERR) and (FStream.Position < FStream.Size - 4*Inbufsize) then
         raise EAuException.Create('MP3 data error');
      if (FStream.Position < FStream.Size) and (FBytesRead = 0) then
      begin
        FStream.Read(iBuf, Inbufsize);
        err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, nil, 0, nil);
        while (err = MPG123_NEED_MORE) and (FStream.Position < FStream.Size) do
        begin
          FStream.Read(iBuf, Inbufsize);
          err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, nil, 0, nil);
        end;
      end;
      FBytesRead := FBOffset;
      FBOffset := 0;
    end;
    if FBytesRead = 0 then
    begin
      mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _lBufSize - FBOffset, @FBytesRead);
    end;
    if Bytes >= FBytesRead - FBOffset then
    begin
      Bytes := FBytesRead - FBOffset;
      FBOffset := 0;
      FBytesRead := 0;
    end else
    begin
      FBOffset := FBOffset + Bytes;
    end;
  end;

  procedure TMpgIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      mpg123_delete(FHandle);
      mpg123_exit;
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  function TMpgIn.SeekInternal(var SampleNum: Int64) : Boolean;
  const
    iBufSize = _lBufSize div 4;
  var
    Pos : Longint;
  begin
    Result := False;
    if FSeekable then
    begin
      if SampleNum > FTotalSamples then SampleNum := FTotalSamples;
      mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _lBufSize - FBOffset, @FBytesRead);
      FBOffset := FBytesRead;
      while (FBOffset < _lBufSize) and (FBytesRead <> 0) do
      begin
        mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _lBufSize - FBOffset, @FBytesRead);
        FBOffset := FBOffset + FBytesRead;
      end;
      FBOffset := 0;
      FBytesRead := 0;
      Pos := 0;
      mpg123_feedseek(FHandle, SampleNum, 0, @Pos);
      FStream.Seek(Pos, soFromBeginning);
      SampleNum := Round(FStream.Position/FStream.Size*FTotalSamples);
     //mpg123_tell(FHandle);
     Result := True;
    end;
  end;

  procedure TMpgIn.FindTags;
  var
    fv1: Pmpg123_id3v1;
    fv2: Pmpg123_id3v2;
    handle :  pmpg123_handle;
    S : AnsiString;
    meta : Integer;
  begin
    _Id3v2Tags.Artist := '';
    _Id3v2Tags.Album := '';
    _Id3v2Tags.Title := '';
    _Id3v2Tags.Year := '';
    _Id3v2Tags.Track := '';
    _Id3v2Tags.Genre := '';
    _Id3v2Tags.Comment := '';
    _Id3v1Tags.Artist := '';
    _Id3v1Tags.Album := '';
    _Id3v1Tags.Title := '';
    _Id3v1Tags.Year := 0;
    _Id3v1Tags.Track := 0;
    _Id3v1Tags.Genre := '';
    mpg123_init();
    handle := mpg123_new(nil, nil);
    //AbsiString
    S := AnsiString(FFileName);
    mpg123_open(handle, @S[1]);
    mpg123_scan(handle);
    meta := mpg123_meta_check(handle);
    fv1 := nil;
    fv2 := nil;
   if(meta and MPG123_ID3 <> 0) and (mpg123_id3_(handle, fv1, fv2) = MPG123_OK) then
   begin
     if fv1 <>  nil then
     begin
       _Id3v1Tags.Title := fv1.title;
       _Id3v1Tags.Artist := fv1.artist;
       _Id3v1Tags.Album := fv1.album;
       if fv1.year[0] > ' ' then
        _Id3v1Tags.Year := StrToInt(String(fv1.year));
     end;
   try
     if fv2 <>  nil then
     begin
        if fv2.title <> nil then
        begin
          SetLength(S, fv2.title.Size);
          Move(fv2.title.p[0], S[1], fv2.title.Size);
          {$IF CompilerVersion < 20}
          _Id3v2Tags.Title := UTF8Decode(S);
          {$IFEND}
          {$IF CompilerVersion >= 20}
          _Id3v2Tags.Title := UTF8ToString(S);
          {$IFEND}
        end;
        if fv2.artist <> nil then
        begin
          SetLength(S, fv2.artist.Size);
          Move(fv2.artist.p[0], S[1], fv2.artist.Size);
          {$IF CompilerVersion < 20}
          _Id3v2Tags.Artist := UTF8Decode(S);
         {$IFEND}
          {$IF CompilerVersion >= 20}
          _Id3v2Tags.Artist := UTF8ToString(S);
          {$IFEND}
        end;
        if fv2.album <> nil then
        begin
          SetLength(S, fv2.album.Size);
          Move(fv2.album.p[0], S[1], fv2.album.Size);
          {$IF CompilerVersion < 20}
          _Id3v2Tags.Album := UTF8Decode(S);
         {$IFEND}
          {$IF CompilerVersion >= 20}
          _Id3v2Tags.Album := UTF8ToString(S);
          {$IFEND}
        end;
        if fv2.year <> nil then
        begin
          SetLength(S, fv2.year.Size);
          Move(fv2.year.p[0], S[1], fv2.year.Size);
          _Id3v2Tags.Year := S;
        end;
        if fv2.genre <> nil then
        begin
          SetLength(S, fv2.genre.Size);
          Move(fv2.genre.p[0], S[1], fv2.genre.Size);
          {$IF CompilerVersion < 20}
          _Id3v2Tags.Genre := UTF8Decode(S);
         {$IFEND}
         {$IF CompilerVersion >= 20}
          _Id3v2Tags.Genre := UTF8ToString(S);
         {$IFEND}
        end;
        if fv2.comment <> nil then
        begin
          SetLength(S, fv2.comment.Size);
          Move(fv2.comment.p[0], S[1], fv2.comment.Size);
          {$IF CompilerVersion < 20}
          _Id3v2Tags.Comment := UTF8Decode(S);
         {$IFEND}
         {$IF CompilerVersion >= 20}
          _Id3v2Tags.Comment := UTF8ToString(S);
         {$IFEND}
        end;
     end else
     begin
      {$WARNINGS OFF}
       _Id3v2Tags.Artist := _Id3v1Tags.Artist;
       _Id3v2Tags.Album := _Id3v1Tags.Album;
       _Id3v2Tags.Title := _Id3v1Tags.Title;
       _Id3v2Tags.Year := IntToStr(_Id3v1Tags.Year);
       _Id3v2Tags.Track := IntToStr(_Id3v1Tags.Track);
       _Id3v2Tags.Genre := _Id3v1Tags.Genre;
      {$WARNINGS ON}
     end;
     {$WARNINGS OFF}
     _CommonTags.Clear;
     _CommonTags.Artist := _Id3v2Tags.Artist;
     _CommonTags.Album := _Id3v2Tags.Album;
     _CommonTags.Title := _Id3v2Tags.Title;
     _CommonTags.Year := _Id3v2Tags.Year;
     _CommonTags.Track := _Id3v2Tags.Track;
     _CommonTags.Genre := _Id3v2Tags.Genre;
     {$WARNINGS ON}
   except
   end;
   end;
    mpg123_close(handle);
  end;

  function TMpgIn.GetId3v1Tags;
  begin
    OpenFile;
    Result := _Id3v1Tags;
  end;

  function TMpgIn.GetId3v2Tags;
  begin
    OpenFile;
    Result := _Id3v2Tags;
  end;


end.


