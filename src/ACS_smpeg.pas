(*
  This file is a part of New Audio Components package v 2.4
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_smpeg;

(* Title: ACS_SMPEG
    Components for the mp3 playback. *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, ACS_WinMedia, mpg123_;


type

// Yeah, folks that's all there is to it.

(* Class: TMP3In
   The mp3 file/stream decoder that uses the Windows built-in decoder,
   descends from <TWMIn> *)

  TMP3In = class (TWMIn)
  protected
    procedure OpenFile; override;
  end;

(* Class: TMpgIn
   Yet another mp3 file/stream decoder that uses libmpg123.dll.
   Descends from <TAuTaggedFileIn>.
   This decoder provides better sound than the Windows one but doesn't suppoer the precise positioning.
 *)

  TMpgIn = class(TAuTaggedFileIn)
  private
    const
      _BufSize = 32768;
    var
      FHandle : Pmpg123_handle;
      FBytesRead, FBOffset : LongWord;
      _Buf : array[0.._BufSize - 1] of Byte;
      FBitrate : LongWord;
      procedure FindTags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
    function GetId3v1Tags : TId3v1Tags;
  public
    property Id3v1Tags : TId3v1Tags read GetId3v1Tags;
    property Bitrate : LongWord read FBitrate;
  end;


implementation

  procedure TMP3In.OpenFile;
  begin
    if FOpened = 0 then
    begin
      inherited;
      FSize := FSize + (FSR div 3)*(FChan*(FBPS div 8));
      FTotalSamples := FTotalSamples + (FSR div 3);
    end else
      Inherited;
  end;

  procedure TMpgIn.OpenFile;
  const
    Inbufsize = _BufSize div 10;
  var
    err : LongInt;
    iBuf : array[0..Inbufsize - 1] of Byte;
    fi : Tmpg123_frameinfo;
  begin
    Loadmpg123;
    if not Libmpg123Loaded then
    raise EAuException.Create(Libmpg123Path + ' library could not be loaded.');
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      FValid := False;
      if FFileName <> '' then
        FindTags;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      mpg123_init;
      FHandle :=  mpg123_new(nil, @err);
      if FHandle = nil then
        raise Exception.Create('');
      mpg123_param(FHandle, MPG123_VERBOSE, 2, 0);
      mpg123_open_feed(FHandle);
      if FHandle = nil then
        raise Exception.Create('');
      FBPS := 16;
      err := 0;
      FStream.Read(iBuf, Inbufsize);
      err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, nil, 0, @FBytesRead);
      while err = MPG123_NEED_MORE do
      begin
        FStream.Read(iBuf, Inbufsize);
        err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, nil, 0, @FBytesRead);
      end;
      if err = MPG123_NEW_FORMAT then
      begin
         mpg123_getformat(FHandle, @FSR, @FChan, @err);
      end else
      begin
        raise EAuException.Create('Failed to open stream');
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
        err := mpg123_decode(FHandle, @iBuf[0], Inbufsize, @_Buf[0], _BufSize, @FBytesRead);
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
    Inbufsize = _BufSize div 10;
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
      err := mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _BufSize - FBOffset, @FBytesRead);
      FBOffset := FBOffset + FBytesRead;
      while (FBOffset < _BufSize) and (FBytesRead <> 0) do
      begin
        err := mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _BufSize - FBOffset, @FBytesRead);
        FBOffset := FBOffset + FBytesRead;
      end;
      if err = MPG123_ERR then
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
      err := mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _BufSize - FBOffset, @FBytesRead);
    end;
//    if err = MPG123_ERR then
//       raise EAuException.Create('MP3 data error');
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
    iBufSize = _BufSize div 4;
  var
    Pos, err : Longint;
    iBuf : array[0..iBufSize - 1] of Byte;
  begin
    Result := False;
    if FSeekable then
    begin
      if SampleNum > FTotalSamples then SampleNum := FTotalSamples;
      mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _BufSize - FBOffset, @FBytesRead);
      FBOffset := FBytesRead;
      while (FBOffset < _BufSize) and (FBytesRead <> 0) do
      begin
        mpg123_decode(FHandle, nil, 0, @_Buf[FBOffset], _BufSize - FBOffset, @FBytesRead);
        FBOffset := FBOffset + FBytesRead;
      end;
      FBOffset := 0;
      FBytesRead := 0;
      Pos := 0;
      mpg123_feedseek(FHandle, SampleNum, 0, @Pos);
      FStream.Seek(Pos, soFromBeginning);
      SampleNum := Round(FStream.Position/FStream.Size*FTotalSamples);
      FStream.Read(iBuf, iBufSize);
      err := mpg123_decode(FHandle, @iBuf, iBufSize, nil, 0, nil);
      while (err = MPG123_NEED_MORE) and (FStream.Position < FStream.size) do
      begin
        FStream.Read(iBuf, iBufSize);
        err := mpg123_decode(FHandle, @iBuf, iBufSize, nil, 0, nil);
      end;
      if err = -1 then
        Exit;
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
    mpg123_init();
    handle := mpg123_new(nil, nil);
    //AbsiString
    S := FFileName;
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
       if fv1.year <> '' then
        _Id3v1Tags.Year := StrToInt(fv1.year);
     end;
   end;
    mpg123_close(handle);
  end;

  function TMpgIn.GetId3v1Tags;
  begin
    OpenFile;
    Result := _Id3v1Tags;
  end;

end.


