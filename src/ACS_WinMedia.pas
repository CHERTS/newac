(*
  This file is a part of New Audio Components package v. 1.3.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit ACS_WinMedia;

(* Title: ACS_WinMedia
    Delphi interface for Windows Media Audio (WMA) using Windows' built-in codec. *)

interface

uses
  Windows, Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, libwma1;

type

   (* Class: TWMIn
      Windows Media Audio file/stream decoder. This component can read not
      only WMA files but sound tracks from WMV files as well. It can also read
      mp3 files. Descends from <TAuTaggedFileIn> .*)

  TWMIn = class(TAuTaggedFileIn)
  private
    reader : wma_sync_reader;
    FDuration : LongWord;
    function GetHasAudio : Boolean;
    function GetProtected : Boolean;
    function GetBitrate : LongWord;
    function GetId3v2Tags : TId3v2Tags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : Integer); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       The False value indicates that either an audio stream is missing (in
       WMV file) or the input file is corrupt.
       
       Note: 
       Windows Media files may contain several audio streams. In the current
       version TWMIn reads data only from the first audio stream it finds.*)
    property HasAudio : Boolean read GetHasAudio;
    (* Property: IsProtected
       If the value of this property is True, th file is DRM-protected and hence not supported.
       This property has no meaning for mp3 files.*)
    property IsProtected : Boolean read GetProtected;
    (* Property: Bitrate
       Read this property to get the file's bitrate.
       Note: for video and multi-streamed files the total bitrate is returned.*)
     property Bitrate : LongWord read GetBitrate;
    (* Property: Id3v2Tags
       This property contains file's tags in Id3v2 format.*)
    property Id3v2Tags : TId3v2Tags read GetId3v2Tags;
  end;

   (* Class: TWMAOut
      Windows Media Audio file/stream encoder. Descends from
      <TAuTaggedFileOut> .*)

  TWMAOut = class(TAuTaggedFileOut)
  private
    Writer : wma_writer;
    EndOfStream : Boolean;
    Buf : Pointer;
    BufSize : Integer;
    FBitrate : LongWord;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Id3v2Tags
        Set an output file's tags in Id3v2 format.*)
    property Id3v2Tags;
    (* Property: DesiredBitrate
       Set the desied bitrate for an output file. The component will search
       for the best configuration matching your parameters, so the actual
       bitrate may be less than this value.*)
    property DesiredBitrate : LongWord read FBitrate write FBitrate;
  end;

implementation

  constructor TWMIn.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TWMIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWMIn.OpenFile;
  var
    Size, res : Integer;
    SampleSize, Samples : Integer;
//    Tag : TID3Tag;
    ch, bps : Word;
    sr : LongWord;
    str : WideString;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if (not FStreamAssigned) and (FWideFileName = '') then
      raise EAuException.Create('File name is not assigned');
      if not FStreamAssigned then FStream := TAuFileStream.Create(FWideFileName, fmOpenRead, fmShareDenyNone);
      lwma_reader_init(reader, FStream);
      FValid := reader.has_audio;
      if reader._protected then
        raise EAuException.Create('File is protected');
      FValid := True;
      FDuration := lwma_reader_get_duration(reader);
      lwma_reader_get_audio_properties(reader, ch, bps, sr);
      FChan := ch;
      FBPS := bps;
      FSR := sr;
      FTotalSamples := Trunc((FDuration/100)*sr);
      FSize := FTotalSamples*ch*(bps shr 3);
      FSeekable := True;
      SetLength(Str, 256);
      lwma_reader_get_author(reader, Str);
      _Id3v2Tags.Artist := Str;
      lwma_reader_get_title(reader, Str);
      _Id3v2Tags.Title := Str;
      lwma_reader_get_album(reader, Str);
      _Id3v2Tags.Album := Str;
      lwma_reader_get_genre(reader, Str);
      _Id3v2Tags.Genre := Str;
      lwma_reader_get_track(reader, Str);
      _Id3v2Tags.Track := Str;
      lwma_reader_get_year(reader, Str);
      _Id3v2Tags.Year := Str;
      lwma_reader_get_copyright(reader, Str);
      _Id3v2Tags.Comment := Str;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      lwma_reader_free(reader);
      FOpened := 0;
      if not FStreamAssigned then FStream.Free
      else FStream.Seek(0, soFromBeginning);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMIn.GetDataInternal(var Buffer : Pointer; var Bytes : Integer);
  begin
    if FSize - FPosition < Bytes then
      Bytes := FSize - FPosition;
    lwma_reader_get_data(reader, Buffer, Bytes);
  end;

  function TWMIn.SeekInternal(var SampleNum : Int64) : Boolean;
  var
    Offset : LongWord;
  begin
    Result := False;
    if Busy then
    begin
      Offset := Round(SampleNum/FTotalSamples*FDuration);
      lwma_reader_seek(reader, Offset);
      Result := True;
    end;
  end;

  function TWMIn.GetHasAudio;
  begin
    OpenFile;
    Result := reader.has_audio;
  end;

  function TWMIn.GetProtected;
  begin
    OpenFile;
    Result := reader._protected;
  end;

  function TWMIn.GetBitrate;
  begin
    OpenFile;
    Result := lwma_reader_get_bitrate(reader);
  end;

  function TWMIn.GetId3v2Tags;
  begin
    OpenFile;
    Result := _Id3v2Tags;
  end;

  constructor TWMAOut.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TWMAOut.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWMAOut.Prepare;
  begin
    if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
    FInput.Init;
    lwma_writer_init(Writer, PWideChar(FWideFileName));
    if not Writer.Initialized then
      raise Exception.Create('Cannot create file');
    lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, true, FBitrate);
    BufSize := FInput.Channels*FInput.BitsPerSample*FInput.SampleRate div 100;
    GetMem(Buf, BufSize);
    if Id3v2Tags.Artist <> '' then
      lwma_writer_set_author(Writer, Id3v2Tags.Artist);
    if Id3v2Tags.Album <> '' then
      lwma_writer_set_album(Writer, Id3v2Tags.Album);
    if Id3v2Tags.Genre <> '' then
      lwma_writer_set_genre(Writer, Id3v2Tags.Genre);
    if Id3v2Tags.Year <> '' then
      lwma_writer_set_year(Writer, Id3v2Tags.Year);
    if Id3v2Tags.Track <> '' then
      lwma_writer_set_track(Writer, Id3v2Tags.Track);
    if Id3v2Tags.Title <> '' then
      lwma_writer_set_title(Writer, Id3v2Tags.Title);
    if Id3v2Tags.Comment <> '' then
      lwma_writer_set_copyright(Writer, Id3v2Tags.Comment);
    lwma_writer_begin(Writer);
    EndOfStream := false;
  end;

  function TWMAOut.DoOutput;
  var
    l : Integer;
  begin
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfStream then
    begin
      Result := False;
      Exit;
    end;
    l := Finput.FillBuffer(Buf, BufSize, EndOfStream);
    lwma_writer_write(Writer, Buf, l);
  end;

  procedure TWMAOut.Done;
  begin
    FInput.Flush;
    lwma_writer_free(Writer);
  end;

end.
