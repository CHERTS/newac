(*
  This file is a part of New Audio Components package v 1.9
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

*)

(* $Id$ *)

unit ACS_LAME;

(* Title: ACS_LAME
    Delphi interface for MP3 encoding using lame-enc.dll. *)

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, ACS_Classes, ACS_Types, lame,

  ACS_Tags, Windows;

type


  TMP3Mode = (mmStereo = 0,
              mmJointStereo,
              mmDual, // LAME doesn't supports this!
              mmMono);

  TMP3BitRate = (mbrAuto, mbr32, mbr40, mbr48, mbr56, mbr64, mbr80, mbr96,
                 mbr112, mbr128, mbr144, mbr160, mbr192, mbr224, mbr256, mbr320);

   TMP3Quality = (mp3ql0, mp3ql1, mp3ql2, mp3ql3, mp3ql4, mp3ql5,
   mp3ql6, mp3ql7, mp3ql8, mp3ql9);

  (* Class: TMP3Out
     This component is a LAME-based mp3 encoder. Descends from <TAuTaggedFileOut>. Requires lame_enc.dll. *)

  TMP3Out = class(TAuTaggedFileOut)
  private
    Buffer : PSmallInt;
    BufferSize : LongWord;
    FBitRate : TMP3BitRate;
    Config : BE_CONFIG;
    _Stream : HBE_STREAM;
    mp3buf : PByte;
    mp3buf_size : LongWord;

    FVBRQuality : TMP3Quality;
    FCopyright: BOOL;
    FOriginal: BOOL;
    FEnableVBR : BOOL;
    FCRC: BOOL;
    FMode: TMP3Mode;
    FAverageBitrate : TMp3Bitrate;
    FMaximumBitrate : TMP3Bitrate;
    FEnableBitReservoir : Boolean;
    FStrictISO : Boolean;
    function BitrateToInt(br : TMp3Bitrate) : Word;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: BitRate
    In CBR mode sets the output file's bitrate, in VBR mode sets the output file's minimum bitrate. *)
    property BitRate : TMP3BitRate read FBitRate write FBitRate stored True;

     (* Property: Id3v1Tags
    Sets Id3V1 tags. *)
    property Id3v1Tags;

    (* Property: Mode
    Sets the Mono/Stereo encoding options for the output file.
    if the input data is mono, don't set this property to mmSTEREO. *)
    property Mode : TMP3Mode read FMode write FMode default mmSTEREO;

    //Extras
    (* Property: CRC
    Use this property to enable/disable CRC-checksum in the output bitstream. *)
    property CRC	          : BOOL read FCRC write FCRC default false;
    property Copyright	    : BOOL read FCopyright write FCopyright default false;
    property Original       : BOOL read FOriginal write FOriginal default true;

    property EnableBitReservoir : Boolean read FEnableBitReservoir write FEnableBitReservoir;
    property StrinctISO : Boolean read FStrictISO write FStrictISO;

    //VBR encoding

    (* Property: VBRQuality
    Use this property to set the VBR quality in VBR mode.
    It is preferable to set VBR quality instead of setting average birate directly. *)
    property VBRQuality : TMP3Quality read FVBRQuality write FVBRQuality;
    (* Property: EnableVBR
    Use this property to switch between the VBR and CBR modes. *)
    property EnableVBR : BOOL read FEnableVBR write FEnableVBR;
    (* Property: AverageBitrate
    Use this property to set the average bitrate in VBR mode.
    By default AverageBitrate is set to mbrAuto which tells the component to use VBRQuality instead (the preferable way).
    When encoding in CBR mode make sure this property is set to mbrAuto. *)
    property AverageBitrate : TMP3BitRate read FAverageBitrate write FAverageBitrate;
    (* Property: MaximumBitrate
    Use this property to set the maximum bitrate in VBR mode. *)
    property MaximumBitrate : TMP3BitRate read FMaximumBitrate write FMaximumBitrate;

  end;

implementation

type

  TID3Tag = packed record
    ID : array [0..2] of AnsiChar;
    Title : array [0..29] of AnsiChar;
    Artist : array [0..29] of AnsiChar;
    Album : array [0..29] of AnsiChar;
    Year : array [0..3] of AnsiChar;
    Comment : array [0..29] of AnsiChar;
    Genre : Byte;
  end;

  constructor TMP3Out.Create;
  begin
    inherited Create(AOwner);
    LoadLAME;
    FBitRate := mbr128;
    FVBRQuality := mp3ql0;
    FMode := mmSTEREO;
    FCRC  := false;
    FCopyright := false;
    FOriginal := true;
    FAverageBitrate := mbrAuto;
    FMaximumBitrate := mbrAuto;
  end;

  destructor TMP3Out.Destroy;
  begin
//    UnloadLAME;
    inherited Destroy;
  end;

  procedure TMP3Out.Prepare;
  var
    samples, br, mbr, abr, ql : LongWord;
    res : Integer;
  begin
    LoadLame;
    if not LameLoaded then
      raise EAuException.Create(LAME_PATH + ' library could not be loaded.');
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or FShareMode, FAccessMask);
    end;
    FInput.Init;

    br := BitrateToInt(FBitRate);
    mbr := BitrateToInt(FMaximumBitrate);
    abr := BitrateToInt(FAverageBitrate);

    FillChar(Config, SizeOf(Config), 0);

      if FEnableVBR then
        case FVBRQuality of
          mp3ql0: ql:=0;
          mp3ql1: ql:=1;
          mp3ql2: ql:=2;
          mp3ql3: ql:=3;
          mp3ql4: ql:=4;
          mp3ql5: ql:=5;
          mp3ql6: ql:=6;
          mp3ql7: ql:=7;
          mp3ql8: ql:=8;
          mp3ql9: ql:=9;
        end;


    Config.dwConfig := 256;
    Config.dwStructVersion := 1;
    Config.dwStructSize := 331;
    Config.dwReSampleRate := 0;
    Config.dwSampleRate1 := FInput.SampleRate;
    Config.nMode := Byte(FMode);
    Config.bCRC1 := FCRC;
    Config.bPrivate1 := False;
    Config.dwBitrate := br;
    Config.bCopyright1 := False;
    Config.bOriginal1 := FOriginal;
    Config.bEnableVBR := FEnableVBR;
    Config.nVBRQuality := ql;
    Config.dwMaxBitrate := mbr;
    Config.dwVbrAbr_bps := abr;
    config.bNoRes := not FEnableBitReservoir;
    config.bStrictIso := FStrictISO;
    if abr <> 0 then
      Config.nVbrMethod := 4
    else
      Config.nVbrMethod := 2;
    Config.bWriteVBRHeader := FEnableVBR;

    res := beInitStream(@Config, Samples, mp3buf_size, _Stream);

    mp3buf := nil;
    Buffer := nil;

    if res <> 0 then
    begin
      FreeAndNil(FStream);
      Busy := False;
      FInput.Flush;
      raise EAuException.Create('LAME Error: ' + IntToStr(res));
    end;




     BufferSize := Samples*FInput.Channels;
     GetMem(Buffer, BufferSize);
     GetMem(mp3buf, mp3buf_size);
  end;

  procedure TMP3Out.Done;
  var
    res, l : LongWord;
    Tag : TID3Tag;
    S : AnsiString;
  begin
(*    id3tag_init(_plgf);

    id3tag_set_title(_plgf, PChar(Id3v1Tags.Title));
    id3tag_set_artist(_plgf, PChar(Id3v1Tags.Artist));
    id3tag_set_album(_plgf, PChar(Id3v1Tags.Album));
    id3tag_set_year(_plgf, PChar(IntToStr(Id3v1Tags.Year)));
    id3tag_set_track(_plgf, PChar(IntToStr(Id3v1Tags.Track)));
    id3tag_set_comment(_plgf, PChar(Id3v1Tags.Comment));
    id3tag_set_genre(_plgf, PChar(Id3v1Tags.Genre)); *)

    beDeinitStream(_Stream, mp3buf, res);
    FStream.Write(mp3buf^, res);
    FillChar(Tag, SizeOf(Tag), 0);
    Tag.ID := 'TAG';

    S := Id3v1Tags.Title;
    l := Length(S);
    if l > 30 then l := 30;
    Move(S[1], Tag.Title[0], l);

    S :='';
    S := Id3v1Tags.Artist;
    l := Length(S);
    if l > 30 then l := 30;
    Move(S[1], Tag.Artist[0], l);

    S := Id3v1Tags.Album;
    l := Length(S);
    if l > 30 then l := 30;
    Move(S[1], Tag.Album[0], l);

    S := Id3v1Tags.Comment;
    l := Length(S);
    if l > 30 then l := 30;
    Move(S[1], Tag.Comment[0], l);
    if Id3v1Tags.Year <> 0 then

    S := IntToStr(Id3v1Tags.Year);
    l := Length(S);
    if l = 4 then
      Move(S[1], Tag.Year[0], 4);

    FStream.Write(Tag, 128);
    if not FStreamAssigned then FStream.Free;
    beCloseStream(_Stream);
    if Config.bEnableVBR and (FileName <> '') then
      beWriteVBRHeader(PChar(FileName));
    if mp3buf <> nil then
      FreeMem(mp3buf);
    if Buffer <> nil then
        FreeMem(Buffer);
    FInput.Flush;
  end;

  function TMP3Out.DoOutput;
  var
    Len, OutSize : LongWord;
    EOF : Boolean;
  begin
    Result := True;
    if not CanOutput then Exit;
    if Abort then
    begin
      Result := False;
      Exit;
    end;
    Len  := FInput.FillBuffer(Buffer, BufferSize, EOF);
    if Len <> 0 then
    begin
      beEncodeChunk(_Stream, Len div 2, Buffer, mp3buf, OutSize);
      FStream.Write(mp3buf^, OutSize);
      Result := not EOF;
    end else
    begin
      Result := False;
      Exit;
    end;
  end;

  function TMp3Out.BitrateToInt(br : TMp3Bitrate) : Word;
  begin
    case br of
      mbrAuto : Result := 0;
      mbr32 : Result := 32;
      mbr40 : Result := 40;
      mbr48 : Result := 48;
      mbr56 : Result := 56;
      mbr64 : Result := 64;
      mbr80 : Result := 80;
      mbr96 : Result := 96;
      mbr112 : Result := 112;
      mbr128 : Result := 128;
      mbr144 : Result := 144;
      mbr160 : Result := 160;
      mbr192 : Result := 192;
      mbr224 : Result := 224;
      mbr256 : Result := 256;
      mbr320 : Result := 320;
      else Result := 128;
    end;
  end;

 {$WARNINGS ON}
end.
