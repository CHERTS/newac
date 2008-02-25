(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  Special thanks to Christophe De Vocht for updating this unit.

  CHANGES since v. 2.3
  Changed mono streams encoding

  CHANGES since V. 2.5
  Cleaned op some mess
  Support for all samplerates
  Added IDtag support
*)

(* $Id$ *)

unit ACS_LAME;

(* Title: ACS_LAME
    Delphi interface for MP3 encoding using lame.dll. *)

interface

{$WARNINGS OFF}

uses
  Classes, SysUtils, ACS_Classes, ACS_Types, lame,

  ACS_Tags,

{$IFDEF LINUX}
  libc;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

const
  OUT_BUF_SIZE = $4000;

type

  TMP3Quality = (ql0, ql1, ql2, ql3, ql4, ql5,
   ql6, ql7, ql8, ql9);

  TMP3Mode = (STEREO = 0,
              JOINT_STEREO,
              DUAL_CHANNEL, // LAME doesn't supports this!
              MONO);

  TMP3SampleRate = (srDefault, sr8kHz, sr11kHz, sr12kHz, sr16kHz, sr22kHz, sr24kHz, sr32kHz, sr44kHz, sr48kHz);

  TMP3BitRate = (br8, br16, br24, br32, br40, br48, br56, br64, br80, br96,
                 br112, br128, br144, br160, br192, br224, br256, br320);

  TMP3Out = class(TAuTaggedFileOut)
  private
    Buffer : PSmallIntArray;
    FBitRate : TMP3BitRate;
    _plgf : PLame_global_flags;
    mp3buf : PByte;
    mp3buf_size : Integer;

    FCopyright: BOOL;
    FOriginal: BOOL;
    FEnableVBR: BOOL;
    FWriteVBRHeader: BOOL;
    FCRC: BOOL;
    FVBRQuality: TMP3Quality;
    FVBRMinBitrate: integer;
    FVBRMaxBitrate: integer;
    FQuality: TMP3Quality;
    FMode: TMP3Mode;
    FSampleRate: TMP3SampleRate;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BitRate : TMP3BitRate read FBitRate write FBitRate stored True;

    property Id3v1Tags;

    property Quality     : TMP3Quality read FQuality write FQuality default ql5;

    property SampleRate     : TMP3SampleRate read FSampleRate write FSampleRate default srDefault;
    property Mode           : TMP3Mode read FMode write FMode default STEREO;

    //Extras
    property CRC	          : BOOL read FCRC write FCRC default false;
    property Copyright	    : BOOL read FCopyright write FCopyright default false;
    property Original       : BOOL read FOriginal write FOriginal default true;

    //VBR encoding
    property WriteVBRHeader : BOOL read FWriteVBRHeader write FWriteVBRHeader default false;
    property EnableVBR	    : BOOL read FEnableVBR write FEnableVBR default false;
    property VBRQuality     : TMP3Quality read FVBRQuality write FVBRQuality default ql5;
    property VBRMinBitrate  : integer read FVBRMinBitrate write FVBRMinBitrate default 128;
    property VBRMaxBitrate  : integer read FVBRMaxBitrate write FVBRMaxBitrate default 192;
  end;

implementation

  constructor TMP3Out.Create;
  begin
    inherited Create(AOwner);
    LoadLAME;
    FBitRate := br128;

    FQuality := ql5;
    FSampleRate := srDefault;
    FMode := STEREO;
    FCRC  := false;
    FCopyright := false;
    FOriginal := true;

    FWriteVBRHeader := false;
    FEnableVBR := false;
    FVBRQuality := ql5;
    FVBRMinBitrate := 128;
    FVBRMaxBitrate := 192;

    if not (csDesigning in ComponentState) then
    if not LameLoaded then
    raise EAuException.Create(LAME_PATH + ' library could not be loaded.');
  end;

  destructor TMP3Out.Destroy;
  begin
    UnloadLAME;
    inherited Destroy;
  end;

  procedure TMP3Out.Prepare;
  var
    samples, br, tbr, sr, ql : Integer;
  begin
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;
    _plgf := lame_init;
    if FInput.Size > 0 then
    samples := FInput.Size div ((Finput.BitsPerSample shr 3)*Finput.Channels);
    lame_set_num_samples(_plgf, samples);
    lame_set_in_samplerate(_plgf, Finput.SampleRate);
    lame_set_num_channels(_plgf, 2); // not Finput.Channels see the note below
    case FBitRate of
      br8 : br := 8;
      br16 : br := 16;
      br24 : br := 24;
      br32 : br := 32;
      br40 : br := 40;
      br48 : br := 48;
      br56 : br := 56;
      br64 : br := 64;
      br80 : br := 80;
      br96 : br := 96;
      br112 : br := 112;
      br128 : br := 128;
      br144 : br := 144;
      br160 : br := 160;
      br192 : br := 192;
      br224 : br := 224;
      br256 : br := 256;
      br320 : br := 320;
    end;
    case FSampleRate of
      srDefault: sr := 0;
      sr8kHz: sr := 8000;
      sr11kHz: sr := 11025;
      sr12kHz: sr := 12000;
      sr16kHz: sr := 16000;
      sr22kHz: sr := 22050;
      sr24kHz: sr := 24000;
      sr32kHz: sr := 32000;
      sr44kHz: sr := 44100;
      sr48kHz: sr := 48000;
    end;

    if Finput.SampleRate < 11025 then
      sr := 8000;

    lame_set_out_samplerate(_plgf,sr);

    if Finput.Channels = 1 then
      lame_set_mode(_plgf, Integer(MONO))  // It is no use to make stereo
                                           // out of mono, isn't it?
    else
      lame_set_mode(_plgf, Integer(FMode));

    if FCopyright then
    lame_set_copyright(_plgf,1)
    else
    lame_set_copyright(_plgf,0);
    if FOriginal then
    lame_set_original(_plgf,1)
    else
    lame_set_original(_plgf,0);
    if FCRC then
    lame_set_error_protection(_plgf,1)
    else
    lame_set_error_protection(_plgf,0);

    if not EnableVBR then
      begin
        lame_set_brate(_plgf, br);
        case FQuality of
          ql0: ql:=0;
          ql1: ql:=1;
          ql2: ql:=2;
          ql3: ql:=3;
          ql4: ql:=4;
          ql5: ql:=5;
          ql6: ql:=6;
          ql7: ql:=7;
          ql8: ql:=8;
          ql9: ql:=9;
        end;
        lame_set_quality(_plgf,ql);
      end
    else
      begin
        if FWriteVBRHeader then
        lame_set_bWriteVbrTag(_plgf,1);
        lame_set_VBR(_plgf,2);
        case FVBRQuality of
          ql0: ql:=0;
          ql1: ql:=1;
          ql2: ql:=2;
          ql3: ql:=3;
          ql4: ql:=4;
          ql5: ql:=5;
          ql6: ql:=6;
          ql7: ql:=7;
          ql8: ql:=8;
          ql9: ql:=9;
        end;
        lame_set_VBR_q(_plgf,ql);
        if FVBRMinBitrate > FVBRMaxBitrate then
          begin
            tbr := FVBRMinBitrate;
            FVBRMinBitrate := FVBRMaxBitrate;
            FVBRMinBitrate := tbr;
          end;
        lame_set_VBR_min_bitrate_kbps(_plgf,FVBRMinBitrate);
        lame_set_VBR_max_bitrate_kbps(_plgf,FVBRMaxBitrate);
      end;

    if lame_init_params(_plgf) = -1 then
      raise Exception.Create('LAME init failed');
    mp3buf_size := (OUT_BUF_SIZE shr 1) + (OUT_BUF_SIZE shr 3) + 7200;
    GetMem(mp3buf, mp3buf_size);
  end;

  procedure TMP3Out.Done;
  var
    res : Integer;
  begin
    id3tag_init(_plgf);

    id3tag_set_title(_plgf, PChar(Id3v1Tags.Title));
    id3tag_set_artist(_plgf, PChar(Id3v1Tags.Artist));
    id3tag_set_album(_plgf, PChar(Id3v1Tags.Album));
    id3tag_set_year(_plgf, PChar(IntToStr(Id3v1Tags.Year)));
    id3tag_set_track(_plgf, PChar(IntToStr(Id3v1Tags.Track)));
    id3tag_set_comment(_plgf, PChar(Id3v1Tags.Comment));
    id3tag_set_genre(_plgf, PChar(Id3v1Tags.Genre));

    res := lame_encode_flush(_plgf, mp3buf, mp3buf_size);
    FStream.Write(mp3buf^, res);
    if not FStreamAssigned then FStream.Free;
    lame_close(_plgf);
    FreeMem(mp3buf);
    FInput.Flush;
  end;

  function TMP3Out.DoOutput;
  var
    res, ns, i : Integer;
    Len : LongWord;
    inBuf : array of SmallInt;
    Ptr : Pointer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Abort then
    begin
      Result := False;
      Exit;
    end;
    Len := OUT_BUF_SIZE;
    Finput.GetData(Ptr, Len);
    if Len <> 0 then
    begin
      Buffer := Ptr;
      if FInput.Channels = 2 then
      begin
        ns := Len shr 2;
        res := lame_encode_buffer_interleaved(_plgf, @Buffer[0], ns, mp3buf, mp3buf_size);
      end else
      begin
        (* If the input stream is mono we turn it into stereo here.
           This is the way to bypass some pour performance on mono streams,
           maybe not the best way, but for a while ...*)
        SetLength(inBuf, Len);
        ns := Len shr 1;
        for i := 0 to ns-1 do
        begin
          inBuf[i shl 1] := Buffer[i+1];
          inBuf[(i shl 1)+1] := Buffer[i+1];
        end;
        res := lame_encode_buffer_interleaved(_plgf, @inBuf[0], ns, mp3buf, mp3buf_size);
        SetLength(inBuf, 0);
      end;
      if res < 0 then
      begin
        Result := False;
        Exit;
      end;
      FStream.Write(mp3buf^, res);
    end else
    begin
      Result := False;
      Exit;
    end;
  end;

 {$WARNINGS ON} 
end.
