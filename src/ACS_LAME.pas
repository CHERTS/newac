(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

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


  TMP3Mode = (mmStereo = 0,
              mmJointStereo,
              mmDual, // LAME doesn't supports this!
              mmMono);

  TMP3BitRate = (mbr32, mbr40, mbr48, mbr56, mbr64, mbr80, mbr96,
                 mbr112, mbr128, mbr144, mbr160, mbr192, mbr224, mbr256, mbr320);

  TMP3Out = class(TAuTaggedFileOut)
  private
    Buffer : PSmallInt;
    BufferSize : LongWord;
    FBitRate : TMP3BitRate;
    Config : BE_CONFIG;
    _Stream : HBE_STREAM;
    mp3buf : PByte;
    mp3buf_size : LongWord;

    FCopyright: BOOL;
    FOriginal: BOOL;
    FWriteVBRHeader: BOOL;
    FCRC: BOOL;
    FMode: TMP3Mode;
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

    property Mode : TMP3Mode read FMode write FMode default mmSTEREO;

    //Extras
    property CRC	          : BOOL read FCRC write FCRC default false;
    property Copyright	    : BOOL read FCopyright write FCopyright default false;
    property Original       : BOOL read FOriginal write FOriginal default true;

    //VBR encoding
    property WriteVBRHeader : BOOL read FWriteVBRHeader write FWriteVBRHeader default false;
  end;

implementation

  constructor TMP3Out.Create;
  begin
    inherited Create(AOwner);
    LoadLAME;
    FBitRate := mbr128;

    FMode := mmSTEREO;
    FCRC  := false;
    FCopyright := false;
    FOriginal := true;

    FWriteVBRHeader := false;

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
    samples, br : LongWord;
    res : Integer;  
  begin
    if not FStreamAssigned then
    begin
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
      FStream := TAuFileStream.Create(FWideFileName, fmCreate or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;

    case FBitRate of
      mbr32 : br := 32;
      mbr40 : br := 40;
      mbr48 : br := 48;
      mbr56 : br := 56;
      mbr64 : br := 64;
      mbr80 : br := 80;
      mbr96 : br := 96;
      mbr112 : br := 112;
      mbr128 : br := 128;
      mbr144 : br := 144;
      mbr160 : br := 160;
      mbr192 : br := 192;
      mbr224 : br := 224;
      mbr256 : br := 256;
      mbr320 : br := 320;
    end;

    Config.dwConfig := 0;
    Config.StructType := 0;
    Config.dwSampleRate := FInput.SampleRate;
    Config.byMode := Byte(FMode);
    Config.bCRC := True;
    Config.bPrivate := False;
    Config.wBitrate := br;
    Config.bCopyright := False;
    Config.bOriginal := False;

    res := beInitStream(@Config, Samples, mp3buf_size, _Stream);

    if res <> 0 then
      raise EAuException.Create('InitStream Error : ' + IntToStr(res));



{    if not EnableVBR then
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
        end;                       }

     BufferSize := Samples*FInput.Channels;
     GetMem(Buffer, BufferSize);
     GetMem(mp3buf, mp3buf_size);
  end;

  procedure TMP3Out.Done;
  var
    res : LongWord;
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
    if not FStreamAssigned then FStream.Free;
    beCloseStream(_Stream);
    FreeMem(mp3buf);
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
      beEncodeChunk(_Stream, Len div (FInput.Channels*2), Buffer, mp3buf, OutSize);
      FStream.Write(mp3buf^, OutSize);
      Result := not EOF;
    end else
    begin
      Result := False;
      Exit;
    end;
  end;

 {$WARNINGS ON} 
end.
