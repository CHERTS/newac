(*
  This file is a part of New Audio Components package 2.1
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewACDTS;

(* Title: NewACDTS
    Digital Theater System (DTS) decoder component *)

interface

uses
  Windows, Classes, SysUtils, math, ACS_Classes, ACS_Procs, ACS_Types, libdca;

type

 (* Class: TDTSIn
      Descends from <TAuFileIn>.
      This component decods DTS-encoded audio streams.
      You will need libdca.dll (included with other NewAC dlls) to use the component.
 *)

  TDTSIn = class(TAuFileIn)
  private
    state : pdca_state;
    _Buf : array of SmallInt;
    FrameBuf : array of Byte;
    FrameSize : Integer;
    _BufSize : Integer;
    _SampleSize : Word;
    _StartSample, _StartFrom : LongWord;
    Offset, BufEnd : Integer;
    BlockCount, CurrentBlock : Integer;
    FBitRate : LongWord;
    FFlags : Integer;
    __EOF : Boolean;
    function ReadFrame : Boolean;
    function GetBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
  public
    (* Property: BitRate
       Read this property to determine the bit rate for the DTS file.
    *)
    property BitRate : LongWord read GetBitrate;
  end;


implementation

  function TDTSIn.ReadFrame;
  var
    CurPos : Int64;
    tmpbuf : array[0..13] of Byte;
    i : Integer;
    sample_rate, bit_rate, frame_length : Integer;
    ChanInfo : Integer;
  begin
    Result := False;
    CurPos := FStream.Position;
    for i := 0 to 255 do
    begin
      FStream.Seek(i+CurPos, soFromBeginning);
      FStream.Read(tmpbuf, 14);
      FrameSize := dca_syncinfo(state, @tmpbuf[0], FFlags, sample_rate, bit_rate, frame_length);
      if FrameSize <> 0 then
      begin
        Result := True;
        FSR := sample_rate;
        FBitRate := bit_rate;
        FBPS := 16;
        SetLength(FrameBuf, FrameSize);
        Move(tmpbuf, FrameBuf[0], 14);
        FStream.Read(FrameBuf[14], FrameSize-14);
        ChanInfo := FFlags and DCA_CHANNEL_MASK;
        case ChanInfo of
          0 : FChan := 1;
          1,2,3,4 : FChan := 2;
          5,6 : FChan := 3;
          7,8 : FChan := 4;
          9 : FChan := 5;
          10 : FChan := 6;
        end;
          if (FFlags and DCA_LFE) <> 0 then
            Inc(FChan);
        Break;
      end;
    end;
  end;

  procedure TDTSIn.OpenFile;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      LoadDCALib;
      FValid := False;
      if not LibDCALoaded then
      raise EAuException.Create(LibDCAPath + ' library could not be loaded.');
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      state := dca_init(0);
      CurrentBlock := 1;
      BlockCount := 1;
      FValid := ReadFrame;
      if FValid = False then
      begin
        OpenCS.Leave;
        raise Exception.Create('');
      end;
      FSize := -1;
      _SampleSize := (FBPS div 8) * FChan;
      FSeekable := False;
      Inc(FOpened);
      SetLength(_Buf, 256*FChan);
      _BufSize := 256*FChan*2;
      Offset := 0;
      BufEnd := 0;
      __EOF := False;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TDTSIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
  var
    level, bias : Single;
    samples : psingle;
    i, j, Res : Integer;
    SamplesReq : Integer;
  begin
    if Offset >= BufEnd then
    begin
      if CurrentBlock > BlockCount then
      begin
        if not ReadFrame then
          if FStream.Position < FStream.Size then
          raise EAuException.Create('Sync lost')
        else begin
          Bytes := 0;
          Buffer := nil;
        end;
        CurrentBlock := 1;
      end;
      if CurrentBlock = 1 then
      begin
        FFlags := FFlags or DCA_ADJUST_LEVEL;
        level := 1;
        bias := 0;
        res := dca_frame(state, @FrameBuf[0], FFlags, level, bias);
        if res <> 0 then
          raise EAuException.Create('');
        BlockCount := dca_blocks_num(state);
      end;
      dca_block(state);
      samples := psingle(dca_samples(state));
      for i := 0 to FChan - 1 do
        for j := 0 to 255 do
        begin
          _Buf[j*FChan + i] := Floor(samples^*High(SmallInt));
          Inc(samples);
        end;
      Offset := 0;
      BufEnd := 256;
      Inc(CurrentBlock);
    end;
    Bytes := Bytes - (Bytes mod _SampleSize);
    SamplesReq := Bytes div _SampleSize;
    if SamplesReq > BufEnd - Offset then
       SamplesReq := BufEnd - Offset;
    Buffer := @_Buf[Offset*FChan];
    Bytes := SamplesReq*_SampleSize;
    Inc(Offset, SamplesReq);
  end;

  procedure TDTSIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      dca_free(state);
      _buf := nil;
      FrameBuf := nil;
      if not FStreamAssigned then
         FStream.Free;
      FOpened  := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  function TDTSIn.GetBitrate;
  begin
    OpenFile;
    Result := FBitRate;
  end;

end.
