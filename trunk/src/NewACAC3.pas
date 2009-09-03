(*
  This file is a part of New Audio Components package 2.2
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit NewACAC3;

(* Title: NewACAC3
    Dolby Digital (AC-3) decoder component *)

interface

uses
  Windows, Classes, SysUtils, math, ACS_Classes, ACS_Procs, ACS_Types, liba52;

type

  TReadFunc = function : Boolean of object;

 (* Class: TAC3In
      Descends from <TAuFileIn>.
      This component decodes AC-3 encoded audio streams.
      You will need liba52.dll (included with other NewAC dlls) to use the component.
 *)

  TAC3In = class(TAuFileIn)
  private
    state : pa52_state;
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
//    FExtract : Boolean;
    ReadFunc : TReadFunc;
    StreamSize : Int64;
    function ReadFrame : Boolean;
//    function ExtractFrame : Boolean;
    function GetBitrate : LongWord;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
  public
    (* Property: BitRate
       Read this property to determine the bit rate for the AC-3 file.
    *)
    property BitRate : LongWord read GetBitrate;
 // published
    //(* Property: Extract
      // Set this property to True if you are going to extract DTS stream from a *.VOB file.
       //Otherwise this property should be set to False.
    //*)
   // property Extract : Boolean read FExtract write FExtract;
  end;


implementation

  const
    SafeOffset : Integer = 0;


  (* function TAC3In.ReadFrame;
  var
    CurPos : Int64;
    i : Integer;
    sample_rate, bit_rate : Integer;
    ChanInfo : Integer;
  begin
    Result := False;
    CurPos := FStream.Position;
    for i := 0 to 4096*256 do
    begin
      FStream.Seek(i+CurPos, soFromBeginning);
      FStream.Read(FrameBuf[0], 7);
      if FStream.Position >=  StreamSize then
      begin
        Result := False;
        Exit;
      end;
      FrameSize := a52_syncinfo(@FrameBuf[0], FFlags, sample_rate, bit_rate);
      if FrameSize <> 0 then
      begin
        FSR := sample_rate;
        FBitRate := bit_rate;
        FBPS := 16;
        ChanInfo := FFlags and A52_CHANNEL_MASK;
        case ChanInfo of
          0, 1 : FChan := 1;
          2, 10 : FChan := 2;
          3,4 : FChan := 3;
          5,6 : FChan := 4;
          7 : FChan := 5;
        end;
          if (FFlags and A52_LFE) <> 0 then
            Inc(FChan);
        FStream.Read(FrameBuf[7], FrameSize-7);
        Result := True;
        Break;
      end;
    end;
  end;              *)

  function TAC3In.ReadFrame;
  var
    CurPos : Int64;
    i : Integer;
    sample_rate, bit_rate, frame_length : Integer;
    ChanInfo : Integer;
  begin
    Result := False;
    while FStream.Position < StreamSize do
    begin
      FStream.Read(FrameBuf[0], 2);
      if PWord(@FrameBuf[0])^ = $770B then
      begin
        FStream.Read(FrameBuf[2], 5);
        FrameSize := a52_syncinfo(@FrameBuf[0], FFlags, sample_rate, bit_rate);
        if FrameSize <> 0 then
        begin
          FSR := sample_rate;
          FBitRate := bit_rate;
          FBPS := 16;
          ChanInfo := FFlags and a52_CHANNEL_MASK;
          case ChanInfo of
            0, 1 : FChan := 1;
            2, 10 : FChan := 2;
            3,4 : FChan := 3;
            5,6 : FChan := 4;
            7 : FChan := 5;
          end;
          if (FFlags and a52_LFE) <> 0 then
            Inc(FChan);
          if (FChan <> 6) or ((FSR <> 44100) and (FSR <> 48000) and (FSR <> 36000)) then
          begin
            FStream.Seek(-5, soFromCurrent);
            Continue;
          end;
          if FStream.Position >= FStream.Size - FrameSize + 7 then
          begin
            STreamSize := FStream.Position;
            Break;
          end;

          FStream.Read(FrameBuf[7], FrameSize-7);
          Result := True;
          Break;
        end else
        FStream.Seek(-5, soFromCurrent);
      end else
       // FStream.Seek(-3, soFromCurrent);
      begin
        if FrameBuf[1] = $0B then
          FStream.Seek(-1, soFromCurrent);
      end;
    end;
  end;

  procedure TAC3In.OpenFile;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      LoadA52Lib;
      FValid := False;
      if not LibA52Loaded then
      raise EAuException.Create(LibA52Path + ' library could not be loaded.');
      FValid := False;
      if not FStreamAssigned then
      try
        FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      except
        raise EAuException.Create('Failed to open stream');
      end;
      SetLength(FrameBuf, 32*1024);
      state := a52_init();
      CurrentBlock := 1;
      BlockCount := 6;
   (*   if FExtract then
      begin
        ReadFunc := ExtractFrame;
        StreamSize := FStream.Size - SafeOffset;
      end else *)
      begin
        ReadFunc := ReadFrame;
        StreamSize := FStream.Size;
      end;
      FValid := ReadFunc;
      if FValid = False then
      begin
        OpenCS.Leave;
        Exit;
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

  procedure TAC3In.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
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
        if not ReadFunc then
        if FStream.Position < StreamSize then
        begin
          raise EAuException.Create('Sync lost')
        end else
        begin
          Bytes := 0;
          Buffer := nil;
        end;
        CurrentBlock := 1;
      end;
      if CurrentBlock = 1 then
      begin
        FFlags := FFlags or A52_ADJUST_LEVEL;
        level := 1;
        bias := 0;
        res := a52_frame(state, @FrameBuf[0], FFlags, level, bias);
        if res <> 0 then
          raise EAuException.Create('Error reading AC3 frame');
      end;
      a52_block(state);
      samples := psingle(a52_samples(state));
      for i := 0 to FChan - 1 do
        for j := 0 to 255 do
        begin
          _Buf[j*FChan + i] := Floor(samples^*(High(SmallInt)));
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

  procedure TAC3In.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      a52_free(state);
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

  function TAC3In.GetBitrate;
  begin
    OpenFile;
    Result := FBitRate;
  end;

end.
