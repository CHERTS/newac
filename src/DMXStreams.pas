(*
  This file is a part of New Audio Components package v 2.2
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id$ *)

unit DMXStreams;

interface

uses
  Windows, Classes, SysUtils, math, ACS_Classes, ACS_Procs, ACS_Types;

type

  TAC3VOBStream = (acvStreamFirst, acvStreamSecond);

  TAuVOBAC3Demuxer = class(TAuFileStream)
  private
    F : TFileStream;
    Block : array [0..2047] of Byte;
    InBuff : array [0..2047] of Byte;
    DataSize : LongWord;
    FStream : Byte;
    function IsPackHeader : Boolean;
    function IsAudioPacketHeader(buf : PByte) : Boolean;
    function AudioPacketLenth(buf : PByte) : Integer;
    function IsAc3AudioStream(StreamID : Byte) : Boolean;
    procedure ReadBlock;
  public
    procedure Init(Stream : TAC3VOBStream = acvStreamFirst);
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

implementation

const
  SECTOR_SIZE = 2048;
  AC3_PACK_HEADER_LENGTH = 14;
  AC3_PRIVATE_DATA_LENGTH = 4;
  AC3_FIRST_STREAM =	$80;
  AC3_SECOND_STREAM	= $81;

  function TAuVOBAC3Demuxer.IsPackHeader : Boolean;
  begin
    Result := PLongWord(@InBuff[0])^ = $BA010000;
  end;

  function TAuVOBAC3Demuxer.IsAudioPacketHeader(buf : PByte) : Boolean;
  begin
    Result := PLongWord(buf)^ = $BD010000;
  end;

  function TAuVOBAC3Demuxer.AudioPacketLenth(buf : PByte) : Integer;
  begin
    Result := buf[8] + 9;
  end;
  function TAuVOBAC3Demuxer.IsAc3AudioStream(StreamID : Byte) : Boolean;
  begin
    Result := (StreamID = FStream);
  end;

   procedure TAuVOBAC3Demuxer.Init(Stream : TAC3VOBStream = acvStreamFirst);
   begin
     DataSize := 0;
     if Stream = acvStreamFirst then
       FStream := AC3_FIRST_STREAM
     else
       FStream := AC3_SECOND_STREAM;
   end;

   procedure TAuVOBAC3Demuxer.ReadBlock;
   var
     packet_point, private_data_point, ac3_data_point : Integer;
   begin
     while Position < Size do
     begin
       inherited Read(InBuff[0], 2048);
       if  not IsPackHeader then
         continue;
    	 packet_point := AC3_PACK_HEADER_LENGTH;
		   if not IsAudioPacketHeader(@InBuff[packet_point]) then
         continue;
    	 private_data_point := packet_point + AudioPacketLenth(@InBuff[packet_point]);
       if not IsAc3AudioStream(InBuff[private_data_point]) then
         continue;
    		ac3_data_point  := private_data_point + AC3_PRIVATE_DATA_LENGTH;
        FastCopyMem(@Block[0], @InBuff[ac3_data_point], SECTOR_SIZE-ac3_data_point);
        DataSize := SECTOR_SIZE-ac3_data_point;
        Break;
     end;
   end;

   function TAuVOBAC3Demuxer.Read(var Buffer; Count: Longint): Longint;
   var
     C : LongInt;
     b : array of Byte;
     i : Integer;
   begin
     Result := 0;
     C := Count;
     SetLength(b, C);
     while (Position < Size) do
     begin
       while C > 0 do
       begin
         if DataSize >= C then
         begin
           FastCopyMem(@b[Count - C], @Block[0], C);
           DataSize := DataSize - C;
           FastCopyMem(@Block[0], @Block[C], DataSize);
           Move(b[0], Buffer, Count);
           Result := Count;
           C := 0;
         end else
         begin
           FastCopyMem(@b[Count - C], @Block[0], DataSize);
           C := C - DataSize;
           ReadBlock;
         end;
       end;
       Break;
     end;
   end;


end.
