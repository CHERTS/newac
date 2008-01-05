(*
  This file is a part of New Audio Components package v. 1.3.2
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

unit libwma1;

(* Title: libwma1.pas
    Delphi headers for WMA files. *)

interface

uses
  Windows, Classes, SysUtils, ActiveX, MMSystem, wmfintf, ACS_Classes, ACS_Types;

type

   QWORD = Int64;

   TAudioStream = class(TInterfacedObject, IStream)
   private
     FStream : TStream;
   public
     procedure AssignStream(Stream : TStream); stdcall;
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
   end;


   wma_sync_reader = record
     AudioStream : TAudioStream;
     reader : IWMSyncReader;
     buffer : INSSBuffer;
     HeaderInfo : IWMHeaderInfo;
     offset : Integer;
     AuStream : IStream;
     stream : Word;
     output : LongWord;
     has_audio : Boolean;
     _protected : Boolean;
     channels : Integer;
     SampleRate :  Integer;
     BitsPerSample : Integer;
     duration : QWORD;
   end;
   pwma_sync_reader = ^wma_sync_reader;

   wma_writer = record
     file_sink : IWMWriterFileSink;
     writer : IWMWriter;
     buffer : INSSBuffer;
     input : LongWord;
     BytesPerSecond : LongWord;
     TotalTime : QWORD;
     Initialized : Boolean;
   end;


 const
   VOICE_AUDIO = 1;
   AUDIO_64 = 2;
   AUDIO_96 = 3;
   AUDIO_128 = 4;



   procedure lwma_reader_init(var sync_reader : wma_sync_reader; Stream : TStream);
//   function lwma_reader_has_audio(reader : Pointer) : Byte;
//   function lwma_reader_is_protected(reader : Pointer) : Byte;
   function lwma_reader_get_duration(var sync_reader : wma_sync_reader) : LongWord;
   function lwma_reader_get_bitrate(var sync_reader : wma_sync_reader) : LongWord;
   procedure lwma_reader_get_author(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_title(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_album(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_genre(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_track(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_year(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_copyright(var sync_reader : wma_sync_reader; var result : WideString);
   procedure lwma_reader_get_audio_properties(var sync_reader : wma_sync_reader; var channels, BitsPerSample : Word; var SampleRate : LongWord);
   procedure lwma_reader_seek(var sync_reader : wma_sync_reader; start_from : LongWord);
   procedure lwma_reader_get_data(var sync_reader : wma_sync_reader; var buffer : Pointer; var bufsize : Integer);
   procedure lwma_reader_free(var sync_reader : wma_sync_reader);

  procedure lwma_writer_init(var writer : wma_writer; pwszFilename : PWChar);
  procedure lwma_writer_set_audio_properties(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; vbr : Boolean; DesiredBitrate : LongWord);
  procedure lwma_writer_set_author(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_title(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_album(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_genre(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_year(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_copyright(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_set_track(var writer : wma_writer; const value : WideString);
  procedure lwma_writer_begin(var writer : wma_writer);
  procedure lwma_writer_write(var writer : wma_writer; Buffer : Pointer; len : LongWord);
  procedure lwma_writer_free(var writer : wma_writer);

  procedure lwma_get_codec_names(var SL : TStringList);
  procedure create_configs(var List : TList);

implementation


   function GUIDSEqual(const g1, g2 : TGUID) : Boolean;
   begin
     Result := False;
     if g1.D1 <> g2.D1 then Exit;
     if g1.D2 <> g2.D2 then Exit;
     if g1.D3 <> g2.D3 then Exit;
     if g1.D4[0] <> g2.D4[0] then Exit;
     if g1.D4[1] <> g2.D4[1] then Exit;
     if g1.D4[2] <> g2.D4[2] then Exit;
     if g1.D4[3] <> g2.D4[3] then Exit;
     if g1.D4[4] <> g2.D4[4] then Exit;
     if g1.D4[5] <> g2.D4[5] then Exit;
     if g1.D4[6] <> g2.D4[6] then Exit;
     if g1.D4[7] <> g2.D4[7] then Exit;
     Result := True;
   end;

   procedure lwma_reader_init(var sync_reader : wma_sync_reader; Stream : TStream);
   var
     OutputCount, FormatCount : LongWord;
     i, j, size : LongWord;
     MediaProps : IWMOutputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     datatype : WMT_ATTR_DATATYPE;
     len, astream : Word;
   begin
     CoInitialize(nil);
     FillChar(sync_reader, SizeOf(sync_reader), 0);
//     sync_reader.has_audio := False;
//     sync_reader.offset := 0;
//     sync_reader.buffer := nil;
//     sync_reader.HeaderInfo := nil;
     if WMCreateSyncReader(nil, 0, sync_reader.reader) <> S_OK then
     Exit;
     sync_reader.AudioStream := TAudioStream.Create;
     sync_reader.AudioStream.AssignStream(Stream);
     sync_reader.AuStream := sync_reader.AudioStream as IStream;
     if sync_reader.reader.OpenStream(sync_reader.AuStream) <> S_OK then
     begin
//       sync_reader.reader := nil;
       sync_reader.AudioStream.Free;
       Exit;
     end;
     if sync_reader.reader.GetOutputCount(OutputCount) <> S_OK then
     begin
//       sync_reader.reader := nil;
       sync_reader.AudioStream.Free;
       Exit;
     end;
     for i := 0 to OutputCount - 1 do
     begin

       sync_reader.reader.GetOutputProps(i, MediaProps);
         MediaProps.GetMediaType(nil, size);
         GetMem(MediaType, size);
         MediaProps.GetMediaType(MediaType, size);
         if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
	 begin
	   sync_reader.has_audio := True;
	   sync_reader.output := i;
	   format := PWAVEFORMATEX(MediaType.pbFormat);
	   sync_reader.channels := format.nChannels;
	   sync_reader.SampleRate := format.nSamplesPerSec;
	   sync_reader.BitsPerSample := format.wBitsPerSample;
	 end;
	 FreeMem(MediaType);
	 MediaProps := nil;


       //sync_reader.reader.GetOutputFormatCount(i, FormatCount);
       (*for j := 0 to FormatCount - 1 do
       begin
         sync_reader.reader.GetOutputFormat(i, j, MediaProps);
         MediaProps.GetMediaType(nil, size);
         GetMem(MediaType, size);
         MediaProps.GetMediaType(MediaType, size);
         if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
	 begin
	   sync_reader.has_audio := True;
	   sync_reader.output := i;
	   format := PWAVEFORMATEX(MediaType.pbFormat);
	   sync_reader.channels := format.nChannels;
	   sync_reader.SampleRate := format.nSamplesPerSec;
	   sync_reader.BitsPerSample := format.wBitsPerSample;
	 end;
	 FreeMem(MediaType);
	 MediaProps := nil;
 	 if sync_reader.has_audio then break;
       end;
       if sync_reader.has_audio then break; *)
     end;
     if sync_reader.has_audio then
     begin
       sync_reader.reader.GetStreamNumberForOutput(sync_reader.output, sync_reader.stream);
       if sync_reader.reader.SetReadStreamSamples(sync_reader.stream, false) = NS_E_PROTECTED_CONTENT then
	 sync_reader._protected := True
       else
       sync_reader._protected := False;
       len := 8;
       astream := 0;
       sync_reader.reader.QueryInterface(IID_IWMHeaderInfo, sync_reader.HeaderInfo);
       if sync_reader.HeaderInfo.GetAttributeByName(astream, g_wszWMDuration, datatype, PByte(@(sync_reader.duration)), len) <> S_OK then
         sync_reader.duration := 0;
     end;
   end;

   function lwma_reader_has_audio(reader : Pointer) : Byte;
   var
     sync_reader : pwma_sync_reader;
   begin
     sync_reader := reader;
     Result := Byte(sync_reader.has_audio);
   end;

   function lwma_reader_is_protected(reader : Pointer) : Byte;
   var
     sync_reader : pwma_sync_reader;
   begin
     sync_reader := reader;
     Result := Byte(sync_reader._protected);
   end;

   function lwma_reader_get_duration(var sync_reader : wma_sync_reader) : LongWord;
   begin
     Result := Round(sync_reader.duration/0.1e6);
   end;

   function lwma_reader_get_bitrate(var sync_reader : wma_sync_reader) : LongWord;
   var
     len, stream : Word;
     datatype : WMT_ATTR_DATATYPE;
   begin
     len := 4;
     stream := 0;
     if sync_reader.HeaderInfo.GetAttributeByName(stream, g_wszWMBitrate, datatype, PByte(@Result), len) <> S_OK then
       Result:=0;
   end;

   procedure get_tag(var sync_reader : wma_sync_reader; name : PWideChar; var result : WideString);
   var
     stream : Word;
     datatype : WMT_ATTR_DATATYPE;
      len : Word;
   begin
     stream := 0;
     if sync_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, nil, len) <> S_OK then
     begin
       result := '';
       Exit;
     end;
     SetLength(result, len div 2);
     if sync_reader.HeaderInfo.GetAttributeByName(stream, name, datatype, PByte(@result[1]), len) <> S_OK then
     begin
       result := '';
       Exit;
     end;
   end;

   procedure lwma_reader_get_author(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMAuthor, result);
   end;

   procedure lwma_reader_get_title(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMTitle, result);
   end;

   procedure lwma_reader_get_album(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMAlbumTitle, result);
   end;

   procedure lwma_reader_get_genre(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMGenre, result);
   end;

   procedure lwma_reader_get_track(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMTrack, result);
   end;

   procedure lwma_reader_get_year(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMYear, result);
   end;

   procedure lwma_reader_get_copyright(var sync_reader : wma_sync_reader; var result : WideString);
   begin
     get_tag(sync_reader, g_wszWMCopyright, result);
   end;

   procedure lwma_reader_get_audio_properties(var sync_reader : wma_sync_reader; var channels, BitsPerSample : Word; var SampleRate : LongWord);
   begin
     channels := sync_reader.channels;
     BitsPerSample := sync_reader.BitsPerSample;
     SampleRate := sync_reader.SampleRate;
   end;

   procedure lwma_reader_seek(var sync_reader : wma_sync_reader; start_from : LongWord);
   begin
     sync_reader.reader.SetRange(Round(start_from*0.1e6), 0);
   end;

   procedure lwma_reader_get_data(var sync_reader : wma_sync_reader; var buffer : Pointer; var bufsize : Integer);
   var
     buf : PByte;
     len : LongWord;
     copylen : Integer;
     time, duration : Int64;
     flags : LongWord;
     Output : LongWord;
     w : Word;
   begin
     while (True) do
     begin
       if sync_reader.buffer <> nil then
       begin
	 sync_reader.buffer.GetBufferAndLength(buf, len);
         if sync_reader.offset < len then
         begin
           if bufsize < (len - sync_reader.offset) then
             copylen :=  bufsize
           else
             copylen := len - sync_reader.offset;
	   LongWord(buffer) := LongWord(buf) + sync_reader.offset;
	   bufsize := copylen;
	   sync_reader.offset := sync_reader.offset + copylen;
	   Exit;
	 end;
	// sync_reader.buffer._Release;
         sync_reader.buffer := nil;
	 sync_reader.offset := 0;
       end;
       if sync_reader.reader.GetNextSample(sync_reader.stream, sync_reader.buffer, time, duration, flags, Output, w) <> S_OK then
       begin
	 buffer := nil;
	 bufsize := 0;
         Exit;
       end;
     end;
   end;

   procedure lwma_reader_free(var sync_reader : wma_sync_reader);
   begin
     sync_reader.reader.Close;
      if sync_reader.buffer <> nil then
		sync_reader.buffer := nil;
     sync_reader.reader := nil;
     sync_reader.HeaderInfo := nil;
     sync_reader.AudioStream := nil;
     sync_reader.AudioStream.Free;
     sync_reader.AudioStream := nil;
   end;

   procedure lwma_get_codec_names(var SL : TStringList);
   var
     ProfileManager : IWMProfileManager;
     CodecInfo : IWMCodecInfo3;
     Codecs, CodecIndex : LongWord;
     CNameSize : LongWord;
     Name : WideString;
   begin
     WMCreateProfileManager(ProfileManager);
     CodecInfo := ProfileManager as IWMCodecInfo3;
     CodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, Codecs);
     for CodecIndex := 0 to Codecs - 1 do
     begin
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, nil, CNameSize);
       SetLength(Name, CNameSize);
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, @Name[1], CNameSize);
       SL.Add(String(Name));
     end;
     CodecInfo := nil;
     ProfileManager := nil;

   end;

   procedure create_configs(var List : TList);
   var
     ProfileManager : IWMProfileManager;
     CodecInfo : IWMCodecInfo3;
     Codecs, CodecIndex, Formats, FormatIndex : LongWord;
     CNameSize : LongWord;
     Name : WideString;
     Profile : IWMProfile;
     VBREnabled : LongBool;
     Passes : LongWord;
     StreamConfig : IWMStreamConfig;
   begin
     WMCreateProfileManager(ProfileManager);
     CodecInfo := ProfileManager as IWMCodecInfo3;
     CodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, Codecs);
     for CodecIndex := 0 to Codecs - 1 do
     begin
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, nil, CNameSize);
       SetLength(Name, CNameSize);
       CodecInfo.GetCodecName(WMMEDIATYPE_Audio, CodecIndex, @Name[1], CNameSize);
       if PWideChar(Name) = 'WM-AUDIO' then
       begin
         Passes := 1;
         VBREnabled := False;
           CodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszVBREnabled, WMT_TYPE_BOOL, PByte(@VBREnabled), 4);
           CodecInfo.SetCodecEnumerationSetting(WMMEDIATYPE_Audio, CodecIndex, g_wszNumPasses, WMT_TYPE_DWORD, PByte(@Passes), 4);
           Formats := 0;
           CodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, Formats);
           for FormatIndex := 0 to Formats - 1 do
           begin
             Name := ' ';
             CodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, StreamConfig, nil, CNameSize);
             SetLength(Name, CNameSize);
             CodecInfo.GetCodecFormatDesc(WMMEDIATYPE_Audio, CodecIndex, FormatIndex, StreamConfig, @Name[1], CNameSize);
           end;
       end;
     end;

     ProfileManager.CreateEmptyProfile($00090000, Profile);
 //    Profile.CreateNewStream()
    CodecInfo := nil;
    Profile := nil;
    ProfileManager := nil;
   StreamConfig := nil;
  end;

   procedure lwma_writer_init(var writer : wma_writer; pwszFilename : PWChar);
   var
     guid : TGuid;
     pSinkBase : IWMWriterSink;
     pWriterAdvanced : IWMWriterAdvanced;
   begin
     CoInitialize(nil);
     FillChar(writer, SizeOf(writer), 0);
     if WMCreateWriterFileSink(writer.file_sink) <> S_OK then
       Exit;
     if WMCreateWriter(nil, writer.writer) <> S_OK then
     begin
       writer.file_sink := nil;
       Exit;
     end;
     writer.file_sink.QueryInterface(IID_IWMWriterSink, pSinkBase);
     writer.writer.QueryInterface(IID_IWMWriterAdvanced, pWriterAdvanced);
     pWriterAdvanced.AddSink(pSinkBase);
     pWriterAdvanced := nil;
     pSinkBase := nil;
     if writer.file_sink.Open(pwszFilename) <> S_OK then
     begin
       writer.file_sink := nil;
       writer.writer := nil;
       exit;
     end;
     Writer.Initialized := True;
   end;


 (*  function SeletBestConfig(SubType : TGUID; pWaveLimits : pWAVEFORMATEX; dwMaxRate : LongWord; out ppStreamConfig : IWMStreamConfig) : Boolean;
   var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    pStreamConfig : IWMStreamConfig;
    pBestMatch : IWMStreamConfig;
    pProps : IWMMediaProps;
    pType : PWMMEDIATYPE;
    cbType : LongWord;
    pWave  : PWAVEFORMATEX;
    index, cEntries, dwBestRate, dwCurRate, PacketsPerSecond, CodecIndex, CurSR : LongWord;
    const INVALID_INDEX = $FFFFF;
   begin
     dwCurRate := 0;
     CurSR := 0;
     cEntries := 0;
     dwBestRate := 0;
     PacketsPerSecond := 0;
     CodecIndex := 0;
     pBestMatch := nil;

     WMCreateProfileManager(pProfileMgr);
     pCodecInfo := pProfileMgr as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     CodecIndex := INVALID_INDEX;
     for index := 0 to  cEntries -1 do
     begin
       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, index, 0, pStreamConfig);
       if pStreamConfig = nil then
        Break;
       pProps := pStreamConfig as IWMMediaProps;
       pProps.GetMediaType(nil, cbType);
       if pProps = nil then
        Break;

       GetMem(pType, cbType);
       pProps.GetMediaType(pType, cbType);
       // Check this codec against the one requested.

       if GUIDSEqual(pType.subtype, SubType) then
         CodecIndex := index;
       pStreamConfig := nil;
       pProps := nil;
       FreeMem(pType);
       if CodecIndex <> INVALID_INDEX then
         break;
     end;
     if CodecIndex = INVALID_INDEX then
     begin
       Result := False;
       Exit;
     end;
     pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, CodecIndex, cEntries);

     for index := 0 to cEntries - 1 do
     begin
       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, CodecIndex, index, pStreamConfig);
       pProps := pStreamConfig as IWMMediaProps;
       pProps.GetMediaType(nil, cbType);
       GetMem(pType, cbType);
       pProps.GetMediaType(pType, cbType);
       pWave := PWAVEFORMATEX(pType.pbFormat);
       pStreamConfig.GetBitrate(dwCurRate);
       if Abs(dwCurRate - dwMaxRate) < Abs(dwBestRate - dwMaxRate)
       then
       begin
         dwBestRate := dwCurRate;
         if (pWave.nChannels = pWaveLimits.nChannels) then
         if (pWave.wBitsPerSample = pWaveLimits.wBitsPerSample) then
         if Abs(pWave.nSamplesPerSec - pWaveLimits.nSamplesPerSec) < Abs(CurSR - pWaveLimits.nSamplesPerSec) then
         begin
           CurSR := pWave.nSamplesPerSec;
           pBestMatch := nil;
           pBestMatch := pStreamConfig;
           //pStreamConfig._AddRef;
           dwBestRate := (pWave.nAvgBytesPerSec * 8);
         end;
       end;
       pStreamConfig := nil;
       pProps := nil;
       FreeMem(pType);
    end;
    if pBestMatch = nil then
    begin
      Result := False;
      Exit;
    end;
    ppStreamConfig := pBestMatch;
    Result := True;
  end;  *)

  function SeletBestConfig(SubType : TGUID; pWaveLimits : pWAVEFORMATEX; dwMaxRate : LongWord; out ppStreamConfig : IWMStreamConfig) : Boolean;
   var
    pProfileMgr : IWMProfileManager;
    pCodecInfo : IWMCodecInfo3;
    pStreamConfig : IWMStreamConfig;
    pBestMatch : IWMStreamConfig;
    pProps : IWMMediaProps;
    pType : PWMMEDIATYPE;
    cbType : LongWord;
    pWave  : PWAVEFORMATEX;
    index, index2, cEntries, cEntries2, dwBestRate, dwCurRate, PacketsPerSecond, CodecIndex, CurSR : LongWord;
    const INVALID_INDEX = $FFFFF;
   begin
     dwCurRate := 0;
     CurSR := 0;
     cEntries := 0;
     dwBestRate := 0;
     PacketsPerSecond := 0;
     CodecIndex := 0;
     pBestMatch := nil;

     WMCreateProfileManager(pProfileMgr);
     pCodecInfo := pProfileMgr as IWMCodecInfo3;
     pCodecInfo.GetCodecInfoCount(WMMEDIATYPE_Audio, cEntries);
     CodecIndex := INVALID_INDEX;
     for index := 0 to  cEntries -1 do
     begin
       pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, index, 0, pStreamConfig);
//       if pStreamConfig = nil then
//        Break;
       pCodecInfo.GetCodecFormatCount(WMMEDIATYPE_Audio, Index, cEntries2);
       for index2 := 0 to cEntries2 - 1 do
       begin
         pCodecInfo.GetCodecFormat(WMMEDIATYPE_Audio, Index, index2, pStreamConfig);
         if pStreamConfig = nil then
          Break;
         pProps := pStreamConfig as IWMMediaProps;
         pProps.GetMediaType(nil, cbType);
         GetMem(pType, cbType);
         pProps.GetMediaType(pType, cbType);
         pWave := PWAVEFORMATEX(pType.pbFormat);
         pStreamConfig.GetBitrate(dwCurRate);
         if Abs(dwCurRate - dwMaxRate) < Abs(dwBestRate - dwMaxRate)
         then
         begin
           if  (pWave.nChannels = pWaveLimits.nChannels) then
           if (pWave.wBitsPerSample = pWaveLimits.wBitsPerSample) then
           if Abs(pWave.nSamplesPerSec - pWaveLimits.nSamplesPerSec) <= Abs(CurSR - pWaveLimits.nSamplesPerSec) then
           begin
             dwBestRate := dwCurRate;
             CurSR := pWave.nSamplesPerSec;
             pBestMatch := nil;
             pBestMatch := pStreamConfig;
             //pStreamConfig._AddRef;
//             dwBestRate := (pWave.nAvgBytesPerSec * 8);
           end;
         end;
         pStreamConfig := nil;
         pProps := nil;
         FreeMem(pType);
       end;
    end;
    if pBestMatch = nil then
    begin
      Result := False;
      Exit;
    end;
 //   end else
 //    pBestMatch.SetBitrate(dwMaxRate);
    ppStreamConfig := pBestMatch;
    Result := True;
  end;


   procedure lwma_writer_set_audio_properties(var writer : wma_writer; channels, BitsPerSample : Word; SampleRate : LongWord; vbr : Boolean; DesiredBitrate : LongWord);
   var
     Size : LongWord;
     StreamConfig : IWMStreamConfig;
     cInputs, inputIndex : LongWord;
     pProps : IWMInputMediaProps;
     MediaType : PWMMEDIATYPE;
     format : PWAVEFORMATEX;
     Config : IWMStreamConfig;
     ProfileManager : IWMProfileManager;
     Profile : IWMProfile;
   begin
     if DesiredBitRate < 1000 then DesiredBitRate := DesiredBitRate*1000;
     New(format);
     format.nChannels := channels;
     format.nSamplesPerSec := SampleRate;
     format.wBitsPerSample := BitsPerSample;
     format.wFormatTag := 1;
     if not SeletBestConfig(WMMEDIASUBTYPE_WMAudioV8, format, DesiredBitrate, Config) then
       raise exception.Create('Failed to init codec');
     WMCreateProfileManager(ProfileManager);
     ProfileManager.CreateEmptyProfile(WMT_VER_9_0, Profile);
     Config.SetStreamNumber(1);
     Config.SetStreamName('NAAudioStream');
     Config.SetConnectionName('NAAudioConnection');
     if Profile.AddStream(Config) <> S_OK then
     raise exception.Create('fff');
     writer.writer.SetProfile(Profile);
     Config := nil;
     Profile := nil;
     writer.writer.GetInputCount(cInputs);
     for inputIndex := 0 to cInputs - 1 do
     begin
       writer.writer.GetInputProps(inputIndex, pProps);
       pProps.GetMediaType(nil, size);
       GetMem(MediaType, size);
       pProps.GetMediaType(MediaType, size);
       if GUIDSEqual(MediaType.majortype, WMMEDIATYPE_Audio) and GUIDSEqual(MediaType.formattype, WMFORMAT_WaveFormatEx) then
       begin
   	 writer.input := inputIndex;
	 format := PWAVEFORMATEX(MediaType.pbFormat);
	 format.nChannels := channels;
	 format.nSamplesPerSec := SampleRate;
	 format.wBitsPerSample := BitsPerSample;
	 format.wFormatTag := 1;
	 format.nBlockAlign := (BitsPerSample*channels) div 8;
	 format.nAvgBytesPerSec := SampleRate*format.nBlockAlign;
	 writer.BytesPerSecond := format.nAvgBytesPerSec;
	 pProps.SetMediaType(MediaType);
	 writer.writer.SetInputProps(inputIndex, pProps);
 	 FreeMem(MediaType);
	 pProps := nil;
	 Exit;
       end;
       FreeMem(MediaType);
       pProps := nil;
     end;
   end;

   procedure set_tag(var writer : wma_writer; name : PWideChar; const tag : WideString);
   var
     hi : IWMHeaderInfo;
     Stream : Word;
   begin
     stream := 0;
     writer.writer.QueryInterface(IID_IWMHeaderInfo, hi);
     hi.SetAttribute(stream, name, WMT_TYPE_STRING, PByte(@tag[1]), Length(tag)*2);
     hi := nil;
   end;

   procedure lwma_writer_set_author(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMAuthor, value);
   end;

   procedure lwma_writer_set_title(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMTitle, value);
   end;

   procedure lwma_writer_set_album(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMAlbumTitle, value);
   end;

   procedure lwma_writer_set_genre(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMGenre, value);
   end;

   procedure lwma_writer_set_year(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMYear, value);
   end;

   procedure lwma_writer_set_copyright(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMCopyright, value);
   end;

   procedure lwma_writer_set_track(var writer : wma_writer; const value : WideString);
   begin
     set_tag(writer, g_wszWMTrack, value);
   end;

   procedure lwma_writer_begin(var writer : wma_writer);
   begin
     writer.writer.BeginWriting;
     writer.TotalTime := 0;
   end;

   procedure lwma_writer_write(var writer : wma_writer; Buffer : Pointer; len : LongWord);
   var
     buf : PByte;
   begin
     writer.writer.AllocateSample(len, writer.buffer);
     writer.buffer.GetBuffer(buf);
     Move(Buffer^, Buf^, len);
     writer.buffer.SetLength(len);
     writer.TotalTime := writer.TotalTime + Round(len/writer.BytesPerSecond/0.1e-6);
     writer.writer.WriteSample(writer.input, writer.TotalTime, 0, writer.buffer);
     writer.buffer := nil;
   end;

   procedure lwma_writer_free(var writer : wma_writer);
   begin
     writer.writer.EndWriting();
     writer.file_sink := nil;
     writer.writer := nil;
   end;

   procedure TAudioStream.AssignStream;
   begin
     FStream := Stream;
   end;

   function TAudioStream.Read;
   begin
     pcbRead^ := FStream.Read(pv^, cb);
     if pcbRead^ <> 0 then Result := S_OK
     else Result := -1;
   end;

   function TAudioStream.Write;
   begin
     pcbWritten^ := FStream.Write(pv^, cb);
     if pcbWritten^ <> 0 then Result := S_OK
     else Result := -1;
   end;

   function TAudioStream.Seek;
   begin
     FStream.Seek(dlibMove, dwOrigin);
//     libNewPosition := FStream.Position;
     Result := S_OK;
   end;

   function TAudioStream.SetSize;
   begin
     FStream.Seek(libNewSize, 0)
   end;

   function TAudioStream.CopyTo;
   begin
     MessageBox(0, nil, nil, 0);
   end;

   function TAudioStream.Commit;
   begin
     MessageBox(0, nil, nil, 0);
   end;

   function TAudioStream.Revert;
   begin
      MessageBox(0, nil, nil, 0);
   end;

   function TAudioStream.LockRegion;
   begin
     MessageBox(0, nil, nil, 0);
   end;

   function TAudioStream.UnlockRegion;
   begin
        MessageBox(0, nil, nil, 0);
   end;


   function TAudioStream.Stat;
   begin
     statstg.cbSize := FStream.Size;
     statstg.grfLocksSupported := 0;
   end;

   function TAudioStream.Clone;
   begin
        MessageBox(0, nil, nil, 0);
   end;



end.
