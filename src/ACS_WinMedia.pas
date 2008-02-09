(*
  This file is a part of New Audio Components package v. 1.5
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

  TStreamedAudioEvent = procedure(Sender : TComponent) of object;

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
    function GetIsVBR : Boolean;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
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
    (* Property: IsVBR
       This property's value is True if the input file is VBR-encoded and False otherwise.*)
    property IsVBR : Boolean read GetIsVBR;
  published
    property EndSample;
    property Loop;
    property StartSample;
  end;

   (* Class: TWMAOut
      Windows Media Audio file/stream encoder.
      This component supports CBR/VBR, lossy/lossless encoding.
      There are two mutually exclusive groups of properties affecting the output audio quality.
      One group allows you to set output file bitrate or quaity and let the component select the most appropriate codec.
      The other group allows you to specify the codec and format directly.
      This component descends from  <TAuTaggedFileOut> .*)

  TWMAOut = class(TAuTaggedFileOut)
  private
    FCodecs : TStringList;
    FFormats : TStringList;
    FCodecIndex, FFormatIndex : Integer;
    Writer : wma_writer;
    EndOfStream : Boolean;
    Buf : Pointer;
    BufSize : Integer;
    FBitrate : LongWord;
    FLossless, FVBR : Boolean;
    FVBRQuality : Byte;
    function GetCodecs : TStringList;
    function GetCodecsCount : Word;
    function GetCodecName(Index : Word) : String;
    function GetFormats(Index : Word) : TStringList;
    function GetFormatsCount(Index : Word) : Word;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: GetFormatDesc
        This method returns a format description based on the CodecIndex and FormatIndex parameters. *)
    function GetFormatDesc(CodecIndex, FormatIndex : Word) : String;
    (* Property: Codecs
        Returns the names of all the WMA codecs installed in the system. *)
    property Codecs : TStringList read GetCodecs;
    (* Property: CodecsCount
        Returns the total number of the WMA codecs available in the system. *)
    property CodecsCount : Word read GetCodecsCount;
    (* Property: CodecIndex
        Use this property to set the index number of the codec to use when encoding.
        The valid values for this property range from -1 to <CodecsCount> -1.
        If this property's value is set to -1 (the default setting), the <FormatIndex> property is ignored and
        the codec is selected automatically by the component depending on <DesiredBitrate> and <VBRQuality> values.
        If this property's value is greater than -1, the <DesiredBitrate> and <VBRQuality> properties are ignored and
        CodecIndex along with <FormatIndex> specify how audio data will be encoded. Wav2WMA-2 demo uses this method. *)
    property CodecIndex : Integer read FCodecIndex write FCodecIndex;
    (* Property: CodecName
        Returns the name of the WMA codec specified by its index.
        The valid indeces range from 0 to <CodecsCount> -1. *)
    property CodecName[Index : Word] : String read GetCodecName;
    (* Property: FormatIndex
        Use this property to set the index of the format to encode data.
         The valid values range from 0 to <FormatsCount> -1.
         This property has any effect only if <CodecIndex> is greater than -1.*)
    property FormatIndex : Integer read FFormatIndex write FFormatIndex;
    (* Property: Formats
        Returns the names of all formats supported by the codec, specified by index, and the current encoding mode.
        Note that <VBR> and <Lossless> settings affect the number of available formats.
        For example, Windows Media Audio Lossless codec will return any format only if <VBR> or <Losless> property is set to True.
        On the contraty, Windows Media Voice codec exposes only CBR formats. It will expose no formats if <VBR> or <Lossless> are set to True.
        You shoud re-read the format related properties each time. you change either the <VBR> or the <Lossless> value.
        See Wav2WMA2 demo for more details. *)
    property Formats[Index : Word] : TStringList read GetFormats;
    (* Property: FormatsCount
        Returns the total number of formats supported by the codec, specified by its index, and the current encoding mode.
        The valid indeces range from 0 to <CodecsCount> -1.
        Note that <VBR> and <Lossless> settings affect the number of available formats.
        On the contraty, Windows Media Voice codec exposes only CBR formats. It will expose no formats if <VBR> or <Lossless> are set to True.
        For example, Windows Media Audio Lossless codec will return any format only if <VBR> or <Losless> property is set to True.
        You shoud re-read the format related properties each time. you change either the <VBR> or the <Lossless> value. *)
    property FormatsCount[index : Word] : Word read GetFormatsCount;
  published
    (* Property: Id3v2Tags
        Set an output file's tags in Id3v2 format. *)
    property Id3v2Tags;
    (* Property: DesiredBitrate
       Set the desired bitrate for an output file (in a constant bitrate lossy mode). The component will search
       for the best configuration matching your parameters, so the actual
       bitrate may be less than this value.*)
    property DesiredBitrate : LongWord read FBitrate write FBitrate;
    (* Property: Lossless
       Use this property to switch between the lossless and lossy compression modes.
       In the lossless mode the <DesiredBitrate> and <VBRQuality> values are ignored.
       Lossless encoding is always VBR. *)
    property Lossless : Boolean read FLossless write FLossless;
    (* Property: VBR
       Use this property to switch between constant bitrate and variable bitrate lossy encoding modes.
       In VBR mode <DesiredBitrate> value is ignored. The quality of the output sound is defined by
       the <VBRQuality> property. If you encode data by directly selecting the codec and format, note that the VBR setting affects <Formats> and <FormatCount> values for every codec.
       In the lossless mode the this property's value is ignored. *)
    property VBR : Boolean read FVBR write FVBR;
    (* Property: VBRQuality
       Use this property to set the output audio quality in VBR mode.
       The valid values range from 1 to 99. This property has any efect only if <VBR> is set to True and <Lossless> to False. *)
    property VBRQuality : Byte read FVBRQuality write FVBRQuality;
  end;

   (* Class: TWMStreamedIn
      Streamable WMA/MP3 decoder component. This component can do three things:
       - decode wma and mp3 files stored on your hard drive.
       - decode wma and mp3 files stored at some network (http) location. The component
         starts downloading the file and decoding it simultaneously.
       - decode wma and mp3 audio streamed from "live" audio servers such as Internet radio servers.

       You shuld assign file or stream URL to the components <FileName> property.

       Important note: most links to Internet streamed media that you can find on Web sites
       point not to media itself but to wax, asx, or m3u shortcuts instead.
       These shortcuts contain information about the content, and, among other things, a direct link to
       a stremed audio server. Although it is relatively easy to parse wax, asx, and m3u shortcuts
       and extract required links from them, it is not TWMStreamedIn's job.
       The component expects a direct link to a wma/mp3 file or a network stream to be assigned to its <Filename> property.
       See also the <LoggingURL> property.

       If you write a live audio player, you have to take care about http links traversal and shortcuts parsing.
       I-Radio demo that accompanies NewAC, uses preset links to several live audio servers.

       This decoder is not seekable.

       Descends from <TAuTaggedFileIn> .*)

  TWMStreamedIn = class(TAuTaggedFileIn)
  private
    reader : wma_async_reader;
    FBitrate : LongWord;
    FStretchFactor : Single;
    FBufferingTime : Word;
    FEnableHTTP : Boolean;
    FEnableTCP : Boolean;
    FEnableUDP : Boolean;
    FMaxWait : LongWord;
    FProxyProtocol, FProxyHost : String;
    FProxyPort : LongWord;
    FLoggingURL : String;
    FOnStreamOpened : TStreamedAudioEvent;
    FOnStartedPlaying : TStreamedAudioEvent;
    FFirstTime : Boolean;
    function GetHasAudio : Boolean;
    function GetId3v2Tags : TId3v2Tags;
    function GetTimedOut : Boolean;
    procedure SetBufferingTime(value : Word);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
    function GetTotalTime : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetStretch(NewStretch : Single);
    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       The False value indicates that either an audio stream is missing (in
       WMV file) or the input file is corrupt.

       Note:
       Windows Media files may contain several audio streams. In the current
       version TWMIn reads data only from the first audio stream it finds.*)
    property HasAudio : Boolean read GetHasAudio;
    procedure _Pause; override;
    procedure _Resume; override;
    (* Property: Bitrate
       Read this property to get the file's bitrate.
       Note: for video and multi-streamed files the total bitrate is returned.*)
     property Bitrate : LongWord read FBitrate;
    (* Property: Id3v2Tags
       This property contains file's tags in Id3v2 format.*)
    property Id3v2Tags : TId3v2Tags read GetId3v2Tags;
    (* Property: TimedOut
       This property indicates if some network operation has timed out.
       If the timeout has occured, the component reports the end of data.
       You can change the time the component waits before timeout in the <MaxWaitMilliseconds> property. *)
    property TimedOut : Boolean read GetTimedOut;
  published
  (* Property: BufferingTime
       This property allows you to set the size of internal buffer in terms of playback duation.
       The value of this property should be buffer's playback time in seconds raging from 1 to 60.
       Larger bufering time imposes some delay at the beginning of the playback, but guarantees a smoother playback later. *)
    property BufferingTime : Word read FBufferingTime write SetBufferingTime;
  (* Property: EnableHTTP
       Use this property to enable or disable HTTP support. This property's value matters only if an URL supplied to <FileName> has no http:, mms: or rtsp: prefix. *)
    property EnableHTTP : Boolean read FEnableHTTP write FEnableHTTP;
  (* Property: EnableTCP
       Use this property to enable or disable TCP support. This property's value matters only if an URL supplied to <FileName> has no http:, mms: or rtsp: prefix. *)
    property EnableTCP : Boolean read FEnableTCP write FEnableTCP;
  (* Property: EnableUDP
       Use this property to enable or disable UDP support. This property's value matters only if an URL supplied to <FileName> has no http:, mms: or rtsp: prefix. *)
    property EnableUDP : Boolean read FEnableUDP write FEnableUDP;
  (* Property: LoggingURL
       Use this property to set a logging URL, if you have one. Logging URLs may be obtained from wax, asx, or m3u shortcuts. *)
    property LoggingURL : String read FLoggingURL write FLoggingURL;
  (* Property: MaxWaitMilliseconds
       This property allows you to set the maximum waiting time for some network operations to complete.
       The time is set in milliseconds. If this time is exceeded, the <TimedOut> property is set to True and the coomponent
       stops its operation. The default value of this property is 10000 (10 seconds). Setting this property's value too low
       will result in too many premature timeouts. Setting it too high will mean that you will have to wait too long just to learn that the remote server
       could not be reached. *)
    property MaxWaitMilliseconds : LongWord read FMaxWait write FMaxWait;
  (* Property: ProxyProtocol
       If your application requires a proxy to connect to Internet, use this property to set the proxy protocol. *)
    property ProxyProtocol : String read FProxyProtocol write FProxyProtocol;
  (* Property: ProxyHost
       If your application requires a proxy to connect to Internet, use this property to set the proxy host name. *)
    property ProxyHost : String read FProxyHost write FProxyHost;
  (* Property: ProxyPort
       If your application requires a proxy to connect to Internet, use this property to set the proxy port value. *)
    property ProxyPort : LongWord read FProxyPort write FProxyPort;
  (* Property: StretchFactor
       Use this property to change the speed at with content is delivered to the component.
       The default value is 1.0. Possible values range from 1.0 to 10.0 and from -10.0 to -1.0.
       This property has no effect when handling live audio. *)
    property StretchFactor : Single read FStretchFactor write FStretchFactor;
  (* Property: OnStreamOpened
       This event informs you that the audio stream has been opened successfully. *)
    property OnStreamOpened : TStreamedAudioEvent read FOnStreamOpened write FOnStreamOpened;
  (* Property: OnStartedPlaying
       This event informs you that the decoder has decoded the first chunk of audio data. *)
    property OnStartedPlaying : TStreamedAudioEvent read FOnStartedPlaying write FOnStartedPlaying;
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
      if reader.reader = nil then Exit;
      if reader._protected then
        raise EAuException.Create('File is protected');
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

  procedure TWMIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  begin
    if (FSize > 0) and (FSize - FPosition < Bytes) then
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

  function TWMIn.GetIsVBR;
  begin
    OpenFile;
    Result := lwma_reader_get_is_vbr(reader);
  end;

  constructor TWMAOut.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
      FCodecs := TStringList.Create;
      lwma_enumerate_codecs(FCodecs);
      FFormats := TStringList.Create;
      FCodecIndex := -1;
    end;
  end;

  destructor TWMAOut.Destroy;
  begin
    if not (csDesigning in ComponentState) then
    begin
      FCodecs.Free;
      FFormats.Free;
    end;
    inherited Destroy;
  end;

  procedure TWMAOut.Prepare;
  begin
    if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
    FInput.Init;
    lwma_writer_init(Writer, PWideChar(FWideFileName));
    if not Writer.Initialized then
      raise Exception.Create('Cannot create file');
    if FCodecIndex < 0 then
    begin
      if not FVBR then
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FBitrate)
      else
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FVBRQuality);
    end  
    else
      lwma_writer_set_audio_properties2(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FVBR or FLossless, FCodecIndex, FFormatIndex);
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


  constructor TWMStreamedIn.Create;
  begin
    inherited Create(AOwner);
    FStretchFactor := 1.0;
    FMaxWait := 10000;
    BufferingTime := 2;
  end;

  destructor TWMStreamedIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWMStreamedIn.OpenFile;
  var
//    Tag : TID3Tag;
    str : WideString;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if FWideFileName = '' then
      raise EAuException.Create('File name or URL is not assigned');
      lwma_async_reader_init(reader);
      if FProxyHost <> '' then
        lwma_async_reader_set_proxy(reader, FProxyProtocol, FProxyHost, FProxyPort);
      if FLoggingURL <> '' then
        lwma_async_reader_add_logging_url(reader, FLoggingURL);
      reader.StretchFactor := FStretchFactor;
      reader.MaxWaitMilliseconds := FMaxWait;
      reader.TimedOut := False;
      reader.EnableTCP := FEnableTCP;
      reader.EnableHTTP := FEnableHTTP;
      reader.EnableUDP := FEnableUDP;
      reader.BufferingTime := FBufferingTime * 10000000;
      lwma_async_reader_open(reader, FWideFileName);
      if Assigned(FOnStreamOpened) then
        EventHandler.PostGenericEvent(Self, FOnStreamOpened);
      FValid := reader.has_audio;
      if reader.reader = nil then Exit;
      FChan := reader.channels;
      FBPS := reader.BitsPerSample;
      FSR := reader.SampleRate;
      FBitrate := reader.Bitrate;
      FTotalSamples := reader.duration*FSR;
      if FTotalSamples = 0 then
      begin
        FSize := -1;
        FTotalSamples := -1;
      end else
        FSize := FTotalSamples*FChan*(FBPS shr 3);
      FSeekable := False;
      SetLength(Str, 256);
      lwma_async_reader_get_author(reader, Str);
      _Id3v2Tags.Artist := Str;
      lwma_async_reader_get_title(reader, Str);
      _Id3v2Tags.Title := Str;
      lwma_async_reader_get_album(reader, Str);
      _Id3v2Tags.Album := Str;
      lwma_async_reader_get_genre(reader, Str);
      _Id3v2Tags.Genre := Str;
      lwma_async_reader_get_track(reader, Str);
      _Id3v2Tags.Track := Str;
      lwma_async_reader_get_year(reader, Str);
      _Id3v2Tags.Year := Str;
      lwma_async_reader_get_copyright(reader, Str);
      _Id3v2Tags.Comment := Str;
      FFirstTime := True;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMStreamedIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      reader.MaxWaitMilliseconds := 2000;
      lwma_async_reader_clear_logging_urls(reader);
      lwma_async_reader_free(reader);
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMStreamedIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  begin
    lwma_async_reader_get_data(reader, Buffer, Bytes);
    if FFirstTime then
    begin
      if Assigned(FOnStartedPlaying) then
          EventHandler.PostGenericEvent(Self, FOnStartedPlaying);
      FFirstTime := False;    
    end;
  end;

  function TWMStreamedIn.SeekInternal(var SampleNum : Int64) : Boolean;
//  var
//    Offset : LongWord;
  begin
    Result := False;
(*    if Busy then
    begin
      Offset := Round(SampleNum/FTotalSamples*FDuration);
      lwma_reader_seek(reader, Offset);
      Result := True;
    end; *)
  end;

  function TWMStreamedIn.GetHasAudio;
  begin
    OpenFile;
    Result := reader.has_audio;
  end;

  function TWMStreamedIn.GetId3v2Tags;
  begin
    Result := _Id3v2Tags;
  end;

  function TWMStreamedIn.GetTotalTime;
  begin
    Result := reader.duration;
  end;

  procedure TWMStreamedIn.ResetStretch;
  begin
    FStretchFactor := NewStretch;
    lwma_async_reader_reset_stretch(reader, NewStretch);
  end;

  procedure TWMStreamedIn._Pause;
  begin
    _Lock;
    lwma_async_reader_pause(reader);
    _Unlock;
  end;

  procedure TWMStreamedIn._Resume;
  begin
    lwma_async_reader_resume(reader);
  end;

  procedure TWMStreamedIn.SetBufferingTime;
  begin
    if value in [1..60] then
      FBufferingTime := value;
  end;

  function TWMStreamedIn.GetTimedOut;
  begin
    Result := reader.TimedOut;
  end;

  function TWMAOut.GetCodecs;
  begin
    Result := FCodecs;
  end;

  function TWMAOut.GetCodecsCount;
  begin
    Result := FCodecs.Count;
  end;

  function TWMAOut.GetCodecName;
  begin
    if Index < FCodecs.Count then
      Result := FCodecs.Strings[Index];
  end;

  function TWMAOut.GetFormats;
  begin
    FFormats.Clear;
    if Index < FCodecs.Count then
      lwma_enumerate_codec_formats(Index, FVBR or FLossless, FFormats);
    Result := FFormats;
  end;

  function TWMAOut.GetFormatsCount;
  begin
    GetFormats(Index);
    Result := FFormats.Count;
  end;

  function TWMAOut.GetFormatDesc;
  begin
      GetFormats(CodecIndex);
      if FormatIndex >= FFormats.Count then Result := ''
      else
        Result := FFormats.Strings[FormatIndex];
  end;


end.
